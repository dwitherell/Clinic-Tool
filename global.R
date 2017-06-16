#### check for packages, install and load ------

package_list <- c("shiny", 
                  "shinydashboard",
                  "shinyjs",
                  "weights",
                  "Hmisc",
                  "tidyverse",
                  "ggrepel",
                  "colourpicker",
                  "magrittr",
                  "DT",
                  "RColorBrewer",
                  "scales",
                  "FactoMineR")

# checks to see which packages to install
to_install <- package_list[!(package_list %in% installed.packages()[, "Package"])]

# install them
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

# clean up
rm(package_list, to_install)

library(shiny)              # shiny framework
library(shinydashboard)     # dashboard framework
library(shinyjs)            # not sure yet...
library(weights)            # weighted corr
library(Hmisc)              # weighted stdev
library(tidyverse)          # data manip, plotting
library(ggrepel)            # label repels
library(colourpicker)       # color picker
library(magrittr)           # pipes
library(RColorBrewer)       # color palettes 
library(DT)                 # customizing data tables
library(scales)             # percents and stuff
library(FactoMineR)         # for correspondence analysis
library(stringr)            # label wrapping

#### global objects ----
bar_width <- 250

lineList <- c("twodash", "solid", "longdash", "dotted", "dotdash", "dashed", "blank")
fontList <- c("plain", "bold", "italic")
axisList <- c("axis labels", "axis text", "axis ticks")

pointList <- tibble(
    code = 21:25,
    shape = c("circle", "square", "diamond", "triangle (up)", "triangle (dwn)")
)

# CSS, this needes to be placed in CSS file
tweaks <- list(
    tags$head(
        tags$style(
            HTML(
                    "
                    .multicol { 
                        height: 500px;
                        -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                        -moz-column-count: 5;    /* Firefox */ 
                        column-count: 5; 
                        -moz-column-fill: auto;
                        -column-fill: auto;
                    } 
                    "
            )
        ) 
    )
)


### Morpace colors
## Dark
# 0, 75, 142
# #004B8E
## Light
# 108, 174, 224
# #6CAEE0

calc_lever_data <- function(data) {
    
    # sample size calculations
    N_Competitors <- length(unique(data$Banner)) - 1
    
    Tgt_SS <- data %>%
        group_by(Serial) %>%
        filter(row_number() == 1) %>%
        nrow(.)
    
    Comp_SS <- Tgt_SS * N_Competitors
    
    # means
    means <- data %>%
        mutate(Group = ifelse(Banner == input$lvr_banner,
                              "Tgt_Mean", "Comp_Mean")) %>%
        group_by(Group, Attribute) %>%
        summarise(avg = mean(Weight * Rating)) %>%
        spread(Group, avg) %>%
        mutate(Gap = Tgt_Mean - Comp_Mean)
    
    # standard deviation
    stdev <- data %>%
        mutate(Group = ifelse(Banner == input$lvr_banner, 
                              "Tgt_Std", "Comp_Std")) %>%
        group_by(Group, Attribute) %>%
        summarise(std = sqrt(wtd.var(x = Rating, weights = Weight))) %>%
        spread(Group, std)
    
    # correlations
    corr <- data %>%
        select(-Attribute_Code) %>%
        spread(Attribute, Rating) %>%
        select(-Serial, -c(Banner_Code:Banner)) %$%
        wtd.cors(., weight = Weight) %>%
        as.data.frame() %>%
        rownames_to_column("Attribute") %>%
        gather(Corr_var, Correlation, -Attribute) %>%
        filter(Corr_var ==  "Cargo Suitability") %>%
        select(Attribute, Correlation)
    
    # combine together
    final <- means %>%
        mutate(Comp_SS = Comp_SS,
               Tgt_SS  = Tgt_SS) %>%
        left_join(stdev, by = "Attribute") %>%
        left_join(corr, by = "Attribute") %>%
        mutate(
            Tgt_Std2  = Tgt_Std ^ 2,
            Comp_Std2 = Comp_Std ^ 2,
            Pool_Std  = (((Tgt_SS - 1) * Tgt_Std2) + ((Comp_SS - 1) * Comp_Std2)) / (Tgt_SS + Comp_SS - 2),
            Tstat     = Gap / sqrt(Pool_Std * ((1 / Tgt_SS) + (1 / Comp_SS))),
            Sig       = as.factor(ifelse(Gap > 0 & abs(Tstat) > abs(qnorm(0.1)), "Sig higher", 
                                         ifelse(Gap < 0 & abs(Tstat) > abs(qnorm(0.1)), "Sig lower", "Not sig")))
        ) %>%
        select(Attribute, Correlation, Gap, Tgt_Mean, Comp_Mean, 
               Tgt_SS, Comp_SS, Tgt_Std, Comp_Std, Sig) %>%
        set_names(c("Attribute", "Correlation", "Gap", 
                    "Tgt. Mean", "Comp. Mean", 
                    "Tgt. N", "Comp. N",
                    "Tgt. Std", "Comp. Std",
                    "Sig"))
    
    
    final
    
}


