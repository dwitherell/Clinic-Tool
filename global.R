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




