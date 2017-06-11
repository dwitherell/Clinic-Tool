#### i) PACKAGES ----
library(tidyverse)
library(forcats)
library(haven)

#### 1) LOAD DATA ----
# data should include:
# - original dataset
# - lookup file of attribute codes
# - lookup file of brand/vehicle codes

original <- read_sav("./preprocess/m170036.sav")
banner <- read_csv("./preprocess/banner.csv")
attribute <- read_csv("./preprocess/attribute.csv")

#### 2) GET/RENAME VARIABLES ----
# a) select variables associated with:
# - lever maps
# - perceptual maps
# - correlations
# b) rename the 5 consistent variables:
# - serial, weight, filters 1, 2 & 3

keep <- original %>%

    # main 5 variables
    select(Respondent_Serial,
           wgt036,
           TGT,
           # filter 2
           # filter 3
           
           # attribute variables
           contains("_B40_"), 
           contains("_B1_"), 
           contains("_B30_"), 
           contains("_B9_"), 
           contains("_B10_"), 
           contains("_B11_"), 
           contains("_B12_"), 
           contains("_B13_a_"), 
           contains("_B13_b_"), 
           contains("_B13_c_"), 
           contains("_B13_d_"), 
           contains("_B13_e_"), 
           contains("_B15_"), 
           contains("_B18_a_"), 
           contains("_B18_b_"), 
           contains("_B18_c_"), 
           contains("_B18_d_"), 
           contains("_B18_e_"), 
           contains("_B18_f_"), 
           contains("_B18_g_"), 
           contains("_B18_h_"), 
           contains("_B18_i_"), 
           contains("_B29_"), 
           contains("_B29a_"), 
           contains("_B31_a_"), 
           contains("_B31_b_"), 
           contains("_B31_c_"), 
           contains("_B31_d_"), 
           contains("_B31_e_"), 
           contains("_B31_f_"), 
           contains("_B35_"), 
           contains("_B39_"), 
           contains("_B43_"), 
           contains("_D4_"), 
           contains("_C1_"), 
           contains("_C21_"), 
           contains("_C2_"), 
           contains("_C3_"), 
           contains("_C4_"), 
           contains("_C5_"), 
           contains("_C6_"), 
           contains("_C7_"), 
           contains("_C8_"), 
           contains("_C9_"), 
           contains("_C10_"), 
           contains("_C12_"), 
           contains("_C14_"), 
           contains("_C15_"), 
           contains("_C16_"), 
           contains("_C17_"), 
           contains("_C18_"), 
           contains("_C19_"), 
           contains("_C22_"), 
           contains("_C27_"), 
           contains("_C23_"), 
           contains("_C24_"), 
           contains("_C25_"), 
           contains("_C26_"), 
           contains("_C28_"), 
           contains("_C29_"), 
           contains("_D1_"), 
           contains("_D3_"), 
           contains("_E1_"), 
           contains("_C28_"), 
           contains("_C20NEW_a_"), 
           contains("_C20NEW_b_"), 
           contains("_C20NEW_c_"), 
           contains("_C20NEW_d_"), 
           contains("_C20NEW_e_"), 
           contains("_C20NEW_f_"), 
           contains("_C20NEW_g_"), 
           contains("_C20NEW_h_"), 
           contains("_C20NEW_i_"), 
           contains("_C20NEW_j_"), 
           contains("_C20NEW_k_"), 
           contains("_E4_"), 
           contains("_C18_"), 
           contains("_C19_"), 
           contains("_C17_"), 
           contains("_C15_"), 
           contains("_C16_"), 
           contains("_C30_"), 
           contains("_C28_"), 
           contains("_C29_"), 
           contains("_B35_"), 
           contains("_B29a_"), 
           contains("_B15_"), 
           contains("_B39_"), 
           contains("_B40_"), 
           contains("_B45_"), 
           contains("_B39_"), 
           contains("_C26_"), 
           contains("_C23_"), 
           contains("_C24_"), 
           contains("_C25_"), 
           contains("_D3_"), 
           contains("_C12_"), 
           contains("_E1_"), 
           contains("_D4_"), 
           contains("_B40_"), 
           contains("_B46_a_"), 
           contains("_B46_b_"), 
           contains("_B46_c_"), 
           contains("_B46_d_"), 
           contains("_B46_e_"), 
           contains("_B46_f_"), 
           contains("_B46_g_"), 
           contains("_B46_h_"), 
           contains("_B46_i_"), 
           contains("_B46_j_"), 
           contains("_B46_k_"), 
           contains("_B46_l_"), 
           contains("_B46_m_"), 
           contains("_B46_n_"), 
           contains("_B46_o_"), 
           contains("_B46_p_"), 
           contains("_B46_q_")) %>%
    
    # rename the consistent 5 variables
    rename(Serial = Respondent_Serial,
           Weight = wgt036,
           Target = TGT
           # filter 2
           # filter 3
    )

#### 3) FIX EXTRANEOUS VARIABLE NAMES ----
# - this will need to be personalized to the dataset
better_names <- keep %>%
    setNames(gsub("SecB_", "", names(.))) %>%
    setNames(gsub("_GV1", "", names(.))) %>%
    setNames(gsub("_0", "", names(.)))

#### 4) FIX FILTER VARIABLES ----
# a) give proper labels to factors
# b) if needed... create blank filter variables

fixfactors <- better_names %>%
    
    # fix the factor labels & give proper name
    mutate(Target = 
               as.factor(Target) %>%
               fct_recode(
                   "Target"     = "1",
                   "Non-target" = "2"),

           # create blank filters
           `Filter 2` = "Total",
           `Filter 3` = "Total") %>%
    
    # put in proper order for stacking
    select(Serial, Weight, Target, `Filter 2`, `Filter 3`, everything())


#### 5) STACK THE DATA ----
# double stacking by vehicle/brand and attribute
stacked <- fixfactors %>% 
    
    # stack and split
    gather(Comb, Rating, -c(1:5)) %>%
    separate(Comb, c("Banner_Code", "Attribute_Code"), extra = "merge") %>%
    
    # get rid of banners not used in output
    # SOMETIMES DON'T DO THIS
    filter(Banner_Code != "I")

#-----------------------------------------------------------------------------#
# STOP! ---------------------------- STOP! ---------------------------- STOP! #
#-----------------------------------------------------------------------------#

# This is a good time to check and see if the lookup tables
# will match your data for the succeeding table join
unique(stacked$Attribute_Code) %>% as.data.frame()
unique(stacked$Banner_Code)

#### 6) ADD IN LABELS, REORDER, SORT ----
clean <- stacked %>%
    
    # add vehicle and qid labels
    left_join(attribute, by = "Attribute_Code") %>%
    left_join(banner, by = "Banner_Code") %>%
    
    # put in order
    select(Serial, Weight, Banner_Code, Banner, 
           Attribute_Code, Attribute, Rating,
           Target, `Filter 2`, `Filter 3`) %>%
    
    # sort
    arrange(Serial, Banner_Code, Attribute_Code)

#### 7) SAVE FILE ----
saveRDS(object = clean, file = "./data/maps_corrs.rds")
