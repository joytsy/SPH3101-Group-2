# ++++++++ Basic Information TB Disease Cohort EDA ++++++++ 

# ++++++++ FOLLOWUP SURVEY EDA ++++++++ 
# Run 'load_data.r' script to load datatb2 dataset, else can run the following line
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")


# -------- Data Processing and Manipulation --------
# Check number of non responses (cross check with individual's status later)
## 216 non responses
datatb2 %>% 
  filter(is.na(a1_q3_fu)) %>%
  group_by(a1_status_tb_during_fu) %>%
  summarise(
    count=n(),
  ) %>% ungroup()

# Subset and obtain basic info of TB cohort
# Basic info columns: a1_record_id to a1_intervent_fu
tb2_basicInfo <- datatb2 %>% select(a1_record_id:a1_intervent_fu)



# -------- Functions Implemented -------- 
# Provides Percentage Distribution of Labels of 'col' variables, percent=FALSE to get counts, results are sorted in descending order
tableDistribution <- function(col, percent=TRUE) { # pass in 'col' that we want to obtain distribution for, percent is default TRUE, which converts into percentage
  tab <- table(as_factor(col)) # provides contingency table
  if (percent==TRUE) { # if TRUE, will get percentage distribution
    sort((tab/sum(tab))*100, decreasing = TRUE)
  } else { # if not TRUE (FALSE), will get frequency distribution only
    sort(tab, decreasing = TRUE)
  }
}



# -------- Summary Statistics and Distributions --------

# Cross check province and operational district data with baseline survey

# Distribution of Individuals from Different Provinces
## Brief Summary: Tbong Khmum and Kampong Cham has same percentages, not the same for the other 2 provinces
provFuTab <- tableDistribution(tb2_basicInfo$a1_prov_fu)
provFuTab


# Distribution of Individuals from Different Operational Districts
## Brief Summary: Aside from S'ang and Pou Senchey districts, all other districts maintained same percentages
operDistFuTab <- tableDistribution(tb2_basicInfo$a1_operat_dist_fu)
operDistFuTab

### Additional EDA later for micro view: Check to see if individuals did move across provinces and districts


# Distribution of Patients' Status during Follow Up
## Brief Summary: 74.2% were still alive during follow-up visit, whereas 14.6% were loss of F/U, 7.6% refused to F/U and 
## the remaining 3.6% had died.
# statusFuTab <- tableDistribution(tb2_basicInfo$a1_status_tb_during_fu, percent=FALSE) # cross check with NAs 
statusFuTab  <- tableDistribution(tb2_basicInfo$a1_status_tb_during_fu)
statusFuTab


# Distribution of Relapse and Non-relapse Population
## Brief Summary: 64.8% did not experience relapse/reinfection, whereas 12.1% had. 23.2% were NA values (could be due to loss to F/U)
relapseFuTab <- tableDistribution(tb2_basicInfo$a1_exp_relap_reinf_lfu_fu, percent=FALSE)
relapseFuTab

### To plot or get bivariate distribution later (status vs relapse)


# Distribution of Types of TB
## Brief Summary: Only 3 types of TB detected in this cohort, with 56.2% with TB Bac-, 38.7% with TB Bac+ and 5.1% with EP TB.
## Same percentages obtained for TB Bac- and TB Bac+, RR TB percentage = EP TB percentage
tbTypesFuTab <- tableDistribution(tb2_basicInfo$a1_type_tb_fu)
tbTypesFuTab



# -------- Other EDA Stuff i.e. Bivariate Variables  --------
# Frequency Distribution of Relapse/Non-Relapse Cases among Different F/U Statuses
tb2_basicInfo %>%
  group_by(as_factor(a1_status_tb_during_fu), as_factor(a1_exp_relap_reinf_lfu_fu)) %>%
  summarise(
    count=n()
  ) %>% 
  pivot_wider(id_cols = 'as_factor(a1_status_tb_during_fu)', 
              names_from='as_factor(a1_exp_relap_reinf_lfu_fu)', 
              values_from="count")


# Frequency Distribution of Types of TB among Different F/U Statuses
tb2_basicInfo %>%
  group_by(as_factor(a1_status_tb_during_fu), as_factor(a1_type_tb_fu)) %>%
  summarise(
    count=n()
  ) %>% 
  pivot_wider(id_cols = 'as_factor(a1_status_tb_during_fu)', 
              names_from='as_factor(a1_type_tb_fu)', 
              values_from="count")


# Frequency Distribution of Relapse/Non-Relapse Cases among Different Types of TB
tb2_basicInfo %>%
  group_by(as_factor(a1_status_tb_during_fu), as_factor(a1_exp_relap_reinf_lfu_fu)) %>%
  summarise(
    count=n()
  ) %>% 
  pivot_wider(id_cols = 'as_factor(a1_status_tb_during_fu)', 
              names_from='as_factor(a1_exp_relap_reinf_lfu_fu)', 
              values_from="count")
