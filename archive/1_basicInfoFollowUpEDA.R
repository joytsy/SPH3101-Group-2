# ++++++++ Basic Information TB Disease Cohort EDA ++++++++ 

# ++++++++ FOLLOWUP SURVEY EDA ++++++++ 
# Run '1_load_data.r' script to load datatb2 dataset, else can run the following line
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")


# -------- Data Processing and Manipulation --------
data <- datatb2 # created new df 'data' to modify on

# 20240926: Removed Following Observations
# - 4 observations with age < 18 (only interested in adult cohort >= 18 years)
below18 <- c("NEWH0071", "NEWP0063", "NEWP0263", "TBD0036")

data <- data %>%
  filter(!(a1_record_id %in% below18)) # gets observations that are not inside the below18 vector

### Create additional "stigma_score" column using sum a1_q7_fu to a1_q18_fu, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
data <- data %>%
  mutate(stigma_score = rowSums(across(a1_q7_fu:a1_q18_fu)))

# Check number of non responses (cross check with individual's status later)
## 212 non responses
data %>% 
  filter(is.na(a1_q3_fu)) %>%
  group_by(a1_status_tb_during_fu) %>%
  summarise(
    count=n(),
  ) %>% ungroup()



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
## Brief Summary: Tbong Khmum and Kampong Cham has same percentages as previously, not the same for the other 2 provinces
provFuTab <- tableDistribution(data$a1_prov_fu, percent=FALSE)
provFuTab


# Distribution of Individuals from Different Operational Districts
## Brief Summary: Aside from S'ang and Leuk Deuk districts, all other districts maintained same percentages
operDistFuTab <- tableDistribution(data$a1_operat_dist_fu, percent=FALSE)
operDistFuTab

### Additional EDA later for micro view: Check to see if individuals did move across provinces and districts


# Distribution of Patients' Status during Follow Up
## Brief Summary: 74.5% were still alive during follow-up visit, whereas 14.3% were loss of F/U, 7.6% refused to F/U and 
## the remaining 3.6% had died.
statusFuTab  <- tableDistribution(data$a1_status_tb_during_fu, percent=FALSE)
statusFuTab


# Distribution of Relapse and Non-relapse Population
## Brief Summary: 65.1% did not experience relapse/reinfection, whereas 12.1% had. 22.8% were NA values (could be due to loss to F/U)
relapseFuTab <- tableDistribution(data$a1_exp_relap_reinf_lfu_fu, percent=FALSE)
relapseFuTab

### To plot or get bivariate distribution later (status vs relapse)


# Distribution of Types of TB
## Brief Summary: Only 3 types of TB detected in this cohort, with 56.4% with TB Bac-, 38.5% with TB Bac+ and 5.0% with EP TB.
## Same percentages obtained for TB Bac- and TB Bac+, RR TB percentage = EP TB percentage
tbTypesFuTab <- tableDistribution(data$a1_type_tb_fu, percent=FALSE)
tbTypesFuTab



# -------- Other EDA Stuff i.e. Bivariate Variables  --------
# Frequency Distribution of Relapse/Non-Relapse Cases among Different F/U Statuses
data %>%
  group_by(as_factor(a1_status_tb_during_fu), as_factor(a1_exp_relap_reinf_lfu_fu)) %>%
  summarise(
    count=n()
  ) %>% 
  pivot_wider(id_cols = 'as_factor(a1_status_tb_during_fu)', 
              names_from='as_factor(a1_exp_relap_reinf_lfu_fu)', 
              values_from="count")


# Frequency Distribution of Types of TB among Different F/U Statuses
data %>%
  group_by(as_factor(a1_status_tb_during_fu), as_factor(a1_type_tb_fu)) %>%
  summarise(
    count=n()
  ) %>% 
  pivot_wider(id_cols = 'as_factor(a1_status_tb_during_fu)', 
              names_from='as_factor(a1_type_tb_fu)', 
              values_from="count")


# Frequency Distribution of Relapse/Non-Relapse Cases among Different Types of TB
data %>%
  group_by(as_factor(a1_exp_relap_reinf_lfu_fu), as_factor(a1_type_tb_fu)) %>%
  summarise(
    count=n()
  ) %>% 
  pivot_wider(id_cols = 'as_factor(a1_exp_relap_reinf_lfu_fu)', 
              names_from='as_factor(a1_type_tb_fu)', 
              values_from="count")


# Group by f/u status and get boxplot of stigma scores by different f/u status
## Brief Summary: summary statistics of alive group, mean score is 13.2 and median is 14
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_status_tb_during_fu), y=stigma_score), na.rm=TRUE) +
  labs(title="Stigma Scores by Follow-Up Status",
       x="Follow-Up Status",
       y="Stigma Scores")

# Group by follow up status and get summary statistics of stigma scores in each group
data %>% 
  group_by(a1_status_tb_during_fu) %>%
  summarise(
    meanScore=mean(stigma_score, na.rm=TRUE),
    minScore=min(stigma_score, na.rm=TRUE),
    maxScore=max(stigma_score, na.rm=TRUE),
    medianScore=median(stigma_score, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(stigma_score)) # count number of missing values (should be 212)
  )


# Group by relapse status and get boxplot of stigma scores by different relapse status
## Brief Summary: those that had relapsed/experienced reinfection had a higher median stigma score as compared to those who did not
## IQR range for relapsed group generally higher than that of non-relapse group
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_exp_relap_reinf_lfu_fu), y=stigma_score), na.rm=TRUE) +
  labs(title="Stigma Scores by Relapse Status",
       x="Relapse Status",
       y="Stigma Scores")

# Group by relapse status and get summary statistics of stigma scores in each case group
data %>% 
  group_by(a1_exp_relap_reinf_lfu_fu) %>%
  summarise(
    meanScore=mean(stigma_score, na.rm=TRUE),
    minScore=min(stigma_score, na.rm=TRUE),
    maxScore=max(stigma_score, na.rm=TRUE),
    medianScore=median(stigma_score, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(stigma_score)) # count number of missing values (should be 212)
  )


# Group by province and get boxplot of stigma scores by different province
## Brief Summary: Kampong Cham and Phnom Penh seem to have the highest stigma scores, followed by Kandal and Tboung Khmum
## Median for Kampong Cham and Kandal seems to be similar from baseline, 
## while median score had decreased from baseline for Tboung Khmum, and increased from baseline for Phnom Penh
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_prov_fu), y=stigma_score), na.rm=TRUE) +
  labs(title="Stigma Scores by Province",
       x="Province",
       y="Stigma Scores")

# Group by a1_prov_fu and get summary statistics of stigma scores in each province
data %>% 
  group_by(a1_prov_fu) %>%
  summarise(
    meanScore=mean(stigma_score, na.rm=TRUE),
    minScore=min(stigma_score, na.rm=TRUE),
    maxScore=max(stigma_score, na.rm=TRUE),
    medianScore=median(stigma_score, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(stigma_score)) # count number of missing values (should be 212)
  )


# Group by tb type and get boxplot of stigma scores by different tb types
## Brief Summary: 
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_type_tb_fu), y=stigma_score), na.rm=TRUE) +
  labs(title="Stigma Scores by TB Type",
       x="TB Type",
       y="Stigma Scores")

# Group by a1_type_tb_fu and get summary statistics of stigma scores for each tb type
data %>% 
  group_by(a1_type_tb_fu) %>%
  summarise(
    meanScore=mean(stigma_score, na.rm=TRUE),
    minScore=min(stigma_score, na.rm=TRUE),
    maxScore=max(stigma_score, na.rm=TRUE),
    medianScore=median(stigma_score, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(stigma_score)) # count number of missing values (should be 212)
  )

