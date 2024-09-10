# ++++++++ Basic Information TB Disease Cohort EDA ++++++++ 

# ++++++++ BASELINE SURVEY EDA ++++++++ 
# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
# load('data/datatb1.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")

# -------- Data Processing and Manipulation --------
# Subset and obtain basic info of TB cohort
# Basic info columns: a1_record_id to a1_intervention
tb1_basicInfo <- datatb1 %>% select(a1_record_id:a1_intervention)


# Create New Column 'treatmentDuration': Duration of Treatment Period (Difference between Start and Finish Treatment Dates)
# New column added after Finish.treatment.date column
tb1_basicInfo <- tb1_basicInfo %>% 
  mutate(treatmentDuration = Finish.treatment.date - Start.treatment.date, .after=Finish.treatment.date)


# Check whether there are missing duration values in treatmentDuration 
# and their case status i.e. could be due to death, loss to F/U or others
## Brief Summary: There are 38 NAs, mostly due to death and lost to F/U.
tb1_basicInfo %>% 
  filter(is.na(treatmentDuration)) %>% 
  group_by(case_status) %>%
  summarise(
    count=n()
  )

# -------- Summary Statistics and Distributions --------
# ** None of the observations (individual's data) were removed i.e. columns with some missing data and outliers are included. **
# Summary statistics of treatment duration
## Brief Summary: Generally treatment durations hover around 180+ days, with min 116 days and max 364 days.
summary(as.numeric(tb1_basicInfo$treatmentDuration))
# boxplot(tb1_basicInfo$treatmentDuration)


# Distribution of Treatment Status
## Brief Summary: Majority (58.1%) have completed their treatment, followed by being cured (37.6%). 
## Remaining cases have died, lost to F/U or classified under other reasons.
statusTab <- table(tb1_basicInfo$case_status)
sort((statusTab/sum(statusTab))*100, decreasing=TRUE)


# Distribution of Individuals from Different Provinces
## Brief Summary: 46.0% come from Kandal, followed by 22.1% from Tboung Khmum, 18.8% from Kampong Cham and 13.1% from Phnom Penh.
provTab <- table(as_factor(tb1_basicInfo$a1_prov))
sort((provTab/sum(provTab))*100, decreasing = TRUE)


# Distribution of Individuals from Operational Districts
## Brief Summary: Top 3 Operational Districts with this cohort of TB diagnosed individuals are
## Sa Ang Health OD (28.O%), Ou Raing Euv Health OD (19.6%) and Stung Trang Health OD (10.9%).
operDistTab <- table(as_factor(tb1_basicInfo$a1_operat_dist))
sort((operDistTab/sum(operDistTab))*100, decreasing=TRUE)


# Distribution of Types of TB
## Brief Summary: Only 3 types of TB detected in this cohort, with 56.2% with TB Bac-, 38.7% with TB Bac+ and 5.1% with RR TB.
tbTypesTab <- table(as_factor(tb1_basicInfo$a1_type_tb))
sort((tbTypesTab/sum(tbTypesTab))*100, decreasing=TRUE)


# -------- Other EDA Stuff i.e. Bivariate Variables  --------
# Boxplot of Duration of Treatment Period for Each Treatment Group
## Brief Summary: Range of treatment duration larger for completed group as compared to cured group.
## Other case status do not have duration values, which is expected
ggplot(tb1_basicInfo) +
  geom_boxplot(aes(x=case_status, y=as.numeric(treatmentDuration)), na.rm=TRUE) + # removed NA values
  labs(title = "Treatment Duration by Case Status",
       x = "Case Status",
       y = "Treatment Duration (days)") 

# Group by case_status and get summary statistics of treatment duration in each treatment group
# removed NA values when calculating statistics in mean, range and median columns
tb1_basicInfo %>% 
  group_by(case_status) %>%
  summarise(
    meanDuration=mean(treatmentDuration, na.rm=TRUE),
    minDuration=min(treatmentDuration, na.rm=TRUE),
    maxDuration=max(treatmentDuration, na.rm=TRUE),
    medianDuration=median(treatmentDuration, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(treatmentDuration)) # count number of missing values (should be 38)
  )

# Extra: TB Type Distribution among each treatment group
tb1_basicInfo %>% 
  select(!contains("inter")) %>% 
  group_by(case_status, a1_type_tb) %>%
  summarise(
    count=n() 
  ) %>%
  pivot_wider(id_cols = case_status, names_from="a1_type_tb", values_from="count")

# Extra: TB Type Distribution among patients in each province
tb1_basicInfo %>% 
  select(!contains("inter")) %>% 
  group_by(a1_prov, a1_type_tb) %>%
  summarise(
    count=n() 
  ) %>%
  pivot_wider(id_cols = a1_prov, names_from="a1_type_tb", values_from="count")
