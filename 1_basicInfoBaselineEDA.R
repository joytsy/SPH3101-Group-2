# ++++++++ Basic Information TB Disease Cohort EDA ++++++++ 

# ++++++++ BASELINE SURVEY EDA ++++++++ 
# Run 'load_data.r' script to load data dataset, else can run line 6
source('load_data.r')
# load('data/datatb1.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")



# -------- Data Processing and Manipulation --------
data <- datatb1 # created new df 'data' to modify on

# 20240925: Removed Following Observations
# - 4 observations with age < 18 (only interested in adult cohort >= 18 years)
data <- data %>%
  filter(a1_q3>=18)


# Create New Column 'treatmentDuration': Duration of Treatment Period (Difference between Start and Finish Treatment Dates)
# New column added after Finish.treatment.date column
data <- data %>% 
  mutate(treatmentDuration = Finish.treatment.date - Start.treatment.date, .after=Finish.treatment.date)


# Create additional "stigma_score" column using sum a1_q30 to a1_q41, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
data <- data %>%
  mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))


# statistical summary of crude sigma_score (mean is still 15.4)
summary(data$stigma_score) # mean 15.4 and median 16


# Categorize stigma_score using mean
data <- data %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > 15.4 ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= 15.4 ~ "Low",  # Categorize as Low if stigma_score <= 15.4
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )


# Check whether there are missing duration values in treatmentDuration 
# and their case status i.e. could be due to death, loss to F/U or others
## Brief Summary: There are 37 NAs, mostly due to death and lost to F/U.
# A tibble: 4 × 2
# case_status    count
# <chr>          <int>
# 1 completed          2
# 2 died              10
# 3 lost_follow_up    17 (minus one due to removed observations)
# 4 other              8

data %>% 
  filter(is.na(treatmentDuration)) %>%
  group_by(case_status) %>%
  summarise(
    count=n()
  )



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
# ** Apart from removed observations in data processing section, all missing data and outliers are still included. **

# Summary statistics of treatment duration
## Brief Summary: Generally treatment duration hover around 180+ days, with min 116 days and max 364 days.
## According to IQR outlier calculation, there are 187 observations that are considered outliers.
## From histogram, around 10 observations with treatment duration of < 150 or > 200 days.
summary(as.numeric(data$treatmentDuration))
boxplot(as.numeric(data$treatmentDuration))
hist(as.numeric(data$treatmentDuration))

q1 <- quantile(data$treatmentDuration, 0.25, na.rm=TRUE) # remove na values and get lower quartile
q3 <- quantile(data$treatmentDuration, 0.75, na.rm=TRUE) # remove na values and get upper quartile
IQR <- IQR(data$treatmentDuration, na.rm = TRUE)  # get interquartile range
outliers <- subset(data, data$treatmentDuration<(q1 - 1.5*IQR) | data$treatmentDuration>(q3 + 1.5*IQR)) # get outliers out of the calculated range
length(outliers[,1]) # 187 outliers 

data %>% filter(between(treatmentDuration, q1 - 1.5*IQR, q3 + 1.5*IQR)) %>% 
  count() # 609 observations within calculated range (not outliers)


# Distribution of Treatment Status
## Brief Summary
## Univariate Analysis: Majority (58.2%) have completed their treatment, followed by being cured (37.6%). Remaining cases have died, lost to F/U or classified under other reasons.
## Contingency Table: Relatively balanced for completed, cured and loss to f/u cases, but quite skewed for the rest.
statusTab <- tableDistribution(data$case_status, percent=FALSE)
statusTab

# 2xn contingency table of Stigma Levels and Case Status
table(data$stigma_threshold, data$case_status)


# Distribution of Individuals from Different Provinces
## Brief Summary
## Univariate Analysis: 46.1% come from Kandal, followed by 22.2% from Tboung Khmum, 18.7% from Kampong Cham and 12.9% from Phnom Penh.
## Contingency Table: Stigma levels relatively balanced in Kandal and Phnom Penh. Most individuals experience high stigma in Kampong Cham, while the opposite for Tboung Khmum.
provTab <- tableDistribution(data$a1_prov, percent=FALSE)
provTab

# 2xn contingency table of Stigma Levels and Provinces
table(data$stigma_threshold, as_factor(data$a1_prov))


# Distribution of Individuals from Operational Districts
## Brief Summary
## Univariate Analysis: Top 3 Operational Districts with this cohort of TB diagnosed individuals are Sa Ang Health OD (28.1%), Ou Raing Euv Health OD (19.7%) and Stung Trang Health OD (10.8%).
## Contingency Table: Most skewed stigma levels in Kang Meas Health OD, Stung Trang Health OD and Lvea Em Health OD. Most balanced relative to the rest: Leuk Dek Health OD, Sen Sok Health OD and Po Sen Chey Health OD.
operDistTab <- tableDistribution(data$a1_operat_dist, percent=FALSE)
operDistTab

# 2xn contingency table of Stigma Levels and Operational Districts
table(data$stigma_threshold, as_factor(data$a1_operat_dist))


# Distribution of Types of TB
## Brief Summary: Only 3 types of TB detected in this cohort, with 56.4% with TB Bac-, 38.5% with TB Bac+ and 5.1% with RR TB.
## Contingency Table: Still relatively balanced for Bac+ and Bac-, slightly skewed for RR-TB cases.
tbTypesTab <- tableDistribution(data$a1_type_tb, percent=FALSE)
tbTypesTab

# 2xn contingency table of Stigma Levels and types of TB
table(data$stigma_threshold, as_factor(data$a1_type_tb))


# -------- Other EDA Stuff i.e. Bivariate Variables  --------
# Boxplot of Duration of Treatment Period for Each Treatment Group
## Brief Summary: Range of treatment duration larger for completed group as compared to cured group.
## Other case status do not have duration values, which is expected
ggplot(data) +
  geom_boxplot(aes(x=case_status, y=as.numeric(treatmentDuration)), na.rm=TRUE) + # removed NA values
  labs(title = "Treatment Duration by Case Status",
       x = "Case Status",
       y = "Treatment Duration (days)") 

# Group by case_status and get summary statistics of treatment duration in each treatment group
# removed NA values when calculating statistics in mean, range and median columns
data %>% 
  group_by(case_status) %>%
  summarise(
    meanDuration=mean(treatmentDuration, na.rm=TRUE),
    minDuration=min(treatmentDuration, na.rm=TRUE),
    maxDuration=max(treatmentDuration, na.rm=TRUE),
    medianDuration=median(treatmentDuration, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(treatmentDuration)) # count number of missing values (should be 38)
  )

# Group by case_status and get boxplot of stigma scores by different case_status
## Brief Summary: From boxplot, we observe that lost to f/u group has a higher median stigma score than the other groups. 
## While cured and completed groups seems to have similar median stigma scores. 
## Died group has the 2nd lowest median stigma score, and other group has the lowest median stigma score.
## Tried removing outlier observation with score 36, results remain the same except mean=15.0 (down from 15.1).
## Removing both scores 0 and 36 -> results same except mean=15.2 (up from 15.1).
data %>% 
  ggplot() +
  geom_boxplot(aes(x=case_status, y=stigma_score)) +
  labs(title="Stigma Scores by Case Status",
       x="Case Status",
       y="Stigma Scores")

# Group by case_status and get summary statistics of stigma scores in each case group
data %>% 
  group_by(case_status) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )

# Group by province and get boxplot of stigma scores by different province
## Brief Summary: Kampong Cham seems to have the highest median stigma score among all provinces. 
## Seems like Kandal has the 2nd highest median stigma score, followed by Phnom Penh and Tboung Khmum.
## Observations with stigma scores 0 or 36 are found in Tboung Khmum.
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_prov), y=stigma_score)) +
  labs(title="Stigma Scores by Province",
       x="Province",
       y="Stigma Scores")

# Group by a1_prov and get summary statistics of stigma scores in each province
data %>% 
  group_by(a1_prov) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )

# Group by tb type and get boxplot of stigma scores by different tb types
## Brief Summary: TB Bac+ and Bac- seems to have similar median stigma scores, while RR TB has the lowest median stigma score.
## IQR range for Bac- is larger than Bac+ type.
## RR TB cases have a lower IQR score range as compared to the other 2 types.
## Bac- Type has the extreme scores 0 and 36.
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_type_tb), y=stigma_score)) +
  labs(title="Stigma Scores by TB Type",
       x="TB Type",
       y="Stigma Scores")

# Group by a1_type_tb and get summary statistics of stigma scores for each tb type
data %>% 
  group_by(a1_type_tb) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )

# Extra: TB Type Distribution among each treatment group
data %>% 
  select(!contains("inter")) %>% 
  group_by(case_status, a1_type_tb) %>%
  summarise(
    count=n() 
  ) %>%
  pivot_wider(id_cols = case_status, names_from="a1_type_tb", values_from="count")

# Extra: TB Type Distribution among patients in each province
data %>% 
  select(!contains("inter")) %>% 
  group_by(a1_prov, a1_type_tb) %>%
  summarise(
    count=n() 
  ) %>%
  pivot_wider(id_cols = a1_prov, names_from="a1_type_tb", values_from="count")

# Extra: Treatment Duration among each tb type
## Extracting observations that are considered outliers in each of the TB type groups, 
## we find that the observation with treatment duration of 364 days is much longer than the treatment regimen gone through by others in the same tb type group
## Might want to consider removing this observation
## Might also want to consider removing observation with treatment duration of 116 days.
# Extract observations that have anomalous treatment periods in their respective groups
durationOut <- data %>%
  group_by(a1_type_tb) %>%
  summarise(outliers=boxplot(treatmentDuration, plot=FALSE)$out) %>%
  ungroup()

# Boxplot of treatment duration according to each tb type
ggplot(data) +
  geom_boxplot(mapping=aes(x=as_factor(a1_type_tb), y=as.numeric(treatmentDuration)), outlier.shape = NA) + #outliers are hidden from display of boxplot initially
  geom_point(data=durationOut, aes(x=as_factor(a1_type_tb), y=outliers), alpha=0.5, col="red", position=position_jitter(width=0, height=0.1)) + # outliers are added as point here
  labs(title="Treatment Duration by TB Type",
       x="TB Type",
       y="Treatment Duration")

