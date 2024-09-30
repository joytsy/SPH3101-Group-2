# ++++++++ Socio-Demographics EDA ++++++++ 

# ++++++++ BASELINE SURVEY EDA ++++++++ 

# -------- Load Libraries and Necessary Scripts --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")

# Run 1_basicInfoBaselineEDA script to get processed data (might need to modify code later, probs put processed data in 1 script)
source('1_basicInfoBaselineEDA.r')

# -------- Data Processing and Manipulation --------
# create column to categorise education levels into (primary and below) and (above primary)
data <- data %>%
  mutate(
    abovePrimary = case_when(
      a1_q5 %in% c(1, 5) ~ "No",  # Categorize as No if no formal schooling or attended primary school only
      a1_q5 %in% c(2, 3, 4) ~ "Yes",  # Categorize as Yes if attended above primary school
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
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
# Gender Distribution
## Brief Summary
## Univariate Analysis: Gender Distribution almost even among Males and Females. There are slight more Males (51.9%) than Females (48.1%).
## Contingency Table: Stigma Levels are almost balanced in 50/50 ratio for females. Males also quite balanced, around 46/54 ratio in low/high levels.
genderTab <- tableDistribution(data$a1_q1, percent=FALSE)
genderTab

# 2xn contingency table of Stigma Levels and Gender
table(data$stigma_threshold, as_factor(data$a1_q1))


# Race Distribution
## Brief Summary
## Univariate Anaylsis: Majority of Khmer race (97.1%), followed by (2.0%), Vietnamese (0.7%) and others (0.1%).
## Contingency Table: Quite well-balanced for Khmer and Khmer Cham race groups, while Vietnamese and Other groups only have a few observations
## Race not a good variable to add inside analysis since quite homogenous.
raceTab <- tableDistribution(data$a1_q2, percent=FALSE)
raceTab

# 2xn contingency table of Stigma Levels and Race
table(data$stigma_threshold, as_factor(data$a1_q2))


# Age Distribution Summary Statistics and Histogram Plot
## Brief Summary: Age range is between 18-96, with median of 59 and mean 57.9.
## From histogram plot, 212 individuals come from 60-70 range, followed by 194 from 50-60 and 142 from 70-80.

# Summary Statistics of Age Distribution
summary(data$a1_q3)

# Histogram Plot of Age Distribution
ggplot(data, aes(x = a1_q3)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 10, boundary=10) + # split into ranges of 10 years i.e. 11-20, 21-30, ...
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 10, boundary=10,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Age Distribution among Adult TB Cohort",
       x = "Age", y = "Count")

# boxplot of age distribution
ggplot(data, aes(x="", y=a1_q3)) +
  geom_boxplot(fill = "indianred") +
  labs(title = "Boxplot of Age Distribution", x = "", y = "Age")


# Marital Status Distribution
## Brief Summary
## Univariate Analysis: Most (67.7%) are married, whereas 21.7% are widowed, 'Divorced/separated' are 5.4% and 'Never married' are 5.2%.
## Contingency Table: Relatively balanced for married, divorced/separated and widowed groups. While never married group is slightly balanced at high/low ~ 60/40 ratio
maritalTab <- tableDistribution(data$a1_q4, percent=FALSE)
maritalTab

# 2xn contingency table of Stigma Levels and Marital Status
table(data$stigma_threshold, as_factor(data$a1_q4))


# Highest Level of Education Distribution
## Brief Summary
## Univariate: Almost half (50.1%) had Primary School Education as their highest level of education, followed by 26.3%
## not having formal schooling, 15.0% had Secondary School Education, 7.0% had High School Education and 1.7% with University and higher education.
## Contingency Table: For all groups except University and higher, high and low stigma levels are generally quite well balanced across, with high stigma slightly higher across the board
## Hard to comment about university and higher group due to small sample
highestEduTab <- tableDistribution(data$a1_q5, percent=FALSE)
highestEduTab

# 2xn contingency table of Stigma Levels and Education Level
table(data$stigma_threshold, as_factor(data$a1_q5))

# group into abovePrimary Yes and No
abovePrimaryTab <- tableDistribution(data$abovePrimary, percent=FALSE)
abovePrimaryTab

# 2xn contingency table of Stigma Levels and abovePrimary Levels
table(data$stigma_threshold, data$abovePrimary)


# Current Occupation Distribution
## Brief Summary
## Univariate Analysis: Top 3 occupations are Unemployed (39.1%), Farmer (25.8%) and Housewife (11.2%).
## Contingency Table: Farmer, Small business, Unemployed, Construction worker and Other occupation groups are quite balanced
## While the other groups are slightly unbalanced. 
## Hard to analyse for student, retired and factory worker groups
occupationTab <- tableDistribution(data$a1_q6, percent=FALSE)
occupationTab

# 2xn contingency table of Stigma Levels and Occupation
table(data$stigma_threshold, as_factor(data$a1_q6))


# Summary Statistics and Plot of Average Family Income 
## Brief Summary
## Univariate Analysis: Ranges between 0-11400, with median income of 1200 and mean 1527.
## 2 individuals have an average of 1140 (could potentially be an outlier) from Histogram Plot
## 1 individual with 0 income
summary(data$a1_q7)

ggplot(data, aes(x = a1_q7)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 500, boundary=0) + # split into ranges of 500 ...
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 500, boundary=0,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Average Family Income in past 6 months among Adult TB Cohort",
       x = "Average Income", y = "Count")

# boxplot of income distribution
ggplot(data, aes(x="", y=a1_q7)) +
  geom_boxplot(fill = "indianred") +
  labs(title = "Boxplot of Average Family Income Distribution", x = "", y = "Income")


# Residential Address Distribution (more for checking whether there is loss to f/u later)
## Brief Summary: Most of them stay in the same OD where interview takes place (821) while the remaining 12 are classified as other.
residentialAddTab <- tableDistribution(data$a1_q8, percent=FALSE)
residentialAddTab


# Summary Statistics and Plot of Household Size
## Brief Summary: Ranges between 1-21, with median 5 household members and mean 5.2 members.
## Majority of counts concentrated in smaller household sizes, with a few individuals having more than 11 members.
summary(data$a1_q9)

ggplot(data, aes(x = a1_q9)) +
  geom_bar(aes(y = ..count..), col = "white", fill = "indianred") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..),
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Household Size among Adult TB Cohort",
       x = "Household Size", y = "Count")


# Summary Statistics and Plot of Number of Rooms in House
## Brief Summary: Ranges between 1-7, with a mean of 2.0 and median 2.
## Majority of counts concentrated in fewer rooms, 1 individual has 6 rooms while another has 7 rooms.
summary(data$a1_q10)

ggplot(data, aes(x = a1_q10)) +
  geom_bar(aes(y = ..count..), col = "white", fill = "indianred") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..),
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Number of Rooms in Individuals' Houses",
       x = "Number of Rooms", y = "Count")


# Summary Statistics and Plot of Number of People per Room
## Brief Summary: Ranges between 1-11. Mean is 3.85 and median is 3.
## Right skewed distribution, with 3 counts of 9, 10 counts of 10 and 6 counts of 11 people per room.
summary(data$a1_q11)

ggplot(data, aes(x = a1_q11)) +
  geom_bar(aes(y = ..count..), col = "white", fill = "indianred") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..),
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Number of People Staying Per Room",
       x = "Number of People", y = "Count")


# Summary Statistics of Distance between House and Nearest Health Facilities
## Brief Summary: Ranges from 0.01-18. Mean is 2.84 and median is 2.
## Seems like there is an outlier observation (18).
summary(data$a1_q12)

ggplot(data, aes(x="", y=a1_q12)) +
  geom_boxplot(fill = "indianred") +
  labs(title = "Boxplot of Distance to Nearest Health Facilities", x = "", y = "Distance")

ggplot(data, aes(x = a1_q12)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 1, boundary=0) + # split into range of 1
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 1, boundary=0,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Distance to Nearest Health Facilities",
       x = "Distance", y = "Count")

# Summary Statistics of Duration to Reach Nearest Health Facilities
## Brief Summary: Ranges from 1-37. Mean is 8.0 and median is 6.0.
## Seems like there is an outlier observation (37).
summary(data$a1_q13)

ggplot(data, aes(x="", y=a1_q13)) +
  geom_boxplot(fill = "indianred") +
  labs(title = "Boxplot of Travel Time to Nearest Health Facilites", x = "", y = "Time")

ggplot(data, aes(x = a1_q13)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 5, boundary=0) + # split into ranges of 5
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 5, boundary=0,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Travel Time to Nearest Health Facilities",
       x = "Time Taken", y = "Count")


# -------- Other EDA Stuff i.e. Bivariate Variables  --------
# Group by sex and get boxplot of stigma scores by different sex
## Brief Summary: Not much difference in median stigma scores across box sexes.
## IQR range for males is wider than female's, but min-max range is larger in female group.
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_q1), y=stigma_score)) +
  labs(title="Stigma Scores by Sex",
       x="Sex",
       y="Stigma Scores")

# Group by sex and get summary statistics of stigma scores in each sex
data %>% 
  group_by(a1_q1) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )


# Scatterplot of stigma scores against age 
## Brief Summary: Seems like there is no association between age and stigma scores as shown by no linear trend.
data %>% 
  ggplot() +
  geom_point(mapping=aes(x=a1_q3, y=stigma_score), alpha=0.4, colour="red") +
  labs(title="Scatterplot of stigma scores and age",
       x = "Age", y="Stigma Score")


# Group by marital status and get boxplot of stigma scores by different marital status
## Brief Summary: all groups have similar median stigma scores.
## IQR for all groups except ‘never married’ have similar IQR ranges, while never married group 
## seems to have a higher IQR range as compared to other groups
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_q4), y=stigma_score)) +
  labs(title="Stigma Scores by Marital Status",
       x="Marital Status",
       y="Stigma Scores")

# Group by marital status and get summary statistics of stigma scores in each status
data %>% 
  group_by(a1_q4) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )


# Group by education level and get boxplot of stigma scores by different education levels
## Brief Summary: Median scores across all levels seem to be the same.
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_q5), y=stigma_score)) +
  labs(title="Stigma Scores by Education Level",
       x="Education Level",
       y="Stigma Scores")

# Group by education level and get summary statistics of stigma scores in each level
data %>% 
  group_by(a1_q5) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )

# Group by 2 categories (Yes or No in abovePrimary) and get boxplot
data %>% 
  ggplot() +
  geom_boxplot(aes(x=abovePrimary, y=stigma_score)) +
  labs(title="Stigma Scores across Individuals who received Above Primary School Education or Not",
       x="Education Received",
       y="Stigma Scores")

# Get summary stats of the 2 categories
data %>% 
  group_by(abovePrimary) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )


# Scatterplot of stigma scores against average family income 
## Brief Summary: Seems like there is no association between income and stigma scores as shown by the lack of linearity
data %>% 
  ggplot() +
  geom_point(mapping=aes(x=a1_q7, y=stigma_score), alpha=0.4, colour="red") +
  labs(title="Scatterplot of stigma scores and average family income",
       x = "Average Family Income", y="Stigma Score")

