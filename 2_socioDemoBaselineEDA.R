# ++++++++ Socio-Demographics EDA ++++++++ 

# ++++++++ BASELINE SURVEY EDA ++++++++ 
# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
# load('data/datatb1.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")


# -------- Data Processing and Manipulation --------
tb1_socioDemo <- datatb1[, which(colnames(datatb1) %in% c('a1_record_id',paste0('a1_q', 1:13)))]
tb1_socioDemo


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
## Brief Summary: Gender Distribution almost even among Males and Females. There are slight more Males (51.6%) than Females (48.4%).
genderTab <- tableDistribution(tb1_socioDemo$a1_q1)
genderTab


# Race Distribution
## Brief Summary: Majority of Khmer race (97.0%), followed by (2.2%), Vietnamese (0.7%) and others (0.1%).
raceTab <- tableDistribution(tb1_socioDemo$a1_q2)
raceTab


# Age Distribution Summary Statistics and Histogram Plot
## Brief Summary: Age range is between 16-96, with median of 59 and mean 57.7.
## From histogram plot, 212 individuals come from 60-70 range, followed by 194 from 50-60 and 142 from 70-80.

# Summary Statistics of Age Distribution
summary(tb1_socioDemo$a1_q3)

# Histogram Plot of Age Distribution
ggplot(tb1_socioDemo, aes(x = a1_q3)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 10, boundary=10) + # split into ranges of 10 years i.e. 11-20, 21-30, ...
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 10, boundary=10,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Age Distribution among Adult TB Cohort",
       x = "Age", y = "Count")


# Marital Status Distribution
## Brief Summary: Most (67.6%) are married, whereas 21.6% are widowed, and both 'Never married' and 'Divorced/separated'
## are 5.4% respectively.
maritalTab <- tableDistribution(tb1_socioDemo$a1_q4)
maritalTab


# Highest Level of Education Distribution
## Brief Summary: Almost half (49.9%) had Primary School Education as their highest level of education, followed by 26.2%
## not having formal schooling, 14.9% had Secondary School Education, 7.0% had High School Education and 1.9% with University and higher education.
highestEduTab <- tableDistribution(tb1_socioDemo$a1_q5)
highestEduTab


# Current Occupation Distribution
## Brief Summary: Top 3 occupations are Unemployed (39.1%), Farmer (25.7%) and Housewife (11.2%).
occupationTab <- tableDistribution(tb1_socioDemo$a1_q6)
occupationTab


# Summary Statistics and Plot of Average Family Income 
## Brief Summary: Ranges between 0-11400, with median income of 1200 and mean 1527.
## 2 individuals have an average of 1140 (could potentially be an outlier) from Histogram Plot
## 1 individual with 0 income
summary(tb1_socioDemo$a1_q7)

ggplot(tb1_socioDemo, aes(x = a1_q7)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 500, boundary=0) + # split into ranges of 500 ...
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 500, boundary=0,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Average Family Income in past 6 months among Adult TB Cohort",
       x = "Average Income", y = "Count")


# Summary Statistics and Plot of Household Size
## Brief Summary: Ranges between 1-21, with median 5 household members and mean 5.2 members.
## Right skewed distribution, with a few individuals having more than 11 members.
summary(tb1_socioDemo$a1_q9)

# table(tb1_socioDemo$a1_q9)

ggplot(tb1_socioDemo, aes(x = a1_q9)) +
  geom_bar(aes(y = ..count..), col = "white", fill = "indianred") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..),
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Household Size among Adult TB Cohort",
       x = "Household Size", y = "Count")


# Summary Statistics and Plot of Number of Rooms in House
## Brief Summary: Ranges between 1-7, with a mean of 2.0 and median 2.
## Right skewed distribution, 1 individual has 6 rooms while another has 7 rooms.
summary(tb1_socioDemo$a1_q10)

table(tb1_socioDemo$a1_q10)

ggplot(tb1_socioDemo, aes(x = a1_q10)) +
  geom_bar(aes(y = ..count..), col = "white", fill = "indianred") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..),
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Number of Rooms in Individuals' Houses",
       x = "Number of Rooms", y = "Count")


# Summary Statistics and Plot of Number of People per Room
## Brief Summary: Ranges between 1-11. Mean is 3.9 and median is 3.
## Right skewed distribution, with 3 counts of 9, 10 counts of 10 and 6 counts of 11 people per room.
summary(tb1_socioDemo$a1_q11)

table(tb1_socioDemo$a1_q11)

ggplot(tb1_socioDemo, aes(x = a1_q11)) +
  geom_bar(aes(y = ..count..), col = "white", fill = "indianred") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..),
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Number of People Staying Per Room",
       x = "Number of People", y = "Count")


# Summary Statistics of Distance between House and Nearest Health Facilities
## Brief Summary: Ranges from 0.01-18. Mean is 2.84 and median is 2.
## Seems like there is an outlier observation (18).
summary(tb1_socioDemo$a1_q12)

ggplot(tb1_socioDemo, aes(x="", y=a1_q12)) +
  geom_boxplot(fill = "indianred") +
  labs(title = "Boxplot of Distance between House and Nearest Facility", x = "", y = "Distance")


# Summary Statistics of Duration to Reach Nearest Health Facilities
## Brief Summary: Ranges from 1-37. Mean is 8.0 and median is 6.0.
## Seems like there is an outlier observation (37).
summary(tb1_socioDemo$a1_q13)

ggplot(tb1_socioDemo, aes(x="", y=a1_q13)) +
  geom_boxplot(fill = "indianred") +
  labs(title = "Boxplot of Travel Time between House and Nearest Facility", x = "", y = "Time")
