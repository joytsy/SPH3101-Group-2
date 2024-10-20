# ++++++++ Socio-Demographics EDA ++++++++ 

# ++++++++ FOLLOW-UP SURVEY EDA ++++++++ 
# Run 'load_data.r' script to load datatb2 dataset, else can run the following line
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")

# Run 1_basicInfoFollowUpEDA script to get processed data (might need to modify code later, probs put processed data in 1 script)
source('archive/1_basicInfoFollowUpEDA.r')

# -------- Data Processing and Manipulation --------
data <- data %>%
  mutate(
    abovePrimary = case_when(
      a1_q4_fu %in% c(1, 5) ~ "No",  # Categorize as No if no formal schooling or attended primary school only
      a1_q4_fu %in% c(2, 3, 4) ~ "Yes",  # Categorize as Yes if attended above primary school
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

# remove lost to follow up observations (should have 621 left)
data <- data %>% 
  filter(!is.na(stigma_score))

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
# ** Note that Stats are performed based on dataframe with NA values for stigma score removed. **

# Gender Distribution
## Brief Summary: Gender Distribution almost even among Males and Females. There are slight more Males (50.6%) than Females (49.4%). 
genderFuTab <- tableDistribution(data$a1_q1_fu, percent=FALSE)
genderFuTab


# Marital Status Distribution
## Brief Summary: Most (68.9%) are married, whereas 24.2% are widowed, 3.7% are divorced/separated and 3.2% are never married.
maritalFuTab <- tableDistribution(data$a1_q3_fu, percent=FALSE)
maritalFuTab


# Highest Level of Education Distribution
## Brief Summary: More than half (50.9%) had Primary School Education as their highest level of education, followed by 27.9%
## not having formal schooling, 15.0% had Secondary School Education, 5.2% had High School Education and 1.1% with University and higher education.
highestEduFuTab <- tableDistribution(data$a1_q4_fu, percent=FALSE)
highestEduFuTab

# group into abovePrimary groups yes and no
# 78.7% received primary school education and below, while the remaining 21.3% received above primary school education. 
abovePrimaryTab <- tableDistribution(data$abovePrimary, percent=FALSE)
abovePrimaryTab


# Current Occupation Distribution
## Brief Summary: Top 3 occupations are Farmer (36.6%), Unemployed (26.1%) and Housewife (16.3%).
occupationFuTab <- tableDistribution(data$a1_q5_fu, percent=FALSE)
occupationFuTab


# Summary Statistics and Plot of Average Family Income during treatment
## Brief Summary: Ranges from 0 to 7200. Mean is 1289 and median 1080.
## Right skewed distribution. Possibly an outlier with average income of 7200.
## 4 individuals have 0 income.
summary(data$a1_q6_fu)

data %>% 
  ggplot(aes(x = a1_q6_fu)) +
    geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 500, boundary=0) + # split into ranges of 500
    geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 500, boundary=0,
              vjust = -0.5, color = "black") +  # Adding text of counts above the bars
    labs(title = "Average Family Income during Treatment among Adult TB Cohort",
         x = "Average Income", y = "Count")



# -------- Other EDA Stuff i.e. Bivariate Variables  --------

# boxplot of stigma scores by different sex
## Brief Summary: Not much difference in median stigma scores across box sexes.
## IQR range for males is wider than female's, and min-max range is larger in male group as compared to before
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_q1_fu), y=stigma_score)) +
  labs(title="Stigma Scores by Sex",
       x="Sex",
       y="Stigma Scores")

# Group by sex and get summary statistics of stigma scores in each sex
data %>% 
  group_by(a1_q1_fu) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )


# Group by marital status and get boxplot of stigma scores by different marital status
## Brief Summary: Seems to be slight variation in median scores across the different groups as seen in boxplot. 
## Never married group seems to have the highest score, while divorced/separated group has the lowest score.
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_q3_fu), y=stigma_score)) +
  labs(title="Stigma Scores by Marital Status",
       x="Marital Status",
       y="Stigma Scores")

# Group by marital status and get summary statistics of stigma scores in each status
data %>% 
  group_by(a1_q3_fu) %>%
  summarise(
    meanScore=mean(stigma_score),
    minScore=min(stigma_score),
    maxScore=max(stigma_score),
    medianScore=median(stigma_score),
    count=n() # number of observations
  )


# Group by education level and get boxplot of stigma scores by different education levels
## Brief Summary: Median scores across all levels seem to be similar.
data %>% 
  ggplot() +
  geom_boxplot(aes(x=as_factor(a1_q4_fu), y=stigma_score)) +
  labs(title="Stigma Scores by Education Level",
       x="Education Level",
       y="Stigma Scores")

# Group by education level and get summary statistics of stigma scores in each level
data %>% 
  group_by(a1_q4_fu) %>%
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
  geom_point(mapping=aes(x=a1_q6_fu, y=stigma_score), alpha=0.4, colour="red") +
  labs(title="Scatterplot of stigma scores and average family income",
       x = "Average Family Income", y="Stigma Score")
