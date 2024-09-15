# ++++++++ Socio-Demographics EDA ++++++++ 

# ++++++++ FOLLOW-UP SURVEY EDA ++++++++ 
# Run 'load_data.r' script to load datatb2 dataset, else can run the following line
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package tidyverse (to be used for data manipulation and visualisation)
library(tidyverse)
# If package not installed yet, run the following code before the code above 
# install.packages("tidyverse")


# -------- Data Processing and Manipulation --------
tb2_socioDemo <- datatb2[, which(colnames(datatb2) %in% c('a1_record_id',paste0('a1_q', 1:6, '_fu')))]
tb2_socioDemoNARemoved <- tb2_socioDemo %>% # removed rows with missing data
  filter(!is.na(a1_q3_fu))


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
# ** Note that Stats are performed based on dataframe with NA values removed. **
# Gender Distribution
## Brief Summary: Gender Distribution almost even among Males and Females. There are slight more Males (50.6%) than Females (49.4%). 
genderFuTab <- tableDistribution(tb2_socioDemoNARemoved$a1_q1_fu)
genderFuTab


# Marital Status Distribution
## Brief Summary: Most (68.9%) are married, whereas 24.2% are widowed, 3.7% are divorced/separated and 3.2% are never married.
maritalFuTab <- tableDistribution(tb2_socioDemoNARemoved$a1_q3_fu)
maritalFuTab


# Highest Level of Education Distribution
## Brief Summary: More than half (50.9%) had Primary School Education as their highest level of education, followed by 27.9%
## not having formal schooling, 15.0% had Secondary School Education, 5.2% had High School Education and 1.1% with University and higher education.
highestEduFuTab <- tableDistribution(tb2_socioDemoNARemoved$a1_q4_fu)
highestEduFuTab


# Current Occupation Distribution
## Brief Summary: Top 3 occupations are Farmer (36.6%), Unemployed (26.1%) and Housewife (16.3%).
occupationFuTab <- tableDistribution(tb2_socioDemoNARemoved$a1_q5_fu)
occupationFuTab


# Summary Statistics and Plot of Average Family Income during treatment
## Brief Summary: Ranges from 0 to 7200. Mean is 1289 and median 1080.
## Right skewed distribution. Possibly an outlier with average income of 7200.
## 4 individuals have 0 income.
summary(tb2_socioDemoNARemoved$a1_q6_fu)

ggplot(tb2_socioDemoNARemoved, aes(x = a1_q6_fu)) +
  geom_histogram(aes(y = ..count..), col = "white", fill = "indianred", binwidth = 500, boundary=0) + # split into ranges of 500
  geom_text(stat = 'bin', aes(label = ..count.., y = ..count..), binwidth = 500, boundary=0,
            vjust = -0.5, color = "black") +  # Adding text of counts above the bars
  labs(title = "Average Family Income during Treatment among Adult TB Cohort",
       x = "Average Income", y = "Count")
