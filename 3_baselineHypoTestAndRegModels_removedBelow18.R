# ++++++++ BASELINE SURVEY Hypothesis Testing and Regression Models ++++++++ 

# -------- SUMMARY -------- 
# Tests used for hypothesis testing include t-test and wilcoxon for continuous variables,
# while chi-square test for categorical variables.
# Regression Models were also fitted with statistically significant variables (after conducting hypo testing)
# and we slowly reduced the number of predictors in the models until all variables had a p-value of <0.05.
# Additional things to try: ANOVA?
# Initial factors we thought would be significant in determining stigma levels: age, sex, education level, smoking, alchohol, income
# After conducting hypothesis testing, signficant factors were: several tb symptoms experienced (cough, chest pain, dyspnea, weight loss, fever, night sweat),
# current diagnosis of tb type, province and operational districts.
# After running linear regression on the significant factors, we are left with cough, fever and night sweat as
# significant predictor variables for stigma scores.
# Whereas for logistic regression, we are left with cough, weight loss, fever, night sweat
# and operational district as significant predictor variables for whether one has a high or low stigma score.


# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
load('data/datatb1.rdata')



# -------- Load Libraries --------
# Load package ggplot2 and dplyr(to be used for data manipulation and data visualisation)
library(ggplot2)
library(dplyr)
library(haven)



# -------- Data Processing and Manipulation --------
data <- datatb1 # created new df 'data' to modify on

# to remove individuals < 18 from data
data <- data %>%
  filter(a1_q3>=18)



# convert categorical variables to factors
data[,"a1_q1"] <- as.factor(data[,"a1_q1"]) # sex
data[,"a1_q5"] <- as.factor(data[,"a1_q5"]) # education
data[,"a1_q16"] <- as.factor(data[,"a1_q16"]) # smoked previously (follow Teo et al research)
data[,"a1_q18"] <- as.factor(data[,"a1_q18"]) # drink alcohol

# perform row sum across stigma scores
data <- data %>%
  mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))

# create new column to categorise individuals experiencing high and low stigma (string ver)
data <- data %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > 15.4 ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= 15.4 ~ "Low",  # Categorize as Low if stigma_score <= 15.4
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

# create new column to categorise individuals experiencing high and low stigma (numerical ver)
data <- data %>%
  mutate(
    stigma_thresNumber = case_when(
      stigma_score > 15.4 ~ 1,  # Categorize as 1 if stigma_score > 15.4
      stigma_score <= 15.4 ~ 0,  # Categorize as 0 if stigma_score <= 15.4
      TRUE ~ NA_real_  # Handle any unexpected cases
    )
  )

# create column to categorise education levels into (primary and below) and (above primary)
data <- data %>%
  mutate(
    abovePrimary = case_when(
      a1_q5 %in% c(1, 5) ~ "No",  # Categorize as No if no formal schooling or attended primary school only
      a1_q5 %in% c(2, 3, 4) ~ "Yes",  # Categorize as Yes if attended above primary school
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )



# -------- Hypothesis Testing for Continuous Variables (t-test and wilcox) --------
# T-test for age (since normally distributed)
## Not significant --> p-value = 0.3987
t.test(data$a1_q3[data$stigma_threshold %in% "High"], data$a1_q3[data$stigma_threshold %in% "Low"])

# Wilcox Test for income (since right-skewed distribution)
## Not significant --> p-value = 0.3455
wilcox.test(data$a1_q7[data$stigma_threshold %in% "High"], data$a1_q7[data$stigma_threshold %in% "Low"])

# T-test for distance to nearest health facility
## Not significant --> p-value = 0.1419
t.test(data$a1_q12[data$stigma_threshold %in% "High"], data$a1_q12[data$stigma_threshold %in% "Low"])

# T-test for time to travel to nearest facility
## Not significant --> p-value = 0.8238
t.test(data$a1_q13[data$stigma_threshold %in% "High"], data$a1_q13[data$stigma_threshold %in% "Low"])



# -------- Hypothesis Testing for Categorical Variables (Chisq Test) --------
# Chisq for sociodemo, smoking (previously) and alcohol consumption factors
# sex
## Not significant --> p-value = 0.3379
chisq.test(data$stigma_threshold, data$a1_q1)

# education level
## Not significant --> p-value = 0.2173
chisq.test(data$stigma_threshold, data$abovePrimary)

# smoked previously
## Not significant --> p-value = 0.3244
chisq.test(data$stigma_threshold, data$a1_q16)

# alcohol consumption
## Not significant --> p-value = 0.3905
chisq.test(data$stigma_threshold, data$a1_q18)

# marital status
## Not significant --> p-value = 0.513
chisq.test(data$stigma_threshold, data$a1_q4)


# Chisq Test for symptoms
# cough symptoms (significant)
## Significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_q28___1)

# cough with blood symptoms
## Not significant --> p-value = 0.3472
chisq.test(data$stigma_threshold, data$a1_q28___2)

# chest pain (significant)
## Significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_q28___3)

# dyspnea (significant)
## Significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_q28___4)

# fever (significant)
## Significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_q28___5)

# chills
## Not significant --> p-value = 0.136
chisq.test(data$stigma_threshold, data$a1_q28___6)

# weight loss (significant)
## Significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_q28___7)

# night sweat (significant)
## Significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_q28___8)


# Chisq for medical conditions
# stroke (NA for stroke --> no individuals experienced stroke)

# heart disease
## Not significant --> p-value = 1
chisq.test(data$stigma_threshold, data$a1_q20___2)

# hypertension
## Not significant --> p-value = 0.8602
chisq.test(data$stigma_threshold, data$a1_q20___3)

# diabetes
## Not significant --> p-value = 0.3488
chisq.test(data$stigma_threshold, data$a1_q20___4)

# asthma (NA since not accurate due to separation error)
# chisq.test(data$stigma_threshold, data$a1_q20___5)

# lung disease
## Not significant --> p-value = 0.8199
chisq.test(data$stigma_threshold, data$a1_q20___6)

# current diagnosis of type of tb (significant)
# significant --> p-value = 0.03122
chisq.test(data$stigma_threshold, data$a1_type_tb)

# previous diagnosis of TB 
## Not significant --> p-value = 0.05892
chisq.test(data[data$a1_q24 %in% c(1,2), ]$stigma_threshold, data[data$a1_q24 %in% c(1,2), ]$a1_q24)


# Chisq for basic info of cohort
# province (significant)
## significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_prov)

# Chisq for operational district (significant)
## significant --> p-value < 0.01
chisq.test(data$stigma_threshold, data$a1_operat_dist)


# -------- Linear Regresion of Stigma Scores against Significant Predictor Variables  --------
## For each model, remove predictor variable with largest p-value until the remaining predictors are significant.
## After removing variables with p-value > 0.05, we are left with cough, fever and night sweat as significant
## predictors for stigma scores.

mod1 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb+data$a1_prov+data$a1_operat_dist)
summary(mod1)

mod2 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+ # removed a1_q28___7 (weight loss)
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb+data$a1_prov+data$a1_operat_dist)
summary(mod2)

mod3 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+ # removed a1_prov 
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb+data$a1_operat_dist)
summary(mod3)

mod4 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+ # removed a1_q28___4 (dyspnea)
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb+data$a1_operat_dist)
summary(mod4)

## --------- without removing < 18 yo individuals ---------

mod5 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+ # removed a1_operat_dist 
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb)
summary(mod5)

mod6 <- lm(data$stigma_score~data$a1_q28___1+ # removed a1_q28___3 (chest pain)
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb)
summary(mod6)

mod7 <- lm(data$stigma_score~data$a1_q28___1+ # removed a1_type_tb
             data$a1_q28___5+data$a1_q28___8)
summary(mod7)

## ---------  removing < 18 yo individuals ---------

mod5_1 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+ # removed a1_type_tb from mod4 
             data$a1_q28___5+data$a1_q28___8+data$a1_operat_dist)
summary(mod5_1)

mod6_1 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+ # removed a1_operat_dist 
               data$a1_q28___5+data$a1_q28___8)
summary(mod6_1)

mod7_1 <- lm(data$stigma_score~data$a1_q28___1+ # removed a1_q28___3 (chest pain)
               data$a1_q28___5+data$a1_q28___8)
summary(mod7_1)


# -------- Logistic Regresion of Stigma Levels against Significant Predictor Variables  --------
## Use stigma_thresNumber outcome variable for logistic regression models.
## For each model, remove predictor variable with largest p-value until the remaining predictors are significant.
## After removing variables with p-value > 0.05, we are left with cough, weight loss, fever, night sweat
# and operational district as significant predictors for whether one has high or low stigma.

m1 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
             data$a1_q28___5+data$a1_q28___8+data$a1_type_tb+data$a1_prov+data$a1_operat_dist, family='binomial')
summary(m1)

m2 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+ # removed a1_type_tb
            data$a1_q28___5+data$a1_q28___8+data$a1_prov+data$a1_operat_dist, family='binomial')
summary(m2)

m3 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+ # removed a1_prov
            data$a1_q28___5+data$a1_q28___8+data$a1_operat_dist, family='binomial')
summary(m3)

m4 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___4+data$a1_q28___7+ # removed a1_q28___3 (chest pain)
            data$a1_q28___5+data$a1_q28___8+data$a1_operat_dist, family='binomial')
summary(m4)

m5 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___7+ # removed a1_q28___4 (dyspnea)
            data$a1_q28___5+data$a1_q28___8+data$a1_operat_dist, family='binomial')
summary(m5)

