# -------- Load Libraries --------
# Load package ggplot2 and dplyr(to be used for data manipulation and data visualisation)
library(ggplot2)
library(dplyr)
library(haven)

# load('data/datatb1.rdata')
# -------- Data Processing and Manipulation --------
data <- datatb1 # created new df 'data' to modify on

# remove observations where individuals < 18
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

# Create column to categorise alcohol consumption into drinker or non-drinker
data <- data %>%
  mutate(
    alcoholConsumer = case_when(
      a1_q18 %in% c(5) ~ "Non-Drinker",  # Categorize as No 
      a1_q5 %in% c(1, 2, 3, 4) ~ "Drinker",  # Categorize as Yes if person drinks
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

# Create variable for treatment duration
data<- data %>%
  mutate(treatmentDuration = Finish.treatment.date - Start.treatment.date, .after=Finish.treatment.date)

# -------------- Exploring Overall Distribution of Outcome Variable (Stigma Score) ----------------
summary(data$stigma_score)
hist(data$stigma_score, main = "Histogram of Stigma Scores (Baseline)")
boxplot(data$stigma_score, main = "Boxplot of Stigma Scores (Baseline)")

# -------------- EDA / Significance Testing for Continuous Variables --------------------------

# Age - Normally Distributed
summary(data$a1_q3)
hist(data$a1_q3, main = "Histogram of Participant Age (Baseline)")
boxplot(data$a1_q3, main = "Boxplot of Participant Age (Baseline)")
# T-test for age (since normally distributed)
## Not significant --> p-value = 0.3987 (after removing <18: p-value=0.3814, still not significant)
t.test(data$a1_q3[data$stigma_threshold %in% "High"], data$a1_q3[data$stigma_threshold %in% "Low"])

# Income 
summary(data$a1_q7)
boxplot(data$a1_q7, main = "Boxplot of Average Family Monthly Income (Baseline)")
hist(data$a1_q7, main = "Histogram of Average Family Monthly Income (Baseline)")
# Wilcox Test for income (since right-skewed distribution)
## Not significant --> p-value = 0.3455 (after removing <18: p-value=0.3361, still not significant)
wilcox.test(data$a1_q7[data$stigma_threshold %in% "High"], data$a1_q7[data$stigma_threshold %in% "Low"])

# Distance to Nearest Health Facility
summary(data$a1_q12)
hist(data$a1_q12, breaks = 40, main = "Histogram of Distance to Nearest Health Facilities (Baseline)")
boxplot(data$a1_q12,main = "Boxplot of Distance to Nearest Health Facilities (Baseline)")
# T-test for distance to nearest health facility
## Not significant --> p-value = 0.1419 (after removing <18: p-value=0.1377, still not significant) 
t.test(data$a1_q12[data$stigma_threshold %in% "High"], data$a1_q12[data$stigma_threshold %in% "Low"])

# Time to travel to nearest facility
summary(data$a1_q13)
hist(data$a1_q13, breaks = 30, main = "Histogram of Travel Time to Nearest Health Facilities (Baseline)")
boxplot(data$a1_q13,main = "Boxplot of Travel Time to Nearest Health Facilities (Baseline)")
# T-test for time to travel to nearest facility
## Not significant --> p-value = 0.8238 (after removing <18: p-value=0.7969, still not significant)
t.test(data$a1_q13[data$stigma_threshold %in% "High"], data$a1_q13[data$stigma_threshold %in% "Low"])

# Treatment Duration
summary(as.numeric(data$treatmentDuration))
hist(as.numeric(data$treatmentDuration), breaks = 30)
boxplot(as.numeric(data$treatmentDuration), breaks = 30)
# T-test for Treatment Duration
## Not significant --> p-value = 0.1615 (after removing <18: p-value=0.1617, still not significant)
t.test(as.numeric(data$treatmentDuration)[data$stigma_threshold %in% "High"], 
       as.numeric(data$treatmentDuration)[data$stigma_threshold %in% "Low"])

# -------------- EDA / Significance Testing for Categorical Variables --------------------------

# ----- Sex -----
table(data$stigma_threshold, factor(data$a1_q1,
             levels = c(1, 2), 
             labels = c("Male", "Female")))
chisq.test(data$stigma_threshold, data$a1_q1) ## Not significant --> p-value = 0.3379 (after removing <18: p-value=0.3402, still not significant)



# ---- Education Level ----
table(data$stigma_threshold, factor(data$a1_q5,
             levels = c(1, 2, 3, 4, 5), 
             labels = c("Primary", "Secondary", "High School", "University or Higher", "No Formal Schooling")))
chisq.test(data$stigma_threshold, data$a1_q5) ## Not significant --> p-value = 0.7161 (after removing <18: p-value=0.7299, still not significant)

# group into abovePrimary Yes and No
table(data$stigma_threshold, data$abovePrimary)
chisq.test(data$stigma_threshold, data$abovePrimary) ## Not significant --> p-value = 0.2173 (after removing <18: p-value=0.2429, still not significant)



# ---- Smoking History (so long as the individual has smoked before) ----
table(data$stigma_threshold, factor(data$a1_q16,
                                    levels = c(1, 2), 
                                    labels = c("Smoker", "Non Smoker")))
chisq.test(data$stigma_threshold, data$a1_q16) ## Not significant --> p-value = 0.3244 (after removing <18: p-value=0.3265, still not significant)



# ---- Alcohol Consumption ----
table(data$stigma_threshold,data$alcoholConsumer)
chisq.test(data$stigma_threshold, data$alcoholConsumer) # Not significant, p-value = 0.9414 (after removing <18: p-value=1, still not significant)
table(data$stigma_threshold,factor(datatb1$a1_q18, 
             levels = c(1, 2, 3, 4, 5), 
             labels = c(">=4x per Week", "2-3x per Week", "2-4x per Month", "Once a month or less", "No")))
chisq.test(data$stigma_threshold, data$a1_q18) # Not significant --> p-value = 0.3905 (after removing <18: p-value=.4725, still not significant)



# ---- Marital Status -----
table(data$stigma_threshold, factor(data$a1_q4,
             levels = c(1, 2, 3, 4), 
             labels = c("Never Married", "Married", "Divorced/separated", "Widowed")))
chisq.test(data$stigma_threshold, data$a1_q4) ## Not significant --> p-value = 0.513 (after removing <18: p-value=0.6619, still not significant)

# try grouping divorced/separated and widowed together (were married group had higher stigma scores in research done in Central China)
table(data$stigma_threshold, factor(data$a1_q4,
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Never Married", "Married", "Were Married", "Were Married"))) 
wereMarriedData <- data %>%
  mutate(a1_q4 = case_when(
    a1_q4 %in% c(1) ~ "Never Married",
    a1_q4 %in% c(2) ~ "Married",
    a1_q4 %in% c(3, 4) ~ "Were Married",
  ))
chisq.test(wereMarriedData$stigma_threshold, wereMarriedData$a1_q4) ## Not significant --> p-value = 0.4829 (after removing below 18 and grouping were married status tgt)



# ---- Case Status ----
table(data$stigma_threshold, data$case_status)
chisq.test(data$stigma_threshold, data$case_status) # Not significant, chi-sq approx may not be appropriate
# Considering only those "completed" / "cured", Not significant, but p-value = 0.08838 which is quite small (after removing <18: p-value=0.1108, increase from before)
table(data$stigma_threshold[data$case_status %in% c("completed", "cured")], data$case_status[data$case_status %in% c("completed", "cured")])
chisq.test(data$stigma_threshold[data$case_status %in% c("completed", "cured")], data$case_status[data$case_status %in% c("completed", "cured")]) 



# ---- Current Diagnosis of TB Type ----
table(data$stigma_threshold, factor(data$a1_type_tb,
             levels = c(1, 2, 3, 4), 
             labels = c("TB Bac+", "TB Bac-", "Multidrug-resistant TB", "RR TB")))
chisq.test(data$stigma_threshold, data$a1_type_tb) # significant --> p-value = 0.03122 (after removing <18: p-value=0.0434, still significant)



# ---- Province ----
table(data$stigma_threshold, factor(data$a1_prov,
             levels = c(1, 2, 3, 4), 
             labels = c("Kampong Cham", "Tboung Khmum", "Kandal", "Phnom Penh")))
chisq.test(data$stigma_threshold, data$a1_prov) ## significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)



# ---- Operational District ----
table(data$stigma_threshold, factor(data$a1_operat_dist,
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
             labels = c("Stung Trang", "Kang Meas", "Suong", "Ou Raing", "Mukkampul",
                        "Lvea Em", "Sa Ang", "Leuk Dek", "Po Sen Chey", "Sen Sok")))
chisq.test(data$stigma_threshold, data$a1_operat_dist) ## significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)

# ---- HIV Status ----
table(data$stigma_threshold, factor(data$a1_q21,
             levels = c(1, 2, 3, 4), 
             labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose")))
chisq.test(data$stigma_threshold, factor(data$a1_q21,
                                         levels = c(1, 2, 3, 4), 
                                         labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose"))) #  Chi-squared approximation may be incorrect

# ---- Previous TB Diagnosis ----
table(data$stigma_threshold, factor(datatb1$a1_q24,
             levels = c(1, 2, 3), 
             labels = c("Yes", "No", "Don't Know")))
chisq.test(data[data$a1_q24 %in% c(1,2), ]$stigma_threshold, data[data$a1_q24 %in% c(1,2), ]$a1_q24) ## Not significant --> p-value = 0.05892 (after removing <18, p-value=0.05858, still not significant)


# ----------------------------- Various TB Symptoms Contracted --------------------------------
# 1. Cough
table(data$stigma_threshold, factor(datatb1$a1_q28___1,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___1) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)

# 2. Cough with Blood
table(data$stigma_threshold, factor(datatb1$a1_q28___2,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___2) ## Not significant --> p-value = 0.3472 (after removing <18, p-value=0.3462, still not significant)

# 3. Chest Pain
table(data$stigma_threshold, factor(datatb1$a1_q28___3,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___3) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)

# 4. Dyspnea (Shortness of Breath)
table(data$stigma_threshold, factor(datatb1$a1_q28___4,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___4) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)

# 5. Fever
table(data$stigma_threshold, factor(datatb1$a1_q28___5,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___5) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)

# 6. Chills
table(data$stigma_threshold, factor(datatb1$a1_q28___6,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___6) ## Not significant --> p-value = 0.136 (after removing <18, p-value=0.1363, still not significant)

# 7. Loss of Weight
table(data$stigma_threshold, factor(datatb1$a1_q28___7,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___7) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)


# 8. Night Sweat
table(data$stigma_threshold, factor(datatb1$a1_q28___8,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___8) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)


# 9. Other Symptoms
table(data$stigma_threshold, factor(datatb1$a1_q28___9,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___9) ## Significant --> p-value < 0.01 (after removing <18, p-value<0.01, still significant)


# ----------------------------- Various Other Medical Conditions --------------------------------
# Stroke
table(factor(datatb1$a1_q20___1, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___1, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___1) ## (NA for stroke --> no individuals experienced stroke)

# Heart Disease
table(factor(datatb1$a1_q20___2, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___2, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___2) ## both times (before and after removing <18) p-value=1, not significant

# Hypertension
table(factor(datatb1$a1_q20___3, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___3, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___3) ## both times not significant (before 0.8602, after 0.8627)

# Diabetes
table(factor(datatb1$a1_q20___4, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___4, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___4) ## both times not significant (before 0.3488, after 0.3475)

# Asthma
table(factor(datatb1$a1_q20___5, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___5, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___5) ## (NA since not accurate due to separation error)

# Lung Disease
table(factor(datatb1$a1_q20___6, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___6, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___6) ## both times not significant (before 0.8199, after 0.8191)

# Liver Disease
table(factor(datatb1$a1_q20___7, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___7, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___7) ## both times (before and after removing <18) p-value=1, not significant

# Mental Illness
table(factor(datatb1$a1_q20___8, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___8, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___8) ## NA since not accurate due to separation error

# Other Conditions Diagnosed
table(factor(datatb1$a1_q20___9, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___9, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___9) ## both times not significant before (0.305) and after removing (0.3033)

# No Other Conditions - 'Yes' means no other conditions, 'No' means individual has one of the above conditions
table(factor(datatb1$a1_q20___10, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(datatb1$a1_q20___10, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___10) ## both times (before and after removing <18) p-value=1, not significant
