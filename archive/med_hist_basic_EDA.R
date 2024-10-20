# I only loaded Baseline Dataset since Followup Dataset doesn't have questions on Medical History
source("load_data.R")
# load('data/datatb1.rdata')

# -------------------------------------------  HIV Status --------------------------------------------------------

# Findings: 7 "Positive", 757 "Negative", 73 "Not Sure", 0 "I don't want to disclose"
table(factor(datatb1$a1_q21,
      levels = c(1, 2, 3, 4), 
      labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose")))
# Q: If positive, when were you diagnosed? (Date)
# Findings: 830 'NA's, 7 dates for the 7 HIV Positive Observations as follows: 
#           2002-09-09 2003-02-15 2006-08-01 2010-02-15 2012-06-15 2022-02-28 2022-04-01
table(datatb1$a1_q22, useNA = "ifany")

# Q: If positive, are you currently on antiretroviral therapy (1: Yes, 2: No)
# Findings: 830 'NA's, 7 HIV Positive Observations are ALL currently on anti-retroviral therapy
table(datatb1$a1_q23, useNA = "ifany")

# ------------------------------------------- Previous TB Diagnosis --------------------------------------------------------

# Q: Have you ever been told by a health provider that you had TB?
# Findings: 75 "Yes", 761 "No", 1 "Don't Know"
table(factor(datatb1$a1_q24,
             levels = c(1, 2, 3), 
             labels = c("Yes", "No", "Don't Know")))
# Q: If Yes, when was it diagnosed? (Date)
# Findings: 762 'NA's, 75 Dates from 1988-06-15 to 2022-07-19 
table(datatb1$a1_q25, useNA = "ifany")
# Q: What type of TB were you previously diagnosed with?
# Findings: 762 'NA's, 
#           TB Bac+: 19, TB Bac-: 22, Multidrug-resistant TB: 0, RR TB: 0,
#           Extrapulmonary TB: 3, Others: 2, Don't Know: 29
table(factor(datatb1$a1_q26,
             levels = c(1, 2, 3, 4, 5, 6, 7), 
             labels = c("TB Bac+", "TB Bac-", "Multidrug-resistant TB", "RR TB", "Extrapulmonary TB", "Others", "Don't Know")), useNA = "ifany")
# Q: Did you complete TB Treatment in your previous diagnosis? 
# Findings: Not everyone in the 75 who had previous TB Diagnosis answered for this question, resulting in more 'NA's (791) than expected (762)
#           791 'NA's, 39 "Yes", 4 "No", 3 "Did not start treatment"
table(factor(datatb1$a1_q27, 
             levels = c(1, 2, 3), 
             labels = c("Yes", "No", "Did not start treatment")), useNA = "ifany")

# ------------------------------------------- Smoking --------------------------------------------------------

# Smoking - Current and History (a1_q14:a1_q17)
# Q14: Current Smoking Status
# Findings: 103 Current Smokers ("Yes"), 734 Current Non-Smokers ("No")
table(factor(datatb1$a1_q14,
             levels = c(1, 2), 
             labels = c("Yes", "No")), useNA = 'ifany')
# Q15 (Linked to Q14): Number of Cigarettes smoked in the last week
# Findings: 734 NAs as expected from Current Non-Smokers, 

# Table Displaying No. of Cigarettes smoked/week and corresponding number of smokers
summary(datatb1$a1_q15)
smoking_table <- table(datatb1$a1_q15, useNA = "ifany")
smoking_df <- data.frame(
  "No. Cigarettes Smoked" = names(smoking_table),
  "Number of Smokers" = as.vector(smoking_table)
)
print(smoking_df)

# Histogram displaying distribution - potential outliers of 30 and 70, which could still logically make sense 
hist(datatb1$a1_q15, 
     main = "Distribution of Cigarettes Smoked by Current Smokers in the Last Week", 
     xlab = "Number of Cigarettes", 
     ylab = "Frequency", 
     col = "lightgreen", 
     breaks = 80)
# Boxplot of the number of cigarettes smoked - potential outliers of 30 and 70, which could still logically make sense 
boxplot(datatb1$a1_q15, 
        main = "Boxplot of Cigarettes Smoked in the Last Week", 
        ylab = "Number of Cigarettes", 
        col = "lightblue", 
        na.rm = TRUE)  # Exclude NA values

# Q16: Smoking History
# Findings: 365 have smoking history ("Yes"), 472 Do Not have Smoking History ("No")
table(factor(datatb1$a1_q16,
             levels = c(1, 2), 
             labels = c("Yes", "No")), useNA = 'ifany')

# Q17: Months/Duration Smoked before they stopped/till now
# Findings: 365 have smoking history ("Yes"), 472 Do Not have Smoking History ("No")
summary(datatb1$a1_q17)
quit_smoking_table <- table(datatb1$a1_q17, useNA = "ifany")
quit_smoking_df <- data.frame(
  "Months Smoked before you stopped" = names(quit_smoking_table),
  "Number of Smokers" = as.vector(quit_smoking_table)
)
print(quit_smoking_df)
# Histogram
hist(datatb1$a1_q17, 
     main = "Distribution of Months they smoked till now/before they stopped", 
     xlab = "Months Smoked", 
     ylab = "Frequency", 
     col = "lightgreen", 
     breaks = 50)
# Boxplot
boxplot(datatb1$a1_q17, 
        main = "Boxplot of Months they smoked till now/before they stopped", 
        ylab = "Months Smoked", 
        col = "lightblue", 
        na.rm = TRUE)  # Exclude NA values

# To examine how many smokers quit smoking
# Create a contingency table for current smoking status (a1_q14) and smoking history (a1_q16)
smoking_contingency <- table(
  Current_Smoker = factor(datatb1$a1_q14, levels = c(1, 2), labels = c("Yes", "No")),
  Smoking_History = factor(datatb1$a1_q16, levels = c(1, 2), labels = c("Yes", "No")),
  useNA = "ifany"
)
# Print the table
print(smoking_contingency)


# ------------------------------------------- Alcohol/Drinking History --------------------------------------------------------

# Check whether patient drinks alcohol
# Findings: 641 Non-Drinkers, 196 Drinkers (74 >=4x weekly, 39 2-3x weekly, 56 2-4x monthly, 27 1x per month or less)
table(factor(datatb1$a1_q18, 
             levels = c(1, 2, 3, 4, 5), 
             labels = c(">=4x per Week", "2-3x per Week", "2-4x per Month", "Once a month or less", "No")))

summary(datatb1$a1_q19___1)
# Q: Do you drink Beer?
# Findings: 718 No, 119 Yes, and all consumed in cans instead of bottles, rmajority drinking from 0.5 to 5 cans per ___? (undefined)
table(factor(datatb1$a1_q19___1,
             levels = c(0, 1), 
             labels = c("No", "Yes")))
# Number of bottles/cans drank (Survey didn't specify in weeks/months etc)
table(datatb1$a1_q19_1_bottle, useNA = 'ifany')
table(datatb1$a1_q19_1_can, useNA = 'ifany')

# Q: Do you drink Khmer Rice Wine?
# Findings: 740 No, 97 Yes, majority drinking 1-3 Glasses per ___? (undefined)
table(factor(datatb1$a1_q19___2,
             levels = c(0, 1), 
             labels = c("No", "Yes")))
# Number of Glasses drank (Survey didn't specify in weeks/months etc)
table(datatb1$a1_q19_2, useNA = 'ifany')


# Q: Do you drink Liquor?
# Findings: 837 No, 0 Yes
table(factor(datatb1$a1_q19___3,
             levels = c(0, 1), 
             labels = c("No", "Yes")))
# Number of Glasses drank (Survey didn't specify in weeks/months etc) - NA
table(datatb1$a1_q19_3, useNA = 'ifany')

# Q: Do you drink Alcopop?
# Findings: 836 No, 1 Yes, and that 1 obervation drank 6 Glasses per ___? (undefined)
table(factor(datatb1$a1_q19___4,
             levels = c(0, 1), 
             labels = c("No", "Yes")))
# Number of Glasses drank (Survey didn't specify in weeks/months etc)
table(datatb1$a1_q19_4, useNA = 'ifany')

# -------------------------------------------  Other Co-Morbidities --------------------------------------------------------
# Findings: Hypertension (163), Diabetes (87) were the 2 main known medical conditions which the TB participants also suffered from 
# 
# Condition  No Yes Total Percentage_Yes
# 1               Stroke 837   0   837           0.00
# 2        Heart Disease 812  25   837           2.99
# 3         Hypertension 674 163   837          19.47
# 4             Diabetes 750  87   837          10.39
# 5               Asthma 835   2   837           0.24
# 6         Lung Disease 816  21   837           2.51
# 7        Liver Disease 825  12   837           1.43
# 8       Mental Illness 830   7   837           0.84
# 9     Other Conditions 686 151   837          18.04
# 10 No Other Conditions 343 494   837          59.02

conditions <- c("Stroke", "Heart Disease", "Hypertension", "Diabetes", 
                "Asthma", "Lung Disease", "Liver Disease", 
                "Mental Illness", "Other Conditions", "No Other Conditions")

# Create a matrix with counts of Yes (1) and No (0) for each condition
condition_counts <- rbind(
  table(factor(datatb1$a1_q20___1, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___2, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___3, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___4, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___5, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___6, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___7, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___8, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___9, levels = c(0, 1), labels = c("No", "Yes"))),
  table(factor(datatb1$a1_q20___10, levels = c(0, 1), labels = c("No", "Yes")))
)

# Generate Summary Data Frame
summary_df <- data.frame(
  Condition = conditions,
  No = condition_counts[, "No"],
  Yes = condition_counts[, "Yes"],
  Total = rowSums(condition_counts),
  Percentage_Yes = round((condition_counts[, "Yes"] / rowSums(condition_counts)) * 100, 2)
)
print(summary_df)

# ------------------------------------------- Symptoms, Help-Seeking for TB --------------------------------------------
# This section doesn't seem very relevant for us
# Findings: Cough (600 sought help due to this symptom / 639 who had this symptom), Dyspnea (369/417), Fever (193/273), 
#           Night Sweat (153/230), Loss of Weight (138/205), Chest Pain (120/162)

# 1. Cough
# Q: Did you have Cough as a symptom?
table(factor(datatb1$a1_q28___1,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Cough as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___1,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 2. Cough with Blood
# Q: Did you have Cough with Blood as a symptom?
table(factor(datatb1$a1_q28___2,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Cough with Blood as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___2,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 3. Chest Pain
# Q: Did you have Chest Pain as a symptom?
table(factor(datatb1$a1_q28___3,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Chest Pain as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___3,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')


# 4. Dyspnea (Shortness of Breath)
# Q: Did you have Dyspnea as a symptom?
table(factor(datatb1$a1_q28___4,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Dyspnea as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___4,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 5. Fever
# Q: Did you have Fever as a symptom?
table(factor(datatb1$a1_q28___5,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Fever as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___5,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 6. Chills
# Q: Did you have Chills as a symptom?
table(factor(datatb1$a1_q28___6,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Chills as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___6,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 7. Loss of Weight
# Q: Did you have Loss of Weight as a symptom?
table(factor(datatb1$a1_q28___7,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Loss of Weight as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___7,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 8. Night Sweat
# Q: Did you have Night Sweat as a symptom?
table(factor(datatb1$a1_q28___8,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did Night Sweat as a symptom, make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___8,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')

# 9. Others
# Q: Did you have (OTHER SYMPTOMS) as a symptom?
table(factor(datatb1$a1_q28___9,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
# Q: Did (OTHER SYMPTOMS), make you seek healthcare at current TB diagnosis?
table(factor(datatb1$a1_q29___9,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
