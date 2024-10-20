# Run Scripts for data and preprocessing functions
source('load_data.R')
source('MyFunctions.R')

# Dataset
data <- process_datatb1_function(datatb1)

# -------------- EDA / Significance Testing for Categorical Variables --------------------------

# ----------------------------- Various Other Medical Conditions --------------------------------
# Stroke
table(factor(data$a1_q20___1, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___1, levels = c(0, 1), labels = c("No", "Yes")))
# chisq.test(data$stigma_threshold, data$a1_q20___1) # Separation Error

# Heart Disease
table(factor(data$a1_q20___2, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___2, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___2) # Not Significant, p-value = 1

# Hypertension
table(factor(data$a1_q20___3, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___3, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___3) # Not Significant, p-value = 0.8627

# Diabetes
table(factor(data$a1_q20___4, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___4, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___4) # Not Significant, p-value = 0.3475

# Asthma
table(factor(data$a1_q20___5, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___5, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___5) # Separation Error, p-value = 0.4362

# Lung Disease
table(factor(data$a1_q20___6, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___6, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___6) # Not Significant, p-value = 0.8191

# Liver Disease
table(factor(data$a1_q20___7, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___7, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___7) # Not Significant, p-value = 1

# Mental Illness
table(factor(data$a1_q20___8, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___8, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___8) # Separation Issue, p-value = 0.1648

# Other Conditions Diagnosed
table(factor(data$a1_q20___9, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___9, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___9) # Not Significant, p-value = 0.3033

# No Other Conditions - 'Yes' means no other conditions, 'No' means individual has one of the above conditions
table(factor(data$a1_q20___10, levels = c(0, 1), labels = c("No", "Yes")))
table(data$stigma_threshold, factor(data$a1_q20___10, levels = c(0, 1), labels = c("No", "Yes")))
chisq.test(data$stigma_threshold, data$a1_q20___10) # Not Significant, p-value = 1
