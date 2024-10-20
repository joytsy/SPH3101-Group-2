# Run Scripts for data and preprocessing functions
source('load_data.R')
source('MyFunctions.R')

# Dataset
data <- process_datatb1_function(datatb1)

# -------------- EDA / Significance Testing for Continuous Variables --------------------------

# Age - Normally Distributed
summary(data$a1_q3)
hist(data$a1_q3, main = "Histogram of Participant Age (Baseline)")
boxplot(data$a1_q3, main = "Boxplot of Participant Age (Baseline)")
# T-test for age (since normally distributed)
## Not significant --> p-value = 0.3814
t.test(data$a1_q3[data$stigma_threshold %in% "High"], data$a1_q3[data$stigma_threshold %in% "Low"])

# Income 
summary(data$a1_q7)
boxplot(data$a1_q7, main = "Boxplot of Average Family Monthly Income (Baseline)")
hist(data$a1_q7, main = "Histogram of Average Family Monthly Income (Baseline)")
# Wilcox Test for income (since right-skewed distribution)
## Not significant --> p-value = 0.3361
wilcox.test(data$a1_q7[data$stigma_threshold %in% "High"], data$a1_q7[data$stigma_threshold %in% "Low"])

# Distance to Nearest Health Facility
summary(data$a1_q12)
hist(data$a1_q12, breaks = 40, main = "Histogram of Distance to Nearest Health Facilities (Baseline)")
boxplot(data$a1_q12,main = "Boxplot of Distance to Nearest Health Facilities (Baseline)")
# T-test for distance to nearest health facility
## Not significant --> p-value = 0.1377
t.test(data$a1_q12[data$stigma_threshold %in% "High"], data$a1_q12[data$stigma_threshold %in% "Low"])

# Time to travel to nearest facility
summary(data$a1_q13)
hist(data$a1_q13, breaks = 30, main = "Histogram of Travel Time to Nearest Health Facilities (Baseline)")
boxplot(data$a1_q13,main = "Boxplot of Travel Time to Nearest Health Facilities (Baseline)")
# T-test for time to travel to nearest facility
## Not significant --> p-value = 0.7969
t.test(data$a1_q13[data$stigma_threshold %in% "High"], data$a1_q13[data$stigma_threshold %in% "Low"])

# Treatment Duration
summary(as.numeric(data$treatmentDuration))
hist(as.numeric(data$treatmentDuration), breaks = 30)
boxplot(as.numeric(data$treatmentDuration), breaks = 30)
# T-test for Treatment Duration
## Not significant --> p-value = 0.1617
t.test(as.numeric(data$treatmentDuration)[data$stigma_threshold %in% "High"], 
       as.numeric(data$treatmentDuration)[data$stigma_threshold %in% "Low"])
