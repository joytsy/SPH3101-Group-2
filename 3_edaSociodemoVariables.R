# Run Scripts for data and preprocessing functions
source('1_load_data.R')
source('2_MyFunctions.R')

# Dataset
data <- process_datatb1_function(datatb1)

# -------------- EDA / Significance Testing for Categorical Variables --------------------------

# ----- Sex -----
table(data$stigma_threshold, factor(data$a1_q1,
                                    levels = c(1, 2), 
                                    labels = c("Male", "Female")))
chisq.test(data$stigma_threshold, data$a1_q1) ## Not significant --> p-value = 0.3402



# ---- Education Level ----
table(data$stigma_threshold, factor(data$a1_q5,
                                    levels = c(1, 2, 3, 4, 5), 
                                    labels = c("Primary", "Secondary", "High School", "University or Higher", "No Formal Schooling")))
chisq.test(data$stigma_threshold, data$a1_q5) ## Not significant --> p-value = 0.7299

# group into abovePrimary Yes and No
table(data$stigma_threshold, data$abovePrimary)
chisq.test(data$stigma_threshold, data$abovePrimary) ## Not significant --> p-value = 0.2429


# ---- Smoking History (so long as the individual has smoked before) ----
table(data$stigma_threshold, factor(data$a1_q16,
                                    levels = c(1, 2), 
                                    labels = c("Smoker", "Non Smoker")))
chisq.test(data$stigma_threshold, data$a1_q16) ## Not significant --> p-value = 0.3265



# ---- Alcohol Consumption ----
table(data$stigma_threshold,data$alcoholConsumer)
chisq.test(data$stigma_threshold, data$alcoholConsumer) # Not significant, p-value = 0.9414
table(data$stigma_threshold,factor(data$a1_q18, 
                                   levels = c(1, 2, 3, 4, 5), 
                                   labels = c(">=4x per Week", "2-3x per Week", "2-4x per Month", "Once a month or less", "No")))
chisq.test(data$stigma_threshold, data$a1_q18) # Not significant --> p-value = 0.4725



# ---- Marital Status -----
table(data$stigma_threshold, factor(data$a1_q4,
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Never Married", "Married", "Divorced/separated", "Widowed")))
chisq.test(data$stigma_threshold, data$a1_q4) ## Not significant --> p-value = 0.6619



# ---- Case Status ----
table(data$stigma_threshold, data$case_status)
chisq.test(data$stigma_threshold, data$case_status) # Not significant, chi-sq approx may not be appropriate
# Considering only those "completed" / "cured", Not significant, but p-value = 0.08838 which is quite small
table(data$stigma_threshold[data$case_status %in% c("completed", "cured")], data$case_status[data$case_status %in% c("completed", "cured")])
chisq.test(data$stigma_threshold[data$case_status %in% c("completed", "cured")], data$case_status[data$case_status %in% c("completed", "cured")]) 



# ---- Current Diagnosis of TB Type ----
table(data$stigma_threshold, factor(data$a1_type_tb,
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("TB Bac+", "TB Bac-", "Multidrug-resistant TB", "RR TB")))
chisq.test(data$stigma_threshold, data$a1_type_tb) # significant --> p-value = 0.0434

################## Create a stacked bar chart
data <- data %>% filter(a1_type_tb != 3)
data$a1_type_tb <- factor(data$a1_type_tb,
                          levels = c(1, 2, 4), 
                          labels = c("TB Bac+", "TB Bac-","RR TB"))
ggplot(data %>%
         group_by(a1_type_tb, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_type_tb, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Type of TB", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center title
    axis.title.x = element_text(hjust = 0.5, size = 18,margin = margin(t = 15)),  # Center x-axis title
    axis.title.y = element_text(hjust = 0.5, size = 18,margin = margin(t = 15)),  # Center y-axis title
    axis.text.x = element_text(size = 18),  # Adjust x-axis text size
    axis.text.y = element_text(size = 18),  # Adjust y-axis text size
    legend.title = element_text(size = 16),  # Legend title size and style
    legend.text = element_text(size = 16),  # Legend text size
    legend.key.size = unit(1, "cm"),  # Size of legend keys for better spacing
    plot.margin = margin(10, 20, 10, 20),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines# Adjust plot margins
  ) +
  ggtitle("Percentage of Stigma Threshold by Type of TB (Baseline)")



# ---- Province ----
table(data$stigma_threshold, factor(data$a1_prov,
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Kampong Cham", "Tboung Khmum", "Kandal", "Phnom Penh")))
chisq.test(data$stigma_threshold, data$a1_prov) ## significant --> p-value < 0.01

data$a1_prov <- factor(data$a1_prov,
                       levels = c(1, 2, 3, 4), 
                       labels = c("Kampong Cham", "Tboung Khmum", "Kandal", "Phnom Penh"))
ggplot(data %>%
         group_by(a1_prov, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_prov, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Province", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center title
    axis.title.x = element_text(hjust = 0.5, size = 18),  # Center x-axis title
    axis.title.y = element_text(hjust = 0.5, size = 18),  # Center y-axis title
    axis.text.x = element_text(angle = 30, hjust = 1,size = 18),  # Adjust x-axis text size
    axis.text.y = element_text(size = 18),  # Adjust y-axis text size
    legend.title = element_text(size = 16),  # Legend title size and style
    legend.text = element_text(size = 16),  # Legend text size
    legend.key.size = unit(1, "cm"),  # Size of legend keys for better spacing
    plot.margin = margin(10, 20, 10, 20),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Province (Baseline)")


# ---- Operational District ----
table(data$stigma_threshold, factor(data$a1_operat_dist,
                                    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                    labels = c("Stung Trang", "Kang Meas", "Suong", "Ou Raing", "Mukkampul",
                                               "Lvea Em", "Sa Ang", "Leuk Dek", "Po Sen Chey", "Sen Sok")))
chisq.test(data$stigma_threshold, data$a1_operat_dist) ## significant --> p-value < 0.01



# ---- HIV Status ----
table(data$stigma_threshold, factor(data$a1_q21,
                                    levels = c(1, 2, 3, 4), 
                                    labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose")))
chisq.test(data$stigma_threshold, factor(data$a1_q21,
                                         levels = c(1, 2, 3, 4), 
                                         labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose"))) #  Chi-squared approximation may be incorrect

# ---- Previous TB Diagnosis ----
table(data$stigma_threshold, factor(data$a1_q24,
                                    levels = c(1, 2, 3), 
                                    labels = c("Yes", "No", "Don't Know")))
chisq.test(data[data$a1_q24 %in% c(1,2), ]$stigma_threshold, data[data$a1_q24 %in% c(1,2), ]$a1_q24) ## Not significant --> p-value = 0.05858
