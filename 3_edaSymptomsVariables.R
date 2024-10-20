# Run Scripts for data and preprocessing functions
source('1_load_data.R')
source('2_MyFunctions.R')

# Dataset
data <- process_datatb1_function(datatb1)

# -------------- EDA / Significance Testing for Categorical Variables --------------------------

# ----------------------------- Various TB Symptoms Contracted --------------------------------
# 1. Cough
table(factor(data$a1_q28___1,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___1,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___1) ## Significant --> p-value < 0.01

data$a1_q28___1 <- factor(data$a1_q28___1,
                          levels = c(0,1), 
                          labels = c("No cough", "Cough"))
ggplot(data %>%
         group_by(a1_q28___1, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_q28___1, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Experience of Cough during Disease", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
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
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Experience of Cough during Disease (Baseline)")



# 2. Cough with Blood
table(factor(data$a1_q28___2,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___2,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___2) ## Not significant --> p-value = 0.3462

# 3. Chest Pain
table(factor(data$a1_q28___3,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___3,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___3) ## Significant --> p-value < 0.01

data$a1_q28___3 <- factor(data$a1_q28___3,
                          levels = c(0,1), 
                          labels = c("No chest pain", "Chest pain"))
ggplot(data %>%
         group_by(a1_q28___3, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_q28___3, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Experience of Chest Pain during Disease", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
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
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Experience of Chest Pain during Disease (Baseline)")


# 4. Dyspnea (Shortness of Breath)
table(factor(data$a1_q28___4,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___4,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___4) ## Significant --> p-value < 0.01

data$a1_q28___4 <- factor(data$a1_q28___4,
                          levels = c(0,1), 
                          labels = c("No Dyspnea", "Dyspnea"))
ggplot(data %>%
         group_by(a1_q28___4, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_q28___4, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Experience of Dyspnea during Disease", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
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
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Experience of Dyspnea during Disease (Baseline)")


# 5. Fever
table(factor(data$a1_q28___5,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___5,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___5) ## Significant --> p-value < 0.01

data$a1_q28___5 <- factor(data$a1_q28___5,
                          levels = c(0,1), 
                          labels = c("No Fever", "Fever"))
ggplot(data %>%
         group_by(a1_q28___5, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_q28___5, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Experience of Fever during Disease", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
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
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Experience of Fever during Disease (Baseline)")


# 6. Chills
table(factor(data$a1_q28___6,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___6,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___6) ## Not significant --> p-value = 0.1363

# 7. Loss of Weight
table(factor(data$a1_q28___7,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___7,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___7) ## Significant --> p-value < 0.01

data$a1_q28___7 <- factor(data$a1_q28___7,
                          levels = c(0,1), 
                          labels = c("No Weight Loss", "Weight Loss"))
ggplot(data %>%
         group_by(a1_q28___7, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_q28___7, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Experience of Weight Loss during Disease", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
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
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Experience of Weight Loss during Disease (Baseline)")


# 8. Night Sweat
table(factor(data$a1_q28___8,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___8,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___8) ## Significant --> p-value < 0.01

data$a1_q28___8 <- factor(data$a1_q28___8,
                          levels = c(0,1), 
                          labels = c("No Night Sweat", "Night Sweat"))
ggplot(data %>%
         group_by(a1_q28___8, stigma_threshold) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count) * 100),
       aes(x = a1_q28___8, 
           y = percentage, 
           fill = stigma_threshold)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Low" = "#A8D5BA", "High" = "#F4A7A3")) +
  labs(x = "Experience of Night Sweat during Disease", 
       y = "Percentage",
       fill = "Stigma Threshold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Position the legend to the right
    plot.title.position = "plot",
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
    panel.grid.minor = element_blank()
  )+
  ggtitle("Percentage of Stigma Threshold by Experience of Night Sweat during Disease (Baseline)")


# 9. Other Symptoms
table(factor(data$a1_q28___9,
             levels = c(0, 1), 
             labels = c("No", "Yes")), useNA = 'ifany')
table(data$stigma_threshold, factor(data$a1_q28___9,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes")), useNA = 'ifany')
chisq.test(data$stigma_threshold, data$a1_q28___9) ## Significant --> p-value < 0.01

