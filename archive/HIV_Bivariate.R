# Note: File to be run after fu_stigma_preprocess.R
library(tidyverse)

# Examining HIV and Followup
# Create a contingency table of HIV status and Follow-up status
# Findings: Out of 7 HIV Positive at Baseline, 4 Alive and Followed-up, 1 Dead, 1 Refused to FU, 1 Lost to FU 
hiv_fu_status_contingency <- table(
  HIV_Status = factor(combined_tbl$a1_q21, 
                      levels = c(1, 2, 3, 4), 
                      labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose")),
  fu_status = factor(combined_tbl$a1_status_tb_during_fu, 
                     levels = c(1, 2, 3, 4), 
                     labels = c("Alive", "Dead", "Refused to Followup", "Lost to Followup"))
)
print(hiv_fu_status_contingency)


# Examining HIV and Stigma Score
# Table Displaying Summary Stats of Stigma Score when Grouped by HIV Status
combined_tbl %>% 
  group_by(HIV_Status = factor(combined_tbl$a1_q21, 
                               levels = c(1, 2, 3, 4), 
                               labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose"))) %>%
  summarise(
    meanScore=mean(stigma_score_baseline, na.rm=TRUE),
    minScore=min(stigma_score_baseline, na.rm=TRUE),
    maxScore=max(stigma_score_baseline, na.rm=TRUE),
    medianScore=median(stigma_score_baseline, na.rm=TRUE),
    count=n(), # number of observations
    n_missing= sum(is.na(stigma_score_baseline)) # count number of missing values (should be 38)
  )


# Boxplot of stigma scores by HIV status
boxplot(stigma_score_baseline ~ factor(a1_q21, 
                                       levels = c(1, 2, 3, 4), 
                                       labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose")),
        data = combined_tbl,
        main = "Stigma Scores at Baseline by HIV Status",
        xlab = "HIV Status",
        ylab = "Stigma Score at Baseline",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        notch = TRUE)

# Get the number of observations for each HIV status group
counts <- table(factor(combined_tbl$a1_q21, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("Positive", "Negative", "Not Sure", "I do not want to disclose")))

# Add the counts to the boxplot, slightly above the box
text(x = 1:4, 
     y = tapply(combined_tbl$stigma_score_baseline, 
                factor(combined_tbl$a1_q21, levels = c(1, 2, 3, 4)), 
                max, na.rm = TRUE) + 1,  # Position above the max score for each boxplot
     labels = paste("n =", counts), 
     cex = 0.8)


