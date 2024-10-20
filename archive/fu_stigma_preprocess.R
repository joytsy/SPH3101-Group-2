# Data Processing
# source("load_data.R")
load('data/datatb1.rdata')
load('data/datatb2.rdata')

# Combine Dataframes
combined_tbl <- merge(datatb1, datatb2, by = "a1_record_id")

# -------------------------------------------------------------------------------------------
# Check on Status of whether patient was followed up
# Findings: Alive: 621, Dead: 30, Refuse to followup: 64, Lost to Followup: 122
table(factor(combined_tbl$a1_status_tb_during_fu, 
             levels = c(1, 2, 3, 4), 
             labels = c("Alive", "Dead", "Refused to Followup", "Lost to Followup")))

# Add new variable for to determine those not followed up in second survey
# Findings:  Of the 837 participants, 216 Not-Followed Up, and 621 Followed up
combined_tbl$followed_up <- ifelse(combined_tbl$a1_status_tb_during_fu == 1,
                                   'Followed Up', 'Not Followed Up'
)
table(combined_tbl$followed_up)


# -------------------------------------------------------------------------------------------
# Add new Variable for Stigma Total Score - Baseline
combined_tbl$stigma_score_baseline <- rowSums(combined_tbl[, colnames(combined_tbl) %in% c(paste0('a1_q', 30:41))], na.rm = TRUE)
summary(combined_tbl$stigma_score_baseline)
table(combined_tbl$stigma_score_baseline, useNA = 'ifany')  

# Add new variable for Stigma Total Score - Followup Survey
combined_tbl$stigma_score_fu <- rowSums(combined_tbl[, colnames(combined_tbl) %in% c(paste0('a1_q', 7:18, '_fu'))], na.rm = TRUE)
summary(combined_tbl$stigma_score_fu)
table(combined_tbl$stigma_score_fu, useNA = 'ifany')  

# Add new variable to calculate stigma score difference for Followed-Up Observations
# Calculate stigma score difference only for those who are followed up
combined_tbl$stigma_score_diff <- ifelse(
  combined_tbl$followed_up == "Followed Up", 
  combined_tbl$stigma_score_fu - combined_tbl$stigma_score_baseline, 
  NA
)
# Check the summary of the new variable
table(combined_tbl$stigma_score_diff, useNA = "ifany")

