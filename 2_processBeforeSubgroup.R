# Run Scripts for data and preprocessing functions
source('1_load_data.R')
source('2_MyFunctions.R')

# Processed Datasets
datatb1 <- process_datatb1_function(datatb1)
datatb2 <- process_datatb2_function(datatb2)

####################################################################
# Final Filtered Datasets for all 621 Participants who followed up #
####################################################################
# Convert the labelled variable for status during follow-up to numeric
datatb2 <- datatb2 %>%
  mutate(a1_status_tb_during_fu = as.numeric(a1_status_tb_during_fu))

# Followup Dataset for those 621 who completed followup
datatb2_followup <- datatb2 %>%
  filter(a1_status_tb_during_fu %in% 1)

## Baseline Dataset for the 621 participants who completed followup
baseline_followup_complete <- datatb1[datatb1$a1_record_id %in% datatb2_followup$a1_record_id, ]


################################################################
# Filtered Datasets for all Participants who did not follow up #
################################################################
# filtered follow-up dataset for participants who did not follow up (dead/loss/refuse) to get their a1_record_id
# all dnf
datatb2_no_followup <- datatb2 %>%
  filter(a1_status_tb_during_fu %in% c(2, 3, 4))
# refuse dnf
datatb2_refuse_to_followup_stigma <- datatb2 %>%
  filter(a1_status_tb_during_fu == 3)
# loss dnf
datatb2_loss_to_followup_stigma <- datatb2 %>%
  filter(a1_status_tb_during_fu == 4)

## results from baseline for participants who did not follow up 
baseline_dnf <- datatb1[datatb1$a1_record_id %in% datatb2_no_followup$a1_record_id, ]
baseline_refuse_followup <- datatb1[datatb1$a1_record_id %in% datatb2_refuse_to_followup_stigma$a1_record_id, ]
baseline_loss_followup <- datatb1[datatb1$a1_record_id %in% datatb2_loss_to_followup_stigma$a1_record_id, ]


####################################################
# Filtered Datasets for <55 and >= 55 yo Age group #
####################################################
# Based on Cambodia's definition of older population,
# split 621 individuals into following age groups: < 55 and >= 55
baseline_followup_complete_oldYoung <- baseline_followup_complete %>% 
  mutate(older = case_when(
    a1_q3 >= 55 ~ "Yes",  # Categorize as Yes if individual is 55 and above
    a1_q3 < 55 ~ "No",  # Categorize as No if individual is below 55
  ),
  .after = a1_q3
  )