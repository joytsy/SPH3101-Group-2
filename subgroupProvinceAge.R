#####################################################################################
## PRECONDITIONS: Ran 'processBeforeSubgroup.R' or relevant code which:
## - Loaded required libraries
## - Loaded both DFs: datatb2_followup and baseline_followup_complete ##
#####################################################################################

################################################################
## Subgroup Analysis on stigma_scores for Province over time) ##
################################################################
# correlation between province and operational district
test <- table(as_factor(datatb1$a1_operat_dist), as_factor(datatb1$a1_prov))
chisq.test(test) # statistically significant --> p-value < 0.01

# head(test)

## ---------------- Kampong Cham ----------------
# get dataset
kampCham_baseline <- baseline_followup_complete %>%
  filter(a1_prov==1) # filter for Kampong Cham (label=1)

kampCham_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% kampCham_baseline$a1_record_id,]

# summary
summary(kampCham_baseline$stigma_score)
summary(kampCham_followup$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(kampCham_baseline, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Kampong Cham (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(kampCham_followup, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Kampong Cham (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(kampCham_baseline$stigma_score)
shapiro.test(kampCham_followup$stigma_score)

# Since not normally distributed, use wilcoxon to compare the sample at different time frames
wilcox.test(kampCham_baseline$stigma_score, kampCham_followup$stigma_score, paired=TRUE)
boxplot(kampCham_baseline$stigma_score, kampCham_followup$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Kampong Cham")



## ---------------- Tboung Khmum ----------------
# get dataset
tboung_baseline <- baseline_followup_complete %>%
  filter(a1_prov==2) # filter for Tboung Khmum (label=2)

tboung_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% tboung_baseline$a1_record_id,]

# summary
summary(tboung_baseline$stigma_score)
summary(tboung_followup$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(tboung_baseline, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Tboung Khmum (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(tboung_followup, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Tboung Khmum (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(tboung_baseline$stigma_score)
shapiro.test(tboung_followup$stigma_score)

# Since not normally distributed, use wilcoxon to compare the sample at different time frames
wilcox.test(tboung_baseline$stigma_score, tboung_followup$stigma_score, paired=TRUE)
boxplot(tboung_baseline$stigma_score, tboung_followup$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Tboung Khmum")



## ---------------- Kandal ----------------
# get dataset
kandal_baseline <- baseline_followup_complete %>%
  filter(a1_prov==3) # filter for Kandal (label=3)

kandal_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% kandal_baseline$a1_record_id, ]

# summary
summary(kandal_baseline$stigma_score)
summary(kandal_followup$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(kandal_baseline, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Kandal (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(kandal_followup, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Kandal (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(kandal_baseline$stigma_score)
shapiro.test(kandal_followup$stigma_score)

# Since not normally distributed, use wilcoxon to compare the sample at different time frames
wilcox.test(kandal_baseline$stigma_score, kandal_followup$stigma_score, paired=TRUE)
boxplot(kandal_baseline$stigma_score, kandal_followup$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Kandal")



## ---------------- Phnom Penh ----------------
# get dataset
phnom_baseline <- baseline_followup_complete %>%
  filter(a1_prov==4) # filter for Phnom Penh (label=4)

phnom_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% phnom_baseline$a1_record_id, ]

# summary
summary(phnom_baseline$stigma_score)
summary(phnom_followup$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(phnom_baseline, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Phnom Penh (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(phnom_followup, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Phnom Penh (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(phnom_baseline$stigma_score)
shapiro.test(phnom_followup$stigma_score)

# Since stigma scores in both time frames likely normally distributed, use t-test to compare the sample
t.test(phnom_baseline$stigma_score, phnom_followup$stigma_score, paired=TRUE)
boxplot(phnom_baseline$stigma_score, phnom_followup$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Phnom Penh")



##################################################################
## Subgroup Analysis on stigma_scores for Age Groups over time  ##
##################################################################

## ---------- Data Preprocessing for Age Groups ----------
# Based on Teo et al. paper, split 621 individuals into following age groups: 18-24, 25-34, 35-44, 45-54, 55-64, >=65
baseline_followup_complete_ageGroup <- baseline_followup_complete %>% 
  mutate(age_group = cut(a1_q3, # ages
                         breaks = c(18, 25, 35, 45, 55, 65, 100), # define bin edges
                         labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), # new age group labels
                         right = FALSE # group does not include upper bound of interval
                         ),
         .after = a1_q3)# create column after a1_q3 for cross-checking

# get count of each age group
# age_group count
# <fct>     <int>
# 1 18-24         7
# 2 25-34        28
# 3 35-44        50
# 4 45-54       127
# 5 55-64       178
# 6 65+         231
baseline_followup_complete_ageGroup %>% 
  group_by(age_group) %>% 
  summarise(count=n())

## ---------------- Ages 18-24 ----------------
# get dataset
baseline_18to24 <- baseline_followup_complete_ageGroup %>%
  filter(age_group=="18-24")

followup_18to24 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_18to24$a1_record_id, ]

# summary
summary(baseline_18to24$stigma_score)
summary(followup_18to24$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_18to24, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 18-24 (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_18to24, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 18-24 (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_18to24$stigma_score)
shapiro.test(followup_18to24$stigma_score)

# Since stigma scores in both time frames likely normally distributed, use t-test to compare the sample
t.test(baseline_18to24$stigma_score, followup_18to24$stigma_score, paired=TRUE)
boxplot(baseline_18to24$stigma_score, followup_18to24$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 18-24")



## ---------------- Ages 25-34 ----------------
# get dataset
baseline_25to34 <- baseline_followup_complete_ageGroup %>%
  filter(age_group=="25-34")

followup_25to34 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_25to34$a1_record_id, ]

# summary
summary(baseline_25to34$stigma_score)
summary(followup_25to34$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_25to34, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 25-34 (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_25to34, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 25-34 (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_25to34$stigma_score)
shapiro.test(followup_25to34$stigma_score)

# Since stigma scores in both time frames likely normally distributed, use t-test to compare the sample
t.test(baseline_25to34$stigma_score, followup_25to34$stigma_score, paired=TRUE)
boxplot(baseline_25to34$stigma_score, followup_25to34$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 25-34")



## ---------------- Ages 35-44 ----------------
# get dataset
baseline_35to44 <- baseline_followup_complete_ageGroup %>%
  filter(age_group=="35-44")

followup_35to44 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_35to44$a1_record_id, ]

# summary
summary(baseline_35to44$stigma_score)
summary(followup_35to44$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_35to44, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 35-44 (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_35to44, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 35-44 (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_35to44$stigma_score)
shapiro.test(followup_35to44$stigma_score)

# Since stigma scores in follow-up likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_35to44$stigma_score, followup_35to44$stigma_score, paired=TRUE)
boxplot(baseline_35to44$stigma_score, followup_35to44$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 35-44")



## ---------------- Ages 45-54 ----------------
# get dataset
baseline_45to54 <- baseline_followup_complete_ageGroup %>%
  filter(age_group=="45-54")

followup_45to54 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_45to54$a1_record_id, ]

# summary
summary(baseline_45to54$stigma_score)
summary(followup_45to54$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_45to54, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 45-54 (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_45to54, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 45-54 (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_45to54$stigma_score)
shapiro.test(followup_45to54$stigma_score)

# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_45to54$stigma_score, followup_45to54$stigma_score, paired=TRUE)
boxplot(baseline_45to54$stigma_score, followup_45to54$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 45-54")



## ---------------- Ages 55-64 ----------------
# get dataset
baseline_55to64 <- baseline_followup_complete_ageGroup %>%
  filter(age_group=="55-64")

followup_55to64 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_55to64$a1_record_id, ]

# summary
summary(baseline_55to64$stigma_score)
summary(followup_55to64$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_55to64, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 55-64 (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_55to64, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 55-64 (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_55to64$stigma_score)
shapiro.test(followup_55to64$stigma_score)

# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_55to64$stigma_score, followup_55to64$stigma_score, paired=TRUE)
boxplot(baseline_55to64$stigma_score, followup_55to64$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 55-64")



## ---------------- Ages >=65 ----------------
# get dataset
baseline_65AndAbove <- baseline_followup_complete_ageGroup %>%
  filter(age_group=="65+")

followup_65AndAbove <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_65AndAbove$a1_record_id, ]

# summary
summary(baseline_65AndAbove$stigma_score)
summary(followup_65AndAbove$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_65AndAbove, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 65+ (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_65AndAbove, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 65+ (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_65AndAbove$stigma_score)
shapiro.test(followup_65AndAbove$stigma_score)

# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_65AndAbove$stigma_score, followup_65AndAbove$stigma_score, paired=TRUE)
boxplot(baseline_65AndAbove$stigma_score, followup_65AndAbove$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 65+")


#########################################################################
## Subgroup Analysis on stigma_scores for <55 and >=55 y.o. over time) ##
#########################################################################

## ---------- Data Preprocessing for Age Groups ----------
# Based on Cambodia's definition of older population,
# split 621 individuals into following age groups: < 55 and >= 55
baseline_followup_complete_oldYoung <- baseline_followup_complete %>% 
  mutate(older = case_when(
    a1_q3 >= 55 ~ "Yes",  # Categorize as Yes if individual is 55 and above
    a1_q3 < 55 ~ "No",  # Categorize as No if individual is below 55
    ),
    .after = a1_q3
  )

# get count of each age group
# older count
# <chr> <int>
# 1 No      212
# 2 Yes     409
baseline_followup_complete_oldYoung %>% 
  group_by(older) %>% 
  summarise(count=n())

## --------- Overall Visualisation in Baseline of the 2 groups ---------
boxplot(baseline_followup_complete_oldYoung$stigma_score[baseline_followup_complete_oldYoung$older=="No"], 
        baseline_followup_complete_oldYoung$stigma_score[baseline_followup_complete_oldYoung$older=="Yes"],
        names=c("Younger than 55", "55 and above"),
        main="Boxplot of Stigma Scores of Age Groups at Baseline")

## ---------------- Ages <55 ----------------
# get dataset
baseline_below55 <- baseline_followup_complete_oldYoung[baseline_followup_complete_oldYoung$older=="No",]

followup_below55 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_below55$a1_record_id, ]

# summary
summary(baseline_below55$stigma_score)
summary(followup_below55$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_below55, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages Below 55 (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_below55, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages Below 55 (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_below55$stigma_score)
shapiro.test(followup_below55$stigma_score)

# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_below55$stigma_score, followup_below55$stigma_score, paired=TRUE)
boxplot(baseline_below55$stigma_score, followup_below55$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group Below 55")


## ---------------- Ages >=55 ----------------
# get dataset
baseline_55AndAbove <- baseline_followup_complete_oldYoung[baseline_followup_complete_oldYoung$older=="Yes",]

followup_55AndAbove <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_55AndAbove$a1_record_id, ]

# summary
summary(baseline_55AndAbove$stigma_score)
summary(followup_55AndAbove$stigma_score)

## Visualise distributions
# Visualize distributions of stigma_score at baseline 
ggplot(baseline_55AndAbove, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 55 and Above (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up
ggplot(followup_55AndAbove, aes(x = stigma_score)) + 
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Ages 55 and Above (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_55AndAbove$stigma_score)
shapiro.test(followup_55AndAbove$stigma_score)

# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_55AndAbove$stigma_score, followup_55AndAbove$stigma_score, paired=TRUE)
boxplot(baseline_55AndAbove$stigma_score, followup_55AndAbove$stigma_score, names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Subgroup in Age Group 55 and Above")
