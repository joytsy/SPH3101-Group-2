# 1. Load relevant libraries and date files
source('1_load_data.R')

# 2. Preprocessing
# load data manipulation and processing functions and remove outliers
source('2_MyFunctions.R')
# load pre-processing functions and filtered datasets for subgroup analysis
source('2_processBeforeSubgroup.R')

# 3. Conduct EDA/Bivariate analysis
source('3_edaContinuousVariables.R')
source('3_edaSociodemoVariables.R')
source('3_edaSymptomsVariables.R')
source('3_edaMedicalConditionsVariables.R')

# 4. Run Linear and Logistic Regression
source('4_baselineHypoTestAndRegModels.R')

# 5. Analysis of Stigma Scores Over Time and Relevant Plot
source('5_stigmaScoresOverTimeHypoTest.R')
source('5_stigmaScoresComparisonPlot.R')

# 6. Perform Subgroup Analysis for the various factors
source('6_subgroupAgeGroups.R')
source('6_subgroupCaseStatus.R')
source('6_subgroupChestPain.R')
source('6_subgroupCough.R')
source('6_subgroupFever.R')
source('6_subgroupNightSweat.R')
source('6_subgroupProvince.R')
source('6_subgroupSmokingStatus.R')
source('6_subgroupTbtype.R')
source('6_subgroupWeightLoss.R')

# 7. Load Violin Plots for subgroup analysis
source('7_violinPlotSubgroup2Levels.R')
source('7_violinPlotProvSubgroup.R')

# FYI: The remaining R scripts that are no longer not being utilised are in the 'archive' folder