# ++++++++  Regression Coefficients and Confidence Intervals at each step of Stepwise Regression ++++++++ 

# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')

# Datasets
data <- process_datatb1_function(datatb1)

# -------- Linear Regression of Stigma Scores against Significant Variables  --------
## Employ backward stepwise regression function to remove least statistically significant variables until model no longer improves.
## After removing variables with p-value > 0.05, we are left with cough, chest pain, night sweat and province as significant variables for stigma scores.

# For Reference: Original Implementation of backward stepwise regression using step() to obtain best fit of model based on data
linearRegMod <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
                     data$a1_q28___5+data$a1_q28___8+factor(data$a1_type_tb)+factor(data$a1_prov))
linearRegMod_back <- step(linearRegMod, direction = "backward", scope = formula(~ .))
summary(linearRegMod_back)
confint(linearRegMod_back)
anova(linearRegMod_back)

# To obtain coefficients and confidence intervals for models for each step of stepwise regression
# Mod1: All Predictors
linearRegMod1 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
                     data$a1_q28___5+data$a1_q28___8+factor(data$a1_type_tb)+factor(data$a1_prov))
summary(linearRegMod1)
confint(linearRegMod1)

# Mod2: Removed TB Type:
linearRegMod2 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
                      data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov))
summary(linearRegMod2)
confint(linearRegMod2)

# Mod3: Removed Weight Loss
linearRegMod3 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+
                      data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov))
summary(linearRegMod3)
confint(linearRegMod3)

# Mod4: Removed Dyspnea
linearRegMod4 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+
                      data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov))
summary(linearRegMod4)
confint(linearRegMod4)

# Mod5 (Final): Removed Fever
linearRegMod5 <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+
                      data$a1_q28___8+factor(data$a1_prov))
summary(linearRegMod5)
confint(linearRegMod5)

# -------- Final Confirmed Logistic Regression of Stigma Levels against Significant Variables  --------
## Use stigma_thresNumber outcome variable for logistic regression models.
## For each model, remove variable with largest p-value and check for drop in deviance until the model no longer improve and 
## remaining variables are significant.
## After removing variables with p-value > 0.05, we are left with cough, fever, night sweat and province
# as significant variables for whether one experiences high or low stigma.

# For Reference: Original Implementation of Backwards Stepwise Regression for Logistic Regression
m1 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
            data$a1_q28___5+data$a1_q28___8+factor(data$a1_type_tb)+factor(data$a1_prov), family='binomial')
logRegMod_back <- step(m1, direction="backward")
summary(logRegMod_back)
exp(cbind(Odds_Ratio = coef(logRegMod_back), confint(logRegMod_back)))
# Perform Wald test
Anova(logRegMod_back, test = "Wald")


# To obtain coefficients and confidence intervals for models for each step of stepwise regression
# Mod1: All Predictors
logRegMod1 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
            data$a1_q28___5+data$a1_q28___8+factor(data$a1_type_tb)+factor(data$a1_prov), family='binomial')
summary(logRegMod1)
exp(cbind(Odds_Ratio = coef(logRegMod1), confint(logRegMod1)))

# Mod2: Removed TB Type
logRegMod2 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
                    data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov), family='binomial')
summary(logRegMod2)
exp(cbind(Odds_Ratio = coef(logRegMod2), confint(logRegMod2)))

# Mod3: Removed Dyspnea
logRegMod3 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___7+
                    data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov), family='binomial')
summary(logRegMod3)
exp(cbind(Odds_Ratio = coef(logRegMod3), confint(logRegMod3)))

# Mod4: Removed Weight Loss
logRegMod4 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+
                    data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov), family='binomial')
summary(logRegMod4)
exp(cbind(Odds_Ratio = coef(logRegMod4), confint(logRegMod4)))

# Mod5 (Final): Removed Chest Pain
logRegMod5 <- glm(data$stigma_thresNumber~data$a1_q28___1+
                    data$a1_q28___5+data$a1_q28___8+factor(data$a1_prov), family='binomial')
summary(logRegMod5)
exp(cbind(Odds_Ratio = coef(logRegMod5), confint(logRegMod5)))