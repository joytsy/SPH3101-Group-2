# ++++++++ BASELINE SURVEY Hypothesis Testing and Regression Models ++++++++ 

# -------- SUMMARY -------- 
# Tests used for hypothesis testing include t-test and wilcoxon for continuous variables,
# while chi-square test for categorical variables.

# Initial factors we thought would be significant in determining stigma levels: age, sex, education level, smoking, alcohol, income
# After conducting hypothesis testing, significant factors were: several tb symptoms experienced (cough, chest pain, dyspnea, weight loss, fever, night sweat),
# TB type, and Province.

# Regression Models were then fitted with statistically significant variables (after conducting hypo testing)
# and we slowly reduced the number of variables in the models until model AIC value could not be decreased further.

# After running linear regression on the significant factors, we are left with cough, chest pain, night sweat and province as significant variables for stigma scores.
# Whereas for logistic regression, we are left with cough, fever, night sweat and province as significant variables for whether one experiences high or low stigma.

# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')

# Datasets
data <- process_datatb1_function(datatb1)


# -------- Linear Regression of Stigma Scores against Significant Variables  --------
## Employ backward stepwise regression function to remove least statistically significant variables until model no longer improves.
## After removing variables with p-value > 0.05, we are left with cough, chest pain, night sweat and province as significant variables for stigma scores.

# Implement backward stepwise regression using step() to obtain best fit of model based on data
linearRegMod <- lm(data$stigma_score~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
                     data$a1_q28___5+data$a1_q28___8+factor(data$a1_type_tb)+factor(data$a1_prov))
linearRegMod_back <- step(linearRegMod, direction = "backward", scope = formula(~ .))
summary(linearRegMod_back)
confint(linearRegMod_back)
anova(linearRegMod_back)

# -------- Final Confirmed Logistic Regression of Stigma Levels against Significant Variables  --------
## Use stigma_thresNumber outcome variable for logistic regression models.
## For each model, remove variable with largest p-value and check for drop in deviance until the model no longer improve and 
## remaining variables are significant.
## After removing variables with p-value > 0.05, we are left with cough, fever, night sweat and province
# as significant variables for whether one experiences high or low stigma.
m1 <- glm(data$stigma_thresNumber~data$a1_q28___1+data$a1_q28___3+data$a1_q28___4+data$a1_q28___7+
            data$a1_q28___5+data$a1_q28___8+factor(data$a1_type_tb)+factor(data$a1_prov), family='binomial')
logRegMod_back <- step(m1, direction="backward")

summary(logRegMod_back)
# Perform Wald test
Anova(logRegMod_back, test = "Wald")
