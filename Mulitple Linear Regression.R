
# Multiple Linear Regression : y = b0 + b1*x1 + ...... + bn*xn
# Assumptions of a Linear Regression;
# - Linearity
# - Homoscedasticity
# - Multivariate Normality
# - Independence of Errors
# - Lack of Mulitcollinearity

# Building a Model;

# 1. All-in
# - One is if you have prior knowledge if you know that these exact variavle are the ones are your true 
# predictors you dont have to build anything you already know that this is the case.
# - Other one is you have to.
# - Preparing for backward elimination

# 2. Backward Elimination
# - Step 1: Select a significaance level to stay in the model(eg Significance Level = 0.05)
# - Step 2: Fit the full model with akk possible predictors.
# - Step 3: Consider the predictor with the hghest p-value, if p > sl, go to step 4, otherwise go to FIN (Keep  
# the previous mode.).
# - Step 4: Remove the predictor.
# - Step 5: Fit model without this variables.

# 3. Forward Selection
# - Step 1: Select a significaance level to enter in the model(eg Significance Level = 0.05)
# - Step 2: Fit all the simple regression model Y ~ Xn, then select the one with the lowest p-value.
# - Step 3: Keep this variable and fit all possible models with one extra predictor added to the one you 
# already have.
# - Step 4: Consider the predictor with the lowest p-value. If p < sl, go to step 3, otherwise go to FIN.

# 4. Bidirectional Elimination
# - Step 1: Select a significance level to enter and to stay in the model. (eg SLENTER = 0.05, SLSTAY = 0.05)
# - Step 2: Perfomr the next step of Forward Selection. (new variable must have p < SLENTER to enter)
# - Step 3: Perform ALL step of Backward Elemination. (old variable must have p < SLSTAY to stay)
# - Step 4: No new variable can enter and no old variabel can exit.
# - Now your model is ready.

# 5. Score Comparison/All Possible Models
# - Step 1: Select a criterion of goodness of fit. (eg Akaike criterion)
# - Step 2: Construct all possible regression model; 2^n - 1 total combination.
# - Step 3: Select the one with the best criterion

# ------------------------------------------------ Importing Data -------------------------------------------- #

Startup = read.csv("50_Startups.csv")

# ------------------------------------------ Encoding Categorical Data --------------------------------------- #

Startup$State = factor(Startup$State, levels = c("New York", "California", "Florida"),
                       labels = c(1, 2, 3))

# ------------------------- Splitting the Dataset into the Training set and Testing Set ---------------------- #

# install.packages("caTools") <----- Remove comment if not installed

library(caTools)
set.seed(123)

# In Python we put the percentage for Test Set, in R we put for Training Set.
split = sample.split(Startup$Profit, SplitRatio = 0.8)
split
# True mean observation goes to Training Set and False means observation goes to Test Set.

train_set = subset(Startup, split == TRUE)
train_set
test_set = subset(Startup, split == FALSE)
test_set

# ----------------------------- Fitting Simple Linear Regression to the Training Set ------------------------- #

reg = lm(Profit ~ ., data = train_set)
summary(reg)

# Most important things are p-value and significance level, because these help us about the statistical 
# sifnificance of the independent variable onto the dependent variable.

# The lower the p-value is and the most statistic significant independent variable is going to be.
# If the p-valu eis lower than the 5% then that means that dependent variable would be highly statistically 
# significant and more than 5% then the less it will be statistically signifiant.

# ------------------------------------------ Predicting the Test Set results --------------------------------- #

y_pred = predict(reg, newdata = test_set)
y_pred

# ------------------------------ Building the optimal model using Backward Elimination ----------------------- #

reg = lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = Startup)
summary(reg)
# remove the p-value greater than 5%

# Removing Predictor
reg = lm(Profit ~ R.D.Spend + Administration + Marketing.Spend, data = Startup)
summary(reg)

reg = lm(Profit ~ R.D.Spend + Marketing.Spend, data = Startup)
summary(reg)

