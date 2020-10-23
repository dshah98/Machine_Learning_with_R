
# SLR: y = b0+ b1*x1
# y is Dependent Variable, b0 is Constant, b1 is Coefficient, x1 us Independent Variable.

# Constant is the point where the line crosses the vertical axis.
# B1 is the Slope of the line.

# ------------------------------------------------ Importing Data -------------------------------------------- #

Salary_Data = read.csv("Salary_Data.csv")

# ------------------------- Splitting the Dataset into the Training set and Testing Set ---------------------- #

# install.packages("caTools") <----- Remove comment if not installed

library(caTools)
set.seed(123)

# In Python we put the percentage for Test Set, in R we put for Training Set.
split = sample.split(Salary_Data$Salary, SplitRatio = 0.8)
split
# True mean observation goes to Training Set and False means observation goes to Test Set.

train_set = subset(Salary_Data, split == TRUE)
train_set
test_set = subset(Salary_Data, split == FALSE)
test_set

# ----------------------------- Fitting Simple Linear Regression to the Training Set ------------------------- #

reg = lm(Salary ~ YearsExperience, data = train_set)
summary(reg)

# Most important things are p-value and significance level, because these help us about the statistical 
# sifnificance of the independent variable onto the dependent variable.

# The lower the p-value is and the most statistic significant independent variable is going to be.
# If the p-valu eis lower than the 5% then that means that dependent variable would be highly statistically 
# significant and more than 5% then the less it will be statistically signifiant.

# ------------------------------------------ Predicting the Test Set results ---------------------------------- #

y_pred = predict(reg, newdata = test_set)
y_pred

# ------------------------------------------- Visualising the Training Set ------------------------------------ #

# install.packages("ggplot2") <----- Remove comment if not installed
library(ggplot2)

ggplot() + 
  geom_point(aes(x = train_set$YearsExperience, y = train_set$Salary),
             colour = "red") +
  geom_line(aes(x = train_set$YearsExperience, y = predict(reg, newdata = train_set)),
            color = "blue") + 
  ggtitle("Salary vs Experience (Trainging Set)") + 
  xlab("Years of Experience") + 
  ylab("Salary")

# --------------------------------------------- Visualising the Test Set -------------------------------------- #

ggplot() + 
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = "red") +
  geom_line(aes(x = train_set$YearsExperience, y = predict(reg, newdata = train_set)),
            color = "blue") + 
  ggtitle("Salary vs Experience (Test Set)") + 
  xlab("Years of Experience") + 
  ylab("Salary")
