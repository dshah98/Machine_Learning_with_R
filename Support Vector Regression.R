
# -------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv("Position_Salaries.csv")

# Selecting particular columns
dataset = dataset[2:3]

# Level is the independent variable.
# Salary is the dependent variable.

# -------------------------------- Fitting Support Vector Regression to the dataset --------------------------- #

install.packages('e1071')
library(e1071)

reg = svm(Salary ~  ., data = dataset, type = "eps-regression")
summary(reg)

# -------------------------------- Predictiing a new result with Linear Regression ---------------------------- #

y_pred = predict(reg, data.frame(Level =  6.5))
y_pred

# ------------------------------- Visualising the Support Vector Regression results --------------------------- # 

# Installing Package
install.packages("ggplot2")
library(ggplot2)

# Plotting
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(reg, newdata = dataset)),
            colour = 'blue') + 
  ggtitle("Truth or Bluff (Support Vector Regression)") + 
  xlab("Level") + ylab("Salary")
