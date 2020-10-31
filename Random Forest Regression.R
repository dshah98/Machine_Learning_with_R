# Steps;
# - Pick at random K data points from the Training set.
# - Build the Decision Tree associated to these K data points.
# - Choose the number Ntree of trees you want to build and repeat step 1 and 2.
# - For a new data point, make each one of your Ntree trees predict the value of Y to for the data point in question
# and assign the new data point the average across all of the predict Y values.

# ---------------------------------------------------- Importing Data ------------------------------------------ #

dataset = read.csv("Position_Salaries.csv")

# Selecting particular columns
dataset = dataset[2:3]

# Level is the independent variable.
# Salary is the dependent variable.

# ---------------------------------- Fitting Support Vector Regression to the dataset -------------------------- #

install.packages("randomForest")
library("randomForest")
set.seed(123)
reg = randomForest(x = dataset[1], y = dataset$Salary, ntree = 10)
summary(reg)

# -------------------------------------------- Predictiing a new result  --------------------------------------- #

y_pred = predict(reg, data.frame(Level = 6.5))
y_pred

# ------------------ Visualising the DTR Model results (for higher resolution and smoother curve) -------------- #

library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(reg, newdata = data.frame(Level = x_grid))), colour = 'blue') +
  ggtitle('Truth or Bluff (Random Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Increasing number of steps.

set.seed(123)
reg1 = randomForest(x = dataset[1], y = dataset$Salary, ntree = 100)

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(reg1, newdata = data.frame(Level = x_grid))), colour = 'blue') +
  ggtitle('Truth or Bluff (Random Tree Regression)') +
  xlab('Level') +
  ylab('Salary')
