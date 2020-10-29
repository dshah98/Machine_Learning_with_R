
# ---------------------------------------------------- Importing Data ------------------------------------------ #

dataset = read.csv("Position_Salaries.csv")

# Selecting particular columns
dataset = dataset[2:3]

# Level is the independent variable.
# Salary is the dependent variable.

# ---------------------------------- Fitting Support Vector Regression to the dataset -------------------------- #

library(rpart)
reg = rpart(Salary ~. , 
            data = dataset)
summary(reg)

# -------------------------------------------- Predictiing a new result  --------------------------------------- #

y_pred = predict(reg, data.frame(Level = 6.5))
y_pred

# --------------------------------- Visualising the Decision Tree Regression results --------------------------- # 

library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(reg, newdata = dataset)), colour = 'blue') + 
  ggtitle("Truth or Bluff (Decision Tree Regression)") + 
  xlab("Position Level") +
  ylab("Salaray")

# We got the straight line because we didnt apply feature scaling in our dataset, but we dont need to appply 
# feature scaling because it is based on Euclidean distance and decision tree regression are based on condition
# on the independent variable.

# So for making a better visualisation there a function in rpart which can help in splitting the dataset.

library(rpart)
reg_1 = rpart(Salary ~. , 
            data = dataset, 
            control = rpart.control(minsplit = 1))
summary(reg)

library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(reg_1, newdata = dataset)), colour = 'blue') + 
  ggtitle("Truth or Bluff (Decision Tree Regression)") + 
  xlab("Position Level") +
  ylab("Salaray")

# This is a new regression which is non-linear and non-continuous regression model.

# ------------------ Visualising the DTR Model results (for higher resolution and smoother curve) -------------- #

library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(reg_1, newdata = data.frame(Level = x_grid))), colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')
