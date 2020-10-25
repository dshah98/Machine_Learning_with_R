
# -------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv("Position_Salaries.csv")

# Selecting particular columns
dataset = dataset[2:3]

# Level is the independent variable.
# Salary is the dependent variable.

# ------------------------------------ Fitting Linear Regression to the dataset ------------------------------- #

lin_reg = lm(Salary ~  Level, data = dataset)
summary(lin_reg)

# --------------------------------- Fitting Polynomial regression to the dataset ------------------------------ #

# Creating the degree 2
dataset$Level2 = dataset$Level^2

# Creating the degree 3
dataset$Level3 = dataset$Level^3

# Creating the degree 4
dataset$Level4 = dataset$Level^4

poly_reg = lm(Salary ~ ., data = dataset)
summary(poly_reg)

# ---------------------------------- Visualising the Linear Regression results ------------------------------- # 

# Installing Package
install.packages("ggplot2")
library(ggplot2)

# Plotting
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
                colour = 'blue') + 
  ggtitle("Truth or Bluff (Linear Regression)") + 
  xlab("Level") + ylab("Salary")

# -------------------------------- Visualising the Polynomial Regression results ------------------------------ #

# Plotting
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') + 
  ggtitle("Truth or Bluff (Polynomial Regression)") + 
  xlab("Level") + ylab("Salary")

# ------------ Visualising the Regression Model results (for higher resolution and smoother curve) ------------ #

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg, newdata = data.frame(Level = x_grid, Level2 = x_grid^2, 
                                                                       Level3 = x_grid^3, Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# -------------------------------- Predictiing a new result with Linear Regression ---------------------------- #

lin_y_pred = predict(lin_reg, data.frame(Level =  6.5))
lin_y_pred

# ------------------------------ Predictiing a new result with Polynomial Regression -------------------------- #

poly_y_pred = predict(poly_reg, data.frame(Level =  6.5, Level2 = 6.5^2 , Level3 = 6.5^3, Level4 = 6.5^4))
poly_y_pred
