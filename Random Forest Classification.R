# Steps ;
# Step 1 : Pick at random K data points from the Training set.
# Step 2 : Build the Decision Tree associated to these K data points.
# Step 3 : Choose the number Ntree of trees you want to build and repeat step 1 and 2.
# Step 4 : For a new data point, make each one of your ntree tree predict the category to which the dagta point belongs ans 
# assign the new data point to the category that wins the majority vote.

# ---------------------------------------------------- Importing Data ------------------------------------------ #

dataset = read.csv('Social_Network_Ad.csv')

# Selecting particular columns
dataset = dataset[3:5]

# ---------------------------------------- Encoding the target feature as factor ------------------------------- #

dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# ---------------------------------- Splitting the dataset into Training and Test Set ------------------------- #

# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# --------------------------------------------------- Feature Scalling ----------------------------------------- #

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# ------------------------------------- Fitting Classifier to the Training Set --------------------------------- #

library(randomForest)
classifier = randomForest(x = training_set[-3], y = training_set$Purchased, ntree = 10)
summary(classifier)

# -------------------------------------------- Predicting the Test Set result ---------------------------------- #

y_pred = predict(classifier, newdata = test_set[-3])
y_pred

# ------------------------------------------------- Confusion Matrix ------------------------------------------- #

cm = table(test_set[, 3], y_pred)
cm

# --------------------------------------- Visualising the Training Set results --------------------------------- #

library(ElemStatLearn)

set = training_set
train1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
train2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(train1, train2)

colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set, type = 'class')

# Plotting
plot(set[, -3],
     main = 'Random Forest Classification (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(train1), ylim = range(train2))

# Regression Line
contour(train1, train2, matrix(as.numeric(y_grid), length(train1), length(train2)), add = TRUE)

# Giving Colour
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# ------------------------------------------- Visualising the Test Set results --------------------------------- #

library(ElemStatLearn)

set = test_set

test1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
test2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(test1, test2)

colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set, type = 'class')

# Plotting
plot(set[, -3], main = 'Random Forest Classification (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Colour
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
