# ---------------------------------------------------- Importing Data ----------------------------------------- #

dataset = read.csv("Social_Network_Ad.csv")

# Selecting particular columns
dataset = dataset[, 3:5]

# ---------------------------------- Splitting the dataset into Training and Test Set ------------------------- #

library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# --------------------------------------------------- Feature Scalling ----------------------------------------- #

training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale((test_set[, 1:2]))

# ------------------------------------- Fitting Classifier to the Training Set --------------------------------- #

library(e1071)
classifier = svm(Purchased ~ ., data = training_set, type = "C-classification", kernel = "linear")
classifier

# -------------------------------------------- Predicting the Test Set result ---------------------------------- #

y_pred = predict(classifier, newdata = test_set[-3])
y_pred

# ------------------------------------------------- Confusion Matrix ------------------------------------------- #

cm = table(test_set[, 3], y_pred)
cm

# --------------------------------------- Visualising the Training Set results --------------------------------- #

# Download from the site, it will be in zip from and then install.
library(ElemStatLearn)

set = training_set

train1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
train2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(train1, train2)

colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3],
     main = 'Support Vector Machine (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(train1), ylim = range(train2))

# Regression Line
contour(train1, train2, matrix(as.numeric(y_grid), length(train1), length(train2)), add = TRUE)

# Giving Colors
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# ------------------------------------------- Visualising the Test Set results --------------------------------- #

set = test_set

test1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
test2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(test1, test2)

colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3],
     main = 'Support Vector Machine (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Colour
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
