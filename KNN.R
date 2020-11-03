
# KNN is used to identify the new observation that in which category it will be assigned.

# Steps;
# Step1 : Choose the number K of neighbors. (defualt value of K is 5)
# Step2 : Take the K nearest neighbors of the new data point, acoording to the Euclidean distance.
# Step3 : Among these K neighbors, count the number of data point in each category.
# Step4 : Assign the new data point to the category where you counted the most neighbors

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

library(class)
y_pred = knn(train = training_set[, -3], test = test_set[, -3], cl = training_set[, 3], k = 5)
y_pred

# -------------------------------------------- Predicting the Test Set result ---------------------------------- #

prob_pred = predict(classifier, type = "response", newdata = test_set[-3])
prob_pred

# Making Vector probability prediction
y_pred = ifelse(prob_pred > 0.5, 1, 0)
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

y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)

# Plotting
plot(set[, -3],
     main = 'KNN Model (Training set)',
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

y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)

# Plotting
plot(set[, -3],
     main = 'KNN Model (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Colour
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
