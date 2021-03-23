# PCA is one of the most used unsupervised algorithms and can be seen as the most popular dimensionality reduction 
# algorithm.

# PCA is used as
# - Noise Filtering
# - Visualization
# - Feature Extraction
# - Stock Market Predictions
# - Gene Data Analysis

# The goal of PCA is to identify and detect the correlation between variables.
# If there is a strong correlation then you could reduce the dimensionality which really PCA is intended for.

# It reduces the dimension of a d-dimensional dataset by projecting it onto a (k) - dimensional subspace 
# (where k < d)

# Main function of the PCA are;
# - Standardize the data.
# - Obtain the Eighenvector and Eigenvalues from the covariance matrix or correlation matrix, or perform Singular 
# Vector Decomposition.
# - Sort eigenvalues in descending order and choose the k eigenvector that correspond to the k largest eigenvector 
# where k is the number of dimensions of the new feature subspace ( k <= d)/.
# - Construct the projection matrix W from the selected k eigenvectors.
# - Transform the original dataset X via W to obtain a k-dimensional feature subspace Y.

# Web page : http://setosa.io/ev/principal-component-analysis

# ---------------------------------------------------- Importing Data ----------------------------------------- #

dataset = read.csv("Wine.csv")

# ---------------------------------- Splitting the dataset into Training and Test Set ------------------------- #

library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# --------------------------------------------------- Feature Scalling ----------------------------------------- #

training_set[-14] = scale(training_set[-14])
test_set[-14] = scale((test_set[-14]))

# ---------------------------------------------------- Applying PCA -------------------------------------------- #

library(caret)
library(e1071)

pca = preProcess(x = training_set[-14], method = 'pca', pcaComp = 2)

training_set = predict(pca, training_set)
training_set = training_set[c(2, 3, 1)]

test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

# ------------------------------------------- Fitting SVM to the Training Set --------------------------------- #

classifier = svm(formula = Customer_Segment ~ ., data = training_set, type = 'C-classification', kernel = 'linear')
summary(classifier)

# -------------------------------------------- Predicting the Test Set result ---------------------------------- #

# Making Vector probability prediction
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

colnames(grid_set) = c('PC1', 'PC2')

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(train1), ylim = range(train2))

# Regression Line
contour(train1, train2, matrix(as.numeric(y_grid), length(train1), length(train2)), add = TRUE)

# Giving Colours
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# ------------------------------------------- Visualising the Test Set results --------------------------------- #

set = test_set

test1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
test2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(test1, test2)

colnames(grid_set) = c('PC1', 'PC2')

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Colours
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))