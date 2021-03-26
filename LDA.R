# LDA is used as a dimensionality reduction technique.
# It is used in the preprocessing step for pattern classification.
# LDA has the goal to project a dataset onto a lower-dimensional space.
# LDA differs to PCA because in addition to finding the component axises with LDA we are interested in the axes 
# that maximize the separation between multiple classes.
# The goal of LDA is to project a feature space (a dataset n-dimensional samples) onto a small subspace 
# k(where k <= n -1) while maintaining the class discriminatory information.
# Both PCA and LDA are linear transforamation techiques used for dimensional reductions. PCA is described as 
# unsupervised but LDA is supervised because of the relation to the dependent variable.
# Additional Information : https://sebastianraschka.com/Articles/2014_python_lda.html

# The main 5 steps for LDA;
# - Compute the d-dimensional mean vectors for the different classes from the datasets.
# - Compute the scatter matrices (in-between class and within-class scatter matrix)
# - Computer the eigenvector and corresponding eigenvalues for the scatter matrices.
# - Sort the eigenvectors by decreasing eigenvalues and choose k eigenvectors with the largest eigenvalues to for, 
# a (d * k) dimensional matrix W (where every column represents an eigenvector)
# - Use this (d * k) eigenvector matrix to tranform the sample onto the new subspace. This can be summarized by the 
# matrix multiplication : Y = Y * W (where X is an n * d-dimensional matrix representing the n samples, and y are the transformed n * k-dimensional samples in the new subspace.)

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

# ---------------------------------------------------- Applying LDA ------------------------------------------- #

library(MASS)

lda = lda(formula = Customer_Segment ~ ., data = training_set)

training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(5, 6, 1)]

test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(5, 6, 1)]

# ------------------------------------------- Fitting SVM to the Training Set --------------------------------- #

classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
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

X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('x.LD1', 'x.LD2')

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))

# Regression Line
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

# Giving Color
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# ------------------------------------------- Visualising the Test Set results --------------------------------- #

library(ElemStatLearn)

set = test_set

test1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
test2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(test1, test2)

colnames(grid_set) = c('x.LD1', 'x.LD2')

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Color
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))