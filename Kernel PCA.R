
# ---------------------------------------------------- Importing Data ----------------------------------------- #

dataset = read.csv('Social_Network_Ad.csv')

# Selecting particular column
dataset = dataset[, 3:5]

# ---------------------------------- Splitting the dataset into Training and Test Set ------------------------- #

# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# --------------------------------------------------- Feature Scalling ----------------------------------------- #

training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# -------------------------------------------------- Applying Kernal PCA --------------------------------------- #

install.packages('kernlab')
library(kernlab)
kpca = kpca(~., data = training_set[-3], kernel = 'rbfdot', features = 2)
training_set_pca = as.data.frame(predict(kpca, training_set))
training_set_pca$Purchased = training_set$Purchased
test_set_pca = as.data.frame(predict(kpca, test_set))
test_set_pca$Purchased = test_set$Purchased

# ------------------------------------- Fitting Logistic Regression to the Training Set ------------------------ #

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set_pca)
summary(classifier)

# -------------------------------------------- Predicting the Test Set result ---------------------------------- #

prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-3])
prob_pred

# Making Vector probability prediction
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

# ------------------------------------------------- Confusion Matrix ------------------------------------------- #

cm = table(test_set_pca[, 3], y_pred)
cm

# --------------------------------------- Visualising the Training Set results --------------------------------- #

# Download from the site, it will be in zip from and then install.
# install.packages('ElemStatLearn')

library(ElemStatLearn)

set = training_set_pca

train1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
train2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(train1, train2)

colnames(grid_set) = c('V1', 'V2')

prob_set = predict(classifier, type = 'response', newdata = grid_set)

y_grid = ifelse(prob_set > 0.5, 1, 0)

# Plotting
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'PC1', ylab = 'PC2',
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

colnames(grid_set) = c('V1', 'V2')

prob_set = predict(classifier, type = 'response', newdata = grid_set)

y_grid = ifelse(prob_set > 0.5, 1, 0)

# Plotting
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Colour
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))