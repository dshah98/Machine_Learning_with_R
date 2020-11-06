# Bayes Theorem
# P(A|B) = (P(B|A) * P(A) / (P(B))

# Naive Bayes
# - Suppose there are 2 categories; Person who goes to work by walk or drive. 
# - Adding a new observation and lets predict weather a person will walk or drive.

# Steps of Naive Bayes;
# - Step 1 : Calculating the Bayes theorem for walk and they are named as,
#             P(Walks|X) = (P(X|Walks) * P(Walks) / (P(X)
#             1. X = Feature of new data point
#             2. P(Walks) = Prior Probability
#             3. P(X) = Marginal Probability
#             4. P(X|Walks) = Likelihood
#             5. P(Walks|X) = Posterior Porobability
# - Step 2 : Calculating the Bayes theorem for drive and they are named as,
#             P(Drives|X) = (P(X|Drives) * P(Drives) / (P(X)
#             1. X = Feature of new data point
#             2. P(Drives) = Prior Probability
#             3. P(X) = Marginal Probability
#             4. P(X|Drives) = Likelihood
#             5. P(Drives|X) = Posterior Porobability
# - Step 3 : P(Walks|X) vs P(Drives|X)

                                                                                 
# ** Calculating for Walkers **
# - P(Walks) = Number of Walkers / Total Observation
# - P(Walks) = 10 / 30 
                                                                                  
                                                                                  
# - For Marginal Probability, you need to make a circle around a new data point of your own radius.
# - P(X) = Number of Similar Observation / Total Observation
# - P(X) = 4 / 30
                                                                                  
                                                                                  
# For Likelihood, you need to make a circle around a new data point of your own radius and select only walker.
# - P(X|Walks) =  Number of Similar Observation Among those who Walks / Total Number of Walkers
# - P(X|Walks) = 3 / 10

# - P(Walks|X) = [ (3/10) * (10/30) ] / (4/30)
# - P(Walks|X) = 0.75
                                                                                 
# ** Calculating for Drivers **
# - P(Drives)` = Number of Drives / Total Observation
# - P(Drives) = 20 / 30`

                                                                                 
# For Marginal Probability, you need to make a circle around a new data point of your own radius.
# - P(X) = Number of Similar Observation / Total Observation
# - P(X) = 4 / 30


# For Likelihood, you need to make a circle around a new data point of your own radius and select only walker.
# - P(X|Drives) =  Number of Similar Observation Among those who Drives / Total Number of Drivers
# - P(X|Drives) = 1 / 20

                                                                               
# P(Drives|X) = [ (1/20) * (20/30) ] / (4/30)
# - P(Drives|X) = 0.25
                                                                              
# ** So the P(Walks|X) > P(Drives|X) **


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

library(e1071)
classifier = naiveBayes(x = training_set[-3], y = training_set$Purchased)
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

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3],
     main = ' Naive Bayes (Training set)',
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

y_grid = predict(classifier, newdata = grid_set)

# Plotting
plot(set[, -3], main = 'Naive Bayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(test1), ylim = range(test2))

# Regression Line
contour(test1, test2, matrix(as.numeric(y_grid), length(test1), length(test2)), add = TRUE)

# Giving Colour
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
