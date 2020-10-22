
# ------------------------------------------------ Importing Data -------------------------------------------- #

Data = read.csv("Data.csv")

# ----------------------------------------- Taking Care of Missing Data -------------------------------------- #

# Replacing an average value in the missing value.
# For Age
Data$Age = ifelse(is.na(Data$Age), 
                  ave(Data$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                  Data$Age)

# For Salary
Data$Salary = ifelse(is.na(Data$Salary), 
                     ave(Data$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                     Data$Salary)

# ------------------------------------------ Encoding Categorical Data --------------------------------------- #

# Changing the Categorical Data into Numerical Data
# For Country
Data$Country = factor(Data$Country, 
                      levels = c("France", "Spain", "Germany"),
                      labels = c(1, 2, 3))

# For Purchase
Data$Purchased = factor(Data$Purchased, 
                        levels = c("No", "Yes"),
                        labels = c(0, 1))

# ------------------------- Splitting the Dataset into the Training set and Testing Set ---------------------- #

# Training Set on which we build the machine learning model.
# Test Set on which we test the performance of the machine learning model.

# The performance on the test set should not be that different from the performance on the training set because
# this would mean that the machine learning model understood well the correlations and didnt learn them by heart
# so that he can adapt to new sets and new situations.

# install.packages("caTools") <----- Remove comment if not installed

library(caTools)
set.seed(123)

# In Python we put the percentage for Test Set, in R we put for Training Set.
split = sample.split(Data$Purchased, SplitRatio = 0.8)
split
# True mean observation goes to Training Set and False means observation goes to Test Set.

train_set = subset(Data, split == TRUE)
train_set
test_set = subset(Data, split == FALSE)
test_set

# ---------------------------------------------- Feature Scaling --------------------------------------------- #

# As Age and Salary are not in the same scale, this will cause some issue in our machinery models.

# This is because a lot of machine learning model are based on Euclidean Distance between Point1 and Point2 is
# the square root of the sum of squared coordinates.
# ED = sqrt[(x2 - x1)^2 + (y2 - y1)^2]

# The range of the value will be from -1 to +1.

# There are 2 methods;
# Standardisation:
# Xstand = (x - mean(x)) / standard deviation(x)

# Normalisation:
# Xnorm = (x - min(x)) / (max(x) - min(x)

train_set[2:3] = scale(train_set [2:3])
train_set
test_set[2:3] = scale(test_set[2:3])
test_set
