# Suppose there is a datset containing movie recommendations for customers.

# Apriori has 3 parts;
# 1. Support
# - support (M) = # user watchlist containing M / # user watchlists
# 2. Confidence
# - confidence (M1 -> M2) = # user watchlist containing M1 and M2 / # user watchlists M1
# 3. Lift
# - lift (M1 -> M2) = confidence (M1 -> M2) / support (M2) 

# Steps for Apriori
# Step 1 : Set a minimum support and confidence
# Step 2 : Take all the subsets in transactions having higher support than minimum support.
# Step 3 : Take all the rules of these subsets having higher confidence than minimum confidence.
# Step 4 : Sort the rules by decreasing lift.

# ----------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

# --------------------------------------------------- Data Preprocessing ------------------------------------------ #

# install.packages('arules')
library(arules)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)

# Most purchased Products
itemFrequencyPlot(dataset, topN = 10)

# ------------------------------------------ Training Apriori on the dataset -------------------------------------- #

rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))

# ---------------------------------------------- Visualising the Result ------------------------------------------- #

inspect(sort(rules, by = 'lift')[1:10])
