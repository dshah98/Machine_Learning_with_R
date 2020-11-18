# Steps for Eclat algorithm;
# - Step 1 : Set a minimum support.
# - Step 2 : Take all the subsets in transactions having higher support than minimum support.
# - Step 3 : Sort these subsets by decreasing support.

# ----------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

# --------------------------------------------------- Data Preprocessing ------------------------------------------ #

# install.packages('arules')
library(arules)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)

# Most purchased Products
itemFrequencyPlot(dataset, topN = 10)

# ---------------------------------------- Training Eclat Model on the dataset ------------------------------------ #

rules = eclat(data = dataset, parameter = list(support = 0.004, minlen = 2))

# ---------------------------------------------- Visualising the Result ------------------------------------------- #

inspect(sort(rules, by = 'support')[1:10])
