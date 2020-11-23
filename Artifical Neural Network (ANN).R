
# ----------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv("Churn_Modelling.csv")
dataset = dataset[4:14]

# Encoding the Categorical Variable as factors

dataset$Geography = as.numeric(factor(dataset$Geography, levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender, levels = c('Male', 'Female'),
                                      labels = c(1, 2)))

# ---------------------------------- Splitting the dataset into Training and Test Set --------------------------- #

library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# ------------------------------------------------- Feature Scalling -------------------------------------------- #

training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

# ------------------------------------------ Fitting ANN to the Training Set ------------------------------------ #

library(h2o)
h2o.init(nthreads = -1)
classifer = h2o.deeplearning(y = 'Exited', 
                             training_frame = as.h2o(training_set), 
                             activation = "Rectifier", 
                             hidden = c(6,6),
                             epochs = 100,
                             train_samples_per_iteration = -2
                             )

# ----------------------------------------- Predicting the Test Set results ------------------------------------- #

prob_pred = h2o.predict(classifer, newdata = as.h2o(test_set[-11]))
y_pred = (prob_pred > 0.5)
y_pred = as.vector(y_pred)

# ------------------------------------------- Making the confusion Matrix --------------------------------------- #

cm = table(test_set[, 11], y_pred)
cm

# --------------------------- According to Confusion Matrix this will show the accuracy ------------------------- #
(1538 + 192) / 2000

# Shutting down the h2o.
h2o.shutdown()
