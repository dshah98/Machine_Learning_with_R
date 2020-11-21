# - NLP is applying Machine Learning models to text and language. Teaching machine to understand what is said in 
# spoken and written word is the focus of NLP.
# - Whenever you dictate something into your phone device that is then converted to text that is an NLP algorithm in 
# action.
# - You can also use NLP on a text review to predict if the review is good one or a bad one. It can used on an 
# article to predict some categories of the article you are trying to segment.
# - Most of the NLP algorithms are classification models, and they include Logistic Regression, Naive Bayes, CART, 
# which is a model based on decision trees.
# - A veery well known model in NLP is the Bag of Words Models. It a model used to preprocess the texts to classify 
# before fitting the classification algorithms on the observation containing the text.

# Types
# 1. Natural Language Processing 
# 2. Deep Learning
# 3. Deep Natural Language Processing
# 4. Seq2Seq

# Some Examples
# 1. Natural Language Processing
# - If / Else Rule (Chatbot)
# - Audio frequency component analysis (Speech Recognition)
# - Bag-of-Words Model (Classification)
# 2. Deep Learning
# - CNN for Text Recognition (Classification)

# ----------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.delim("Restaurant_Reviews.tsv", quote = '', stringsAsFactors = FALSE)
dataset_original = read.delim("Restaurant_Reviews.tsv", quote = '', stringsAsFactors = FALSE)

# -------------------------------------------------- Cleaning the Dataset ---------------------------------------- #

# install.packages("tm")
# install.packages("SnowballC")
library(tm)
library(SnowballC)

corpus = VCorpus(VectorSource(dataset$Review))
as.character(corpus[[1]])

# Making capital letter to lower case.
corpus = tm_map(corpus, content_transformer(tolower))
as.character(corpus[[1]])

# Removing all the number from the reviews
as.character(corpus[[841]])
corpus = tm_map(corpus, removeNumbers)
as.character(corpus[[841]])

# Removing all the punctuation
corpus = tm_map(corpus, removePunctuation)
as.character(corpus[[1]])

# Removing all the non relavent words like; Articles and Pronoun
corpus = tm_map(corpus, removeWords, stopwords())
as.character(corpus[[1]])

# Stemming
# for getting the root word.
corpus = tm_map(corpus, stemDocument)
as.character(corpus[[1]])

# Removing extra spaces
corpus = tm_map(corpus, stripWhitespace)
as.character(corpus[[841]])

# ------------------------------------------- Creating the Bag of Words Model ----------------------------------- #

dtm = DocumentTermMatrix(corpus)
dtm

# Applying a filter to clean even more the reviews by only considering the most frequent words.
dtm = removeSparseTerms(dtm, 0.999)
dtm
# Transforming Sparse Matrix of feature into a data frame.
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

# -------------------------------------- Encoding the target feature as factor ---------------------------------- #

dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# ----------------------------- Splitting the dataset into the Training set and Test set ------------------------ #

library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# ----------------------------- Fitting Random Forest Classification to the Training set ------------------------- #

# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# ------------------------------------------ Predicting the Test set results ------------------------------------- #

y_pred = predict(classifier, newdata = test_set[-692])
y_pred

# ------------------------------------------- Making the Confusion Matrix ---------------------------------------- #

cm = table(test_set[, 692], y_pred)
cm

# According to Confusion Matrix this will show the accuracy
(82 + 77) / 200
