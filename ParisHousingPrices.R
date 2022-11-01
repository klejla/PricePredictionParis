##### EE418 Intro to Machine Learning - Project - Paris Housing Prices #####


# Importing data set
housing <- read.csv("ParisHousing.csv")


###------------------Exploratory data analysis------------------###

# Check for missing data
sum(is.na(housing))


# First and last ten observations
head(housing, 10)
tail(housing, 10)


# Examining data
str(housing)


# Summary of data
summary(housing)


# Library for additional stats about data set
#install.packages("skimr")
library(skimr)
skim(housing)


# Summary, histogram, and box-plot of housing price
summary(housing$price)
par(mfrow = c(1, 2))
boxplot(housing$price, main="Housing prices box-plot")
hist(housing$price, xlab="price", main="Housing prices histogram")


# Correlation coefficient between features
cor(housing$price, housing)


##----Correlation between price and squareMeters----##

# price and squareMeters plot
par(mfrow = c(1,1))
plot(housing$price, housing$squareMeters, 
     xlab = "price", ylab = "square meters", 
     type="p", 
     main="Price and m^2 correlation")


# price and squareMeters correlation coefficient
cor(housing$price, housing$squareMeters)


###------------------Feature selection (maybe)------------------###


###------------------Train/Test split------------------###

#install.packages("caTools")
library(caTools)

# Setting seed so that the sample can be reproduced
set.seed(123)

#70% Train and 30% Test
split = sample.split(housing$price, SplitRatio = 0.70)
housing_train = subset(housing, split == TRUE)
housing_test = subset(housing, split == FALSE)

###------------------Training the model------------------###

model_lm = housing(price ~ ., data = train, method = "lm", trControl = myControl)
