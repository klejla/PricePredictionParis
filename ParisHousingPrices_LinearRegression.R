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


###------------------Train/Test split------------------###

#install.packages("caTools")
library(caTools)

# Setting seed so that the sample can be reproduced
set.seed(123)

#70% Train and 30% Test
split = sample.split(housing$price, SplitRatio = 0.70)
housing_train = subset(housing, split == TRUE)
housing_test = subset(housing, split == FALSE)

##--------linear regression------##
head(housing)
nrow(housing)
ncol(housing)
colnames(housing)
summary(housing)
summary(housing$price)

#mean
meanprice <- mean(housing$price)
print( meanprice )

#median
medianP <- median(housing$price)
print( medianP )

#mode
getmode <- function(PriceMode){
  uniqd <- unique(PriceMode)
  uniqd[which.max(tabulate(match(PriceMode, uniqd)))]
}
hprice <- housing$price

result1 <- getmode(hprice)
print(result1)

#subsetting
squareMeters <- housing$squareMeters
price <- housing$price

#NA value
is.na(squareMeters)
is.na(price)

#linear regression model 
housing <- lm(Price ~ squareMeters )
summary(housing)
attributes(housing)
coef(housing)

#visualisation
plot(squareMeters, Price, col ="green", main="House price vs SquareMeters",
     abline(housing), xlab="SquareMeters", ylab="House Price")

#check model
x <- data.frame(squareMeters=50)
resulthp <- predict (housing, x)
print(resulthp)

#multiple
## 75% of the sample size
smp_size <- floor(0.75*nrow(housing))

##set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(housing)), size = smp_size)

#MR
inputDataM <- housing_train[ , c("price", "squareMeters", "numberOfRooms", "hasPool")]
inputDataM

modelMR <- lm(price ~ squareMeters + numberOfRooms + hasPool, data=inputDataM )
modelMR

cat("### The COEFFICIENT VALUE ###", "/n")
a <- coef(modelMR)[1]
print(a)

xsquareMeters <- coef(modelMR)[2]
xnumberOfRooms <- coef(modelMR)[3]
xhasPool <- coef(modelMR)[4]

print(xsquareMeters)
print(xnumberOfRooms)
print(xhasPool)

# y=a + xsquareMeters.x1 + xnumberofRooms.x2 + xhasPool.x3
# price = 4963.720 + (100.000)*(60) + (1.516)*(1) + (2941.703)*(1)

# Check #

View(housing)
squareMeters=60
numberOfRooms=1
hasPool = 1
price = 4963.720 + (100.000)*(60) + (1.516)*(1) + (2941.703)*(1)
price

#predict
predict(modelMR, data = inputDataM)
summary(modelMR)

#data cleaning
install.packages('Amelia')
library(Amelia)
missmap(housing,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

#cor
cor(housing$price, housing)^ 2
res <- residuals(modelMR)
modelMR

#res
res <- as.data.frame(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(modelMR)

#model building
set.seed(123)

#Split the data , `split()` assigns a booleans to a new column based on the SplitRatio specified. 

split <- sample.split(housing,SplitRatio =0.75)
 
#train and test data
train <- subset(housing,split==TRUE)
test <- subset(housing,split==FALSE)

#training model
modelMR <- lm(price ~ squareMeters + numberOfRooms + hasPool, data=inputDataM )
modelMR

#visualizing the model
res <- residuals(modelMR)

# Convert residuals to a DataFrame 

res <- as.data.frame(res)
install.packages('ggplot2')
library(ggplot2)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(modelMR)

#rmse
rmse <- sqrt(mean(error)^2)
plot(modelMR)

#error
error <- housing$medv-housing$predicted.medv
plot(modelMR)


