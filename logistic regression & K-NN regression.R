#Q : Logistic regression?
#Training Dataset
install.packages("MASS")

library(MASS)

my.data <- birthwt

my.data$race.1 <- ifelse(my.data$race==1, 1,0) #dummy variables created
my.data$race.2 <- ifelse(my.data$race==2, 1,0) #dummy variables created

set.seed(12345)

selected <- sample(1:nrow(my.data), size=nrow(my.data)*0.8, replace=FALSE) # replace=FALSE(SAMPLing with replacement or without replacement) IN MOST CASES WE USE SAMPLING WITHOUT REPLACEMENT

train <- my.data[selected,]
test <- my.data[-selected,]

library(caret)

predictors <- train[,c("age", "lwt", "race.1", "race.2", "smoke", 
                       "ptl", "ht", "ui", "ftv")]

train$low <- as.factor(train$low) # Must be in factor format for train or rfe

outcome <- train[,c("low")]

model.1 <- rfe(x=predictors,
               y=outcome,
               sizes= c(1:9),
               rfeControl= rfeControl(
                 functions= lrFuncs,
                 method= "cv",
                 number= 10,
                 verbose = TRUE #allows to see intermediate model if there is error
               )
)

print(model.1)


#Visualize above model

ggplot(model.1, metric="Accuracy") + theme_classic()
print(model.1$fit)


#Evaluate performance on validation set

low_hat <- predict(model.1, newdata= test)
test$low <- as.factor(test$low) # convert into factor variable
confusionMatrix(test$low, low_hat$pred)

index <- test$low != low_hat$pred # tells which is is wrong predictor in console



#K-Nearest Neighbors:

#Paramatrics & Non- Paramatrics - Pramatrics means you asume a linear functions whereas non- paramtrics is data driven process you don't make any assumtions for outcomes and predictons

#K-NN Algorithm- basically you want to find categorial outcomes and prediction(numerical outcomes)

#finding similar units in some form of neighborsto form the basis of the prediction.
#how to find neighbors : Eclidean distance is most popular measure of similarity
#predictors should be standarlized before calculating distance

setwd("C:/Visual Analytics/Machine Learning")

my.data.2 <- read.csv("green.csv")


#4th April 2024 K-Nearest Neighbours
#All factor variable should be converted into dummy varibales
#All numeric variables in pre-process

setwd("C:/Visual Analytics/Machine Learning")

my.data <- read.csv("UniversalBank.csv")

my.data$PersonalLoan <- as.factor(my.data$PersonalLoan)

#Convert education variable in 2 dummy variables as per raw data 

my.data$Education_1 <- ifelse(my.data$Education==1, 1,0)
my.data$Education_2 <- ifelse(my.data$Education==2, 1,0)

#standarized numeric predictors

library(caret)
#pre-process should be done for all numeric variables(eg:age,income,exp,family) for the continuation
my.data.pre <- preProcess(my.data[,c("Age", "Experience", "Income", "Family")],
                          method= "range")

#return back to orginal data
my.data <- predict(my.data.pre,my.data)

#Run K-NN model method now

model.1 <- knn3(PersonalLoan~ Age + Experience + Income + Family + Education_1 + Education_2,
                data= my.data,
                k = 3)

print(model.1)


#make predictions- Final Predictons

my.data$PersonalLoan_hat <- predict(model.1, newdata=my.data, type="class")

confusionMatrix(my.data$PersonalLoan, my.data$PersonalLoan_hat)

#Do training and validation process

#How to Tune Model(select the number of k)

set.seed(1234)
#human approach
model.2 <- train(PersonalLoan~ Age + Experience + Income + Family + Education_1 + Education_2,
                 data= my.data,
                 method= "knn",
                 trControl = trainControl(method="cv", number=10),
                 tuneGrid= data.frame(k= c(1,3,5,7,9,11,13,15,17))
                
)

print(model.2)

plot(model.2)

#automated approach #large k gives decreasing values so select 1st to 20 and then do it onces again
#use all the predictors from raw data eg:age,exp,family
model.3 <- train(PersonalLoan~ Age + Experience + Income + Family + Education_1 + Education_2,
                 data= my.data,
                 method= "knn",
                 trControl = trainControl(method="cv", number=10),
                 tuneLength= 50
                 
)
print(model.3)

plot(model.3)

best.model <- knn3(PersonalLoan~ Age + Experience + Income + Family + Education_1 + Education_2,
                   data= my.data,
                   k = 5)

print(best.model)


my.data$PersonalLoan_hat.2 <- predict(best.model, newdata= my.data, type="class")

confusionMatrix(my.data$PersonalLoan, my.data$PersonalLoan_hat.2)


