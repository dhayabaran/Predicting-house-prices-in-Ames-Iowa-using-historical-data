start.time = proc.time()
##check for packages

list.of.packages <- c("ggplot2", "lsr","psych", "glmnet", "xgboost", "Rmisc","DescTools","randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



#loading packages

library(randomForest)
library(ggplot2)
library(Rmisc)
library(glmnet) 
library(lsr)
library(psych)
library(xgboost)
library(DescTools)
library(dplyr)

# datum = read.csv('Ames_data.csv')
# 
# datum<- datum[ !(datum$PID%in% c(902207130, 910251050)), ]
# 
# working_data = read.table('Project1_test_id.txt')
# num=7
# testing_set = datum[datum$PID%in%working_data[,num],]
# pred = datum[datum$PID%in%working_data[,num], c(1,83)]
# 
# training_set = datum[!datum$PID%in%working_data[,num],]
# 
# 
# write.csv(testing_set,'test.csv', row.names = FALSE)
# write.csv(training_set,'train.csv', row.names = FALSE)
# write.csv(pred,'pred.csv', row.names = FALSE)


# read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

PID = test$PID

# training_set = read.csv('train_set.csv')
# testing_set = read.csv('test_set.csv')

data <- rbind(train, test)


# install.packages('devtools')
# library(devtools)
# require(devtools)
# install_version("gbm", version = "2.1.3", repos = "http://cran.us.r-project.org")



#removing columns

data <- subset(data, select = -c(Condition_2, Street, Utilities, Roof_Matl, Heating, Latitude, Longitude ))

data <- subset(data, select = -c(BsmtFin_SF_2, Low_Qual_Fin_SF, Three_season_porch, Misc_Val ))

data <- subset(data, select = -c(Garage_Yr_Blt, Misc_Feature, Bsmt_Cond, Alley, Land_Slope, Pool_QC ))

# data <- subset(data, select = -c(Land_Slope, Land_Contour, Alley, Year_Remod_Add, Central_Air,
 #                                Electrical, Functional, Pool_QC,Paved_Drive, Misc_Feature, Sale_Condition ))

data$Mo_Sold = as.factor(as.character(data$Mo_Sold))
data$Year_Sold = as.factor(as.character(data$Year_Sold))

#####################################################################################################
# 
# # #grouping the values
# 
# 
# # ###overall
# # cat1 = c('Very_Poor','Poor','Fair')
# # cat2 = c('Below_Average','Average','Above_Average','Good')
# # cat3 = c('Very_Good','Excellent','Very_Excellent')
# # data$Overall_Qual = ifelse(data$Overall_Qual%in%cat1, 'low', ifelse(data$Overall_Qual%in%cat2, 'medium', 'high'))
# # data$Overall_Cond = ifelse(data$Overall_Cond%in%cat2, 'low', ifelse(data$Overall_Cond%in%cat2, 'medium', 'high'))
# # data$Overall_Qual = as.factor(data$Overall_Qual)
# # data$Overall_Cond = as.factor(data$Overall_Cond)
# #
# #
# # ###basement
# # cat3 = c('Excellent','Good')
# # cat2 = c('Fair', 'Typical')
# # cat1 = c(' No_Basement','Poor')
# # data$Bsmt_Qual = ifelse(data$Bsmt_Qual%in%cat1, 'low', ifelse(data$Bsmt_Qual%in%cat2, 'medium', 'high'))
# # data$Bsmt_Cond = ifelse(data$Bsmt_Cond%in%cat2, 'low', ifelse(data$Bsmt_Cond%in%cat2, 'medium', 'high'))
# # data$Bsmt_Qual = as.factor(data$Bsmt_Qual)
# # data$Bsmt_Cond = as.factor(data$Bsmt_Cond)
# #
# #
# # ###Fireplace
# # cat3 = c('Excellent','Good')
# # cat2 = c('Fair', 'Typical')
# # cat1 = c(' No_Fireplace','Poor')
# # data$Fireplace_Qu = ifelse(data$Fireplace_Qu%in%cat1, 'low', ifelse(data$Fireplace_Qu%in%cat2, 'medium', 'high'))
# # data$Fireplace_Qu = as.factor(data$Fireplace_Qu)
# 


# # levels(data$Overall_Qual)
# # levels(data$Overall_Cond)

data$Overall_Qual <- ordered(data$Overall_Qual, levels = c("Very_Poor", "Poor", "Below_Average", "Average", "Above_Average", "Fair", "Good",
                                                           "Very_Good", "Excellent", "Very_Excellent"))
data$Overall_Cond <- ordered(data$Overall_Cond, levels = c("Very_Poor", "Poor", "Below_Average", "Average", "Above_Average", "Fair", "Good",
                                                           "Very_Good", "Excellent"))


#data$Overall_Qual <- ordered(data$Overall_Qual, levels = c("Low","Medium", "High"))
#data$Overall_Cond <- ordered(data$Overall_Cond, levels = c("Low","Medium", "High"))
# 
# 
data$Lot_Shape <- ordered(data$Lot_Shape, levels = c("Irregular", "Slightly_Irregular", "Moderately_Irregular", "Regular"))
data$Exter_Qual <- ordered(data$Exter_Qual, levels = c("Fair", "Typical", "Good", "Excellent"))
data$Exter_Cond <- ordered(data$Exter_Qual, levels = c("Poor","Fair", "Typical", "Good", "Excellent"))

data$Bsmt_Qual <- ordered(data$Bsmt_Qual, levels = c("Poor","No_Basement", "Fair", "Typical", "Good", "Excellent"))
#data$Bsmt_Cond <- ordered(data$Bsmt_Cond, levels = c("Poor","No_Basement", "Fair", "Typical", "Good", "Excellent"))

#data$Bsmt_Qual <- ordered(data$Bsmt_Qual, levels = c("Low","Medium", "High"))
##data$Bsmt_Cond <- ordered(data$Bsmt_Cond, levels = c("Low","Medium", "High"))

data$Heating_QC <- ordered(data$Heating_QC, levels = c("Poor","Fair", "Typical", "Good", "Excellent"))

data$Fireplace_Qu <- ordered(data$Fireplace_Qu, levels = c("Poor","No_Fireplace", "Fair", "Typical", "Good", "Excellent"))

#data$Fireplace_Qu <- ordered(data$Fireplace_Qu, levels =  c("Low","Medium", "High"))

data$Garage_Qual <- ordered(data$Garage_Qual, levels = c("Poor","No_Garage", "Fair", "Typical", "Good", "Excellent"))
data$Garage_Cond <- ordered(data$Garage_Cond, levels = c("Poor","No_Garage", "Fair", "Typical", "Good", "Excellent"))

data$Overall_Qual = as.numeric(data$Overall_Qual)
data$Overall_Cond = as.numeric(data$Overall_Cond)
data$Lot_Shape = as.numeric(data$Lot_Shape)
data$Exter_Qual = as.numeric(data$Exter_Qual)
data$Exter_Cond = as.numeric(data$Exter_Cond)
data$Bsmt_Qual = as.numeric(data$Bsmt_Qual)
#data$Bsmt_Cond = as.numeric(data$Bsmt_Cond)
data$Heating_QC = as.numeric(data$Heating_QC)
data$Fireplace_Qu = as.numeric(data$Fireplace_Qu)
data$Garage_Qual = as.numeric(data$Garage_Qual)
data$Garage_Cond = as.numeric(data$Garage_Cond)


#####################################################################################################

#one-hot
fake.y = rep(0, length(data[,1]))
one_hot = model.matrix(~.,data = data)
one_hot = data.frame(one_hot[, -1])  # remove the 1st column (the intercept) of tmp
write.csv(one_hot,'one_hot.csv')
data_onehot = one_hot


testing_set = data_onehot[data_onehot$PID%in%working_data[,num],]
training_set = data_onehot[!data_onehot$PID%in%working_data[,num],]

training_set <- subset(training_set, select = -c(PID ))
#training_set$Sale_Price = log(training_set$Sale_Price)

#testing_set$Sale_Price = log(testing_set$Sale_Price)

train_cols = ncol(training_set)
test_cols = ncol(testing_set)

test_rows = nrow(testing_set)

training_set$Gr_Liv_Area = Winsorize(training_set$Gr_Liv_Area)
training_set$Total_Bsmt_SF = Winsorize(training_set$Total_Bsmt_SF)
training_set$Garage_Area = Winsorize(training_set$Garage_Area)
training_set$Mas_Vnr_Area = Winsorize(training_set$Mas_Vnr_Area)


# training_set$Bedroom_AbvGr = Winsorize(training_set$Bedroom_AbvGr)
# training_set$Fireplaces = Winsorize(training_set$Fireplaces)
# training_set$Wood_Deck_SF = Winsorize(training_set$Wood_Deck_SF)
# training_set$Open_Porch_SF = Winsorize(training_set$Open_Porch_SF)
# training_set$Enclosed_Porch = Winsorize(training_set$Enclosed_Porch)
# training_set$Screen_Porch = Winsorize(training_set$Screen_Porch)


X_train = as.matrix(training_set[,1:(train_cols-1)])
Y_train = as.matrix(training_set[,train_cols])
X_test = as.matrix(testing_set[,2:(test_cols-1)]) 
Y_test = as.matrix(testing_set[,test_cols])



#Lasso
set.seed(100)
lam = exp(seq(-15, 5, length=500))
cv.out = cv.glmnet(X_train, Y_train, lambda=lam)
best.lam = cv.out$lambda.min
Ytest.pred = predict(cv.out, s = best.lam, newx = X_test)
mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
model_size = sum(mylasso.coef != 0) - 1
#error = sqrt(mean((Ytest.pred - Y_test)^2))

output2 = data.frame(PID, Ytest.pred)
colnames(output2) = c('PID','Sale_Price')
write.csv(output2,'output2.txt',row.names = FALSE)

# test.y = read.csv("pred.csv")
#
# pred <- read.csv("output2.txt")
# names(test.y)[2] <- "True_Sale_Price"
# pred <- merge(pred, test.y, by="PID")
# sqrt(mean((log(pred$Sale_Price) -log(pred$True_Sale_Price))^2)




# Perform training
rf_classifier = randomForest(Y_train ~ ., data=X_train, ntree=300, mtry=100, importance=TRUE)

prediction_for_table <- predict(rf_classifier,X_test)


# varImpPlot(rf_classifier)

output1 = data.frame(PID, prediction_for_table)
colnames(output1) = c('PID','Sale_Price')
write.csv(output1,'mysubmission1.txt',row.names = FALSE)

# test.y = read.csv("pred.csv")
# 
# pred <- read.csv("mysubmission1.txt")
# names(test.y)[2] <- "True_Sale_Price"
# pred <- merge(pred, test.y, by="PID")
# sqrt(mean((log(pred$Sale_Price) -log(pred$True_Sale_Price))^2))


proc.time() - start.time

# 300 5 0.14
# 200 7 0.135
#200 10 0.130
#300 12 0.126 (initial)

#300 15 0.124 (initial)
#300 20 0.121 (initial)
#300 40 0.118 (initial)
#300 100 0.116 (initial)
#200 120 0.117(initial)
#300 90 0.1163(initial)

