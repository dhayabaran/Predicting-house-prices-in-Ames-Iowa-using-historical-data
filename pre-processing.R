

##read the initial data

data = read.csv('Ames_data.csv')

rows = nrow(data)
cols = ncol(data)

hist(data$Total_Bsmt_SF)
summary(data)

#removing columns

data <- subset(data, select = -c(Condition_2, Street, Utilities, Roof_Matl, Heating, Latitude, Longitude ))

data <- subset(data, select = -c(BsmtFin_SF_2, Low_Qual_Fin_SF, Three_season_porch, Misc_Val ))

data <- subset(data, select = -c(Garage_Yr_Blt ))

 # data <- subset(data, select = -c(Land_Slope, Land_Contour, Alley, Year_Remod_Add, Central_Air,
 #                                Electrical, Functional, Pool_QC,Paved_Drive, Misc_Feature, Sale_Condition ))




#correlation between variables and sales price

cor = correlate(data, data$Sale_Price, test=FALSE, corr.method="pearson", p.adjust.method="holm")

cor


#plots

plot(data$Sale_Price,data$Gr_Liv_Area)

hist(data$Total_Bsmt_SF)

psych::describe(data)

#count of na for each variable (stackoverflow)

na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count = data.frame(na_count)

dtypes = data.frame(sapply(data,class))

data$Mo_Sold = as.factor(as.character(data$Mo_Sold))
data$Year_Sold = as.factor(as.character(data$Year_Sold))

data %>% summarise_all(funs(n_distinct(.)))



#####################################################################################################
# 
# #grouping the values


# ###overall
# cat1 = c('Very_Poor','Poor','Fair')
# cat2 = c('Below_Average','Average','Above_Average','Good')
# cat3 = c('Very_Good','Excellent','Very_Excellent')
# data$Overall_Qual = ifelse(data$Overall_Qual%in%cat1, 'low', ifelse(data$Overall_Qual%in%cat2, 'medium', 'high'))
# data$Overall_Cond = ifelse(data$Overall_Cond%in%cat2, 'low', ifelse(data$Overall_Cond%in%cat2, 'medium', 'high'))
# data$Overall_Qual = as.factor(data$Overall_Qual)
# data$Overall_Cond = as.factor(data$Overall_Cond)
# 
# 
# ###basement
# cat3 = c('Excellent','Good')
# cat2 = c('Fair', 'Typical')
# cat1 = c(' No_Basement','Poor')
# data$Bsmt_Qual = ifelse(data$Bsmt_Qual%in%cat1, 'low', ifelse(data$Bsmt_Qual%in%cat2, 'medium', 'high'))
# data$Bsmt_Cond = ifelse(data$Bsmt_Cond%in%cat2, 'low', ifelse(data$Bsmt_Cond%in%cat2, 'medium', 'high'))
# data$Bsmt_Qual = as.factor(data$Bsmt_Qual)
# data$Bsmt_Cond = as.factor(data$Bsmt_Cond)
# 
# 
# ###Fireplace
# cat3 = c('Excellent','Good')
# cat2 = c('Fair', 'Typical')
# cat1 = c(' No_Fireplace','Poor')
# data$Fireplace_Qu = ifelse(data$Fireplace_Qu%in%cat1, 'low', ifelse(data$Fireplace_Qu%in%cat2, 'medium', 'high'))
# data$Fireplace_Qu = as.factor(data$Fireplace_Qu)



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
data$Bsmt_Cond <- ordered(data$Bsmt_Cond, levels = c("Poor","No_Basement", "Fair", "Typical", "Good", "Excellent"))

#data$Bsmt_Qual <- ordered(data$Bsmt_Qual, levels = c("Low","Medium", "High"))
##data$Bsmt_Cond <- ordered(data$Bsmt_Cond, levels = c("Low","Medium", "High"))

data$Heating_QC <- ordered(data$Heating_QC, levels = c("Poor","Fair", "Typical", "Good", "Excellent"))

#data$Fireplace_Qu <- ordered(data$Fireplace_Qu, levels = c("Poor","No_Fireplace", "Fair", "Typical", "Good", "Excellent"))

#data$Fireplace_Qu <- ordered(data$Fireplace_Qu, levels =  c("Low","Medium", "High"))

data$Garage_Qual <- ordered(data$Garage_Qual, levels = c("Poor","No_Garage", "Fair", "Typical", "Good", "Excellent"))
data$Garage_Cond <- ordered(data$Garage_Cond, levels = c("Poor","No_Garage", "Fair", "Typical", "Good", "Excellent"))

data$Overall_Qual = as.numeric(data$Overall_Qual)
data$Overall_Cond = as.numeric(data$Overall_Cond)
data$Lot_Shape = as.numeric(data$Lot_Shape)
data$Exter_Qual = as.numeric(data$Exter_Qual)
data$Exter_Cond = as.numeric(data$Exter_Cond)
data$Bsmt_Qual = as.numeric(data$Bsmt_Qual)
data$Bsmt_Cond = as.numeric(data$Bsmt_Cond)
data$Heating_QC = as.numeric(data$Heating_QC)
data$Fireplace_Qu = as.numeric(data$Fireplace_Qu)
data$Garage_Qual = as.numeric(data$Garage_Qual)
data$Garage_Cond = as.numeric(data$Garage_Cond)


#####################################################################################################


#one_hot
fake.y = rep(0, length(data[,1]))
one_hot = model.matrix(~.,data = data)
one_hot = data.frame(one_hot[, -1])  # remove the 1st column (the intercept) of tmp
write.csv(one_hot,'one_hot.csv')
data_onehot = one_hot


#loading the data
working_data = read.table('Project1_test_id.txt')
num=7
testing_set = data_onehot[data_onehot$PID%in%working_data[,num],]
training_set = data_onehot[!data_onehot$PID%in%working_data[,num],]
write.csv(testing_set,'test_set.csv', row.names = FALSE)
write.csv(training_set,'train_set.csv', row.names = FALSE)


#######################################################################################################


