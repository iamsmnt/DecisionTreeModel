#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
     "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
marketing_train = read.csv("marketing_tr.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Explore the data##########################################

str(marketing_train)

## Univariate Analysis and Variable Consolidation
marketing_train$schooling[marketing_train$schooling %in% "illiterate"] = "unknown"
marketing_train$schooling[marketing_train$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_train$default[marketing_train$default %in% "yes"] = "unknown"
marketing_train$default = as.factor(as.character(marketing_train$default))
marketing_train$marital[marketing_train$marital %in% "unknown"] = "married"
marketing_train$marital = as.factor(as.character(marketing_train$marital))
marketing_train$month[marketing_train$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_train$month[marketing_train$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_train$month = as.factor(as.character(marketing_train$month))
marketing_train$loan[marketing_train$loan %in% "unknown"] = "no"
marketing_train$loan = as.factor(as.character(marketing_train$loan))
marketing_train$schooling = as.factor(as.character(marketing_train$schooling))
marketing_train$profession[marketing_train$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_train$profession[marketing_train$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_train$profession = as.factor(as.character(marketing_train$profession))

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(marketing_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(marketing_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)


# kNN Imputation
marketing_train = knnImputation(marketing_train, k = 3)
#sum(is.na(marketing_train))


##################################Feature Scaling################################################
#Normality check
qqnorm(marketing_train$custAge)
hist(marketing_train$campaign)

#Normalisation
cnames = c("custAge","campaign","previous","cons.price.idx","cons.conf.idx","euribor3m","nr.employed",
           "pmonths","pastEmail")

for(i in cnames){
  print(i)
  marketing_train[,i] = (marketing_train[,i] - min(marketing_train[,i]))/
                                (max(marketing_train[,i] - min(marketing_train[,i])))
}


###################################Model Development#######################################
#Clean the environment
rmExcept("marketing_train")

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(marketing_train$responded, p = .80, list = FALSE)
train = marketing_train[ train.index,]
test  = marketing_train[-train.index,]

##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(responded ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-22], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$responded, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

write(capture.output(confusionMatrix(ConfMatrix_C50)), "ModelErrorMetrics.txt")

#False Negative rate
#FNR = FN/FN+TP 

#Accuracy: 90.89%
#FNR: 63.09%

