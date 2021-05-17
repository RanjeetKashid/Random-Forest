##### Company Data #####

library(randomForest)

company_data <- read.csv(file.choose())
summary(company_data)
sum(is.na(company_data))

# Categorizing o/p

company_data$category <- NA
company_data$category[company_data$Sales <= 5] = "Low Sale"
company_data$category[company_data$Sales > 5 & company_data$Sales <= 9] = "Medium Sale"
company_data$category[company_data$Sales >= 9] = "High Sale"

company_data$category = factor(company_data$category, levels = c("Low Sale", "Medium Sale", "High Sale"))

# Splitting data

library(caTools)

split <- sample.split(company_data$category, SplitRatio = 0.70)
split
table(split)
cd_train <- subset(company_data, split == TRUE)
cd_test  <- subset(company_data, split == FALSE)
table(cd_train$category)
table(cd_test$category)
cd_train <- cd_train[,-1]
cd_test <- cd_test[,-1]


# Building Model

cd_forest <- randomForest(cd_train$category~.,data = cd_train, na.action = na.roughfix, ntree = 500, importance=TRUE)
mean(predict(cd_forest,cd_train)==cd_train$category) # training accuracy

# Testing model

pred_test <- predict(cd_forest,cd_test[,-11])
table(pred_test,cd_test$category)
mean(pred_test==cd_test$category)

library(caret)
confusionMatrix(pred_test,cd_test$category)

# Visulaization
plot(cd_forest,lwd=2)
legend("topright", colnames(cd_forest$err.rate),col=1:4,cex=0.8,fill=1:4)



##### Fraud Check #####

fraud_data <- read.csv(file.choose())
summary(fraud_data)
str(fraud_data)
sum(is.na(fraud_data))

# Categorizing o/p

fraud_data$category <- NA
fraud_data$category[fraud_data$Taxable.Income <= 30000] = "Risky"
fraud_data$category[fraud_data$Taxable.Income > 30000] = "Good"

fraud_data$category = factor(fraud_data$category, levels = c("Risky", "Good"))
str(fraud_data)

# Splitting data

split_fd <- sample.split(fraud_data$category, SplitRatio = 0.70)
table(split_fd)
fd_train <- subset(fraud_data, split == TRUE)
fd_test  <- subset(fraud_data, split == FALSE)
table(fd_train$category)
table(fd_test$category)
fd_train <- fd_train[,-3]
fd_test <- fd_test[,-3]

# Building Model

fd_forest <- randomForest(fd_train$category~.,data = fd_train, na.action = na.roughfix, ntree = 500, importance=TRUE)
mean(predict(fd_forest,fd_train)==fd_train$category) # training accuracy

# Testing model

pred_test_fd <- predict(fd_forest,fd_test[,-6])
table(pred_test_fd,fd_test$category)
mean(pred_test_fd==fd_test$category)

# Visulaization
plot(fd_forest,lwd=2)
legend("topright", colnames(fd_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

