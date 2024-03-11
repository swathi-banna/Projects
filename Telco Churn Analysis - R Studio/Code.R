#Swathi Banna

rm(list=ls())
library(readxl)
setwd
telco <- read_excel("BAIS/Statistical Data Mining/Assignments/A7 TelcoChurn.xlsx")

colSums(is.na(telco))
str(telco)
attach(telco)   
boxplot(Churn)

table(Churn,gender)

#dropping features that are not required

telco$customerID=NULL
telco$PhoneService=NULL
telco$gender=NULL
telco$Partner=NULL
telco$TotalCharges=NULL

#changing the levels to 1 and 0

telco$phint=ifelse((MultipleLines!="No phone service"& InternetService!="No"),1,0)
telco$onlyphone=ifelse(MultipleLines!="No phone service" & InternetService=="No",1,0)                   
telco$onlyint=ifelse(MultipleLines=="No phone service" & InternetService!="No",1,0)
telco$Churn=ifelse(Churn=="Yes",1,0)
telco$OnlineSecurity=ifelse(OnlineSecurity=="Yes",1,0)
telco$OnlineBackup=ifelse(OnlineBackup=="Yes",1,0)
telco$DeviceProtection=ifelse(DeviceProtection=="Yes",1,0)
telco$TechSupport=ifelse(TechSupport=="Yes",1,0)
telco$StreamingTV=ifelse(StreamingTV=="Yes",1,0)
telco$StreamingMovies=ifelse(StreamingMovies=="Yes",1,0)

telco$MultipleLines=NULL
telco$InternetService=NULL

attach(telco) 
table(phint)
table(onlyphone)
table(onlyint)

temp <- c("Churn","SeniorCitizen", "Dependents", "OnlineSecurity",
          "OnlineBackup","DeviceProtection","TechSupport", "StreamingTV","StreamingMovies",
          "Contract","PaperlessBilling","PaymentMethod", "phint", "onlyphone", "onlyint")
telco[temp] <- lapply(telco[temp], factor)

#Correlation chart for numeric variables

temp1=telco[,c(3,13)]
library(PerformanceAnalytics)
chart.Correlation(temp1)

#Analysing Categorical variables

table(Churn)
table(Churn,phint)
table(Churn,onlyphone)
table(Churn,onlyint)
table(Churn,Contract)
table(Churn,SeniorCitizen)
table(Churn,PaperlessBilling)
table(Churn,PaymentMethod)

#Train and Test datasets

set.seed(1024)
trainIndex <- sample(1:nrow(telco), size=round(0.75*nrow(telco)), replace=FALSE)
train <- telco[trainIndex,]
test  <- telco[-trainIndex,]
dim(train); dim(test)
summary(test)
summary(train)
test_x <- test[ , -14]

#creating 3 dataframes for the 3 models (Only phone, Only Internet, Both)

train.phint=subset(train,train$phint==1)
train.phone=subset(train,train$onlyphone==1)
train.int=subset(train,train$onlyint==1)

test.phint=subset(test,test$phint==1)
test.phone=subset(test,test$onlyphone==1)
test.int=subset(test,test$onlyint==1)

test_x.phint=subset(test_x,test_x$phint==1)
test_x.phone=subset(test_x,test_x$onlyphone==1)
test_x.int=subset(test_x,test_x$onlyint==1)



#Model for both phone and internet services

logit.phint <- glm(Churn ~  Dependents + tenure + OnlineSecurity + OnlineBackup + 
                   DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract +
                   SeniorCitizen*PaymentMethod +  PaperlessBilling + MonthlyCharges,  
                   family=binomial (link="logit"), data=train.phint)
summary(logit.phint)

predlogit <-predict(logit.phint, newdata=test_x.phint, type="response")
summary(predlogit)
predlogit <- ifelse(predlogit>0.5, 1, 0)

confusion=table(test.phint$Churn, predlogit) # Confusion matrix
confusion
ClassificationError <- mean(predlogit != test.phint$Churn) # Classification error
print(ClassificationError)
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

library(ROCR)
pr <- prediction(predlogit, test.phint$Churn)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)                                                 # ROC plot: TPR vs FPR

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc 

recall.phint=confusion[4]/(confusion[4]+confusion[2])
precision.phint=confusion[4]/(confusion[3]+confusion[4])
f1.phint=(2*recall.phint*precision.phint)/(recall.phint+precision.phint)

recall.phint
precision.phint
f1.phint

#Model for "only phone services"

logit.phone  <- glm(Churn ~ SeniorCitizen + Dependents + tenure + Contract +
                     PaperlessBilling + PaymentMethod + MonthlyCharges,
                      family=binomial (link="logit"), data=train.phone)
summary(logit.phone)

predlogit <-predict(logit.phone,newdata=test_x.phone, type="response")
summary(predlogit)
predlogit <- ifelse(predlogit>0.2, 1, 0)    #Mean of predlogit is 0.144. Hence taking threshold as 0.2
table(predlogit)                            #0.5 threshold is giving a lot of False Negatives.

confusion=table(test.phone$Churn, predlogit)                         # Confusion matrix
confusion
ClassificationError <- mean(predlogit != test.phone$Churn)           # Classification error
print(paste("Accuracy = ", 1-ClassificationError))                   # Accuracy rate

pr <- prediction(predlogit, test.phone$Churn)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)                                                            # ROC plot: TPR vs FPR

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc

recallphone=confusion[4]/(confusion[4]+confusion[2])
precisionphone=confusion[4]/(confusion[3]+confusion[4])
f1phone=(2*recallphone*precisionphone)/(recallphone+precisionphone)
recallphone
precisionphone
f1phone

#Model for "only internet services"

logit.int  <- glm(Churn ~ SeniorCitizen + Dependents + tenure + OnlineSecurity + OnlineBackup + 
                     DeviceProtection + TechSupport + StreamingTV* Contract + StreamingMovies*Contract + 
                     PaperlessBilling + PaymentMethod + MonthlyCharges, 
                     family=binomial (link="logit"), data=train.int)
summary(logit.int)

predlogit <-predict(logit.int, newdata=test_x.int, type="response")
predlogit <- ifelse(predlogit>0.35, 1, 0)    #taking threshold as 0.35
summary(predlogit)

confusion=table(test.int$Churn, predlogit)                         # Confusion matrix
confusion
ClassificationError <- mean(predlogit != test.int$Churn)      # Classification error
print(ClassificationError)
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

pr <- prediction(predlogit, test.int$Churn)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)                                                 # ROC plot: TPR vs FPR

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc

recallint=confusion[4]/(confusion[4]+confusion[2])
precisionint=confusion[4]/(confusion[3]+confusion[4])
f1int=(2*recallint*precisionint)/(recallint+precisionint)
recallint
precisionint
f1int

stargazer::stargazer(logit.phone, logit.int,logit.phint,type="text",single.row = TRUE)


exp(coef(logit.phint))
exp(coef(logit.int))
exp(coef(logit.phone))
#install.packages("car")
car::vif(logit.phone)
car::vif(logit.int)
car::vif(logit.phint)
install.packages("lmtest")
lmtest::dwtest(logit.phone)
lmtest::dwtest(logit.int)
lmtest::dwtest(logit.phint)

