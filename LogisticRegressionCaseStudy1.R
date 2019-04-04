#1. Loading the data
setwd('/Users/aakash/Desktop/R Programming/AnalytixLabsR/Regression Case Studies/Regression Case Studies - Linear & Logistic/Proactive Attrition Management-Logistic Regression Case Study/')
Mydata<-read.csv('Proactive Attrition Management-Logistic Regression Case Study.csv')
#Run file='LogisticRegressionCaseStudy2.R' with colnames and factor conversion before Proceeding further

#2.Setting 0 values=NULL as mentioned
Mydata$INCOME[Mydata$INCOME==0]<-NA
Mydata$SETPRC[Mydata$SETPRC==0]<-NA
str(Mydata)

#3.Removing Redundant Columns from Mydata
Mydata<-Mydata[c(-74)] #SETPRCM-Only tells if the Handset Price data is missing. Already replaced missing values in Handset price with NA
Mydata<-Mydata[c(-70)] #INCMISS-Only tells if the income data is missing. Already replaced missing values in income with NA
Mydata<-Mydata[c(-68)] #NEWCELLN-Doesn't make sense with NewcellY
Mydata<-Mydata[c(-19)] #MeanNumberOfDroppedOrBlockedVoiceCalls-'Dropped' And 'Blocked Voice Calls' Variable already there in the dataset. This is just a union of them

#Handling the 'Marry' variable
Mydata[Mydata$MARRYNO==0 & Mydata$MARRYUN==1,"MARRYNO"]<-NA
Mydata[Mydata$MARRYYES==0 & Mydata$MARRYUN==1,"MARRYYES"]<-NA
Mydata$MARRYUN<-as.numeric(Mydata$MARRYUN)
Mydata[Mydata$MARRYUN==1 & Mydata$MARRYYES==1,"MARRYUN"]<-4
Mydata[Mydata$MARRYUN==1 & Mydata$MARRYNO==1,"MARRYUN"]<-3
class(Mydata$MARRYUN)

Mydata$MARRYUN<-factor(Mydata$MARRYUN,
                       levels=c(2,3,4),
                       labels =c("NA","No","Yes"))

levels(Mydata$MARRYUN)
table(Mydata$MARRYUN)
which(names(Mydata)=="MARRYUN")
#Removing More Redundant Variables
Mydata<-Mydata[c(-57)] #Removing MARRYNO variable bcz redundant
Mydata<-Mydata[c(-56)] #Removing MARRYYES variable bcz redundant
#
colnames(Mydata)[55]<-"Married"
levels(Mydata$Married)
levels(Mydata$Married)[levels(Mydata$Married)=="NA"] <- NA
levels(Mydata$Married)<-c(0,1)
levels(Mydata$Married)

Mydata$Married<-factor(Mydata$Married,
                       levels=c(0,1),
                       labels =c("No","Yes"))


#4.Separting Calibration and Validation Data
CalibrationData<-Mydata[Mydata$CALIBRAT==1,]
ValidationData<-Mydata[Mydata$CALIBRAT==0,]

#Removing Redundant Columns From Calibration Data
library(compare)
which(names(CalibrationData)=="CustomerID")
compare(CalibrationData[21], CalibrationData[72], ignoreNames = T) 
CalibrationData<-CalibrationData[c(-72)] #CHURNDEP- Identical to Churn Variable in the dataframe
CalibrationData<-CalibrationData[c(-71)] #CALIBRAT- This variable was only to differentiate Calibration and Validation Data
CalibrationData<-CalibrationData[c(-29)] #CustomerID- Customer ID won't be of any help in prediction purpose
CalibrationData<-CalibrationData[c(-25)] #CSA-Communications Service Area has too many levels and will create problems in predicting customer churn

#5.For Calibration Data, Separating Numeric and Categorical Data
num_vars<-sapply(CalibrationData,is.numeric)
cat_vars<-sapply(CalibrationData,is.factor)
CategoricalVariables<-CalibrationData[cat_vars]
NumericVariables<-CalibrationData[num_vars]

#6.Descriptive Statistics
#Function for Numeric Variables
mystats<-function(x){
  var_type<-class(x)
  n<-length(x)
  nmiss<-sum(is.na(x))
  meanx<-mean(x, na.rm = T)
  stdx<-sd(x, na.rm = T)
  minx<-min(x, na.rm = T)
  prcntl<-quantile(x,na.rm = T, probs = c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))
  maxx<-max(x, na.rm = T)
  return(c(VarType=var_type, length=n, NumberOfMiss=nmiss,mean=meanx, sd=stdx,min=minx, percentile=prcntl, max=maxx))
}
#Function for Categorical Variables
prop.table(table(CategoricalVariables$CHILDREN))
mystats1<-function(x){
  varType=class(x)
  lengthx<-length(x)
  NoMiss<-sum(is.na(x))
  fre<-table(x)
  propx<-prop.table(table(x))
  return(c(varType=varType,length=lengthx,NumberOfMiss=NoMiss,freq=fre,prop=propx))
}

DataStats<-data.frame(t(apply(NumericVariables,2,mystats))) #Numeric variables Stats
DataStats1<-data.frame(t(sapply(CategoricalVariables,mystats1)))#Categorical variables Stats



#7.Missing Value Treatment
mean_imputation<-function(x){
  x[is.na(x)]<-mean(x,na.rm = T)
  return(x)
}

#Imputing Married Variable

install.packages('mice')
library(mice)
library(lattice)

tempdata<-mice(CategoricalVariables,m=5,maxit=40,meth='logreg',seed=500)

completedData <- complete(tempdata,1)
table(is.na(completedData))
CategoricalVariables$Married<-completedData$Married
table(is.na(CategoricalVariables))
#Imputing Numeric variables
NumericVariables<-apply(NumericVariables,2,mean_imputation)
NumericVariables<-data.frame(NumericVariables)
table(is.na(NumericVariables))

#8.Outlier Capping
outliersper <- function(x){
  (length(which(x >  mean(x) + 3 * sd(x) | x < mean(x) - 3 * sd(x))  ) / length(x))*100
}

OutlierPercentage<-data.frame(apply(NumericVariables,2,outliersper))

Outlier_capping<-function(x){
  m<-mean(x, na.rm = T)
  s<-sd(x, na.rm = T)
  uc<-m+3*s
  lc<-m-3*s
  x<-ifelse(x>uc,uc,x)
  x<-ifelse(x<lc,lc,x)  
  return(x)
}

NumericVariables<-apply(NumericVariables,2,Outlier_capping)
NumericVariables<- data.frame(NumericVariables)

#9.Combining Numeric and Categorical data
Final_data<-cbind(NumericVariables,CategoricalVariables)

#10.TREATING VALIDATION DATA FOR MISSING VALUES AND OUTLIERS
which(names(ValidationData)=="CSA")
ValidationData<-ValidationData[c(-72)] #CHURNDEP- ALL NA VALUES FOR VALIDATION DATASET
ValidationData<-ValidationData[c(-71)] #CALIBRAT- This variable was only to differentiate Calibration and Validation Data
ValidationData<-ValidationData[c(-29)] #CustomerID- Customer ID won't be of any help in prediction purpose
ValidationData<-ValidationData[c(-25)] #CSA-Communications Service Area has too many levels and will create problems in predicting customer churn

num_varsV<-sapply(ValidationData,is.numeric)
cat_varsV<-sapply(ValidationData,is.factor)
CategoricalVariablesV<-ValidationData[cat_vars]
NumericVariablesV<-ValidationData[num_vars]
colnames(NumericVariablesV)
tempdataV<-mice(CategoricalVariablesV,m=5,maxit=40,meth='logreg',seed=500)

completedDataV <- complete(tempdataV,1)
table(is.na(completedDataV$Married))

CategoricalVariablesV$Married<-completedDataV$Married
table(is.na(CategoricalVariablesV$Married))

NumericVariablesV<-apply(NumericVariablesV,2,mean_imputation)

table(is.na(NumericVariablesV))

ValidationData<-cbind(CategoricalVariablesV,NumericVariablesV)
table(is.na(ValidationData))

#11.Splitting Data into Training and testing Datasets
str(Final_data$CHURN)
Final_data$CHURN<-factor(Final_data$CHURN)
library(caTools)
set.seed(12345)
sample<-sample.split(Final_data$CHURN, SplitRatio = 0.7)
trainingData<-subset(Final_data, sample==TRUE)
testingData1<-subset(Final_data, sample==FALSE)
testingData2<-ValidationData

#12.Model
fit<-glm(CHURN~. , family = binomial(link = 'logit'), data = trainingData)
summary(fit)
#Run Concordance.R before Proceeding Further

Concordance(fit)
library(MASS)
newmodel<-stepAIC(fit)
summary(newmodel)
Concordance(newmodel)
vif(newmodel)


#13.Prediction Time

#Predicting for Training Data
trainingData$predictedChurn<-predict(newmodel,trainingData, type = 'response')

library(ROCR)
ROCRPRED<-prediction(trainingData$predictedChurn,trainingData$CHURN)
ROCRPREF<-performance(ROCRPRED,"tpr","fpr")
plot(ROCRPREF,colorize=TRUE ,print.cutoffs.at=seq(0.1, by=0.1))

#ConfusionMatrix
table(ActualValue=trainingData$CHURN,PredictedValue=trainingData$predictedChurn>0.51)
acc<-(8885+7510)/(8885+7510+6490+5115)
acc


#Predicting for Testing data
testingData1$predictedChurn<-predict(newmodel,newdata =testingData1, type = 'response')
testingData1$churnOrNot <- ifelse(testingData1$predictedChurn>0.51, 1,0)

#Confusion Matrix
table(ActualValue=testingData1$CHURN,PredictedValue=testingData1$predictedChurn>0.51)
acc<-(3754+3244)/(3754+3244+2240+2756)
acc


#Predicting for Validation data
setdiff(trainingData,ValidationData)

#There are 4 column names in validation dataset with changed names due to special characters 
which(names(ValidationData)=="%ChangeInMOU")
colnames(ValidationData)[40]<-"X.ChangeInMOU"

which(names(ValidationData)=="%ChangeInRevenues")
colnames(ValidationData)[41]<-"X.ChangeInRevenues" 

which(names(ValidationData)=="MeanNumberOfIn&outPeakVoiceCalls")
colnames(ValidationData)[50]<-"MeanNumberOfIn.outPeakVoiceCalls"

which(names(ValidationData)=="MeanNumberOfIn&outOffPeakVoiceCalls")
colnames(ValidationData)[51]<-"MeanNumberOfIn.outOffPeakVoiceCalls"

testingData2<-ValidationData

testingData2$predictedChurn<-predict(newmodel,newdata =testingData2, type = 'response')
testingData2$churnOrNot <- ifelse(testingData2$predictedChurn>0.51, 1,0)
#Confusion Matrix
table(ActualValue=testingData2$CHURN,PredictedValue=testingData2$predictedChurn>0.51)
acc<-(19202+320)/(19202+320+11236+289)
acc  

#Accuracy of 62%

#14. Key Factors Predicting Customer Churn 

#Picking Only those variables which are present in finalmodel
factordf<-Final_data[c(2,3,5,6,7,8,9,10,12,13,14,16,17,20,22,23,24,25,26,27,28,29,30,
                       31,33,34,36,38,39,41,45,47,48,52,57,59,61,66)]
factorNumbers<- sapply(factordf,is.numeric)
factorCategorical<-sapply(factordf,is.factor)
factorNumber<-factordf[factorNumbers]
factorCategoricals<-factordf[factorCategorical]


corrm<-cor(factorNumber)

library(psych)
scree(corrm)

eigen(corrm)$values

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen(corrm)$values)
                       , pct_var=eigen(corrm)$values/sum(eigen(corrm)$values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen(corrm)$values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE  

fa1<-fa(corrm,8, rotate = "varimax",fm="ml")

faSort<-fa.sort(fa1)
faSort$loadings
loadingss<-data.frame(faSort$loadings[1:ncol(corrm),])
write.csv( loadingss, 'mydata3.csv')
#factors Predicting Customer Churn
#a) USAGE DETAILS of Customer  b) Number of Equipment Issued  c) Number of Subscriptions
#d) Duration for which Services and Equipment are used by Customer  e)Retention Calls  f)Age of household members
#g) If Customer has Children:Direct Correlation  h) If Customer has 'Refusrbished Handset':Direct correlation  
#i) If Customer's handset is webCapable:Inverse correlation  j)If Customer is Married:Direct Correlation  
#k) If Customer responds to Mail Offers:Inverse Correlation  l)If customer has New Cellphone:Inverse correlation 

#Offers:
# 1.As usage increases(MeanMonthlyMinutesOfUse,MeanNumberOfIn.outPeakVoiceCalls,MeanNumberOfIncomingCalls), The Customer Churn decreases and vice versa. So we can tempt the customers to use more of our plans by offering low tarriff plans to NEW Customers as older customers are already likely to stay with us.
# 2.To existing Customers-we can offer combo subcription plans, so that the number of subscriptions increase for them. This is because, there is an inverse relation between NumberOfActiveSubscriptions and Churn rate
# 3.Since the number of handsets has a direct correlation with churn rate and if the Customer's handset is web capable has an inverse relation with churn rate, To increase the sales of our handsets and reduce churn rate, we can issue handsets with Web Capability and connections.
# 4. Since Customers who are Married and have Children are more likely to churn, we can send more MailOffers to Single people who are more likely to stick if they respond to mail offers 



#Q1:Data cleaning including missing values, outliers and multi-collinierity. Describe your predictive churn model. How did you select variables to be included in the model?
#Ans1: 
#a) Data Cleaning:The initial model had 78 variables. The 1st final model had 68 variables. So I removed 10 redundant numeric and categorical variables from the initial dataset after carefully analysing the descriptive statistics for each.
#b) Missing Values: The numeric missing values were imputed using mean value imputation.The categoric missing values were present for just one derived variable called 'Married' which were imputed using the mice function from the MICE Package.
#c) Outliers: Outliers for numeric variable were treated using a user defined function which capped the outliers beyond the range of mean+ 3*standard deviation.
#d) Multicollinearity and Variable selection: After initially analysing the dataset for similar and redundant variables, 10 variables were deleted from the dataset for prediction purpose. Then, to reduce multicollineaity further, we ran a stepAIC on first final model(fit) to get 2nd final model(newmodel) which had 39 variables. Then checked VIF scores for all variables which were all under 5 indicating less multicollinearity in our model. 

#Q2:Describe your predictive churn model & Demonstrate the predictive performance of the model.
#Ans2: 
#a) My final predictive Churn Model(newmodel) has 38 independent variables and one dependent variable 'CHURN'. 
#b) It has a concordance of 62%. We take the probability cut-off of 0.51 after various trial and error methods with the ROC curve. 
#c) The model was tested on 30% of the training data and then the Validation dataset, 
#d) A confusion matrix was built for each testing dataset and the Accuracy of the model came out to be around 60%.

#Q3:What are the key factors that predict customer churn? Do these factors make sense?
#Ans3: Answered in 14th part of the code. Yes, there are roughly 12 factors which predict customer churn and they make sense.

#Q4: Answered in 12th part of the code.






