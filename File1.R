setwd('/Users/aakash/Desktop/R Programming/AnalytixLabsR/Regression Case Studies/Regression Case Studies - Linear & Logistic/Linear Regression Case/')
#Reading the data
library(readxl)
library(plyr)
library(dplyr)
cust_dbase<-read_xlsx('Linear Regression Case.xlsx', sheet = 1)
#Run Whole File CreditcardFactorConversion.R first before proceeding further
summary(cust_dbase)
#Separting Numeric and Categorical data
temp1<-sapply(cust_dbase,is.numeric)
numericColumns<-cust_dbase[,temp1]
temp2<-sapply(cust_dbase,is.factor)
categoricalColumns<-cust_dbase[,temp2]
colnames(numericColumns)

#Removing Redunant variables at first glance

#Numeric
numericColumns$pets<-NULL
numericColumns$carvalue[numericColumns$carvalue==-1]<-NA
numericColumns$carditems<-NULL
numericColumns$card2items<-NULL
numericColumns$spoused<-NULL  #Because 60% values are missing in the variable
#Removing All log variables of original variables
numericColumns$lntollmon<-NULL
numericColumns$lntollten<-NULL
numericColumns$lnequipmon<-NULL
numericColumns$lnequipten<-NULL
numericColumns$lncardmon<-NULL
numericColumns$lncardten<-NULL
numericColumns$lnwiremon<-NULL
numericColumns$lnwireten<-NULL
numericColumns$lnlongten<-NULL
numericColumns$lnlongmon<-NULL
numericColumns$lninc<-NULL
numericColumns$lncreddebt<-NULL
numericColumns$lnothdebt<-NULL

#Categorical
categoricalColumns$employ<-NULL
categoricalColumns$inccat<-NULL
categoricalColumns$address<-NULL
categoricalColumns$carcatvalue<-NULL

#Descriptive Analytics of Numeric Data
myStats<-function(x){
  nmiss<-sum(is.na(x))
  n<-length(x)
  a<-x[!is.na(x)]
  meanx<-mean(a,na.rm = T)
  minx<-min(a)
  prcntl<-quantile(a,probs = c(0.1,0.25,0.50,0.75,0.9,0.95,0.99))
  maxx<-max(a)
  return(c(n=n, nmiss=nmiss, mean=meanx,min=minx, percentile=prcntl, max=maxx))
}

numericDataStats<-data.frame(t(apply(numericColumns,2,myStats)))

#MissingValueImputation

#For NumericData
table(is.na(numericColumns))

mean_imputation<-function(x){
  x[is.na(x)]<-mean(x,na.rm = T)
  return(x)
}

numericColumns<-data.frame(apply(numericColumns,2,mean_imputation))
table(is.na(numericColumns))

#For CategoricalData
mode_imputation<-function(x){
  x[is.na(x)]<-which.max(table(x))
  return(x)
}

table(is.na(categoricalColumns))
categoricalColumns<-data.frame(apply(categoricalColumns,2,mode_imputation))
table(is.na(categoricalColumns))

#Checking normality of a few variables and transforming them
hist(numericColumns$creddebt)
numericColumns$creddebt[numericColumns$creddebt==0]<-numericColumns$creddebt[numericColumns$creddebt==0]+1 #Take Log
hist(log(numericColumns$creddebt))#Need
numericColumns$creddebt<-log(numericColumns$creddebt)
# names(numericColumns)[4]<-"lncreddebt"
hist(numericColumns$othdebt)
numericColumns$othdebt[numericColumns$othdebt==0]<-numericColumns$othdebt[numericColumns$othdebt==0]+1 # Take Log
hist(log(numericColumns$othdebt)) #Need
numericColumns$othdebt<-log(numericColumns$othdebt)
# names(numericColumns)[5]<-"lnothdebt"

hist(numericColumns$income)
hist(log(numericColumns$income))#Need
numericColumns$income<-log(numericColumns$income)
# names(numericColumns)[2]<-"lnincome"

hist(numericColumns$longmon)
hist(log(numericColumns$longmon)) #Need
numericColumns$longmon<-log(numericColumns$longmon)
# names(numericColumns)[names(numericColumns)=="longmon"]<-'lnlongmon'

hist(numericColumns$longten)
hist(log(numericColumns$longten)) #Need
numericColumns$longten<-log(numericColumns$longten)
# names(numericColumns)[names(numericColumns)=="longten"]<-'lnlongten'

numericColumns<- mutate(numericColumns, totalSpend=cardspent+card2spent)

#Removing Individual spend of primary and secondary card
numericColumns[,"cardspent"]<-NULL
numericColumns[,"card2spent"]<-NULL



#Outlier Capping
outliersper <- function(x){
  (length(which(x >  mean(x) + 2.5 * sd(x) | x < mean(x) - 2.5 * sd(x))  ) / length(x))*100
}

OutlierPercentage<-data.frame(apply(numericColumns,2,outliersper))

Outlier_capping<-function(x){
  m<-mean(x, na.rm = T)
  s<-sd(x, na.rm = T)
  uc<-m+2.5*s
  lc<-m-2.5*s
  x<-ifelse(x>uc,uc,x)
  x<-ifelse(x<lc,lc,x)  
  return(x)
}

numericColumns<-data.frame(apply(numericColumns,2,Outlier_capping))

#Checking for Multicollinearity

library(psych)
corrm<-cor(numericColumns)


#Factor Analysis
scree(corrm)
eigen(corrm)$values
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen(corrm)$values)
                       , pct_var=eigen(corrm)$values/sum(eigen(corrm)$values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen(corrm)$values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE  


fa1<-fa(corrm,11, rotate = "varimax", fm="ml")
faSort<-fa.sort(fa1)
faSort$loadings
loadingss<-data.frame(faSort$loadings[1:ncol(numericColumns),])
#Can remove:reside,pets_dogs,hourstv,pets_small,commutetime,pets_cats,pets_saltfish,pets_birds,pets_reptiles

#Checking Significant categorical variables
categoricalColumns$totalSpend<-numericColumns$totalSpend
av1<-aov(totalSpend~. , data=categoricalColumns )
summary(av1)
#Highly correlated variables with total spend: region,agecat,gender,edcat,jobcat,empcat,retire,default,homeown,carown,reason,vote,card,card2,voice,ownvcr,owndvd
categoricalColumns<-categoricalColumns[,c("region","agecat","gender","edcat","jobcat","empcat",
                                          "retire","default","homeown","carown","reason","vote",
                                          "card","card2","card2tenure","voice" ,"ownvcr","owndvd","response_03")]

#Combining numerical and categorical variables
colnames(categoricalColumns)
final_data<-data.frame(cbind(numericColumns,categoricalColumns))

#Checking Normality of Dependent variable
hist(final_data$totalSpend)

#Splitting into training and testing data
library(caTools)
set.seed(12345)
sample<-sample.split(final_data$totalSpend,SplitRatio = 0.7)
trainingData<-subset(final_data, sample==TRUE)
testingData<-subset(final_data, sample==FALSE)

#Model Development

install.packages('car', dependencies = T)
library(car)
library(MASS)
colnames(numericColumns)
colnames(categoricalColumns)
#1. Approch 1: Taking All vars
fit1<-lm(totalSpend~., data = trainingData)
vif(fit1)
summary(fit1)
step1<-stepAIC(fit1,direction = "both")
summary(step1)
finalfit1<-lm(formula = totalSpend ~ age + ed + income + equipmon + cardten + 
                gender + jobcat + reason + vote + card + card2 + voice + 
                response_03, data = trainingData)
summary(finalfit1)
vif(finalfit1)

trainingData$Cd<- cooks.distance(finalfit1) 
trainingData1<-subset(trainingData, Cd< (4/3500))
trainingData1$Cd<-NULL

fit1<-lm(totalSpend~., data = trainingData1)
vif(fit1)
summary(fit1)

step1<-stepAIC(fit1,direction = "both")
summary(step1)
finalfit1<-lm(formula = totalSpend ~ age + ed + income+othdebt + pets_reptiles + 
                pets_saltfish + pets_freshfish + equipten + hourstv + gender + 
                jobcat + empcat + reason + card + card2 + voice, data = trainingData1)
summary(finalfit1)
vif(finalfit1)

#Plotting Regression Coefficients
plot_coeffs <- function(mlr_model) {
  coeffs <- coefficients(mlr_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
}

plot_coeffs(finalfit1)

plot(finalfit1)

#Predicting values based on model
#For training data
t1temp<-cbind(trainingData1,predSpend= predict(finalfit1,trainingData1))
#For testing data
t2temp<-cbind(testingData,predSpend= predict(finalfit1,testingData))

#Deciling Analysis
quantile(t1temp$predSpend, probs = seq(0.1,0.9, by=0.1))
delocationstemp<-quantile(t1temp$predSpend, probs = seq(0.1,0.9, by=0.1))
t1temp$decile<-findInterval(t1temp$predSpend,c(-Inf,delocationstemp,Inf))

delocations1temp<-quantile(t2temp$predSpend, probs = seq(0.1,0.9, by=0.1))
t2temp$decile<-findInterval(t2temp$predSpend,c(-Inf,delocations1temp,Inf))

install.packages('sqldf', dependencies = T)
library(sqldf)

TrainingPredictedSpend<-sqldf("select decile, count(decile) as count , avg(predSpend) as avg_pred_Spend,   avg(totalSpend) as avg_total_Spend
                              
                              from t1temp 
                              group by decile
                              order by decile desc")

TestingPredictedSpend<-sqldf("select decile, count(decile) as count , avg(predSpend) as avg_pred_Spend,   avg(totalSpend) as avg_total_Spend
                             
                             from t2temp 
                             group by decile
                             order by decile desc")

plot(finalfit2)

