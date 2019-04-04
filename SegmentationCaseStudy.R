setwd('/Users/aakash/Desktop/R Programming/AnalytixLabsR/BA Class3/Final Case Study 1 - Credit Card Segmentation/')
cc<-read.csv('CC GENERAL.csv')
str(cc)
numericVars<-cc[2:18]
factorVars<-cc[1]

#Descriptive Statistics

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  n<-length(a)
  mean<-mean(a)
  sd<-sd(a)
  min<-min(a)
  percentiles<-quantile(a,probs = c(0.01,0.05,0.1,0.25,0.50,0.75,0.9,0.95,0.99))
  max<-max(a)
  return(c(number=n,numberofmissing=nmiss,Mean=mean , standardeviation=sd,minimum=min,percntle=percentiles,maximum=max))
}

datastats <- data.frame(t(apply(numericVars,2,mystats)))

#Missing Values
table(is.na(numericVars))
summary(cc)
library(mice)

tempdata<-mice(numericVars,m=5,maxit=40,meth='pmm',seed=500)

completedData <- complete(tempdata,1)

numericVars<-completedData

table(is.na(numericVars))

#Outlier Capping

outliersper <- function(x){
  (length(which(x >  quantile(x,probs = c(0.99))| x < quantile(x,probs = c(0.01)))  ) / length(x))*100
}

OutlierPercentage<-data.frame(apply(numericVars,2,outliersper))

Outlier_capping<-function(x){
  
  uc<-quantile(x,probs = c(0.99))
  lc<-quantile(x,probs = c(0.01))
  x<-ifelse(x>uc,uc,x)
  x<-ifelse(x<lc,lc,x)  
  return(x)
}

numericVars<-apply(numericVars,2,Outlier_capping)
numericVars<- data.frame(numericVars)

cc_final<-cbind(factorVars,numericVars)


#Q1. Advanced data preparation: Build an enriched customer profile by deriving intelligent KPIs such as:
# 1. Monthly average purchase and cash advance amount
# 2. Purchases by type (one-off, installments)
# 3. Average amount per purchase and cash advance transaction,
# 4. Limit usage (balance to credit limit ratio),
# 5. Payments to minimum payments ratio etc

#Q1
#a)
library(dplyr)
cc_final<-mutate(cc_final,monthlyAveragePurchases=PURCHASES/TENURE)
cc_final<-mutate(cc_final,monthlyCashAdvanceAmount=CASH_ADVANCE/TENURE)

#b)
cc_final$purchase_type[cc_final$ONEOFF_PURCHASES > 0] <- "ONE-OFF PURCHASE" #4
cc_final$purchase_type[cc_final$INSTALLMENTS_PURCHASES > 0] <- "INSTALMENTS PURCHASE" #3
cc_final$purchase_type[cc_final$INSTALLMENTS_PURCHASES > 0 & cc_final$ONEOFF_PURCHASES > 0] <- "BOTH" #2
cc_final$purchase_type[cc_final$INSTALLMENTS_PURCHASES == 0 & cc_final$ONEOFF_PURCHASES == 0] <- "NEITHER" #1
cc_final$purchase_type<-factor(cc_final$purchase_type)
str(cc_final$purchase_type)

#c)
#Already given by variable:AVERAGE_PURCHASE_TRX AND CASH_ADVANCE_TRX

#d)
cc_final<-mutate(cc_final,limitUsage=(CREDIT_LIMIT-BALANCE)/CREDIT_LIMIT)

#e)
cc_final<-mutate(cc_final,PaymentsToMinimumPayments=PAYMENTS/MINIMUM_PAYMENTS)

#Separating numeric and categorical variables again
cc_final$purchase_type<-as.numeric(cc_final$purchase_type)
nums<-sapply(cc_final, is.numeric)
fact <- sapply(cc_final, is.factor)
finalNumericVars<-cc_final[nums]
finalFactorVars<-cc_final[fact]

#Factor Analysis

library(psych)
fadf<-finalNumericVars
fadf$PURCHASES<-NULL #Done
fadf$PaymentsToMinimumPayments<-NULL #Done
fadf$CREDIT_LIMIT<-NULL #Done
fadf$TENURE<-NULL #Done
fadf$CASH_ADVANCE<-NULL #Done
corrm<-cor(fadf)
scree(corrm)


eigen(corrm)$values
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen(corrm)$values)
                       , pct_var=eigen(corrm)$values/sum(eigen(corrm)$values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen(corrm)$values))
fa_new<-fa(corrm,5, rotate = "varimax",fm="ml")
faSort<-fa.sort(fa_new)
faSort$loadings
loadings_new<-data.frame(faSort$loadings[1:ncol(fadf),])
write.csv(loadings_new ,'factorAnalysis.csv')


#Standardization

inputdata_final<-scale(fadf)


#Clustering
set.seed(123)
cluster_three <- kmeans(inputdata_final,3) #Rejected Based on Segment Size
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)   #Rejected Based on Segment Size
cluster_seven <- kmeans(inputdata_final,7)   #Rejected Based on Segment Size


Hmisc::describe(cluster_three$cluster)
Hmisc::describe(cluster_four$cluster)
Hmisc::describe(cluster_five$cluster)
Hmisc::describe(cluster_six$cluster)
Hmisc::describe(cluster_seven$cluster)

table(cluster_five$cluster)



cc_new<-cbind(fadf,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster)

cc_new$km_clust_4=factor(cc_new$km_clust_4)
cc_new$km_clust_5=factor(cc_new$km_clust_5)
str(cc_new)
colnames(cc_new)
profile<-tabular(1+BALANCE+BALANCE_FREQUENCY+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                   CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+PAYMENTS+MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+monthlyAveragePurchases+monthlyCashAdvanceAmount+limitUsage+purchase_type ~ mean +(mean*km_clust_4)+(mean*km_clust_5),
                 data=cc_new)
profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
write.csv(profile1,"profilesheetCS.csv") 
# 5-cluster solution is better. Couldn't come up with marketing strategy because some variables are not clear in their meaning.


