
#Renaming the Variables
colnames(Mydata)[2]<-"MeanMonthlyMinutesOfUse"
colnames(Mydata)[1]<-"MeanMonthlyRevenue"
colnames(Mydata)[3]<-"MeanTotalRecurringCharge"
colnames(Mydata)[4]<-"MeanTotalDirectorAssistedCalls"
colnames(Mydata)[6]<-"MeanTotalRoamingCalls"
colnames(Mydata)[7]<-"%ChangeInMOU"
colnames(Mydata)[8]<-"%ChangeInRevenues"
colnames(Mydata)[9]<-"MeanNumberOfDroppedVoiceCalls"
colnames(Mydata)[10]<-"MeanNumberOfBlockedVoiceCalls"
colnames(Mydata)[11]<-"MeanNumberOfUnansweredVoiceCalls"
colnames(Mydata)[12]<-"MeanNumberOfCustomerCareCalls"
colnames(Mydata)[13]<-"MeanNumberOfThreewayCalls"
colnames(Mydata)[15]<-"MeanNumberOfOutgoingCalls"
colnames(Mydata)[16]<-"MeanNumberOfIncomingCalls"
colnames(Mydata)[17]<-"MeanNumberOfIn&outPeakVoiceCalls"
colnames(Mydata)[18]<-"MeanNumberOfIn&outOffPeakVoiceCalls"
colnames(Mydata)[19]<-"MeanNumberOfDroppedOrBlockedVoiceCalls"
colnames(Mydata)[20]<-"MeanNumberOfCallForwardingCalls"
colnames(Mydata)[21]<-"MeanNumberOfCallWaitingCalls"
colnames(Mydata)[23]<-"MonthsInService"
colnames(Mydata)[24]<-"NumberOfUniqueSubscriptions"
colnames(Mydata)[25]<-"NumberOfActiveSubscriptions"
colnames(Mydata)[27]<-"NumberOfHandsetsIssued"
colnames(Mydata)[28]<-"NumberOfModelsIssued"
colnames(Mydata)[29]<-"NumberOfDaysWithCurrentEquipment"
colnames(Mydata)[30]<-"CustomerID"
colnames(Mydata)[31]<-"AgeOfHHMember1"
colnames(Mydata)[32]<-"AgeOfHHMember2"
colnames(Mydata)[34]<-"CreditAHighest"
colnames(Mydata)[35]<-"CreditAAHigh"
colnames(Mydata)[36]<-"CreditBGood"
colnames(Mydata)[37]<-"CreditCMedium"
colnames(Mydata)[38]<-"CreditDELow"
colnames(Mydata)[39]<-"CreditGYVeryLow"
colnames(Mydata)[40]<-"CreditZLowest"
colnames(Mydata)[44]<-"Refurbished"
colnames(Mydata)[47]<-"RecreationalVehicle"
colnames(Mydata)[60]<-"RespondsToMailOffers"
colnames(Mydata)[62]<-"TravelledOutOfUSA"

#Converting Categorical Variables to factor
Mydata$CHILDREN<-factor(Mydata$CHILDREN,
                        levels = c(0,1),
                        labels = c("No","Yes"))
Mydata$CreditAHighest<-factor(Mydata$CreditAHighest,
                              levels = c(0,1),
                              labels = c("No","Yes"))
Mydata$CreditAAHigh<-factor(Mydata$CreditAAHigh,
                            levels = c(0,1),
                            labels = c("No","Yes"))
Mydata$CreditBGood<-factor(Mydata$CreditBGood,
                           levels = c(0,1),
                           labels = c("No","Yes"))
Mydata$CreditCMedium<-factor(Mydata$CreditCMedium,
                             levels = c(0,1),
                             labels = c("No","Yes"))
Mydata$CreditDELow<-factor(Mydata$CreditDELow,
                           levels = c(0,1),
                           labels = c("No","Yes"))
Mydata$CreditGYVeryLow<-factor(Mydata$CreditGYVeryLow,
                               levels = c(0,1),
                               labels = c("No","Yes"))
Mydata$CreditZLowest<-factor(Mydata$CreditZLowest,
                             levels = c(0,1),
                             labels = c("No","Yes"))
Mydata$PRIZMRUR<-factor(Mydata$PRIZMRUR,
                        levels = c(0,1),
                        labels = c("No","Yes"))
Mydata$PRIZMUB<-factor(Mydata$PRIZMUB,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$PRIZMTWN<-factor(Mydata$PRIZMTWN,
                        levels = c(0,1),
                        labels = c("No","Yes"))
Mydata$Refurbished<-factor(Mydata$Refurbished,
                           levels = c(0,1),
                           labels = c("No","Yes"))
Mydata$WEBCAP<-factor(Mydata$WEBCAP,
                      levels = c(0,1),
                      labels = c("No","Yes"))
Mydata$TRUCK<-factor(Mydata$TRUCK,
                     levels = c(0,1),
                     labels = c("No","Yes"))
Mydata$RecreationalVehicle<-factor(Mydata$RecreationalVehicle,
                                   levels = c(0,1),
                                   labels = c("No","Yes"))
Mydata$OCCPROF<-factor(Mydata$OCCPROF,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$OCCCLER<-factor(Mydata$OCCCLER,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$OCCCRFT<-factor(Mydata$OCCCRFT,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$OCCSTUD<-factor(Mydata$OCCSTUD,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$OCCHMKR<-factor(Mydata$OCCHMKR,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$OCCRET<-factor(Mydata$OCCRET,
                      levels = c(0,1),
                      labels = c("No","Yes"))
Mydata$OCCSELF<-factor(Mydata$OCCSELF,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$OWNRENT<-factor(Mydata$OWNRENT)
Mydata$MARRYUN<-factor(Mydata$MARRYUN)
Mydata$MARRYYES<-factor(Mydata$MARRYYES)
Mydata$MARRYNO<-factor(Mydata$MARRYNO)
Mydata$MAILORD<-factor(Mydata$MAILORD,
                       levels = c(0,1),
                       labels = c("No","Yes"))
Mydata$RespondsToMailOffers<-factor(Mydata$RespondsToMailOffers,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
Mydata$MAILFLAG<-factor(Mydata$MAILFLAG,
                        levels = c(0,1),
                        labels = c("No","Yes"))
Mydata$TravelledOutOfUSA<-factor(Mydata$TravelledOutOfUSA,
                                 levels = c(0,1),
                                 labels = c("No","Yes"))
Mydata$PCOWN<-factor(Mydata$PCOWN,
                     levels = c(0,1),
                     labels = c("No","Yes"))
Mydata$CREDITCD<-factor(Mydata$CREDITCD,
                        levels = c(0,1),
                        labels = c("No","Yes"))
Mydata$NEWCELLY<-factor(Mydata$NEWCELLY,
                        levels = c(0,1),
                        labels = c("No","Yes"))

Mydata$MCYCLE<-factor(Mydata$MCYCLE,
                      levels = c(0,1),
                      labels = c("No","Yes"))

Mydata$RETCALL<-factor(Mydata$RETCALL,
                       levels = c(0,1),
                       labels = c("No","Yes"))

