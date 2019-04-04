install.packages('expss', dependencies = T)
library(expss)


cust_dbase <- apply_labels(cust_dbase,custid="Customer ID",
                           region="Geographic indicator",
                           townsize="Size of hometown",
                           gender="Gender",
                           age="Age in years",
                           agecat="Age category",
                           birthmonth="Birth month",
                           ed="Years of education",
                           edcat="Level of education",
                           jobcat="Job category",
                           union="Union member",
                           employ="Years with current employer",
                           empcat="Years with current employer",
                           retire="Retired",
                           income="Household income in thousands",
                           lninc="Log-income",
                           inccat="Income category in thousands",
                           debtinc="Debt to income ratio (x100)",
                           creddebt="Credit card debt in thousands",
                           lncreddebt="Log-credit card debt",
                           othdebt="Other debt in thousands",
                           lnothdebt="Log-Other debt",
                           default="Ever defaulted on a bank loan",
                           jobsat="Job satisfaction",
                           marital="Marital status",
                           spoused="Spouse years of education",
                           spousedcat="Spouse level of education",
                           reside="Number of people in household",
                           pets="Number of pets owned",
                           pets_cats="Number of cats owned",
                           pets_dogs="Number of dogs owned",
                           pets_birds="Number of birds owned",
                           pets_reptiles="Number of reptiles owned",
                           pets_small="Number of small animals owned",
                           pets_saltfish="Number of saltwater fish owned",
                           pets_freshfish="Number of freshwater fish owned",
                           homeown="Home ownership",
                           hometype="Building type",
                           address="Years at current address",
                           addresscat="Years at current address",
                           cars="Number of cars owned/leased",
                           carown="Primary vehicle lease/own",
                           cartype="Primary vehicle domestic/import",
                           carvalue="Primary vehicle sticker price",
                           carcatvalue="Primary vehicle price category",
                           carbought="Primary vehicle bought/leased within last year",
                           carbuy="Plan to purchase/lease vehicle within next year",
                           commute="Primary commute transportation",
                           commutecat="Commute category",
                           commutetime="Commute time in minutes",
                           commutecar="Used car to commute within last year",
                           commutemotorcycle="Used motorcycle to commute within last year",
                           commutecarpool="Used carpool to commute within last year",
                           commutebus="Used bus to commute within last year",
                           commuterail="Used train/subway to commute within last year",
                           commutepublic="Used other public transport to commute within last year",
                           commutebike="Used bike to commute within last year",
                           commutewalk="Walked to commute within last year",
                           commutenonmotor="Used other non-motorized transport to commute within last year",
                           telecommute="Telecommuted within last year",
                           reason="Primary reason for being a customer here",
                           polview="Political outlook",
                           polparty="Political party membership",
                           polcontrib="Political contributions",
                           vote="Voted in last election",
                           card="Primary credit card",
                           cardtype="Designation of primary credit card",
                           cardbenefit="Benefit program for primary credit card",
                           cardfee="Annual fee for primary credit card",
                           cardtenure="Years held primary credit card",
                           cardtenurecat="Years held primary credit card",
                           card2="Secondary credit card",
                           card2type="Designation of secondary credit card",
                           card2benefit="Benefit program for secondary credit card",
                           card2fee="Annual fee for secondary credit card",
                           card2tenure="Years held secondary credit card",
                           card2tenurecat="Years held secondary credit card",
                           carditems="Number of items on primary card last month",
                           cardspent="Amount spent on primary card last month",
                           card2items="Number of items on secondary card last month",
                           card2spent="Amount spent on secondary card last month",
                           active="Active lifestyle",
                           bfast="Preferred breakfast",
                           tenure="Number of months with service",
                           churn="Switched providers within last month",
                           longmon="Long distance last month",
                           lnlongmon="Log-long distance last month",
                           longten="Long distance over tenure",
                           lnlongten="Log-long distance over tenure",
                           tollfree="Toll free service",
                           tollmon="Toll-free last month",
                           lntollmon="Log-toll free last month",
                           tollten="Toll-free over tenure",
                           lntollten="Log-toll free over tenure",
                           equip="Equipment rental",
                           equipmon="Equipment last month",
                           lnequipmon="Log-equipment last month",
                           equipten="Equipment over tenure",
                           lnequipten="Log-equipment over tenure",
                           callcard="Calling card service",
                           cardmon="Calling card last month",
                           lncardmon="Log-calling card last month",
                           cardten="Calling card over tenure",
                           lncardten="Log-calling card over tenure",
                           wireless="Wireless service",
                           wiremon="Wireless last month",
                           lnwiremon="Log-wireless last month",
                           wireten="Wireless over tenure",
                           lnwireten="Log-wireless over tenure",
                           multline="Multiple lines",
                           voice="Voice mail",
                           pager="Paging service",
                           internet="Internet",
                           callid="Caller ID",
                           callwait="Call waiting",
                           forward="Call forwarding",
                           confer="3-way calling",
                           ebill="Electronic billing",
                           owntv="Owns TV",
                           hourstv="Hours spent watching TV last week",
                           ownvcr="Owns VCR",
                           owndvd="Owns DVD player",
                           owncd="Owns stereo/CD player",
                           ownpda="Owns PDA",
                           ownpc="Owns computer",
                           ownipod="Owns portable digital audio player",
                           owngame="Owns gaming system",
                           ownfax="Owns fax machine",
                           news="Newspaper subscription",
                           response_01="Response to product offer 01",
                           response_02="Response to product offer 02",
                           response_03="Response to product offer 03",
                           
                           region=c("Zone 1"=1,"Zone 2"=2,"Zone 3"=3,"Zone 4"=4,"Zone 5"=5),
                           townsize=c("> 250,000"=1,"50,000-249,999"=2,"10,000-49,999"=3,"2,500-9,999"=4,"< 2,500"=5),
                           
                           
                           
                           
                           gender=c("Male"=0,"Female"=1),
                           
                           agecat=c("<18"=1,"18-24"=2,"25-34"=3,"35-49"=4,"50-64"=5,">65"=6,"No response"=9),
                           birthmonth=c("April"="April","August"="August","December"="December","February"="February","January"="January","July"="July","June"="June","March"="March","May"="May","November"="November","October"="October","September"="September"),
                           edcat=c("Did not complete high school"=1,"High school degree"=2,"Some college"=3,"College degree"=4,"Post-undergraduate degree"=5),
                           
                           
                           
                           
                           jobcat=c("Managerial and Professional"=1,"Sales and Office"=2,"Service"=3,"Agricultural and Natural Resources"=4,"Precision Production, Craft, Repair"=5,"Operation, Fabrication, General Labor"=6),
                           
                           
                           
                           
                           
                           union=c("No"=0,"Yes"=1),
                           employ=c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20,"21"=21,"22"=22,"23"=23,"24"=24,"25"=25,"26"=26,"27"=27,"28"=28,"29"=29,"30"=30,"31"=31,"32"=32,"33"=33,"34"=34,"35"=35,"36"=36,"37"=37,"38"=38,"39"=39,"40"=40,"41"=41,"42"=42,"43"=43,"44"=44,"45"=45,"46"=46,"47"=47,"48"=48,"49"=49,"51"=51,"52"=52),
                           empcat=c("Less than 2"=1,"2 to 5"=2,"6 to 10"=3,"11 to 15"=4,"More than 15"=5),
                           
                           
                           
                           
                           retire=c("No"=0,"Yes"=1),
                           
                           inccat=c("Under $25"=1,"$25 - $49"=2,"$50 - $74"=3,"$75 - $124"=4,"$125+"=5),
                           default=c("No"=0,"Yes"=1),
                           
                           jobsat=c("Highly dissatisfied"=1,"Somewhat dissatisfied"=2,"Neutral"=3,"Somewhat satisfied"=4,"Highly satisfied"=5),
                           marital=c("Unmarried"=0,"Married"=1),
                           
                           spousedcat=c("Not married"=-1,"Did not complete high school"=1,"High school degree"=2,"Some college"=3,"College degree"=4,"Post-undergraduate degree"=5),
                           
                           
                           
                           
                           
                           homeown=c("Rent"=0,"Own"=1),
                           
                           hometype=c("Single-family"=1,"Multiple-Family"=2,"Condominium/Townhouse"=3,"Mobile Home"=4),
                           address=c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20,"21"=21,"22"=22,"23"=23,"24"=24,"25"=25,"26"=26,"27"=27,"28"=28,"29"=29,"30"=30,"31"=31,"32"=32,"33"=33,"34"=34,"35"=35,"36"=36,"37"=37,"38"=38,"39"=39,"40"=40,"41"=41,"42"=42,"43"=43,"44"=44,"45"=45,"46"=46,"47"=47,"48"=48,"49"=49,"50"=50,"51"=51,"52"=52,"53"=53,"54"=54,"55"=55,"57"=57),
                           addresscat=c("Less than 3"=1,"4 to 7"=2,"8 to 15"=3,"16 to 25"=4,"More than 25"=5),
                           
                           
                           
                           
                           cars=c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8),
                           
                           
                           
                           
                           
                           
                           
                           
                           carown=c("N/A"=-1,"Lease"=0,"Own"=1),
                           
                           
                           cartype=c("N/A"=-1,"Domestic"=0,"Import"=1),
                           
                           
                           carcatvalue=c("N/A"=-1,"Economy"=1,"Standard"=2,"Luxury"=3),
                           
                           
                           
                           carbought=c("N/A"=-1,"No"=0,"Yes"=1),
                           carbuy=c("No"=0,"Yes"=1),
                           
                           commute=c("Car"=1,"Motorcycle"=2,"Carpool"=3,"Bus"=4,"Train/Subway"=5,"Other public transit"=6,"Bicycle"=7,"Walk"=8,"Other non-motorized transit"=9,"Telecommute"=10),
                           commutecat=c("Single occupancy"=1,"Multiple occupancy"=2,"Public transportation"=3,"Non-motorized"=4,"Telecommute"=5),
                           
                           
                           
                           
                           commutecar=c("No"=0,"Yes"=1),
                           
                           commutemotorcycle=c("No"=0,"Yes"=1),
                           
                           commutecarpool=c("No"=0,"Yes"=1),
                           
                           commutebus=c("No"=0,"Yes"=1),
                           
                           commuterail=c("No"=0,"Yes"=1),
                           
                           commutepublic=c("No"=0,"Yes"=1),
                           
                           commutebike=c("No"=0,"Yes"=1),
                           
                           commutewalk=c("No"=0,"Yes"=1),
                           
                           commutenonmotor=c("No"=0,"Yes"=1),
                           
                           telecommute=c("No"=0,"Yes"=1),
                           reason=c("Prices"=1,"Convenience"=2,"Service"=3,"Other"=4,"N/A"=8,"No response"=9),
                           
                           
                           
                           
                           
                           polview=c("Extremely liberal"=1,"Liberal"=2,"Slightly liberal"=3,"Moderate"=4,"Slightly conservative"=5,"Conservative"=6,"Extremely conservative"=7),
                           polparty=c("No"=0,"Yes"=1),
                           
                           polcontrib=c("No"=0,"Yes"=1),
                           
                           vote=c("No"=0,"Yes"=1),
                           
                           card=c("American Express"=1,"Visa"=2,"Mastercard"=3,"Discover"=4,"Other"=5),
                           cardtype=c("None"=1,"Gold"=2,"Platinum"=3,"Other"=4),
                           
                           
                           
                           cardbenefit=c("None"=1,"Cash back"=2,"Airline miles"=3,"Other"=4),
                           cardfee=c("No"=0,"Yes"=1),
                           
                           cardtenure=c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20,"21"=21,"22"=22,"23"=23,"24"=24,"25"=25,"26"=26,"27"=27,"28"=28,"29"=29,"30"=30,"31"=31,"32"=32,"33"=33,"34"=34,"35"=35,"36"=36,"37"=37,"38"=38,"39"=39,"40"=40),
                           cardtenurecat=c("Less than 2"=1,"2 to 5"=2,"6 to 10"=3,"11 to 15"=4,"More than 15"=5),
                           
                           
                           
                           
                           card2=c("American Express"=1,"Visa"=2,"Mastercard"=3,"Discover"=4,"Other"=5),
                           
                           
                           
                           
                           card2type=c("None"=1,"Gold"=2,"Platinum"=3,"Other"=4),
                           
                           
                           
                           card2benefit=c("None"=1,"Cash back"=2,"Airline miles"=3,"Other"=4),
                           card2fee=c("No"=0,"Yes"=1),
                           
                           card2tenure=c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20,"21"=21,"22"=22,"23"=23,"24"=24,"25"=25,"26"=26,"27"=27,"28"=28,"29"=29,"30"=30),
                           card2tenurecat=c("Less than 2"=1,"2 to 5"=2,"6 to 10"=3,"11 to 15"=4,"More than 15"=5),
                           
                           
                           
                           
                           active=c("No"=0,"Yes"=1),
                           
                           bfast=c("Energy bar"=1,"Oatmeal"=2,"Cereal"=3),
                           
                           churn=c("No"=0,"Yes"=1),
                           
                           tollfree=c("No"=0,"Yes"=1),
                           
                           equip=c("No"=0,"Yes"=1),
                           
                           callcard=c("No"=0,"Yes"=1),
                           
                           wireless=c("No"=0,"Yes"=1),
                           
                           multline=c("No"=0,"Yes"=1),
                           
                           voice=c("No"=0,"Yes"=1),
                           
                           pager=c("No"=0,"Yes"=1),
                           
                           internet=c("None"=0,"Dial-up"=1,"DSL"=2,"Cable modem"=3,"Other"=4),
                           callid=c("No"=0,"Yes"=1),
                           
                           callwait=c("No"=0,"Yes"=1),
                           
                           forward=c("No"=0,"Yes"=1),
                           
                           confer=c("No"=0,"Yes"=1),
                           
                           ebill=c("No"=0,"Yes"=1),
                           
                           owntv=c("No"=0,"Yes"=1),
                           
                           ownvcr=c("No"=0,"Yes"=1),
                           
                           owndvd=c("No"=0,"Yes"=1),
                           
                           owncd=c("No"=0,"Yes"=1),
                           
                           ownpda=c("No"=0,"Yes"=1),
                           
                           ownpc=c("No"=0,"Yes"=1),
                           
                           ownipod=c("No"=0,"Yes"=1),
                           
                           owngame=c("No"=0,"Yes"=1),
                           
                           ownfax=c("No"=0,"Yes"=1),
                           
                           news=c("No"=0,"Yes"=1),
                           
                           response_01=c("No"=0,"Yes"=1),
                           
                           response_02=c("No"=0,"Yes"=1),
                           
                           response_03=c("No"=0,"Yes"=1)
)

cust_dbase$polview<-factor(cust_dbase$polview)
cust_dbase$cardtenure<-factor(cust_dbase$cardtenure)

cust_dbase$cardtenurecat<-factor(cust_dbase$cardtenurecat)

cust_dbase$card2<-factor(cust_dbase$card2)


cust_dbase$card2type<-factor(cust_dbase$card2type)

cust_dbase$card2benefit<-factor(cust_dbase$card2benefit)

cust_dbase$card2fee<-factor(cust_dbase$card2fee)

cust_dbase$card2tenure<-factor(cust_dbase$card2tenure)

cust_dbase$card2tenurecat<-factor(cust_dbase$card2tenurecat)

cust_dbase$active<-factor(cust_dbase$active)

cust_dbase$bfast<-factor(cust_dbase$bfast)

cust_dbase$churn<-factor(cust_dbase$churn)

cust_dbase$tollfree<-factor(cust_dbase$tollfree)

cust_dbase$equip<-factor(cust_dbase$equip)

cust_dbase$callcard<-factor(cust_dbase$callcard)

cust_dbase$wireless<-factor(cust_dbase$wireless)

cust_dbase$multline<-factor(cust_dbase$multline)

cust_dbase$voice<-factor(cust_dbase$voice)

cust_dbase$pager<-factor(cust_dbase$pager)

cust_dbase$internet<-factor(cust_dbase$internet)

cust_dbase$callid<-factor(cust_dbase$callid)

cust_dbase$callwait<-factor(cust_dbase$callwait)

cust_dbase$forward<-factor(cust_dbase$forward)

cust_dbase$confer<-factor(cust_dbase$confer)

cust_dbase$ebill<-factor(cust_dbase$ebill)

cust_dbase$owntv<-factor(cust_dbase$owntv)

cust_dbase$ownvcr<-factor(cust_dbase$ownvcr)

cust_dbase$owndvd<-factor(cust_dbase$owndvd)

cust_dbase$owncd<-factor(cust_dbase$owncd)

cust_dbase$ownpda<-factor(cust_dbase$ownpda)

cust_dbase$ownpc<-factor(cust_dbase$ownpc)

cust_dbase$ownipod<-factor(cust_dbase$ownipod)

cust_dbase$owngame<-factor(cust_dbase$owngame)

cust_dbase$ownfax<-factor(cust_dbase$ownfax)

cust_dbase$news<-factor(cust_dbase$news)

cust_dbase$response_01<-factor(cust_dbase$response_01)

cust_dbase$response_02<-factor(cust_dbase$response_02)

cust_dbase$response_03<-factor(cust_dbase$response_03)






cust_dbase$polparty<-factor(cust_dbase$polparty)

cust_dbase$polcontrib<-factor(cust_dbase$polcontrib)

cust_dbase$vote<-factor(cust_dbase$vote)

cust_dbase$card<-factor(cust_dbase$card)

cust_dbase$cardtype<-factor(cust_dbase$cardtype)

cust_dbase$cardbenefit<-factor(cust_dbase$cardbenefit)

cust_dbase$cardfee<-factor(cust_dbase$cardfee)

cust_dbase$cardtenure<-factor(cust_dbase$cardtenure)

cust_dbase$region<-factor(cust_dbase$region)
cust_dbase$townsize<-factor(cust_dbase$townsize)

cust_dbase$agecat<-factor(cust_dbase$agecat)

cust_dbase$birthmonth<-factor(cust_dbase$birthmonth)

cust_dbase$edcat<-factor(cust_dbase$edcat)
cust_dbase$jobcat<-factor(cust_dbase$jobcat)

cust_dbase$union<-factor(cust_dbase$union)

cust_dbase$employ<-factor(cust_dbase$employ)

cust_dbase$empcat<-factor(cust_dbase$empcat)

cust_dbase$retire<-factor(cust_dbase$retire)

cust_dbase$inccat<-factor(cust_dbase$inccat)

cust_dbase$default<-factor(cust_dbase$default)

cust_dbase$jobsat<-factor(cust_dbase$jobsat)
cust_dbase$marital<-factor(cust_dbase$marital)
cust_dbase$spousedcat<-factor(cust_dbase$spousedcat)
cust_dbase$homeown<-factor(cust_dbase$homeown)
cust_dbase$hometype<-factor(cust_dbase$hometype)
cust_dbase$address<-factor(cust_dbase$address)
cust_dbase$addresscat<-factor(cust_dbase$addresscat)
cust_dbase$cars<-factor(cust_dbase$cars)
cust_dbase$carown<-factor(cust_dbase$carown)
cust_dbase$cartype<-factor(cust_dbase$cartype)
cust_dbase$carcatvalue<-factor(cust_dbase$carcatvalue)

cust_dbase$carbought<-factor(cust_dbase$carbought)
cust_dbase$carbuy<-factor(cust_dbase$carbuy)

cust_dbase$commute<-factor(cust_dbase$commute)

cust_dbase$commutecat<-factor(cust_dbase$commutecat)

cust_dbase$commutecar<-factor(cust_dbase$commutecar)

cust_dbase$commutemotorcycle<-factor(cust_dbase$commutemotorcycle)

cust_dbase$commutecarpool<-factor(cust_dbase$commutecarpool)

cust_dbase$commutebus<-factor(cust_dbase$commutebus)

cust_dbase$commuterail<-factor(cust_dbase$commuterail)

cust_dbase$commutepublic<-factor(cust_dbase$commutepublic)

cust_dbase$commutebike<-factor(cust_dbase$commutebike)

cust_dbase$commutewalk<-factor(cust_dbase$commutewalk)

cust_dbase$commutenonmotor<-factor(cust_dbase$commutenonmotor)

cust_dbase$telecommute<-factor(cust_dbase$telecommute)

cust_dbase$reason<-factor(cust_dbase$reason)

cust_dbase$gender<-factor(cust_dbase$gender)

class(cust_dbase$gender)






