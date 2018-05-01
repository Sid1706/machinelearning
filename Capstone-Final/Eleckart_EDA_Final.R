##############################################################################################################################################################################################################################################################################################
#Given Data Set
# FSN ID: The unique identification of each SKU
# Order Date: Date on which the order was placed
# Order ID: The unique identification number of each order
# Order item ID: Suppose you order 2 different products under the same order, it generates 2 different order Item IDs under the same order ID; orders are tracked by the Order Item ID.
# GMV: Gross Merchandise Value or Revenue
# Units: Number of units of the specific product sold
# Order payment type: How the order was paid – prepaid or cash on delivery
# SLA: Number of days it typically takes to deliver the product
# Cust id: Unique identification of a customer
# Product MRP: Maximum retail price of the product
# Product procurement SLA: Time typically taken to procure the product
# Apart from this, the following information is also available:
# Monthly spends on various advertising channels
# Days when there was any special sale
# Monthly NPS score – this may work as a proxy to ‘voice of customer’

###############################################################################################################################################################################################################################################################################

# What is Expected out of this analysis -
# You have to create market mix models for three product sub-categories
# 1. camera accessory,
# 2. home audio and
# 3. gaming accessory.
#Also, the models have to be built at a weekly level.
## Choose the best ones for each of the three product sub-categories and explain them through presentation.

################################################################################################################################################################################################################################################################################################################################


# library(dplyr)
# library(car)
# library(plyr)
# library(zoo)
# library(MASS)
# #library(DAAG)
# #install.packages("DAAG")
# library(DataCombine)

#Import the eleckart dataset
consumerElectronics <- read.csv("ConsumerElectronics.csv", stringsAsFactors = TRUE, header = TRUE)

str(consumerElectronics)
summary(consumerElectronics)


nrow(consumerElectronics) #1648824
ncol(consumerElectronics) # 20
colnames(consumerElectronics)


# Convert Date into POSIXlt
# seggregate the data (order date) into -
# i) Year
# ii)month
# iii) date
# iv ) hour
# v) minute
# Filter data based on the given date range -

#Uncomment below line when executing this code first time.
#install.packages("lubridate") # Could be optimize by using %W


library(lubridate)


#Seggregating the date into hours, minute and date of the month
consumerElectronics$order_date <- as.POSIXct(consumerElectronics$order_date, format = "%Y-%m-%d %H:%M:%S")
consumerElectronics$order_day <- format(consumerElectronics$order_date, "%d")
consumerElectronics$order_hour <-format(consumerElectronics$order_date, "%H")
consumerElectronics$order_minute <-format(consumerElectronics$order_date, "%M")

consumerElectronics$order_week<-lubridate::week(consumerElectronics$order_date)
#This is giving wrong mapping of weeks for saturdays. Need to check.
nrow(consumerElectronics) 
max(consumerElectronics$order_week)


#You have to use the data from July 2015 to June 2016.
consumerElectronics <- subset(consumerElectronics, order_date > "2015-6-30" & order_date < "2016-7-1") #1648215


nrow(consumerElectronics) #1648215
str(consumerElectronics)

#consumerElectronics$Order_date <- date(consumerElectronics$Order_date)  -TODO


#Since the model building will happen on a weekly basis extracing week of the year from the order date
#consumerElectronics$order_week <- week(consumerElectronics$Order_date)

#TODO REvisit this.
consumerElectronics$order_week<- ifelse(consumerElectronics$order_week<=26 & consumerElectronics$Year==2016,consumerElectronics$order_week+53,consumerElectronics$order_week)
 
summary(consumerElectronics$order_week)
nrow(consumerElectronics)
#1648215 Revisit this


sum(is.na(consumerElectronics))#14658 which is a small number and hence dropping all the NA's

# removing rows with NA values
consumerElectronics <- na.omit(consumerElectronics)
nrow(consumerElectronics)#1643311


#TODO
#consumerElectronics$date<-as.Date(consumerElectronics$Order_date)

#Analysing product_mrp column
summary(consumerElectronics$product_mrp)
nrow(subset(consumerElectronics,product_mrp ==0))#5290 Which is a small number. Hence filtering them out.
consumerElectronics <- subset(consumerElectronics,product_mrp>0)

#Analyzing the dependent variable gmv column
nrow(subset(consumerElectronics,gmv ==0))#985 so filter them out as well.
consumerElectronics <- subset(consumerElectronics,gmv>0)
nrow(consumerElectronics)

View(consumerElectronics)


#abc<- sqldf:: sqldf("select * from consumerElectronics group by fsn_id")
# Fact: you can sell items at a price less than or equal to product_mrp and not go beyond that.
# check if gmv is > units in product_mrp
nrow(subset(consumerElectronics, gmv > product_mrp*units)) #33632
View(subset(consumerElectronics, gmv > product_mrp*units))
#Remove all such records from the dataset.
consumerElectronics <- subset(consumerElectronics, gmv <= product_mrp*units)
nrow(consumerElectronics)#1603404

#Derived Metrics /KPI

#Payment mode Prepaid =1 and COD =0 TODO: Flip the flag
consumerElectronics$payment_mode<-ifelse(consumerElectronics$s1_fact.order_payment_type=="COD",0,1)
summary(consumerElectronics)
#Remove s1_fact.order_payment_type
#consumerElectronics <- consumerElectronics[,-11]

 library(sqldf)
# 
# total_orders_by_week<- sqldf('select order_week, count(1) as count from consumerElectronics group by order_week')
# total_prepaid_orders_by_week<- sqldf('select order_week, count(1) as count from consumerElectronics group by order_week having payment_mode =1')
# 
# prepaid_order_precentage<- 

summary(consumerElectronics$order_week)
#Calculate the list price
View(consumerElectronics)

consumerElectronics$list_price <- consumerElectronics$gmv/consumerElectronics$units

#Calculate the discount/markdown percentage TODO: Change the name to markdown
consumerElectronics$markdown <- (consumerElectronics$product_mrp-consumerElectronics$list_price)/consumerElectronics$product_mrp


#Removing columns that are not required.
colnames(consumerElectronics)

#1. s1_fact.order_payment_type
#2. product_analytic_super_category
#3. product_analytic_category
#4. product_analytic_vertical
consumerElectronics <- consumerElectronics[,-c(11,15,16,18)]

colnames(consumerElectronics)

#Load the other metadata given.
investment_data <- read.csv("investment.csv", stringsAsFactors = TRUE, header = TRUE)
nps_data<- read.csv("nps.csv", stringsAsFactors = TRUE, header = TRUE)
sale_data<- read.csv("sale.csv", stringsAsFactors = TRUE, header = TRUE)


#Merge NPS data
consumerElectronics<-merge(consumerElectronics,nps_data,by=c("Month","Year"),all.x=TRUE)

#Merge sale_data
sale_data$order_week <- week(ISOdate(sale_data$Year, sale_data$Month, sale_data$Day))
  
sale_data$order_week<- ifelse(sale_data$order_week<=26 & sale_data$Year==2016,sale_data$order_week+53,sale_data$order_week)


promotion_days_in_week <- sqldf("select count(1) count_promotion_days_in_week,order_week from sale_data group by order_week")

sale_data <- merge(sale_data,promotion_days_in_week, by=c("order_week"), all.x = TRUE)
sale_data<- sale_data[,-c(2,3,4,5)]
sale_data<- unique(sale_data)

# 
total_orders_by_week<- sqldf('select order_week, count(1) as orders_by_week from consumerElectronics group by order_week')
total_prepaid_orders_by_week<- sqldf('select order_week, count(1) as prepaid_orders_by_week from consumerElectronics where payment_mode =1 group 
                                     by order_week')
# 
order_aggregation<- sqldf("select a.order_week,a.orders_by_week,b.prepaid_orders_by_week from total_orders_by_week a join 
                                 total_prepaid_orders_by_week b on a.order_week=b.order_week")
  
order_aggregation$prepaid_percentage <- order_aggregation$prepaid_orders_by_week/order_aggregation$orders_by_week
order_aggregation<-order_aggregation[,-c(2,3)]

consumerElectronics<- merge(consumerElectronics,order_aggregation,by="order_week",all.x = TRUE)


gamingAccessoryDF <-consumerElectronics[consumerElectronics$product_analytic_sub_category=="GamingAccessory" ,]

cameraAccessoryDF<-consumerElectronics[consumerElectronics$product_analytic_sub_category=="CameraAccessory",]

homeAudioDF<-consumerElectronics[consumerElectronics$product_analytic_sub_category=="HomeAudio",]

#Writing to 3 different CSV.
write.csv(cameraAccessoryDF, file = "cameraAccessory.csv",row.names=FALSE)
write.csv(homeAudioDF, file = "homeAudio.csv",row.names=FALSE)
write.csv(gamingAccessoryDF, file = "gamAccessory.csv",row.names=FALSE)

########################################################################################################################################################################################################################################################################################################################################################################

# EDA analysis strategy -  

# 1) check total null values 
# 2) check which all columns have null values and how can those null values can be treated 
# 3) check data quality for every column like pin code value is non negative etc 
# 4) Univariate and BiVariate analysis based on the - 
# i)   Year 
# ii)  month
# iii) date 
# iv ) hour
# v)   minute
# 5) Perfrom bivariate analysis for 
# i)   Time vs units
# ii)  shipment type
# iii) pin code vs quantity 

#Univariate,  BiVariate & MultiVariate Analysis 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
  
  library(dplyr)
  ## Number of orders year, month, day, week and hour basis
  order_year_group <-  group_by(consumerElectronics, Year)
order_month_group <- group_by(consumerElectronics,Month) 
order_week_group <-  group_by(consumerElectronics, order_week) 
order_day_group <-   group_by(consumerElectronics,  order_day)
order_hour_group <-  group_by(consumerElectronics, order_hour)
order_pincode_group <- group_by(consumerElectronics, pincode) 
order_fsn_id_group <- group_by(consumerElectronics, fsn_id) 
#order_paymentType_group <- group_by(consumerElectronics, s1_fact.order_payment_type) 



order_year_group_summary <- summarise(order_year_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_year_group_summary, desc(count))

order_month_group_summary <- summarise(order_month_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_month_group_summary, desc(count))


order_week_group_summary <- summarise(order_week_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_week_group_summary, desc(count))


order_day_group_summary <- summarise(order_day_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_day_group_summary, desc(count))

order_hour_group_summary <- summarise(order_hour_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_hour_group_summary, desc(count))


order_pincode_group_summary <- summarise(order_pincode_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_pincode_group_summary, desc(count))

order_fsn_id_group_summary <- summarise(order_fsn_id_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_fsn_id_group_summary, desc(count))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
a<-ggplot(order_year_group_summary, aes(x=Year, y=meangvm, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
b<-ggplot(order_year_group_summary, aes(x=Year, y=mean, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
c<-ggplot(order_year_group_summary, aes(x=Year, y=count, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
d<-ggplot(order_year_group_summary, aes(x=Year, y=sum, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
e<-ggplot(order_year_group_summary, aes(x=Year, y=sumgvm, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")

#install.packages("cowplot")
library("cowplot")
plot_grid(a, b, c,d,e , 
          labels = c("A", "B", "C", "D","E"),
          ncol = 2, nrow = 3)


#a1<-ggplot(order_month_group_summary, aes(x=Month, y=meangvm, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
b1<-ggplot(order_month_group_summary, aes(x=Month, y=mean, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_month_group_summary, aes(x=Month, y=count, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_month_group_summary, aes(x=Month, y=sum, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_month_group_summary, aes(x=Month, y=sumgvm, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")

plot_grid(b1, c1,d1,e1 ,
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)


#a1<-ggplot(order_day_group_summary, aes(x=order_day, y=meangvm, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
#b1<-ggplot(order_day_group_summary, aes(x=order_day, y=mean, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_day_group_summary, aes(x=order_day, y=count, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_day_group_summary, aes(x=order_day, y=sum, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_day_group_summary, aes(x=order_day, y=sumgvm, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")

plot_grid( c1,d1,e1 , 
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)


a1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=meangvm, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
b1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=mean, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=count, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=sum, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=sumgvm, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")

plot_grid(a1, b1, c1,d1,e1 , 
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)

a1<-ggplot(order_week_group_summary, aes(x=order_week, y=order_week, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
b1<-ggplot(order_week_group_summary, aes(x=order_week, y=mean, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_week_group_summary, aes(x=order_week, y=count, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_week_group_summary, aes(x=order_week, y=sum, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_week_group_summary, aes(x=order_week, y=sumgvm, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")

plot_grid(a1, b1, c1,d1,e1 , 
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#############################################################################################################################


