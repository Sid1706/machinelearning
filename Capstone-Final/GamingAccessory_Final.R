
library(lubridate)
library(sqldf)

Cross_game<- rep(0,8)
sale_data<- read.csv("sale.csv", stringsAsFactors = TRUE, header = TRUE)


#Merge sale_data
sale_data$order_week <- week(ISOdate(sale_data$Year, sale_data$Month, sale_data$Day))

sale_data$order_week<- ifelse(sale_data$order_week<=26 & sale_data$Year==2016,sale_data$order_week+53,sale_data$order_week)


promotion_days_in_week <- sqldf("select count(1) count_promotion_days_in_week,order_week from sale_data group by order_week")

sale_data <- merge(sale_data,promotion_days_in_week, by=c("order_week"), all.x = TRUE)
sale_data<- sale_data[,-c(2,3,4,5)]
sale_data<- unique(sale_data)

gamingAccessory <- read.csv("gamAccessory.csv", stringsAsFactors = TRUE, header = TRUE)




############################################################################################################################################
gamingAccessory1 <- sqldf("select order_week, sum(gmv) as gmv from gamingAccessory group by order_week ")

#SLA,DeliverybDays and DeliveryCDays, procurement sla cannot be aggregated at the week level and hence not considering them

gamingAccessory2<- sqldf("select order_week,avg(list_price) as list_price,avg(product_mrp) as product_mrp,avg(units) as units,avg(sla) as sla,
                             avg(markdown) as markdown,avg(product_procurement_sla) as product_procurement_sla,avg(prepaid_percentage) as prepaid_percentage,
                             avg(NPS_Score) as NPS_Score from gamingAccessory
                             group by order_week")

gamingAccessory2 <- merge(gamingAccessory2,sale_data, by="order_week",all.x = TRUE)

gamingAccessory2$count_promotion_days_in_week[is.na(gamingAccessory2$count_promotion_days_in_week)] <-0.001

gamingAccessory3 <- merge(gamingAccessory1,gamingAccessory2, by="order_week", all.x = TRUE)

# # Define Adstock Rate
# adstock_rate = 0.50
# 
# # Create Data
# advertising = c(117.913, 120.112, 125.828, 115.354, 177.090, 141.647, 137.892,   0.000,   0.000,   0.000,   0.000, 
#                 0.000,   0.000,   0.000,   0.000,   0.000,   0.000, 158.511, 109.385,  91.084,  79.253, 102.706, 
#                 78.494, 135.114, 114.549,  87.337, 107.829, 125.020,  82.956,  60.813,  83.149,   0.000,   0.000, 
#                 0.000,   0.000,   0.000,   0.000, 129.515, 105.486, 111.494, 107.099,   0.000,   0.000,   0.000, 
#                 0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000)
# 
# # Calculate Advertising Adstock
# # Credit: http://stackoverflow.com/questions/14372880/simple-examples-of-filter-function-recursive-option-specifically
# adstocked_advertising = filter(x=advertising, filter=adstock_rate, method="recursive")


alldays <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weeks<- data.frame(week(alldays), month(alldays), year(alldays))
colnames(weeks) <- c("order_week","Month","year")
weeks$order_week<- ifelse(weeks$order_week<=26 & weeks$year==2016,weeks$order_week +53,weeks$order_week)

weeks1<- sqldf("select order_week,month from weeks group by order_week")


investment_data <- read.csv("investment.csv", stringsAsFactors = TRUE, header = TRUE)
str(investment_data)
sum(is.na(investment_data))
colnames(investment_data)


# dividing the monthly spend by 4.3 (avg number of weeks in a month) to make it weekly
investment_data <- cbind(Month=investment_data[,c(2)],
                         investment_data[,-c(1,2)]/4.30)
# Add weekly information
investment_data_weekly <- merge(weeks1,investment_data, by='Month',
                                all.x = TRUE)
#adstock_rate = 0.90
investment_data_weekly$adTV <- stats::filter(x=investment_data_weekly$TV, filter=0.8, method="recursive")
investment_data_weekly$adDigital <- stats::filter(x=investment_data_weekly$Digital, filter=0.4, method="recursive")
investment_data_weekly$adSponsorship <- stats::filter(x=investment_data_weekly$Sponsorship, filter=0.6, method="recursive")
investment_data_weekly$adContent.Marketing <-stats::filter(x=investment_data_weekly$Content.Marketing, filter=0.65, method="recursive")
investment_data_weekly$adOnline.marketing <- stats::filter(x=investment_data_weekly$Online.marketing, filter=0.3, method="recursive")
investment_data_weekly$adAffiliates <- stats::filter(x=investment_data_weekly$Affiliates, filter=0.4, method="recursive")
investment_data_weekly$adSEM <- stats::filter(x=investment_data_weekly$SEM, filter=0.3, method="recursive")
investment_data_weekly$adRadio <- stats::filter(x=investment_data_weekly$Radio, filter=0.2, method="recursive")
investment_data_weekly$adOther <- stats::filter(x=investment_data_weekly$Other, filter=0.2, method="recursive")

head(investment_data_weekly)

#MERGE investment_data_weekly with the main dataset.
gamingAccessory_intermediate <- merge(gamingAccessory3 , investment_data_weekly,by.x = "order_week")

######################## EDA PLOTS ###################################################################################################
colnames(gamingAccessory_intermediate)


data_week <- sqldf("select order_week as week,sum(gmv) as gmv,avg(product_mrp) as product_mrp,
                   avg(markdown) as markdown,avg(sla) as sla,avg(product_procurement_sla) as product_procurement_sla,
                   avg(count_promotion_days_in_week) as count_promotion_days_in_week,TV=avg(TV) as TV,Digital=avg(Digital) as Digital,
                   avg(Sponsorship) as Sponsorship,avg('Content.Marketing') as 'Content.Marketing',
                   avg('Online.marketing') as 'Online.Marketing',avg(Affiliates) as Affiliates,avg(SEM) as SEM,
                   avg(Radio) as Radio,avg(Other) as Other,avg('Total.Investment') as 'Total.Investment',
                   avg(NPS_Score) as NPS_Score,avg(list_price) as list_price,
                   sum(units) as units,sum(prepaid_percentage) as prepaid_percentage from gamingAccessory_intermediate group by order_week")

#Scale the media investment values in crores to make it real.
data_week[,c(8:17)] <- data_week[,c(8:17)]*10000000

#PLOT1: Analyze if there is an impact of special sales on GMV.
quant <- quantile(data_week$gmv,c(0.25,0.5,0.75))

matplot(data_week$week, cbind(data_week$gmv,rep(quant[1],52),rep(quant[2],52)),
        type='l',lwd=2,xlab = 'week',ylab = 'gmv')
saledays <- data_week$week[data_week$count_promotion_days_in_week > 1]
abline(v=saledays,col='blue',lwd=2)
legend('topright', inset = 0, legend = c('GMV','25th percentile',
                                         '75th percentile','sale days'),
       lty = c(1:4), col=c(1,2,3,4), lwd = 2, cex = 0.75)

#You can observe that the GMV increases in line with the sale days except for the first two as that could be initial sales where people
#were not aware of the sale. Additionally we can observe that business as usual gradually increasing from each sale

#PLOT2: Plotting variation of the list mrp and product mrp over the period and look for trends during sale.
matplot(data_week$week, cbind(data_week$product_mrp,data_week$list_price),
        type='l',lwd = 2,xlab='week',ylab='price')
abline(v=saledays,col='green',lwd=2)
legend('topright', inset = 0, legend = c('product mrp','list price','sale days'),
       lty = c(1:3), lwd = 2, col=c(1,2,3), horiz = TRUE, cex = 0.75)
#You can observe that the list_mrp is reduced significantly during the sale except for the first sale indicating that they did not know 
# how to conduct a sale for the first time and have learnt the lessions consequently.

#PLOT3: NPS
quant <- quantile(data_week$NPS_Score,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$NPS_Score,rep(quant[1],52),
                              rep(quant[2],52),rep(quant[3],52)),
        type='l',lwd=2,xlab = 'week',ylab = 'NPS')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('NPS','25th Percentile','50th Percentile',
                                         '75th Percentile','sale days'),
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)

#NPS plot shows that first sale customers were satisfied as they were less in number and were served well. 
#However the next 2 sales suddenly due to too many customers coming to the platform they were not prepared 
#to server them. NPS increased in the later sales as the company was better prepared.

#PLOT4: discount inflation 
quant <- quantile(data_week$markdown,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$markdown,rep(quant[1],52),
                              rep(quant[2],52),rep(quant[3],52)),
        type='l',lwd=2,xlab = 'week',ylab = 'Discount')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('Discount','25th Percentile',
                                         '50th Percentile','75th Percentile',
                                         'Sale days'),
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)

#Shows clear indication that discount being provided inline with the sales. Also discounts were very deep in the initial sales.


#PLOT5: sales volume
quant <- quantile(data_week$units,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$units,rep(quant[1],52),
                              rep(quant[2],52),rep(quant[3],52)),
        type='l',lwd=2,xlab = 'week',ylab = 'Sales Volume')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('Sales Volume','25th Percentile',
                                         '50th Percentile','75th Percentile',
                                         'Sale days'),
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)
#We can see a spike in sales volumes whenever there are sales. Hence sales can be considered as a factor that impacts sales.

######################## Leniar Model ###################################################################################################

colnames(gamingAccessory_intermediate)
str(gamingAccessory_intermediate)
Linear_model<-  sapply(gamingAccessory_intermediate, function(x) scale(x))

colnames(Linear_model)
Linear_model <- data.frame(Linear_model[,-c(1,5,12,13,23:31)])

sum(is.na(Linear_model))
head(Linear_model)

model_1 <- lm(gmv~.,Linear_model)


################################################################################################

# Summary of Linear Model 
summary(model_1)


library(car)
library(MASS)

model_2 <- stepAIC(model_1,direction = "both")
summary(model_2) 
vif(model_2)


#NPS_Score
model_3 <- lm(formula = gmv ~ list_price + product_mrp + sla + markdown + 
                product_procurement_sla + prepaid_percentage  + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                SEM + Other, data = Linear_model)

summary(model_3) 
vif(model_3)

#markdown
model_4 <- lm(formula = gmv ~ list_price + product_mrp + sla  + 
                product_procurement_sla + prepaid_percentage  + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                SEM + Other, data = Linear_model)



summary(model_4) 
vif(model_4)

#SEM
model_5 <- lm(formula = gmv ~ list_price + product_mrp + sla  + 
                product_procurement_sla + prepaid_percentage  + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing
                 + Other, data = Linear_model)



summary(model_5) 
vif(model_5)

#sla
model_6 <- lm(formula = gmv ~ list_price + product_mrp  + 
                product_procurement_sla + prepaid_percentage  + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing
              + Other, data = Linear_model)



summary(model_6) 
vif(model_6)

#list_price
model_7 <- lm(formula = gmv ~   product_mrp  + 
                product_procurement_sla + prepaid_percentage  + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing
              + Other, data = Linear_model)



summary(model_7) 
vif(model_7)

#prepaid_percentage
model_8 <- lm(formula = gmv ~   product_mrp  + 
                product_procurement_sla  + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing
              + Other, data = Linear_model)

summary(model_8) 
vif(model_8)

#product_procurement_sla
model_9 <- lm(formula = gmv ~   product_mrp  + 
              TV + Digital + Sponsorship + Content.Marketing + Online.marketing
              + Other, data = Linear_model)

summary(model_9) 
vif(model_9)

#product_mrp
model_10 <- lm(formula = gmv ~ TV + Digital + Sponsorship + Content.Marketing + Online.marketing
              + Other, data = Linear_model)

summary(model_10) 
vif(model_10)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -3.261e-16  1.177e-01   0.000 1.000000    
# TV                -1.100e+00  3.931e-01  -2.798 0.007547 ** 
#   Digital            2.128e+00  8.727e-01   2.438 0.018784 *  
#   Sponsorship        1.153e+00  3.208e-01   3.593 0.000806 ***
#   Content.Marketing -3.035e+00  1.081e+00  -2.808 0.007347 ** 
#   Online.marketing   1.436e+00  4.370e-01   3.286 0.001972 ** 
#   Other              9.380e-01  3.155e-01   2.973 0.004719 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.849 on 45 degrees of freedom
# Multiple R-squared:  0.364,	Adjusted R-squared:  0.2792 
# F-statistic: 4.292 on 6 and 45 DF,  p-value: 0.001672

############################# ELASTICITY ANALYSIS LINEAR MODEL ##############################################################
##Final Model 
Linear_Final_model_gaming <- model_10

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ TV + Digital + Sponsorship + Content.Marketing + Online.marketing
                                                              + Other),m = 10)
Cross_game[1] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- Linear_model


grlm <- Linear_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear Model") +xlab("Variables")

######################## Leniar + AdStock Model ###########################
colnames(gamingAccessory_intermediate)
str(gamingAccessory_intermediate)
AdLinear_model<-  sapply(gamingAccessory_intermediate, function(x) scale(x))

colnames(AdLinear_model)
AdLinear_model <- data.frame(AdLinear_model[,-c(1,5,12,13)])

sum(is.na(AdLinear_model))
head(AdLinear_model)

adModel_1 <- lm(gmv~.,AdLinear_model)

summary(adModel_1)


library(car)
library(MASS)

adModel_2 <- stepAIC(adModel_1,direction = "both")
summary(adModel_2) 
vif<-vif(adModel_2)
vif[order(vif,decreasing = TRUE)]

#Other
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                  TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                  Affiliates + Radio + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Digital
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                  TV  + Sponsorship + Content.Marketing + Online.marketing + 
                  Affiliates + Radio + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Radio
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                  TV  + Sponsorship + Content.Marketing + Online.marketing + 
                  Affiliates + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Online.marketing
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                  TV  + Sponsorship + Content.Marketing  + 
                  Affiliates + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Affiliates
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                  TV  + Sponsorship + Content.Marketing
                   + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                  TV + Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#TV
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla 
                   + Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#markdown
adModel_3 <- lm(formula = gmv ~ product_mrp + sla  + product_procurement_sla 
                + Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#sla
adModel_3 <- lm(formula = gmv ~ product_mrp   + product_procurement_sla 
                + Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

cor(AdLinear_model$adSEM, AdLinear_model$Content.Marketing)
#product_mrp
adModel_3 <- lm(formula = gmv ~ product_procurement_sla 
                + Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
adModel_3 <- lm(formula = gmv ~  
                 Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM + adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adRadio
adModel_3 <- lm(formula = gmv ~  
                  Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM  + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adOther
adModel_3 <- lm(formula = gmv ~  
                  Content.Marketing
                + adTV + adContent.Marketing + 
                  adOnline.marketing + adSEM, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -6.847e-17  1.149e-01   0.000 1.000000    
# Content.Marketing   -1.482e+00  5.849e-01  -2.533 0.014778 *  
#   adTV                -6.154e-01  2.369e-01  -2.598 0.012552 *  
#   adContent.Marketing -1.930e+00  5.045e-01  -3.826 0.000391 ***
#   adOnline.marketing   1.639e+00  3.707e-01   4.423 5.92e-05 ***
#   adSEM                2.639e+00  8.249e-01   3.199 0.002499 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8286 on 46 degrees of freedom
# Multiple R-squared:  0.3807,	Adjusted R-squared:  0.3134 
# F-statistic: 5.655 on 5 and 46 DF,  p-value: 0.0003815

# #Content.Marketing
# adModel_3 <- lm(formula = gmv ~  
#                   
#                 adTV + adContent.Marketing + 
#                   adOnline.marketing + adSEM, data = AdLinear_model)
# 
# summary(adModel_3) 
# vif<-vif(adModel_3)
# vif[order(vif,decreasing = TRUE)]
# 
# #adTV
# adModel_3 <- lm(formula = gmv ~
# 
#                   adContent.Marketing +
#                   adOnline.marketing + adSEM, data = AdLinear_model)
# 
# summary(adModel_3)
# vif<-vif(adModel_3)
# vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          4.649e-17  1.212e-01   0.000 1.000000    
# adContent.Marketing -9.659e-01  3.731e-01  -2.589 0.012713 *  
#   adOnline.marketing   7.371e-01  1.738e-01   4.241 0.000101 ***
#   adSEM                6.489e-01  3.220e-01   2.015 0.049534 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.874 on 48 degrees of freedom
# Multiple R-squared:  0.2811,	Adjusted R-squared:  0.2362 
# F-statistic: 6.257 on 3 and 48 DF,  p-value: 0.001132

############################# ELASTICITY ANALYSIS LINEAR + Ad stock  MODEL ##############################################################
##Final Model 
Linear_Adstock_Final_model_gaming <- adModel_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = AdLinear_model, form.lm = formula(gmv ~ Content.Marketing
                                                                + adTV + adContent.Marketing + 
                                                                  adOnline.marketing + adSEM),m = 10)
Cross_game[2] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- AdLinear_model


grlm <- Linear_Adstock_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear + AdStock Model") +xlab("Variables")

######################## Multiplicative Model #############################################################################################

colnames(gamingAccessory_intermediate)
str(gamingAccessory_intermediate)
#Multply by 1 crore
gamingAccessory_intermediate[,c(13:22)]<-gamingAccessory_intermediate[,c(13:22)]*10000000 
#Take log of the data 
multiplicative_model <-log(gamingAccessory_intermediate + 0.01) 
#Scale the data
multiplicative_model<-  sapply(multiplicative_model, function(x) scale(x))

colnames(multiplicative_model)
multiplicative_model <- data.frame(multiplicative_model[,-c(1,5,12,13,23:31)])

sum(is.na(multiplicative_model))
head(multiplicative_model)

mm_1 <- lm(gmv~.,multiplicative_model)


################################################################################################

# Summary of multiplicative_model
summary(mm_1)


mm_2 <- stepAIC(mm_1,direction = "both")
summary(mm_1) 
vif<-vif(mm_1)
vif[order(vif,decreasing = TRUE)]

#Radio
mm_3<- lm(formula = gmv ~ product_mrp + prepaid_percentage + NPS_Score + 
           Digital + Online.marketing + Affiliates + SEM  + Other, 
         data = multiplicative_model)
summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

cor(multiplicative_model$Affiliates, multiplicative_model$Online.marketing)

#Affiliates
mm_3<- lm(formula = gmv ~ product_mrp + prepaid_percentage + NPS_Score + 
            Digital + Online.marketing  + SEM  + Other, 
          data = multiplicative_model)
summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]
#NPS_Score
mm_3<- lm(formula = gmv ~ product_mrp + prepaid_percentage  + 
            Digital + Online.marketing  + SEM  + Other, 
          data = multiplicative_model)
summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#Other
mm_3<- lm(formula = gmv ~ product_mrp + prepaid_percentage  + 
            Digital + Online.marketing  + SEM , 
          data = multiplicative_model)
summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
mm_3<- lm(formula = gmv ~ product_mrp  + 
            Digital + Online.marketing  + SEM , 
          data = multiplicative_model)
summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       6.056e-16  8.315e-02   0.000 1.000000    
# product_mrp       3.256e-01  8.963e-02   3.633 0.000692 ***
#   Digital           3.167e-01  1.750e-01   1.810 0.076696 .  
# Online.marketing  9.802e-01  1.216e-01   8.061 2.07e-10 ***
#   SEM              -5.129e-01  2.046e-01  -2.507 0.015699 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5996 on 47 degrees of freedom
# Multiple R-squared:  0.6687,	Adjusted R-squared:  0.6405 
# F-statistic: 23.71 on 4 and 47 DF,  p-value: 8.895e-11

############################# ELASTICITY ANALYSIS Multiplicative  MODEL ##############################################################
##Final Model 
Multiplicative_Final_model_gaming <- mm_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = multiplicative_model, form.lm = formula(gmv ~ product_mrp  + 
                                                                  Digital + Online.marketing  + SEM),m = 10)
Cross_game[3] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- multiplicative_model


grlm <- Multiplicative_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables")



############################################### Multiplicative Ad Stock Model #######################################################################
colnames(gamingAccessory_intermediate)
str(gamingAccessory_intermediate)
#Multply by 1 crore
#gamingAccessory_intermediate[,c(13:22)]<-gamingAccessory_intermediate[,c(13:22)]*10000000 
#Take log of the data 
adMultiplicative_model <-log(gamingAccessory_intermediate + 0.01) 
#Scale the data
adMultiplicative_model<-  sapply(adMultiplicative_model, function(x) scale(x))

colnames(adMultiplicative_model)
adMultiplicative_model <- data.frame(adMultiplicative_model[,-c(1,5,12,13)])

sum(is.na(adMultiplicative_model))
head(adMultiplicative_model)

admm_1 <- lm(gmv~.,adMultiplicative_model)
summary(admm_1)


admm_2 <- stepAIC(admm_1,direction = "both")
summary(admm_2) 
vif<-vif(admm_2)
vif[order(vif,decreasing = TRUE)]

#Other
admm_3<- lm(formula = gmv ~ sla + markdown + prepaid_percentage + NPS_Score + 
              count_promotion_days_in_week + Sponsorship + Online.marketing + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
admm_3<- lm(formula = gmv ~ sla + markdown + prepaid_percentage + NPS_Score + 
              count_promotion_days_in_week  + Online.marketing + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Online.marketing
admm_3<- lm(formula = gmv ~ sla + markdown + prepaid_percentage + NPS_Score + 
              count_promotion_days_in_week + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
admm_3<- lm(formula = gmv ~ sla + markdown + prepaid_percentage + 
              count_promotion_days_in_week + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#markdown
admm_3<- lm(formula = gmv ~ sla  + prepaid_percentage + 
              count_promotion_days_in_week + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
admm_3<- lm(formula = gmv ~ sla  + 
              count_promotion_days_in_week + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week
admm_3<- lm(formula = gmv ~ sla  + 
              Affiliates + adDigital + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#adDigital
admm_3<- lm(formula = gmv ~ sla  + 
              Affiliates  + adOnline.marketing + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#adOnline.marketing
admm_3<- lm(formula = gmv ~ sla  + 
              Affiliates + adAffiliates + 
              adSEM, data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#adSEM
admm_3<- lm(formula = gmv ~ sla  + 
              Affiliates + adAffiliates
              , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]


#Affiliates
admm_3<- lm(formula = gmv ~ sla  
               + adAffiliates
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.466e-16  8.583e-02   0.000   1.0000    
# sla           2.184e-01  8.969e-02   2.435   0.0186 *  
#   adAffiliates  7.102e-01  8.969e-02   7.918 2.55e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6189 on 49 degrees of freedom
# Multiple R-squared:  0.6319,	Adjusted R-squared:  0.6169 
# F-statistic: 42.06 on 2 and 49 DF,  p-value: 2.319e-11

############################# ELASTICITY ANALYSIS Multiplicative +Adstock MODEL ##############################################################
##Final Model 
Multiplicative_Adstock_Final_model_gaming <- admm_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = adMultiplicative_model, form.lm = formula(gmv ~ sla  
                                                                        + adAffiliates),m = 10)
Cross_game[4] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- adMultiplicative_model


grlm <- Multiplicative_Adstock_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative +Ad Stock Model") +xlab("Variables")



###################################### Koyck Model ########################################################################################################
colnames(gamingAccessory_intermediate)
str(gamingAccessory_intermediate)

#Get the dataset
koyck_model <- data.frame(gamingAccessory_intermediate[,-c(1,5,12,13,23:31)])

#generate the lag for gmv
#cameraAccessorylog$gmvLag  <- shift(cameraAccessorylog$gmv,1, NA,type="lag")

koyck_model$lag_gmv <- data.table::shift(koyck_model$gmv,1,NA,type="lag")
sum(is.na(koyck_model))
koyck_model<- na.omit(koyck_model)


#scale the dataset
koyck_model<-  sapply(koyck_model, function(x) scale(x))

sum(is.na(koyck_model))

head(koyck_model)

koyck_model<- data.frame(koyck_model)

kyockm_1 <- lm(gmv~.,koyck_model)


################################################################################################

# Summary of Linear Model 
summary(kyockm_1)

kyockm_2 <- stepAIC(kyockm_1,direction = "both")
summary(kyockm_2) 
vif<- vif(kyockm_2)
vif[order(vif,decreasing = TRUE)]

#list_price
kyockm_3 <- lm(formula = gmv ~   product_mrp + sla + markdown + 
                 product_procurement_sla + prepaid_percentage + TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates + SEM + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
kyockm_3 <- lm(formula = gmv ~   product_mrp + sla + markdown + 
                  + prepaid_percentage + TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates + SEM + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_mrp
kyockm_3 <- lm(formula = gmv ~     sla + markdown 
                 + prepaid_percentage + TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates + SEM + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#sla
kyockm_3 <- lm(formula = gmv ~ markdown  
                 + prepaid_percentage + TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates + SEM + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#SEM
kyockm_3 <- lm(formula = gmv ~ markdown  
                 + prepaid_percentage + TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates  + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
kyockm_3 <- lm(formula = gmv ~ markdown 
                  + TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates  + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

cor(koyck_model$Content.Marketing, koyck_model$markdown)
#markdown
kyockm_3 <- lm(formula = gmv ~  TV + Digital + 
                 Sponsorship + Content.Marketing + Affiliates  + Other, 
               data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        3.642e-16  1.131e-01   0.000 1.000000    
# TV                -1.555e+00  4.184e-01  -3.716 0.000567 ***
#   Digital            2.559e+00  8.052e-01   3.178 0.002713 ** 
#   Sponsorship        1.546e+00  3.430e-01   4.507 4.81e-05 ***
#   Content.Marketing -3.510e+00  9.760e-01  -3.597 0.000811 ***
#   Affiliates         1.607e+00  3.918e-01   4.100 0.000175 ***
#   Other              1.161e+00  3.094e-01   3.752 0.000510 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8079 on 44 degrees of freedom
# Multiple R-squared:  0.4256,	Adjusted R-squared:  0.3473 
# F-statistic: 5.434 on 6 and 44 DF,  p-value: 0.0002832

############################# ELASTICITY ANALYSIS Kyock MODEL ###############################################################################################
##Final Model 
Koyck_Final_model_gaming <- kyockm_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = koyck_model, form.lm = formula(gmv ~ TV + Digital + 
                                                               Sponsorship + Content.Marketing + Affiliates  + Other),m = 10)
Cross_game[5] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- koyck_model


grlm <- Koyck_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Koyck Model") +xlab("Variables")

###################################### Kyock + Adstock Model #######################################################################
colnames(gamingAccessory_intermediate)
str(gamingAccessory_intermediate)

#Get the dataset
ad_koyck_model <- data.frame(gamingAccessory_intermediate[,-c(1,5,12,13)])

#generate the lag for gmv
#cameraAccessorylog$gmvLag  <- shift(cameraAccessorylog$gmv,1, NA,type="lag")

ad_koyck_model$lag_gmv <- data.table::shift(ad_koyck_model$gmv,1,NA,type="lag")
sum(is.na(ad_koyck_model))
ad_koyck_model<- na.omit(ad_koyck_model)


#scale the dataset
ad_koyck_model<-  sapply(ad_koyck_model, function(x) scale(x))

sum(is.na(ad_koyck_model))

head(ad_koyck_model)

ad_koyck_model<- data.frame(ad_koyck_model)

ad_kyockm_1 <- lm(gmv~.,ad_koyck_model)


################################################################################################

# Summary of Linear Model 
summary(kyockm_1)

ad_kyockm_2 <- stepAIC(ad_kyockm_1,direction = "both")
summary(ad_kyockm_2) 
vif<- vif(ad_kyockm_2)
vif[order(vif,decreasing = TRUE)]

#Other
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                    TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + Radio + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM + adRadio + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Digital
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                    TV  + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + Radio + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM + adRadio + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Radio
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                    TV  + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM + adRadio + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adRadio
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla + 
                    TV  + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + markdown  + 
                    TV  + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_mrp
ad_kyockm_3 <- lm(formula = gmv ~  sla + markdown  + 
                    TV  + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#sla
ad_kyockm_3 <- lm(formula = gmv ~  markdown  + 
                    TV  + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#TV
ad_kyockm_3 <- lm(formula = gmv ~  markdown 
                      + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#markdown
ad_kyockm_3 <- lm(formula = gmv ~   
                  Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Online.marketing + 
ad_kyockm_3 <- lm(formula = gmv ~   
                    Sponsorship + Content.Marketing + 
                    Affiliates  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Affiliates
ad_kyockm_3 <- lm(formula = gmv ~   
                    Sponsorship + Content.Marketing
                      + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
ad_kyockm_3 <- lm(formula = gmv ~   
                     Content.Marketing
                  + adTV + adContent.Marketing + 
                    adOnline.marketing + adSEM  + adOther, data = ad_koyck_model)
summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.976e-17  1.069e-01   0.000 1.000000    
# Content.Marketing   -3.009e+00  7.275e-01  -4.136 0.000156 ***
#   adTV                -1.004e+00  2.506e-01  -4.007 0.000234 ***
#   adContent.Marketing -2.837e+00  5.526e-01  -5.133 6.20e-06 ***
#   adOnline.marketing   2.177e+00  3.805e-01   5.722 8.64e-07 ***
#   adSEM                4.830e+00  1.044e+00   4.629 3.25e-05 ***
#   adOther              4.640e-01  1.656e-01   2.802 0.007513 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7634 on 44 degrees of freedom
# Multiple R-squared:  0.4872,	Adjusted R-squared:  0.4172 
# F-statistic: 6.966 on 6 and 44 DF,  p-value: 2.988e-05

# #adOther
# ad_kyockm_3 <- lm(formula = gmv ~   
#                     Content.Marketing
#                   + adTV + adContent.Marketing + 
#                     adOnline.marketing + adSEM, data = ad_koyck_model)
# summary(ad_kyockm_3) 
# vif<- vif(ad_kyockm_3)
# vif[order(vif,decreasing = TRUE)]
# 
# #adSEM
# ad_kyockm_3 <- lm(formula = gmv ~
#                     Content.Marketing
#                   + adTV + adContent.Marketing +
#                     adOnline.marketing , data = ad_koyck_model)
# summary(ad_kyockm_3)
# vif<- vif(ad_kyockm_3)
# vif[order(vif,decreasing = TRUE)]
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)         -7.332e-17  1.277e-01   0.000  1.00000   
# Content.Marketing    2.159e-01  2.450e-01   0.881  0.38269   
# adTV                -4.398e-02  1.691e-01  -0.260  0.79590   
# adContent.Marketing -4.705e-01  2.557e-01  -1.840  0.07225 . 
# adOnline.marketing   6.165e-01  2.042e-01   3.019  0.00413 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9117 on 46 degrees of freedom
# Multiple R-squared:  0.2353,	Adjusted R-squared:  0.1688 
# F-statistic: 3.539 on 4 and 46 DF,  p-value: 0.01339
############################# ELASTICITY ANALYSIS Kyock + Adstock MODEL ##############################################################
##Final Model 
Koyck_Adstock_Final_model_gaming <- ad_kyockm_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = ad_koyck_model, form.lm = formula(gmv ~ Content.Marketing
                                                                + adTV + adContent.Marketing + 
                                                                  adOnline.marketing + adSEM  + adOther),m = 10)
Cross_game[6] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- ad_koyck_model


grlm <- Koyck_Adstock_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Koyck _ AdStock Model") +xlab("Variables")

###################################### FEATURE ENGINEERING PART - 2:######################################

###################################### Moving Averages  & DISTRIBUTED LAG MODEL######################################

#install.packages("TTR")
library(TTR)
gamingAccessory_intermediate$list_price_maOne <- runMean(gamingAccessory_intermediate$list_price,1)
gamingAccessory_intermediate$list_price_ma1 <- runMean(gamingAccessory_intermediate$list_price,2)
gamingAccessory_intermediate$list_price_ma2 <- runMean(gamingAccessory_intermediate$list_price,3)
gamingAccessory_intermediate$list_price_ma3 <- runMean(gamingAccessory_intermediate$list_price,4)

gamingAccessory_intermediate$markdown_ma1 <- runMean(gamingAccessory_intermediate$markdown,2)
gamingAccessory_intermediate$markdown_ma2 <- runMean(gamingAccessory_intermediate$markdown,3)
gamingAccessory_intermediate$markdown_ma3 <- runMean(gamingAccessory_intermediate$markdown,4)


#Shelf price inflationith week = List price ith week / List price (i-1)th week
#TODO:Revisit This
gamingAccessory_intermediate$shelf_price_inflation_1= gamingAccessory_intermediate$list_price/gamingAccessory_intermediate$list_price_maOne
gamingAccessory_intermediate$shelf_price_inflation_2= gamingAccessory_intermediate$list_price/gamingAccessory_intermediate$list_price_ma1
gamingAccessory_intermediate$shelf_price_inflation_3= gamingAccessory_intermediate$list_price/gamingAccessory_intermediate$list_price_ma2
gamingAccessory_intermediate$shelf_price_inflation_4= gamingAccessory_intermediate$list_price/gamingAccessory_intermediate$list_price_ma3

#% discount offered ith week = Discounted price ith week / List price ith week ------(wrt List price)
#% discount offered ith week = Discounted price ith week / MRP ith week ------(wrt MRP)

gamingAccessory_intermediate$markdown_inflation1<-
  (gamingAccessory_intermediate$markdown - gamingAccessory_intermediate$markdown_ma1)/gamingAccessory_intermediate$markdown_ma1
gamingAccessory_intermediate$markdown_inflation2<-
  (gamingAccessory_intermediate$markdown - gamingAccessory_intermediate$markdown_ma2)/gamingAccessory_intermediate$markdown_ma2

gamingAccessory_intermediate$markdown_inflation1<-
  (gamingAccessory_intermediate$markdown - gamingAccessory_intermediate$markdown_ma3)/gamingAccessory_intermediate$markdown_ma3


#install.packages("DataCombine")
library(DataCombine)

#List of list price by 1,2,3 dates (Date values are ordered)
#Previous List price
gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "list_price",slideBy = -1)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "list_price",slideBy = -2)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "list_price", slideBy = -3)

#9.lag the promotion variables

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "markdown", slideBy = -1)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "markdown", slideBy = -2)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "markdown", slideBy = -3)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "NPS_Score", slideBy = -1)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "NPS_Score", slideBy = -2)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "NPS_Score", slideBy = -3)


gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "count_promotion_days_in_week", slideBy = -1)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "count_promotion_days_in_week", slideBy = -2)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "count_promotion_days_in_week", slideBy = -3)


gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "gmv",slideBy = -1)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "gmv",slideBy = -2)

gamingAccessory_intermediate <- slide(gamingAccessory_intermediate, Var = "gmv",slideBy = -3)



sum(is.na(gamingAccessory_intermediate))
gamingAccessory_final <- na.omit(gamingAccessory_intermediate)



sum(is.na(gamingAccessory_final))

head(gamingAccessory_final)
Linear_model<-  sapply(gamingAccessory_final, function(x) scale(x))
Linear_model<- data.frame(Linear_model[, -c(1,5,12,13)])
Linear_model<-na.omit(Linear_model)
sum(is.na(Linear_model))   
head(Linear_model)
model_1 <- lm(gmv~.,Linear_model)   


# Summary of Linear Model
summary(model_1)   
library(car)
library(MASS)

model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
vif(model_2)

#list_price
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates + SEM +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + adOther +
               list_price_ma1 + list_price_ma2 + list_price_ma3 + markdown_ma1 +
               markdown_ma2 + markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adonline.marketing
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates + SEM +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates + adSEM + adRadio + adOther +
               list_price_ma1 + list_price_ma2 + list_price_ma3 + markdown_ma1 +
               markdown_ma2 + markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown_ma2
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates + SEM +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates + adSEM + adRadio + adOther +
               list_price_ma1 + list_price_ma2 + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#adRadio
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates + SEM +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates + adSEM  + adOther +
               list_price_ma1 + list_price_ma2 + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





#SEM
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates + adSEM  + adOther +
               list_price_ma1 + list_price_ma2 + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#Online.marketing
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates + adSEM  + adOther +
               list_price_ma1 + list_price_ma2 + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#list_price_ma2
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates + adSEM  + adOther +
               list_price_ma1  + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adSEM
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates   + adOther +
               list_price_ma1  + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3 + shelf_price_inflation_4 + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#shelf_price_inflation_4
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               Radio + adTV + adDigital + adSponsorship + adContent.Marketing 
             + adAffiliates   + adOther +
               list_price_ma1  + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





#adContent.Marketing
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               Radio + adTV + adDigital + adSponsorship 
             + adAffiliates   + adOther +
               list_price_ma1  + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#Radio
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV + adDigital + adSponsorship 
             + adAffiliates   + adOther +
               list_price_ma1  + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 + NPS_Score.1 + NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#NPS_Score.1
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV + adDigital + adSponsorship 
             + adAffiliates   + adOther +
               list_price_ma1  + list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 +  NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#list_price_ma1
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV + adDigital + adSponsorship 
             + adAffiliates   + adOther +
               list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 +  NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#adAffiliates
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV + adDigital + adSponsorship 
             + adOther +
               list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 +  NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#adDigital
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV  + adSponsorship 
             + adOther +
               list_price_ma3 + markdown_ma1 +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 +  NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#markdown_ma1
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV  + adSponsorship 
             + adOther +
               list_price_ma3  +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               markdown_inflation2 +  NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#markdown_inflation2
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV  + adSponsorship 
             + adOther +
               list_price_ma3  +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





#adSponsorship
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               list_price_ma3  +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#NPS_Score
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               list_price_ma3  +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





#list_price_ma3
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               NPS_Score.2 + NPS_Score.3 +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#NPS_Score.3
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 +
               NPS_Score.2  +
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#NPS_Score.2
model_2<- lm(formula = gmv ~  product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#product_mrp
model_2<- lm(formula = gmv ~   sla + markdown +
               product_procurement_sla + prepaid_percentage +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3 + shelf_price_inflation_1 + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#shelf_price_inflation_1
model_2<- lm(formula = gmv ~   sla + markdown +
               product_procurement_sla + prepaid_percentage +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#prepaid_percentage
model_2<- lm(formula = gmv ~   sla + markdown +
               product_procurement_sla  +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#product_procurement_sla
model_2<- lm(formula = gmv ~   sla + markdown +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2 + gmv.3, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#gmv.3
model_2<- lm(formula = gmv ~   sla + markdown +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#sla
model_2<- lm(formula = gmv ~   markdown +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.1 + gmv.2, data = Linear_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#gmv.1
model_2<- lm(formula = gmv ~   markdown +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.2, data = Linear_model)
summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#gmv.1
model_2<- lm(formula = gmv ~   markdown +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 +
               count_promotion_days_in_week.3 + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#count_promotion_days_in_week.2
model_2<- lm(formula = gmv ~   markdown +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + 
               count_promotion_days_in_week.3 + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#count_promotion_days_in_week
model_2<- lm(formula = gmv ~   markdown +
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + 
               count_promotion_days_in_week.3 + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#count_promotion_days_in_week.3
model_2<- lm(formula = gmv ~   markdown +
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + 
               + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown
model_2<- lm(formula = gmv ~  
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3  + shelf_price_inflation_2 +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + 
               + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#shelf_price_inflation_2
model_2<- lm(formula = gmv ~  
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               markdown_ma3   +
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + 
               + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown_ma3
model_2<- lm(formula = gmv ~  
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               
               shelf_price_inflation_3  + markdown_inflation1 + 
               count_promotion_days_in_week.1 + 
               + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week
model_2<- lm(formula = gmv ~  
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               
               shelf_price_inflation_3  + markdown_inflation1 +  
               + gmv.2, data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#gmv.2
model_2<- lm(formula = gmv ~  
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther +
               
               shelf_price_inflation_3  + markdown_inflation1, 
             data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#shelf_price_inflation_3
model_2<- lm(formula = gmv ~  
               TV + Digital + Sponsorship +
               Content.Marketing +  Affiliates +
               adTV   
             + adOther
             + markdown_inflation1, 
             data = Linear_model)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#Estimate Std. Error t value Pr(>|t|)   
# (Intercept)         -1.687e-15  8.624e-02   0.000  1.00000   
#  TV                  -2.096e+00  3.244e-01  -6.462 9.53e-08 ***
#  Digital              4.571e+00  7.059e-01   6.476 9.12e-08 ***
#  Sponsorship          2.566e+00  3.107e-01   8.258 2.94e-10 ***
#  Content.Marketing   -6.390e+00  8.961e-01  -7.132 1.08e-08 ***
#  Affiliates           2.542e+00  3.474e-01   7.316 5.93e-09 ***
#  adTV                -5.100e-01  1.717e-01  -2.971  0.00495 **
#  adOther              1.892e+00  2.652e-01   7.136 1.06e-08 ***
#  markdown_inflation1  6.942e-01  1.179e-01   5.889 6.21e-07 ***

#Residual standard error: 0.6098 on 41 degrees of freedom
#Multiple R-squared:  0.6888,     Adjusted R-squared:  0.6281
#F-statistic: 11.35 on 8 and 41 DF,  p-value: 2.664e-08

############################# ELASTICITY ANALYSIS DISTRIBUTED LAG MODEL ##############################################################
##Final Model 
Distributed_Lag_Final_model_gaming <- model_2

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ TV + Digital + Sponsorship +
                                                                Content.Marketing +  Affiliates +
                                                                adTV   
                                                              + adOther
                                                              + markdown_inflation1),m = 10)
Cross_game[7] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- Linear_model


grlm <- Distributed_Lag_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Distributed Lag Model") +xlab("Variables")


#################################################################################################################################################################################################
###############################################################  Multiplicative and Distibuted lag ##########################################################################
#################################################################################################################################################################################################

multiplicative_distibuted_final <- gamingAccessory_intermediate 
colnames(multiplicative_distibuted_final)
head(multiplicative_distibuted_final) 


gamingAccessory_intermediate[,c(13:22)]<-gamingAccessory_intermediate[,c(13:22)]*10000000


gamingAccessory_intermediate <- gamingAccessory_intermediate +01
#Take log of the data
multiplicative_model <-log(gamingAccessory_intermediate)

#Scale the data
multiplicative_model<-  sapply(multiplicative_model, function(x) scale(x))

colnames(multiplicative_model)
multiplicative_distibuted_final<- multiplicative_model[, -c(1,5,12,13,32:38)]  ## Not considered the Moving Average KPIs

multiplicative_distibuted_final<-na.omit(multiplicative_distibuted_final)
sum(is.na(multiplicative_distibuted_final)) 

multiplicative_distibuted_final<- data.frame(multiplicative_distibuted_final)
model_1 <- lm(gmv~.,multiplicative_distibuted_final)  

summary(model_1) 


library(car)
library(MASS)

model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
vif(model_2)


model_2 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing + Online.marketing +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing + adOnline.marketing + adAffiliates +
                adSEM + adRadio + adOther + shelf_price_inflation_2 + shelf_price_inflation_3 +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#Online.marketing
model_2 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing + adOnline.marketing + adAffiliates +
                adSEM + adRadio + adOther + shelf_price_inflation_2 + shelf_price_inflation_3 +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown_inflation2
model_2 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing + adOnline.marketing + adAffiliates +
                adSEM + adRadio + adOther + shelf_price_inflation_2 + shelf_price_inflation_3 +
                shelf_price_inflation_4 + markdown_inflation1  +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#shelf_price_inflation_2
model_2 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing + adOnline.marketing + adAffiliates +
                adSEM + adRadio + adOther  + shelf_price_inflation_3 +
                shelf_price_inflation_4 + markdown_inflation1  +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adOnline.marketing
model_2 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing  + adAffiliates +
                adSEM + adRadio + adOther  + shelf_price_inflation_3 +
                shelf_price_inflation_4 + markdown_inflation1  +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#shelf_price_inflation_3
model_2 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing  + adAffiliates +
                adSEM + adRadio + adOther   +
                shelf_price_inflation_4 + markdown_inflation1  +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#list_price
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing  + adAffiliates +
                adSEM + adRadio + adOther   +
                shelf_price_inflation_4 + markdown_inflation1  +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#markdown_inflation1
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing  + adAffiliates +
                adSEM + adRadio + adOther   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#adSEM
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing  + adAffiliates +
                adRadio + adOther   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]






#adAffiliates
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing   +
                adRadio + adOther   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#adAffiliates
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV + adDigital + adSponsorship +
                adContent.Marketing   +
                adRadio + adOther   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]






#adDigital
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                adRadio + adOther   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





#adOther
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                NPS_Score + Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                adRadio    +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#NPS_Score
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                adRadio    +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#adRadio
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





#NPS_Score.2
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#NPS_Score.3
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                shelf_price_inflation_4   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#shelf_price_inflation_4
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other + adTV  + adSponsorship +
                adContent.Marketing   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#adTV
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other  + adSponsorship +
                adContent.Marketing   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# adContent.Marketing
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other  + adSponsorship +
                adContent.Marketing   +
                list_price.1 + list_price.2 + list_price.3 + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# list_price.3
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other  + adSponsorship +
                adContent.Marketing   +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# adSponsorship
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                adContent.Marketing   +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# adSponsorship
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                adContent.Marketing   +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# adContent.Marketing
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + markdown.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# markdown.3
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# count_promotion_days_in_week.2
model_2 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# markdown
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.3 +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# count_promotion_days_in_week.3
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 + count_promotion_days_in_week.1 +
                +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]






# count_promotion_days_in_week.1
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2 +
                +
                gmv.2, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# gmv.2
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio + Other +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#Other

# Other
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates + SEM + Radio  +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




# SEM
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates  + Radio  +
                list_price.1 + list_price.2  + markdown.1 +
                markdown.2,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#  markdown.2
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates  + Radio  +
                list_price.1 + list_price.2  + markdown.1,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#  list_price.2
model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Digital + Content.Marketing  +
                Affiliates  + Radio  +
                list_price.1   + markdown.1,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

# Digital

model_2 <- lm(formula = gmv ~ product_mrp  + product_procurement_sla +
                Content.Marketing  +
                Affiliates  + Radio  +
                list_price.1   + markdown.1,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#product_procurement_sla


model_2 <- lm(formula = gmv ~ product_mrp  +
                Content.Marketing  +
                Affiliates  + Radio  +
                list_price.1   + markdown.1,
              data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)        0.006667   0.067194   0.099 0.921426   
#product_mrp        0.228991   0.069340   3.302 0.001936 **
#  Content.Marketing -0.409025   0.109494  -3.736 0.000547 ***
#  Affiliates         0.898111   0.096164   9.339 6.59e-12 ***
#  Radio              0.168203   0.079637   2.112 0.040523 * 
#  list_price.1      -0.213838   0.099261  -2.154 0.036864 * 
#  markdown.1        -0.312042   0.102522  -3.044 0.003977 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4695 on 43 degrees of freedom
#Multiple R-squared:  0.7366,     Adjusted R-squared:  0.6998

############################# ELASTICITY ANALYSIS Multiplicative + DISTRIBUTED LAG MODEL ##############################################################
##Final Model 
Multiplicative_Distributed_Lag_Final_model_gaming <- model_2

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = multiplicative_distibuted_final, form.lm = formula(gmv ~ product_mrp  +
                                                                                   Content.Marketing  +
                                                                                   Affiliates  + Radio  +
                                                                                   list_price.1   + markdown.1),m = 10)
Cross_game[8] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- multiplicative_distibuted_final


grlm <- Multiplicative_Distributed_Lag_Final_model_gaming


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)

library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative + Distributed Lag Model") +xlab("Variables")



