
library(lubridate)
library(sqldf)

Cross_camera<- rep(0,8)

sale_data<- read.csv("sale.csv", stringsAsFactors = TRUE, header = TRUE)


#Merge sale_data
sale_data$order_week <- week(ISOdate(sale_data$Year, sale_data$Month, sale_data$Day))

sale_data$order_week<- ifelse(sale_data$order_week<=26 & sale_data$Year==2016,sale_data$order_week+53,sale_data$order_week)


promotion_days_in_week <- sqldf("select count(1) count_promotion_days_in_week,order_week from sale_data group by order_week")

sale_data <- merge(sale_data,promotion_days_in_week, by=c("order_week"), all.x = TRUE)
sale_data<- sale_data[,-c(2,3,4,5)]
sale_data<- unique(sale_data)

cameraAccessory <- read.csv("cameraAccessory.csv", stringsAsFactors = TRUE, header = TRUE)

cameraAccessory1 <- sqldf("select order_week, sum(gmv) as gmv from cameraAccessory group by order_week ")

#SLA,DeliverybDays and DeliveryCDays, procurement sla cannot be aggregated at the week level and hence not considering them

cameraAccessory2<- sqldf("select order_week,avg(list_price) as list_price,avg(product_mrp) as product_mrp,avg(units) as units,avg(sla) as sla,
                         avg(markdown) as markdown,avg(product_procurement_sla) as product_procurement_sla,avg(prepaid_percentage) as prepaid_percentage,
                         avg(NPS_Score) as NPS_Score from cameraAccessory
                         group by order_week")

cameraAccessory2 <- merge(cameraAccessory2,sale_data, by="order_week",all.x = TRUE)

cameraAccessory2$count_promotion_days_in_week[is.na(cameraAccessory2$count_promotion_days_in_week)] <-0.001

cameraAccessory3 <- merge(cameraAccessory1,cameraAccessory2, by="order_week", all.x = TRUE)




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
cameraAccessory_intermediate <- merge(cameraAccessory3 , investment_data_weekly,by.x = "order_week")

######################## EDA PLOTS #################################
colnames(cameraAccessory_intermediate)


data_week <- sqldf("select order_week as week,sum(gmv) as gmv,avg(product_mrp) as product_mrp,
avg(markdown) as markdown,avg(sla) as sla,avg(product_procurement_sla) as product_procurement_sla,
                   avg(count_promotion_days_in_week) as count_promotion_days_in_week,TV=avg(TV) as TV,Digital=avg(Digital) as Digital,
                   avg(Sponsorship) as Sponsorship,avg('Content.Marketing') as 'Content.Marketing',
                   avg('Online.marketing') as 'Online.Marketing',avg(Affiliates) as Affiliates,avg(SEM) as SEM,
                   avg(Radio) as Radio,avg(Other) as Other,avg('Total.Investment') as 'Total.Investment',
                   avg(NPS_Score) as NPS_Score,avg(list_price) as list_price,
                   sum(units) as units,sum(prepaid_percentage) as prepaid_percentage from cameraAccessory_intermediate group by order_week")

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



######################## Leniar Model #####################################################################################################

colnames(cameraAccessory_intermediate)
str(cameraAccessory_intermediate)
Linear_model<-  sapply(cameraAccessory_intermediate, function(x) scale(x))

colnames(Linear_model)
Linear_model <- data.frame(Linear_model[,-c(1,5,12,13,23:31)])

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


#Online.marketing
model_3 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla + 
                prepaid_percentage + NPS_Score + Digital + Sponsorship
                 + SEM + Radio + Other, data = Linear_model)

summary(model_3) 
vif(model_3)

#Sponsorship
model_4 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla + 
                prepaid_percentage + NPS_Score + Digital
              + SEM + Radio + Other, data = Linear_model)


summary(model_4) 
vif(model_4)

#Radio
model_5 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla + 
                prepaid_percentage + NPS_Score + Digital
              + SEM  + Other, data = Linear_model)

summary(model_5) 
vif(model_5)

#Other
model_6 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla + 
                prepaid_percentage + NPS_Score + Digital
              + SEM , data = Linear_model)



summary(model_6) 
vif(model_6)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -5.925e-16  1.058e-01   0.000  1.00000    
# product_mrp              5.001e-01  1.487e-01   3.363  0.00161 ** 
#   markdown                 5.378e-01  1.601e-01   3.360  0.00162 ** 
#   product_procurement_sla  3.668e-01  1.327e-01   2.765  0.00828 ** 
#   prepaid_percentage       2.991e-01  1.222e-01   2.448  0.01841 *  
#   NPS_Score               -1.145e+00  2.237e-01  -5.115 6.58e-06 ***
#   Digital                  1.709e+00  7.314e-01   2.337  0.02408 *  
#   SEM                     -2.116e+00  8.170e-01  -2.591  0.01295 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7632 on 44 degrees of freedom
# Multiple R-squared:  0.4974,	Adjusted R-squared:  0.4175 
# F-statistic: 6.221 on 7 and 44 DF,  p-value: 4.302e-05

#SEM
model_7 <- lm(formula = gmv ~ product_mrp + markdown + product_procurement_sla + 
                prepaid_percentage + NPS_Score + Digital
               , data = Linear_model)



summary(model_7) 
vif(model_7)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -3.961e-16  1.124e-01   0.000   1.0000    
# product_mrp              3.504e-01  1.455e-01   2.409   0.0202 *  
#   markdown                 3.371e-01  1.487e-01   2.267   0.0282 *  
#   product_procurement_sla  2.607e-01  1.339e-01   1.946   0.0579 .  
# prepaid_percentage       1.966e-01  1.227e-01   1.602   0.1161    
# NPS_Score               -6.785e-01  1.412e-01  -4.805 1.76e-05 ***
#   Digital                 -1.595e-01  1.289e-01  -1.238   0.2223    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8102 on 45 degrees of freedom
# Multiple R-squared:  0.4208,	Adjusted R-squared:  0.3435 
# F-statistic: 5.448 on 6 and 45 DF,  p-value: 0.0002644

############################# ELASTICITY ANALYSIS Leniar + Adstock MODEL ##############################################################
##Final Model 
Leniar_Final_model_camera <- model_7


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ product_mrp + markdown + product_procurement_sla + 
                                                                prepaid_percentage + NPS_Score + Digital),m = 10)
Cross_camera[1] <- attr(temp_crossval, "ms")


# Elasticity Analysis

train <- Linear_model


grlm <- Leniar_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Leniar Model") +xlab("Variables")

######################## Leniar + AdStock Model ##################################################################################
colnames(cameraAccessory_intermediate)
str(cameraAccessory_intermediate)
AdLinear_model<-  sapply(cameraAccessory_intermediate, function(x) scale(x))

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

#adDigital
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score + TV + Digital + Affiliates + SEM + Radio + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM + 
                  adRadio + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adRadio
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score + TV + Digital + Affiliates + SEM + Radio + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                   + adOther, data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adOther
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score + TV + Digital + Affiliates + SEM + Radio + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#TV
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score + Digital + Affiliates + SEM + Radio + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Digital
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score  + Affiliates + SEM + Radio + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Affiliates
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score   + SEM + Radio + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Radio
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score   + SEM  + Other + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Other
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                  NPS_Score   + SEM   + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
adModel_3 <- lm(formula = gmv ~ list_price + product_mrp  + 
                  NPS_Score   + SEM   + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#list_price
adModel_3 <- lm(formula = gmv ~   product_mrp  + 
                  NPS_Score   + SEM   + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#product_mrp
adModel_3 <- lm(formula = gmv ~ 
                  NPS_Score   + SEM   + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -5.582e-18  1.047e-01   0.000 1.000000    
# NPS_Score           -5.030e-01  1.910e-01  -2.633 0.011550 *  
#   SEM                 -2.556e+00  8.821e-01  -2.898 0.005787 ** 
#   adTV                -5.592e-01  1.952e-01  -2.865 0.006315 ** 
#   adContent.Marketing -2.572e+00  6.321e-01  -4.069 0.000188 ***
#   adAffiliates         1.106e+00  3.332e-01   3.319 0.001799 ** 
#   adSEM                4.507e+00  1.333e+00   3.381 0.001500 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7553 on 45 degrees of freedom
# Multiple R-squared:  0.4966,	Adjusted R-squared:  0.4295 
# F-statistic: 7.399 on 6 and 45 DF,  p-value: 1.519e-05

#NPS_Score
adModel_3 <- lm(formula = gmv ~ 
                      SEM   + 
                  adTV  + adContent.Marketing + adAffiliates + adSEM 
                , data = AdLinear_model)

summary(adModel_3) 
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          6.092e-17  1.113e-01   0.000 1.000000    
# SEM                 -2.674e+00  9.360e-01  -2.857 0.006407 ** 
#   adTV                -7.094e-01  1.983e-01  -3.577 0.000832 ***
#   adContent.Marketing -2.728e+00  6.687e-01  -4.079 0.000178 ***
#   adAffiliates         1.569e+00  3.006e-01   5.220 4.19e-06 ***
#   adSEM                4.907e+00  1.407e+00   3.488 0.001085 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8026 on 46 degrees of freedom
# Multiple R-squared:  0.419,	Adjusted R-squared:  0.3559 
# F-statistic: 6.636 on 5 and 46 DF,  p-value: 9.982e-05

############################# ELASTICITY ANALYSIS Leniar + Adstock MODEL ##############################################################
##Final Model 
Leniar_Adstock_Final_model_camera <- adModel_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = AdLinear_model, form.lm = formula(gmv ~ SEM + adTV  + adContent.Marketing + adAffiliates + adSEM  ),m = 10)
Cross_camera[2] <- attr(temp_crossval, "ms")


# Elasticity Analysis

train <- AdLinear_model


grlm <- Leniar_Adstock_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Leniar _ Adstock Model") +xlab("Variables")


######################## Multiplicative Model #################################################################################

colnames(cameraAccessory_intermediate)
str(cameraAccessory_intermediate)
#Multply by 1 crore
cameraAccessory_intermediate[,c(13:22)]<-cameraAccessory_intermediate[,c(13:22)]*10000000 
#Take log of the data 
multiplicative_model <-log(cameraAccessory_intermediate + 0.01) 
#Scale the data
multiplicative_model<-  sapply(multiplicative_model, function(x) scale(x))

colnames(multiplicative_model)
multiplicative_model <- data.frame(multiplicative_model[,-c(1,5,12,13,23:31)])

sum(is.na(multiplicative_model))
head(multiplicative_model)

mm_1 <- lm(gmv~.,multiplicative_model)
# Summary of multiplicative_model
summary(mm_1)


mm_2 <- stepAIC(mm_1,direction = "both")
summary(mm_2) 
vif<-vif(mm_2)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
mm_3<- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla + 
            prepaid_percentage + NPS_Score + Digital  + 
            Online.marketing + Affiliates + SEM, data = multiplicative_model)

summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
mm_3<- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla + 
            prepaid_percentage  + Digital  + 
            Online.marketing + Affiliates + SEM, data = multiplicative_model)

summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#SEM
mm_3<- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla + 
            prepaid_percentage  + Digital  + 
            Online.marketing + Affiliates , data = multiplicative_model)

summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#Digital
mm_3<- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla + 
            prepaid_percentage  + 
            Online.marketing + Affiliates , data = multiplicative_model)

summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]


#prepaid_percentage
mm_3<- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
              + Online.marketing + Affiliates , data = multiplicative_model)

summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#Online.marketing
mm_3<- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
           + Affiliates , data = multiplicative_model)

summary(mm_3) 
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.023e-15  6.309e-02   0.000 1.000000    
# list_price               1.603e+00  2.439e-01   6.572  4.0e-08 ***
#   product_mrp             -6.709e-01  1.469e-01  -4.567  3.7e-05 ***
#   markdown                 5.247e-01  1.515e-01   3.463 0.001165 ** 
#   product_procurement_sla -3.152e-01  8.940e-02  -3.526 0.000969 ***
#   Affiliates               3.272e-01  8.166e-02   4.007 0.000223 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4549 on 46 degrees of freedom
# Multiple R-squared:  0.8133,	Adjusted R-squared:  0.793 
# F-statistic: 40.09 on 5 and 46 DF,  p-value: 1.147e-15

############################# ELASTICITY ANALYSIS Multiplicative  MODEL ##############################################################
##Final Model 
Multiplicative_Final_model_camera <- mm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = multiplicative_model, form.lm = formula(gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
                                                                      + Affiliates ),m = 10)
Cross_camera[3] <- attr(temp_crossval, "ms")


# Elasticity Analysis

train <- multiplicative_model


grlm <- Multiplicative_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")


############################################### Multiplicative Ad Stock Model ######################################
colnames(cameraAccessory_intermediate)
str(cameraAccessory_intermediate)

#Take log of the data 
adMultiplicative_model <-log(cameraAccessory_intermediate + 0.01) 
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

#adOther
admm_3<- lm(formula = gmv ~ product_procurement_sla + prepaid_percentage + 
              NPS_Score + count_promotion_days_in_week + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other + 
              adSponsorship + adContent.Marketing + adOnline.marketing + 
              adAffiliates + adRadio , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]
#adRadio
admm_3<- lm(formula = gmv ~ product_procurement_sla + prepaid_percentage + 
              NPS_Score + count_promotion_days_in_week + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other + 
              adSponsorship + adContent.Marketing + adOnline.marketing + 
              adAffiliates  , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + count_promotion_days_in_week + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other + 
              adSponsorship + adContent.Marketing + adOnline.marketing + 
              adAffiliates  , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#adAffiliates
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + count_promotion_days_in_week + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other + 
              adSponsorship + adContent.Marketing + adOnline.marketing 
                , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]


#adSponsorship
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + count_promotion_days_in_week + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other 
               + adContent.Marketing + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other 
            + adContent.Marketing + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#adContent.Marketing
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV + Digital + 
              Content.Marketing + Online.marketing + SEM + Radio + Other 
             + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Content.Marketing
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV + Digital 
               + Online.marketing + SEM + Radio + Other 
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#SEM
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV + Digital 
            + Online.marketing  + Radio + Other 
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Radio
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV + Digital 
            + Online.marketing + Other 
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Other
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV + Digital 
            + Online.marketing  
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Digital
admm_3<- lm(formula = gmv ~  prepaid_percentage + 
              NPS_Score + TV  
            + Online.marketing  
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
admm_3<- lm(formula = gmv ~ 
              NPS_Score + TV  
            + Online.marketing  
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
admm_3<- lm(formula = gmv ~ 
               TV  
            + Online.marketing  
            + adOnline.marketing 
            , data = adMultiplicative_model)
summary(admm_3) 
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         8.175e-16  6.496e-02   0.000        1    
# TV                  1.016e+00  1.709e-01   5.945 3.05e-07 ***
#   Online.marketing   -1.907e+00  4.413e-01  -4.321 7.77e-05 ***
#   adOnline.marketing  1.748e+00  4.053e-01   4.312 7.99e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4684 on 48 degrees of freedom
# Multiple R-squared:  0.7935,	Adjusted R-squared:  0.7806 
# F-statistic: 61.48 on 3 and 48 DF,  p-value: < 2.2e-16

############################# ELASTICITY ANALYSIS Multiplicative + Adstock MODEL ##############################################################
##Final Model 
Multiplicative_Adstock_Final_model_camera <- admm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = adMultiplicative_model, form.lm = formula(gmv ~ TV  
                                                                        + Online.marketing  
                                                                        + adOnline.marketing ),m = 10)
Cross_camera[4] <- attr(temp_crossval, "ms")


# Elasticity Analysis

train <- adMultiplicative_model


grlm <- Multiplicative_Adstock_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative + Adstock Model") +xlab("Variables")


###################################### Koyck Model #######################################################################
colnames(cameraAccessory_intermediate)
str(cameraAccessory_intermediate)

#Get the dataset
koyck_model <- data.frame(cameraAccessory_intermediate[,-c(1,5,12,13,23:31)])


koyck_model$lag_gmv <- data.table::shift(koyck_model$gmv,1,NA,type="lag")
sum(is.na(koyck_model))
koyck_model<- na.omit(koyck_model)


#scale the dataset
koyck_model<-  sapply(koyck_model, function(x) scale(x))

sum(is.na(koyck_model))

head(koyck_model)

koyck_model<- data.frame(koyck_model)

kyockm_1 <- lm(gmv~.,koyck_model)

# Summary of Linear Model 
summary(kyockm_1)

kyockm_2 <- stepAIC(kyockm_1,direction = "both")
summary(kyockm_2) 
vif<- vif(kyockm_2)
vif[order(vif,decreasing = TRUE)]

#Content.Marketing
kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                 prepaid_percentage + NPS_Score + Digital + Sponsorship 
                  + Affiliates + SEM + Radio + Other, data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla + 
                 prepaid_percentage  + Digital + Sponsorship 
               + Affiliates + SEM + Radio + Other, data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + product_procurement_sla 
                   + Digital + Sponsorship 
               + Affiliates + SEM + Radio + Other, data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#list_price
kyockm_3 <- lm(formula = gmv ~  product_mrp + product_procurement_sla 
               + Digital + Sponsorship 
               + Affiliates + SEM + Radio + Other, data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
kyockm_3 <- lm(formula = gmv ~  product_mrp  
               + Digital + Sponsorship 
               + Affiliates + SEM + Radio + Other, data = koyck_model)
summary(kyockm_3) 
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.204e-15  1.088e-01   0.000 1.000000    
# product_mrp  3.768e-01  1.285e-01   2.931 0.005385 ** 
#   Digital      5.973e+00  1.680e+00   3.555 0.000933 ***
#   Sponsorship  1.406e+00  3.174e-01   4.430 6.38e-05 ***
#   Affiliates   9.240e-01  2.374e-01   3.892 0.000341 ***
#   SEM         -7.050e+00  1.908e+00  -3.696 0.000616 ***
#   Radio        3.189e+00  8.750e-01   3.645 0.000716 ***
#   Other       -2.934e+00  8.229e-01  -3.566 0.000905 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7767 on 43 degrees of freedom
# Multiple R-squared:  0.4812,	Adjusted R-squared:  0.3968 
# F-statistic: 5.699 on 7 and 43 DF,  p-value: 0.0001057


############################# ELASTICITY ANALYSIS KYOCK  MODEL ##############################################################
##Final Model 
Kyock_Final_model_camera <- kyockm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = koyck_model, form.lm = formula(gmv ~ product_mrp  
                                                             + Digital + Sponsorship 
                                                             + Affiliates + SEM + Radio + Other),m = 10)
Cross_camera[5] <- attr(temp_crossval, "ms")

# Elasticity Analysis

train <- koyck_model


grlm <- Kyock_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Kyock Model") +xlab("Variables")

###################################### Kyock + Adstock Model ##############################################################
colnames(cameraAccessory_intermediate)
str(cameraAccessory_intermediate)

#Get the dataset
ad_koyck_model <- data.frame(cameraAccessory_intermediate[,-c(1,5,12,13)])

ad_koyck_model$lag_gmv <- data.table::shift(ad_koyck_model$gmv,1,NA,type="lag")
sum(is.na(ad_koyck_model))
ad_koyck_model<- na.omit(ad_koyck_model)


#scale the dataset
ad_koyck_model<-  sapply(ad_koyck_model, function(x) scale(x))

sum(is.na(ad_koyck_model))

head(ad_koyck_model)

ad_koyck_model<- data.frame(ad_koyck_model)

ad_kyockm_1 <- lm(gmv~.,ad_koyck_model)

# Summary of Linear Model 
summary(kyockm_1)

ad_kyockm_2 <- stepAIC(ad_kyockm_1,direction = "both")
summary(ad_kyockm_2) 
vif<- vif(ad_kyockm_2)
vif[order(vif,decreasing = TRUE)]

#adAffiliates
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla + 
                    NPS_Score + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital + adSponsorship + 
                    adOnline.marketing  + adSEM + adRadio + adOther, 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
                     + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital + adSponsorship + 
                    adOnline.marketing  + adSEM + adRadio + adOther, 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]


#adSponsorship
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
                  + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital + 
                    adOnline.marketing  + adSEM + adRadio + adOther, 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adOnline.marketing
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
                  + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital 
                      + adSEM + adRadio + adOther, 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adOther
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp + markdown + product_procurement_sla 
                  + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital 
                  + adSEM + adRadio , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#markdown
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp  + product_procurement_sla 
                  + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital 
                  + adSEM + adRadio , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adRadio
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp  + product_procurement_sla 
                  + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital 
                  + adSEM , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Content.Marketing
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp  + product_procurement_sla 
                  + Digital   + Online.marketing + 
                    Affiliates + SEM + Radio + Other + adDigital +Sponsorship
                  + adSEM , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.356e-15  8.079e-02   0.000 1.000000    
# list_price              -8.000e-01  2.175e-01  -3.678 0.000724 ***
#   product_mrp              1.271e+00  2.433e-01   5.223 6.61e-06 ***
#   product_procurement_sla  3.847e-01  1.008e-01   3.817 0.000484 ***
#   Digital                  1.155e+01  1.678e+00   6.883 3.55e-08 ***
#   Online.marketing        -5.981e+00  1.441e+00  -4.152 0.000180 ***
#   Affiliates               6.549e+00  1.334e+00   4.907 1.77e-05 ***
#   SEM                     -1.468e+01  2.229e+00  -6.587 8.96e-08 ***
#   Radio                    5.123e+00  8.663e-01   5.914 7.47e-07 ***
#   Other                   -4.603e+00  7.951e-01  -5.790 1.11e-06 ***
#   adDigital               -5.345e+00  1.455e+00  -3.674 0.000733 ***
#   Sponsorship              2.030e+00  2.995e-01   6.779 4.91e-08 ***
#   adSEM                    7.473e+00  2.035e+00   3.672 0.000736 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5769 on 38 degrees of freedom
# Multiple R-squared:  0.747,	Adjusted R-squared:  0.6672 
# F-statistic: 9.352 on 12 and 38 DF,  p-value: 4.908e-08


#SEM
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp  + product_procurement_sla 
                  + Digital   + Online.marketing + 
                    Affiliates  + Radio + Other + adDigital +Sponsorship
                  + adSEM , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adDigital
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp  + product_procurement_sla 
                  + Digital   + Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                  + adSEM , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adSEM
ad_kyockm_3 <- lm(formula = gmv ~ list_price + product_mrp  + product_procurement_sla 
                  + Digital   + Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                   , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]


#list_price
ad_kyockm_3 <- lm(formula = gmv ~  product_mrp  + product_procurement_sla 
                  + Digital   + Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                  , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Digital
ad_kyockm_3 <- lm(formula = gmv ~  product_mrp  + product_procurement_sla 
                     + Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                  , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
ad_kyockm_3 <- lm(formula = gmv ~  product_mrp   
                  + Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                  , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#product_mrp
ad_kyockm_3 <- lm(formula = gmv ~     
                   Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                  , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

ad_kyockm_3 <- lm(formula = gmv ~     
                    Online.marketing + 
                    Affiliates  + Radio + Other  +Sponsorship
                  , 
                  data = ad_koyck_model)

summary(ad_kyockm_3) 
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       2.154e-16  1.212e-01   0.000  1.00000   
# Online.marketing -4.008e+00  1.823e+00  -2.198  0.03311 * 
#   Affiliates        3.876e+00  1.667e+00   2.325  0.02464 * 
#   Radio             1.733e+00  7.570e-01   2.289  0.02684 * 
#   Other            -1.532e+00  6.922e-01  -2.213  0.03201 * 
#   Sponsorship       9.428e-01  3.048e-01   3.094  0.00339 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8658 on 45 degrees of freedom
# Multiple R-squared:  0.3254,	Adjusted R-squared:  0.2504 
# F-statistic: 4.341 on 5 and 45 DF,  p-value: 0.002619


############################# ELASTICITY ANALYSIS KYOCK _ Ad Stock MODEL ##############################################################
##Final Model 
Kyock_AdStock_Final_model_camera <- ad_kyockm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = ad_koyck_model, form.lm = formula(gmv ~ Online.marketing + Affiliates  + Radio + Other  +Sponsorship),m = 10)
Cross_camera[6] <- attr(temp_crossval, "ms")

# Elasticity Analysis

train <- ad_koyck_model


grlm <- Kyock_AdStock_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory -Kyock + AdStock Model") +xlab("Variables")



###################################### FEATURE ENGINEERING PART - 2:######################################

###################################### Moving Averages ######################################

#install.packages("TTR")
library(TTR)
cameraAccessory_intermediate$list_price_maOne <- runMean(cameraAccessory_intermediate$list_price,1)
cameraAccessory_intermediate$list_price_ma1 <- runMean(cameraAccessory_intermediate$list_price,2)
cameraAccessory_intermediate$list_price_ma2 <- runMean(cameraAccessory_intermediate$list_price,3)
cameraAccessory_intermediate$list_price_ma3 <- runMean(cameraAccessory_intermediate$list_price,4)

cameraAccessory_intermediate$markdown_ma1 <- runMean(cameraAccessory_intermediate$markdown,2)
cameraAccessory_intermediate$markdown_ma2 <- runMean(cameraAccessory_intermediate$markdown,3)
cameraAccessory_intermediate$markdown_ma3 <- runMean(cameraAccessory_intermediate$markdown,4)


#Shelf price inflationith week = List price ith week / List price (i-1)th week
#TODO:Revisit This
cameraAccessory_intermediate$shelf_price_inflation_1= cameraAccessory_intermediate$list_price/cameraAccessory_intermediate$list_price_maOne
cameraAccessory_intermediate$shelf_price_inflation_2= cameraAccessory_intermediate$list_price/cameraAccessory_intermediate$list_price_ma1
cameraAccessory_intermediate$shelf_price_inflation_3= cameraAccessory_intermediate$list_price/cameraAccessory_intermediate$list_price_ma2
cameraAccessory_intermediate$shelf_price_inflation_4= cameraAccessory_intermediate$list_price/cameraAccessory_intermediate$list_price_ma3

#% discount offered ith week = Discounted price ith week / List price ith week ------(wrt List price) 
#% discount offered ith week = Discounted price ith week / MRP ith week ------(wrt MRP)

cameraAccessory_intermediate$markdown_inflation1<- 
  (cameraAccessory_intermediate$markdown - cameraAccessory_intermediate$markdown_ma1)/cameraAccessory_intermediate$markdown_ma1
cameraAccessory_intermediate$markdown_inflation2<- 
  (cameraAccessory_intermediate$markdown - cameraAccessory_intermediate$markdown_ma2)/cameraAccessory_intermediate$markdown_ma2

cameraAccessory_intermediate$markdown_inflation1<- 
  (cameraAccessory_intermediate$markdown - cameraAccessory_intermediate$markdown_ma3)/cameraAccessory_intermediate$markdown_ma3


#install.packages("DataCombine")
library(DataCombine)

#List of list price by 1,2,3 dates (Date values are ordered)
#Previous List price
cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "list_price",slideBy = -1)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "list_price",slideBy = -2)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "list_price", slideBy = -3)

#9.lag the promotion variables

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "markdown", slideBy = -1)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "markdown", slideBy = -2)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "markdown", slideBy = -3)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "NPS_Score", slideBy = -1)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "NPS_Score", slideBy = -2)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "NPS_Score", slideBy = -3)


cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "count_promotion_days_in_week", slideBy = -1)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "count_promotion_days_in_week", slideBy = -2)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "count_promotion_days_in_week", slideBy = -3)


cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "gmv",slideBy = -1)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "gmv",slideBy = -2)

cameraAccessory_intermediate <- slide(cameraAccessory_intermediate, Var = "gmv",slideBy = -3)



sum(is.na(cameraAccessory_intermediate))
cameraAccessory_final <- na.omit(cameraAccessory_intermediate)



sum(is.na(cameraAccessory_final))

head(cameraAccessory_final)
Linear_model<-  sapply(cameraAccessory_final, function(x) scale(x))

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


#count_promotion_days_in_week 
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other + adDigital + adSponsorship + adContent.Marketing + 
               adOnline.marketing + adAffiliates + adSEM + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               NPS_Score.3 + count_promotion_days_in_week.1 + count_promotion_days_in_week.2 + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#adOnline.marketing
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other + adDigital + adSponsorship + adContent.Marketing + 
                adAffiliates + adSEM + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               NPS_Score.3 + count_promotion_days_in_week.1 + count_promotion_days_in_week.2 + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#adSEM
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other + adDigital + adSponsorship + adContent.Marketing + 
               adAffiliates  + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               NPS_Score.3 + count_promotion_days_in_week.1 + count_promotion_days_in_week.2 + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#adDigital

#NPS_Score.3
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
              count_promotion_days_in_week.1 + count_promotion_days_in_week.2 + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#list_price
model_3<- lm(formula = gmv ~  product_mrp + sla + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#sla
model_3<- lm(formula = gmv ~  product_mrp  + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1 + count_promotion_days_in_week.2 + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week.2
model_3<- lm(formula = gmv ~  product_mrp  + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1  + 
               gmv.1 + gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#gmv.1
model_3<- lm(formula = gmv ~  product_mrp  + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + adOther + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1  + 
              gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#adOther
model_3<- lm(formula = gmv ~  product_mrp  + markdown + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1  + 
               gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#markdown
model_3<- lm(formula = gmv ~  product_mrp  + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               adAffiliates  + list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1  + 
               gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#adAffiliates
model_3<- lm(formula = gmv ~  product_mrp  + 
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1  + 
               gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#product_mrp
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2 + NPS_Score.2 + 
               count_promotion_days_in_week.1  + 
               gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#NPS_Score.2 
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 + gmv.3, data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#gmv.3
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               list_price_ma1 + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#list_price_ma1
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               list_price_ma2 + list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#list_price_ma2
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
              list_price_ma3 + markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#list_price_ma3
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               TV + Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#TV
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               Digital + Sponsorship + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               Digital  + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1  + 
               gmv.2 , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#gmv.2
model_3<- lm(formula = gmv ~  
               product_procurement_sla + prepaid_percentage  + 
               Digital  + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1 
                , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
model_3<- lm(formula = gmv ~  
               prepaid_percentage  + 
               Digital  + Online.marketing + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#Online.marketing
model_3<- lm(formula = gmv ~  
               prepaid_percentage  + 
               Digital + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
model_3<- lm(formula = gmv ~  
               Digital + Affiliates + 
               SEM + Radio + Other  + adSponsorship + adContent.Marketing + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#adContent.Marketing
model_3<- lm(formula = gmv ~  
               Digital + Affiliates + 
               SEM + Radio + Other  + adSponsorship  + 
               markdown_ma2 + markdown_ma3 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#markdown_ma3
model_3<- lm(formula = gmv ~  
               Digital + Affiliates + 
               SEM + Radio + Other  + adSponsorship  + 
               markdown_ma2 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation1 + markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#markdown_inflation1
model_3<- lm(formula = gmv ~  
               Digital + Affiliates + 
               SEM + Radio + Other  + adSponsorship  + 
               markdown_ma2 + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]
#markdown_ma2
model_3<- lm(formula = gmv ~  
               Digital + Affiliates + 
               SEM + Radio + Other  + adSponsorship  + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#Radio
model_3<- lm(formula = gmv ~  
               Digital + Affiliates + 
               SEM  + Other  + adSponsorship  + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#SEM
model_3<- lm(formula = gmv ~  
               Digital + Affiliates
                 + Other  + adSponsorship  + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#adSponsorship
model_3<- lm(formula = gmv ~  
               Digital + Affiliates
             + Other  + 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#Other
model_3<- lm(formula = gmv ~  
               Digital + Affiliates+ 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#Digital
model_3<- lm(formula = gmv ~  
               Affiliates+ 
               shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
               markdown_inflation2  + 
               count_promotion_days_in_week.1 
             , data = Linear_model)

summary(model_3) 
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    -0.08653    0.10660  -0.812  0.42151    
# Affiliates                      0.26994    0.12083   2.234  0.03086 *  
#   shelf_price_inflation_1         0.24158    0.12403   1.948  0.05814 .  
# shelf_price_inflation_2        -0.48957    0.17522  -2.794  0.00781 ** 
#   shelf_price_inflation_4         1.10928    0.19301   5.747 9.17e-07 ***
#   markdown_inflation2             0.38871    0.18032   2.156  0.03689 *  
#   count_promotion_days_in_week.1 -0.26076    0.10292  -2.534  0.01511 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6783 on 42 degrees of freedom
# Multiple R-squared:  0.5974,	Adjusted R-squared:  0.5399 
# F-statistic: 10.39 on 6 and 42 DF,  p-value: 4.831e-07

############################# ELASTICITY ANALYSIS DISTRIBUTED LAG MODEL ##############################################################
##Final Model 
Distributed_Lag_Final_model_camera <- model_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ Affiliates+ 
                                                                shelf_price_inflation_1 + shelf_price_inflation_2 + shelf_price_inflation_4 + 
                                                                markdown_inflation2  + 
                                                                count_promotion_days_in_week.1),m = 10)
Cross_camera[7] <- attr(temp_crossval, "ms")

# Elasticity Analysis

train <- Linear_model


grlm <- Distributed_Lag_Final_model_camera


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

#library(ggplot2)

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory -Distributed Lag Model") +xlab("Variables")


#################################################################################################################################################################################################
###############################################################  Multiplicative and Distibuted lag ##########################################################################
#################################################################################################################################################################################################

multiplicative_distibuted_final <- cameraAccessory_intermediate 
colnames(multiplicative_distibuted_final)
head(multiplicative_distibuted_final) 


cameraAccessory_intermediate[,c(13:22)]<-cameraAccessory_intermediate[,c(13:22)]*10000000


cameraAccessory_intermediate <- cameraAccessory_intermediate +01
#Take log of the data
multiplicative_model <-log(cameraAccessory_intermediate)

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


#sla
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage + NPS_Score + 
                count_promotion_days_in_week + TV + Digital + Content.Marketing + 
                Online.marketing + Affiliates + SEM + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing + adOnline.marketing + 
                adAffiliates + adSEM + adRadio + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3 + shelf_price_inflation_4 + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#adOnline.marketing
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage + NPS_Score + 
                count_promotion_days_in_week + TV + Digital + Content.Marketing + 
                Online.marketing + Affiliates + SEM + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM + adRadio + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3 + shelf_price_inflation_4 + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#shelf_price_inflation_4
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage + NPS_Score + 
                count_promotion_days_in_week + TV + Digital + Content.Marketing + 
                Online.marketing + Affiliates + SEM + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM + adRadio + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adRadio
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage + NPS_Score + 
                count_promotion_days_in_week + TV + Digital + Content.Marketing + 
                Online.marketing + Affiliates + SEM + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#SEM
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage + NPS_Score + 
                count_promotion_days_in_week + TV + Digital + Content.Marketing + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#Content.Marketing
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage + NPS_Score + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage  + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2 + NPS_Score.3 + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#NPS_Score.3
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage  + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week.3 
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla + prepaid_percentage  + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla  + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2 + list_price.1 + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#list_price.1
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla  + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates + adSEM  + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adSEM
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                product_procurement_sla  + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2 + shelf_price_inflation_3  + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#shelf_price_inflation_3
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2  + count_promotion_days_in_week.1 + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week.1
model_2 <- lm(formula = gmv ~ list_price + product_mrp  + markdown + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#markdown
model_2 <- lm(formula = gmv ~ list_price + product_mrp   + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#list_price
model_2 <- lm(formula = gmv ~ product_mrp   + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1 + markdown.2 + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown.2
model_2 <- lm(formula = gmv ~ product_mrp   + 
                count_promotion_days_in_week + TV + Digital  + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#Digital
model_2 <- lm(formula = gmv ~ product_mrp   + 
                count_promotion_days_in_week + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 + gmv.3, data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#gmv.3
model_2 <- lm(formula = gmv ~ product_mrp   + 
                count_promotion_days_in_week + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week
model_2 <- lm(formula = gmv ~ product_mrp   
                 + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + NPS_Score.1 + 
                NPS_Score.2   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#NPS_Score.2
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + NPS_Score.1
                   + 
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#NPS_Score.1
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther + shelf_price_inflation_1 + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + 
              
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#shelf_price_inflation_1
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther  + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2  + 
                list_price.2 + list_price.3 + markdown.1  + 
                
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#list_price.2
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates   + adOther  + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2 
                 + list_price.3 + markdown.1  + 
                
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#adOther
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     + 
                shelf_price_inflation_2   + 
                markdown_inflation1 + markdown_inflation2 
              + list_price.3 + markdown.1  + 
                
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#shelf_price_inflation_2
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     +
                
                markdown_inflation1 + markdown_inflation2 
              + list_price.3 + markdown.1  + 
                
                count_promotion_days_in_week.2  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week.2
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates  + Radio + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     +
                markdown_inflation1 + markdown_inflation2 
                + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#Radio
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV   + 
                Online.marketing + Affiliates   + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     +
                markdown_inflation1 + markdown_inflation2 
              + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#Online.marketing
model_2 <- lm(formula = gmv ~ product_mrp   
                 + TV
                 + Affiliates   + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     +
                markdown_inflation1 + markdown_inflation2 
              + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#Affiliates
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
               + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     +
                markdown_inflation1 + markdown_inflation2 
              + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown_inflation2
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                adAffiliates     +
                markdown_inflation1 + 
              + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adAffiliates
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other + adTV + 
                adDigital + adSponsorship + adContent.Marketing  + 
                markdown_inflation1 + 
                + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#adDigital
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other + adTV + 
                adSponsorship + adContent.Marketing  + 
                markdown_inflation1 + 
                + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adSponsorship
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other + adTV 
                 + adContent.Marketing  + 
                markdown_inflation1 + 
                + list_price.3 + markdown.1  + 
                gmv.1 + gmv.2 , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#gmv.2
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other + adTV 
              + adContent.Marketing  + 
                markdown_inflation1 + 
                + list_price.3 + markdown.1  + 
                gmv.1  , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#markdown.1
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other + adTV 
              + adContent.Marketing  + 
                markdown_inflation1 + 
                + list_price.3   + 
                gmv.1  , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adTV
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other  
              + adContent.Marketing  + 
                markdown_inflation1 + 
                + list_price.3   + 
                gmv.1  , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#adContent.Marketing
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV
              + Other  
                + 
                markdown_inflation1 + 
                + list_price.3   + 
                gmv.1  , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#list_price.3
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV+ Other  
              + markdown_inflation1 + gmv.1  , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#Other
model_2 <- lm(formula = gmv ~ product_mrp   
              + TV  
              + markdown_inflation1 + gmv.1  , data = multiplicative_distibuted_final)

summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.00863    0.06288    0.14  0.89139    
# product_mrp          0.12352    0.06786    1.82  0.07553 .  
# TV                   0.62619    0.07685    8.15  2.5e-10 ***
#   markdown_inflation1 -0.17195    0.07583   -2.27  0.02832 *  
#   gmv.1                0.27717    0.06906    4.01  0.00023 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.44 on 44 degrees of freedom
# Multiple R-squared:  0.832,	Adjusted R-squared:  0.817 
# F-statistic: 54.6 on 4 and 44 DF,  p-value: <2e-16



############################# ELASTICITY ANALYSIS Multiplicative + DISTRIBUTED LAG MODEL ##############################################################
##Final Model 
Multiplicative_Distributed_Lag_Final_model_camera <- model_2

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = multiplicative_distibuted_final, form.lm = formula(gmv ~ product_mrp   
                                                                                 + TV  
                                                                                 + markdown_inflation1 + gmv.1),m = 10)
Cross_camera[8] <- attr(temp_crossval, "ms")


# Elasticity Analysis

train <- multiplicative_distibuted_final


grlm <- Multiplicative_Distributed_Lag_Final_model_camera


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
  ggtitle("Camera Accessory - Multiplicative + Distributed Lag Model") +xlab("Variables")

