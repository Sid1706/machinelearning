library(lubridate)
library(sqldf)

#install.packages("DAAG")
library(DAAG)

Cross_home<- rep(0,8)

sale_data<- read.csv("sale.csv", stringsAsFactors = TRUE, header = TRUE)


#Merge sale_data
sale_data$order_week <- week(ISOdate(sale_data$Year, sale_data$Month, sale_data$Day))

sale_data$order_week<- ifelse(sale_data$order_week<=26 & sale_data$Year==2016,sale_data$order_week+53,sale_data$order_week)


promotion_days_in_week <- sqldf("select count(1) count_promotion_days_in_week,order_week from sale_data group by order_week")

sale_data <- merge(sale_data,promotion_days_in_week, by=c("order_week"), all.x = TRUE)
sale_data<- sale_data[,-c(2,3,4,5)]
sale_data<- unique(sale_data)

homeAudio <- read.csv("homeAudio.csv", stringsAsFactors = TRUE, header = TRUE)




############################################################################################################################################
homeAudio1 <- sqldf("select order_week, sum(gmv) as gmv from homeAudio group by order_week ")

#SLA,DeliverybDays and DeliveryCDays, procurement sla cannot be aggregated at the week level and hence not considering them

homeAudio2<- sqldf("select order_week,avg(list_price) as list_price,avg(product_mrp) as product_mrp,avg(units) as units,avg(sla) as sla,
                   avg(markdown) as markdown,avg(product_procurement_sla) as product_procurement_sla,avg(prepaid_percentage) as prepaid_percentage,
                   avg(NPS_Score) as NPS_Score from homeAudio
                   group by order_week")

homeAudio2 <- merge(homeAudio2,sale_data, by="order_week",all.x = TRUE)

homeAudio2$count_promotion_days_in_week[is.na(homeAudio2$count_promotion_days_in_week)] <-0.001

homeAudio3 <- merge(homeAudio1,homeAudio2, by="order_week", all.x = TRUE)




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
homeAudio_intermediate <- merge(homeAudio3 , investment_data_weekly,by.x = "order_week")


######################## EDA PLOTS ###################################################################################################
colnames(homeAudio_intermediate)


data_week <- sqldf("select order_week as week,sum(gmv) as gmv,avg(product_mrp) as product_mrp,
                   avg(markdown) as markdown,avg(sla) as sla,avg(product_procurement_sla) as product_procurement_sla,
                   avg(count_promotion_days_in_week) as count_promotion_days_in_week,TV=avg(TV) as TV,Digital=avg(Digital) as Digital,
                   avg(Sponsorship) as Sponsorship,avg('Content.Marketing') as 'Content.Marketing',
                   avg('Online.marketing') as 'Online.Marketing',avg(Affiliates) as Affiliates,avg(SEM) as SEM,
                   avg(Radio) as Radio,avg(Other) as Other,avg('Total.Investment') as 'Total.Investment',
                   avg(NPS_Score) as NPS_Score,avg(list_price) as list_price,
                   sum(units) as units,sum(prepaid_percentage) as prepaid_percentage from homeAudio_intermediate group by order_week")

#Scale the media investment values in crores to make it real.
data_week[,c(8:17)] <- data_week[,c(8:17)]*10000000

#PLOT1: Analyze if there is an impact of special sales on GMV.
quant <- quantile(data_week$gmv,c(0.25,0.5,0.75))

matplot(data_week$week, cbind(data_week$gmv,rep(quant[1],51),rep(quant[2],51)),
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
matplot(data_week$week, cbind(data_week$NPS_Score,rep(quant[1],51),
                              rep(quant[2],51),rep(quant[3],51)),
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
matplot(data_week$week, cbind(data_week$markdown,rep(quant[1],51),
                              rep(quant[2],51),rep(quant[3],51)),
        type='l',lwd=2,xlab = 'week',ylab = 'Discount')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('Discount','25th Percentile',
                                         '50th Percentile','75th Percentile',
                                         'Sale days'),
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)

#Shows clear indication that discount being provided inline with the sales. Also discounts were very deep in the initial sales.


#PLOT5: sales volume
quant <- quantile(data_week$units,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$units,rep(quant[1],51),
                              rep(quant[2],51),rep(quant[3],51)),
        type='l',lwd=2,xlab = 'week',ylab = 'Sales Volume')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('Sales Volume','25th Percentile',
                                         '50th Percentile','75th Percentile',
                                         'Sale days'),
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)
#We can see a spike in sales volumes whenever there are sales. Hence sales can be considered as a factor that impacts sales.



######################## Leniar Model ###################################################################################################

colnames(homeAudio_intermediate)
str(homeAudio_intermediate)
Linear_model<-  sapply(homeAudio_intermediate, function(x) scale(x))

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
vif<-vif(model_2)
vif[order(vif,decreasing = TRUE)]


#Content.Marketing
model_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + prepaid_percentage +
                NPS_Score + Digital + Sponsorship  + Affiliates +
                SEM + Radio + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]

#markdown
model_3 <- lm(formula = gmv ~ product_mrp + sla  + prepaid_percentage +
                NPS_Score + Digital + Sponsorship  + Affiliates +
                SEM + Radio + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
model_3 <- lm(formula = gmv ~ product_mrp + sla  + prepaid_percentage
              + Digital + Sponsorship  + Affiliates +
                SEM + Radio + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
model_3 <- lm(formula = gmv ~ product_mrp + sla  
              + Digital + Sponsorship  + Affiliates +
                SEM + Radio + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]

#Radio
model_3 <- lm(formula = gmv ~ product_mrp + sla 
              + Digital + Sponsorship  + Affiliates +
                SEM  + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]

#SEM
model_3 <- lm(formula = gmv ~ product_mrp + sla 
              + Digital + Sponsorship  + Affiliates
              + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]


#Digital
model_3 <- lm(formula = gmv ~ product_mrp + sla 
              + Sponsorship  + Affiliates
              + Other, data = Linear_model)

summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -1.218e-16  1.145e-01   0.000  1.00000  
# product_mrp  5.447e-01  1.282e-01   4.248  0.00011 ***
#   sla          2.992e-01  1.324e-01   2.260  0.02879 *
#   Sponsorship  3.366e-01  1.358e-01   2.479  0.01708 *
#   Affiliates   8.024e-02  1.537e-01   0.522  0.60420  
# Other        7.351e-02  1.364e-01   0.539  0.59269  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.8095 on 44 degrees of freedom
# Multiple R-squared:  0.4115,    Adjusted R-squared:  0.3447
# F-statistic: 6.154 on 5 and 44 DF,  p-value: 0.0002095
############################# ELASTICITY ANALYSIS LINEAR MODEL ##############################################################
##Final Model
Linear_Final_model_home <- model_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
library(DAAG)
temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ product_mrp + sla + Sponsorship  + Affiliates + Other),m = 10)
Cross_home[1] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- Linear_model


grlm <- Linear_Final_model_home


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
  ggtitle("Home Audio - Linear Model") +xlab("Variables")


######################## Leniar + AdStock Model #############################################################################################
colnames(homeAudio_intermediate)
str(homeAudio_intermediate)
AdLinear_model<-  sapply(homeAudio_intermediate, function(x) scale(x))

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

#adOther
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital + Sponsorship +
                  Content.Marketing + SEM + Radio + Other + adTV + adSponsorship +
                  adContent.Marketing + adAffiliates + adSEM + adRadio ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Content.Marketing
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital + Sponsorship
                + SEM + Radio + Other + adTV + adSponsorship +
                  adContent.Marketing + adAffiliates + adSEM + adRadio ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adRadio
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital + Sponsorship
                + SEM + Radio + Other + adTV + adSponsorship +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Radio
adModel_3 <- lm(formula = gmv ~ product_mrp + sla + markdown + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital + Sponsorship
                + SEM  + Other + adTV + adSponsorship +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]


#markdown
adModel_3 <- lm(formula = gmv ~ product_mrp + sla  + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital + Sponsorship
                + SEM  + Other + adTV + adSponsorship +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
adModel_3 <- lm(formula = gmv ~ product_mrp + sla  + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital
                + SEM  + Other + adTV + adSponsorship +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adSponsorship
adModel_3 <- lm(formula = gmv ~ product_mrp + sla  + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital
                + SEM  + Other + adTV  +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#Other
adModel_3 <- lm(formula = gmv ~ product_mrp + sla  + product_procurement_sla +
                  prepaid_percentage + NPS_Score + Digital
                + SEM   + adTV  +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla
adModel_3 <- lm(formula = gmv ~ product_mrp + sla  +
                  prepaid_percentage + NPS_Score + Digital
                + SEM   + adTV  +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
adModel_3 <- lm(formula = gmv ~ product_mrp + sla
                + NPS_Score + Digital
                + SEM   + adTV  +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)         -9.723e-16  8.124e-02   0.000        1  
# product_mrp          4.999e-01  1.005e-01   4.977 1.28e-05 ***
#   sla                  7.044e-01  1.259e-01   5.595 1.75e-06 ***
#   NPS_Score           -1.147e+00  2.007e-01  -5.714 1.19e-06 ***
#   Digital              3.921e+00  6.652e-01   5.895 6.64e-07 ***
#   SEM                 -8.238e+00  1.277e+00  -6.450 1.10e-07 ***
#   adTV                -8.155e-01  1.571e-01  -5.191 6.43e-06 ***
#   adContent.Marketing -4.391e+00  6.680e-01  -6.574 7.39e-08 ***
#   adAffiliates         1.894e+00  3.044e-01   6.223 2.30e-07 ***
#   adSEM                7.309e+00  1.247e+00   5.862 7.39e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5745 on 40 degrees of freedom
# Multiple R-squared:  0.7306,    Adjusted R-squared:   0.67
# F-statistic: 12.05 on 9 and 40 DF,  p-value: 6.397e-09

#SEM
adModel_3 <- lm(formula = gmv ~ product_mrp + sla
                + NPS_Score + Digital
                + adTV  +
                  adContent.Marketing + adAffiliates + adSEM  ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adSEM
adModel_3 <- lm(formula = gmv ~ product_mrp + sla
                + NPS_Score + Digital
                + adTV  +
                  adContent.Marketing + adAffiliates   ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adAffiliates
adModel_3 <- lm(formula = gmv ~ product_mrp + sla
                + NPS_Score + Digital
                + adTV  +
                  adContent.Marketing,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

#adContentMarketing
adModel_3 <- lm(formula = gmv ~ product_mrp + sla
                + NPS_Score + Digital
                + adTV
                ,
                data = AdLinear_model)

summary(adModel_3)
vif<-vif(adModel_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|) 
# (Intercept) -1.714e-16  1.154e-01   0.000  1.00000 
# product_mrp  4.377e-01  1.354e-01   3.233  0.00232 **
#   sla          2.311e-01  1.440e-01   1.605  0.11565 
# NPS_Score   -2.829e-01  1.435e-01  -1.972  0.05492 .
# Digital      1.542e-01  1.444e-01   1.068  0.29139 
# adTV        -1.001e-01  1.238e-01  -0.808  0.42328 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.816 on 44 degrees of freedom
# Multiple R-squared:  0.4021,    Adjusted R-squared:  0.3342
# F-statistic: 5.919 on 5 and 44 DF,  p-value: 0.0002879
############################# ELASTICITY ANALYSIS LINEAR + ADSTOCK MODEL ##############################################################
##Final Model
AdLinear_Final_model_home <- adModel_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = AdLinear_model, form.lm = formula(gmv ~ product_mrp + sla  + NPS_Score + Digital+ adTV ),m = 10)
Cross_home[2] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- AdLinear_model


grlm <- AdLinear_Final_model_home


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
  ggtitle("Home Audio - Linear + Adstock Model") +xlab("Variables")



######################## Multiplicative Model ###########################

colnames(homeAudio_intermediate)
str(homeAudio_intermediate)
#Multply by 1 crore
homeAudio_intermediate[,c(13:22)]<-homeAudio_intermediate[,c(13:22)]*10000000
#Take log of the data
multiplicative_model <-log(homeAudio_intermediate + 0.01)
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
summary(mm_2)
vif<-vif(mm_2)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week
mm_3<- lm(formula = gmv ~ sla + markdown + NPS_Score  +
            Digital + Online.marketing + Affiliates + SEM, data = multiplicative_model)
summary(mm_3)
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#NPS_Score
mm_3<- lm(formula = gmv ~ sla + markdown   +
            Digital + Online.marketing + Affiliates + SEM, data = multiplicative_model)
summary(mm_3)
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]

#SEM
mm_3<- lm(formula = gmv ~ sla + markdown   +
            Digital + Online.marketing + Affiliates , data = multiplicative_model)
summary(mm_3)
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]
#Digital
mm_3<- lm(formula = gmv ~ sla + markdown   +
            + Online.marketing + Affiliates , data = multiplicative_model)
summary(mm_3)
vif<-vif(mm_3)
vif[order(vif,decreasing = TRUE)]


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       1.796e-15  1.173e-01   0.000 1.000000  
# sla               3.135e-01  1.444e-01   2.170 0.035300 *
#   markdown          5.792e-01  1.485e-01   3.901 0.000317 ***
#   Online.marketing  2.815e+00  1.165e+00   2.416 0.019818 *
#   Affiliates       -2.661e+00  1.185e+00  -2.246 0.029692 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.8297 on 45 degrees of freedom
# Multiple R-squared:  0.3678,    Adjusted R-squared:  0.3116
# F-statistic: 6.545 on 4 and 45 DF,  p-value: 0.0003065
############################# ELASTICITY ANALYSIS MULTIPLICATIVE MODEL ##############################################################
##Final Model
Multiplicative_Final_model_home <- mm_3

# Adj R square  = 0.3606  with 3 variables
#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = multiplicative_model, form.lm = formula(gmv ~ sla + markdown + Online.marketing + Affiliates ),m = 10)
Cross_home[3] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- multiplicative_model


grlm <- Multiplicative_Final_model_home


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
  ggtitle("Home Audio - Multiplicative Model") +xlab("Variables")

############################################### Multiplicative Ad Stock Model ######################################
colnames(homeAudio_intermediate)
str(homeAudio_intermediate)

#Take log of the data
adMultiplicative_model <-log(homeAudio_intermediate + 0.01)
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

#NPS_Score
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown +
              prepaid_percentage  + count_promotion_days_in_week +
              TV + Sponsorship + Online.marketing + Affiliates + Radio +
              Other + adTV + adSponsorship + adContent.Marketing + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#prepaid_percentage
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            + count_promotion_days_in_week +
              TV + Sponsorship + Online.marketing + Affiliates + Radio +
              Other + adTV + adSponsorship + adContent.Marketing + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#TV
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            + count_promotion_days_in_week
            + Sponsorship + Online.marketing + Affiliates + Radio +
              Other + adTV + adSponsorship + adContent.Marketing + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#adContent.Marketing
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            + count_promotion_days_in_week
            + Sponsorship + Online.marketing + Affiliates + Radio +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            + count_promotion_days_in_week
            + Online.marketing + Affiliates + Radio +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]


#Radio
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            + count_promotion_days_in_week
            + Online.marketing + Affiliates  +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]


#Affiliates
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            + count_promotion_days_in_week
            + Online.marketing   +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#count_promotion_days_in_week
admm_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown
            
            + Online.marketing   +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#product_mrp
admm_3<- lm(formula = gmv ~ list_price  + sla + markdown
            + Online.marketing   +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#list_price
admm_3<- lm(formula = gmv ~   sla + markdown
            + Online.marketing   +
              Other + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

#Other
admm_3<- lm(formula = gmv ~   sla + markdown
            + Online.marketing
            + adTV + adSponsorship  + adOnline.marketing +
              adAffiliates, data = adMultiplicative_model)
summary(admm_3)
vif<-vif(admm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)        -5.195e-15  1.089e-01   0.000  1.00000  
# sla                 4.771e-01  1.409e-01   3.386  0.00155 **
#   markdown            7.096e-01  1.536e-01   4.621 3.61e-05 ***
#   Online.marketing    2.325e+00  7.700e-01   3.020  0.00429 **
#   adTV               -1.175e+00  3.454e-01  -3.401  0.00148 **
#   adSponsorship       7.809e-01  2.339e-01   3.338  0.00177 **
#   adOnline.marketing -8.409e+00  2.653e+00  -3.170  0.00285 **
#   adAffiliates        6.829e+00  2.277e+00   2.999  0.00454 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.7698 on 42 degrees of freedom
# Multiple R-squared:  0.4921,    Adjusted R-squared:  0.4075
# F-statistic: 5.813 on 7 and 42 DF,  p-value: 9.4e-05
############################# ELASTICITY ANALYSIS MULTIPLICATIVE AD STOCK MODEL ##############################################################
##Final Model
Multiplicative_AD_Stock_Final_model_home <- admm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = adMultiplicative_model, form.lm = formula(gmv ~ sla + markdown + Online.marketing
                                                                        + adTV + adSponsorship  + adOnline.marketing + adAffiliates),m = 10)
Cross_home[4] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- adMultiplicative_model


grlm <- Multiplicative_AD_Stock_Final_model_home


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
  ggtitle("Home Audio - Multiplicative Ad Stock Model") +xlab("Variables")


###################################### Koyck Model #######################################################################
colnames(homeAudio_intermediate)
str(homeAudio_intermediate)

#Get the dataset
koyck_model <- data.frame(homeAudio_intermediate[,-c(1,5,12,13,23:31)])


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

#Content.Marketing
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + Digital + Sponsorship
               + Online.marketing + Affiliates + SEM +
                 Radio + Other, data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Online.marketing
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + Digital + Sponsorship
               + Affiliates + SEM +
                 Radio + Other, data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -3.606e-16  9.521e-02   0.000  1.00000  
# product_mrp  8.785e-01  1.325e-01   6.630 6.16e-08 ***
#   sla          7.585e-01  1.533e-01   4.949 1.39e-05 ***
#   Digital      7.925e+00  1.739e+00   4.557 4.80e-05 ***
#   Sponsorship  1.879e+00  3.690e-01   5.092 8.82e-06 ***
#   Affiliates   1.039e+00  2.492e-01   4.168  0.00016 ***
#   SEM         -9.381e+00  2.042e+00  -4.593 4.28e-05 ***
#   Radio        3.771e+00  9.173e-01   4.111  0.00019 ***
#   Other       -3.530e+00  8.721e-01  -4.048  0.00023 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.6665 on 40 degrees of freedom
# Multiple R-squared:  0.6298,    Adjusted R-squared:  0.5558
# F-statistic: 8.508 on 8 and 40 DF,  p-value: 1.123e-06

#SEM
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + Digital + Sponsorship
               + Affiliates  +
                 Radio + Other, data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Radio
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla + Digital + Sponsorship
               + Affiliates
               + Other, data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]


#Digital
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla  + Sponsorship
               + Affiliates
               + Other, data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Affiliates
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla  + Sponsorship
               
               + Other, data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Other
kyockm_3 <- lm(formula = gmv ~ product_mrp + sla  + Sponsorship
               , data = koyck_model)
summary(kyockm_3)
vif<- vif(kyockm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -4.695e-16  1.122e-01   0.000  1.00000  
# product_mrp  5.914e-01  1.224e-01   4.833  1.6e-05 ***
#   sla          3.012e-01  1.224e-01   2.462  0.01772 *
#   Sponsorship  3.338e-01  1.133e-01   2.946  0.00509 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.7852 on 45 degrees of freedom
# Multiple R-squared:  0.4221,    Adjusted R-squared:  0.3835
# F-statistic: 10.95 on 3 and 45 DF,  p-value: 1.595e-05
############################# ELASTICITY ANALYSIS KOYCK MODEL ##############################################################
##Final Model
Koyck_Final_model_home <- kyockm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = koyck_model, form.lm = formula(gmv ~ product_mrp + sla  + Sponsorship),m = 10)
Cross_home[5] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- koyck_model


grlm <- Koyck_Final_model_home


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
  ggtitle("Home Audio -Koyck Model") +xlab("Variables")


###################################### Kyock + Adstock Model #######################################################################
colnames(homeAudio_intermediate)
str(homeAudio_intermediate)

#Get the dataset
ad_koyck_model <- data.frame(homeAudio_intermediate[,-c(1,5,12,13)])

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

#sla
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates + SEM +
                    Radio + Other + adContent.Marketing + adOnline.marketing +
                    adAffiliates + adSEM + adRadio + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adRadio
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates + SEM +
                    Radio + Other + adContent.Marketing + adOnline.marketing +
                    adAffiliates + adSEM  + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#SEM
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates  +
                    Radio + Other + adContent.Marketing + adOnline.marketing +
                    adAffiliates + adSEM  + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adSEM
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates  +
                    Radio + Other + adContent.Marketing + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Other
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates  +
                    Radio + adContent.Marketing + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Radio
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates
                  + adContent.Marketing + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]


#adContent.Marketing
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp + markdown + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates
                  + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#markdown
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp  + prepaid_percentage +
                    Digital + Sponsorship + Online.marketing + Affiliates
                  + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Sponsorship
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp  + prepaid_percentage +
                    Digital  + Online.marketing + Affiliates
                  + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Affiliates
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp  + prepaid_percentage +
                    Digital  + Online.marketing
                  + adOnline.marketing +
                    adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#adOnline.marketing
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp  + prepaid_percentage +
                    Digital  + Online.marketing
                  + adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

#Online.marketing
ad_kyockm_3 <- lm(formula = gmv ~ product_mrp  + prepaid_percentage +
                    Digital  
                  + adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]


ad_kyockm_3 <- lm(formula = gmv ~ product_mrp  + prepaid_percentage +
                    Digital  
                  + adAffiliates   + adOther + lag_gmv, data = ad_koyck_model)
summary(ad_kyockm_3)
vif<- vif(ad_kyockm_3)
vif[order(vif,decreasing = TRUE)]

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|) 
# (Intercept)        -3.332e-16  1.239e-01   0.000  1.00000 
# product_mrp         4.198e-01  1.362e-01   3.081  0.00363 **
#   prepaid_percentage -4.554e-02  1.334e-01  -0.341  0.73447 
# Digital             2.696e-01  1.503e-01   1.794  0.07999 .
# adAffiliates        1.948e-02  1.437e-01   0.136  0.89282 
# adOther             1.850e-02  1.372e-01   0.135  0.89338 
# lag_gmv             9.097e-02  1.515e-01   0.600  0.55142 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.8675 on 42 degrees of freedom
# Multiple R-squared:  0.3416,    Adjusted R-squared:  0.2475
# F-statistic: 3.631 on 6 and 42 DF,  p-value: 0.005427

############################# ELASTICITY ANALYSIS KOYCK + Ad Stock MODEL ##############################################################
##Final Model
Koyck_Adstock_Final_model_home <- ad_kyockm_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = ad_koyck_model, form.lm = formula(gmv ~ product_mrp  + prepaid_percentage +
                                                                  Digital+ adAffiliates   + adOther + lag_gmv),m = 10)
Cross_home[6] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- ad_koyck_model


grlm <- Koyck_Adstock_Final_model_home


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
  ggtitle("Home Audio -Koyck + Adstock Model") +xlab("Variables")


###################################### FEATURE ENGINEERING PART - 2:#######################################################################

###################################### Moving Averages ######################################

library(TTR)
homeAudio_intermediate$list_price_maOne <- runMean(homeAudio_intermediate$list_price,1)
homeAudio_intermediate$list_price_ma1 <- runMean(homeAudio_intermediate$list_price,2)
homeAudio_intermediate$list_price_ma2 <- runMean(homeAudio_intermediate$list_price,3)
homeAudio_intermediate$list_price_ma3 <- runMean(homeAudio_intermediate$list_price,4)

homeAudio_intermediate$markdown_ma1 <- runMean(homeAudio_intermediate$markdown,2)
homeAudio_intermediate$markdown_ma2 <- runMean(homeAudio_intermediate$markdown,3)
homeAudio_intermediate$markdown_ma3 <- runMean(homeAudio_intermediate$markdown,4)


#Shelf price inflationith week = List price ith week / List price (i-1)th week
#TODO:Revisit This
homeAudio_intermediate$shelf_price_inflation_1= homeAudio_intermediate$list_price/homeAudio_intermediate$list_price_maOne
homeAudio_intermediate$shelf_price_inflation_2= homeAudio_intermediate$list_price/homeAudio_intermediate$list_price_ma1
homeAudio_intermediate$shelf_price_inflation_3= homeAudio_intermediate$list_price/homeAudio_intermediate$list_price_ma2
homeAudio_intermediate$shelf_price_inflation_4= homeAudio_intermediate$list_price/homeAudio_intermediate$list_price_ma3

#% discount offered ith week = Discounted price ith week / List price ith week ------(wrt List price)
#% discount offered ith week = Discounted price ith week / MRP ith week ------(wrt MRP)

homeAudio_intermediate$markdown_inflation1<-
  (homeAudio_intermediate$markdown - homeAudio_intermediate$markdown_ma1)/homeAudio_intermediate$markdown_ma1
homeAudio_intermediate$markdown_inflation2<-
  (homeAudio_intermediate$markdown - homeAudio_intermediate$markdown_ma2)/homeAudio_intermediate$markdown_ma2

homeAudio_intermediate$markdown_inflation1<-
  (homeAudio_intermediate$markdown - homeAudio_intermediate$markdown_ma3)/homeAudio_intermediate$markdown_ma3


#install.packages("DataCombine")
library(DataCombine)

#List of list price by 1,2,3 dates (Date values are ordered)
#Previous List price
homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "list_price",slideBy = -1)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "list_price",slideBy = -2)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "list_price", slideBy = -3)

#9.lag the promotion variables

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "markdown", slideBy = -1)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "markdown", slideBy = -2)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "markdown", slideBy = -3)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "NPS_Score", slideBy = -1)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "NPS_Score", slideBy = -2)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "NPS_Score", slideBy = -3)


homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "count_promotion_days_in_week", slideBy = -1)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "count_promotion_days_in_week", slideBy = -2)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "count_promotion_days_in_week", slideBy = -3)


homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "gmv",slideBy = -1)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "gmv",slideBy = -2)

homeAudio_intermediate <- slide(homeAudio_intermediate, Var = "gmv",slideBy = -3)

homeAudioDFUsedForMultiplicativeDistibutedLag <- homeAudio_intermediate

sum(is.na(homeAudio_intermediate))
homeAudio_final <- na.omit(homeAudio_intermediate)
sum(is.na(homeAudio_final))
head(homeAudio_final)
DL_model<-  sapply(homeAudio_final, function(x) scale(x))

colnames(DL_model)
#list_price_maOne
# list_price.1                          NA         NA      NA       NA
# list_price.2                          NA         NA      NA       NA
# list_price.3                          NA         NA      NA       NA
# markdown.1                            NA         NA      NA       NA
# markdown.2                            NA         NA      NA       NA
# markdown.3
#gmv.3

DL_model<- data.frame(DL_model[, -c(1,5,12,13,32,45:50,59)])

DL_model<-na.omit(DL_model)

sum(is.na(DL_model))


head(DL_model)

model_1 <- lm(gmv~.,DL_model)

# Summary of Linear Model
summary(model_1) 

library(car)
library(MASS)

model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
vif(model_2)


#shelf_price_inflation_3
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates + SEM +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1 + markdown_ma2 +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#SEM
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla + markdown +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1 + markdown_ma2 +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#markdown
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1 + markdown_ma2 +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#markdown_ma2
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]






#markdown_inflation2
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#count_promotion_days_in_week.2
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               product_procurement_sla + prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]




#product_procurement_sla
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               count_promotion_days_in_week + TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]





#count_promotion_days_in_week
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#count_promotion_days_in_week
model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV + Digital + Sponsorship +
               Content.Marketing + Online.marketing + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#removing Online.marketing

model_3<- lm(formula = gmv ~ list_price + product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV + Digital + Sponsorship +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#removing list_price
model_3<- lm(formula = gmv ~   product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV + Digital + Sponsorship +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#removing adOnline.marketing
model_3<- lm(formula = gmv ~   product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV + Digital + Sponsorship +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#removing Digital
model_3<- lm(formula = gmv ~   product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV  + Sponsorship +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#removing Sponsorship
model_3<- lm(formula = gmv ~   product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM + adRadio + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#adRadio
model_3<- lm(formula = gmv ~   product_mrp + sla  +
               prepaid_percentage + NPS_Score +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#product_mrp

model_3<- lm(formula = gmv ~sla  +
               prepaid_percentage + NPS_Score +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]




#NPS_Score
model_3<- lm(formula = gmv ~sla  +
               prepaid_percentage  +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma2 + list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]







# list_price_ma2

model_3<- lm(formula = gmv ~sla  +
               prepaid_percentage  +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma3 + markdown_ma1  +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#markdown_ma1
model_3<- lm(formula = gmv ~sla  +
               prepaid_percentage  +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]




# NPS_Score.1
model_3<- lm(formula = gmv ~sla  +
               prepaid_percentage  +
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


# prepaid_percentage
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               NPS_Score.3 + count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]




# NPS_Score.3  
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adAffiliates + adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



# adAffiliates  
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



# adAffiliates  
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.1 + gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#gmv.1
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               count_promotion_days_in_week.1 +
               count_promotion_days_in_week.3 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#count_promotion_days_in_week.3
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]




#count_promotion_days_in_week.3
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               shelf_price_inflation_4 + markdown_inflation1  +
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#shelf_price_inflation_4
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3 + shelf_price_inflation_2  +
               markdown_inflation1  +
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#shelf_price_inflation_2

model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3   +
               markdown_inflation1  +
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#shelf_price_inflation_2
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3   +
               markdown_inflation1  +
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#markdown_inflation1
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + list_price_ma1 +
               list_price_ma3   +
               markdown_ma3   + 
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#list_price_ma1

model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  + 
               list_price_ma3   +
               markdown_ma3   + 
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#list_price_ma3

model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship + adContent.Marketing +
               adSEM  +  
               markdown_ma3   + 
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


# adContent.Marketing

model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship  +
               adSEM  +  
               markdown_ma3   + 
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#markdown_ma3
model_3<- lm(formula = gmv ~sla  + 
               TV   +
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship  +
               adSEM  +  
               
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#TV
model_3<- lm(formula = gmv ~sla  +  
               Content.Marketing  + Affiliates +
               Radio + Other + adTV + adSponsorship  +
               adSEM  +   
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]


#adSponsorship
model_3<- lm(formula = gmv ~sla  +  
               Content.Marketing  + Affiliates +
               Radio + Other + adTV   +
               adSEM  +   
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]



#adSponsorship
model_3<- lm(formula = gmv ~sla  +  
               Content.Marketing  + Affiliates +
               Radio + Other + adTV   +
               adSEM  +   
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]

#sla
model_3<- lm(formula = gmv ~  
               Content.Marketing  + Affiliates +
               Radio + Other + adTV   +
               adSEM  +   
               count_promotion_days_in_week.1 +
               gmv.2, data = DL_model)


summary(model_3)
vif<- vif(model_3)
vif[order(vif,decreasing = TRUE)]







#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)                     1.228e-16  1.095e-01   0.000 1.000000   
#Content.Marketing              -1.530e+00  5.231e-01  -2.925 0.005711 **
#  Affiliates                      9.430e-01  2.227e-01   4.235 0.000135 ***
#  Radio                          -1.140e+00  3.982e-01  -2.863 0.006712 **
#  Other                           1.155e+00  3.727e-01   3.100 0.003584 **
#  adTV                           -8.151e-01  1.873e-01  -4.351 9.46e-05 ***
#  adSEM                           1.866e+00  5.123e-01   3.641 0.000787 ***
#  count_promotion_days_in_week.1 -2.813e-01  1.145e-01  -2.456 0.018602 * 
#  gmv.2                          -4.280e-01  1.394e-01  -3.070 0.003886 **


#####################################################################################################################################################################
############################# ELASTICITY ANALYSIS DISTRIBUTED LAG MODEL ##############################################################
##Final Model
Distributed_Lag_Final_model_home <- model_3


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = DL_model, form.lm = formula(gmv ~ product_mrp + sla + Sponsorship ),m = 10)
Cross_home[7] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- DL_model


grlm <- Distributed_Lag_Final_model_home


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
  ggtitle("Home Audio -Distributed Lag Model") +xlab("Variables")


#################################################################################################################################################################################################
###############################################################  Multiplicative and Distibuted lag ##########################################################################
#################################################################################################################################################################################################


gamingAccessory_intermediate <- homeAudioDFUsedForMultiplicativeDistibutedLag
multiplicative_distibuted_final <- gamingAccessory_intermediate 

colnames(multiplicative_distibuted_final)
head(multiplicative_distibuted_final) 

# Making the money in same unit by multiplying 10000000
gamingAccessory_intermediate[,c(13:22)]<-gamingAccessory_intermediate[,c(13:22)]*10000000


gamingAccessory_intermediate <- gamingAccessory_intermediate + .01
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


#shelf_price_inflation_3
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla + markdown +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                count_promotion_days_in_week + TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates + SEM +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3 + markdown_ma1 + markdown_ma2 +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#SEM

model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla + markdown +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                count_promotion_days_in_week + TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3 + markdown_ma1 + markdown_ma2 +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#markdown
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                count_promotion_days_in_week + TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3 + markdown_ma1 + markdown_ma2 +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#markdown_ma2
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                count_promotion_days_in_week + TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3 + markdown_ma1  +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#markdown_ma1
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                count_promotion_days_in_week + TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1 + markdown_inflation2 +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




# markdown_inflation2
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                count_promotion_days_in_week + TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




# count_promotion_days_in_week
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage + NPS_Score +
                TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#product_procurement_sla

model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                prepaid_percentage + NPS_Score +
                TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#NPS_Score
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage  +
                TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.1 + NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#NPS_Score.1
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                product_procurement_sla + prepaid_percentage  +
                TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]






#product_procurement_sla
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                prepaid_percentage  +
                TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma2 + list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#list_price_ma2
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                prepaid_percentage  +
                TV + Digital + Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# Digital
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                prepaid_percentage  +
                TV +  Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                NPS_Score.3 + count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




# NPS_Score.3
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                prepaid_percentage  +
                TV +  Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  +
                count_promotion_days_in_week.1 +
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# count_promotion_days_in_week.1
model_2 <- lm(formula = gmv ~ list_price + product_mrp + sla  +
                prepaid_percentage  +
                TV +  Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# product_mrp
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +  Sponsorship +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




# Sponsorship
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.2 + count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# count_promotion_days_in_week.2
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio + list_price_ma1 +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]





# list_price_ma1
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM + adRadio  +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]






# adRadio
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM   +
                list_price_ma3   +
                markdown_ma3 + shelf_price_inflation_2  +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# shelf_price_inflation_2
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing + Affiliates  +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM   +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# Affiliates
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing   +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adOnline.marketing + adAffiliates + adSEM   +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                count_promotion_days_in_week.3 +
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# count_promotion_days_in_week.3
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing + Online.marketing   +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adAffiliates + adSEM   +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# Online.marketing
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing    +
                Radio + Other + adTV + adSponsorship + adContent.Marketing +
                adAffiliates + adSEM   +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# adContent.Marketing
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing    +
                Radio + Other + adTV + adSponsorship  +
                adAffiliates + adSEM   +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# adSEM
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV +
                Content.Marketing    +
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]




# Content.Marketing
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   +
                markdown_ma3   +
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# markdown_ma3 
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# gmv.2 
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1 + gmv.2, data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# gmv.2 
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  +
                TV + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1  , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# TV 
model_2 <- lm(formula = gmv ~ list_price  + sla  +
                prepaid_percentage  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1  , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



# prepaid_percentage 
model_2 <- lm(formula = gmv ~ list_price  + sla  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1  + 
                gmv.1  , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# gmv.1    
model_2 <- lm(formula = gmv ~ list_price  + sla  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1 
              , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


# gmv.1    
model_2 <- lm(formula = gmv ~ list_price  + sla  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1 
              , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#list_price

model_2 <- lm(formula = gmv ~ sla  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                shelf_price_inflation_4 + markdown_inflation1 
              , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]

#shelf_price_inflation_4
model_2 <- lm(formula = gmv ~ sla  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    +
                list_price_ma3   + 
                markdown_inflation1 
              , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#list_price_ma3
model_2 <- lm(formula = gmv ~ sla  + 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    + 
                markdown_inflation1 
              , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]


#sla
model_2 <- lm(formula = gmv ~ 
                Radio + Other + adTV + adSponsorship  +
                adAffiliates    + 
                markdown_inflation1 
              , data = DL_model)


summary(model_2)
vif<- vif(model_2)
vif[order(vif,decreasing = TRUE)]



#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)          3.06e-17   1.10e-01    0.00  1.00000   
#Radio               -8.03e-01   3.81e-01   -2.11  0.04102 * 
#  Other                8.44e-01   3.58e-01    2.36  0.02329 * 
#  adTV                -5.81e-01   1.62e-01   -3.58  0.00090 ***
#  adSponsorship        4.45e-01   1.42e-01    3.13  0.00324 **
#  adAffiliates         4.99e-01   1.64e-01    3.04  0.00414 **
#  markdown_inflation1  4.05e-01   1.13e-01    3.57  0.00092 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 0.761 on 41 degrees of freedom
#Multiple R-squared:  0.495,  Adjusted R-squared:  0.421


#Radio               Other        adAffiliates                adTV       adSponsorship markdown_inflation1
#11.76               10.40                2.19                2.14                1.64                1.04

######################################################################################################################################################################

######################################################################################################################################################################

############################# ELASTICITY ANALYSIS MULTIPLICATIVE + DISTRIBUTED LAG MODEL ##############################################################
##Final Model
Multiplicative_Distributed_Lag_Final_model_home <- model_2


#install.packages("DAAG")
#library(DAAG)
temp_crossval <- cv.lm(data = DL_model, form.lm = formula(gmv ~ Radio + Other + adTV + adSponsorship  +
                                                                adAffiliates    + 
                                                                markdown_inflation1 ),m = 10)
Cross_home[7] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- DL_model


grlm <- Multiplicative_Distributed_Lag_Final_model_home


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
  ggtitle("Home Audio -Multiplicative + Distributed Lag Model") +xlab("Variables")

























