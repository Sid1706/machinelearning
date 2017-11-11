
################################################################

### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("Rcpp")
#install.packages("ModelMetrics")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)

#Loading all the data files
emp_survey <-read.csv(file = "employee_survey_data.csv")
gen_data<- read.csv(file="general_data.csv")
mgr_survey <- read.csv(file="manager_survey_data.csv")
in_time <- read.csv(file="in_time.csv")
out_time <- read.csv(file="out_time.csv")

str(emp_survey) #4410 observations with 4 variables
str(gen_data)   #4410 observations with 24 variables
str(mgr_survey) #4410 observations with 3 variables
str(in_time)    #4410 observations with 262 variables
str(out_time)   #4410 observations with 262 variables


# Collate the data together in one single file
length(unique(tolower(emp_survey$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(gen_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(mgr_survey$EmployeeID))) # 4410, confirming EmployeeID is key

length(unique(tolower(in_time$X))) # 4410, confirming EmployeeID is key
length(unique(tolower(out_time$X))) # 4410, confirming EmployeeID is key

setdiff(gen_data$EmployeeID,emp_survey$EmployeeID) # Identical EmployeeID across these employeeDatas
setdiff(gen_data$EmployeeID,mgr_survey$EmployeeID) # Identical EmployeeID across these employeeDatas

#Rename the employee id column from X
colnames(in_time)[colnames(in_time)=="X"] <- "EmployeeID"
in_time$EmployeeID

colnames(out_time)[colnames(out_time)=="X"] <- "EmployeeID"
out_time$EmployeeID

setdiff(gen_data$EmployeeID,in_time$EmployeeID) # Identical EmployeeID across these employeeDatas
setdiff(gen_data$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these employeeDatas



employeeData <- merge(gen_data,emp_survey,by="EmployeeID",all = F)
employeeData<- merge(employeeData,mgr_survey, by="EmployeeID",all=F)

#Merge the intime and out time here
#This function will convert the String DateTime into Standred R Object DateTime with dd:mm:YYYY HH:MM format.
convertIntoDateTime <- function(x)
{
  return(as.POSIXlt(x,format="%Y-%m-%d %H:%M",tz=""))
}

outTimeDataFrameTemp<-  as.data.frame(apply(out_time[,-1], 2, convertIntoDateTime))

outTimeDataFrameTemp$EmployeeId <- out_time$X
#View(outTimeDataFrameTemp)


inTimeDataFrameTemp<-  as.data.frame(apply(in_time[,-1], 2, convertIntoDateTime))

inTimeDataFrameTemp$EmployeeId <- in_time$X
#View(inTimeDataFrameTemp)


timeDifference <- outTimeDataFrameTemp[,-1]-inTimeDataFrameTemp[,-1]

sum(!is.na(timeDifference))
# Data Preprocessing

timeDifference[-1] <- lapply(timeDifference[-1], function(x) as.numeric(sub("\\s+\\D+$", "", x)))

str(timeDifference)
timeDifference$DeskTime<-apply(timeDifference[-1],1, mean, na.rm=TRUE)
employeeData$deskTime<- timeDifference$DeskTime


View(employeeData)

###############################################  REMOVE UNUSED DATAFRAMES TO KEEP MEMPRY FREE FOR FURTHER OPEARTIONS #############################################
remove(emp_survey)
remove(in_time)
remove(inTimeDataFrameTemp) 
remove(out_time)
remove(outTimeDataFrameTemp) 
remove(gen_data)
remove(mgr_survey)
remove(timeDifference)

################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(employeeData) #4410 obs. of 30 variables;

nrow(employeeData[ which(employeeData$Over18=="N"),])
# All employees are above 18years and hence dropping this folder.
employeeData <- employeeData[,-16]

str(employeeData)
nrow(employeeData[ which(employeeData$StandardHours !=8),])
# For all the employees StandardHours is 8 hours and hence removing this factor as well.
employeeData<- employeeData[,-17]

str(employeeData)
# For all the employees employee count is 1 and hence removing this factor as well.
nrow(employeeData[ which(employeeData$EmployeeCount !=1),])
employeeData<- employeeData[,-9]

#check for NA in the employeeData
sum(is.na(employeeData)) #111

#The following are the fields that are of categorical but values are numeric. Converting them to categorical
# Education
employeeData$Education <-factor(employeeData$Education)
levels(employeeData$Education) <- c("Below College","College","Bachelor","Master","Doctor")

# JobLevel
employeeData$JobLevel <-factor(employeeData$JobLevel)
levels(employeeData$JobLevel) <- c("One","Two","Three","Four","Five")

# StockOptionLevel
employeeData$StockOptionLevel <-factor(employeeData$StockOptionLevel)
levels(employeeData$StockOptionLevel) <- c("Zero","One","Two","Three")

#TODO: Remove this TrainingTimesLastYear
#employeeData$TrainingTimesLastYear <-factor(employeeData$TrainingTimesLastYear)
#levels(employeeData$TrainingTimesLastYear) <- c("Zero","One","Two","Three","Four","Five","Six")

# EnvironmentSatisfaction
employeeData$EnvironmentSatisfaction <-factor(employeeData$EnvironmentSatisfaction)
levels(employeeData$EnvironmentSatisfaction) <- c("Low","Medium","High","Very High")

# JobSatisfaction
employeeData$JobSatisfaction <-factor(employeeData$JobSatisfaction)
levels(employeeData$JobSatisfaction) <- c("Low","Medium","High","Very High")

# WorkLifeBalance
employeeData$WorkLifeBalance <-factor(employeeData$WorkLifeBalance)
levels(employeeData$WorkLifeBalance) <- c("Bad","Good","Better","Best")

# JobInvolvement
employeeData$JobInvolvement <-factor(employeeData$JobInvolvement)
levels(employeeData$JobInvolvement) <- c("Low","Medium","High","Very High")

# PerformanceRating
employeeData$PerformanceRating <-factor(employeeData$PerformanceRating)
levels(employeeData$PerformanceRating) <- c("Excellent","Outstanding")

#TODO: Remove this Convert the dependent variable Attrition into factor variable
#employeeData$Attrition <-factor(employeeData$Attrition)

write.csv(employeeData, "intermediateData.csv")

str(employeeData)
###########################EDA PLOTS#######################################

#Barcharts for categorical features with stacked attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="top")


plot_grid(ggplot(employeeData, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
#Looking at the plot above the following categories seems to have a higher attrition levels:
#1. Travel Rarely
#2.Eduction Level - Bachelor
#3.Research and Development Department
#4. Lifescience and Medical Field

          
plot_grid(ggplot(employeeData, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=JobLevel,fill=Attrition))+ geom_bar(), 
          ggplot(employeeData, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
#Looking at the plot above the following categories seems to have a higher attrition levels:
#Male Emloyees
#Job level 1 & 2
#Research Scientist,Sales Executive Job Role
#Marital Status - Single

plot_grid(ggplot(employeeData, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
#Looking at the plot above the following categories seems to have a higher attrition levels:
#Stock Option Level 0 & 1
#All environment satisfaction level except medium
#Low and high Job Satisfaction Levels

plot_grid(ggplot(employeeData, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeData, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
#Looking at the plot above the following categories seems to have a higher attrition levels:
#Better Worklife balance
#High Job Involvement
#Low Performance Rating


# Histogram and Boxplots for numeric variables to identify existance of outliers and rectify them
          #Age
          #DistanceFromHome
          #MonthlyIncome
          #NumCompaniesWorked
          #PercentSalaryHike
          #TotalWorkingYears
          #YearsAtCompany
          #YearsSinceLastPromotion
          #YearsWithCurrManager
          #desktime
          
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                            axis.ticks=element_blank(), axis.text=element_blank())
          
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                              axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                              legend.position="none")
          
plot_grid(ggplot(employeeData, aes(Age))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#No outliers and more employees in the age group of 30 & 40
          
plot_grid(ggplot(employeeData, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)

#No outliers more employees stay within 10kms from the office
          
plot_grid(ggplot(employeeData, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#Outliers exist will be treated below.

          
plot_grid(ggplot(employeeData, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#Outliers exist, majority of employees have had less than 5 job changes

plot_grid(ggplot(employeeData, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#No outliers and majority employees have had <15% hike
          
plot_grid(ggplot(employeeData, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#Outliers exist, majority employees are between 5-15 years work experience
          
plot_grid(ggplot(employeeData, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#Outliers exist and majority employees are between 0-15 years at the company

plot_grid(ggplot(employeeData, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#Outliers exist and there are some employees who have not been promoted for more than 5 years.

plot_grid(ggplot(employeeData, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#Outliers exist and there arre employees working with the same manager for more than 10 years
          
plot_grid(ggplot(employeeData, aes(deskTime))+ geom_histogram(binwidth = 10),
                    ggplot(employeeData, aes(x="",y=deskTime))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                    align = "v",ncol = 1)
#outliers exist as there are some employees working for 15 hours a day

plot_grid(ggplot(employeeData, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(employeeData, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Outliers exist eon both sides and majority of the employees have had trainings last year.


# Boxplots of numeric variables relative to attrition status
plot_grid(ggplot(employeeData, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=deskTime, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    ggplot(employeeData, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                      coord_flip() +theme(legend.position="none"),
                    align = "v",nrow = 1)
          
#There are outliers in the following attributes:
#Age
#MonthlyIncome
#NumCompaniesWorked
#PercentSalaryHike
#TotalWorkingYears
#YearsAtCompany
#YearsSinceLastPromotion
#YearsWithCurrManager
#deskTime
#TrainingTimesLastYear

          
# Correlation between numeric variables

ggpairs(employeeData[, c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike",
                                   "TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","deskTime","TrainingTimesLastYear")])
          
#TODO: Commentry on the above plot
          
###############################################################
### Data Preparation
          
# Outlier treatment and imputing missing value
#outliers<-Boxplot(~MonthlyIncome, data=employeeData, id.n=Inf) 
#install.packages("scales")
library(scales)
#employeeData$MonthlyIncome <- squish(employeeData$MonthlyIncome, round(quantile(employeeData$MonthlyIncome, c(.01, .95))))

#Treat the outliers with the squish method from scales library
sapply(employeeData[,c("Age","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike",
                       "TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","deskTime","TrainingTimesLastYear")], 
       function(x) squish(x, round(quantile(x, c(.01, .95),na.rm = T)))) 

#Confirming if the outliers have reduced.
plot_grid(ggplot(employeeData, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=deskTime, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeeData, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          align = "v",nrow = 1)

# Missing value treatment
sapply(employeeData, function(x) sum(is.na(x))) 
# shows NAs in the following fields: 
#EnvironmentSatisfaction(25) - Categorical, 
#JobSatisfaction(20) - Categorical ,
#WorkLifeBalance(38) - Categorical,
#TotalWorkingYears(9) - Quantitative,
#NumCompaniesWorked(19) - Quantitative

View(subset(employeeData, is.na(EnvironmentSatisfaction))) 
View(subset(employeeData, is.na(JobSatisfaction)))
View(subset(employeeData, is.na(WorkLifeBalance)))
View(subset(employeeData, is.na(TotalWorkingYears)))
View(subset(employeeData, is.na(NumCompaniesWorked)))

sum(is.na(employeeData))/nrow(employeeData)
#is 0.025 - Since the numbers are quite small remove the NA's

#Categorical Missing value treatment by removing them
employeeData <- employeeData[!is.na(employeeData$EnvironmentSatisfaction),]
employeeData <- employeeData[!is.na(employeeData$JobSatisfaction),]
employeeData <- employeeData[!is.na(employeeData$WorkLifeBalance),] #- 4327 Observations 27 Variables

#Quantative Missing Value treatment - Replace missingvalue with mean
employeeData$NumCompaniesWorked = ifelse(is.na(employeeData$NumCompaniesWorked),
                                         ave(employeeData$NumCompaniesWorked, FUN = function(x) mean(x, na.rm = TRUE)),
                                         employeeData$NumCompaniesWorked)

employeeData$TotalWorkingYears = ifelse(is.na(employeeData$TotalWorkingYears),
                                        ave(employeeData$TotalWorkingYears, FUN = function(x) mean(x, na.rm = TRUE)),
                                        employeeData$TotalWorkingYears)



sum(is.na(employeeData)) #0 No more missing values

################################################################
# Feature standardisation

str(employeeData)

# Normalising continuous features 
employeeData$Age<- scale(employeeData$Age) # scale used: mean 32.4, sd 24.6
employeeData$DistanceFromHome<- scale(employeeData$DistanceFromHome) # scale used: mean 64.8, sd 30.1
employeeData$MonthlyIncome<- scale(employeeData$MonthlyIncome) # scale used: mean 2280, sd 2267


employeeData$NumCompaniesWorked<- scale(employeeData$NumCompaniesWorked) # scale used: mean 32.4, sd 24.6
employeeData$PercentSalaryHike<- scale(employeeData$PercentSalaryHike) # scale used: mean 64.8, sd 30.1


employeeData$TotalWorkingYears<- scale(employeeData$TotalWorkingYears) # scale used: mean 32.4, sd 24.6

employeeData$YearsAtCompany<- scale(employeeData$YearsAtCompany) # scale used: mean 2280, sd 2267
employeeData$TrainingTimesLastYear<- scale(employeeData$TrainingTimesLastYear)  

employeeData$YearsSinceLastPromotion<- scale(employeeData$YearsSinceLastPromotion) # scale used: mean 2280, sd 2267
employeeData$YearsWithCurrManager<- scale(employeeData$YearsWithCurrManager) # scale used: mean 2280, sd 2267
employeeData$deskTime<- scale(employeeData$deskTime) # scale used: mean 2280, sd 2267

View(employeeData)
#Verified that all the quantitative variables are scaled
str(employeeData)

#Converting Attrition variable to numeric
employeeData$Attrition<- ifelse(employeeData$Attrition=="Yes",1,0)

Attrition <- sum(employeeData$Attrition)/nrow(employeeData)
Attrition # 16.2% churn rate. 



# creating a dataframe of categorical features
employeeData_chr<- employeeData[,c(4,5,7,8,9,10,11,12,16,22,23,24,25,26)]

# converting categorical attributes to factor
employeeData_fact<- data.frame(sapply(employeeData_chr, function(x) factor(x)))
str(employeeData_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employeeData_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employeeData_fact))[,-1]))
# Final dataset
employeeData_final<- cbind(employeeData[,c(1,2,3,6,13,14,15,17,18,19,20,21,27)],dummies) 
View(employeeData_final) #4327 obs. of  57 variables
#Verified that all the variables are now numeric and ready for logit.

########################################################################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employeeData_final$Attrition, SplitRatio = 0.7)

train = employeeData_final[indices,] #3029 obs. of 57 variables

test = employeeData_final[!(indices),] #1298 obs. of 57 variables

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 2108.3....57 coeff..nullDev 2684.5...resDev 1994.3

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2) #coeff..36

# Removing multicollinearity through VIF check
library(car)
vif<-vif(model_2)
vif[order(vif,decreasing = TRUE)]
#Rules to remove variables:
#1. Build a model containing all variables
#2. Check VIF and summary
#3. Remove variables with high VIF (>2 generally) and which are insignificant (p>0.05), one by one
#4. If the model has variables which have a high VIF and are significant, check and remove other insignificant variables 
#5. After removing the insignificant variables, the VIFs should decline
#6. If some variables still have a high VIF, remove the variable which is relatively less significant
#7. Now, variables must be significant. If the number of variables is still high, remove them in order of
#  insignificance until you arrive at a limited number of variables that explain the model well.

#Remove Gender                              0.18178    0.11945   1.522 0.128057    
model_3<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor + EducationField.xMarketing + 
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.xZero + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_3)
vif<-vif(model_3)
vif[order(vif,decreasing = TRUE)]

#Remove EducationField.xMarketing          -0.35375    0.23194  -1.525 0.127217    
model_4<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.xZero + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_4)
vif<-vif(model_4)
vif[order(vif,decreasing = TRUE)]

#Remove JobRole.xManager                   -0.38300    0.24224  -1.581 0.113867    

model_5<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.xZero + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_5)
vif<-vif(model_5)
vif[order(vif,decreasing = TRUE)]


#Remove StockOptionLevel.xZero              0.19433    0.11725   1.657 0.097432 .  


model_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_6)
vif<-vif(model_6)
vif[order(vif,decreasing = TRUE)]

#Remove YearsAtCompany                      0.25682    0.14866   1.728 0.084060 .  

model_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_7)
vif<-vif(model_7)
vif[order(vif,decreasing = TRUE)]

#Remove EducationField.xTechnical.Degree   -0.38944    0.22572  -1.725 0.084478 .  


model_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                EducationField.xOther + 
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_8)
vif<-vif(model_8)
vif[order(vif,decreasing = TRUE)]

#Remove EducationField.xOther              -0.47682    0.28985  -1.645 0.099959 .  

model_9<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                JobLevel.xFour + JobLevel.xOne + JobLevel.xThree + 
                JobLevel.xTwo + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_9)
vif<-vif(model_9)
vif[order(vif,decreasing = TRUE)]

#Remove JobLevel.xThree                     0.55664    0.32681   1.703 0.088519 .  

model_10<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.xDoctor +  
                JobLevel.xFour + JobLevel.xOne + 
                JobLevel.xTwo + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_10)
vif<-vif(model_10)
vif[order(vif,decreasing = TRUE)]


#Remove JobLevel.xFour                      0.24220    0.25457   0.951 0.341412    
model_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xDoctor +  
                 JobLevel.xOne + 
                 JobLevel.xTwo + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                 JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_11)
vif<-vif(model_11)
vif[order(vif,decreasing = TRUE)]

#Remove JobLevel.xOne                       0.11016    0.14854   0.742 0.458321    
model_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xDoctor +  
                 JobLevel.xTwo + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                 JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_12)
vif<-vif(model_12)
vif[order(vif,decreasing = TRUE)]

#Remove JobLevel.xTwo                       0.22412    0.11774   1.904 0.056972 .  
model_13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xDoctor +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium + 
                 JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_13)
vif<-vif(model_13)
vif[order(vif,decreasing = TRUE)]

#Remove JobInvolvement.xMedium              0.25492    0.13580   1.877 0.060499 .  
model_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xDoctor +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + 
                 JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_14)
vif<-vif(model_14)
vif[order(vif,decreasing = TRUE)]

#Remove Education.xDoctor                  -0.69178    0.34253  -2.020 0.043419 *  
model_15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + 
                 JobInvolvement.xVery.High, family = "binomial", data = train)
summary(model_15)
vif<-vif(model_15)
vif[order(vif,decreasing = TRUE)]

#Remove JobInvolvement.xVery.High           0.37269    0.17791   2.095 0.036182 *  
model_16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_16)
vif<-vif(model_16)
vif[order(vif,decreasing = TRUE)]


#Remove MaritalStatus.xMarried              0.35653    0.17467   2.041 0.041232 *  
model_17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_17)
vif<-vif(model_17)
vif[order(vif,decreasing = TRUE)]

#Remove BusinessTravel.xTravel_Rarely       0.57994    0.23244   2.495 0.012595 *  
model_18<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_18)
vif<-vif(model_18)
vif[order(vif,decreasing = TRUE)]

#Remove JobRole.xResearch.Director          0.61490    0.21715   2.832 0.004630 ** 
model_19<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_19)
vif<-vif(model_19)
vif[order(vif,decreasing = TRUE)]

#Remove EnvironmentSatisfaction.xVery.High -0.41649    0.13832  -3.011 0.002602 ** 
model_20<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_20)
vif<-vif(model_20)
vif[order(vif,decreasing = TRUE)]

#Remove JobRole.xManufacturing.Director    -0.68023    0.20700  -3.286 0.001016 ** 
model_21<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_21)
vif<-vif(model_21)
vif[order(vif,decreasing = TRUE)]

#At this point all the 17 attributes have *** 
#Continue to remove the least significant of them all
#Remove WorkLifeBalance.xBest              -0.84004    0.25495  -3.295 0.000984 ***
model_22<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", data = train)
summary(model_22)
vif<-vif(model_22)
vif[order(vif,decreasing = TRUE)]

#Remove WorkLifeBalance.xGood              -0.39137    0.16697  -2.344 0.019078 *  
model_23<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter, family = "binomial", data = train)
summary(model_23)
vif<-vif(model_23)
vif[order(vif,decreasing = TRUE)]

#Remove TrainingTimesLastYear              -0.19311    0.05731  -3.369 0.000754 ***
model_24<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter, family = "binomial", data = train)
summary(model_24)
vif<-vif(model_24)
vif[order(vif,decreasing = TRUE)]

#Remove Age                                -0.30299    0.07961  -3.806 0.000141 ***
model_25<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter, family = "binomial", data = train)
summary(model_25)
vif<-vif(model_25)
vif[order(vif,decreasing = TRUE)]

#Remove JobSatisfaction.xVery.High         -0.56151    0.13867  -4.049 5.14e-05 ***
model_26<- glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + 
                 WorkLifeBalance.xBetter, family = "binomial", data = train)
summary(model_26)
vif<-vif(model_26)
vif[order(vif,decreasing = TRUE)]

#Remove WorkLifeBalance.xBetter            -0.49987    0.11129  -4.492 7.06e-06 ***
model_27<- glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow
                 , family = "binomial", data = train)
summary(model_27)
vif<-vif(model_27)
vif[order(vif,decreasing = TRUE)]

#Remove Department.xResearch...Development -0.99693    0.22867  -4.360 1.30e-05 ***
model_27<- glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 Department.xSales + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow
               , family = "binomial", data = train)
summary(model_27)
vif<-vif(model_27)
vif[order(vif,decreasing = TRUE)]

#Now we have top 10 reasons why people quit in the company

#Remove Department.xSales                 -0.27005    0.12209  -2.212    0.027 *   as it is less significant now
model_28<- glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + deskTime + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow
               , family = "binomial", data = train)
summary(model_28)
vif<-vif(model_28)
vif[order(vif,decreasing = TRUE)]

########################################################################
# With 5 significant variables in the model

final_model<- model_28


#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Accuracy : 0.8621          
#Sensitivity : 0.24762         
#Specificity : 0.98070  

#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#Accuracy : 0.8505        
#Sensitivity : 0.30476       
#Specificity : 0.95588 
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.25)]

# Let's choose a cutoff value of 0.2334 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.2334, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 79.66%

sens #63.33%

spec #82.81%

View(test)

#################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

View(Attrition_decile)
