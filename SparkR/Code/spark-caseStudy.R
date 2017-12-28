
#Business requirement -

  #Julian McAuley’s lab at Stanford University, as part of their research, collected 80 million Amazon
  #reviews from 1995 to 2013. This mammoth dataset is divided into different categories.
  #You want to start a new product line to boost your revenue. Being a media company, you want to get into either
  # ebooks, movies, TV shows or similar such product categories in the entertainment industry.
  #Since you are starting a new product line, you want to be sure about the choice of products you’ll buy (and sell).
  #You have three options of product categories to choose from - CDs and Vinyl, Movies and Ebooks (Kindle).

#Expected output -

  #Which product category has a larger market size
  #Which product category is likely to be purchased heavily
  #Which product category is likely to make the customers happy after the purchase

#Assumptions -

  #you can use the ‘number of reviews’ as a proxy for the number of products sold (i.e. the ratio of number of reviews
  #of two product categories will reflect, approximately, the ratio of the number of units sold - you are not trying to
  #estimate the absolute numbers anyway, you want to compare the metrics across categories).

  #Similarly, you can define some metrics to proxy customer satisfaction as well. For instance, if a customer has written a
  #long review and rated the product 5, it indicates that he/she is quite happy with the product.

  #Reviews with less than a threshold total helpfulness votes are filtered out and are not used for analyses.
  # For this analysis, I have taken a threshold as 60% which indicates that the helpful ratio should be 0.60 %

#Steps/approach followed for this case study -

#   1) Load the files into the S3. (avoid using browser loading, it will kill your system as well as network for a month) .
#      Try to download the files into one of the hadoop master node and use aws cli to load the data into the S3.
#   2) load all the three product line json files into the R.
#   3) Find-out total count for all three product lines data after applying threshold filter. This will start indicating the
#      the overall size of product line.
#   4) Findout total unique products in each product line- this will help you to start analyzing product market size.
#   5) Findout how many reviews exist for every product (mean method)
#   6) Findout top50 prodcuts which has maximum number of reviews for all three product lines
#        a) Findout % of total reviews for top 50 products. This will help  us to know what all the top most product line.
#            As this is not related to only one product, I believe this will be a good measure to findout which product line is more
#            famous in market.
#
#    6) Findout number of charaters and group by overall rating
#    7) Number of reviews based on overall rating
#    8) Findout the maket size based on the
#         1) number of reviews based on the unique reviewer. As per one of the given requirement this will be helpful to determine the market size.
#         2) number of reviews (not based on the unique reviewer Id)
#    9) Findout the ration between various unique reviewer count to know the relative market size.
#   10)Findout the mean/average review text size grouped by overall
#   11) Convert the unixTime into the Years, Months and run a query which will give us number of reviews in a given month
#      (basically group by month). This will help us to know if there is a pattern like festive month (like Dec, Nov etc)
#   12) Findout Customer Satisfaction/happiness - Group by all three product line data based on overall, month and review count.. This will help us to mine any pattern which indicates overall customer happiness/satisfaction in any given month

#Final recommendations and results are listed at the bottom of the file.

#######################################################################################################################################################################################################################################################################

devtools::install_github('apache/spark@v2.2.0', subdir='R/pkg',force=TRUE)
install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
install.packages("cowplot")


library("SparkR")
library("aws.s3")
library("sparklyr")
library("dbplyr")

# aws.signature::use_credentials()
# commenting this line as environment varibale SPARK_HOME is not set.

Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAINFCKHF6Z3USEILQ",
           "AWS_SECRET_ACCESS_KEY" =
             "btKfakeaIo4fRV+ZgxGLj87OHXbfPWJ13pjlrvfQ",
           "AWS_DEFAULT_REGION" = "us-west-2")


sparkContext = sparkR.session(master = "local")
sparkContext <- sparkR.init()


#Validate the connection with AWS- Simple Storage service is estabilised and R is able to get the data.
bucketlist()

##########################################################################################################################################################################

# load Data for CD and Vinayl product line from aws - s3
reviewsCDsAndVinyl <- read.df("s3://casestudy-dda1710322//reviews_CDs_and_Vinyl_5.json", source = "json")

#Read Data for KindleStore product line
reviewsKindleStore <- read.df("s3://casestudy-dda1710322//reviews_Kindle_Store_5.json", source = "json")

#Read Data for Movies and TV product line
reviewsMoviesAndTV <- read.df("s3://casestudy-dda1710322//reviews_Movies_and_TV_5.json", source = "json")


####################################################################### FILTER OUT NON HELPFUL REVIEWS ##############################################################################################


# Reviews with less than a threshold total helpfulness votes are filtered out and are not used for analyses.
# Filter out reviews which have less than 10 helpful feedbacks and have more than 60% of them marked as helpful.
# I am consideing 60% is a threshold.


# Initlising it to SPARKSQL  context
sqlContext <- sparkRSQL.init(sc)
createOrReplaceTempView(reviewsCDsAndVinyl, "reviewsCDsAndVinyl")
createOrReplaceTempView(reviewsKindleStore, "reviewsKindleStore")
createOrReplaceTempView(reviewsMoviesAndTV, "reviewsMoviesAndTV")

#########################################################################################################################################################################

reviewsCDsAndVinyl<- SparkR::sql("select * from reviewsCDsAndVinyl
                                 where helpful[1]>=10
                                 and helpful[0]/helpful[1] > 0.6 ")



reviewsMoviesAndTV<- SparkR::sql("select * from reviewsMoviesAndTV
                                 where helpful[1]>=10
                                 and helpful[0]/helpful[1] > 0.6 ")



reviewsKindleStore<- SparkR::sql("select * from reviewsKindleStore
                                 where helpful[1]>=10
                                 and helpful[0]/helpful[1] > 0.6 ")


createOrReplaceTempView(reviewsCDsAndVinyl, "reviewsCDsAndVinyl")
createOrReplaceTempView(reviewsKindleStore, "reviewsKindleStore")
createOrReplaceTempView(reviewsMoviesAndTV, "reviewsMoviesAndTV")


###########################################################################################################################################################################

#Make sure all data frames are SparkData frames
class(reviewsCDsAndVinyl)
class(reviewsKindleStore)
class(reviewsMoviesAndTV)

##########################################################################################################################################################################

# Check the structure of the SparkData Frame
str(reviewsCDsAndVinyl)
str(reviewsKindleStore)
str(reviewsMoviesAndTV)

#Make sure that all the columns are same
colnames(reviewsCDsAndVinyl)
colnames(reviewsKindleStore)
colnames(reviewsMoviesAndTV)

###################################################################################################################################################################################


# Try to get top 5 records to make sure that they are accesible
head(reviewsCDsAndVinyl, 5)
head(reviewsKindleStore, 5)
head(reviewsMoviesAndTV, 5)

#############################################################################################################################################################################

result <- SparkR::sql("select count(*) from reviewsCDsAndVinyl")
head(result)  #Total Number of records 1097592 ~ a million records without filtering
# after filtering 111361


#alternative approach
dim(reviewsCDsAndVinyl) #1097592 without filtering
#111361 after filtering

result <- SparkR::sql("select count(*) from reviewsKindleStore")
head(result) #Total Number of records 982619
# After filtering 18752 (after putting helpful score and text size >10)

#alternative approach
dim(reviewsKindleStore)


result <- SparkR::sql("select count(*) from reviewsMoviesAndTV")
result <-collect(result) # Total Number of records 1697533
# After filtering 131407 (after putting helpful score and text size >10)

#alternative approach
dim(reviewsMoviesAndTV)


################################################################## Unique Product count for all three business line  #######################################################

totalDistinctProductCD <- SparkR::sql("select count(distinct(asin)) from reviewsCDsAndVinyl ")
head(totalDistinctProductCD) # Total UniqueProduct 43144


totalDistinctKindleStore <- SparkR::sql("select count(distinct(asin)) from reviewsKindleStore ")
head(totalDistinctKindleStore) # Total UniqueProduct 9665


totalDistinctMovieTV <- SparkR::sql("select count(distinct(asin)) from reviewsMoviesAndTV ")
head(totalDistinctMovieTV) # Total UniqueProduct 37867

###############################################################################################################################################################################

result <- SparkR::sql("select sum(1) as TotalReviews , asin from reviewsCDsAndVinyl group by asin order by TotalReviews desc")

top50ProductsGettingMaxReviews <- limit(result ,50)
top50ProductsGettingMaxReviews<-collect(top50ProductsGettingMaxReviews)

top50ProductsGettingMaxReviews

top50CDsAndVinyalReviewsCount <-sum(top50ProductsGettingMaxReviews$TotalReviews)
top50ReviewRatioForCDsAndVinyl = top50CDsAndVinyalReviewsCount/nrow(reviewsCDsAndVinyl) * 100   #2.50%

install.packages("ggplot2")
library(ggplot2)

top50ProductsGettingMaxReviewsPlot<-ggplot(top50ProductsGettingMaxReviews,
                                           aes(x = asin, y= TotalReviews)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'CD And Vinyl'", x="unique prodct", y="Number of times reviewed")


#TotalReviews       asin
#1           199 B000F7MG4G
#2           135 B000B8QEZG
#3           127 B00008OWZG
#4           113 B0002GMSC0
#5            97 B00062ZV2E



# Cross checking the above count
result <- SparkR::sql("select count(*)  from reviewsCDsAndVinyl where asin = 'B000F7MG4G'")
head(result)
# 199

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

result <- SparkR::sql("select sum(1) as TotalReviews , asin from reviewsKindleStore group by asin order by TotalReviews desc")

top50KindleStore <- limit(result ,50)
top50KindleStore<-collect(top50KindleStore)


top50KindleStoreTotalReviews <-sum(top50KindleStore$TotalReviews)
top50ReviewRatioForKindleStore = top50KindleStoreTotalReviews/nrow(reviewsKindleStore) * 100
#1.784618

top50KindleStorePlot<-ggplot(top50KindleStore,
                             aes(x = asin, y= TotalReviews)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'Kindle Store'", x="top 50 unique products", y="Number of times reviewed")


#TotalReviews       asin
#1            99 B00IA6QWP8
#2            96 B00KCE1ISM
#3            70 B00EYMXM2I
#4            51 B00J8MO4DA
#5            47 B00GS2W6T2

# Cross checking the above count
result <- SparkR::sql("select count(*)  from reviewsKindleStore where asin = 'B00IA6QWP8'")
head(result)
# 99

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

result <- SparkR::sql("select sum(1) as TotalReviews , asin from reviewsMoviesAndTV group by asin order by TotalReviews desc")
top50MovieTV <- limit(result ,50)
top50MovieTV<-collect(top50MovieTV)

top50MovieAndTVTotalReviews <-sum(top50MovieTV$TotalReviews)
top50ReviewRatioForMovieAndTV = top50MovieAndTVTotalReviews/nrow(reviewsMoviesAndTV) * 100
#3.433335


top50MovieTVPlot<-ggplot(top50MovieTV,
                         aes(x = asin, y= TotalReviews)) +
  geom_line(stat="identity", aes(group = 1)) +
  labs(title="'Movie TV'", x="top 50 unique products", y="Number of times reviewed")

#TotalReviews       asin
#1           293 1417030321
#2           254 B0001VL0K2
#3           234 0793906091
#4           219 B00005JNEI
#5           136 0310263662

# Cross checking the above count
result <- SparkR::sql("select count(*)  from reviewsMoviesAndTV where asin = '1417030321'")
head(result)
# 293


require(cowplot)
theme_set(theme_cowplot(font_size=14)) # reduce default font size
plot_grid(top50ProductsGettingMaxReviewsPlot, top50KindleStorePlot, top50MovieTVPlot,labels = c('A', 'B', 'C'))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
######################                                EDA based on ReviewText Size and number of times product has been reviewed
######################
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

maxNumberOfReviewsTop5MovieAndStoreProduct <- SparkR::sql("select Sum(1) as totalNumberOfTimesReviewd, overall from  reviewsMoviesAndTV
                                                          group by overall order by totalNumberOfTimesReviewd desc")
maxNumberOfReviewsTop5MovieAndStoreProduct <- collect(limit(maxNumberOfReviewsTop5MovieAndStoreProduct,5))

#totalNumberOfTimesReviewd overall
#              70327       5
#              30784       4
#              14037       3
#               9553       1
#               6706       2


maxNumberOfReviewsTop5MovieAndStoreProductPlot<- ggplot(maxNumberOfReviewsTop5MovieAndStoreProduct,
                                                        aes(x = overall, y= totalNumberOfTimesReviewd, fill=overall)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'Movies And TV'", x="Overall rating", y="Number of times reviewed")



maxNumberOfReviewsTop5CDAndVinylProduct <- SparkR::sql("select Sum(1) as totalNumberOfTimesReviewd, overall from  reviewsCDsAndVinyl group by overall order by totalNumberOfTimesReviewd desc")
maxNumberOfReviewsTop5CDAndVinylProduct <- collect(limit(maxNumberOfReviewsTop5CDAndVinylProduct,5))

#totalNumberOfTimes overall
#1              72414       5
#2              22688       4
#3               8512       3
#4               4042       1
#5               3705       2


maxNumberOfReviewsTop5CDAndVinylProductPlot<- ggplot(maxNumberOfReviewsTop5CDAndVinylProduct,
                                                     aes(x = overall, y= totalNumberOfTimesReviewd, fill=overall)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'CD And Vinyl'", x="Overall rating", y="Number of times reviewed")




maxNumberOfReviewsTop5KindleStoreProduct <- SparkR::sql("select Sum(1) as totalNumberOfTimesReviewd, overall from reviewsKindleStore group by overall order by totalNumberOfTimesReviewd desc")
maxNumberOfReviewsTop5KindleStoreProduct <- collect(limit(maxNumberOfReviewsTop5KindleStoreProduct,5))

#totalNumberOfTimes overall
#1              10620       5
#2               2922       4
#3               1887       3
#4               1668       1
#5               1655       2


maxNumberOfReviewsTop5KindleStoreProductPlot <- ggplot(maxNumberOfReviewsTop5KindleStoreProduct,
                                                       aes(x = overall, y= totalNumberOfTimesReviewd, fill=overall)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'Kindle and Store'", x="Overall rating", y="Number of times reviewed")

########################################################################################################################################################################################
############################# Consolidated Graph #########################################

require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
plot_grid(maxNumberOfReviewsTop5MovieAndStoreProductPlot, maxNumberOfReviewsTop5CDAndVinylProductPlot, maxNumberOfReviewsTop5KindleStoreProductPlot,labels = c('A', 'B', 'C'))

#################################################################################### ###################################################################################################################################################

# 1. Which product category has a larger market size
# to estimate the market size, you can use the number of reviewers as a proxy.

totalReviewersCDsAndVinyl <- SparkR::sql("select count(distinct(reviewerID)) as totalReviewerForCD from reviewsCDsAndVinyl")
totalReviewersCDsAndVinyl<-collect(totalReviewersCDsAndVinyl) # 26718

totalReviewersKindleStore <- SparkR::sql("select count(distinct(reviewerID)) as totalReviewerForKindle  from reviewsKindleStore")
totalReviewersKindleStore <-collect(totalReviewersKindleStore) #10739

totalReviewersMovieAndTV <- SparkR::sql("select count(distinct(reviewerID)) as totalReviewerForMovies from reviewsMoviesAndTV")
totalReviewersMovieAndTV<-collect(totalReviewersMovieAndTV) #31978

#Based on the above observations Movies and TV product line has the largest market size with 31978 unique reviewers.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  # 2. Which product category is likely to be purchased heavily (based on ‘number of reviews’ )
  #you can use the ‘number of reviews’ as a proxy for the number of products sold
  #(i.e. the ratio of number of reviews of two product categories will reflect, approximately, the ratio of
  #the number of units sold - you are not trying to estimate the absolute numbers anyway, you want to
  #compare the metrics across categories).
  #Ratio of Movies and TV vs CDs and Vinyl

  nrow(reviewsCDsAndVinyl)/nrow(reviewsKindleStore)   #5.938
  nrow(reviewsMoviesAndTV)/nrow(reviewsKindleStore)   #7.007626
  nrow(reviewsMoviesAndTV)/nrow(reviewsCDsAndVinyl)   #1.18

#From the above it is evident that Movies and TV will be the product category that will be sold heavily

# 2.1  Which product category is likely to be purchased heavily (based on ‘market size’ )
#estimate the market size, you can use the number of reviewers as a proxy

collect(totalReviewersCDsAndVinyl)/collect(totalReviewersKindleStore)  #2.48
collect(totalReviewersMovieAndTV)/collect(totalReviewersKindleStore)   #2.977745
collect(totalReviewersMovieAndTV)/collect(totalReviewersCDsAndVinyl)   # 1.196871

#From the above it is evident that Movies and TV will be the product category that will be sold heavily based on market size.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3. Which product category is likely to make the customers happy after the purchase
#you can define some metrics to proxy customer satisfaction as well. For instance,
#if a customer has written a long review and rated the product 5,
#it indicates that he/she is quite happy with the product.

#testData <- SparkR::sql("select LENGTH('COUNTIUNG EVERY CHARATER ALONG WITH THE SPACE')")
#head(testData) #45

maxNumberOfCharacterTop5CDsAndVinyProduct <- SparkR::sql("select Sum(TotalReviewText) as total, overall from (select LENGTH(reviewText) as TotalReviewText,
                                                         overall from reviewsCDsAndVinyl ) group by overall order by total desc")

maxNumberOfCharatersTop5CDsAndVinyProduct <- limit(maxNumberOfCharacterTop5CDsAndVinyProduct ,50)
maxNumberOfCharatersTop5CDsAndVinyProduct<-collect(maxNumberOfCharatersTop5CDsAndVinyProduct)

maxNumberOfCharatersTop5CDsAndVinyProduct

#total overall
#1 128209501       5
#2  43973632       4
#3  15831747       3
#4   5954653       2
#5   4451059       1



maxNumberOfCharatersTop5CDsAndVinyProductPlot <- ggplot(maxNumberOfCharatersTop5CDsAndVinyProduct,
                                                        aes(x = overall, y= total)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'CDs And Viny'", x="Overall rating", y="total review length")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maxNumberOfCharactersTop5KindleStoreProduct <- SparkR::sql("select Sum(TotalReviewText) as total, overall from (select LENGTH(reviewText) as TotalReviewText,
                                                           overall from reviewsKindleStore ) group by overall order by total desc")

maxNumberOfCharactersTop5KindleStoreProduct <- limit(maxNumberOfCharactersTop5KindleStoreProduct ,5)
maxNumberOfCharactersTop5KindleStoreProduct<-collect(maxNumberOfCharactersTop5KindleStoreProduct)

#total overall
#1 11384785       5
#2  3826224       4
#3  2560815       3
#4  2255946       2
#5  1789562       1


maxNumberOfCharactersTop5KindleStoreProductPlot <- ggplot(maxNumberOfCharactersTop5KindleStoreProduct,
                                                          aes(x = overall, y= total)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'Kindle and Store'", x="Overall rating", y="total review length")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maxNumberOfCharatersTop5MovieAndTVProduct <- SparkR::sql("select Sum(TotalReviewText) as total, overall from (select LENGTH(reviewText) as TotalReviewText,
                                                         overall from reviewsMoviesAndTV ) group by overall order by total desc")
maxNumberOfCharatersTop5MovieAndTVProduct <- limit(maxNumberOfCharatersTop5MovieAndTVProduct ,5)
maxNumberOfCharatersTop5MovieAndTVProduct<-collect(maxNumberOfCharatersTop5MovieAndTVProduct)

#total overall
#137022901       5
#66586380       4
#28639517       3
#12474728       1
#12455690       2


maxNumberOfCharatersTop5MovieAndTVProductPlot <- ggplot(maxNumberOfCharatersTop5MovieAndTVProduct,
                                                        aes(x = overall, y= total)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'Movies and TV'", x="Overall rating", y="total review length")


################################################################## Consolidated Graph - Total Review Length grouped by Overall  ##################################################################################################

require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
plot_grid(maxNumberOfCharatersTop5CDsAndVinyProductPlot, maxNumberOfCharactersTop5KindleStoreProductPlot, maxNumberOfCharatersTop5MovieAndTVProductPlot,labels = c('A', 'B', 'C'))

#######################################################################################################################################################################################################################################


#Of the 3 categories Movies and TV has highest number of customers who are happy after purchasing the product.

#Overall Recommendation
#As a manager of the media company i would be investing in Movies and TV product category per the following findings
#1. it has the largest market size with 123960 unique reviewers compared to 1097592,982619 for CD's and Kindle
#2. it is the highest sold product category with a ratio of 1.54:1 with CDs and Vinyl 1.72:1 with Kindle Store
#3. it has the highest number of satisfied customers 230481 till 2013 compared to 209963, 154738 for CD's and Kindle


############################################################### Review Size and Overall Rating #################################################################################

result  = SparkR::sql("select mean(reviewLength) as totalMeanLength , overall from  (select length(reviewText) as reviewLength, overall from reviewsCDsAndVinyl) group by overall  order by totalMeanLength desc") ;
result <- collect(result)

# totalMeanLength overall
#        1938.189       4
#        1859.933       3
#        1770.507       5
#        1607.194       2
#        1101.202       1

meanCDReviewSize <- ggplot(result, aes(x = overall, y= totalMeanLength)) +
  geom_histogram(stat="identity", aes(group = 1)) +
  labs(title="'CD and Vinyl'", x="Overall rating", y="mesn review length")

meanCDReviewSize


result  = SparkR::sql("select mean(reviewLength) as totalMeanLength , overall from  (select length(reviewText) as reviewLength, overall from
                      reviewsMoviesAndTV) group by overall  order by totalMeanLength desc") ;
result <- collect(result)

#  totalMeanLength    overall
#        2163.019       4
#        2040.288       3
#        1948.368       5
#        1857.395       2
#        1305.844       1

meanMovieAndTVReviewLengthPlot <- ggplot(result, aes(x = overall, y= totalMeanLength)) +
  geom_histogram(stat="identity", aes(group = 1) ) +
  labs(title="'Movie and TV'", x="Overall rating", y="mesn review length")

meanMovieAndTVReviewLengthPlot



result  = SparkR::sql("select mean(reviewLength) as totalMeanLength , overall from  (select length(reviewText) as reviewLength, overall from
                      reviewsKindleStore) group by overall  order by totalMeanLength desc") ;
head(result)

#totalMeanLength overall
#1        1363.109       2
#2        1357.083       3
#3        1309.454       4
#4        1072.879       1
#5        1072.014       5


meanKindleAndTotalReviewLengthPlot <- ggplot(result, aes(x = overall, y= totalMeanLength)) +
  geom_histogram(stat="identity", aes(group = 1) ) +
  labs(title="'Kindle and Store'", x="Overall rating", y="mesn review length")

meanMovieAndTVReviewLengthPlot


require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
plot_grid(meanKindleAndTotalReviewLengthPlot, meanMovieAndTVReviewLengthPlot, meanCDReviewSize,labels = c('A', 'B', 'C'))



###############################################################################################################################################################################
                                                                                        # EDA based on Unix DATE TIME
###############################################################################################################################################################################

str(reviewsCDsAndVinyl)
output <- SparkR::sql("select count(reviewText) as count, month(from_unixtime(unixReviewTime)) as month  from reviewsCDsAndVinyl group by (month) order by count desc")
output <- collect(output)

#   count month
#1  10776    11
#2  10171    10
#3   9694     6
#4   9504     5
#5   9385     3
#6   9359     9
#7   8996    12
#8   8945     4
#9   8804     7
#10  8725     1
#11  8566     8
#12  8436     2

reviewCountGroupedByMonth <- ggplot(output, aes(x = month, y= count)) +
  geom_histogram(stat="identity", aes(group = 1) ) +
  labs(title="'review Count GroupedByMonth CD and Vinyl'", x="month", y="review count")


output <- SparkR::sql("select count(reviewText) as count, month(from_unixtime(unixReviewTime)) as month  from reviewsKindleStore group by (month) order by count desc")
output <- collect(output)

#   count month
#1   1830     3
#2   1802     5
#3   1661     1
#4   1649     2
#5   1584     4
#6   1579     6
#7   1532    12
#8   1455     9
#9   1440     8
#10  1434    10
#11  1417     7
#12  1369    11

reviewCountGroupedByMonthKindleStore <- ggplot(output, aes(x = month, y= count)) +
  geom_histogram(stat="identity", aes(group = 1) ) +
  labs(title="'review Count GroupedByMonth kindle and Store'", x="month", y="review count")



output <- SparkR::sql("select count(reviewText) as count, month(from_unixtime(unixReviewTime)) as month  from reviewsMoviesAndTV group by (month) order by count desc")
output <- collect(output)

#count month
#1  11966     1
#2  11415     3
#3  11215    12
#4  11121    10
#5  10928    11
#6  10885     4
#7  10836     8
#8  10797     7
#9  10703     5
#10 10601     6
#11 10500     2
#12 10440     9

reviewCountGroupedByMonthMovieAndTV <- ggplot(output, aes(x = month, y= count)) +
  geom_histogram(stat="identity", aes(group = 1) ) +
  labs(title="'review Count GroupedByMonth Movie and TV'", x="month", y="review count")

################################################################################ Consolidated Graph based on the month #################################################################

require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
plot_grid(reviewCountGroupedByMonth, reviewCountGroupedByMonthKindleStore, reviewCountGroupedByMonthMovieAndTV,labels = c('A', 'B', 'C'))

##############################################################################################################################################################################
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)

output <- SparkR::sql("select *, month(from_unixtime(unixReviewTime)) as month  from reviewsCDsAndVinyl")
dfGroupByOverallAndMonth <- groupBy(output, "overall", "month")
finaloutput <-collect(SparkR::summarize(dfGroupByOverallAndMonth, count = n(output$reviewText)))
plyr::arrange(finaloutput, desc(count), overall)
dfGroupByOverallAndMonthPlotCD <- ggplot(finaloutput, aes(x=count,y=overall, color=overall)) + geom_line()



output <- SparkR::sql("select *, month(from_unixtime(unixReviewTime)) as month  from reviewsKindleStore")
dfGroupByOverallAndMonth <- groupBy(output, "overall", "month")
finaloutput <-collect(SparkR::summarize(dfGroupByOverallAndMonth, count = n(output$reviewText)))
plyr::arrange(finaloutput, desc(count), overall)
dfGroupByOverallAndMonthPlotKindle <- ggplot(finaloutput, aes(x=count,y=overall, color=overall)) + geom_line()

reviewsMoviesAndTV

output <- SparkR::sql("select *, month(from_unixtime(unixReviewTime)) as month  from reviewsMoviesAndTV")
dfGroupByOverallAndMonth <- groupBy(output, "overall", "month")
finaloutput <-collect(SparkR::summarize(dfGroupByOverallAndMonth, count = n(output$reviewText)))
plyr::arrange(finaloutput, desc(count), overall)
dfGroupByOverallAndMonthPlotMovie <-ggplot(finaloutput, aes(x=count,y=overall, color=month)) + geom_line()

require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
plot_grid(dfGroupByOverallAndMonthPlotCD, dfGroupByOverallAndMonthPlotKindle, dfGroupByOverallAndMonthPlotMovie,labels = c('A', 'B', 'C'))

########################################################################################################################################################################
##                                                                                 RESULTS AND RECOMMENDATIONS
###########################################################################################################################################################################

# 1) Which product category has a larger market size
#    Movie and TV is having the highest market size. This is based on the two Assumptions -
#      1)  you can use the ‘number of reviews’ as a proxy for the number of products sold (i.e. the ratio of number of reviews of two product categories will reflect, approximately, the ratio of the number of units sold - you are not trying to estimate the absolute numbers anyway, you want to compare the metrics across categories).
#      2) Total number of unique products in Market for every product line.

# Quick snapshot of the analysis -
#    1) unique reviewer -
#            Movie and TV  -  31978
#            CD and Vinyl  -  26718
#            Kindle and Store  - 10739
#    2) unique number of products
#            Movie and TV -   37867
#            CD and Vinayl -  43144
#            Kindle and Store - 9665
#    3) Total review length for all theee product lines -
#            This also indicates that Movie and TV has lengthy reviews and as per our assumption lengthy reviews are
#            sign of customer satisfaction.

# CONCLUSION - This indicates that Movie and TV has higher number of reviews but lesser number of unique products.
# This will make our decision easy as company don't have to invert too much to the variety of products but will be able to sell more items.


# 2) Which product category is likely to be purchased heavily
#      You can use the ‘number of reviews’ as a proxy for the number of products sold
#      (i.e. the ratio of number of reviews of two product categories will reflect, approximately, the ratio of
#      the number of units sold - you are not trying to estimate the absolute numbers anyway, you want to
#      compare the metrics across categories).
#         1) "Number of reviews" ratio-
#              nrow(reviewsCDsAndVinyl)/nrow(reviewsKindleStore)   #5.938
#              nrow(reviewsMoviesAndTV)/nrow(reviewsKindleStore)   #7.007626
#              nrow(reviewsMoviesAndTV)/nrow(reviewsCDsAndVinyl)   #1.18

#          2) ‘Market size’ ratio - #estimate the market size, you can use the number of reviewers as a proxy
#               collect(totalReviewersCDsAndVinyl)/collect(totalReviewersKindleStore)  #2.48
#               collect(totalReviewersMovieAndTV)/collect(totalReviewersKindleStore)   #2.977745
#               collect(totalReviewersMovieAndTV)/collect(totalReviewersCDsAndVinyl)   # 1.196871
# CONCLUSION - From the above it is evident that Movies and TV will be the product category that will be sold heavily based on the number of reviews or based on the market size.


# 3) Which product category is likely to make the customers happy after the purchase
#       assumption - if a customer has written a long review and rated the product 5, it indicates that he/she is quite happy with the product.
#
#         1) Count the length of the review and group it by the overall for every product line -
#             a) Movie and TV
#                 total        overall
#                 137022901      5
#                 66586380       4
#                 28639517       3
#                 12474728       1
#                 12455690       2
#             b) CD and Vinyal
#                 total        overall
#                 128209501       5
#                 43973632        4
#                 15831747        3
#                 5954653         2
#                 4451059         1
#            c) Kindle and Store
#                 total        overall
#                 11384785      5
#                 3826224       4
#                 2560815       3
#                 2255946       2
#                 1789562       1
#
#CONCLUSION - Its very much evident that Movie and TV product line customers are writing lengthy reviews and giving overall rating 5. Since Movie and TV has highest happy and satisfied customer I will suggest to invest into Movie and TV product line.
#OVERALL - 'MOVIE AND TV' IS PERFORMING MUCH BETTER AND HENCE I WILL SUGGEST TO INVEST INTO THIS PRODUCT LINE.

##### NOTE - I have perform EDA (univariate and bivariate analysis) on many other factors. All of them are indicating that Movie and TV product line is best product line for any investment. for details please look above code and generated graphs.
