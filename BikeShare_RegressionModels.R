library(stringr) # str_split_fixed()


HealthyRideRentals.2015.Q3 <- read.csv("HealthyRideRentals 2015 Q3.csv", stringsAsFactors = FALSE)
HealthyRideRentals.2015.Q4 <- read.csv("HealthyRideRentals 2015 Q4.csv", stringsAsFactors = FALSE)

#' ------------------------------------------------
#'                  Preprocessing - 1
#' ------------------------------------------------
# combine two dataframe into one
rental_total <- rbind(HealthyRideRentals.2015.Q3,HealthyRideRentals.2015.Q4)

# delete StationName attributes, only using StationID to do prediction
rental_total <- rental_total[, c(1,2,3,4,5,6,8,10)]

# delete rows with missing values
rental_total$UserType[which(rental_total$UserType == "")] <- NA
rental_total <- rental_total[complete.cases(rental_total), ]
str(rental_total)
rental_total$StartTime <- as.character(rental_total$StartTime)
rental_total$StopTime <- as.character(rental_total$StopTime)
rental_total$UserType <- as.character(rental_total$UserType)

# Split StartTime and StopTime into Day, Month, Year, Hour, and Minute 
##' StartTime
tag <- as.data.frame(str_split_fixed(rental_total$StartTime, "/", 3))
rental_total$StartMonth <- tag$V1
rental_total$StartDay <- tag$V2
tag2 <- as.data.frame(str_split_fixed(tag$V3, " ", 2))
rental_total$StartYear <- tag2$V1
tag3 <- as.data.frame(str_split_fixed(tag2$V2, ":", 2))
rental_total$StartHour <- tag3$V1
rental_total$StartMinute <- tag3$V2
rental_total <- rental_total[, - which(colnames(rental_total) == 'StartTime')]

##' StopTime
tag.stop <- as.data.frame(str_split_fixed(rental_total$StopTime, "/", 3))
rental_total$StopMonth <- tag.stop$V1
rental_total$StopDay <- tag.stop$V2
tag.stop2 <- as.data.frame(str_split_fixed(tag.stop$V3, " ", 2))
rental_total$StopYear <- tag.stop2$V1
tag.stop3 <- as.data.frame(str_split_fixed(tag.stop2$V2, ":", 2))
rental_total$StopHour <- tag.stop3$V1
rental_total$StopMinute <- tag.stop3$V2
rental_total <- rental_total[, - which(colnames(rental_total) == 'StopTime')]

# convert training$UserType from factor into numeric
rental_total$UserType <- as.factor(rental_total$UserType)
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}
rental_total$UserType <- as.numeric.factor(rental_total$UserType)

# Split into training and testing set
rental_total$StartDay <- as.numeric(levels(rental_total$StartDay))[rental_total$StartDay]
rental_total$StartMonth <- as.numeric(levels(rental_total$StartMonth))[rental_total$StartMonth]
rental_total$StartYear <- as.numeric(levels(rental_total$StartYear))[rental_total$StartYear]
rental_total$StartHour <- as.numeric(levels(rental_total$StartHour))[rental_total$StartHour]
rental_total$StartMinute <- as.numeric(levels(rental_total$StartMinute))[rental_total$StartMinute]

rental_total$StopDay <- as.numeric(levels(rental_total$StopDay))[rental_total$StopDay]
rental_total$StopMonth <- as.numeric(levels(rental_total$StopMonth))[rental_total$StopMonth]
rental_total$StopYear <- as.numeric(levels(rental_total$StopYear))[rental_total$StopYear]
rental_total$StopHour <- as.numeric(levels(rental_total$StopHour))[rental_total$StopHour]
rental_total$StopMinute <- as.numeric(levels(rental_total$StopMinute))[rental_total$StopMinute]

#' ------------------------------------------------
#'                  Preprocessing - 2
#' ------------------------------------------------
# density plot based on StartHour
d.hour <- density(rental_total$StartHour)
plot(d.hour, main="Pitt Bike Trips by Hour of Day")
polygon(d.hour, col="blue", border="red")
##' according to the "Pitt Bike Trips by Hour of Day" plot, the fastigium of Pitt bike trips is between 8 - 20.

# density plot based on StartDay
d.day <- density(rental_total$StartDay)
plot(d.day, main="Pitt Bike Trips by Day of a Month")
polygon(d.day, col="blue", border="red")
##' According to the "Pitt Bike Trips by Day of a Month" plot, we will not take acount of "Day" element, instead,
##' we will calculate weekdays and weekends from Month and Day, and take weekdays and weekends as variables. 

# density plot based on StartMonth
d.month <- density(rental_total$StartMonth)
plot(d.month, main="Pitt Bike Trips by Month")
polygon(d.month, col="blue", border="red")
##' according to the "Pitt Bike Trips by Month" plot, July and August have the most bike trips,
##'  and the bike trip amount declines month by month till Decemenber. 

# density plot based on UserType
d.type <- density(rental_total$UserType)
plot(d.type, main="Pitt Bike Trips by UserType")
polygon(d.type, col="blue", border="red")
##' usertype 1 is the most, 3 second, 2 the least. 
##' 1 - Member (pay as-you-go customer) 
##' 2 - Subscriber ( deluxe and standard monthly member customer)
##' 3 - Daily (24-hour pass customer)

##' In our final table, the variables will be StationID, UserType, Month, Weekday,
##'  RackQnty, and the "y" will be the bike trip count of a specific station at a day, values with 
##'  "-" means out, "+" means in. So we can compute the final bike amount of a station at one day.
##'  So we can dicide the rebalancing problem based on the amount and the "density plot based on Hour".

stn_data = read.csv("HealthyRideStations2015.csv")

#' ----------------------------------------------------------------------
#' Edit-Start: add a Weekday attribute, delete Day attribute
#' ----------------------------------------------------------------------
library(base)
week.Q3 <- HealthyRideRentals.2015.Q3[,c(1,2,3)]
week.Q4 <- HealthyRideRentals.2015.Q4[,c(1,2,3)]
week.merge <- rbind(week.Q3,week.Q4)
# StartWeekday
tag.week.start <- as.data.frame(str_split_fixed(week.merge$StartTime, " ", 2))
week.merge$Startdate <- gsub("/", "-", tag.week.start$V1)
tag.week.start2 <- as.data.frame(str_split_fixed(week.merge$Startdate, "-", 3))
library(stringr)
tag.week.start2$V1 <- str_pad(tag.week.start2$V1, 2, pad = "0")
week.merge$Startmonth <- tag.week.start2$V1
tag.week.start2$V2 <- str_pad(tag.week.start2$V1, 2, pad = "0")
week.merge$Startday <- tag.week.start2$V2
week.merge$Startyear <- tag.week.start2$V3
week.merge$Startdate <- paste(week.merge$Startmonth, "-", week.merge$Startday, "-", week.merge$Startyear)
week.merge$Startdate <- gsub(" ", "", week.merge$Startdate)
week.merge$Weekday <- weekdays(as.Date(week.merge$Startdate))
# StopWeekday
tag.week.Stop <- as.data.frame(str_split_fixed(week.merge$StopTime, " ", 2))
week.merge$Stopdate <- gsub("/", "-", tag.week.Stop$V1)
tag.week.Stop2 <- as.data.frame(str_split_fixed(week.merge$Stopdate, "-", 3))
library(stringr)
tag.week.Stop2$V1 <- str_pad(tag.week.Stop2$V1, 2, pad = "0")
week.merge$Stopmonth <- tag.week.Stop2$V1
tag.week.Stop2$V2 <- str_pad(tag.week.Stop2$V1, 2, pad = "0")
week.merge$Stopday <- tag.week.Stop2$V2
week.merge$Stopdate <- paste(week.merge$Stopmonth, "-", week.merge$Stopday, "-", week.merge$Startyear)
week.merge$Stopdate <- gsub(" ", "", week.merge$Stopdate)
week.merge$Weekday2 <- weekdays(as.Date(week.merge$Stopdate))
week.merge2 <- week.merge[, c(1,8,12)]
##' Weekday - StartWeek
##' Weekday2 - StopWeek
# merge week.merge2 and rental_total
rental_total <- merge(rental_total, week.merge2, by.x = "TripId", by.y = "TripId")
#' ----------------------------------------------------------------------
#' Edit-End: add a Weekday attribute, delete Day attribute
#' ----------------------------------------------------------------------


#' Extract Rental Stop Data
rental_total_stop <- rental_total[, c(5,6,12,13,15,18)]
head(rental_total_stop)
#' Merge above with Station Data - Left outer join on "ToStationID"
rentalstop_final <- merge(rental_total_stop, stn_data, by.x = "ToStationId", by.y = "StationNum", all.x = TRUE)
head(rentalstop_final)
rentalstop_final <- rentalstop_final[, c(1,2,3,4,5,6,8)]
rentalstop_final <- rentalstop_final[,c(1,2,3,4,5,7)]
head(rentalstop_final)
#' Create data frame with group and "Count" variable
#rentalstop_counts <- data.frame(table(rentalstop_final$ToStationId,rentalstop_final$UserType, rentalstop_final$StopMonth,rentalstop_final$StopDay,rentalstop_final$StopHour, rentalstop_final$Weekday2 ,rentalstop_final$RackQnty))
#rentalstop_counts <- data.frame(table(rentalstop_final$ToStationId,rentalstop_final$UserType, rentalstop_final$StopMonth,rentalstop_final$StopDay,rentalstop_final$RackQnty))
rentalstop_counts <- data.frame(table(rentalstop_final$ToStationId,rentalstop_final$UserType, rentalstop_final$StopMonth,rentalstop_final$StopDay,rentalstop_final$StopHour,rentalstop_final$RackQnty))
head(rentalstop_counts)
#colnames(rentalstop_counts) <- c("ToStationId","StopUserType","StopMonth","StopDay","StopRackQnty","StopCount")
colnames(rentalstop_counts) <- c("ToStationId","StopUserType","StopMonth","StopDay","StopHour","StopRackQnty","StopCount")
head(rentalstop_counts)

#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstop_counts[which.max(rentalstop_counts$StopCount),]

#' Extract Rental Start Data
rental_total_start <- rental_total[, c(1,2,3,4,6,7,8,10,17)]

#' Merge above with Station Data - Left outer join on "FromStationID"
rentalstart_final <- merge(rental_total_start, stn_data, by.x = "FromStationId", by.y = "StationNum", all.x = TRUE)
rentalstart_final <- rentalstart_final[,c(1,2,3,4,5,6,7,8,11)]

#' Create data frame with group and "Count" variable
#rentalstart_counts <- data.frame(table(rentalstart_final$FromStationId,rentalstart_final$UserType, rentalstart_final$StartMonth,rentalstart_final$StartDay,rentalstart_final$StartHour,rentalstart_final$Weekday,rentalstart_final$RackQnty ))
#rentalstart_counts <- data.frame(table(rentalstart_final$FromStationId,rentalstart_final$UserType, rentalstart_final$StartMonth,rentalstart_final$StartDay,rentalstart_final$RackQnty ))
rentalstart_counts <- data.frame(table(rentalstart_final$FromStationId,rentalstart_final$UserType, rentalstart_final$StartMonth,rentalstart_final$StartDay,rentalstart_final$StartHour,rentalstart_final$RackQnty ))
head(rentalstart_counts)
#colnames(rentalstart_counts) <- c("FromStationId","StartUserType","StartMonth","StartDay","StartRackQnty","StartCount")
colnames(rentalstart_counts) <- c("FromStationId","StartUserType","StartMonth","StartDay","StartHour","StartRackQnty","StartCount")
head(rentalstart_counts)
#' Maximum vehicles returned on a given month, day and hour at a particular station
rentalstart_counts[which.max(rentalstart_counts$StartCount),]

#' merge two count data set into one
start_row_to_keep = which(rentalstart_counts$StartCount > 0) 
rentalstart_counts <- rentalstart_counts[start_row_to_keep,]
head(rentalstart_counts)
stop_row_to_keep = which(rentalstop_counts$StopCount > 0) 
rentalstop_counts <- rentalstop_counts[stop_row_to_keep,]
head(rentalstop_counts)
final_total <- merge(rentalstart_counts,rentalstop_counts, by.x = "FromStationId", by.y = "ToStationId")
str(final_total)
final_total$StartMonth <- as.numeric(final_total$StartMonth)
final_total$StopMonth <- as.numeric(final_total$StopMonth)
## Hour_row_to_keep <- which(final_total$StartHour == final_total$StopHour) 
Month_row_to_keep <- which(final_total$StartMonth == final_total$StopMonth) 
final_total.2 <- final_total[Month_row_to_keep,]
Day_row_to_keep <- which(final_total.2$StartDay == final_total.2$StopDay) 
final_total.2 <- final_total.2[Day_row_to_keep,]
## final_total.2 <- final_total[Hour_row_to_keep,]
head(final_total.2)
#final_total.2 <- final_total.2[,c(1,2,3,4,5,6,7,8,9,12,13,15)] 
final_total.2 <- final_total.2[,c(1,2,3,4,5,6,7,8,11,12,13)] 
head(final_total.2)
#' the count of in and out on the same day. "+":in; "-":out.
final_total.2$Count <- final_total.2$StopCount - final_total.2$StartCount
head(final_total.2)
final_total.2 <- final_total.2[,-c(7,10,11)]
colnames(final_total.2) <- c("StationId","StartUserType","Month","Day","StartHour","RackQnty","StopUsertype","StopHour","Count")
final_total.2$y <- final_total.2$Count
final_total.2$Count <- NULL
final_total.2 <- final_total.2[,c(9,1,2,3,4,5,6,7,8)]
str(final_total.2)
final_total.2_num <- data.frame(lapply(final_total.2, function(x) as.numeric(as.character(x))))


training <- final_total.2_num[which(final_total.2_num$Day < 20),]
testing <- final_total.2_num[-which(final_total.2_num$Day < 20),]

#' Building a random training set of 10k rows
training_fact <- training[,c(1,2,4,5,6,7,9)]
training_fact$StationId <- as.factor(training_fact$StationId)
training_fact$Month <- as.factor(training_fact$Month)
training_fact$Day <- as.factor(training_fact$Day)
training_fact$StartHour <- as.factor(training_fact$StartHour)
training_fact$RackQnty <- as.factor(training_fact$RackQnty)
training_fact$StopHour <- as.factor(training_fact$StopHour)
training_factnew <- rbind(head(training_fact[order(-training_fact$y),],n = 5000),tail(training_fact[order(-training_fact$y),],n = 5000))
set.seed(12345)
gp <- runif(nrow(training_factnew))
#' Both as factors and as numerics
training_factnew <- training_factnew[order(gp),]
training_numnew <- data.frame(lapply(training_factnew, function(x) as.numeric(as.character(x))))

testing_fact <- testing[,c(2,4,5,6,7,9)]
test10000 <- testing_fact[1:10000,]
testing_fact.2 <- data.frame(lapply(testing_fact, function(x) as.factor(x)))
test10000_fact <- testing_fact.2[1:10000,]

############################################################
############## REGRESSION MODELS ###########################
############################################################

## LINEAR REGRESSION
lm1 <- lm(y~StationId+Month+Day+StartHour+StopHour,data = training_numnew)
predict.lm1 <- predict(lm1,newdata = test10000)
lm1_test_df <- data.frame(test10000$StationId,test10000$Month,test10000$Day,test10000$StartHour,test10000$RackQnty,test10000$StopHour,predict.lm1)
lm1_test_df$actuals <- testing$y[1:10000]

actual.lm1 <- lm1_test_df$actuals
results.lm1 <- lm1_test_df$predict.lm1

true_count.lm1 <- 0
for(i in 1:length(actual.lm1))
{
  if((results.lm1[i] >= actual.lm1[i]-2)&&(results.lm1[i]<= actual.lm1[i]+2))
  {
    true_count.lm1 = true_count.lm1 + 1
  }
}
true_count.lm1

## POLY REGRESSION - DEG2
lm_new2 <- lm(y~StationId+Month+Day+StartHour+StopHour+I(Month^2)+I(Day^2)+I(StartHour^2)+I(StopHour^2),data = training_numnew)
pred_lm2_dfnew <- data.frame(training_numnew$y,fitted(lm_new2),resid(lm_new2))
pred_lm2_dfnew_rnd <-round(pred_lm2_dfnew)
predict.lm2 <- predict(lm_new2,newdata = test10000)
lm2_test_df <- data.frame(test10000$StationId,test10000$Month,test10000$Day,test10000$StartHour,test10000$RackQnty,test10000$StopHour,predict.lm2)
lm2_test_df$actuals <- testing$y[1:10000]

actual.lm2 <- lm2_test_df$actuals
results.lm2 <- lm2_test_df$predict.lm2

true_count.lm2 <- 0
for(i in 1:length(actual.lm2))
{
  if((results.lm2[i] >= actual.lm2[i]-2)&&(results.lm2[i]<= actual.lm2[i]+2))
  {
    true_count.lm2 = true_count.lm2 + 1
  }
}
true_count.lm2

##POLY REGRESSION - DEG 3
lm_new2_3 <- lm(y~StationId+Month+Day+StartHour+StopHour+I(Month^2)+I(Day^2)+I(StartHour^2)+I(StopHour^2)+I(Month^3)+I(Day^3)+I(StartHour^3)+I(StopHour^3),data = training_numnew)
predict.lm3 <- predict(lm_new2_3,newdata = test10000)
lm3_test_df <- data.frame(test10000$StationId,test10000$Month,test10000$Day,test10000$StartHour,test10000$RackQnty,test10000$StopHour,predict.lm3)
lm3_test_df$actuals <- testing$y[1:10000]
lm3_test_df <- round(lm3_test_df)

actual.lm3 <- lm3_test_df$actuals
results.lm3 <- lm3_test_df$predict.lm3

true_count.lm3 <- 0
for(i in 1:length(actual.lm3))
{
  if((results.lm3[i] >= actual.lm3[i]-2)&&(results.lm3[i]<= actual.lm3[i]+2))
  {
    true_count.lm3 = true_count.lm3 + 1
  }
}
true_count.lm3

##POLY REGRESSION - DEG 4
lm_new2_4 <- lm(y~StationId+Month+Day+StartHour+StopHour+I(Month^2)+I(Day^2)+I(StartHour^2)+I(StopHour^2)+I(Month^3)+I(Day^3)+I(StartHour^3)+I(StopHour^3)+I(Month^4)+I(Day^4)+I(StartHour^4)+I(StopHour^4),data = training_numnew)
predict.lm4 <- predict(lm_new2_4,newdata = test10000)
lm4_test_df <- data.frame(test10000$StationId,test10000$Month,test10000$Day,test10000$StartHour,test10000$RackQnty,test10000$StopHour,predict.lm4)
lm4_test_df$actuals <- testing$y[1:10000]
lm4_test_df <- round(lm4_test_df)

actual.lm4 <- lm4_test_df$actuals
results.lm4<- lm4_test_df$predict.lm4

true_count.lm4 <- 0
for(i in 1:length(actual.lm4))
{
  if((results.lm4[i] >= actual.lm4[i]-2)&&(results.lm4[i]<= actual.lm4[i]+2))
  {
    true_count.lm4 = true_count.lm4 + 1
  }
}
true_count.lm4

##RIDGE REGRESSION
rlm <- lm.ridge(y~., data = training_numnew, lambda = 0.1)
test10k.df <- data.frame(test10000)
mat_test.rlm <- as.matrix(test10k.df)
result.rlm <- data.frame(numeric())
for(i in 1:10000)
{
  for(j in 1:6)
  {
    sum.coef <- sum.coef + mat_test.rlm[i,j]*coef(rlm)[j]
  }
  result.rlm[i] <- sum.coef
  sum.coef <- 0
}
rlm_test_df <- data.frame(test10000$StationId,test10000$Month,test10000$Day,test10000$StartHour,test10000$RackQnty,test10000$StopHour,result.rlm)
rlm_test_df$actuals <- testing$y[1:10000]
actual.rlm <- rlm_test_df$actuals

true_count.rlm <- 0
for(i in 1:length(actual.rlm))
{
  if((result.rlm[i] >= actual.rlm[i]-2)&&(result.rlm[i]<= actual.rlm[i]+2))
  {
    true_count.rlm = true_count.rlm + 1
  }
}
true_count.rlm

