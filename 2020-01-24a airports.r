library(data.table)
library(Metrics)
set.seed(6022) # make sure everyone gets the same random results
# Put your working direcotry here, or do not execute
setwd("~/Junior/Spring/Stat 380/january february codes 2020")

#download.file(url="https://s3.amazonaws.com/stat.184.data/Flights/2008.csv",destfile='./Junior/Spring/Stat 380/january february codes 2020/2008.csv', method='curl')
#download.file(url="https://s3.amazonaws.com/stat.184.data/Flights/airports.csv",destfile='./Junior/Spring/Stat 380/january february codes 2020/airports.csv', method='curl')

flights = fread("2008.csv")

#Let's inspect what is going on.

flights

# What questions may we have?  
# Maybe, predict the departure delay.
# What columns may be useful?  Which ones not?

# I'm thinking Month, DayofMonth, DayOfWeek, CRSDepTime (DepTime is actual
# departure time), UniqueCarrier, FlightNum, TailNum, Origin, Dest, DepDelay
# might be useful.  The others, not so much (Given that we don't know linear
# fitting yet...)  Below subsets for only the columns we want to use.

flights = flights[, .(DepDelay, Month, DayofMonth, DayOfWeek, CRSDepTime, UniqueCarrier, FlightNum, TailNum, Origin, Dest)]
flights

flights[, mean(DepDelay)]  # Doesn't work.  Why?

sum(is.na(flights$DepDelay))  # Many of them are NA

flights = flights[!is.na(DepDelay)] # get rid of the NA's

flights[, mean(DepDelay)]

summary(flights$DepDelay)  #-534 minutes early?  That could be a typo.  -534 = 8:54 EALRY!  It may have been -53(4)  I'm going to remove it because I don't trust it.  In general, this is BAD practice.  Always check with someone who knows.

flights[DepDelay < -60]  # Yup, some flights can be up to 91 minutes early, but -534 looks just too suspicious.

flights = flights[DepDelay > -100] # I'm getting rid of that one flight.  

flights[DepDelay > 1500] # seems reasonable  2400 minutes = 40 hours = 1 day 16 hours.  It could happen...
                         # A lot of them seem to be flying into MSP (Minniapolis-St. Paul)  Who knows.

# Divide data into training and testing sets

idx = sample(1:nrow(flights), 5000000) #5000000 randomized numbers anywhere from 1 to nrow(flights)

train = flights[idx]
test  = flights[-idx]
train[, Id := 1:nrow(train)] #gives Id at end
test[, Id := 1:nrow(test)] #gives Id at end

# Just checking that the dimensions work with how we split it up: train+test adds up to flights 
nrow(flights); nrow(train); nrow(test)

# idea 1, just predict average lateness
train[, avg.delay := mean(DepDelay)]
rmse(train$DepDelay, train$avg.delay)

test[, avg.delay := train$avg.delay[1]]
rmse(test$DepDelay, test$avg.delay)

# Note that the MSE are about the same, this is to be expected

# idea2, predict average for each Origin

train[, origin.delay := mean(DepDelay), Origin] #Depdelay based on where origin is

rmse(train$DepDelay, train$origin.delay) # 35.15.  Only slighty better, so let's keep looking


# idea 3, predict average for each Month

train[, month.delay := mean(DepDelay), Month] #Depdelay based on when month is 

rmse(train$DepDelay, train$month.delay) # 35.07.  Better Still.


# idea 3, predict average for each month and origin combination.
# Hawaii and Florida may not have winter delays, but Chicago and Anchorage will.

train[, combo.delay := mean(DepDelay), .(Month, Origin)]
rmse(train$DepDelay, train$combo.delay) # 34.80.  I'm going to stop here and say this is as good as I care to get


# now to make predictions on the test data.
# We subset to get the only the columns we need and unique makes sure that each row
# is unique so as not to confuse R (no repeated values)

pred = unique(train[, .(combo.delay, Month, Origin)])
setkey(pred, Month, Origin) #creates pred key of variables we used but not combo as its literally a combo
setkey(test, Month, Origin)

# I'm going to keep only the columns I want while I'm at it.
merged = merge(test, pred)[, .(Id, combo.delay)] 
merged

nrow(merged); nrow(test)

# Houston, we have a problem.  It appears that the merged table is missing some rows
# shorter than the test data table. This is a problem.  It means that there is a
# month origin combo in the test data that does not appear in the training data.
# Generally, we must find another model, but I'm going to be a bit more sneaky.
# I'm going to find which are missing and then add them and set the predicted
# delay to be the average delay.  I could do the month delay or the airport
# delay, but I'm feeling to lazy to do that.

missingId = setdiff(test$Id, merged$Id) #finding which obs were missing in merged compared to test
missingDT = data.table(Id = missingId, combo.delay = train$avg.delay[1]) # This is a data table of the missing values
#called it combo.delay but is avg.delay from train

merged = rbind(merged, missingDT) #now merged and test have same rows. merged previously comes from line 96
setkey(merged, Id)

# Now, merged is our data set that we would like to submitted to kaggle.
# the only problem is that maybe the column name "combo.delay" should be "DepDelay"

colnames(merged)[2] = "DepDelay"

merged

# save it

fwrite(merged, file = "merged.csv")

# an upload it to Kaggle.