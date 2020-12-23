#Load appropriate packages needed
library(data.table)
library(rvest)

#set work in the correct working environment
setwd("~/Junior/Spring/380-revisited")
DataDir<- "~/Junior/Spring/380-revisited"

set.seed(460)

#read data
DT = fread("2008.csv")

#subset only the variables we want, as well as getting rid of NA observations
sub_DT = DT[!is.na(DT$DepDelay)][, .(Month, DayOfWeek, CRSDepTime, UniqueCarrier, Origin, DepDelay)]

sub_DT

#%/% is integer division ; %% is modulus => ex) 1955%/%100  => 19 ; 1955%%100=>55
sub_DT[, CRSDepTime := 60 * (CRSDepTime %/% 100) + CRSDepTime %% 100]

sub_DT


# I really want to use aiports are a predictor, but there are 303 aiports.  Even if I limit myself to the 30 top busiest aiports,
# there is just too much memory ussage.  
#What I will do is categorize the aiports as "busy", "moderate" or "low"
#So, I will scrape the 30 busiest airports from wikipedia

url <- "https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_United_States"
BusyUSairports <- url %>%
	read_html() %>%
	html_nodes(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[1]') %>%  #inspect -> choose element -> copy full xPath
	html_table(fill = TRUE)
BusyUSairports <- as.data.table(BusyUSairports[[1]])
BusyUSairports
fwrite(BusyUSairports, file.path(DataDir, "BusyUSairports.csv"))

#top 31-60
ModerateUSairports <- url %>%
	read_html() %>%
	html_nodes(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[2]') %>%
	html_table(fill = TRUE)
ModerateUSairports <- as.data.table(ModerateUSairports[[1]])
ModerateUSairports
fwrite(ModerateUSairports, file.path(DataDir, "ModerateUSairports.csv"))

#
sub_DT[, OriginType := "Not busy"] #create new variable: OriginType
sub_DT[Origin %in% BusyUSairports$IATACode, OriginType := "Busy"] #matching IATAcode for busy airports 
sub_DT[Origin %in% ModerateUSairports$IATACode, OriginType := "Moderate"] #matching IATAcode for moderate airports 
sub_DT[, Origin := NULL] # get rid of the Origin variable from sub_DT

#modify variable types
sub_DT[, Month := as.factor(Month)]
sub_DT[, DayOfWeek := as.factor(DayOfWeek)]
sub_DT[, UniqueCarrier := as.factor(UniqueCarrier)]
sub_DT[, OriginType := as.factor(OriginType)]

unique(sub_DT$UniqueCarrier) #check the unique values of carrier
unique(sub_DT$OriginType) #check the unique values of the origin type variable we created
sub_DT


# here I divide the data into train and test so that I'm working on a similar
# problem as all of you note that you do not need to do this on your dataset
n = nrow(sub_DT)
idx = sample(1:n, n / 2)

#50 train 50 test
train = sub_DT[!idx]
test  = sub_DT[ idx]

saveRDS(train, file = file.path(DataDir, "2008_train.rds"))
saveRDS(test, file = file.path(DataDir, "2008_test.rds"))

#Clean environment as we have saved our train and test and we have other needed resources too.
rm(list = ls())
gc()

#### Models ####

#needed packages
library(caret)
library(data.table)
library(Metrics)

DataDir<- "~/Junior/Spring/380-revisited"

set.seed(460)

train = readRDS(file.path(DataDir, "2008_train.rds"))
test = readRDS(file.path(DataDir, "2008_test.rds"))
test
train
#save the actual values that we want to compare later 
train_y = train$DepDelay
test_y  = test$DepDelay

#create the dummy variable of 
dummies = dummyVars(DepDelay ~ ., data = train) #library(caret)

#predict the dummies we created as the new train and test
train = as.data.table(predict(dummies, newdata = train))
test  = as.data.table(predict(dummies, newdata = test))

#reformat after dummyVars and add back response Var

train[, DepDelay := train_y]

colnames(train)
colnames(test)

#fit a linear model
lm_model <- lm(DepDelay ~ ., data = train)

#assess model
summary(lm_model)

#save model
saveRDS(dummies, file = file.path(DataDir, "dummies.rds"))
saveRDS(lm_model, file = file.path(DataDir, "lm_model.rds"))

#save test results as DepDelay
test$DepDelay = predict(lm_model, newdata = test)

#our file needs to follow the example submission file format. So we need to only have the Id and saleprice column and
#we also need the rows to be in the correct order

submit = test[, .(DepDelay)]

#now we can write out a submission

fwrite(submit, file = file.path(DataDir, "submit_lm.csv"))


##### How did we do? ####

#Train Model compared against average
rmse(train_y, rep(mean(train_y), length(train_y)))

#Train Model compared against fitted model
rmse(train_y, lm_model$fitted.values)

#Test Model compared against average
rmse(test_y, rep(mean(train_y), length(test_y)))

#Test Model compared against fitted model
rmse(test_y, submit$DepDelay)

# We notice how the fitted model does slightly better 
# THe train models do slightly better than the test- is a little disappointing considering they were split half & half