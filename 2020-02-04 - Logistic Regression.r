#load in packages
library(data.table)
setwd("~/Junior/Spring/380-revisited")
load("stock market.rdata")
#take a peak
summary(market)
head(market)

# Today = percentage return for today
# Lagn = percentage return for n-days ago
# Volume = number of shares traded
# Direction = Down: return negative, Up: return positive

cor(market) # problem

cor(market[, -9]) # remove the problem.  Very low correlation between volume and year.

fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = market, family = binomial)

summary(fit)

# Find out if Down or Up is a success
contrasts(market$Direction)

# Two ways to get the coefficients (coefficients of the fit model)
fit$coefficients
coef(fit)

# Two ways to get at the predicted probability (noted down as 0 and up as 1 in 'contrasts')
fit$fitted.values[1:10]
predict(fit, type = "response")[1:10]

probs = fit$fitted.values

# Make predictions if goes up or down
pred = rep("Down", length(probs))#assigning 'down' to amount of 'probs' defined above to a new variable 'pred' 
pred[probs > 0.5] = "Up" #we replace the initial 'down' that had probs >0.5 as 'up' 

# Make a confusion Matrix (actual value is y axis, predicted value is x axis)

table(pred, market$Direction)

#How often are we correct?
(145 + 507) / length(probs)

# Misleading because we are testing on the trained data.  Artificially high
train = market[Year < 2005]
test  = market[Year == 2005]

#new fit to train set we just created
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = train, family = binomial)
summary(fit)

#same method as before
probs = predict(fit, test, type = "response") 
pred  = rep("Down", length(probs))
pred[probs > 0.5] = "Up"
table(pred, test$Direction)

# How well did we do? (both ways work)

(77 + 44) / length(pred)
mean(pred == test$Direction)

#Not very good! IN fact, worse than before ( and this makes sense as smaller train)

#fit by stepwise of the fit glm from before
fit2 = step(fit)
summary(fit)
summary(fit2)
fit2$fitted.values
#So, we just guess "Up" about 50.8% of the time.

mean(train$Direction == "Up")
#just really not so successful overall

### A different Data Set ###

load("default.rdata")
default

library(lattice)
xyplot(income ~ balance, groups = default, data = default, col = c("light blue", "red"), pch = 20)
xyplot(income ~ balance, groups = student, data = default, col = c("light blue", "red"), pch = 20)
default[, student_default := paste0(student, "-", default)]
xyplot(income ~ balance, groups = student_default, data = default, col = c("light blue", "blue", "pink", "red"), pch = 20)

default[, student_default := NULL]

fit = glm(default ~ ., data = default, family = binomial)
summary(fit)

fit2 = step(fit)
summary(fit2)

rep(c("Yes", "No"), 3)
rep(c(1000, 1500, 2000), each = 2)

new_data = data.table(student = rep(c("Yes", "No"), 3), balance = rep(c(1000, 1500, 2000), each = 2))
new_data
new_data[, prob := predict(fit2, new_data, type = "response")]
new_data
