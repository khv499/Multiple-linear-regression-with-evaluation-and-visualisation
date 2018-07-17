# We will be predicting price based on car specifications.


### Define a working directory
# Locate your project directory and use the forward / slash
setwd("C:/Users/Harshavardhan Koneru/Documents/Multiple Regression")
getwd()

##### Read and clean the data and select variables

install.packages("Hmisc", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
library(Hmisc)
library(psych)
library(car)

# Read the Automobile.csv data set and eliminate missing values
auto <- read.csv(file = "Data/automobile.csv", header=TRUE, na.strings="?")
summary(auto)
auto$price <- as.numeric(impute(auto$price, mean))
auto$normalized.losses <- as.numeric(impute(auto$normalized.losses, mean))
auto$num.of.doors <- as.numeric(impute(auto$num.of.doors, median))
auto$horsepower <- as.numeric(impute(auto$horsepower, mean))
auto$peak.rpm <- as.numeric(impute(auto$peak.rpm, mean))
auto$bore <- as.numeric(impute(auto$bore, mean))
auto$stroke <- as.numeric(impute(auto$stroke, mean))
summary(auto)

# Select a subset of numeric variables for regression modelling
auto.sel <- subset(auto, select = c(horsepower, city.mpg, peak.rpm, curb.weight, num.of.doors, price))


##### Analyse variables for
#     - Normality of distribution
#     - Multiple collinearity
#     - Extreme values 
#     - Homoscedasticity (even distribution of residuals)
##### All such problems should have been fixed here

# Here we'll only make a brief visual inspection of vars
pairs.panels(auto.sel, col="red")


##### Develop a linear model
#     The model will be built using the training sample of the data
#     The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]


### Multiple regression model utilises a simple formula:
#    price = B0 + B1 x horsepower + B2 x curb.weight + B3 x city.mpg
#
# We will perform additional tests on the trainig data

# We will use a stepwise selection of variables by backwards elimination
# We will consider all candidate variables and eliminate one at the time

fit <- lm(price ~ horsepower+city.mpg+peak.rpm+curb.weight+num.of.doors, data=train.sample)
summary(fit) # R2=73%

fit <- lm(price ~ horsepower+city.mpg+peak.rpm+curb.weight, data=train.sample)
summary(fit) # R2=73%

fit <- lm(price ~ horsepower+city.mpg+curb.weight, data=train.sample)
summary(fit) # R2=73%

fit <- lm(price ~ horsepower+curb.weight, data=train.sample)
summary(fit) # R2=72.7%
plot(fit)

# Observe that R-Sq almost did not change and all Ps are good

# Note however that we found some extreme values, which could be removed, here they are
train.sample[which(rownames(train.sample) %in% c("130", "128", "127")),]

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.price <- predict(fit, 
                                   newdata = subset(train.sample, select=c(price, horsepower, curb.weight)))
valid.sample$Pred.price <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(price, horsepower, curb.weight)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.price, train.sample$price), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.price - train.sample$price)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.price - train.sample$price)))
c(train.corr^2, train.RMSE, train.MAE)
# 0.7225 4175.0000 2746.0000

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.price, valid.sample$price), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.price - valid.sample$price)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.price - valid.sample$price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.7569 4101.0000 2848.0000

# This results could be improved when eliminating extreme values and normalising vars

### Check for non-linearity (visually) and transform vars
pairs.panels(auto.sel, col="red")
auto.sel$price <- log10(auto.sel$price)
auto.sel$horsepower <- log10(auto.sel$horsepower)
pairs.panels(auto.sel, col="red")

set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]

### Fit the model (1)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=80%, F=158
# plot(fit)

# First check for non-linearity properly (from "car"), if good go further
# This can only be done after the model was created
crPlots(fit)

# Eliminate extreme values
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("46", "128", "130")),]     

### Refit the model (2)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=83.5%, F=157.8

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("45", "127", "156")),]     

### Refit the model (3)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=85.7%, F=182.4

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("17", "14", "13")),]     

### Refit the model (4)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=86.58%, F=192.2

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!

train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("67", "75", "110")),] 
### Refit the model (5)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=87.51%, F=204.5

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!

train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("15", "31", "111")),] 
### Refit the model (6)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=88.31%, F=216


# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)

### Refit the model (5) - drop horsepower due to multiple collinearity
fit <- lm(price ~ curb.weight+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=87.9% but tough it was inflated, F=351.3
vif(fit)

### Refit the model (6) - drop num.of.doors due to p-value
fit <- lm(price ~ curb.weight+peak.rpm, data=train.sample)
summary(fit) # R2=87.9%, F=530.1
vif(fit)

### VIF, F-ratio and p-values say it is good, so no need to do anything else

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.price <- predict(fit, 
                                   newdata = subset(train.sample, select=c(price, peak.rpm, curb.weight)))
valid.sample$Pred.price <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(price, peak.rpm, curb.weight)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.price, train.sample$price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.price - 10 ^ train.sample$price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.price - 10 ^ train.sample$price)))
c(train.corr^2, train.RMSE, train.MAE)
# With all prep is: 0.8836 2519.0000 1711.0000 / As above
# Do nothing was:   0.7225 4175.0000 2746.0000 / See previous value

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.price, valid.sample$price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.price - 10 ^ valid.sample$price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.price - 10 ^ valid.sample$price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# With all prep is: 0.8281 4205.0000 2107.0000 / As above
# Do nothing was:   0.7569 4101.0000 2848.0000 / See previous value

# Small data set - Cross-validation should be used, but vars selection needs to be auto!
# These results and the model should now be interpreted

install.packages("ggplot2", dependencies = TRUE)
install.packages("rgl", dependencies = TRUE)

library(ggplot2)
library(rgl)

##### Now explore the model and results (we will be using auto.sel)

auto.explore <- subset(auto.sel, select=c(peak.rpm, curb.weight, price))
pairs.panels(auto.explore, col="red")

#### Let us see if peak.rpm could help curb.weight to boost prices and how

# Define level of price reliance on curb.weight 
mstats <- summary(auto.explore$peak.rpm)
mstats
PrpmMin = as.numeric(mstats[1]) # Just want numbers not data frames
Prpm1Q = as.numeric(mstats[2])
PrpmMedian = as.numeric(mstats[3])
Prpm3Q = as.numeric(mstats[5])
PrpmMax = as.numeric(mstats[6])

# Generate some x-axis points in Horsepower range
cwstats <- summary(auto.explore$curb.weight)
cwstats
CWRange = seq(from = 1400, to = 4100, by = 100)

# let us try two different linear models
fit <- lm(price ~ curb.weight+peak.rpm, data=train.sample)
fit <- loess(price ~ curb.weight+peak.rpm, data=train.sample)

# Predict prices at different curb.weight levels
PredpriceMin = predict(fit, newdata=data.frame(peak.rpm=PrpmMin, curb.weight=CWRange))
Predprice1Q = predict(fit, newdata=data.frame(peak.rpm=Prpm1Q, curb.weight=CWRange))
PredpriceMedian = predict(fit, newdata=data.frame(peak.rpm=PrpmMedian, curb.weight=CWRange))
Predprice3Q = predict(fit, newdata=data.frame(peak.rpm=Prpm3Q, curb.weight=CWRange))
PredpriceMax = predict(fit, newdata=data.frame(peak.rpm=PrpmMax, curb.weight=CWRange))

# Plot it in transformed price units (recall log10 transform)
ggplot(auto.explore, aes(x=curb.weight, y=price)) + 
  ggtitle("Influence of Curb Weight and Peak RPM on price\n(Multiple regression)") +
  labs(x="Curb Weight + Peak RPM Levels (lb)", y="price (log10 $)") +
  xlim(min(CWRange), max(CWRange)) +
  geom_point(color="gray", size=1, pch=19) +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredpriceMin), color="blue") +
  geom_line(data=data.frame(curb.weight=CWRange, price=Predprice1Q), color="green") +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredpriceMedian), color="yellow") +
  geom_line(data=data.frame(curb.weight=CWRange, price=Predprice3Q), color="darkorange") +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredpriceMax), color="red")

# Plot it in $ price units - does curb.weight really matter?
ggplot(data.frame(curb.weight=auto.explore$curb.weight, price=10 ^ auto.explore$price), 
       aes(x=curb.weight, y=price)) + 
  ggtitle("Influence of Curb Weight and Peak RPM on price\n(Multiple regression)") +
  labs(x="Curb Weight + Peak RPM Levels (lb)", y="price ($)") +
  xlim(min(CWRange), max(CWRange)) +
  geom_point(color="gray", size=1, pch=19) +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredpriceMin), color="blue") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ Predprice1Q), color="green") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredpriceMedian), color="yellow") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ Predprice3Q), color="darkorange") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredpriceMax), color="red")

# Consider glm and gls, especially for logistic regression

##### Coplots
#     As we can see a single 2D plot is hampering our ability to see the details
#     Plot with curb.weight variable as conditioning var
#     We plot with curb.weight in different ranges and multiple plots

# Add regression lines to the plot
panel.lm = function(x, y, ...) {
  tmp <- lm(y ~ x, na.action = na.omit)
  if(length(x)>4) {
    points(x, y, ...)
    abline(tmp, col="blue")
    panel.smooth(x, y, col.smooth="red", ...)
  } else {
    points(x, y, ...)
  }
}

# Plot with panels featuring regression and loess lines
coplot(price ~ curb.weight | peak.rpm, data=auto.explore, 
       col="orange", panel=panel.lm)


##### Let's try some really cool multiple regression 3D plots

### Let us check the impact of variable selection on the model
scatter3d(x=auto$curb.weight, z=auto$peak.rpm, y=auto$price)
scatter3d(x=auto$Horsepower, z=auto$City.mpg, y=auto$price)
scatter3d(x=auto$Horsepower, z=auto$curb.weight, y=auto$Normalized.losses)

### Compare the effect of data cleanup on the model, use only training data

# Raw data but without missing values
scatter3d(x=auto[train.index,]$curb.weight, 
          z=auto[train.index,]$peak.rpm, 
          y=auto[train.index,]$price)

# Data with transformed variables
scatter3d(x=auto.sel[train.index,]$curb.weight, 
          z=auto.sel[train.index,]$peak.rpm, 
          y=auto.sel[train.index,]$price)

# Data without extreme values
scatter3d(x=train.sample$curb.weight, 
          z=train.sample$peak.rpm, 
          y=train.sample$price)


### Can we differentiate regression depending on the price bracket, before data prep and after
#   Note that variables which were log transformed need to be delogged

# Define price breaks, row and cleaned up
price.class <- ifelse(auto$price < 10000, "Low", ifelse(auto$price < 30000, "Medium", "High"))
price.train.class <- ifelse(10^train.sample$price < 10000, "Low", ifelse(10^train.sample$price < 30000, "Medium", "High"))

# Show multiple linear regression
scatter3d(price ~ peak.rpm * curb.weight, data=auto, groups=factor(price.class))
scatter3d(10^price ~ peak.rpm * curb.weight, data=train.sample, groups=factor(price.train.class))

# Show Loess plane fitting
scatter3d(price ~ peak.rpm * curb.weight, data=auto, groups=factor(price.class), fit="smooth")
scatter3d(10^price ~ peak.rpm * curb.weight, data=train.sample, groups=factor(price.train.class), fit="smooth")

# Show grouping and orientation of price groups
scatter3d(price ~ peak.rpm * curb.weight, data=auto, groups=factor(price.class), fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)
scatter3d(10^price ~ peak.rpm * curb.weight, data=train.sample, groups=factor(price.train.class), fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)

# Save the last plot to a JPG file
# rgl.snapshot(filename = "last-plot.jpg")

