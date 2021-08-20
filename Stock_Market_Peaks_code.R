# Six Stock Market Sectors
# Market Crash Prediction - Logistic Regression

  # The Data
# Cleaning the Data
# First, let's read each of the CSV files.
xle <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\XLE.csv")
xlf <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\XLF.csv")
xlu <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\XLU.csv")
xlp <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\XLP.csv")
xly <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\XLY.csv")
xlv <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\XLv.csv")
spy <- read.csv("C:\\Users\\G\\Desktop\\MSDS660StatMethExpDesign\\week8\\assignment\\data\\SPY.csv")

# Now let's create the dataframe structure to hold our finalized data.
# Empty df
mydf <- data.frame(matrix(ncol = 9, nrow = 5353))
# Naming columns
colnames(mydf) <- c('Date', 'XLEclose', 'XLFclose', 'XLUclose', 'XLPclose', 'XLYclose', 'XLVclose', 'SPYclose', 'Final3Months')

# Next, let's copy over the date column from one of our data sets.
mydf$Date <- xle$Date

# Remember, we only want the Closing Price from each of the ETF data sets.
mydf$XLEclose <- xle$Close
mydf$XLFclose <- xlf$Close
mydf$XLUclose <- xlu$Close
mydf$XLPclose <- xlp$Close
mydf$XLYclose <- xly$Close
mydf$XLVclose <- xlv$Close
mydf$SPYclose <- spy$Close

# Let's look to make sure we don't have any NULL values in these columns
sapply(mydf, function(x) sum(is.na(x)))

# The Final 3 Months
# Filling column from NULL to 0
mydf$Final3Months[is.na(mydf$Final3Months)] <- 0
sapply(mydf, function(x) sum(is.na(x)))

# Fill the date ranges prior to a stock market crash with 1
# December 23, 1999 - March 24, 2000,  [254 : 317]
# July 12, 2007 - October 11, 2007,  [2150 : 2214]
# November 20, 2019 - February 19, 2020,  [5263 : 5323]
mydf$Final3Months[254:317] <- 1
mydf$Final3Months[2150:2214] <- 1
mydf$Final3Months[5263:5323] <- 1

# A quick check
table(mydf$Final3Months)

# Let's look at our final dataframe!
str(mydf)
head(mydf, 5)
tail(mydf, 5)

# To save a bit of memory, we no longer need our raw data from CSVs.
rm(xle, xlf, xlu, xlp, xly, xlv, spy)

# We need to update the data type for 'Final3Months' to factor 
mydf['Final3Months'] <- lapply(mydf['Final3Months'] , factor)

# Unimportant Visualization
library('ggpubr')
ggboxplot(mydf, x = 'Final3Months', y = 'SPYclose', color = 'Final3Months', palette = c('#Bf360C', '#1565C0'))

  # Logistic Regression Model
# Splitting the data set - Train & Test 
install.packages('caret')
library('caret')
set.seed(99)  # For repeatability
trainDataIndex <- createDataPartition(mydf$Final3Months, p=0.7, list = FALSE)
trainData <- mydf[trainDataIndex, ]  # 70%
testData <- mydf[-trainDataIndex, ]  # 30%
table(trainData$Final3Months)

# Up Sampling Training Data
'%ni%' <- Negate('%in%')  # define 'not in' function
set.seed(99)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% 'Final3Months'],
                         y = trainData$Final3Months)
names(up_train)[9] <- "Final3Months"  # upSample() changed our column name
table(up_train$Final3Months)

# Build Logistic Model
logitModel <- glm(Final3Months ~ XLEclose + XLFclose + XLUclose + XLPclose + XLYclose + XLVclose, family=binomial, data=up_train, maxit = 100)
summary(logitModel)

  # Goodness of Fit - Deviance
# Chi-square test
anova(logitModel, test="Chisq")

# Histogram of fit
hist(logitModel$fitted)

# VIF
library(car)
vif(logitModel)

  # Predictions
# Test Data
pred <- predict(logitModel, newdata = testData, type = "response")
# pred greater than 0.5, it is 3 months before a stock peak
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Final3Months
# calculate accuracy
mean(y_pred == y_act)

# Current Market Data
todaydf <- data.frame(Date = c(2021-08-10), XLEclose = c(49.79), 
                      XLFclose = c(38.39), XLUclose = c(67.49), 
                      XLPclose = c(71.68), XLYclose = c(181.35), 
                      XLVclose =(133.03), SPYclose = c(442.40))
predict(logitModel, newdata = todaydf, type = "response")

# September 21, 2020
sep212020df <- data.frame(Date = c(2020-09-21), XLEclose = c(31.97), 
                      XLFclose = c(23.93), XLUclose = c(57.66), 
                      XLPclose = c(62.83), XLYclose = c(141.51), 
                      XLVclose =(103.18), SPYclose = c(326.97))
predict(logitModel, newdata = sep212020df, type = "response")

# Area Under Curve - ROC 
install.packages('pROC')
library('pROC')
roc(trainData$Final3Months, pred, plot = TRUE, col = "blue")

# Visualization
install.packages('visreg')
library(visreg)
visreg(logitModel, 'XLEclose', scale='response', rug=2,
       xlab='Energy (XLE)', ylab='Final 3 months of bull market')
visreg(logitModel, 'XLFclose', scale='response', rug=2,
       xlab='Financial (XLF)', ylab='Final 3 months of bull market')
visreg(logitModel, 'XLUclose', scale='response', rug=2,
       xlab='Utilities (XLU)', ylab='Final 3 months of bull market')
visreg(logitModel, 'XLPclose', scale='response', rug=2,
       xlab='Consum Staples (XLP)', ylab='Final 3 months of bull market')
visreg(logitModel, 'XLYclose', scale='response', rug=2,
       xlab='Consum Discretion (XLY)', ylab='Final 3 months of bull market')
visreg(logitModel, 'XLVclose', scale='response', rug=2,
       xlab='Health Care (XLV)', ylab='Final 3 months of bull market')

  # Kruskal-Wallis Test
# Boxplot by Sector
# First reshaping the data
library(reshape2)
newshape <- melt(mydf, id.vars='Date', measure.vars=c('XLEclose', 'XLFclose',
                 'XLUclose', 'XLPclose', 'XLYclose', 'XLVclose'))
names(newshape)[2] <- 'MarketSectors'
names(newshape)[3] <- 'ClosingPrice'
newshape %>% ggplot(aes(x=MarketSectors, y=ClosingPrice, 
                    fill=MarketSectors)) + geom_boxplot()

# Kruskal-Wallis Test
kruskal.test(ClosingPrice ~ MarketSectors, data = newshape)

# Wilcoxon Signed Rank test
pairwise.wilcox.test(newshape$ClosingPrice, newshape$MarketSectors,
                     p.adjust.method = "BH")
