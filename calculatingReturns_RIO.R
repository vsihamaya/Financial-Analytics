
#load packages
library(quantmod)
library(PerformanceAnalytics)
library(xts)

#read csv file into r
rio <- read.csv("rio.csv", header = TRUE)

#convert date to Dates
"date" <- as.Date(rio$Date, format = "%Y-%m-%d")

#combining date into dataframe
rio <- cbind(date, rio[, -1])
head(rio)
tail(rio)

#convert data.frame to xts object
rio <- xts(rio[,2:7], order.by = rio[ ,1])

#rename variables
names(rio) <-
  paste(c("rio.Open", "rio.High", "rio.Low", 
          "rio.Close", "rio.Adjusted", "rio.Volume"))

#plotting the data
plot(rio$rio.Close)

#PRICE RETURN > USE CLOSING PRICE
#calculate RIO's price return 
#step1: subset data to only include closing price
rio.price.rtn <- rio[ ,4]
rio.price.rtn[c(1:3,nrow(rio.price.rtn)), ]

#step2: use closing price to calculate the price return
# equation is implemented in R using the Delt command
rio.price.rtn$rio.price.rtn <- Delt(rio.price.rtn$rio.Close)
rio.price.rtn[c(1:3, nrow(rio.price.rtn)), ]

#clean up data object
#delete first observation/trading day (NA) in price return & closing price
options(digits = 3)
rio.price.rtn <- rio.price.rtn[-1, 2]
rio.price.rtn[c(1:3,nrow(rio.price.rtn)), ]


#TOTAL RETURN > USE ADJUSTED PRICE CLOSE
#calculating total return 
#step1: import data into r
rio.rtn<- rio[, 5]
rio.rtn[c(1:3,nrow(rio.rtn)), ]

#step2: apply the Delt command on adj. close price
rio.rtn$rio.total.rtn <- Delt(rio.rtn$rio.Adjusted)
rio.rtn[c(1:3, nrow(rio.rtn)), ]


#clean up data: do not delete the NA observation & limit the decimals to 3 digits
options(digits = 7)
rio.rtn <- rio.rtn[ , 2]
rio.rtn[c(1:3, nrow(rio.rtn)) ]

#LOG TOTAL RETURNS > USE ADJUSTED CLOSE PRICES
#calculating log returns: difference of natural log prices

#step1: import adjusted closing price data
rio.log.rtn <- rio[, 5]

#step2: calculate log returns
# use the "diff" and "log" command of the adjusted close prices
rio.log.rtn$rio.log.rtn <- diff(log(rio.log.rtn$rio.Adjusted))

#check log returns
options(digits = 7)
rio.log.rtn[c(1:3, nrow(rio.log.rtn)), ]

#clean up data > delete the adj return (col 1) & keep only log return series
options(digits = 3)
rio.log.rtn <- rio.log.rtn[ ,2]
rio.log.rtn[c(1:3, nrow(rio.log.rtn)), ]


#compare log returns & total returns
#combine two total return calculations using cbind command
#scipen=100 used to increase the threshold before R converts output into scientific notation
options(digits = 3, scipen = 100)
rio.total.rtn.comp <- cbind(rio.rtn, rio.log.rtn)
rio.total.rtn.comp[c(1:3, nrow(rio.total.rtn.comp)), ]

#req. na.rm=TRUE for max/min calculations
max(abs(rio.total.rtn.comp$rio.total.rtn-rio.total.rtn.comp$rio.log.rtn), na.rm = TRUE)
min(abs(rio.total.rtn.comp$rio.total.rtn-rio.total.rtn.comp$rio.log.rtn), na.rm = TRUE)

#revert options back to default display options
options(digits = 7, scipen = 0)

#calculating cumulating multi-day returns
#performance over specific timeframe & capture effects of reinvested dividends
#returns of the stock going forward determines
#whether the reinvested dividend earned a positive or negative return


#cumulating arithmetic returns > take the product of the daily gross returns
#gross return is one plus the net return R(t)
#step1: import data > total returns 
rio.acum <- rio.rtn
rio.acum[c(1:3, nrow(rio.acum)), ]

#step2: set the first day (investment made) total return value to zero
rio.acum[1,1] <- 0
rio.acum[c(1:3, nrow(rio.acum)), ]

#step3: calculate gross DAILY returns
#create a new variable for gross return > simply one plus the net total return
rio.acum$GrossRtn <- 1+rio.acum$rio.total.rtn
rio.acum[c(1:3, nrow(rio.acum)), ]








