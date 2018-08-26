

library(quantmod)
library(PerformanceAnalytics)
library(xts)

#double check that adjusted close (col 6) & volume (col 7) correlates

jp <- read.csv("jp.csv", header = TRUE)

#convert date to Dates
"date" <- as.Date(jp$Date, format = "%Y-%m-%d")

#combining date into dataframe
jp <- cbind(date, jp[, -1])
head(jp)

#sorting the data into chronological order (if needed)
jp <- jp[order(jp$date) ,]

#convert data.frame to xts object
jp <- xts(jp[,2:7], order.by = jp[,1])
head(jp)
class(jp)

#rename variables
names(jp) <-
  paste(c("jp.Open", "jp.High", "jp.Low", 
          "jp.Close", "jp.Adjusted", "jp.Volume"))

#plotting the data
plot(jp$jp.Close)

#check the dimension
dim(jp)

#check the summary statistics
summary(jp)

#create a data object with only the most recent close
jp.lastclose <- jp[757,]

#view the first and last observation of the data
jp[c(1,nrow(jp)), ]

#keeping continguous rows
jp.lastWeek <- jp[757:753, ]
jp.lastWeek

#obtain closing prices the last 30 days
jp.last30 <- jp[ ((nrow(jp)-29)):nrow(jp),]
jp.last30

#keeping the first three rows and last row
jp.firstThree_Last <- jp[c(1:3, nrow(jp)), ]
jp.firstThree_Last

#keeping only one column [use square brackets]
jp.onlyClosing <- jp[ ,4]

#alternative to calling one column is specify variable name preceded by a $
jp.onlyVolume <- jp$jp.Volume
jp.onlyVolume[c(1:3, nrow(jp.onlyVolume)),]

#deleting one column
jp.deleteAdj <- jp[ ,-5]
jp.deleteAdj[c(1:3, nrow(jp.deleteAdj)), ]

#keeping non-contiguous columns
jp.OpenClose <- jp[ , c(1,4)]

#subsetting rows and columns
#calculate the VWAP (volume-weighted averge price)
#over the last 30 tradings days
jp.vwap <- jp[((nrow(jp)-29)):nrow(jp), c(4,6)]

jp.xts.CloseYTD<- subset(jp[ ,4],
  index(jp) >= "2017-08-10" &
  index(jp) <= "2018-08-10")

#step1: converting data into a data.frame object
jp.ClosingTYD <- cbind(index(jp),
  data.frame(jp[ ,4]))

#step2: change variable names & changing index to identifier of observation
names(jp.ClosingTYD)[1] <- paste("Date")
rownames(jp.ClosingTYD) <-seq(1, nrow(jp.ClosingTYD), 1)

#step3: subset the data
jp.ClosingTYD <- subset(jp.ClosingTYD,
  jp.ClosingTYD$Date >= "2017-08-10" &
  jp.ClosingTYD$Date <= "2018-08-10")

#converting to weekly prices
wk <- jp
data.weekly <- to.weekly(wk)

#confirm conversion of data to weekly prices
jp[2:6, ]
#confirm the volume for the week of the sume of the volime of entire week of shares traded
sum(jp[2:6, 5])

#converting to monthly prices
mo <- jp
data.monthly <- to.monthly(mo)


