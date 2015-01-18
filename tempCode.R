dataFile <- read.csv("activity.csv")
dataFileRemoveNa <- na.omit(dataFile)
stepsValues <- dataFileRemoveNa$steps

averageStepsPerInterval <- mean(dataFileRemoveNa$steps)

stepsCountPerDay <- sqldf("SELECT date, SUM(steps) AS StepsSum FROM dataFileRemoveNa GROUP BY date")

hist(stepsCountPerDay$StepsSum)
stepsMean <- mean(stepsCountPerDay$StepsSum)
stepsMedian <- median(stepsCountPerDay$StepsSum)

hist(stepsCountPerDay$StepsSum)

#plot(stepsCountPerDay$date~stepsCountPerDay$StepsSum, type="l")

stepsMeanPerDay<- sqldf("SELECT date, MEAN(steps) AS StepsSum FROM dataFileRemoveNa GROUP BY date")


stepsMean <- format(mean(stepsValues), nsmall=2)
stepsMedian <- median(stepsValues)

stepsMeanPretty <- format(stepsMean, nsmall=2)



dateValues <- as.ts(dataFileRemoveNa$date)
dateValues2 <- as.Date(dataFileRemoveNa$date)
#plot.ts(dateValues2, y = stepsValues)


library(sqldf)

##table1 = read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
averageDailyActivityPattern <- sqldf("SELECT interval, AVG(steps) AS StepsAverage FROM dataFileRemoveNa GROUP BY interval")
plot(averageDailyActivityPattern$StepsAverage~averageDailyActivityPattern$interval, type="l")

highestValueRow <- sqldf("SELECT interval, MAX(StepsAverage) FROM averageDailyActivityPattern")
highestInterval <- highestValueRow$interval


# Imputing Missing Values

# Calculate and report the total number of missing values.
dataFile2 <- dataFile[!complete.cases(dataFile),]
missingValueCount <- nrow(dataFile2)

#dataFileFixedNa <- dataFile[is.na(dataFile$steps),] <- stepsMean
#dataFileFixedNa <- dataFile[is.na(dataFile$steps),]

# create new dataset with the mean thrown in there.
dataFileFixedNa <- dataFile
dataFileFixedNa$steps[is.na(dataFileFixedNa$steps)] <- stepsMean

dataFileFixedNa <- dataFile[dataFile$steps==NA,]


png(filename="plot1.png", width=480, height=480)
hist(table2$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()



Foo <- function(inputDate){
  dayOfWeek <- weekdays(inputDate)
  if (true){
    return("foo")
  }
  else {
    return (weekdays(inputDate))
  }
}

# Weekdays stuff
WeekendOrWeekday <- function(inputDate){
  dayOfWeek <- weekdays(inputDate)
  if ((dayOfWeek == "Saturday") || (dayOfWeek == "Sunday")){
    return("weekend")
  }
  else {
    return("weekday")
  }
}

dataFileFixedNa$weekday2 <- lapply(as.Date(dataFileFixedNa$date), WeekendOrWeekday)

bar <- weekdays(as.Date("2015-01-18"))

plot(stepsValues~dateValues2, type="l")


##table1 = read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
table2 <- sqldf("SELECT * FROM table1 WHERE table1.Date='1/2/2007' OR table1.Date='2/2/2007'")


png(filename="plot1.png", width=480, height=480)
hist(table2$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()



plot(stepsValues~dateValues2, type="l")