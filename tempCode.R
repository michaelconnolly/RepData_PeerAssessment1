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
    return(0)
  }
  else {
    return(1)
  }
}

dataFileFixedNa$weekday <- lapply(as.Date(dataFileFixedNa$date), WeekendOrWeekday)


# create new dataset with the mean thrown in there.
dataFileFixedNa <- dataFile
dataFileFixedNa$steps[is.na(dataFileFixedNa$steps)] <- stepsMean
dataFileFixedNa$weekday <- lapply(as.Date(dataFileFixedNa$date), WeekendOrWeekday)

weekdays <- dataFileFixedNa[dataFileFixedNa$weekday==0, c("interval", "steps")]
weekends <- dataFileFixedNa[dataFileFixedNa$weekday==1, c("interval", "steps")]

averageWeekdays <- sqldf("SELECT interval, AVG(steps) as StepsAverage FROM weekdays GROUP BY interval")
averageWeekends <- sqldf("SELECT interval, AVG(steps) as StepsAverage FROM weekends GROUP BY interval")

#dataFileFixedNaWeekday <- sqldf("SELECT steps, date, interval FROM dataFileFixedNa WHERE weekday=1")
#averageWeekdayActivityPattern <- sqldf("SELECT interval, AVG(steps) AS StepsAverage FROM dataFileFixedNa GROUP BY interval")


#f#oo <- sqldf("SELECT interval, AVG(steps) AS StepsAverage FROM dataFileFixedNa GROUP BY interval")



par(mfrow=c(2,1))
plot(averageWeekdays$StepsAverage~averageWeekdays$interval, type="l", main="weekdays")
plot(averageWeekends$StepsAverage~averageWeekends$interval, type="l", main="weekends")

bar <- weekdays(as.Date("2015-01-18"))

plot(stepsValues~dateValues2, type="l")


##table1 = read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
table2 <- sqldf("SELECT * FROM table1 WHERE table1.Date='1/2/2007' OR table1.Date='2/2/2007'")


png(filename="plot1.png", width=480, height=480)
hist(table2$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()



plot(stepsValues~dateValues2, type="l")