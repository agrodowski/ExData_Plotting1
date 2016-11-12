# Exploratory Data Analysis course - Course Project 1
# Author: Adam Grodowski (adam.grodowski@gmail.com)
library(data.table)

PrepareHouseholdPowerData <- function(file, cols, filter) {
    # Extracts and refines the relevant observations and variables using fread 
    # and a 'setAs' trick to compensate for fread bug #2660 which makes numeric 
    # column processed as character due to '?' character found by fread during 
    # scanning phase regardless of 'na.strings' value. Requires 'data.table'.
    #
    # Args:
    #   file: The file with household power consumption data.
    #   cols: A character vector with variable types. Use 'mynumeric' instead 
    #         of 'numeric' to treat '?' as NA in numeric columns. Use 'NULL' to 
    #         skip that column. Must have 9 values, one for each column.
    #   filter: A logical closure for filtering the data.
    #
    # Returns:
    #   A data.table object with initially selected variables that satisfy 
    #   given filter with the following variables added (+) and removed (-):
    #   (+) DateTime: a POSIXct object computed from Date and Time variables 
    #                 in %d/%m/%Y %H:%M:%S format.
    #   (-) Date, Time: as not needed anymore
    setClass("mynumeric")
    setAs("character","mynumeric", 
          function(from) if (from=='?') NA else as.numeric(from))
    # Error handling
    if (length(cols) != 9)
        stop("There must be 9 values provided in cols, one for each column.
             Use 'NULL to skip that column")
    if (!file.exists(file))
        stop("File does not exist!")
    data <- fread(file, colClasses=cols, select=which(cols!="NULL"), 
                  sep=';', header=T, na.strings=c("?"))
    datawant <- filter(data)  # subset rows using provided filter
    timestamp <- paste(datawant$Date, datawant$Time)
    datawant[, DateTime := as.POSIXct(timestamp, format="%d/%m/%Y %H:%M:%S")]
    datawant[, c("Date","Time") := NULL]  # drops variables not needed anymore
}


# Retrieve dataset (if not present)
file <- "household_power_consumption.txt"
if (!file.exists(file)) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip",method="curl")
    unzip("household_power_consumption.zip")
}

# Prepare column types and filter closure for data loading
cols <- rep("NULL",9)  # initially nullify all columns
cols[1:2] <- "character"  # Date & Time, it is faster to read as chars
cols[c(3:5,7:9)] <- "mynumeric"  # class trick to prevent #2660
filter <- function(dt) 
    dt[Date %in% c("1/2/2007","2/2/2007")] # only rows wihtin 1-2/2/2007
# Extract only relevant and needed data
data <- PrepareHouseholdPowerData(file, cols, filter)

# [agr] plot in 2x2 grid, line type
png(filename="plot4.png",width=480, height=480)
par(mfrow=c(2,2))
with(data, {
  plot(DateTime,Global_active_power, type="l",xlab="",ylab="Global Active Power")
  plot(DateTime,Voltage, xlab="datetime", type="l")  
  plot(DateTime,Sub_metering_1, type="l",xlab="",ylab="Energy sub metering")
  lines(DateTime,Sub_metering_2, type="l",xlab="",col="red")
  lines(DateTime,Sub_metering_3, type="l",xlab="",col="blue")
  legend("topright",
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
         col = c("black","red","blue"), lty= 1,box.lwd=0)
  
  plot(DateTime,Global_reactive_power, xlab="datetime", type="l")
})
dev.off()
