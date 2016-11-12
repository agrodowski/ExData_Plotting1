#
# Exploratory Data Analysis couse - Course Project 1
# Author: Adam Grodowski (adam.grodowski@gmail.com)
#

# retrieve data (if not present)
file <- "household_power_consumption.txt"
if (!file.exists(file)) {
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip",method="curl")
  unzip("household_power_consumption.zip")
}

# Step 1 - retrieve only relevant variables - Date and Global Active Power, columns 1,3

# build columns vector - nullify everything except 1,3, use 'myDate'
cols <- rep("NULL",9)
cols[1] <- "character"
cols[3] <- "numeric"
data <- read.table(file, colClasses=cols, sep=';', header=T, na.strings='?', comment.char="")
datewant <- c(as.Date("2007-02-01"),as.Date("2007-02-02"))
# take the power within the relevant timeframe
powerfeb <- data[which(as.Date(data$Date,"%d/%m/%Y") %in% datewant), 2]

# plot a histogram to png
png(filename="plot1.png",width=480, height=480)
hist(powerfeb, col="red",ylim=c(0,1200),xlim=c(0,6),main="Global Active Power",xlab="Global Active Power (kilowatts)")
dev.off()
