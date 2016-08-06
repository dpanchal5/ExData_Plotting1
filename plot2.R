fileName <- "Data/household_power_consumption.txt"

## Read data:
data <- read.table(fileName, header=TRUE, sep=";", na.strings = "?")

## Subset the data we are interested in
subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

## get the data we are interested in plotting (datetime and Global_active_power)
datetime <- strptime(paste(subSetData$Date, subSetData$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
globalActivePower <- as.numeric(subSetData$Global_active_power)

## Create .png file for the plot
png("plot2.png", width=480, height=480, units = "px")

## Create x,y plot of data we are interested in
plot(datetime, globalActivePower, type="l", xlab="", ylab="Global Active Power (kilowatts)")

## Close the PNG device
dev.off()
