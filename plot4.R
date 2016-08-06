fileName <- "Data/household_power_consumption.txt"

## Read data:
data <- read.table(fileName, header=TRUE, sep=";", na.strings = "?")

## Subset the data we are interested in
subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

## get the data we are interested in plotting
datetime <- strptime(paste(subSetData$Date, subSetData$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
globalActivePower <- as.numeric(subSetData$Global_active_power)
globalReactivePower <- as.numeric(subSetData$Global_reactive_power)
voltage <- as.numeric(subSetData$Voltage)
Sub_metering_1 <- as.numeric(subSetData$Sub_metering_1)
Sub_metering_2 <- as.numeric(subSetData$Sub_metering_2)
Sub_metering_3 <- as.numeric(subSetData$Sub_metering_3)

## Create .png file for the plot
png("plot4.png", width=480, height=480, units = "px")
par(mfrow = c(2, 2))

## Plot 1
plot(datetime, globalActivePower, type="l", xlab="", ylab="Global Active Power")

## Plot 2
plot(datetime, voltage, type="l", xlab="datetime", ylab="Voltage")

## Plot 3
plot(datetime, Sub_metering_1, type="l", xlab="", ylab="Energy Sub metering")
lines(datetime, Sub_metering_2, type="l", col="red")
lines(datetime, Sub_metering_3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"))

## Plot 4
plot(datetime, globalReactivePower, type="l", xlab="datetime", ylab="Global_reactive_power")

## Close the PNG device
dev.off()
