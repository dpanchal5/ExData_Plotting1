fileName <- "Data/household_power_consumption.txt"

## Read data:
data <- read.table(fileName, header=TRUE, sep=";", na.strings = "?")

## Subset the data we are interested in
subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

## Create .png file for the plot
png("plot1.png", width=480, height=480, units = "px")

## Create histogram plot of data we are interested in
hist(subSetData$Global_active_power, col = "red", xlab="Global Active Power (kilowatts)", main = "Global Active Power")

## Close the PNG device
dev.off()

