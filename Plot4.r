###############################################################################
## Exploratory Data Analysis Course Assignment - Plot 3
##

# Made by Stefan F in stackOverflow
#
phyMemAvailable <- function() {
    Ret <- 0
    if (Sys.info()[["sysname"]] == "Windows") {
        Mem <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
        Mem <- Mem[grepl("FreePhysicalMemory", Mem)]
        Mem <- gsub("FreePhysicalMemory=", "", Mem, fixed = TRUE)
        Mem <- gsub("\r", "", Mem, fixed = TRUE)
        Ret <- as.integer(Mem) / 1024
    } 
    Ret
}

# 1: Reading the data
#    From UC Irvine Machine Learning Repository
#    Electric power consumption
#    2,075,259 rows and 9 columns
#    Data from the dates 2007-02-01 and 2007-02-02
#
if (round(2075259 * 9 * 8 / 2 ^ {20}, 2) > phyMemAvailable()) {
    print("Not enough memory")
} else {
    allData <- read.table("household_power_consumption.txt", 
                          header = TRUE,
                          sep = ";", 
                          na.strings = "?",
                          colClasses = c("character", 
                                         "character", 
                                         "numeric", 
                                         "numeric", 
                                         "numeric", 
                                         "numeric", 
                                         "numeric", 
                                         "numeric", 
                                         "numeric"))
    allData$datetime <- strptime(paste(allData$Date, allData$Time) , "%d/%m/%Y %H:%M:%S")
    allData$Date <- as.Date(strptime(allData$Date, "%d/%m/%Y"))
    
    # Only Data from the dates 2007-02-01 and 2007-02-02
    #
    subData <- subset(allData, Date >= "2007-02-01" & Date <= "2007-02-02")
    
    # Free memory used by the allData object
    #
    rm(allData)
    invisible(gc())
}

if (exists("subData")) {
    # 2: Making the plot
    #
    png(filename = "plot4.png", width = 480, height = 480, units = "px")

    tickpos <- seq(as.POSIXct("2007-02-01"), 
                   as.POSIXct("2007-02-03"), 
                   by = "1 day")
    par(mfrow = c(2, 2))

    # Plot at position [1, 1]
    with(subData, plot(datetime, Global_active_power, type = "l", 
                       xlab = "", ylab = "Global Active Power", 
                       xaxt = "n"))
    axis.POSIXct(side = 1, at = tickpos, labels = c("Thu", "Fri", "Sat"))

    # Plot at position [1, 2]
    with(subData, plot(datetime, Voltage, type = "l", xaxt = "n"))
    axis.POSIXct(side = 1, at = tickpos, labels = c("Thu", "Fri", "Sat"))
    
    # Plot at position [2, 1]
    with(subData, plot(datetime, Sub_metering_1, type = "l", 
                       xlab = "", ylab = "Energy sub metering", 
                       xaxt = "n"))
    with(subData, points(datetime, Sub_metering_2, type = "l", col = "red"))
    with(subData, points(datetime, Sub_metering_3, type = "l", col = "blue"))
    axis.POSIXct(side = 1, at = tickpos, labels = c("Thu", "Fri", "Sat"))
    legend("topright", col = c("black", "red", "blue"), 
           legend = names(subData)[7:9], box.lty = 0, lty = 1, bg = NA)

    # Plot at position [2, 2]
    with(subData, plot(datetime, Global_reactive_power, type = "l", xaxt = "n"))
    axis.POSIXct(side = 1, at = tickpos, labels = c("Thu", "Fri", "Sat"))
    
    # 3: Closing the device
    #
    invisible(dev.off())
}
