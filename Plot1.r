###############################################################################
## Exploratory Data Analysis Course Assignment - Plot 1
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
                          colClasses = c("character", "character", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric"))
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
    png(filename = "plot1.png", width = 480, height = 480, units = "px")

    hist(subData$Global_active_power, 
         col = "red", 
         main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)")

    # 3: Closing the device
    #
    invisible(dev.off())
}
