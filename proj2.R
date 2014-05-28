
library(data.table)
library(ggplot2)

#LoadData : Load NEI and SCC data into global variables if not already present.
#           These variables are then accessed by plot functions.
#           NEI and SCC are loaded as data.table class.
#           If data files do not exist locally then they are downloaded
#           and unzipped. 
LoadData <- function() {

    if (!file.exists("summarySCC_PM25.rds") ||
        !file.exists("Source_Classification_Code.rds")) {
        
        if (!file.exists("FNEI_data.zip")) {
            print("Downloading FNEI_data.zip ...")
            download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", 
                          "FNEI_data.zip", mode="wb")
            unzip("FNEI_data.zip")
        }
    }

    #Note: NEI and SCC are set in global environment
    if (!exists("NEI")) {
        print("Loading NEI data ...")
        NEI <<- as.data.table(readRDS("summarySCC_PM25.rds"))
    }
    if (!exists("SCC")) {
        print("Loading SCC data ...")
        SCC <<- as.data.table(readRDS("Source_Classification_Code.rds"))
    }
}

#plot1 :    Plots a graph showing total PM 2.5 
#           emissions from all sources in USA 
#           for years 1999 - 2000
plot1 <- function() {
    LoadData() #NEI is now in global environment
    
    #get sum of emissions by year
    nei <- NEI[, sum(Emissions), by=year]
    setnames(nei, 2, "total.emissions")

    #plot the data
    plot(nei$year, nei$total.emissions, 
         type="b", yaxt="n", xaxt="n", pch=20,
         xlab="Year", ylab="Emissions (in million tons)",
         main="Total PM 2.5 emissions from all sources in USA")
    round.by <- 1000000
    axis(2, at=3:7 * round.by, labels=format(as.double(3:7), nsmall=1))
    axis(1, at=nei$year, labels=nei$year)
    text(nei$year, nei$total.emissions, 
         round(nei$total.emissions/round.by, 2), 
         cex=0.6, pos=c(4,2,4,2))
    
    #save the plot to png
    dev.copy(png, file="plot1.png")
    dev.off()
    
    #return data to caller
    nei
}

#plot2:     Plot a graph showing total PM 2.5 emissions
#           of the Baltimore City, Maryland
plot2 <- function() {
    LoadData() #NEI is now in global environment
    
    #get the sum of emissions by year for Baltimore City (fips=24510)
    nei <- NEI[fips == "24510", sum(Emissions), by=year]
    setnames(nei, 2, "total.emissions")

    #plot the data
    plot(nei$year, nei$total.emissions, 
         type="b", yaxt="n", xaxt="n", pch=20,
         xlab="Year", ylab="Emissions (in thousand tons)",
         main="Total PM 2.5 emissions of Baltimore City, MD")
    round.by <- 1000
    axis(2, at=1:4 * round.by, labels=format(as.double(1:4), nsmall=1))
    axis(1, at=nei$year, labels=nei$year)
    text(nei$year, nei$total.emissions, 
         round(nei$total.emissions/round.by, 2), 
         cex=0.6, pos=c(4,2,4,2))

    #save the plot to png
    dev.copy(png, file="plot2.png")
    dev.off()
    
    #return data to caller
    nei
}

#plot3:     Plot a graph showing totoal PM 2.5 emissions 
#           by each source type of the Baltimore City, Maryland 
plot3 <- function() {
    LoadData() #NEI is now in global environment

    #get the sum of emissions by year and source type
    #for Baltimore City (fips=24510)    
    nei <- NEI[fips == "24510", sum(Emissions), by=list(type,year)]
    setnames(nei, 3, "total.emissions")
    
    #create and print the plot
    p <- qplot(year, total.emissions, data=nei, 
               color=type,
               geom=c("line"), method="lm",
               xlab = c("Year"), ylab = c("PM 2.5 Emissions (in tons)"),
               main = "Total PM 2.5 emissions of Baltimore City, Maryland by source type") +
        theme(plot.title = element_text(size = 16))
    print(p)
    
    #save the plot to png
    dev.copy(png, file="plot3.png", width=640, height=480)
    dev.off()
    
    #return the data to the caller
    nei
}

#plot4: Plots a graph of PM 2.5 emissions from coal cumbustion
#       from all sources in USA
plot4 <- function() {
    LoadData() #NEI & SCC are now in global environment
    
    # Links:
    #   http://www.epa.gov/air/emissions/basic.htm
    #   http://www.epa.gov/air/emissions/index.htm
    #   http://www.epa.gov/cgi-bin/broker?polchoice=PM&_debug=0&_service=data&_program=dataprog.national_1.sas
    # Studying the links above it is understood that we need to grep
    # "combustion" from level one and "coal" from levels 3 and 4.
    # There are coal types at level 3, e.g Lignite, which do not have
    # the term "coal" in them. Use level 4 also in order to capture this
    
    scc.comb.1 <- grepl("combustion", SCC$SCC.Level.One, ignore.case=TRUE)
    scc.coal.3 <- grepl("coal", SCC$SCC.Level.Three, ignore.case=TRUE)
    scc.coal.4 <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE)
    scc.cumb.coal <- SCC[scc.comb.1 & (scc.coal.3 | scc.coal.4),]
    scc <- scc.cumb.coal$SCC

    #Get coal combustion by year
    nei <- NEI[SCC %in% scc, sum(Emissions), by=year]
    setnames(nei, 2, "total.emissions")
        
    #Plot the data
    plot(nei$year, nei$total.emissions, 
         type="b", yaxt="n", xaxt="n", pch=20,
         xlab="Year", ylab="Emissions (in hundred thousand tons)",
         main="PM 2.5 emissions from coal cumbustion in USA")
    round.by <- 100000
    axis(2, at=3:6 * round.by, labels=format(as.double(3:6), nsmall=1))
    axis(1, at=nei$year, labels=nei$year)
    text(nei$year, nei$total.emissions, 
         round(nei$total.emissions/round.by, 2), 
         cex=0.6, pos=c(4,2,4,2))
    
    #save the plot to png
    dev.copy(png, file="plot4.png")
    dev.off()
    
    #return the data to the caller
    nei
}

#plot5:     Plots a graph showing PM 2.5 emission from all
#           motor vehicles in Baltimore MD
plot5 <- function () {

    LoadData() #NEI & SCC are now in global environment
    
    # Links:
    #   http://www.epa.gov/air/emissions/basic.htm
    #   http://www.epa.gov/air/emissions/index.htm
    #   http://www.epa.gov/cgi-bin/broker?polchoice=PM&_debug=0&_service=data&_program=dataprog.national_1.sas
    # Studying the links above it is understood that we need to grep
    # "Mobile Sources" from SCC level one and ON-ROAD point types from NEI
    
    scc.mobile <- grepl("mobile", SCC$SCC.Level.One, ignore.case=TRUE)
    scc <- SCC[scc.mobile,]$SCC
    nei <- NEI[(fips == "24510") & (SCC %in% scc) & (type=="ON-ROAD"), 
               sum(Emissions), by=year]
    setnames(nei, 2, "total.emissions")
    
    #plot the data
    plot(nei$year, nei$total.emissions, 
         type="b", yaxt="n", xaxt="n", pch=20,
         xlab="Year", ylab="Emissions (in tons)",
         main="PM 2.5 emissions from motor vehicles in Baltimore")
    round.by <- 1
    axis(2, at=seq(0,300,100), labels=format(as.double(seq(0,300,100)), nsmall=0))
    axis(1, at=nei$year, labels=nei$year)
    text(nei$year, nei$total.emissions, 
         round(nei$total.emissions/round.by, 2), 
         cex=0.6, pos=c(4,2,4,2))
    
    #save the plot to png
    dev.copy(png, file="plot5.png")
    dev.off()
    
    #return the data to the caller
    nei
}

#plot6:     Plots graphs showing PM 2.5 comparisions of
#           emissions from motor vehicles between 
#           Baltimore City and LA  in USA
plot6 <- function () {

    LoadData() #NEI & SCC are now in global environment

    # Links:
    #   http://www.epa.gov/air/emissions/basic.htm
    #   http://www.epa.gov/air/emissions/index.htm
    #   http://www.epa.gov/cgi-bin/broker?polchoice=PM&_debug=0&_service=data&_program=dataprog.national_1.sas
    # Studying the links above it is understood that we need to grep
    # "Mobile Sources" from SCC level one and ON-ROAD point types from NEI
    
    scc.mobile <- grepl("mobile", SCC$SCC.Level.One, ignore.case=TRUE)
    scc <- SCC[scc.mobile,]$SCC
    
    #subset data of motor vehicles in Baltimore
    nei <- NEI[(fips == "24510") & (SCC %in% scc) & (type=="ON-ROAD"), 
                         sum(Emissions), by=year]
    setnames(nei, 2, "balt")
    
    #subset data of motor vehicles in LA
    nei.la <- NEI[(fips == "06037") & (SCC %in% scc) & (type=="ON-ROAD"), 
                         sum(Emissions), by=year]
    setnames(nei.la, 2, "la")
    nei[, la := nei.la$la]
    
    #add relative (in %) and absolute differences
    nei[, c("balt.per","la.per", "balt.abs", "la.abs") := 
            list(balt/max(balt), la/max(la), 
                 balt - head(balt, n=1), la - head(la, n=1))]

    #plot 3 graphs showing actual values, absolute differences and relative differences
    par(mfrow = c(1,3), oma = c(0, 0, 3, 0))

    plot(nei$year, nei$la, ylim=c(0, 4700), 
         type="n", xaxt="n",
         xlab="Year", ylab="Emissions (in tons)")
        lines(nei$year, nei$balt, col="red")
        lines(nei$year, nei$la, col="blue")
        axis(1, at=nei$year, labels=nei$year)
        legend("topleft", lty=c(1,1), col=c("red", "blue"),
               legend=c("Baltimore", "LA"))
    
    plot(nei$year, nei$balt.abs, ylim=c(-300, 700), 
         type="n", xaxt="n",
         xlab="Year", ylab="Absolute differeces (in tons)")
        lines(nei$year, nei$balt.abs, col="red")
        lines(nei$year, nei$la.abs, col="blue")
        axis(1, at=nei$year, labels=nei$year)
        legend("topleft", lty=c(1,1), col=c("red", "blue"), 
               legend=c("Baltimore", "LA"))
    
    plot(nei$year, nei$balt.per, 
         type="n", xaxt="n",
         xlab="Year", ylab="Relative %age differeces")
        lines(nei$year, nei$balt.per, col="red")
        lines(nei$year, nei$la.per, col="blue")
        axis(1, at=nei$year, labels=nei$year)
        legend("bottomleft", lty=c(1,1), col=c("red", "blue"),
               legend=c("Baltimore", "LA"))
    
    title("top", outer=TRUE, cex.main=2.0, 
          main="Motor vehicle PM 2.5 emissions comparision of Baltimore and LA")
    
    #save the data to png
    dev.copy(png, file="plot6.png", width=1000, height=500)
    dev.off()
    
    #reset par to original values
    par(mfrow = c(1,1), oma = c(0,0,0,0))
    
    #return the data to the caller
    nei
}

#LoadData()
plot1()
plot2()
plot3()
plot4()
plot5()
plot6()


