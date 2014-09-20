##
## Configuring the World -- Assignment-1
##
## Program written by Brian Greiner
## September 18, 2014
##

##
## set the working directory
setwd("C:/Coursera/Configuring The World/Assignment-1")


##
## load the required libraries
library(xlsx)
library(plotrix)
library(tm)
library(FactoMineR)
library(gplots)


##
## read the databases
## use only the "data without small states" sheet, which is sheet-2
##
dataSet1 <- read.xlsx("..\\data\\CtW 1 Size, wealth and poverty.xlsx",2)
dataSet2 <- read.xlsx("..\\data\\CTW 2 Economic Globalisation and trade.xlsx", 2)


##
## extract the data specific to our region and country of interest
##
region = "Western Europe"
outsideCountry = "Canada"

dataSet1Region <- subset(dataSet1,Region==region ,stringsAsFactors=FALSE)
dataSet1Country <- subset(dataSet1, Country.==outsideCountry ,stringsAsFactors=FALSE)

dataSet2Region <- subset(dataSet2,Region==region ,stringsAsFactors=FALSE)
dataSet2Country <- subset(dataSet2, Country.==outsideCountry ,stringsAsFactors=FALSE)

countryNames <- c(as.character(dataSet2Region$Country.), as.character(dataSet2Country$Country.))


##
## analyse in terms of population
##
#dataSet1Country$Population.
#dataSet1Region$Population.
##
png(filename="figure-1.png", width=800, height=600)
par(mfrow=c(2,1))
popData <- data.frame(c(dataSet1Region$Population.,dataSet1Country$Population.), countryNames)
colnames(popData) <- c("population", "country")
popDataU <- popData
popData <- popData[order(popData$population),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[popData$country==outsideCountry] <- "blue"
barplot(popData$population,
        names.arg = popData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "Population (in thousands)")
box()

# look at all the countries in the database
dataSet1World <- dataSet1[order(dataSet1$Population.),]
barColours <- character(length(dataSet1World$Population.))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[dataSet1World$Country.==countryNames[i]] <- "orange"}
barColours[dataSet1World$Country.==outsideCountry] <- "blue"

barplot(log(dataSet1World$Population.),
        #barplot(dataSet1World$Population.,
        col=barColours,
        main = "Population for all Countries",
        xlab = "FIGURE 1 - Population 2012",
        ylab = "log(Population in thousands)")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of GDP
##
##
png(filename="figure-2.png", width=800, height=600)
par(mfrow=c(2,1))
gdpData <- data.frame(c(as.numeric(as.character(dataSet1Region$GDP.in.current.Dollars.2012..in.millions..)),
                        as.numeric(as.character(dataSet1Country$GDP.in.current.Dollars.2012..in.millions..))), 
                      countryNames)
colnames(gdpData) <- c("gdp", "country")
gdpDataU <- gdpData
gdpData <- gdpData[order(gdpData$gdp),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[gdpData$country==outsideCountry] <- "blue"
barplot(gdpData$gdp,
        names.arg = gdpData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "GDP (in millions)")
box()

# look at all the countries in the database
gdpDataWorld <- data.frame(as.numeric(as.character(dataSet1$GDP.in.current.Dollars.2012..in.millions..)),
                           dataSet1$Country.)
colnames(gdpDataWorld) <- c("gdp", "country")
gdpDataWorld <- gdpDataWorld[order(gdpDataWorld$gdp),]

#gdpDataWorld <- dataSet1[order(dataSet1$GDP.in.current.Dollars.2012..in.millions..),]

#gdpDataWorld <- gdpDataWorld[order(gdpDataWorld$GDP.in.current.Dollars.2012..in.millions..)]
barColours <- character(length(gdpDataWorld$gdp))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[gdpDataWorld$country==countryNames[i]] <- "orange"}
barColours[gdpDataWorld$country==outsideCountry] <- "blue"

barplot(log(gdpDataWorld$gdp),
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 2 - GDP (in current dollars 2012)",
        ylab = "log(GDP)")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of GDP PPP
##
##
png(filename="figure-3.png", width=800, height=600)
par(mfrow=c(2,1))
gdpPPPData <- data.frame(c(as.numeric(as.character(dataSet1Region$GDP.Per.Capita.based.on.PPP..2011)),
                           as.numeric(as.character(dataSet1Country$GDP.Per.Capita.based.on.PPP..2011))), 
                         countryNames)
colnames(gdpPPPData) <- c("gdp", "country")
gdpPPPDataU <- gdpPPPData
gdpPPPData <- gdpPPPData[order(gdpPPPData$gdp),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[gdpPPPData$country==outsideCountry] <- "blue"
barplot(gdpPPPData$gdp,
        names.arg = gdpPPPData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "value")
box()

# look at all the countries in the database
gdpPPPDataWorld <- data.frame(as.numeric(as.character(dataSet1$GDP.Per.Capita.based.on.PPP..2011)),
                              dataSet1$Country.)
colnames(gdpPPPDataWorld) <- c("gdp", "country")
gdpPPPDataWorld <- gdpPPPDataWorld[order(gdpPPPDataWorld$gdp),]
barColours <- character(length(gdpPPPDataWorld$gdp))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[gdpPPPDataWorld$country==countryNames[i]] <- "orange"}
barColours[gdpPPPDataWorld$country==outsideCountry] <- "blue"

barplot(log(gdpPPPDataWorld$gdp),
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 3 - GDP PPP (GDP Per Capita based on PPP 2011)",
        ylab = "log(value)")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of HDI
##
##
png(filename="figure-4.png", width=800, height=600)
par(mfrow=c(2,1))
hdiData <- data.frame(c(as.numeric(as.character(dataSet1Region$Human.Development.Index..HDI..value.2012)),
                        as.numeric(as.character(dataSet1Country$Human.Development.Index..HDI..value.2012))), 
                      countryNames)
colnames(hdiData) <- c("hdi", "country")
hdiDataU <- hdiData
hdiData <- hdiData[order(hdiData$hdi),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[hdiData$country==outsideCountry] <- "blue"
barplot(hdiData$hdi,
        names.arg = hdiData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "value")
box()

# look at all the countries in the database
hdiDataWorld <- data.frame(as.numeric(as.character(dataSet1$GDP.Per.Capita.based.on.PPP..2011)),
                           dataSet1$Country.)
colnames(hdiDataWorld) <- c("hdi", "country")
hdiDataWorld <- hdiDataWorld[order(hdiDataWorld$hdi),]
barColours <- character(length(hdiDataWorld$hdi))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[hdiDataWorld$country==countryNames[i]] <- "orange"}
barColours[hdiDataWorld$country==outsideCountry] <- "blue"

barplot(log(hdiDataWorld$hdi),
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 4 - Human Development Index (HDI) value 2012",
        ylab = "log(value)")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of Economic globalization
##
##
png(filename="figure-5.png", width=800, height=600)
par(mfrow=c(2,1))
eglobData <- data.frame(c(as.numeric(as.character(dataSet2Region$Economic.Globalization.index..KOF..2011)),
                          as.numeric(as.character(dataSet2Country$Economic.Globalization.index..KOF..2011))), 
                        countryNames)
colnames(eglobData) <- c("EconomicGlobalization", "country")
eglobDataU <- eglobData
eglobData <- eglobData[order(eglobData$EconomicGlobalization),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[eglobData$country==outsideCountry] <- "blue"
barplot(eglobData$EconomicGlobalization,
        names.arg = eglobData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "value")
box()

# look at all the countries in the database
eglobDataWorld <- data.frame(as.numeric(as.character(dataSet2$Economic.Globalization.index..KOF..2011)),
                             dataSet2$Country.)
colnames(eglobDataWorld) <- c("EconomicGlobalization", "country")
eglobDataWorld <- eglobDataWorld[order(eglobDataWorld$EconomicGlobalization),]
barColours <- character(length(eglobDataWorld$EconomicGlobalization))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[eglobDataWorld$country==countryNames[i]] <- "orange"}
barColours[eglobDataWorld$country==outsideCountry] <- "blue"

x <-which(is.na(eglobDataWorld$EconomicGlobalization))
barplot(eglobDataWorld$EconomicGlobalization[1:x[1]],
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 5 - Economic globalization index (KOF) 2011",
        ylab = "value")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of Trade as a % of GDP
##
##
png(filename="figure-6.png", width=800, height=600)
par(mfrow=c(2,1))
tradeData <- data.frame(c(as.numeric(as.character(dataSet2Region$Trade.as...GDP.2012)),
                          as.numeric(as.character(dataSet2Country$Trade.as...GDP.2012))), 
                        countryNames)
colnames(tradeData) <- c("trade", "country")
tradeDataU <- tradeData
tradeData <- tradeData[order(tradeData$trade),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[tradeData$country==outsideCountry] <- "blue"
barplot(tradeData$trade,
        names.arg = tradeData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "%")
box()

# look at all the countries in the database
tradeDataWorld <- data.frame(as.numeric(as.character(dataSet2$Trade.as...GDP.2012)),
                             dataSet2$Country.)
colnames(tradeDataWorld) <- c("trade", "country")
tradeDataWorld <- tradeDataWorld[order(tradeDataWorld$trade),]
barColours <- character(length(tradeDataWorld$trade))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[tradeDataWorld$country==countryNames[i]] <- "orange"}
barColours[tradeDataWorld$country==outsideCountry] <- "blue"

x <-which(is.na(tradeDataWorld$trade))
barplot(tradeDataWorld$trade[1:x[1]],
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 6 - Trade as % GDP 2012",
        ylab = "%")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of FDI Inward flow
##
##
png(filename="figure-7.png", width=800, height=600)
par(mfrow=c(2,1))
inwardData <- data.frame(c(as.numeric(as.character(dataSet2Region$FDI.Flow.Inward.in.millions.of.dollars..UNCTAD..2012)),
                           as.numeric(as.character(dataSet2Country$FDI.Flow.Inward.in.millions.of.dollars..UNCTAD..2012))), 
                         countryNames)
colnames(inwardData) <- c("inwardFlow", "country")
inwardDataU <- inwardData
inwardData <- inwardData[order(inwardData$inwardFlow),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[inwardData$country==outsideCountry] <- "blue"
barplot(inwardData$inwardFlow,
        names.arg = inwardData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "millions of $")
box()

# look at all the countries in the database
inwardDataWorld <- data.frame(as.numeric(as.character(dataSet2$FDI.Flow.Inward.in.millions.of.dollars..UNCTAD..2012)),
                              dataSet2$Country.)
colnames(inwardDataWorld) <- c("inwardFlow", "country")
inwardDataWorld <- inwardDataWorld[order(inwardDataWorld$inwardFlow),]
barColours <- character(length(inwardDataWorld$trinwardFlowade))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[inwardDataWorld$country==countryNames[i]] <- "orange"}
barColours[inwardDataWorld$country==outsideCountry] <- "blue"

x <-which(is.na(inwardDataWorld$inwardFlow))
barplot(inwardDataWorld$inwardFlow[1:x[1]],
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 7 - FDI Flow Inward in millions of dollars (UNCTAD) 2012",
        ylab = "millions of $")
box()
par(mfrow=c(1,1))
dev.off()


##
## analyse in terms of FDI Outward flow
##
##
png(filename="figure-8.png", width=800, height=600)
par(mfrow=c(2,1))
outwardData <- data.frame(c(as.numeric(as.character(dataSet2Region$FDI.Flow.outward.in.millions.of.dollars..UNCTAD..2012)),
                            as.numeric(as.character(dataSet2Country$FDI.Flow.outward.in.millions.of.dollars..UNCTAD..2012))), 
                          countryNames)
colnames(outwardData) <- c("outwardFlow", "country")
outwardDataU <- outwardData
outwardData <- outwardData[order(outwardData$outwardFlow),]
barColours <- c("orange", "orange", "orange", "orange", "orange", "orange", "orange")
barColours[outwardData$country==outsideCountry] <- "blue"
barplot(outwardData$outwardFlow,
        names.arg = outwardData$country,
        col=barColours,
        main = "for countries in study",
        xlab = "",
        ylab = "millions of $")
box()

# look at all the countries in the database
outwardDataWorld <- data.frame(as.numeric(as.character(dataSet2$FDI.Flow.outward.in.millions.of.dollars..UNCTAD..2012)),
                               dataSet2$Country.)
colnames(outwardDataWorld) <- c("outwardFlow", "country")
outwardDataWorld <- outwardDataWorld[order(outwardDataWorld$outwardFlow),]
barColours <- character(length(outwardDataWorld$trinwardFlowade))
barColours[1:length(barColours)] <- "gray"
for(i in 1:length(countryNames))
{barColours[outwardDataWorld$country==countryNames[i]] <- "orange"}
barColours[outwardDataWorld$country==outsideCountry] <- "blue"

x <-which(is.na(outwardDataWorld$outwardFlow))
barplot(outwardDataWorld$outwardFlow[1:x[1]],
        col=barColours,
        main = "For all countries",
        xlab = "FIGURE 8 - FDI Flow Outward in millions of dollars (UNCTAD) 2012",
        ylab = "millions of $")
box()
par(mfrow=c(1,1))
dev.off()


##
## summarize everything in a spiderweb plot
##
# first, normalize all the specific items to the maximum value for that item
png(filename="figure-9.png", width=800, height=600)
maxPop <- max(popData$population)
maxGdp <- max(gdpData$gdp)
maxGdpPPP <- max(gdpPPPData$gdp)
maxHDI <- max(hdiData$hdi)
maxEconGlob <- max(eglobData$EconomicGlobalization)
maxTrade <- max(tradeData$trade)
maxFdiIn <- max(inwardData$inwardFlow)
maxFdiOut <- max(outwardData$outwardFlow)

items.names <- c("population", "gdp", "gdp PPP", "HDI", "Econ.Glob.", "Trade", "FDI In", "FDI Out")

spiderColour <- c("blue", "orange", "green", "yellow", "red", "violet", "black")
spiderCountry <- c(outsideCountry,countryNames[1],countryNames[2],countryNames[3],countryNames[4],countryNames[5],countryNames[6])

radial.plot(spd,
            labels=items.names,
            rp.type="p",
            radial.lim=c(-0.2,1),
            line.col=spiderColour,
            lwd=3,
            main = "FIGURE 9 - Comparison of Countries in the study")
legend(1.2, 1.4,spiderCountry, lty=1, col=spiderColour, bty='n', cex=1)
dev.off()


##
## look at "closeness" by using denogram
##
png(filename="figure-10.png", width=800, height=600)
plot(hclust(dist(spd, method = "euclidean")),
     main = "FIGURE 10 - Cluster Dendogram Analysis",
     xlab = "")
dev.off()



## >>>>>  end of code  <<<<<
