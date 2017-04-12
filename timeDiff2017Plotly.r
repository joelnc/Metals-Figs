## Updated Apr 2017 to run all sites for copper, need to carry
## changes through to other analytes (i.e, tdiff f() call)


setwd("c:/Users/95218/Documents/R/public/Metals-Figs")
rm(list=ls())
graphics.off()

## Load file-- this should be a text export from WQD using the appropriate
## site list, Element set to ICS1.1 for FIM, all other fields can be blank
analytesDF <- read.csv("metalsSitesTxt.txt", stringsAsFactors=FALSE, sep=",",
                       header=TRUE)

## Drop to metals only
metalsDF <- analytesDF[which(analytesDF$Analyte %in%
                             c("Arsenic","Beryllium","Cadmium","Chromium",
                               "Copper", "Hardness", "Lead", "Nickel",
                               "Selenium", "Silver", "Zinc")),]
## Format date times
metalsDF$Coldate <- as.POSIXct(metalsDF$Coldate, format="%m/%d/%Y %H:%M:%S")

#############################################################################
#############################################################################
## From the larger all metals / all sites DF, split out by analytes into a
## set of lists(e.g., a hardness list, a zinc list), where each list is a
## collection of data frames (DF), and each DF is all FIM of that list's mtl
## for a site (i.e., for 25 sites, hd is list of 25 DFs)

## Hardness
hd <- split(metalsDF[which(metalsDF$Analyte=="Hardness"),],
             metalsDF[which(metalsDF$Analyte=="Hardness"),"Site"])
## Arsenic
ars <- split(metalsDF[which(metalsDF$Analyte=="Arsenic"),],
             metalsDF[which(metalsDF$Analyte=="Arsenic"),"Site"])
## Beryllium
be <- split(metalsDF[which(metalsDF$Analyte=="Beryllium"),],
             metalsDF[which(metalsDF$Analyte=="Beryllium"),"Site"])
## Cadmium
cd <- split(metalsDF[which(metalsDF$Analyte=="Cadmium"),],
             metalsDF[which(metalsDF$Analyte=="Cadmium"),"Site"])
## Chromium
ch <- split(metalsDF[which(metalsDF$Analyte=="Chromium"),],
             metalsDF[which(metalsDF$Analyte=="Chromium"),"Site"])
## Copper
cu <- split(metalsDF[which(metalsDF$Analyte=="Copper"),],
             metalsDF[which(metalsDF$Analyte=="Copper"),"Site"])
## Lead
pb <- split(metalsDF[which(metalsDF$Analyte=="Lead"),],
             metalsDF[which(metalsDF$Analyte=="Lead"),"Site"])
## Nickel
ni <- split(metalsDF[which(metalsDF$Analyte=="Nickel"),],
             metalsDF[which(metalsDF$Analyte=="Nickel"),"Site"])
## Selenium
se <- split(metalsDF[which(metalsDF$Analyte=="Selenium"),],
             metalsDF[which(metalsDF$Analyte=="Selenium"),"Site"])
## Silver
ag <- split(metalsDF[which(metalsDF$Analyte=="Silver"),],
             metalsDF[which(metalsDF$Analyte=="Silver"),"Site"])
## Zinc
zn <- split(metalsDF[which(metalsDF$Analyte=="Zinc"),],
             metalsDF[which(metalsDF$Analyte=="Zinc"),"Site"])

#############################################################################
#############################################################################

## Custom function to organize and pair the metals and hardness data
orgData <- function(hd, metal) {

    ## call this other function to ....
    source("metalsThresFuns.r")
    ## Extract a given sites hd and metals data, subset of columns
    hdTemp <- hd[,c("Element","Coldate","Site","Analyte","Qualifier",
                   "Result","Aunit","MDL","Type","Storm","Comments")]
    metalTemp <- metal[,c("Element","Coldate","Site","Analyte","Qualifier",
                   "Result","Aunit","MDL","Type","Storm","Comments")]

    ## Merge them (all) creating NAs where no match
    metalHdTemp <- merge(metalTemp, hdTemp, by="Coldate", all=TRUE)

    fun <- match.fun(paste(metalTemp$Analyte[1],"Fun",sep=""))
    funOut <- lapply(X=as.list(metalHdTemp$Result.y),FUN=fun)
    funOut2 <- do.call("rbind", funOut)

    metalHdTemp$cStan <- unlist(funOut2[,"chron"])
    metalHdTemp$aStan <- unlist(funOut2[,"ac"])
    metalHdTemp$cStanValid <- unlist(funOut2[,"chronV"])
    metalHdTemp$aStanValid <- unlist(funOut2[,"acV"])
    metalHdTemp$cStanTot <- unlist(funOut2[,"chronT"])

    site <- hd$Site[1]

    ## assign constructed DF to site name
    assign(site, metalHdTemp)
    return(assign(site, metalHdTemp))
}

asList <- mapply(orgData, hd=hd, metal=ars,
                 SIMPLIFY=FALSE)
beList <- mapply(orgData, hd=hd, metal=be,
                 SIMPLIFY=FALSE)
cdList <- mapply(orgData, hd=hd, metal=cd,
                 SIMPLIFY=FALSE)
chList <- mapply(orgData, hd=hd, metal=ch,
                 SIMPLIFY=FALSE)
cuList <- mapply(orgData, hd=hd, metal=cu,
                 SIMPLIFY=FALSE)
niList <- mapply(orgData, hd=hd, metal=ni,
                 SIMPLIFY=FALSE)
pbList <- mapply(orgData, hd=hd, metal=pb,
                 SIMPLIFY=FALSE)
seList <- mapply(orgData, hd=hd, metal=se,
                 SIMPLIFY=FALSE)
agList <- mapply(orgData, hd=hd, metal=ag,
                 SIMPLIFY=FALSE)
znList <- mapply(orgData, hd=hd, metal=zn,
                 SIMPLIFY=FALSE)


## All data #################################################################
allAs <- do.call("rbind",asList)
allBe <- do.call("rbind",beList)
allCd <- do.call("rbind",cdList)
allCh <- do.call("rbind",chList)
allCu <- do.call("rbind",cuList)
allNi <- do.call("rbind",niList)
allPb <- do.call("rbind",pbList)
allSe <- do.call("rbind",seList)
allAg <- do.call("rbind",agList)
allZn <- do.call("rbind",znList)

########## Setup items   ##################################################
## Custom fun to return site names from site codes
source("c:/Users/95218/Documents/R/returnSiteName.r")
library("plotly")  ## load plotly
Sys.setenv("plotly_username"="jnipper") ## plotly credentials
Sys.setenv("plotly_api_key"="3hqrxn5wot")

############################################################################
############################## Plotting Function ###########################
## Custom Fun to take a formated metals DF (including standards) adn make
## a plotly timeDiff plot inputs are the metal df, metal name (chr), and
## data indices, flagging FIM Dissolved and Total

tdiffPlotFun <- function(dMetalData, tMetalData, metalName, yLimVal) {
    mPlot <- plot_ly() %>%
        layout(
            font = list(
                family = "Arial",
                size = 14),
            hovermode = "closest",
            legend = list(
                font = list(
                    family = "Arial", size = 18),
                x=0.75, y=0.90),
            margin = list(
                r = 80, t = 120, b = 120, l = 80),
            title = metalName,
            titlefont = list(
                family = "Arial", size = 31),
            xaxis = list(
                autorange = TRUE,
                tickfont = list(
                    size = 18),
                type = "date"),
            yaxis = list(
##                autorange=TRUE,
                range = c(-yLimVal, ~yLimVal),
                tickfont = list(
                    size = 18),
                title = "Sample Result Minus Acute Dissolved Standard (ug/L)",
                titlefont = list(
                    family = "Arial",
                    size = 24),
                type = "linear",
                zeroline=FALSE),
            shapes = list(
                list(
                    fillcolor = "green",
                    line = list(
                        color = "white"),
                    opacity = 0.3,
                    type = "rect",
                    x0 = "2010-01-01", x1 = "2017-01-01",
                    xref = "x",
                    y0 = -yLimVal, y1 = 0, yref = "y"),
                list(
                   fillcolor = "red",
                   line = list(
                       color = "white"),
                   opacity = .20,
                   type = "rect",
                   x0 = "2010-01-01", x1 = "2017-01-01",
                   xref = "x",
                   y0 = 0, y1 = yLimVal, yref = "y")
            )
        ) %>%
        add_trace(data=tMetalData,
                  x = ~Coldate,
                  y = (~Result.x-aStan),
                  mode= "markers", type = "scatter",
                  marker = list(color="black", size = 4),
                  hoverinfo="text",
                  text=~paste(Site.x, siteNamesT,
                         '<br> Conc.: ', Result.x, 'ug/L',
                         '<br> Hardness: ', Result.y,
                         '<br> Storm: ', Storm.x, '<br> Date: ',
                         as.Date(Coldate)),
                  name = "Total") %>%
        add_trace(data=dMetalData,
                  x = ~Coldate,
                  y = ~(Result.x-aStan),
                  mode= "markers", type = "scatter",
                  marker = list(color="red", size = 6),
                  hoverinfo="text",
                  text=~paste(Site.x, siteNamesD,
                     '<br> Conc.: ', Result.x, 'ug/L',
                      '<br> Hardness: ', Result.y,
                      '<br> Storm: ', Storm.x, '<br> Date: ',
                      as.Date(Coldate)),
                  name="Dissolved")
    return(mPlot)
}

#############################################################################
#############################################################################
## Following are a set of calls to the above function, once per metal


############################################################################
############################## Copper ######################################
## Index data
FIMD <- which(allCu$Type.x=="Grab - Filtered" &
              allCu$aStanValid!="no" &
              allCu$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allCu$Element.x=="ICS1.1" &
              allCu$Type.x!="Grab - Filtered" &
              allCu$aStanValid=="yes" &
              allCu$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allCu$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allCu$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allCu$Site.x[FIMD])

##cuPlot <- tdiffPlotFun(allCu, "Copper", 100, FIMD, FIMT)
cuPlot <- tdiffPlotFun(allCu[FIMD, ], allCu[FIMT, ], "Copper", 100)

cuPlot

## Post Plot
plotly_POST(cuPlot, filename = "CopperNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)






############################################################################
############################## Arsenic ######################################
## Pull out indexes for dissolved vs total data.. could be simplified by
## by only including FIM data from 2010 in the base text file from WQD
FIMD <- which(allAs$Type.x=="Grab - Filtered" &
              allAs$aStanValid!="no" &
              allAs$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allAs$Element.x=="ICS1.1" &
              allAs$Type.x!="Grab - Filtered" &
              allAs$aStanValid=="yes" &
              allAs$Coldate>as.POSIXct("2010-01-01 0:00"))

## Somewhat klugey interpreter of siteCode--> site name, run seperately
## for dissolved vs total data-- used to make site names 'hover'
funName <- paste0(sort(unique(allAs$Analyte.x))[1],"Fun")
siteNamesT <- sapply(FUN=returnSiteName, X=allAs$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allAs$Site.x[FIMD])

## Call to the plotly function, creating a plotly object
asPlot <- tdiffPlotFun(allAs[FIMD, ], allAs[FIMT, ], "Arsenic", 400)

## Use this simple command to plot locally in a brower tab
asPlot

## Use this to post to the web
plotly_POST(asPlot, filename = "ArsenicNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Beryllium ###################################
## Index data
FIMD <- which(allAs$Type.x=="Grab - Filtered" &
              allBe$aStanValid!="no" &
              allBe$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allBe$Element.x=="ICS1.1" &
              allBe$Type.x!="Grab - Filtered" &
              allBe$aStanValid=="yes" &
              allBe$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allBe$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allBe$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allBe$Site.x[FIMD])


bePlot <- tdiffPlotFun(allBe,"Beryllium",100, FIMD, FIMT)
##bePlot

## Post Plot
plotly_POST(bePlot, filename = "BerylliumNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Cadmium #####################################
## Index data
FIMD <- which(allCd$Type.x=="Grab - Filtered" &
              allCd$aStanValid!="no" &
              allCd$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allCd$Element.x=="ICS1.1" &
              allCd$Type.x!="Grab - Filtered" &
              allCd$aStanValid=="yes" &
              allCd$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allCd$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allCd$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allCd$Site.x[FIMD])

cdPlot <- tdiffPlotFun(allCd, "Cadmium", 10, FIMD, FIMT)
##cdPlot

## Post Plot
plotly_POST(cdPlot, filename = "CadmiumNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Chromium ####################################
## Index data
FIMD <- which(allCh$Type.x=="Grab - Filtered" &
              allCh$aStanValid!="no" &
              allCh$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allCh$Element.x=="ICS1.1" &
              allCh$Type.x!="Grab - Filtered" &
              allCh$aStanValid=="yes" &
              allCh$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allCh$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allCh$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allCh$Site.x[FIMD])

chPlot <- tdiffPlotFun(allCh, "Chromium", 1200, FIMD, FIMT)
##chPlot

## Post Plot
plotly_POST(chPlot, filename = "ChromiumNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Lead ######################################
## Index data
FIMD <- which(allPb$Type.x=="Grab - Filtered" &
              allPb$aStanValid!="no" &
              allPb$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allPb$Element.x=="ICS1.1" &
              allPb$Type.x!="Grab - Filtered" &
              allPb$aStanValid=="yes" &
              allPb$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allPb$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allPb$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allPb$Site.x[FIMD])

pbPlot <- tdiffPlotFun(allPb, "Lead", 200, FIMD, FIMT)
##pbPlot

## Post Plot
plotly_POST(pbPlot, filename = "LeadNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Nickel ######################################
## Index data
FIMD <- which(allNi$Type.x=="Grab - Filtered" &
              allNi$aStanValid!="no" &
              allNi$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allNi$Element.x=="ICS1.1" &
              allNi$Type.x!="Grab - Filtered" &
              allNi$aStanValid=="yes" &
              allNi$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allNi$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allNi$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allNi$Site.x[FIMD])

niPlot <- tdiffPlotFun(allNi, "Nickel", 1000, FIMD, FIMT)
##niPlot

## Post Plot
plotly_POST(niPlot, filename = "NickelNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Selenium ######################################
## Index data
FIMD <- which(allSe$Type.x=="Grab - Filtered" &
              allSe$aStanValid!="no" &
              allSe$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allSe$Element.x=="ICS1.1" &
              allSe$Type.x!="Grab - Filtered" &
              allSe$aStanValid=="yes" &
              allSe$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allSe$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allSe$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allSe$Site.x[FIMD])

sePlot <- tdiffPlotFun(allSe, "Selenium", 10, FIMD, FIMT)
##sePlot

## Post Plot
plotly_POST(sePlot, filename = "SeleniumNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Silver ######################################
## Index data
FIMD <- which(allAg$Type.x=="Grab - Filtered" &
              allAg$aStanValid!="no" &
              allAg$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allAg$Element.x=="ICS1.1" &
              allAg$Type.x!="Grab - Filtered" &
              allAg$aStanValid=="yes" &
              allAg$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allAg$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allAg$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allAg$Site.x[FIMD])

agPlot <- tdiffPlotFun(allAg, "Silver", 1000, FIMD, FIMT)
##agPlot

## Post Plot
plotly_POST(agPlot, filename = "SilverNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

############################################################################
############################## Zinc ########################################
## Index data
FIMD <- which(allZn$Type.x=="Grab - Filtered" &
              allZn$aStanValid!="no" &
              allZn$Coldate>as.POSIXct("2010-01-01 0:00"))
FIMT <- which(allZn$Element.x=="ICS1.1" &
              allZn$Type.x!="Grab - Filtered" &
              allZn$aStanValid=="yes" &
              allZn$Coldate>as.POSIXct("2010-01-01 0:00"))

funName <- paste(sort(unique(allZn$Analyte.x))[1],"Fun",sep="")
siteNamesT <- sapply(FUN=returnSiteName, X=allZn$Site.x[FIMT])
siteNamesD <- sapply(FUN=returnSiteName, X=allZn$Site.x[FIMD])

znPlot <- tdiffPlotFun(allZn, "Zinc", 250, FIMD, FIMT)
##znPlot

## Post Plot
plotly_POST(znPlot, filename = "ZincNov2016")
rm(FIMD, FIMT, funName, siteNamesT, siteNamesD)

