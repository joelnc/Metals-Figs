## Collection of funs to compute thresholds

## Cadmium
CadmiumFun <- function(hd) {
    chronT <- 2
    if (is.na(hd)) {
      chron <- (1.101672-(log(25))*(0.041838))*(exp(0.7998*(log(25))-4.4451))
        ac <- (1.136672-(log(25)*0.041838))*(exp(0.9151*log(25)-3.1485))
        chronV <- "no"
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- (1.101672-(log(25)*0.041838))*(exp(0.7998*log(25)-4.4451))
        ac <- (1.136672-(log(25)*0.041838))*(exp(0.9151*log(25)-3.1485))
        chronV <- "yes"
        acV <- "yes"
    }
    else {
        chron <- (1.101672-(log(hd)*0.041838))*(exp(0.7998*log(hd)-4.4451))
        ac <- (1.136672-(log(hd)*0.041838))*(exp(0.9151*log(hd)-3.1485))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Chromium III
ChromiumFun <- function(hd) {
    chronT <- 50
    if (is.na(hd)) {
        chron <- 0.86*(exp(0.8190*log(25)+0.6848))
        ac <- 0.316*(exp(0.8190*log(25)+3.7256))
        chronV <- "no"
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- 0.86*(exp(0.8190*log(25)+0.6848))
        ac <- 0.316*(exp(0.8190*log(25)+3.7256))
        chronV <- "yes"
        acV <- "yes"
    }
    else {
        chron <- 0.86*(exp(0.8190*log(hd)+0.6848))
        ac <- 0.316*(exp(0.8190*log(hd)+3.7256))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Copper
CopperFun <- function(hd) {
    chronT <- 7
    if (is.na(hd)) {
        chron <- 0.96*(exp(0.8545*log(25)-1.702))
        ac <- 0.96*(exp(0.9422*log(25)-1.7))
        chronV <- "no"
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- 0.96*(exp(0.8545*log(25)-1.702))
        ac <- 0.96*(exp(0.9422*log(25)-1.7))
        chronV <- "yes"
        acV <- "yes"
    }
    else {
        chron <- 0.96*(exp(0.8545*log(hd)-1.702))
        ac <- 0.96*(exp(0.9422*log(hd)-1.7))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Lead
LeadFun <- function(hd) {
    chronT <- 25
    if (is.na(hd)) {
        chron <- (1.46203-(log(25)*0.145712))*(exp(1.273*log(25)-4.705))
        ac <- (1.46203-(log(25)*0.145712))*(exp(1.273*log(25)-1.46))
        chronV <- "no"
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- (1.46203-(log(25)*0.145712))*(exp(1.273*log(25)-4.705))
        ac <- (1.46203-(log(25)*0.145712))*(exp(1.273*log(25)-1.46))
        chronV <- "yes"
        acV <- "yes"
    }
    else {
        chron <- (1.46203-(log(hd)*0.145712))*(exp(1.273*log(hd)-4.705))
        ac <- (1.46203-(log(hd)*0.145712))*(exp(1.273*log(hd)-1.46))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Nickel
NickelFun <- function(hd) {
    chronT <- 88
    if (is.na(hd)) {
        chron <- 0.997*(exp(0.8460*log(25)+0.0584))
        ac <- 0.998*(exp(0.8460*log(25)+2.255))
        chronV <- "no"
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- 0.997*(exp(0.8460*log(25)+0.0584))
        ac <- 0.998*(exp(0.8460*log(25)+2.255))
        chronV <- "yes"
        acV <- "yes"
    }
    else {
        chron <- 0.997*(exp(0.8460*log(hd)+0.0584))
        ac <- 0.998*(exp(0.8460*log(hd)+2.255))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Zinc
ZincFun <- function(hd) {
    chronT <- 50
    if (is.na(hd)) {
        chron <- 0.986*(exp(0.8473*log(25)+0.884))
        ac <- 0.978*(exp(0.8473*log(25)+0.884))
        chronV <- "no"
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- 0.986*(exp(0.8473*log(25)+0.884))
        ac <- 0.978*(exp(0.8473*log(25)+0.884))
        chronV <- "yes"
        acV <- "yes"
    }
    else {
        chron <- 0.986*(exp(0.8473*log(hd)+0.884))
        ac <- 0.978*(exp(0.8473*log(hd)+0.884))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Silver
SilverFun <- function(hd) {
    chronT <- NA
    if (is.na(hd)) {
        chron <- NA
        ac <- 0.85*(exp(1.72*log(25)-6.59))
        chronV <- NA
        acV <- "no"
    }
    else if (hd < 25) {
        chron <- NA
        ac <- 0.85*(exp(1.72*log(25)-6.59))
        chronV <- NA
        acV <- "yes"
    }
    else {
        chron <- NA
        ac <- 0.85*(exp(1.72*log(hd)-6.59))
        chronV <- NA
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Arsenic
ArsenicFun <- function(hd) {
    chronT <- 150
    chron <- 150
    ac <- 340
    chronV <- "yes"
    acV <- "yes"
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Selenium
SeleniumFun <- function(hd) {
    chronT <- 5
    chron <- 5
    ac <- 5
    chronV <- "yes"
    acV <- "yes"
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

## Beryllium
BerylliumFun <- function(hd) {
    chronT <- 6.5
    chron <- 6.5
    ac <- 65
    chronV <- "yes"
    acV <- "yes"
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}
