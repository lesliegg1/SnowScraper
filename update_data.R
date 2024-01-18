# LGG January 2024 - SNOTEL data scraping script
# run this to script to update SNOTEL datasets
# this script runs on a cronjob on OPENCPU at noon daily
# please set your working directory to the location of this script "setwd(SnowScraper/)"
# it will add data since the last date in the exiting datasets in the zdata/ folder
# it will create zdata/ if it does not yet exist and scrape data back to 1972
# NOTE: the number of rows to skip ("skippers") may need manual updates from year to year

library(tidyr) #
library(dplyr)#
library(shiny)#

# states to scrape
state <-    c("AK","AZ","CA","CO","ID","MT","NM","NV","OR","UT","WA","WY")
statelc <-  c("ak","az","ca","co","id","mt","nm","nv","or","ut","wa","wy")
# this is the number of lines that are skipped = line number BEFORE the headers, states in alphabetical order
#skippers2018 <- c(115,  77,  88,  169, 137,146,  82, 107,  135, 185, 130, 143) #NOTE: THESE DIFFER FOR CO & MT FROM dataprep; add 2?
#skippers2020 <- c(116,  77,  88,  169, 136,143,  82, 110,  135, 186, 129, 145)
skippers2021 <- c(132,  82,  93,  174, 144, 151, 88, 115,  140, 194, 135, 148)
skippers2022 <- c(134,  82,  94,  173, 144, 151, 88, 115,  140, 194, 135, 148)
skippers <- c(134,  82,  94,  173, 144, 154, 88, 116,  141, 196, 135, 147)

datadir <- "zdata/"

SNTLmeas <- c("Tavg","Acc_Precip","Snow_Depth","SWE",
              "Tsoil_8")
SNTLname <- c("Daily Average Temperature (F)","Accumulated Precipitation (in.)",
              "Snow Depth (in.)","Snow Water Equivalent (in.)","Soil Temperature at 8 in. (F)")

# i is the state number
for(i in 1:length(state)){

  print(paste("Scraping data for state", state[i]))
  
  # Read data
  if(dir.exists(datadir)){
    searchstring <- paste0("out",i,"_")
    file.names <- dir(datadir, pattern =searchstring)
    
    sites <- list()
    for(e in 1:length(file.names)){
      outfilename <- paste0(datadir,searchstring,e,".csv")
      if(e == 1){
        out3 <- read.csv(outfilename, header=TRUE)
        sites[[e]] <- unique(out3$Site)
      }else{
        temparr7 <- read.csv(outfilename, header=TRUE)
        sites[[e]] <- unique(temparr7$Site)
        out3 <- bind_rows(out3, temparr7)  
      }
    }
    out3$Date <- as.Date(out3$Date)
    out3 <- subset(out3, !is.na(Date))
    if("X" %in% names(out3)) out3 <- out3[,-c(1)]
    
    out3$SiteNum <- as.factor(out3$SiteNum)
    
    # scrape since last update
    lastdate <- max(out3$Date) # read the last day it was scraped
  }else{
    lastdate <- "1972-10-01"
  }
  print(paste("Date last scraped:", lastdate))
  if(Sys.Date() - lastdate > 1){
    print("Scraping new data")
    urlname <- paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/state=%22",statelc[i],"%22%20AND%20network=%22SNTL%22%20AND%20outServiceDate=%222100-01-01%22%7Cname/",lastdate+1,",CurrentCYEnd/stationId,name,WTEQ::value,TAVG::value,PREC::value,SNWD::value,STO:-8:value?fitToScreen=false")
    
    # previous dates - edit by hand to add custom dates
    #urlname <- paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/state=%22",statelc[i],"%22%20AND%20network=%22SNTL%22%20AND%20outServiceDate=%222100-01-01%22%7Cname/2022-01-01,2023-12-31/stationId,name,WTEQ::value,TAVG::value,PREC::value,SNWD::value,STO:-8:value?fitToScreen=false")
    
    
    # Scrape for this calendar year:
    temparr <- read.csv(url(urlname),header=TRUE, skip=skippers[i])
    temparr <- temparr[,order(colnames(temparr))]
    
    # find date column & move it first
    col_idx <- grep("Date", names(temparr))
    temparr <- temparr[, c(col_idx, (1:ncol(temparr))[-col_idx])]
    temparr[,1] <- as.Date(temparr$Date)
    
    temparr2 <- data.frame(matrix(ncol=0,nrow=0))
    ndates <- dim(temparr)[1]
    nsta <- (dim(temparr)[2]-1)/length(SNTLmeas)
    
    SNTLsta <- ""
    SiteNum <- ""
    nums <- seq(2,length(temparr),by=length(SNTLmeas))
    subsnames <- colnames(temparr)[nums]
    for(m in 1:nsta){
      foo <- subsnames[m]
      foo <- sub('\\.\\.[0-9][0-9]', 'ZZZ', foo)
      SNTLsta[m] <- gsub("ZZZ.*","",foo)
      test <- as.numeric(sub('.*(\\d{4}).*', '\\1', subsnames[m]))
      if(is.na(test) == "TRUE"){
        SiteNum[m] <- as.numeric(sub('.*(\\d{3}).*', '\\1', subsnames[m]))
      } else {
        SiteNum[m] <- test
      }
    }
    
    for(j in 1:nsta){
      temparr3 <- data.frame(temparr$Date)
      temparr3$Site <- rep(SNTLsta[j],ndates)
      temparr3$SiteNum <- rep(SiteNum[j],ndates)
      temparr3$State <- rep(state[i],ndates)
      for(k in 1:length(SNTLmeas)){
        temparr3[,k+4] <- temparr[,((j-1)*length(SNTLmeas)+k+1)]
      }
      temparr2 <- bind_rows(temparr2, temparr3)
    }
    colnames(temparr2) <- c("Date","Site","SiteNum","State",SNTLmeas)
    
    # Remove rows of all NA for a given entry
    temparr2 <- temparr2[rowSums(is.na(temparr2[,5:9])) != ncol(temparr2[,5:9]),] 
    # make sure temparr2 types line with out3
    temparr2$Site <- as.factor(temparr2$Site)
    temparr2$SiteNum <- as.factor(temparr2$SiteNum)
    temparr2$State <- as.factor(temparr2$State)
    
    if(dir.exists(datadir)){
      out2 <- bind_rows(out3, temparr2)
    }else{
      out2 <- temparr2
      dir.create(datadir)
    }
    # add doy and water-year doy field
    out2$doy <- as.numeric(strftime(out2$Date, format = "%j"))
    out2$wateryear <- as.numeric(format(as.Date(out2$Date, format="%d/%m/%Y"),"%Y"))
    out2$waterdoy <- out2$doy-275
    out2$wateryear[out2$waterdoy >= 0] <- out2$wateryear[out2$waterdoy >= 0]+1
    out2$waterdoy[out2$waterdoy < 0] <- out2$waterdoy[out2$waterdoy < 0]+366
    out2$juliand <- julian(out2$Date)
    out2$wateryear <- as.factor(out2$wateryear)
    out2$Date <- as.factor(out2$Date)
    
    out2$Site <- as.factor(out2$Site)
    out2$SiteNum <- as.factor(out2$SiteNum)
    out2$State <- as.factor(out2$State)
    
    out2$doy <- as.integer(out2$doy)
    out2$waterdoy <- as.integer(out2$waterdoy)
    out2$juliand <- as.integer(out2$juliand)
    
    for(e in 1:length(file.names)){
      write.csv(file=paste0(datadir, "out", i, "_", e, ".csv"), out2 %>% filter(Site %in% sites[[e]]), row.names=FALSE)
    }
  }else{
    print("No new data")
  }
}
