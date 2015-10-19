#### IMF Data Cleaning and Compatability Script

### Intitialization
setwd("~/IMF/fsi_isf")

## Load required libraries
library(data.table)
library(reshape2)
library(plyr)
library(stringr)
library(hash)

## Source all required scripts.
required_scripts <- c('fsi_ifs_functions.R','corpus.R')
sapply(required_scripts, source, .GlobalEnv)

## Source data files
fsi_raw          <- read.csv("data/fsi.csv", header = T,na.string=c("NA",""," "),
                             check.names = T)
ifs_raw          <- read.csv("data/ifs.csv", header = T,na.string=c("NA",""," "),
                             check.names = T)

## Assign names for ifs and fsi data frames; annual, monthly, and quarterly
fsi_dataframes   <- c('fsi_annual','fsi_monthly','fsi_quarterly')

ifs_dataframes   <- c('ifs_annual','ifs_monthly','ifs_quarterly')

freqs            <- c('a','m','q')

fsi              <- cleaner(fsi_raw, fsi_countries, fsi_indicators)

ifs              <- cleaner(ifs_raw, ifs_countries, ifs_indicators)

################################################################################
### FSI

## Extract series qualitative data columns; Extract observation columns to break
## into frequencies, recombine
fsi_meta         <- fsi[,(1:5)]
fsi_obs          <- fsi[,(6:ncol(fsi))]

## Subset data frames based on frequency
for (i in 1:length(fsi_dataframes)) {
  assign(fsi_dataframes[i],frequenter(frequency = freqs[i],
                                      metadata_frame = fsi_meta,
                                      observations_frame = fsi_obs))
}

## Subset data by country and order by indicator for each frequency, melt
## resutlant data frame to tall/skinny format
for (i in 1:length(freqs)) {
  for (j in 1:length(fsi_fred_nat)) {

    country_name <- paste(keys(fsi_fred_nat)[j],freqs[i],sep="")

    # Working frame is a frequency subset
    frame        <- eval(parse(text=fsi_dataframes[i]))

    assign(country_name,frame[frame$Country.Name == fsi_countries[j],])

    for(k in 1:length(fsi_fred_ind)) {
      if(grepl('PT',fsi_fred_ind[[fred_ind[k]]])) {
        unit <- '163'
      } else if(grepl('PA',fsi_fred_ind[[fred_ind[k]]])) {
        unit <- '193'
      } else if(grepl('XDC',fsi_fred_ind[[fred_ind[k]]])) {
        unit <- '189'
      } else if(grepl('XDR',fsi_fred_ind[[fred_ind[k]]])) {
        unit <- '194'
      } else if(grepl('USD',fsi_fred_ind[[fred_ind[k]]])) {
        unit <- '052'
      }

      if(grepl('_SA_',fsi_fred_ind[[fred_ind[k]]])) {
        seasonality <- 'S'
      } else if (!grepl('_S_',fsi_fred_ind[[fred_ind[k]]])) {
        seasonality <- 'N'
      }

      indicator  <- paste(keys(fsi_fred_ind)[k],country_name,unit,seasonality,
                          sep="")

      # Working frame is a country subset
      frame      <- get(country_name)

      assign(indicator,frame[frame$Indicator.Code == fsi_fred_ind[[fred_ind[k]]],
                             ])

      # Working frame is a indicator subset
      frame      <- get(indicator)

      if(is.data.frame(frame) & nrow(frame)>0) {
        assign(indicator,
               suppressWarnings(melt(frame, id.vars=c(colnames(frame[,(1:5)])),
                               na.rm=T,variable.name = 'date',
                               value.name = indicator)))

        write.table(get(indicator)[,6:7],file=paste('output',indicator,sep='\\'),
                    quote=F,row.names=F)

      } else {
        # remove(get(indicator))
      }
    }
  }
}

##########################################################################################
### IFS

## Extract series qualitative data columns; Extract observation columns to break into frequnecies, recombine
ifs_meta <- ifs[,(1:5)]
ifs_obs <- ifs[,(6:ncol(ifs))]

## Drop leading X from column names
ifs_obs_list     <- lapply(colnames(ifs_obs[,grepl('^X',colnames(ifs_obs))]),
                           function(x) substring(x,2))

ifs_obs          <- setnames(ifs_obs, as.character(ifs_obs_list))

for (i in 1:length(ifs_dataframes)) {
  assign(ifs_dataframes[i],frequenter(frequency = freqs[i],metadata_frame = ifs_meta,
                                      observations_frame = ifs_obs))

  assign(ifs_dataframes[i], melt.data.frame(eval(parse(text = ifs_dataframes[i])),
                                            id.vars=c(colnames(eval(parse(text = ifs_dataframes[i]))[,(1:5)])),na.rm=T))
}