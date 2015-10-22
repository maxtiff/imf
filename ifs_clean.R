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
ifs_raw          <- read.csv("data/ifs.csv", header = T,na.string=c("NA",""," "),
                             check.names = T)

## Assign names for ifs and ifs data frames; annual, monthly, and quarterly
ifs_dataframes   <- c('ifs_annual','ifs_monthly','ifs_quarterly')
freqs            <- c('a','m','q')
ifs              <- cleaner(ifs_raw, ifs_countries, ifs_indicators, break_text)

## Extract series qualitative data columns; Extract observation columns to break
## into frequencies, recombine
ifs_meta         <- ifs[,(1:5)]
ifs_obs          <- ifs[,(6:ncol(ifs))]

## Subset data frames based on frequency
for (i in 1:length(ifs_dataframes)) {
  assign(ifs_dataframes[i],frequenter(frequency = freqs[i], metadata_frame = ifs_meta,
                                      observations_frame = ifs_obs))
}

## Subset data by country and order by indicator for each frequency, melt
## resutlant data frame to tall/skinny format and write to output file.
for (i in 1:length(freqs)) {
  for (j in 1:length(ifs_fred_nat)) {
    # Identify country to work with for iteration
    country_name <- paste(keys(ifs_fred_nat)[j],freqs[i],sep="")

    # Subset by frequency
    frame        <- eval(parse(text=ifs_dataframes[i]))

    # Subset by country
    assign(country_name,frame[frame$Country.Name == ifs_countries[j],])

    for(k in 1:length(ifs_fred_ind)) {
      # Extract components to build FRED series id
      source_id  <- keys(ifs_fred_ind)[k]
      unit       <- unit_assigner(source_id)
      seasonality <- season_assigner(source_id)

      # Combine source_id, unit, and seasonality into FRED series id.
      series  <- paste(values(ifs_fred_ind,USE.NAMES=FALSE)[k],country_name,unit,
                          seasonality,sep="")

      # Get frame that has been subsetted by country
      frame      <- get(country_name)

      # Subset into discrete time series
      assign(series,frame[frame$Indicator.Code == keys(ifs_fred_ind)[k],])

      # Get subsetted time series
      frame      <- get(series)

      # Check if data frame is populated with data
      if(is.data.frame(frame) & nrow(frame)>0) {
        # Create tall/skinny time series data frame
        assign(series,
               suppressWarnings(melt(frame, id.vars=c(colnames(frame[,(1:5)])),
                                     na.rm=T,variable.name = 'date',
                                     value.name = series)))

        # Write output file
        write.table(get(series)[,6:7],file=paste('output',series,sep='\\'),
                    quote=F,row.names=F)
      } else {
        # remove(get(indicator))
      }
    }
  }
}
