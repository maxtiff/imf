#### IMF Data Cleaning and Compatability Script

### Intitialization
setwd("~/IMF/fsi_isf")

## Load required libraries
library(data.table)
library(reshape2)
library(hash)

## Source all required scripts.
required_scripts <- c('fsi_ifs_functions.R','corpus.R')
sapply(required_scripts, source, .GlobalEnv)

## Source data files
fsi_raw            <- read.csv("data/fsi.csv", header = T,na.string=c("NA",""," "),check.names = T)

## Assign names for ifs and fsi data frames; annual, monthly, and quarterly
fsi_dataframes     <- c('fsi_annual','fsi_monthly','fsi_quarterly')
freqs              <- c('a','m','q')
fsi                <- cleaner(fsi_raw, values(fsi_fred_nat, USE.NAMES=F), keys(fsi_fred_ind), break_text)

## Extract series qualitative data columns; Extract observation columns to break
## into frequencies, recombine
fsi_meta           <- fsi[,(1:5)]
fsi_obs            <- fsi[,(6:ncol(fsi))]

## Subset data frames based on frequency
for (i in 1:length(fsi_dataframes)) {
  assign(fsi_dataframes[i],frequenter(frequency = freqs[i], metadata_frame = fsi_meta,
                                      observations_frame = fsi_obs))
}

## Subset data by country and order by indicator for each frequency, melt
## resutlant data frame to tall/skinny format and write to output file.
# Work with each frequency frame
for (i in 1:length(freqs)) {
  # Work with data country-by-country
  for (j in 1:length(fsi_fred_nat)) {
    # Identify country to work with for iteration
    country_name   <- paste(keys(fsi_fred_nat)[j],freqs[i],sep="")

    # Working frame is a frequency subset
    frame          <- eval(parse(text=fsi_dataframes[i]))
    assign(country_name,frame[frame$Country.Name == values(fsi_fred_nat,USE.NAMES=F)[j],])

    # Work through each data indicator
    for(k in 1:length(fsi_fred_ind)) {
      # Extract components to build FRED series id
      source_id    <- keys(fsi_fred_ind)[k]
      unit         <- unit_assigner(source_id)
      seasonality  <- season_assigner(source_id)

      # Combine source_id, unit, and seasonality into FRED series id.
      series       <- paste(values(fsi_fred_ind,USE.NAMES=FALSE)[k],country_name,unit,
                            seasonality,sep="")

      # Get frame that has been subsetted by country
      frame        <- get(country_name)

      # Subset into discrete time series
      assign(series,frame[frame$Indicator.Code == keys(fsi_fred_ind)[k],])

      # Working frame is a indicator subset
      frame        <- get(series)

      # Check if data frame is populated with data
      if(is.data.frame(frame) & nrow(frame)>0) {
        # Create tall/skinny time series data frame
        assign(series,
               suppressWarnings(melt(frame, id.vars=c(colnames(frame[,(1:5)])),na.rm=T,
                                     variable.name = 'date',value.name = series)))
        # Write output file
        write.table(get(series)[,6:7],file=paste('output',series,sep='\\'),quote=F,row.names=F)
      } else {
        # remove(get(indicator))
      }
    }
  }
}