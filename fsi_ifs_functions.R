cleaner     <- function(data, countries, indicators,text) {
#   Cleans a dataframe of extraneous rows, blank cells, and unused countries and indicators.
#
#   Inputs:  A raw dataframe, a list of countires, a list of indicators
#
#   Returns: A cleaned dataframe

  data_name   <- deparse(substitute(data))

  ## Comparability text
  break_text  <- text

  ## Subset by country
  data        <- data[data$Country.Name %in% countries,]
    ## Subset by indicator
  data        <- data[data$Indicator.Code %in% indicators,]

  ## Convert to data.table
  data        <- as.data.table(data)

  if (grepl('^fsi',data_name)) {
    ## Remove columns without data
    data      <- data[,(6:21):=NULL]

  } else if (grepl('^ifs', data_name)) {
    ## Remove base year column
    date      <- data[,Base.Year:=NULL]

  }
  ## Revert to data.frame
  data        <- as.data.frame(data)

  ## Remove break text
  data        <- as.data.frame(sapply(data,function(x) gsub(break_text,NA,x)))


  return(data)
}


frequenter  <- function(frequency,metadata_frame,observations_frame) {
  #   Creates a dataframe of certain frequency from larger data frame
  #
  #   Inputs:  Frequency of Q, M, or A, dataframe containing metadata, & dataframe
  #            containing observations.
  #
  #   Returns: A dataframe of multiple data series in desired frequency. If a series is not
  #            available in a certain frequency, it will not be in the dataframe

  frequency    <- frequency_match(frequency)

  frame        <- observations_frame[,grepl(frequency,colnames(observations_frame))]

  frame_l      <- lapply(colnames(frame[,grepl('^X',colnames(frame))]),function(x) substring(x,2))

  frame_l      <- lapply(frame_l, function (x) if(grepl('\\d{4}M\\d{1,2}',x)) {
                                                 if (nchar(x) == 6) sub('M','.0',x)
                                                 else if (nchar(x) == 7) sub ('M', '.', x)}
                                               else if (grepl('\\d{4}Q\\d',x)) {
                                                 sub('Q','.',x)}
                                               else {x})

  frame        <- setnames(frame,as.character(frame_l))

  frame        <- cbind.data.frame(metadata_frame,frame)

  frame        <- frame[apply(frame[,(6:ncol(frame))],1,function(x)any(!is.na(x))),]

  return(frame)

}


frequency_match <- function(frequency) {
#   Creates a regular expression for a certain frequency to use while subsetting dataframe.
#
#   Inputs:  Frequency of Q, M, or A
#
#   Returns: A regular expression used to check for frequency and subset dataframe.

  frequency <- tolower(frequency)

  if(frequency == 'm') {
    freq_match <- '\\d{4}M\\d{1,2}$'
  } else if(frequency =='q') {
    freq_match <- '\\d{4}Q\\d$'
  } else if(frequency == 'a') {
    freq_match <- '\\d{4}$'
  } else {
    stop('Frequency can be one of monthly, quarterly or annually; denoted by M, Q, or A, respectively ')
  }

  return(freq_match)
}

unit_convert <- function(indicator) {


  if(grepl('PT',indicator)) {
    unit <- '163'
  } else if(grepl('PA',indicator)) {
    unit <- '193'
  } else if(grepl('XDC',indicator)) {
    unit <- '189'
  } else if(grepl('XDR',indicator)) {
    unit <- '194'
  } else if(grepl('USD',indicator)) {
    unit <- '052'
  } else {
    stop('Unit cannot be determined from function input.')
  }
}








