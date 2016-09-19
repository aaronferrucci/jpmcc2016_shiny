
library(httr)
library(XML)
getUrl <- function(bib) {
  url <-
    paste0(
      "https://www.jpmorganchasecc.com/results/search.php",
      "?city_id=14",
      "&series_year=2016",
      "?sub_event_id=561",
      "&year=2016",
      "&search=1",
      "&bib=", bib,
      "&lname=",
      "&fname=",
      "&team_name=",
      "&gender="
    )

  return(url)
}

secondsToTimestr <- function(seconds) {
  hours <- as.integer(seconds / 3600)
  seconds <- seconds - hours * 3600
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60, digits=2)

  minute_prefix <- ifelse(minutes < 10, "0", "")
  minutes <- paste0(minute_prefix, minutes)
  second_prefix <- ifelse(seconds < 10, "0", "")
  seconds <- paste0(second_prefix, seconds)

  time <- paste(hours, minutes, seconds, sep=":")
  return(time)
}


# input: 29:56, 1:07:48, etc
# output: time in seconds
timeToSeconds <- function(time) {
  nums <- sapply(strsplit(time, ":", fixed=T), FUN=as.numeric)
  exps <- length(nums):1 - 1
  mults <- 60^exps
  s_list <- nums * mults
  seconds <- sum(s_list)
  return(seconds)
}

getData <- function() {
  file <- "jpmcc2016.csv"
  if (file.exists(file)) {
    print(paste0("Using cached data from ", file))
    allData <- read.csv(file, stringsAsFactors=FALSE)
  } else {
    print("Loading data from the jpmcc site. This will take a while.")
    allData <- data.frame()
    for (i in 1:12000) {
      url <- getUrl(i)
      resp <- POST(url)
      cont <- content(resp, type="application/x-www-form-urlencoded")
      html <- htmlParse(cont[96][1], asText=TRUE)
      tb <- readHTMLTable(html)
      theData = data.frame()
      if ("results" %in% names(tb)) {
        allData <- rbind(allData, tb$results)
        print(paste0(i, ": yep!"))
      } else {
        print(paste0(i, ": nope"))
      }
    }  
    allData$Name <- as.character(allData$Name)
    allData$Time <- as.character(allData$Time)
    allData$Gender <- as.character(allData$Gender)
    allData$Company <- as.character(allData$Company)

    write.csv(allData, file)
  }

  # Cast some fields to numeric
  allData$Plc <- as.integer(allData$Plc)
  allData$GPlc <- as.integer(allData$GPlc)
  allData$Bib <- as.integer(allData$Bib)

  # time in seconds
  allData$Time.Seconds <- sapply(allData$Time, timeToSeconds)

  return(allData)
}

cleanData <- function(allData) {
  # One row has NAs - summarily drop it.
  has.na <- apply(allData, 1, function(x) any(is.na(x)))
  allData <- allData[!has.na,]

  # One record appears to have an incorrect Plc. Manually fix it.
  # This causes a duplicate "place" data point, which might be a problem.
  allData[allData$Plc == 4135,]$Plc <- 2442

  return(allData)
}

