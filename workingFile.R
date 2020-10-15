library(tidyverse)
library(readr)

comedians <- read_csv("comedian.csv")

jokes <-
  read_csv(
    "jokes.csv",
    col_types = cols(
      number_in_set = col_integer(),
      set_id = col_integer(),
      start_time = col_character(),
      end_time = col_character(),
      reaction = col_integer(),
      X6 = col_skip(),
      `notes:` = col_skip()
    )
  )


convertTimeToDecimal <- function(time) {
  minsAndSeconds <- str_split(time, ":")
  
  minutes <- as.double(minsAndSeconds[[1]][1])
  secondsFrac <- as.double(minsAndSeconds[[1]][2]) / 60.0
  
  return(minutes + secondsFrac)
}

## first get the total amount of time of all the sets

## transmute must use group by otherwise it will be global to the list avg.
jokesWithTimes <-
  jokes %>% 
  group_by(set_id, number_in_set) %>% 
  transmute(startTime = convertTimeToDecimal(start_time), endTime = convertTimeToDecimal(end_time))

totalSetTimes <-
  jokesWithTimes %>%
  group_by(set_id) %>% 
  summarize(totalTimePerSet = max(endTime))

totalTime = sum(totalSetTimes$totalTimePerSet)

## number of comedians

comedianCount = nrow(comedians)

## number of semesters
uniqueSemesters <-
  comedians %>%
  group_by(year) %>%
  summarise(n = n())

semestersCount = nrow(uniqueSemesters)

## ======= DEMOGRAPHICS =======

# dominant handedness vs num comedians

comedianAvgCrowdResponse <-
  jokes %>%
  group_by(set_id) %>%
  summarize(avgResponse = mean(reaction))

comedianAvgCrowdResponseAndDemographic <- merge(comedianAvgCrowdResponse, comedians, by.x = "set_id", by.y = "id")

dominantHandVsCrowdResp <-
  comedianAvgCrowdResponseAndDemographic %>%
  group_by(dominant_hand) %>%
  summarise(avgResponse = mean(avgResponse))
