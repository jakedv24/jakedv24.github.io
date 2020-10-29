library(tidyverse)
library(readr)

comedians <- read_csv("comedian.csv")

jokes <-
  read_csv(
    "jokes.csv",
    col_types = cols(
      joke_number_in_set = col_integer(),
      set_id = col_integer(),
      start_time = col_character(),
      end_time = col_character(),
      reaction = col_integer(),
      X6 = col_skip(),
      `notes:` = col_skip()
    )
  )

swear_word <- read_csv("swear_word.csv", 
                       col_types = cols(timestamp = col_character()))

callback <- read_csv("callback.csv", col_types = cols(timestamp = col_character(), 
                                                      X6 = col_skip(), X7 = col_skip()))

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
  group_by(set_id, joke_number_in_set) %>% 
  mutate(startTime = convertTimeToDecimal(start_time), endTime = convertTimeToDecimal(end_time)) %>%
  mutate(totalTime = (endTime - startTime))

avgJokeTime <- mean(jokesWithTimes$totalTime)

setWithNumJokes <-
  jokesWithTimes %>%
  group_by(set_id) %>%
  summarize(numJokes = n())

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

comedianNumJokes <-
  jokes %>%
  group_by(set_id) %>%
  summarize(numJokes = n())

comedianAvgCrowdResponseAndDemographic <- merge(comedianAvgCrowdResponse, comedians, by.x = "set_id", by.y = "id") %>%
  merge(comedianNumJokes, by = "set_id")

dominantHandVsCrowdResp <-
  comedianAvgCrowdResponseAndDemographic %>%
  group_by(dominant_hand) %>%
  summarise(avgResponse = mean(avgResponse))

numberOfSwearWordsBySet <- 
  swear_word %>%
  group_by(set_id) %>%
  summarise(numSwears = n())

comedianAndNumberOfSwearWords <- merge(comedianAvgCrowdResponseAndDemographic, numberOfSwearWordsBySet, by.x = "set_id", by.y = "set_id", all.x = TRUE) %>%
  replace_na(list(numSwears = 0))

numCallbacksBySet <-
  callback %>%
  group_by(set_id) %>%
  summarise(numCallbacks = n())

comdianAndNumCallbacks <- merge(comedianAvgCrowdResponseAndDemographic, numCallbacksBySet, by.x = "set_id", by.y = "set_id", all.x = TRUE) %>%
  replace_na(list(numCallbacks = 0))

## ========== SWEARING =========

swearWordCountByJoke <-
  swear_word %>%
  group_by(set_id, joke_number_in_set) %>%
  summarise(numSwearsInJoke = n())

jokesWithSwearCount <- merge(swearWordCountByJoke, jokes, all.y = TRUE) %>%
  replace_na(list(numSwearsInJoke = 0))

avgResponseByJokeNumberInSet <-
  jokes %>%
  group_by(joke_number_in_set) %>%
  summarise(avgResponse = mean(reaction))

swearWordCountByNumberInSet <-
  jokesWithSwearCount %>%
  group_by(joke_number_in_set) %>%
  summarise(totalSwearsForNumberInSet = sum(numSwearsInJoke)) %>%
  merge(avgResponseByJokeNumberInSet)

swearWords <-
  swear_word %>%
  group_by(word) %>%
  summarise(count = n())

## ========= CALLBACKS =========

numCallbacksPerJoke <-
  callback %>%
  group_by(set_id, from_joke) %>%
  summarise(numCallbacks = n()) %>%
  merge(jokesWithTimes, by.x = c("set_id", "from_joke"), by.y = c("set_id", "joke_number_in_set"), all.y = TRUE) %>%
  replace_na(list(numCallbacks = 0))

avgResponseByNumCallbacks <-
  numCallbacksPerJoke %>%
  group_by(numCallbacks) %>%
  summarise(avgResponse = mean(reaction))

distanceCallbackWithResponse <-
  callback %>%
  mutate(callbackDistance = (from_joke - to_joke)) %>%
  merge(jokesWithTimes, by.x = c("set_id", "from_joke"), by.y = c("set_id", "joke_number_in_set"))

avgResponseCallbackDist <-
  distanceCallbackWithResponse %>%
  group_by(callbackDistance) %>%
  summarise(avgResponse = mean(reaction))

numCallbacksByRootJoke <-
  callback %>%
  group_by(set_id, to_joke) %>%
  summarise(numCallbacksByRoot = n())
