# Load in behavior data

standardize_strings <- function(strings) {
  sapply(strings, function(s) {
    # Extract components using regex
    # Handle different patterns
    
    # Pattern 1: Day_Month_Year_PTZ_TOD
    if (grepl("^\\d+_[A-Za-z]+_\\d{4}_PTZ", s)) {
      parts <- str_match(s, "^(\\d+)_([A-Za-z]+)_(\\d{4})_(PTZ[^_]+)_(.+)$")
      day <- parts[2]
      month <- parts[3]
      year <- parts[4]
      cam <- parts[5]
      tod <- parts[6]
    }
    # Pattern 2: PTZ_Month_Day_Year_TOD
    else if (grepl("^PTZ", s)) {
      parts <- str_match(s, "^(PTZ[^_]+)_([A-Za-z]+)_?(\\d+)_?(\\d{4})_(.+)$")
      cam <- parts[2]
      month <- parts[3]
      day <- parts[4]
      year <- parts[5]
      tod <- parts[6]
    }
    else {
      return(NA)
    }
    
    # Standardize month (abbreviate to 3 letters, capitalize first)
    month <- str_replace(month, "June", "Jun")
    month <- str_replace(month, "July", "July")
    month <- str_replace(month, "August", "August")
    month <- str_replace(month, "Junr", "Jun")
    
    # Standardize TOD
    tod <- str_replace(tod, "Morn?", "Mor")
    tod <- str_replace(tod, "^Ev.*", "Ev")
    
    # Return standardized format
    paste(day, month, year, cam, tod, sep = "_")
  }, USE.NAMES = FALSE)
}

behavior_data <- read.csv("processed_data/Palmchat_Behavior_Data_Nov_15_2025.csv")

library(tidyverse)

behavior_data %>% 
  select(Observation.id) %>% 
  filter(str_detect(Observation.id, "_Morn")) %>% 
  mutate(Observation.id = str_replace(Observation.id, "_Morn", "_Mor"))


# Focus on specific behaviors for each of the models

# aggregate to get a count of the feeding events per video session.
# each row has a count, date-julian, time of day
# group by video session first, then group by individual
# date is the same = julian day stays the same
# time of day is just morning or evening
# incorporates individual level attributes
# random effect has to be the nest entrance
# Count - number of observations per individual, and per nest entrance

glimpse(behavior_data)


A <- behavior_data %>% 
  select(
    Observation.id,
    Observation.date,
    Media.duration..s.,
    Subject,
    Behavior,
    Behavioral.category,
    Modifier..1,
    Behavior.type,
    Start..s.,
    Stop..s.
  )


split_values <- strsplit(A$Media.duration..s., split =";")
split_values <- sapply(split_values, as.numeric)
A <- A %>% mutate(Duration = sapply(split_values, sum))


attributes <- read.csv("rawdata/PalmchatBandingDataAll.csv")

bird_ids <- attributes$Colors

B <- A %>% 
  filter(Subject %in% bird_ids)

Camera_data <- read.csv("rawdata/FoscamCameraScheduleCodedNew.csv", na.strings = c("", "NA"))

unique(Camera_data$Observation.id)
unique(standardize_strings(B$Observation.id))


C <- B %>% 
  select(-Media.duration..s.) %>% 
  mutate(Observation.id = standardize_strings(Observation.id))

D <- Camera_data %>% 
  select(Observation.id, Nest, Nest.Entrance) %>% 
  drop_na(Observation.id) %>% 
  left_join(C, by = join_by(Observation.id)) %>% 
  drop_na(Observation.id, Nest.Entrance, Subject)


D %>% 
  count(Nest.Entrance, Subject)

F <- D %>% 
  filter(Behavior == "arrive") %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration) %>% 
  mutate(log_effort = log(Duration)) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))

hist(F$n)

library(lme4)

model <- glmer(n ~ tod + (1|Subject) + offset(log_effort),
      data = F,
      family = poisson)

# Check significance
summary(model)

library(DHARMa)

simulationOutput <- simulateResiduals(model)
plot(simulationOutput)
