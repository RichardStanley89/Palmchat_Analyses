# Load in behavior data


library(dplyr)
library(tidyverse)



# Focus on specific behaviors for each of the models

# aggregate to get a count of the feeding events per video session.
# each row has a count, date-julian, time of day
# group by video session first, then group by individual
# date is the same = julian day stays the same
# time of day is just morning or evening
# incorporates individual level attributes
# random effect has to be the nest entrance
# Count - number of observations per individual, and per nest entrance


D <- read.csv("processed_data/Behavior_data.csv")


# Below we will make data frames with each of the behavioral variables of interest.
# There are a bunch of them.

# select "arrive" behavior as response variable

S <- D %>% 
  filter(Behavior == "arrive") %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration) %>% 
  mutate(log_effort = log(Duration)) %>%
  # mutate(log_effort = Duration) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))




hist(S$n)

# select "enter" behavior as response variable

S <- D %>%
  filter(Behavior == "enter") %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration) %>% 
  mutate(log_effort = log(Duration)) %>%
  # mutate(log_effort = Duration) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))


# select "build" behavior as response variable

S <- D %>% 
  filter(Behavior == "build") %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration) %>% 
  mutate(log_effort = log(Duration)) %>%
  # mutate(log_effort = Duration) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))

# Select build DURATION as response:

behaviorfrequency <- D %>% group_by(D$Observation.id, D$Subject) %>% 
  filter(Behavior == "build") %>%
summarise(duration_behavior = sum(Duration..s.), count = n(),)

hist(log(behaviorfrequency$duration_behavior))

## try a log transformation??

# behaviorfrequency <- D %>% group_by(D$Observation.id, D$Subject) %>% 
#  filter(Behavior == "build") %>%
#  summarise(duration_behavior = log(sum(Duration..s.)), count = n(),)

S <- S %>% mutate(duration_behavior = behaviorfrequency$duration_behavior)

# Select "visible" as a response:

S <- D %>% 
  filter(Behavior == "visible") %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration) %>% 
  mutate(log_effort = log(Duration)) %>%
  # mutate(log_effort = Duration) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))

# Select "duration visible" as a response:

behaviorfrequency <- D %>% group_by(D$Observation.id, D$Subject) %>% 
  filter(Behavior == "visible") %>%
  summarise(duration_behavior = sum(Duration..s.), count = n(),)

S <- S %>% mutate(duration_behavior = behaviorfrequency$duration_behavior)




# Select "BringFood" as a response:

S <- D %>% 
  filter(BringFood == "BringFood") %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration) %>% 
  mutate(log_effort = log(Duration)) %>%
  # mutate(log_effort = Duration) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))


# create binary response: helper vs non-helper

helpers <- S %>% mutate(helper = 1)

helpers <- data.frame(helpers$Subject, helpers$helper)

helpers <- helpers %>% rename(Subject = helpers.Subject, helper = helpers.helper)

joined <- left_join(D, helpers, by = 'Subject')

joined <- joined %>% mutate(helper = replace_na(helper, 0))

                   
S <- joined %>% 
  count(Observation.id, Nest.Entrance, Subject, Duration, helper) %>% 
  mutate(log_effort = log(Duration)) %>%
  # mutate(log_effort = Duration.x) %>% 
  mutate(tod = str_extract(Observation.id, "(Mor|Ev)$"))


#########


# Now we can run the models with any of the above selections....

# join filtered data with attributes:

attributes_clean2 <- read.csv("processed_data/attributes_clean2.csv")

Joined <- inner_join(attributes_clean2, S, by = "Subject")

Joined <- Joined %>% mutate(molecular_sex = as.factor(Joined$molecular_sex))

Joined <- Joined %>% mutate(age = as.factor(Joined$age))

str(Joined)


library(lme4)

# Does time of day influence the "point" behavior?

model <- glmer(n ~ tod + (1|Subject) + offset(log_effort),
      data = S,
      family = poisson)


summary(model)


# Does time of day influence the "state" behavior?

model <- lmer(log(duration_behavior) ~ tod + (1|Subject) + offset(log_effort),
               data = Joined)

model <- glmer(duration_behavior ~ tod + (1|Subject) + offset(log_effort),
              data = Joined,
              family = Gamma(link = "inverse"))

model

summary(model)

?glmer


# remove NAs to prepare molecular sex column:


Joined <- Joined %>%
     mutate(molecular_sex = na_if(molecular_sex, ""))

Joined <- Joined %>% drop_na(molecular_sex)


# Does sex influence the "point" behavior?




model <- glmer(n ~ molecular_sex + tod + (1|Subject) + offset(log_effort),
               data = Joined,
               family = poisson)


summary(model)

exp(0.4645)



# Does sex influence the state behavior?


model <- glmer(duration_behavior ~ molecular_sex + (1|Subject) + offset(log_effort),
               data = Joined,
               family = gaussian)




# Does sex influence the binary category?

hist(S$helper)


model <- glmer(helper ~ molecular_sex + (1|Subject) + offset(log_effort),
               data = Joined,
               family = binomial(link = "logit"))

table(S$helper)

# Let's prepare the age variable:

# remove NAs to prepare age column (good bit of data loss):


Joined <- Joined %>%
  mutate(age = na_if(age, ""))

Joined <- Joined %>% drop_na(age)


# if we want, we could simplify "age" to "adult and "juvenile"
# Joined <- Joined %>% mutate(age2 = case_when(age == "FCF" ~ "Immature",
       #                                      age == "FAJ" ~ "Adult",
       #                                      age == "juvenile" ~ "Immature",
       #                                      age == "DCB" ~ "Adult",
         #                                    TRUE ~ "OTHER"))



# Does age influence the point behavior?


model <- glmer(n ~ age + (1|Subject) + offset(log_effort),
               data = Joined,
               family = poisson)

summary(model)


# Does age influence the state behavior?

model <- glmer(duration_behavior ~ age + (1|Subject) + offset(log_effort),
               data = Joined,
               family = gaussian)



# Check significance
summary(model)


install.packages("sjPlot")
library(sjPlot)

tab_model(model)



library(DHARMa)

simulationOutput <- simulateResiduals(model)
plot(simulationOutput)
