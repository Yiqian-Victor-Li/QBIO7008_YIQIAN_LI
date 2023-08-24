library("tidyverse")

Dragon_data_full <- read_csv("Test_data_1.csv")

colnames(Dragon_data_full)

Dragon_data_full <- rename(Dragon_data_full, c("survey_id" = "Survey ID", "sighting_id" = "Sighting ID", "field_season" = "Field Season", 
                                               "body_temp" = "Body Temp (Celcius)", "Tasting_The_Ground" = "Tasting the Ground", "Tail_Slap" = "Tail-Slap",
                                                "Head_Bob_Slow" = "Head-Bob Slow", "Head_Bob_Fast" = "Head-Bob Fast", "Head_Bob" = "Head-Bob",
                                               "Arm_Wave_Left" = "Arm-Wave Left", "Arm_Wave_Right" = "Arm-Wave Right", "Arm_Wave" = "Arm-Wave",
                                               "Fighting_c" = "Fighting (contact)", "Fighting_nc" = "Fighting (non-contact)",
                                               "recorder" = "Created by", "last_modified" = "Last modified"
                                               ))

## Exclude all data without a id, name, body temperature measurement or disease status:
Dragon_dat <- Dragon_data_full |> drop_na(body_temp, survey_id, sighting_id, Name, Diseased)


## Environmental variables are: field_season and location? (but they are not gradient)
unique(Dragon_dat$Location)

unique(Dragon_dat$field_season) 
# How long is one season? 
unique(filter(Dragon_dat, field_season == "Field Season 13")$Date)
unique(filter(Dragon_dat, field_season == "Field Season 1")$Date)
# Note: Seasons are not equally spread

# Dis-assemble day/month/year
Dragon_dat_clean <- separate(Dragon_dat, "Date", c("Day", "Month", "Year"), sep = "/")

# Make the dataset adult individual only:
unique(Dragon_dat_clean$Age)
Dragon_dat_clean <- filter(Dragon_dat_clean, Age == "Adult")

# Filter out unrealistic body temperatures:

Dragon_dat_clean <- filter(Dragon_dat_clean, body_temp > 0 & body_temp < 50)

# Keeping columns that are relevant:
Dragon_dat_clean <- select(Dragon_dat_clean, c(survey_id, sighting_id, Day, Month, Year, 
                                               field_season, Time, Lat, Long, 
                                               Name, Sex, Diseased, Age,
                                               Location, body_temp))

# Paste in the correct latitude and longitude
Dragon_dat_clean$Lat <- paste(-27.462, Dragon_dat_clean$Lat, sep="")
Dragon_dat_clean$Long <- paste(153.019, Dragon_dat_clean$Long, sep="")

# Changing columns into factors and numerics 
str(Dragon_dat_clean)

Dragon_dat_clean[,c(1,2,6,7,10,11,12,14)] <- sapply(Dragon_dat_clean[,c(1,2,6,7,10,11,12,14)], as.factor)
Dragon_dat_clean[,c(3,4,5,8,9,15)] <- sapply(Dragon_dat_clean[,c(3,4,5,8,9,15)], as.numeric)


df_occurence_count <- as.data.frame(table(Dragon_dat_clean$Name))
df_occurence_count$Var1 <- as.character(df_occurence_count$Var1)

Dragon_dat_clean <- left_join(Dragon_dat_clean, df_occurence_count, by = c("Name" = "Var1"))
Dragon_dat_clean <- Dragon_dat_clean |> rename(total_number_recorded = Freq) 
Dragon_dat_clean$total_number_recorded <- as.numeric(Dragon_dat_clean$total_number_recorded) 
str(Dragon_dat_clean)

# Some plot checking:
ggplot(Dragon_dat_clean, aes(body_temp))+geom_histogram() # roughly normally distributed
mean(Dragon_dat_clean$body_temp) # 27.49197
ggplot(Dragon_dat_clean, aes(y = body_temp, x = Diseased)) + geom_point() # may be some differences
ggplot(Dragon_dat_clean, aes(y = body_temp, x = Sex)) + geom_point() # hard to tell if it mattered
ggplot(Dragon_dat_clean, aes(y = body_temp, x = Location)) + geom_point() # may be some differences
ggplot(Dragon_dat_clean, aes(y = body_temp, x = total_number_recorded)) + geom_point() # hard to tell if it mattered



# Follow tutorial Tom Houslay:
library(lme4)
library(MCMCglmm)
library(tidyverse)
library(broom)
library(broom.mixed)
library(nadiv)

# Make a model where name nested within Location
lmer_1 <- lmer(body_temp ~ Diseased + Sex + total_number_recorded +  (1|Location/Name), data = Dragon_dat_clean) 
summary(lmer_1)
plot(lmer_1)
qqnorm(residuals(lmer_1))
hist(residuals(lmer_1))


# Testing for repeatablity: = Variance/(Variance + Variance residual) in random effect
rep_1 <- tidy(lmer_1, effects = "ran_pars", scales = "vcov") %>%
    select(group, estimate) %>%
    spread(group, estimate) %>%
    mutate(repeatability = Location/(Location + Residual))

rep_1 ## ? Only 3% repeatability? How to test for nested random effect?



## Fitting models using brm package
library(brms)

lmer_2 <- brm(body_temp ~ Diseased + Sex + total_number_recorded +  (1|Location/Name), 
              data = Dragon_dat_clean, family = gaussian(), 
              prior = c(set_prior("normal(27,10)", class = "b"),
                        set_prior("cauchy(0,2)", class = "sd")),
              control = list(adapt_delta = 0.95))

summary(lmer_2)
