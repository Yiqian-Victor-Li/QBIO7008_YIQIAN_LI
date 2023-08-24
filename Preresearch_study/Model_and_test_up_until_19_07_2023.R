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

## Exclude all data without a id or body temperature measurement:
Dragon_dat <- Dragon_data_full |> drop_na(body_temp, survey_id, sighting_id)


## Enviromental variables are: field_season and location? (but they are not gradient)
unique(Dragon_dat$Location)

unique(Dragon_dat$field_season) 
# How long is one season? 
unique(filter(Dragon_dat, field_season == "Field Season 13")$Date)
unique(filter(Dragon_dat, field_season == "Field Season 1")$Date)
# Note: Seasons are not equally spread

# Dis-assemble day/month/year
Dragon_dat_days <- separate(Dragon_dat, "Date", c("Day", "Month", "Year"), sep = "/")


#####
##### Now follow the tutorial: 

head(table(Dragon_dat_days$Name,Dragon_dat_days$Month))

# Can one dragon has more than 1 behavior in a single recording? 

# Plot body temperature vs sex 

plot_temp_sex <- ggplot(Dragon_dat_days, aes(x = Sex, y= body_temp, fill = Sex), xlab="") +
    geom_boxplot() +
    theme_classic()

plot_temp_sex # a lot of outlier/false data?

which(Dragon_dat_days$body_temp == max(Dragon_dat_days$body_temp))

Dragon_dat_days[1798,]$body_temp

# removing outlier: using a strict cut-off (is it reasonable?), and plot again 
Dragon_dat_days_correct_temp <- filter(Dragon_dat_days, body_temp < 50)

plot_temp_sex <- ggplot(Dragon_dat_days_correct_temp, aes(x = Sex, y= body_temp, fill = Sex), xlab="") +
    geom_boxplot() +
    theme_classic()
plot_temp_sex



## Making general behaviors into a long format? Assuming each measurement can have only one behaviour?
Dragon_dat_days_behaviour_long <- Dragon_dat_days_correct_temp |> pivot_longer(Head_Bob:Dewlap, names_to = "Behaviours", values_to = "Behaviours_value")
Dragon_dat_clean <- subset(filter(Dragon_dat_days_behaviour_long, Behaviours_value == "Yes"), select = -Behaviours_value)




## Making models for behaviours (this is non-binary): with sex, disease status, and body temp as predictors 
library(lme4)
# m1 <- glmer(, data = Dragon_dat_clean, family = ) not sure if this work 

# Make one model about resting?
Dragon_dat_days_correct_temp$Resting <- as.factor(Dragon_dat_days_correct_temp$Resting)
Dragon_dat_days_correct_temp$Sex <- as.factor(Dragon_dat_days_correct_temp$Sex)
Dragon_dat_days_correct_temp$Month <- as.factor(Dragon_dat_days_correct_temp$Month)
# m2 <- glmer(Resting ~ Sex + Diseased + body_temp + Month + (1|Name) + (1|Year/Month), data = Dragon_dat_days_correct_temp, family = binomial)

m3 <- glmer(Resting ~ Sex + Month + (1|Name), data = Dragon_dat_days_correct_temp, family = binomial)

Dragon_dat_days_correct_temp <- filter(Dragon_dat_days_correct_temp, Sex == "Male" | Sex == "Female")
Dragon_dat_days_correct_temp <- filter(Dragon_dat_days_correct_temp, Age == "Adult")
hist(Dragon_dat_days_correct_temp$body_temp)
Dragon_dat_days_correct_temp <- filter(Dragon_dat_days_correct_temp, body_temp > 0)
Dragon_dat_days_correct_temp$Diseased <- as.factor(Dragon_dat_days_correct_temp$Diseased)
m4 <- lmer(body_temp ~ Disease + ()