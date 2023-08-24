###### Script until 09/08/2023
library("tidyverse")

dragon_physio <- read_csv("Final_updated_data.csv")
dragon_disease <- read_csv("FinalMasterSTudentData.csv")

#Filter out outlier body temperature data:
dragon_physio <- dragon_physio |> filter(Body.Temp.C > 0 & Body.Temp.C < 50)
dragon_disease <- dragon_disease |> filter(Body.Temp..Celcius. > 0 & Body.Temp..Celcius. < 50)


##Remove NA for data missing SVL and weight:
dragon_physio <- dragon_physio |> drop_na(c("SVL", "Weight.g."))
##Remove all duplicated rows: keeping first occurrence of each duplicate
# Do NOT remove duplicated rows in dragon_physio as the duplicates can be same individual
# being sighted multiple times. 
dragon_disease <- dragon_disease[!duplicated(dragon_disease), ]
# Remove rows in dragon_disease where BOTH visual and qPCR are NA
dragon_disease <- dragon_disease[!with(dragon_disease,is.na(Diseased.Visual) & is.na(qPCR.Result)),]


##Converting the latitude and longitude data into the correct format:
dragon_disease$Lat <- as.numeric(dragon_disease$Lat)
dragon_disease$Lat <- ((((dragon_disease$Lat/1000)+27)/60)+27)*-1
dragon_disease$Long<-as.numeric(dragon_disease$Long)
dragon_disease$Long<-(((dragon_disease$Long/1000)+1)/60)+153

##Clean up the dragon disease column wording (some are capitalized some are not):
#Convert yes/no and positive/negative into 1 and 0
unique(dragon_disease$qPCR.Result)
unique(dragon_disease$Diseased.Visual)

dragon_disease$qPCR.Result <- gsub("Positive|POSITIVE",1,dragon_disease$qPCR.Result)
dragon_disease$qPCR.Result <- gsub("Negative|NEGATIVE",0,dragon_disease$qPCR.Result)
dragon_disease$qPCR.Result <- as.numeric(dragon_disease$qPCR.Result)

dragon_disease$Diseased.Visual <- gsub("Yes",1,dragon_disease$Diseased.Visual)
dragon_disease$Diseased.Visual <- gsub("No",0,dragon_disease$Diseased.Visual)
dragon_disease$Diseased.Visual <- as.numeric(dragon_disease$Diseased.Visual)



##Adding a column for determining disease status
dragon_disease$Diseased_sum = NA

## Some double check before running the loop
length(which(is.na(dragon_disease$Diseased.Visual))) # 1827 NAs
length(which(is.na(dragon_disease$qPCR.Result))) # 45162 Nas

for (i in 1:nrow(dragon_disease)) {
    ## Loop all cases with NA first otherwise it won't work!
    
    # ?? case 7: Visual result is NA, PCR result = 1, Asymptomatic 
    if (is.na(dragon_disease$Diseased.Visual[i]) & (dragon_disease$qPCR.Result[i] == 1)) {
        dragon_disease$Diseased_sum[i] <- "Asymptomatic"
    }
    # ?? case 8: Visual result is NA, PCR result = 0, Healthy
    else if (is.na(dragon_disease$Diseased.Visual[i]) & (dragon_disease$qPCR.Result[i] == 0)) {
        dragon_disease$Diseased_sum[i] <- "Healthy"
    }
    # ?? case 2: Visual result = 0, PCR result is NA, Healthy
    else if ((dragon_disease$Diseased.Visual[i] == 0) & is.na(dragon_disease$qPCR.Result[i])) {
        dragon_disease$Diseased_sum[i] <- "Healthy"
    }
    # ?? case 5: Visual result = 1, PCR result is NA, Symptomatic 
    else if ((dragon_disease$Diseased.Visual[i] == 1) & is.na(dragon_disease$qPCR.Result[i])) {
        dragon_disease$Diseased_sum[i] <- "Symptomatic"
    }
    # case 1: Visual result = 0, PCR result = 0, Healthy
    else if((dragon_disease$Diseased.Visual[i] == 0) & (dragon_disease$qPCR.Result[i] == 0)) {
        dragon_disease$Diseased_sum[i] <- "Healthy"
    }
    # case 3: Visual result = 0, PCR result = 1, Asymptomatic 
    else if ((dragon_disease$Diseased.Visual[i] == 0) & (dragon_disease$qPCR.Result[i] == 1)) {
        dragon_disease$Diseased_sum[i] <- "Asymptomatic"
    }
    # case 4: Visual result = 1, PCR result = 0, Symptomatic 
    else if ((dragon_disease$Diseased.Visual[i] == 1) & (dragon_disease$qPCR.Result[i] == 0)) {
    dragon_disease$Diseased_sum[i] <- "Symptomatic"
    }
    # case 6: Visual result = 1, PCR result = 1, Symptomatic 
    else if ((dragon_disease$Diseased.Visual[i] == 1) & (dragon_disease$qPCR.Result[i] == 1)) {
        dragon_disease$Diseased_sum[i] <- "Symptomatic"
    }
}

#Checking how many records are diseased in total:
length(which(dragon_disease$Diseased_sum == "Healthy")) # 46833
length(which(dragon_disease$Diseased_sum == "Symptomatic")) # 1020
length(which(dragon_disease$Diseased_sum == "Asymptomatic")) # 1771
# Only about 5% are diseased? 
# Double check NA just in case 
length(which(is.na(dragon_disease$Diseased_sum))) # 0


##Making a inner join dataset. Does inner join make sense?
dragon_full = merge(dragon_physio, dragon_disease, 
                    by.x = c("Name", "Date.Sighting", "Body.Temp.C", "Lat", "Long"),
                    by.y = c("Name", "Date", "Body.Temp..Celcius.", "Lat", "Long"))
colnames(dragon_full)
#clean up the columns:
dragon_full <- dragon_full |> select(c(Name,Catch.Sex,Body.Temp.C,
                                       SVL,Weight.g.,
                                       Date.Sighting,Location.x,Lat,Long,
                                       NumOverlap50.x,kdarea.x,ConspecificDensity.x,
                                       Diseased.Visual, qPCR.Result, Diseased_sum
                                       ))


##Just in case inner join doesn't make sense, make a left join one:
dragon_full_left = left_join(dragon_physio, dragon_disease, 
                    by = c("Name" = "Name", "Date.Sighting" = "Date", 
                           "Body.Temp.C" = "Body.Temp..Celcius.", 
                           "Lat" = "Lat", "Long" = "Long"))
# Detected an unexpected many-to-many relationship between `x` and `y`.
# Some rows of `x` matches multiple rows in `y`.
# Some dragon have results both positive and negative on the same day???
# remove the problematic row in 'y' and allow 'y' to match any of the any number of rows in 'x'
# 
dragon_disease <- dragon_disease |> group_by(Name, Date) |> 
    filter(n_distinct(Diseased.Visual) == 1) |> ungroup()

dragon_disease <- dragon_disease |> group_by(Name, Date)|> 
    filter(n_distinct(qPCR.Result) == 1) |> ungroup()

dragon_full_left = left_join(dragon_physio, dragon_disease, 
                             by = c("Name" = "Name", "Date.Sighting" = "Date", 
                                    "Body.Temp.C" = "Body.Temp..Celcius.", 
                                    "Lat" = "Lat", "Long" = "Long"),
                             relationship = "many-to-one")


#### Some analysis using the left joined data:

## How many individuals have their disease status changed across multiple measurement?
# remove NA first otherwise won't be able to compare
dragon_full_left_No_NA <- dragon_full_left[!is.na(dragon_full_left$Diseased_sum), ]

Status_change <- dragon_full_left_No_NA |> group_by(Name) |> 
    filter(n_distinct(Diseased_sum) != 1) 

length(unique(Status_change$Name)) #28 out of 370 individuals had their disease status changed, that is a small sample size
length(unique(dragon_full_left_No_NA$Name)) #370

### Sub-setting the data into diseased and non-diseased:

Diseased_individuals <- dragon_full_left |> filter(Diseased_sum == 1)

Healthy_individuals <- dragon_full_left |> filter(Diseased_sum == 0)

##Making some plots:

#Disease predicting body temperature plot:
ggplot(dragon_full_left, aes(x = as.factor(Diseased_sum), y = Body.Temp.C)) + geom_boxplot()
#Diseased individuals seemed to have a slightly lower body temp 

#Sex predicting body temperature plot: 
ggplot(dragon_full_left, aes(x = Sex.sightings, y = Body.Temp.C)) + geom_boxplot()
#Males and Females don't have an average difference

#Sex on body length and weight: 
ggplot(dragon_full_left, aes(x = Sex.sightings, y = SVL)) + geom_boxplot()
ggplot(dragon_full_left, aes(x = Sex.sightings, y = Weight.g.)) + geom_boxplot()
#Males are longer and heavier than females on average

#Body length predicting body temp:
ggplot(dragon_full_left, aes(x = SVL, y = Body.Temp.C)) + geom_point() + geom_smooth()
#No strong correlation?

#Body weight predicting body temp:
ggplot(dragon_full_left, aes(x = Weight.g., y = Body.Temp.C)) + geom_point() + geom_smooth()
#No strong correlation?

#Location predicting body temp:
ggplot(dragon_full_left, aes(x = Location.x, y = Body.Temp.C)) + geom_boxplot()
#No significant correlation

#Conspecific density predicting body temp:
ggplot(dragon_full_left, aes(x = ConspecificDensity.x, y = Body.Temp.C)) + geom_point() + geom_smooth()
#Slight positive correlation?

#Location and conspecific density correlations:
ggplot(dragon_full_left, aes(x = Location.x, y = ConspecificDensity.x)) + geom_boxplot()
#Some correlations!!

#Disease and conspecific density:
ggplot(dragon_full_left, aes(x = as.factor(Diseased_sum), y = ConspecificDensity.x)) + geom_boxplot()
#No correlation? This is unexpected??


#Diseased individuals grouped by seasons
season_d <- ggplot(Diseased_individuals, aes(x = as.factor(Season.x), y = Body.Temp.C, fill = Sex.sightings)) + geom_boxplot()
#Healthy individuals grouped by seasons
season_h <- ggplot(Healthy_individuals, aes(x = as.factor(Season.x), y = Body.Temp.C, fill = Sex.sightings)) + geom_boxplot()

library(cowplot)
plot_grid(season_h, season_d, labels = c("Healthy", "Diseased") )
#Health individuals seems to have their body temp fluctuate more
#Males seem to have their body temperature fluctuate more than females

library(lme4)
#Diseased individuals simple model
lmer_d_1 <- lmer(Body.Temp.C ~ Diseased_sum + Sex.sightings + ConspecificDensity.x*Location.x + (1|Name), data = Diseased_individuals)

#Health individuals simple model
lemr_h_1 <- lmer(Body.Temp.C ~ Diseased_sum + Sex.sightings + ConspecificDensity.x*Location.x + (1|Name), data = Healthy_individuals)

#?? Error in fixed-effect model matrix is rank deficient so dropping 1 column / coefficient