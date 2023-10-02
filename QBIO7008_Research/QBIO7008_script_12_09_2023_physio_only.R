###### Script until 12/09/2023
library("tidyverse")

dragon_physio <- read_csv("Data/Final_updated_data.csv")

#### Checking and saving outliers with 25% and 70% quantile: 


## Body temp outlier
unique_season <- unique(dragon_physio$Season)

column_names <- colnames(dragon_physio)
Total_outlier_body_temp <- data.frame(matrix(nrow = 0, ncol = length(dragon_physio)))
colnames(Total_outlier_body_temp) <- column_names

## Loop through body temp outliers by seasons
for (x in unique_season) {
    data_for_this_season <- dragon_physio |> filter(Season == x)
    
    Quantile_body_temp <- quantile(data_for_this_season$Body.Temp.C, probs=c(.25, .75), na.rm = FALSE)
    IQR_body_temp <- IQR(data_for_this_season$Body.Temp.C)
    
    Lower_body_temp <- Quantile_body_temp[1] - 1.5*IQR_body_temp
    Upper_body_temp <- Quantile_body_temp[2] + 1.5*IQR_body_temp
    
    Outlier_body_temp <- subset(data_for_this_season, Body.Temp.C < Lower_body_temp | Body.Temp.C > Upper_body_temp)
    Total_outlier_body_temp <- rbind(Total_outlier_body_temp, Outlier_body_temp)
}



## Body weight outlier with 0 or NA: 

hist(dragon_physio$Weight.g.)

NA_body_weight <- dragon_physio[is.na(dragon_physio$Weight.g.),]

zero_body_weight <- dragon_physio |> filter(Weight.g. <= 0)

Outlier_body_weight <- rbind(NA_body_weight, zero_body_weight)

## SVL outliers: 

hist(dragon_physio$SVL)
max(dragon_physio$SVL)

NA_body_length <- dragon_physio[is.na(dragon_physio$SVL),]

zero_body_length <- dragon_physio |> filter(SVL <= 0)

Outlier_body_length <- rbind(NA_body_length, zero_body_length)

##Remove NA for data missing SVL and weight:
dragon_physio_no_NA <- dragon_physio |> drop_na(c("SVL", "Weight.g."))

##Filter out body weight and length = 0 data 
dragon_physio_no_NA <- dragon_physio_no_NA |> filter(Weight.g. > 0)
dragon_physio_no_NA <- dragon_physio_no_NA |> filter(SVL > 0)

## Adding weight to length ratio on log scale
dragon_physio_no_NA$W_to_L_ratio <- log10(dragon_physio_no_NA$Weight.g.)/log10(dragon_physio_no_NA$SVL)

## Outlier of body weight to length log ratio outlier using 3 sd: 
mean(dragon_physio_no_NA$W_to_L_ratio)
Outlier_W_to_L_ratio <- filter(dragon_physio_no_NA, W_to_L_ratio < (mean(dragon_physio_no_NA$W_to_L_ratio, na.rm=TRUE) - 3*sd(dragon_physio_no_NA$W_to_L_ratio, na.rm=TRUE)) | W_to_L_ratio > (mean(dragon_physio_no_NA$W_to_L_ratio, na.rm=TRUE) + 3*sd(dragon_physio_no_NA$W_to_L_ratio, na.rm=TRUE)))
Outlier_W_to_L_ratio <- Outlier_W_to_L_ratio[, -which(names(Outlier_W_to_L_ratio) == "W_to_L_ratio")]

## Appending all outliers together and remove duplicated data. 

Total_Outlier <- rbind(Total_outlier_body_temp, Outlier_body_weight, Outlier_body_length, Outlier_W_to_L_ratio)
Total_Outlier <- Total_Outlier[!duplicated(Total_Outlier), ]

## Filtering out all the outlier rows
## https://stackoverflow.com/questions/17338411/delete-rows-that-exist-in-another-data-frame

dragon_physio_clean <- anti_join(dragon_physio, Total_Outlier)
dragon_physio_clean$W_to_L_ratio <- log10(dragon_physio_clean$Weight.g.)/log10(dragon_physio_clean$SVL)

# Clear up any leftover NAs in any other columns

dragon_physio_clean <- na.omit(dragon_physio_clean)
dragon_physio_clean$Season <- as.factor(dragon_physio_clean$Season)
#############
############# Analysis following Anne G. Hertel tutorial 

## How many samples in each season: it is not very evenly distributed
table(dragon_physio_clean$Season)

## How many samples are male vs female: the number is pretty even 
table(dragon_physio_clean$Sex.sightings)

## Make a linear mixed effect model 
colnames(dragon_physio_clean)
library(lme4)
model_m_1 <- lmer(Body.Temp.C ~ Sex.sightings + Location + Season + ConspecificDensity + W_to_L_ratio + (1|Name) + (1|Season:Name), data = dragon_physio_clean)
qqnorm(residuals(model_m_1),main=NULL)
summary(model_m_1)
r.squaredGLMM(model_m_1)

# Season and Body temperature by sex plot:
ggplot(dragon_physio_clean,
       aes(x = as.numeric(Season), y = Body.Temp.C, color = Sex.sightings)) +
    geom_jitter(size=0.5) +
    theme_classic()+
    geom_line(aes(y = predict(model_m_1,re.form = NA)),size=1.5)+
    ylab("mean temperature") 


# Individual variation plot for Season and Location:

pr_individual <- data.frame(Name = dragon_physio_clean[1:2000, "Name"],
                 Season = dragon_physio_clean[1:2000,"Season"],
                 Location = dragon_physio_clean[1:2000,"Location"])
pr_individual$fit <- predict(model_m_1)[1:2000]


ggplot() + geom_line(data = pr_individual,
              aes(x = Season,
                  y = pr_individual[,"fit"],
                  group = Location,
                  color = Name), size=0.3)+
    labs(y = "Body temp",color = "Names")+
    theme_classic()

## Calculating repeatability of model_m_1:

print(VarCorr(model_m_1),comp = c("Variance","Std.Dev."))

VarCorr(model_m_1)$"Name"[1]/(VarCorr(model_m_1)$"Name"[1] + attr(VarCorr(model_m_1), "sc")^2)
