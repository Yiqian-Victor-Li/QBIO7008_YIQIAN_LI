###### Script until 30/08/2023
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

write_csv(Total_outlier_body_temp, "Data/Total_outlier_body_temp.csv")


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
dragon_physio <- dragon_physio |> drop_na(c("SVL", "Weight.g."))

##Filter out body weight and length = 0 data 
dragon_physio <- dragon_physio |> filter(Weight.g. > 0)
dragon_physio <- dragon_physio |> filter(SVL > 0)

## Adding weight to length ratio on log scale
dragon_physio$W_to_L_ratio <- log10(dragon_physio$Weight.g.)/log10(dragon_physio$SVL)

## Outlier of body weight to length log ratio outlier using 3 sd: 
mean(dragon_physio$W_to_L_ratio)
Outlier_W_to_L_ratio <- filter(dragon_physio, W_to_L_ratio < (mean(dragon_physio$W_to_L_ratio, na.rm=TRUE) - 3*sd(dragon_physio$W_to_L_ratio, na.rm=TRUE)) | W_to_L_ratio > (mean(dragon_physio$W_to_L_ratio, na.rm=TRUE) + 3*sd(dragon_physio$W_to_L_ratio, na.rm=TRUE)))
Outlier_W_to_L_ratio <- Outlier_W_to_L_ratio[, -which(names(Outlier_W_to_L_ratio) == "W_to_L_ratio")]

## Appending all outliers together and remove duplicated data. 

Total_Outlier <- rbind(Total_outlier_body_temp, Outlier_body_weight, Outlier_body_length, Outlier_W_to_L_ratio)
Total_Outlier <- Total_Outlier[!duplicated(Total_Outlier), ]
write_csv(Total_Outlier, "Data/Total_Outlier.csv")

Weight_and_SVL_outlier <- rbind(Outlier_body_weight, Outlier_body_length, Outlier_W_to_L_ratio)
Weight_and_SVL_outlier <- Weight_and_SVL_outlier[!duplicated(Weight_and_SVL_outlier), ]
write_csv(Weight_and_SVL_outlier, "Data/Weight_and_SVL_outlier.csv")


#############
#Filter out outlier body temperature data:
dragon_physio <- dragon_physio |> filter(Body.Temp.C > 0 & Body.Temp.C < 50)

##Remove NA for data missing SVL and weight:
dragon_physio <- dragon_physio |> drop_na(c("SVL", "Weight.g."))

##Filter out body weight and length = 0 data 
dragon_physio <- dragon_physio |> filter(Weight.g. > 0)
dragon_physio <- dragon_physio |> filter(SVL > 0)


## Adding weight to length ratio on log scale
dragon_physio$W_to_L_ratio <- log10(dragon_physio$Weight.g.)/log10(dragon_physio$SVL)


## Adding number of sighting per individual 

Name_occurence <- as.data.frame(table(dragon_physio$Name))

dragon_physio_new <- left_join(dragon_physio,Name_occurence, by = c("Name" = "Var1"), relationship = "many-to-one")


# Checking body temperature distribution see if Gaussian 
hist(dragon_physio$Body.Temp.C)

colnames(dragon_physio_new)

library("brms")

## Among individual model:

# This stuck on loading
Model_A_1 <- brm(Body.Temp.C ~ Sex.sightings + Location + W_to_L_ratio + ConspecificDensity + Freq + (1|Name), data = dragon_physio_new,family = gaussian())

# This one works
Model_A_test <- brm(Body.Temp.C ~ ConspecificDensity + Freq, data = dragon_physio_new,family = gaussian())
summary(Model_A_test)

# This one also works
Model_A_test_2 <- brm(Body.Temp.C ~ Sex.sightings + Location, data = dragon_physio_new,family = gaussian())
summary(Model_A_test_2)


Model_A_3 <- brm(Body.Temp.C ~ Sex.sightings + Location + ConspecificDensity + W_to_L_ratio + (1|Name), data = dragon_physio_new,family = gaussian())
summary(Model_A_3)
## Within individual model: 