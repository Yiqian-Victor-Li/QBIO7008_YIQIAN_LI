###### Script until 26/10/2023
library("tidyverse")
library(arm)
library(MuMIn)
library(plyr)
library(broom)
library(coda)
library(grid)
library(gridExtra)
library(brms)
library(broom.mixed)
library(merTools)
library(tidybayes)
library(parallel)
library(ggplot2)
library(ggcorrplot)
library(GGally)


dragon_physio <- read_csv("Data/Final_Masters_Data.csv")
colnames(dragon_physio)
length(unique(dragon_physio$Name))
# Checking for NA in body temp, length, weight, sex, health status etc.: 

which(is.na(dragon_physio$Body.Temp..Celcius.), arr.ind=TRUE) # all good
which(is.na(dragon_physio$Torso..mms.), arr.ind=TRUE) # all good
which(is.na(dragon_physio$Weight..gms.), arr.ind=TRUE) # Some NAs
which(is.na(dragon_physio$Sex_from_sightings), arr.ind=TRUE) # all good
which(is.na(dragon_physio$Health_Status), arr.ind=TRUE) # all good
which(is.na(dragon_physio$Health_Visual_combined), arr.ind=TRUE) # all good
which(is.na(dragon_physio$Location), arr.ind=TRUE) # Some NAs 
which(is.na(dragon_physio$density50_per_season), arr.ind=TRUE) # all good
which(is.na(dragon_physio$Season), arr.ind=TRUE) # all good

## Clean up the data to excluding data with NA in weight and location:

dragon_physio_clean <- dragon_physio |> drop_na(c("Weight..gms.", "Location"))
                                                
                                                
#### Checking and saving outliers with 25% and 70% quantile: 

## Finding body temp outlier:
unique_season <- unique(dragon_physio_clean$Season)
column_names <- colnames(dragon_physio_clean)
Total_outlier_body_temp <- data.frame(matrix(nrow = 0, ncol = length(dragon_physio_clean)))
colnames(Total_outlier_body_temp) <- column_names

## Loop through body temp outliers by seasons
for (season in unique_season) {
    data_for_this_season <- dragon_physio_clean |> filter(Season == season)
    
    Quantile_body_temp <- quantile(data_for_this_season$Body.Temp..Celcius., probs=c(.25, .75), na.rm = FALSE)
    IQR_body_temp <- IQR(data_for_this_season$Body.Temp..Celcius.)
    
    Lower_body_temp <- Quantile_body_temp[1] - 1.5*IQR_body_temp
    Upper_body_temp <- Quantile_body_temp[2] + 1.5*IQR_body_temp
    
    Outlier_body_temp <- subset(data_for_this_season, Body.Temp..Celcius. < Lower_body_temp | Body.Temp..Celcius. > Upper_body_temp)
    Total_outlier_body_temp <- rbind(Total_outlier_body_temp, Outlier_body_temp)
}


## Body weight outlier with 0 or NA: 

hist(dragon_physio_clean$Weight..gms.)

NA_body_weight <- dragon_physio_clean[is.na(dragon_physio_clean$Weight..gms.),]

zero_body_weight <- dragon_physio_clean |> filter(Weight..gms. <= 0)

Outlier_body_weight <- rbind(NA_body_weight, zero_body_weight)

## SVL outliers: 

hist(dragon_physio_clean$Torso..mms.)
max(dragon_physio_clean$Torso..mms.)

NA_body_length <- dragon_physio_clean[is.na(dragon_physio_clean$Torso..mms.),]

zero_body_length <- dragon_physio_clean |> filter(Torso..mms. <= 0)

Outlier_body_length <- rbind(NA_body_length, zero_body_length)

##Filter out body weight and length = 0 data 
dragon_physio_clean <- dragon_physio_clean |> filter(Weight..gms. > 0)
dragon_physio_clean <- dragon_physio_clean |> filter(Torso..mms. > 0)

## Adding weight to length ratio on log scale
dragon_physio_clean$W_to_L_ratio <- log10(dragon_physio_clean$Weight..gms.)/log10(dragon_physio_clean$Torso..mms.)

## Outlier of body weight to length log ratio outlier using 3 sd: 
mean(dragon_physio_clean$W_to_L_ratio)
Outlier_W_to_L_ratio <- filter(dragon_physio_clean, W_to_L_ratio < (mean(dragon_physio_clean$W_to_L_ratio, na.rm=TRUE) - 3*sd(dragon_physio_clean$W_to_L_ratio, na.rm=TRUE)) | W_to_L_ratio > (mean(dragon_physio_clean$W_to_L_ratio, na.rm=TRUE) + 3*sd(dragon_physio_clean$W_to_L_ratio, na.rm=TRUE)))
Outlier_W_to_L_ratio <- Outlier_W_to_L_ratio[, -which(names(Outlier_W_to_L_ratio) == "W_to_L_ratio")] # drop the W_to_L_ratio column

## Appending all outliers together and remove duplicated data. 
Total_Outlier <- rbind(Total_outlier_body_temp, Outlier_body_weight, Outlier_body_length, Outlier_W_to_L_ratio)
Total_Outlier <- Total_Outlier[!duplicated(Total_Outlier), ]

## Filtering out all the outlier rows
## https://stackoverflow.com/questions/17338411/delete-rows-that-exist-in-another-data-frame

dragon_physio_clean <- anti_join(dragon_physio_clean, Total_Outlier)
dragon_physio_clean$W_to_L_ratio <- log10(dragon_physio_clean$Weight..gms.)/log10(dragon_physio_clean$Torso..mms.) # Clean up complete


######
# Factorise seasons: 
dragon_physio_clean$Season <- as.factor(dragon_physio_clean$Season)

# Factorise health status:
dragon_physio_clean$Health_Status <- as.factor(dragon_physio_clean$Health_Status)

# Adding number of sightings: 
df_occurence_count <- as.data.frame(table(dragon_physio_clean$Name))
df_occurence_count$Var1 <- as.character(df_occurence_count$Var1)

dragon_physio_clean <- left_join(dragon_physio_clean, df_occurence_count, by = c("Name" = "Var1"))
dragon_physio_clean <- dragon_physio_clean |> rename(c("Freq" = "number_of_sighting")) 
dragon_physio_clean$number_of_sighting <- as.numeric(dragon_physio_clean$number_of_sighting) 

# Standardize data to make mean centred around mean and divide 1 standard deviation: 
columns_to_standardize <- c("density50_per_season", "W_to_L_ratio", "number_of_sighting")
dragon_physio_clean[columns_to_standardize] <- scale(dragon_physio_clean[columns_to_standardize])

str(dragon_physio_clean)


# Subsets of diseased and healthy individuals since some individual may have their status changed:
unique(dragon_physio_clean$Health_Status) #"Asymptomatic" "Diseased" "No"  
dragon_physio_clean_diseased <- dragon_physio_clean |> filter(Health_Status == c('Asymptomatic', 'Diseased'))
dragon_physio_clean_healthy <- dragon_physio_clean |> filter(Health_Status == 'No')

# Checking individuals with disease status changed:

dragon_physio_status_changed <- dragon_physio_clean |> group_by(Name) |> 
    filter(n_distinct(Health_Status) != 1) 

length(unique(dragon_physio_status_changed$Name)) # 95 individuals



############# Analysis
######## Bayesian Approach:
## https://discourse.mc-stan.org/t/brms-runs-too-slow-error-in-definition-on-my-behalf/25104
## https://ourcodingclub.github.io/tutorials/brms/#fit

colnames(dragon_physio_clean)

## Checking correlation between all numerical predictors:
model_predictor_columns <- dragon_physio_clean[, c("W_to_L_ratio", "density50_per_season", "number_of_sighting")]
correlation_matrix <- cor(model_predictor_columns)
plot_predictors <- ggcorrplot(correlation_matrix, type = "upper", lab = TRUE)
ggsave("Results/numerical_predictor_correlation_plot.png", plot_predictors, width = 10, height = 10) # very weak correlation, all good
dev.off()


my.cores <- detectCores()


model_1 <- brm(Body.Temp..Celcius. ~ Health_Status + Sex_from_sightings + Location + W_to_L_ratio + density50_per_season + Season + number_of_sighting
               + Health_Status*Sex_from_sightings + Health_Status*Location + Health_Status*W_to_L_ratio + Health_Status*density50_per_season + Health_Status*Season + Health_Status*number_of_sighting
               + (1|Name) + (1|Season:Name),
              data = dragon_physio_clean,
              warmup = 1000,
              iter = 5000,
              thin= 4,
              chains = 4,
              cores = my.cores,
              seed = 12345)

model_1 <- add_criterion(model_1, "waic")

# Saving model 
saveRDS(model_1, "Results/model_1.rds") 
# model_1 <- readRDS("Results/model_1.rds")

# Checking model summary: 
summary(model_1)
model_1_summary <- tidy(model_1)
write.csv(model_1_summary, file = "Results/brms_model_1_summary.csv")

# Some model diagnositics:
bayes_R2(model_1) # 0.148945

loo(model_1)

pp_check(model_1, ndraws = 100)  # posterior predictive checks
dev.off()

plot(model_1)

### Calculate the repeatability of the result:
## Take the mean and credible interval of the posterior distribution.
colnames(posterior_samples(model_1))[1:63]

var.Name <- posterior_samples(model_1)$"sd_Name__Intercept"^2
var.NameSeason <- posterior_samples(model_1)$"sd_Season:Name__Intercept"^2
var.res <- posterior_samples(model_1)$"sigma"^2

RTemp <- var.Name/(var.Name + var.NameSeason + var.res) 
mean(RTemp);HPDinterval(as.mcmc(RTemp),0.95) ## 2% can be explained by individual variance

RNameSeason <- var.NameSeason/(var.Name + var.NameSeason + var.res)
mean(RNameSeason);HPDinterval(as.mcmc(RNameSeason),0.95) ## 3% can be explained by seasons

RRes <- var.res/(var.Name + var.NameSeason + var.res)
mean(RRes);HPDinterval(as.mcmc(RRes),0.95) ## 94% unexplained residual 



### Try plotting the posterior predictions: did not work...

(location_fit <- dragon_physio_clean %>%
        group_by(Location) %>%
        add_predicted_draws(model_1) %>%
        ggplot(aes(x = Health_Status, y = Body.Temp..Celcius., color = ordered(Location), fill = ordered(Location))) +
        stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
        geom_point(data = dragon_physio_clean) +
        scale_fill_brewer(palette = "Set2") +
        scale_color_brewer(palette = "Dark2") +
        theme_bw() +
        ylab("Body temp C") +
        xlab("Health Status") +
        theme_bw() +
        theme(legend.title = element_blank()))



#### Building lmer models using lme4:

model_diseased <- lmer(Body.Temp..Celcius. ~ Sex_from_sightings + Location + W_to_L_ratio + density50_per_season + Season + number_of_sighting 
                       + (1|Name) + (1|Season:Name), data = dragon_physio_clean_diseased)

model_healthy <- lmer(Body.Temp..Celcius. ~ Sex_from_sightings + Location + W_to_L_ratio + density50_per_season + Season + number_of_sighting 
                       + (1|Name) + (1|Season:Name), data = dragon_physio_clean_healthy)

summary(model_diseased)
summary(model_healthy)

qqnorm(residuals_diseased, main = "")
qqline(residuals_diseased)
title("QQ Plot of Residuals - Diseased Model", cex.main = 0.9)
dev.off()

qqnorm(residuals_healthy, main = "")
qqline(residuals_healthy)
title("QQ Plot of Residuals - Healthy Model", cex.main = 0.9)
dev.off()

r.squaredGLMM(model_diseased)
r.squaredGLMM(model_healthy)

residuals_diseased <- residuals(model_diseased)
residuals_healthy <- residuals(model_healthy)

# Perform a Wilcoxon rank-sum test
wilcoxon_test_result <- wilcox.test(residuals_diseased, residuals_healthy)

# View the test result
print(wilcoxon_test_result)
