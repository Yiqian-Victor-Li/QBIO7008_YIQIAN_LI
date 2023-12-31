###### Script until 20/09/2023
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

# Factorise seasons: 
dragon_physio_clean$Season <- as.factor(dragon_physio_clean$Season)

# Adding number of sightings: 
df_occurence_count <- as.data.frame(table(dragon_physio_clean$Name))
df_occurence_count$Var1 <- as.character(df_occurence_count$Var1)

dragon_physio_clean <- left_join(dragon_physio_clean, df_occurence_count, by = c("Name" = "Var1"))
dragon_physio_clean <- dragon_physio_clean |> rename(c("Freq" = "number_of_sighting")) 
dragon_physio_clean$number_of_sighting <- as.numeric(dragon_physio_clean$number_of_sighting) 

str(dragon_physio_clean)
#############
############# Analysis following Anne G. Hertel tutorial 

## How many samples in each season: it is not very evenly distributed
table(dragon_physio_clean$Season)

## How many samples are male vs female: the number is pretty even 
table(dragon_physio_clean$Sex.sightings)

## Make a linear mixed effect model 
colnames(dragon_physio_clean)
library(lme4)
max(dragon_physio_clean$number_of_sighting) #Maximum 452, log scale may make more sense
model_m_1 <- lmer(Body.Temp.C ~ Sex.sightings + Location + Season + ConspecificDensity + W_to_L_ratio + log(number_of_sighting) + (1|Name) + (1|Season:Name), data = dragon_physio_clean)
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

# Repeatability of individuals: ~3%
VarCorr(model_m_1)$"Name"[1]/(VarCorr(model_m_1)$"Season:Name"[1] + VarCorr(model_m_1)$"Name"[1] + attr(VarCorr(model_m_1), "sc")^2)

# Repeatability of individuals within seasons:~2.8%
VarCorr(model_m_1)$"Season:Name"[1]/(VarCorr(model_m_1)$"Season:Name"[1] + VarCorr(model_m_1)$"Name"[1] + attr(VarCorr(model_m_1), "sc")^2)


####
#### Simulate the model 1000 times to get a posterior distribution:

set.seed(1)
simulated <- sim(model_m_1, n.sim = 1000)
posterior_Name <- apply(simulated@ranef$"Name"[ , , 1],1,var)
posterior_NameSeason <- apply(simulated@ranef$"Season:Name"[ , , 1],1,var)
posterior_residual <- simulated@sigma^2

quantile(posterior_Name /
             (posterior_Name + posterior_NameSeason + posterior_residual),
         prob=c(0.025, 0.5, 0.975))

quantile(posterior_NameSeason /
             (posterior_Name + posterior_NameSeason + posterior_residual),
         prob=c(0.025, 0.5, 0.975))

# Calculate the coefficient of variation for between individual variance:
CVi <- sqrt(posterior_Name) / summary(model_m_1)$coefficients[1]
quantile(CVi,prob=c(0.025, 0.5, 0.975))


## Which individual's body temp vary more?
randomSims <- REsim(model_m_1, n.sims = 1000)
head(randomSims[randomSims$groupFctr=="Name",])


# add the sex of the individual
randomSims <- merge(randomSims[randomSims$groupFctr=="Name",],
                    dragon_physio_clean[!duplicated(dragon_physio_clean$Name),c("Name","Sex.sightings")],
                    by.x = "groupID",by.y="Name")


# add population intercept and sex specific differences for easier
# interpretation of realized average daily movement distance
summary(model_m_1)
randomSims[randomSims$Sex.sightings == "Female",]$mean <- randomSims[randomSims$Sex.sightings == "Female",]$mean +
    fixef(model_m_1)["(Intercept)"]
randomSims[randomSims$Sex.sightings == "Male",]$mean <- randomSims[randomSims$Sex.sightings == "Male",]$mean +
    (fixef(model_m_1)["(Intercept)"] + fixef(model_m_1)["Sex.sightingsMale"])

# sort data
randomSims$groupID <- factor(randomSims$groupID,
                             levels = randomSims$groupID[order(randomSims$mean)])

# Male and Female mean body temp variability plot:
ggplot()+
    geom_errorbar(data = randomSims, aes(x = groupID, ymin = mean-sd,
                                         ymax = mean+sd, color = Sex.sightings)) +
    geom_point(data = randomSims,
               aes(x = groupID, y = mean), shape = 21, size = 2)+
    theme_classic()+
    ylab("mean body temp")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())+
    annotate("text",x = 30, y = 12, label = "R = 0.22*", size = 4)



########
######## Bayesian Approach:
my.cores <- detectCores()

model_b_1 <- brm(Body.Temp.C ~ Sex.sightings + Location + Season + ConspecificDensity + W_to_L_ratio + log10(number_of_sighting) + (1|Name) + (1|Season:Name),
              data = dragon_physio_clean,
              warmup = 1000,
              iter = 3000,
              thin=4,
              chains = 4,
              inits = "random",
              cores = my.cores,
              seed = 12345)

model_b_1 <- add_criterion(model_b_1, "waic")
saveRDS(model_b_1, "model_b_1.rds")
summary(model_b_1)

# model_b_1 <- readRDS("model_b_1.rds")

## Take the mean and credible interval of the posterior distribution.
colnames(posterior_samples(model_b_1))[1:23]

var.Name <- posterior_samples(model_b_1)$"sd_Name__Intercept"^2
var.NameSeason <- posterior_samples(model_b_1)$"sd_Season:Name__Intercept"^2
var.res <- posterior_samples(model_b_1)$"sigma"^2

RTemp <- var.Name/(var.Name + var.NameSeason + var.res) 
mean(RTemp);HPDinterval(as.mcmc(RTemp),0.95) ## 3% can be explained by individual variance

RNameSeason <- var.NameSeason/(var.Name + var.NameSeason + var.res)
mean(RNameSeason);HPDinterval(as.mcmc(RNameSeason),0.95) ## 2.8% can be explained by seasons

RRes <- var.res/(var.Name + var.NameSeason + var.res)
mean(RRes);HPDinterval(as.mcmc(RRes),0.95) ## 94% unexplained residual 

# Calculate CVi: 
CVi <- sqrt(var.Name) / mean(dragon_physio_clean$Body.Temp.C)
mean(CVi);HPDinterval(as.mcmc(CVi),0.95)

# Plot credible interval 

get_variables(model_b_1)
