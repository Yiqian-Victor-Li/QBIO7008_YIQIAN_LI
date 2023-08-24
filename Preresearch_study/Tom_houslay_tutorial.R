library(lme4)
library(MCMCglmm)
library(tidyverse)
library(broom)
library(broom.mixed)
library(nadiv)
df_syndrome <- read_csv("syndrome.csv")


## Boldness:

lmer_b <- lmer(boldness ~ scale(assay_rep, scale=FALSE) +
                   scale(body_size) +
                   (1|ID),
               data = df_syndrome)
plot(lmer_b)
qqnorm(residuals(lmer_b))
hist(residuals(lmer_b))
summary(lmer_b)

rep_bold <- tidy(lmer_b, effects = "ran_pars", scales = "vcov") %>%
    select(group, estimate) %>%
    spread(group, estimate) %>%
    mutate(repeatability = ID/(ID + Residual))

rep_bold


## Exploration: 
lmer_e <- lmer(exploration ~ scale(assay_rep, scale=FALSE) +
                   scale(body_size) +
                   (1|ID),
               data = df_syndrome)

rep_expl <- tidy(lmer_e, effects = "ran_pars", scales = "vcov") %>%
    select(group, estimate) %>%
    spread(group, estimate) %>%
    mutate(repeatability = ID/(ID + Residual))

rep_expl


## best linear unbiased prediction (BLUP) is used in linear mixed models for the estimation of random effects

df_BLUPS_B <- tibble(ID = row.names(ranef(lmer_b)$ID),
                         BLUP_B = ranef(lmer_b)$ID[,"(Intercept)"])
df_BLUPS_E <- tibble(ID = row.names(ranef(lmer_e)$ID),
                         BLUP_E = ranef(lmer_e)$ID[,"(Intercept)"])
df_BLUPS_EB <- left_join(df_BLUPS_E,
                         df_BLUPS_B,
                         by = "ID")

ggplot(df_BLUPS_EB, aes(BLUP_E, BLUP_B)) + geom_point()

# test for correlation: Pearson's product-moment correlation
cor.test(df_BLUPS_EB$BLUP_E,
         df_BLUPS_EB$BLUP_B)


### Bivariate models

# Setting prior:
prior_E_B_1px = list(R = list(V = diag(2), nu = 0.002),
                     G = list(G1 = list(V = diag(2), nu = 2,
                                        alpha.mu = rep(0,2),
                                        alpha.V = diag(25^2,2,2))))

mcmc_E_B_us <- MCMCglmm(cbind(scale(exploration), scale(boldness)) ~ trait-1 +
                            trait:scale(assay_rep, scale = FALSE) +
                            trait:scale(body_size),
                        random =~ us(trait):ID,
                        rcov =~ us(trait):units,
                        family = c("gaussian","gaussian"),
                        prior = prior_E_B_1px,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = as.data.frame(df_syndrome))
par(mar=c(1,1,1,1))
# plot(mcmc_E_B_us$VCV)

summary(mcmc_E_B_us)
