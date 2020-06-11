# Code for ms: "Differences in Prey Personality Mediate Trophic Cascades"
# Author: Nathalie R Sommer
# Last Update: 10 June 2020

#### Packages ----
require(tidyverse)
require(rptR)
require(ggplot2)
require(vegan)
require(lmerTest)
require(dplyr)

#### (A) Whole-population personality assay assessments ----

dat_pop <- read.csv("Population_Terrarium_Assays.csv")
rep_int <- read.csv("Population_IntervalPartitionLongForm.csv")
rep_int$Assay_Round <- as.factor(rep_int$Assay_Round)

#### (A.1) Repeatability ----

rep_within <- rpt(Activity ~ Assay_Type + (1 | Indiv.ID), 
                                grname="Indiv.ID", 
                                data=rep_int, 
                                datatype="Poisson", 
                                nboot=500)
summary(rep_within)

rep_across <- rpt(Activity ~ Assay_Round + (1 | Indiv.ID), 
                                   grname= "Indiv.ID", 
                                   data=rep_int, 
                                   datatype="Poisson", 
                                   nboot=500)
summary(rep_across)

### (A.2) Random intercept model ----

# Does individual explain more variation in activity level than the assay context? 

lme_m1_int <- lmer(Activity ~ Assay_Type + (1|Indiv.ID), 
                   data = rep_int)
lm_m1 <- lm(Activity ~ Assay_Type, data = rep_int)

anova(lme_m1_int, lm_m1)

### (A.3) Personality trait distribution ----

# Defining shy and bold by 25% quartiles
quantile(dat_pop$SCORE)

#### (A.4) Mass and personality ----

cor.test(x=dat_pop$Mass, y=dat_pop$SCORE) # mass by activity level for all indivs
t.test(dat_pop$Mass ~ dat_pop$Personality) # mass by personality category for all indivs

dat_pop_meso <- dat_pop[which(dat_pop$Exp=="Y"),]
t.test(dat_pop_meso$Mass ~ dat_pop_meso$Personality) # mass by personality category for mesocosm indivs only

#### (A.5) Respiration rate ----

bench.met <- read.csv("Benchtop.Metabolism.csv")

# Repeated measures ANOVA
resp_rep_anova <- lmer(Resp ~ Personality + Run + (1|Indiv.ID), data=bench.met)
anova(resp_rep_anova)
boxplot(bench.met$Resp ~ bench.met$Run)

# t-test for mass diff between groups
t.test(bench.met.alone$Weight..g. ~ bench.met.alone$Personality)

#### (A.6) Lifetime personality assessment ----

# Repeatability using three, 4-min partitioned data
body_interval <- read.csv("BodySize_IntervalPartitionLongForm.csv")

overtime_assays <- rpt(Activity ~ 1 + Assay_Type +
                         (1 | Indiv.ID), 
                     grname = "Indiv.ID",
                     data = body_interval, 
                     datatype = "Poisson", 
                     nboot=500)

summary(overtime_assays)

#### (B) Personality under predation ---- 
##### (B.1) Survival ----

meso.dat <- read.csv("Mesocosm_Summary.csv")

anova(lmer(Grasshopper.Surv ~ 
       Personality_Pre + 
       Predator_Treatment + 
       Personality_Pre*Predator_Treatment + 
       (1|Block), 
       data = meso.dat))
mean(meso.dat$Grasshopper.Surv)

#### (B.2) Personality re-assays ----

meso.dat.NAs <- na.omit(meso.dat) # Excludes plots that went extinct

# Change in personality
personality_lmer <- (lmer(Score_Change ~ 
                            Personality_Pre + 
                            Predator_Treatment +
                            Personality_Pre*Predator_Treatment + 
                            (1|Block),
                            data = meso.dat.NAs))
summary(personality_lmer)

# Change in plasticity (CRP)
meso.dat.NAs$CRP_Change <- (meso.dat.NAs$VAR_CRP_POST - meso.dat.NAs$VAR_CRP_PRE)

CRP_change_lmer <- lmer(CRP_Change ~ 
                          Personality_Pre +
                          Predator_Treatment +
                          Personality_Pre*Predator_Treatment + 
                          (1|Block), 
                          data = meso.dat.NAs)
summary(CRP_change_lmer)

## Rudimentary permutation test
personality_lmer_RAND <- (lmer(Score_Change ~ 
                            Personality_Pre_RAND + 
                            Predator_Treatment +
                            Personality_Pre_RAND*Predator_Treatment + 
                            (1|Block),
                          data = meso.dat.NAs))
summary(personality_lmer_RAND)

CRP_change_lmer_RAND <- lmer(CRP_Change ~ 
                          Personality_Pre_RAND +
                          Predator_Treatment +
                          Personality_Pre_RAND*Predator_Treatment + 
                          (1|Block), 
                          data = meso.dat.NAs)
summary(CRP_change_lmer_RAND)

# Repeatability

rep_reassay <- read.csv("Resurvey_IntervalPartitionLongForm.csv")
rep_reassay$Assay_Round <- as.factor(rep_reassay$Assay_Round)

rep_within_reassay <- rpt(Activity ~ Assay.Type + (1 | Indiv.ID),
                  grname="Indiv.ID", 
                  data=rep_reassay, 
                  datatype="Poisson", 
                  nboot=500)

summary(rep_within_reassay)

rep_across_reassay <- rpt(Activity ~ Assay_Round + (1 | Indiv.ID),
                  grname= "Indiv.ID", 
                  data=rep_reassay, 
                  datatype="Poisson", 
                  nboot=500)

summary(rep_across_reassay)

# Warning about convergence is expected, not problematic. See package details cran.r-project.org/web/packages/rptR/vignettes/rptR.html.

#### (C) Trophic impact ----

#### (C.1) Goldenrod biomass ----
summary(lmer(meso.dat$Goldenrod_Biomass_Adj ~ 
               meso.dat$Predator_Treatment +
               meso.dat$Personality_Pre + 
               meso.dat$Grasshopper.Surv +
               meso.dat$Predator_Treatment*meso.dat$Personality_Pre + 
               (1|meso.dat$Block)))

#### (C.2) Grass biomass ----
summary(lmer(meso.dat$Grass_Biomass_Adj ~ 
               meso.dat$Predator_Treatment + 
               meso.dat$Personality_Pre + 
               meso.dat$Grasshopper.Surv +
               meso.dat$Predator_Treatment*meso.dat$Personality_Pre + 
               (1|meso.dat$Block)))

#### (C.3) Forbs biomass ----

summary(lmer(meso.dat$Forbs_Biomass_Adj ~ 
               meso.dat$Predator_Treatment + 
               meso.dat$Personality_Pre + 
               meso.dat$Grasshopper.Surv +
               meso.dat$Predator_Treatment*meso.dat$Personality_Pre + 
               (1|meso.dat$Block)))

#### (D) Chi-sq for habitat domain ----
# For habitat domain isopleths, see "Benchtop_Assays_HabitatDomain.xlsx"

# 95% isopleth for baseline
chisq.test(matrix(c(0, 11, 7, 2, 7, 14, 23, 25, 38, 49, 75, 105, 
                    11, 4, 8, 9, 13, 33, 20, 32, 33, 39, 98, 102), 
                  nrow=12, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("5", "6", "7", "8", "9", "10", 
                                    "11", "12", "13", "14", "15", "16"), 
                                  c("Shy", "Bold"))))

# 50% isopleth for baseline
chisq.test(matrix(c(0, 75, 105,
                    39, 98, 102),
                  nrow=3, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("14", "15", "16"), 
                                  c("Shy", "Bold"))))

# 50% isopleth for predator
chisq.test(matrix(c(18, 23, 64, 91,
                    0, 58, 129, 158),
                  nrow=4, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("13", "14", "15", "16"), 
                                  c("Shy", "Bold"))))

# 50% isopleth for Solidago use, baseline
chisq.test(matrix(c(0, 0, 0, 9, 19, 16,
                    19, 10, 20, 14, 0, 0),
                  nrow=6, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("10", "11", "12", "13", "14", "15"), 
                                  c("Shy", "Bold"))), simulate.p.value = TRUE)

# 50% isopleth for Solidago use, predator
chisq.test(matrix(c(0, 18, 29, 11,
                    28, 17, 28, 0),
                  nrow=4, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("9", "10", "11", "12"), 
                                  c("Shy", "Bold"))), simulate.p.value = TRUE)

# 50% isopleth for foraging, baseline
chisq.test(matrix(c(0, 0, 0, 2, 3, 5, 5,
                    3, 4, 3, 3, 0, 0, 0),
                  nrow=7, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("10", "11", "12", "13", "14", "15", "16"), 
                                  c("Shy", "Bold"))), simulate.p.value = TRUE)

# 50% isopleth for foraging, predator
chisq.test(matrix(c(0, 0, 3, 2, 0, 2,
                    2, 8, 6, 4, 3, 2),
                  nrow=6, 
                  ncol=2, 
                  byrow = FALSE, 
                  dimnames = list(c("8", "9", "10", "11", "12", "13"), 
                                  c("Shy", "Bold"))), simulate.p.value = TRUE)

##### APPENDIX ----

# Full 15 min assay for initial distribution
pop_rep <- read.csv("Population_RepBy15Min.csv")
pop_rep$Activity <- as.integer(pop_rep$Activity)
pop_repest <- rpt(Activity ~ 1 +
                    (1 | Indiv.ID),
                  grname = "Indiv.ID",
                  data = pop_rep, 
                  datatype = "Poisson", 
                  nboot = 500)

summary(pop_repest)
                  

# Full 15 min assay for post-treatment individuals from field mesocosms
resurvey_rep <- read.csv("Resurvey_RepBy15Min.csv")
resurvey_rep$Activity <- as.integer(resurvey_rep$Activity)
resurvey_repest <- rpt(Activity ~ 1 +
                         (1 | Indiv.ID),
                       grname = "Indiv.ID",
                       data = resurvey_rep,
                       datatype="Poisson",
                       nboot = 500)
summary(resurvey_repest)

# Full 15 min assay for long term lab estimates of personality
# Note, this repeatability estimate uses Assay_Type as a fixed effect because each individual was measured in each assay more than once.
body_rep <- read.csv("BodySize_RepBy15Min.csv")
body_rep$Activity <- as.integer(body_rep$Activity)
body_rep_overtime <- rpt(Activity ~ Assay_Type +
                         (1 | Indiv.ID), 
                     grname = "Indiv.ID",
                     data = body_rep, 
                     datatype = "Poisson", 
                     nboot=500)

summary(body_rep_overtime)

# CRP x Trophic Impact
summary(lmer(meso.dat$Grass_Biomass_Adj ~ 
               meso.dat$VAR_CRP_PRE +
               meso.dat$Predator_Treatment + 
               meso.dat$Grasshopper.Surv +
               meso.dat$Predator_Treatment*meso.dat$VAR_CRP_PRE + 
               (1|meso.dat$Block)))

summary(lmer(meso.dat$Goldenrod_Biomass_Adj ~ 
               meso.dat$VAR_CRP_PRE +
               meso.dat$Predator_Treatment +
               meso.dat$Grasshopper.Surv +
               meso.dat$Predator_Treatment*meso.dat$VAR_CRP_PRE + 
               (1|meso.dat$Block)))

summary(lmer(meso.dat$Forbs_Biomass_Adj ~ 
               meso.dat$VAR_CRP_PRE +
               meso.dat$Predator_Treatment +
               meso.dat$Grasshopper.Surv +
               meso.dat$Predator_Treatment*meso.dat$VAR_CRP_PRE + 
               (1|meso.dat$Block)))
