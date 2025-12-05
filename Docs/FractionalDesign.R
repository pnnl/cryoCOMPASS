# Fractional Factorial Design
# Damon Leach, Rose-Marie Haynes
# January 20, 2025

## STEP 1: EXPERIMENTAL DESIGN SETUP
# install necessary libraries
library(AlgDesign)
library(dplyr)
library(ggplot2)
library(car)
library(patchwork)
library(desirability)

# set up factor levels
fact1 <- c(0.25,1)
fact2 <- c(5,9)
fact3 <- c("CHAPSO","None","FOM")

# get overall design for a full fractional factorial design
f.design <- expand.grid(Concentration = fact1,
                        BlotTime = fact2,
                        Detergent = fact3)

# sample information
num_samples = 25
num_replicates = 5

# will need to add a check to make sure that num_replicates can be evenly divided into num_samples
# determine number of unique trials we want
num_trials = num_samples/num_replicates

# nTrials must be larger than the following value
# intercept + col for numeric + num_factor_categorical -1 -> is how we get X

# get optimized design
opt.design <- optFederov(data = f.design, nTrials = num_trials)
opt.design_df <- opt.design$design

# add in the replicates
opt.design_df <- opt.design_df[rep(1:nrow(opt.design_df), each = num_replicates),]
rownames(opt.design_df) <- c(1:nrow(opt.design_df))

# create excel file for adding in the data
# download file

## STEP 2: ANALYSIS
# reupload file that has the response values calculated

# insert file path here (this will vary from based on file name and location)
filepath = "Data/example_experiment.xlsx"
df <- read_xlsx(filepath)

# standard linear regression model
# ensure that results match with what JMP outputs
# run model for ice thickness
mod_ice <- lm(`Ice Thickness` ~ `Concentration` + `Blot Time` + `Detergent Type`, data = df)
summary(mod_ice)
# ANOVA results (JMP uses types 2 sum of squares)
Anova(mod_ice, type = "II")

mod_density <- lm(`Particle Density` ~ `Concentration` + `Blot Time` + `Detergent Type`, data = df)
summary(mod_density)
Anova(mod_density, type = "II")

## STEP 3: DETERMINE DESIRABILITY VALUES AND OPTIMAL SAMPLING

# these desirability values are based on functions from Derringer and Suich (1980)

# set up desirability parameters (default scale is 1)
iceD <- dTarget(-3,0,3, lowScale = 1, highScale = 1)
plot(iceD)
densityD <- dTarget(-3,0,3, lowScale = 1, highScale = 1)
plot(densityD)

# set up a grid of possible values to consider for finding optimal desirability value
Blot_seq = seq(from = 5, to = 9, by = 0.01)
Concentration_seq = seq(from = 0.25, to = 1, by = 0.01)
DetergentType = c("CHAPSO","FOM","None")
optim_df <- expand.grid(Concentration = Concentration_seq, BlotTime = Blot_seq, DetergentType = DetergentType)

# get desirability for all these values
predIce <- predict(mod_ice, newdata = optim_df)
predDensity <- predict(mod_density, newdata = optim_df)
desIce = predict(iceD, predIce)
desDensity = predict(densityD, predDensity)

# overall is the geometric mean of the varying desirability values
overallD <- sqrt(desIce*desDensity)

# determine what value gives us the optimal value
optim_df[which.max(overallD),]

# add desirability as a column in dataset
optim_df$desirability <- overallD

# ggplots to mimic what we see in JMP
p1 <- optim_df %>%
  dplyr::filter(DetergentType == "None" & BlotTime == 6.83) %>%
  ggplot(aes(x = Concentration, y = desirability)) + 
  geom_line() + 
  theme_bw()

p2 <- optim_df %>%
  dplyr::filter(DetergentType == "None" & Concentration == 1) %>%
  ggplot(aes(x = BlotTime, y = desirability)) + 
  geom_line() + 
  theme_bw()

p3 <- optim_df %>%
  dplyr::filter(BlotTime == 6.5 & Concentration == 1) %>%
  ggplot(aes(x = DetergentType, y = desirability,  group = 1)) + 
  geom_point() + 
  geom_line() + 
  theme_bw()

p1|p2|p3
