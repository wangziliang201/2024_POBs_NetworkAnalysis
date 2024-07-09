library(bruceR)
library(psych)
library(lavaan)
library(semTools)
set.wd()
source("../subfunctions.R")

sink(file = "../Res_1_Logs/Log_1_2_Res_CMB_Test.txt",
     append = F,
     split = T)

dat <- import("../Res_3_IntermediateData/prepared_data.rda")


# Harman's Single Factor Test ---------------------------------------------

Res_pca <- dat %>%
  select(starts_with("PU")) %>%
  principal(nfactors = 1,rotate = "none")

eigen_values <- Res_pca$values
first_component_variance_ratio <- eigen_values[1] / sum(eigen_values)

fprintf("------Harman's Single Factor Test------")
fprintf("| Explained Variance Ratio by the first factor: %2.2f%% |",first_component_variance_ratio*100)


# SEM-based CMB Test ------------------------------------------------------

m0 <- "
  # Measurement Model
  PUSN =~ PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6
  PUS =~ PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6
  PUVG =~ PUVG_1 + PUVG_2 + PUVG_3 + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9
"
m0_mdl <- sem(m0,data = dat)

m1 <- "
  # Measurement Model
  PUSN =~ PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6
  PUS =~ PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6
  PUVG =~ PUVG_1 + PUVG_2 + PUVG_3 + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9
  # Define a method factor
  Method =~ PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6 + PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6 + PUVG_1 + PUVG_2 + PUVG_3  + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9
   # Set covariances between latent variables to zero
  PUSN ~~ 0*PUS
  PUSN ~~ 0*PUVG
  PUSN ~~ 0*Method
  PUS ~~ 0*PUVG
  PUS ~~ 0*Method
  PUVG ~~ 0*Method
"
m1_mdl <- sem(m1,data = dat)

summary(m1_mdl, fit.measures = TRUE, standardized = TRUE)

Res_CompFit <- compareFit(m0_mdl,m1_mdl)

summary(Res_CompFit)

anova(m0_mdl, m1_mdl)



