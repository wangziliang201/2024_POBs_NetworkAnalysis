library(caret)
library(bruceR)
set.wd()

set.seed(123)

sink(file = "../Res_1_Logs/Log_3_2_Res_EFA_CFA_SplitHalf.txt",
     append = F,
     split = T)

dat <- import("../Res_3_IntermediateData/regressed_data.rda")

SubsampleIndex <- createDataPartition(dat$Sex,
                                      p = 0.5,
                                      list = F)

table(dat[SubsampleIndex,'Sex'])
table(dat[-SubsampleIndex,'Sex'])

Discovery <- dat[SubsampleIndex,]
Validation <- dat[-SubsampleIndex,]

# Exploratory Factor Analysis ---------------------------------------------

EFA_Result <- Discovery %>%
  select(starts_with("PU")) %>%
  EFA(vars = colnames(.),
      method = "pa",
      nfactors = "parallel",
      hide.loadings = 0,
      file = "../Res_2_Results/T_Res_EFA_DiscoverySample.doc")

export(EFA_Result$loadings,"../Res_2_Results/Res_EFA_DiscoverySample.rda")

# Confirmatory Factor Analysis --------------------------------------------
library(lavaan)
model_specification = "
  PUSN =~ PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6
  PUS  =~ PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6
  PUVG =~ PUVG_1 + PUVG_2 + PUVG_3 + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9
"
CFA_Result <- Validation %>%
  select(starts_with("PU")) %>%
  CFA(model = model_specification,
      estimator = "ML",
      orthogonal = F,
      file = "../Res_2_Results/T_Res_CFA_ValidationSample.doc")


CFA_loadings <- standardizedSolution(CFA_Result)
export(CFA_loadings,"../Res_2_Results/Res_CFA_ValidationSample.rda")

sink()