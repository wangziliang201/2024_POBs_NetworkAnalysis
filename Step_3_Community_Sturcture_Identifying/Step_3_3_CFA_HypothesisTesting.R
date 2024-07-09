library(bruceR)
library(lavaan)
set.wd()

sink(file = "../Res_1_Logs/Log_3_3_Res_CFA_Hypothesis_Testing.txt",
     append = F,
     split = T)

dat <- import("../Res_3_IntermediateData/regressed_data.rda")


# Test our hypothesis by CFA Model ----------------------------------------

model_specification = "
  Preoccupation =~ PUS_1 + PUSN_1 + PUVG_1
  LostControl  =~ PUS_6 + PUSN_4 + PUVG_4
  Escapsim =~ PUS_3 + PUSN_3 +PUVG_8 
  Withdrawal =~ PUS_5 + PUSN_5 + PUVG_2
  Tolerance =~ PUS_4 + PUSN_2 + PUVG_3
  Conflicts =~ PUS_2 + PUVG_6
  Impairments =~ PUSN_6 + PUVG_9
  Others =~ PUVG_5 + PUVG_7
"
CFA_mdl_H <- dat %>%
  select(starts_with("PU")) %>%
  cfa(model = model_specification,
      estimator = "ML",
      orthogonal = F)

summary(CFA_mdl_H,fit.measures = T)

Res_ModInd <- modificationIndices(CFA_mdl_H,sort = T, minimum.value = 1000)
print(Res_ModInd)

lavaan_summary(CFA_mdl_H,ci = "bca.boot",covariance  = T,
               nsim = 5000,
               digits = 2,
               file = "../Res_2_Results/Res_CFA_TestHypothesis.doc")
export(CFA_mdl_H,
       file = "../Res_2_Results/Obj_CFA_FullSample_HypothesisTest.rds")
# Estimate the CFA Model indicated by EFA in Overall Sample ---------------

model_specification = "
  PUSN =~ PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6
  PUS  =~ PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6
  PUVG =~ PUVG_1 + PUVG_2 + PUVG_3 + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9
"
CFA_mdl_D <- dat %>%
  select(starts_with("PU")) %>%
  cfa(model = model_specification,
      estimator = "ML",
      orthogonal = F)

summary(CFA_mdl_D,fit.measures = T)
lavaan_summary(CFA_mdl_D,ci = "bca.boot",covariance  = T,
               nsim = 5000,
               digits = 2,
               file = "../Res_2_Results/Res_CFA_DataDriven.doc")
export(CFA_mdl_D,
       file = "../Res_2_Results/Obj_CFA_FullSample_DataDriven.rds")
