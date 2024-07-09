library(bootnet)
library(NetworkComparisonTest)
library(bruceR)
set.wd()
sink(file = "../Res_1_Logs/Log_2_2_Res_NetEst_SexSpecific_Comparison.txt",
     append = F,
     split = T)

# Load Network Estimation Parameters --------------------------------------

load('./ParaSet_NetEst.rda')

# Split full sample to sex-specific subsamples ----------------------------

dat <- import("../Res_3_IntermediateData/regressed_data.rda")

Item_dat_male <- dat %>%
  filter(Sex == "Male") %>%
  select(starts_with("PU"))

Item_dat_female <- dat %>%
  filter(Sex == "Female") %>%
  select(starts_with("PU"))

# Estimate Sex-specific Psychological Networks ----------------------------

net_male <- Item_dat_male %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)

net_female <- Item_dat_female %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)

# Save Results ------------------------------------------------------------

export(net_male,"../Res_2_Results/Obj_Net_male.rds")
export(net_female,"../Res_2_Results/Obj_Net_female.rds")

# Compare the two networks ------------------------------------------------

Res_compsex <- NCT(Item_dat_male,Item_dat_female,
                   it = 10000,
                   weighted = T,
                   abs = F,
                   test.edges = T,
                   p.adjust.methods = "fdr",
                   test.centrality = T,
                   centrality = c("strength","closeness","betweenness",
                                  "bridgeStrength","bridgeCloseness","bridgeBetweenness"),
                   communities = c("PUSN","PUSN","PUSN","PUSN","PUSN","PUSN",
                                   "PUS", "PUS", "PUS", "PUS", "PUS", "PUS",
                                   "PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG"),
                   estimator = estimateNetwork,
                   estimatorArgs = list("default" = "EBICglasso",
                                        "nlambda" = Num_lambda,
                                        "lambda.min.ratio" = MinRatio_lambda,
                                        "threshold" = Threshold_Flag))
summary(Res_compsex)

# Save Results ------------------------------------------------------------

export(Res_compsex,"../Res_2_Results/Res_NCT_CompSex.rds")
