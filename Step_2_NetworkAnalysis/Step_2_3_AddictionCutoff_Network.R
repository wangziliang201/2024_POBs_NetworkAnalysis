library(bootnet)
library(NetworkComparisonTest)
library(bruceR)
set.wd()
sink(file = "../Res_1_Logs/Log_2_3_Res_NetEst_PUScutoff_Comparison.txt",
     append = F,
     split = T)

# Load Network Estimation Parameters --------------------------------------

load('./ParaSet_NetEst.rda')

# Generate the Individual Label for two groups ----------------------------
dat_label <- import("../Res_3_IntermediateData/prepared_data.rda") %>%
  mutate(PUS_Total = SUM(.,var = "PUS_",items = 1:6)) %>%
  mutate(Label_PUS = as.numeric(PUS_Total > 23))

dat <- import("../Res_3_IntermediateData/regressed_data_sex.rda") %>%
  cbind(.,dplyr::select(dat_label,Label_PUS))

Item_dat_PUS <- dat %>%
  filter(Label_PUS == 1) %>%
  dplyr::select(starts_with("PU"))

Item_dat_NonPUS <- dat %>%
  filter(Label_PUS == 0) %>%
  dplyr::select(starts_with("PU"))

# Estimate the PUS and Non-PUS Networks -----------------------------------

net_PUS <- Item_dat_PUS %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)

net_NonPUS <- Item_dat_NonPUS %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)

# Save Results ------------------------------------------------------------

export(net_PUS,
       file = "../Res_2_Results/Obj_Net_PUS.rds")
export(net_NonPUS,
       file = "../Res_2_Results/Obj_Net_NonPUS.rds")

# Compare the two Estimated Networks --------------------------------------

Res_compPUS <- NCT(Item_dat_PUS,Item_dat_NonPUS,
                   gamma = 0.5,
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

summary(Res_compPUS)

# Save Results ------------------------------------------------------------

export(Res_compPUS,"../Res_2_Results/Res_NCT_CompPUS.rds")
