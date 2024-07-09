library(bootnet)
library(NetworkComparisonTest)
library(qgraph)
library(bruceR)
set.wd()
sink(file = "../Res_1_Logs/Log_2_1_Res_NetEst_FullSample.txt",
     append = F,
     split = T)

# Save Network Estimation Parameter Settings ------------------------------

Num_lambda = 10000
MinRatio_lambda = 0.001
Threshold_Flag = T
save(Num_lambda,MinRatio_lambda,Threshold_Flag,
     file = './ParaSet_NetEst.rda')

# Estimate the Psychological Network in Full Sample -----------------------


dat <- import("../Res_3_IntermediateData/regressed_data.rda")


Item_dat <- dat %>%
  select(starts_with("PU"))


net_fullsample <- Item_dat %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)
summary(net_fullsample)
plot(net_fullsample)
centralityPlot(net_fullsample)

# Bootstrapping the Estimated Network -------------------------------------\

Res_bootstrap <- bootnet(Item_dat,
                         nBoots = 5000,
                         default = "EBICglasso",
                         type = "nonparametric",
                         nCores = 8,
                         statistics = c("edge",
                                        "strength",
                                        "closeness",
                                        "betweenness",
                                        "bridgeStrength",
                                        "bridgeCloseness",
                                        "bridgeBetweenness"),
                         model = "GGM",
                         communities = c("PUSN","PUSN","PUSN","PUSN","PUSN","PUSN",
                                         "PUS", "PUS", "PUS", "PUS", "PUS", "PUS",
                                         "PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG"),
                         nlambda = Num_lambda,
                         lambda.min.ratio = MinRatio_lambda,
                         threshold = Threshold_Flag)


T_Res_Boots <- summary(Res_bootstrap) %>%
  as.data.frame() %>%
  select(c(type,id,node1,node2,
           sample,CIlower,CIupper))

# Save Results ------------------------------------------------------------

export(net_fullsample,"../Res_2_Results/Obj_Net_Fullsample.rds")
export(Res_bootstrap,"../Res_2_Results/Obj_Net_Fullsample_bootstrap.rds")
export(T_Res_Boots,"../Res_2_Results/Res_Net_NodeEdge_Fullsample.rda")

