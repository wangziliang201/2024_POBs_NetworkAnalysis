library(bootnet)
library(EGAnet)
library(bruceR)
set.wd()
# bruceR::set.wd()
net <- import("../Res_2_Results/Obj_Net_Fullsample.rds")


res_infomap <- community.detection(net$graph,
                                   algorithm = "infomap",
                                   nb.trials = 100,
                                   membership.only = F)

res_walktrap <- community.detection(net$graph,
                                    algorithm = "walktrap",
                                    steps  = 10,
                                    membership.only = F)

res_greedy <- community.detection(net$graph,
                                  algorithm = "fast_greedy",
                                  membership.only = F)

res_leiden <- community.detection(net$graph,
                                  n_iterations  = 5000,
                                  membership.only = F)

res_louvain <- community.consensus(net$graph,
                                   consensus.method = "most_common",
                                   consensus.iter = 5000,
                                   membership.only = F)

T_Community <- data.frame("Infomap" = res_infomap$membership,
                          "Walktrap" = res_walktrap$membership,
                          "Fast Greedy" = res_greedy$membership,
                          "Leiden" = res_leiden$membership,
                          "Louvain" = res_louvain$selected_solution)

export(T_Community,"../Res_2_Results/T_Res_CommunityIdentify.rda")



