library(bruceR)
library(igraph)
library(BGGM)
set.wd()
source("../subfunctions.R")
sink(file = "../Res_1_Logs/Log_4_2_Res_BayesianGGM_PUScutoff.txt",
     append = F,
     split = T)

# Split data to PUS and Non-PUS groups ------------------------------------

dat_label <- import("../Res_3_IntermediateData/prepared_data.rda") %>%
  mutate(PUS_Total = SUM(.,var = "PUS_",items = 1:6)) %>%
  mutate(Label_PUS = as.numeric(PUS_Total > 23))

dat <- import("../Res_3_IntermediateData/regressed_data.rda") %>%
  cbind(.,dplyr::select(dat_label,Label_PUS))

Item_dat_PUS <- dat %>%
  filter(Label_PUS == 1) %>%
  dplyr::select(starts_with("PU"))

Item_dat_NonPUS <- dat %>%
  filter(Label_PUS == 0) %>%
  dplyr::select(starts_with("PU"))

# Estimate Group-specific Bayesian Gaussian Network -----------------------

Net_Bayes_PUS <- Item_dat_PUS %>%
  estimate(type = "continuous",
           analytic = F,
           iter = 5000,
           seed = 123)

summary(Net_Bayes_PUS)

export(Net_Bayes_PUS,
       file = "../Res_2_Results/Obj_BayesNet_PUS.rds")

Net_Bayes_NonPUS <- Item_dat_NonPUS %>%
  estimate(type = "continuous",
           analytic = F,
           iter = 5000,
           seed = 123)

summary(Net_Bayes_NonPUS)

export(Net_Bayes_NonPUS,
       file = "../Res_2_Results/Obj_BayesNet_NonPUS.rds")

# Deprecated By Memory Limits ---------------------------------------------
# # Compare the Two Bayesian Networks: Global and Node-wise -----------------
# res_global <- ggm_compare_ppc(Item_dat_PUS,Item_dat_NonPUS,
#                               test = 'global',
#                               iter = 1000,
#                               seed = 123)
# export(res_global,
#        file = "../Res_2_Results/Res_BayesNet_CompPUS_Global.rds")
# 
# res_nodewise <- ggm_compare_ppc(Item_dat_PUS,Item_dat_NonPUS,
#                               test = 'nodewise',
#                               iter = 1000,
#                               seed = 123)
# export(res_global,
#        file = "../Res_2_Results/Res_BayesNet_CompPUS_Nodewise.rds")


# Custom Compare the Two Bayesian Networks --------------------------------

res_global_strength <- ggm_compare_ppc(Item_dat_PUS,Item_dat_NonPUS,
                                test = 'global',
                                iter = 1000,
                                FUN = BayesComp_strength,
                                custom_obs = BayesComp_strength(Item_dat_PUS,Item_dat_NonPUS))
print(res_global_strength)
# Compare the Two Bayesian Networks: Edge-wise ---------------------------

Obj_Comp_Bayes_Edge <- ggm_compare_estimate(Item_dat_PUS,Item_dat_NonPUS,
                                       type = 'continuous',
                                       iter = 5000,
                                       seed = 123)
export(Obj_Comp_Bayes_Edge,
       file = "../Res_2_Results/Res_BayesNet_CompPUS_Edgewise.rds")

Res_Comp_Bayes_Edge <- summary(Obj_Comp_Bayes_Edge)

export(Res_Comp_Bayes_Edge$dat_results[[1]],
       file = "../Res_2_Results/T_Res_BayesNet_CompPUS_Edgewise.rda")

# Extract Centrality Indices Value ----------------------------------------

T_Centrality_NonPUS <- BayesNet_CentralityTable(Net_Bayes_NonPUS)
T_Centrality_PUS <- BayesNet_CentralityTable(Net_Bayes_PUS)

export(T_Centrality_NonPUS,
       "../Res_2_Results/T_Res_BayesNet_Centrality_NonPUS.rda")
export(T_Centrality_PUS,
       "../Res_2_Results/T_Res_BayesNet_Centrality_PUS.rda")


sink()