library(bruceR)
library(igraph)
library(BGGM)
set.wd()
source("../subfunctions.R")
sink(file = "../Res_1_Logs/Log_4_1_Res_BayesianGGM_FullSample.txt",
     append = F,
     split = T)

dat <- import("../Res_3_IntermediateData/regressed_data.rda")

Item_dat <- dat %>%
  dplyr::select(starts_with("PU")) 

# Estimate the Bayesian Gaussian Graphical Model --------------------------

Net_Bayes_FullSample <- Item_dat %>%
  estimate(type = "continuous",
           analytic = F,
           iter = 50000,
           seed = 123)

summary(Net_Bayes_FullSample)
export(Net_Bayes_FullSample,
       file = "../Res_2_Results/Obj_BayesNet_FullSample.rds")

# Custom Computing Node Centrality Indices --------------------------------

communities = c("PUSN","PUSN","PUSN","PUSN","PUSN","PUSN",
                "PUS", "PUS", "PUS", "PUS", "PUS", "PUS",
                "PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG")
centrality_metrics <- c("Strength","Closeness","Betweenness",
                        "Bridge Strength", "Bridge Closeness", "Bridge Betweenness")
centrality_results <- list()

for (metric in centrality_metrics) {
  f <- function(x, ...) calc_centrality(x, metric, communities)
  tmpres_ls <- roll_your_own(object = Net_Bayes_FullSample,
                                                FUN = f,
                                                iter = 5000,
                                                select = T,
                                                cred = 0.95)
  tmpres_df <- extractRollYourOwn(tmpres_ls)
  tmpres_df$Node <- colnames(Net_Bayes_FullSample$Y) %>%
    refactorNode()
  tmpres_df <- tmpres_df %>%
    dplyr::select(-Post.sd) %>%
    dplyr::rename(
      `Posterior Mean` = `Post.mean`,
      `CIlower` = `Cred.lb`,
      `CIupper` = `Cred.ub`
    )
  tmpres_df$Measures <- metric
  centrality_results[[metric]] <- tmpres_df
}

T_Centrality <- dplyr::bind_rows(centrality_results)

export(T_Centrality,"../Res_2_Results/T_Res_BayesNet_Centrality_FullSample.rda")

sink()