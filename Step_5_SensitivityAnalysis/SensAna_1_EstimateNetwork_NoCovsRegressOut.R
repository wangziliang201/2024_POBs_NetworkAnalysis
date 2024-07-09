library(bruceR)
library(bootnet)
library(NetworkComparisonTest)
set.wd()
source("../subfunctions.R")
sink(file = "../Res_1_Logs/Log_5_1_Res_NoCovsAdjusted.txt",
     append = F,
     split = T)

load("../Step_2_NetworkAnalysis/ParaSet_NetEst.rda")
dat <- import("../Res_3_IntermediateData/prepared_data.rda")

Item_dat <- dat %>%
  select(starts_with("PU"))

# Full sample -------------------------------------------------------------

net_fullsample <- Item_dat %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)
fprintf("|==========================FUll Sample==========================|\n")
summary(net_fullsample)
Items <- data.frame("Item_label" = colnames(net_fullsample$graph) )
Items$order <- seq_len(nrow(Items))

wording <- import("../Res_3_IntermediateData/QuestionnaireItemWording.xlsx")

Items <- merge(Items,wording,
               by.x = "Item_label",
               all.x = T)
Items <- Items[order(Items$order), ]
Items$Community <- Items$Item_label %>%
  stringr::str_remove_all("_\\d")
png(file = "../Res_4_Reports/Figure_S5_A.png", width = 12, height = 8,units = 'in',res = 600)
plot_custom(net_fullsample, Items,layout_type = "spring",legend.cex = NULL,legend = F)
dev.off()

# Sex-specific Group ------------------------------------------------------

Item_dat_Males <- dat %>%
  filter(Sex == "Male") %>%
  select(starts_with("PU"))
Item_dat_Females <- dat %>%
  filter(Sex == "Female") %>%
  select(starts_with("PU"))
net_male <- Item_dat_Males %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)
fprintf("|==========================Male Sample==========================|\n")
summary(net_male)
png(file = "../Res_4_Reports/Figure_S5_B.png", width = 6, height = 4,units = 'in',res = 600)
layout_comp <- plot_custom(net_male, Items,layout_type = "spring",legend.cex = NULL,legend = F)
dev.off()

net_female <- Item_dat_Females %>%
  estimateNetwork(default = "EBICglasso",
                  nlambda = Num_lambda,
                  lambda.min.ratio = MinRatio_lambda,
                  threshold = Threshold_Flag)
fprintf("|==========================Female Sample==========================|\n")
summary(net_female)
png(file = "../Res_4_Reports/Figure_S5_C.png", width = 6, height = 4,units = 'in',res = 600)
plot_custom(net_female, Items,layout_type = layout_comp,legend.cex = NULL,legend = F)
dev.off()

Res_compsex <- NCT(Item_dat_Males,Item_dat_Females,
                   it = 5000,
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
fprintf("|==========================Compare Males and Females Network==========================|\n")
summary(Res_compsex)

sink()
