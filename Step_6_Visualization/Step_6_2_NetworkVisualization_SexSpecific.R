library(bruceR)
library(qgraph)

set.wd()
source("../subfunctions.R")

sink(file = "../Res_1_Logs/Log_6_2_Res_Network_Sex.txt",
     append = F,
     split = T)

# Visualize Group-specific Network ----------------------------------------


net_male <- import("../Res_2_Results/Obj_Net_male.rds")
fprintf("|============================Males Group============================|\n")
summary(net_male)
Items <- generateItemsdf(net_male)
svg(file = "../Res_4_Reports/Figure_2_A.svg", width = 6, height = 4)
layout_comp <- plot_custom(net_male, Items,layout_type = "spring",legend.cex = NULL,legend = F)
dev.off()
png(file = "../Res_4_Reports/Figure_2_A.png", width = 6, height = 4,units = 'in',res = 600)
plot_custom(net_male, Items,layout_type = layout_comp,legend.cex = NULL,legend = F)
dev.off()


net_female <- import("../Res_2_Results/Obj_Net_female.rds")
fprintf("|============================Females Group============================|\n")
summary(net_female)
Items <- generateItemsdf(net_female)
svg(file = "../Res_4_Reports/Figure_2_B.svg", width = 6, height = 4)
plot_custom(net_female, Items,layout_type = layout_comp,legend.cex = NULL,legend = F)
dev.off()
png(file = "../Res_4_Reports/Figure_2_B.png", width = 6, height = 4,units = 'in',res = 600)
plot_custom(net_female, Items,layout_type = layout_comp,legend.cex = NULL,legend = F)
dev.off()

# Core Symptoms: Plot Group-differences in Nodes Attributes ------------------------------

df <- compareCentrality(net_male,net_female,
                        include = c("Strength","Closeness","Betweenness"),
                        legendName = "Networks by Sex",
                        net1Name = "Males",
                        net2Name = "Females")
library(NetworkComparisonTest)
net_NCT <- import("../Res_2_Results/Res_NCT_CompSex.rds")
summary(net_NCT)

res_1 <- extractSigNCT_Node(net_NCT,index = "strength")

res_2 <- extractSigNCT_Node(net_NCT,index = "closeness")

res_3 <- extractSigNCT_Node(net_NCT,index = "betweenness")

Res_CoreSymptoms <- rbind(res_1,res_2,res_3) %>%
  mutate(measure = renameMeasure(measure)) %>%
  mutate(measure = factor(measure,
                          c("Strength","Closeness","Betweenness")))

# 合并数据框
sig_df <- df %>%
  left_join(Res_CoreSymptoms, by = c("node" = "node", "measure" = "measure"))

library(ggtext)

p <- ggplot(data = sig_df, aes(x = node, y = value, group = graph)) +
  geom_line(aes(linetype = graph, color = graph), linewidth = 0.8) +
  geom_point(aes(shape = graph, color = graph), na.rm = TRUE, size = 2) +
  scale_linetype_manual(name = "Graph", values = c("Males" = "solid", "Females" = "dashed")) +
  scale_shape_manual(name = "Graph", values = c("Males" = 16, "Females" = 17)) +
  scale_color_manual(name = "Graph", values = c("Males" = "black", "Females" = "lightgrey")) +
  scale_x_discrete(labels = function(labels) { 
    color_labels <- df$color[match(labels, df$node)]
    mapply(function(label, color) paste0("<span style='color:", color, ";'>", label, "</span>"), labels, color_labels)
  }) +
  coord_flip() +
  facet_grid(~measure, scales = "free") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10),   # Y轴文本大小
    plot.subtitle = element_blank(),        # 去除副标题
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(size = 12),   # facet 标签文本大小
    legend.text = element_text(size = 10)   # 图例文本大小
  ) +
  theme(axis.text.y = element_markdown()) +
  xlab("Node") +
  geom_text(aes(label = ifelse(!is.na(pval), "*", ""), y = value), color = "darkred",size = 9, vjust = 0.7,hjust = -0.5)

print(p)

ggsave(filename = "../Res_4_Reports/Figure_2_C.svg",
       plot = p,
       width = 10, height = 6, units = "in", device = "svg")

# Bootstrapping group-specific Network-----------------------------------
library(bootnet)
if (file.exists("../Res_2_Results/Obj_Net_male_bootstrap.rds")){
  net_male <- import("../Res_2_Results/Obj_Net_male_bootstrap.rds")
}else{
  Num_lambda = 10000
  MinRatio_lambda = 0.001
  Threshold_Flag = T
  net_male <- bootnet(net_male,
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
  export(net_PUS,"../Res_2_Results/Obj_Net_male_bootstrap.rds")
}

if (file.exists("../Res_2_Results/Obj_Net_female_bootstrap.rds")){
  net_female <- import("../Res_2_Results/Obj_Net_female_bootstrap.rds")
}else{
  Num_lambda = 10000
  MinRatio_lambda = 0.001
  Threshold_Flag = T
  net_female <- bootnet(net_female,
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
  export(net_NonPUS,"../Res_2_Results/Obj_Net_female_bootstrap.rds")
}
# Bridge Symptoms: Plot Differences ---------------------------------------

res_1 <- summary(net_male,statistics = c("bridgeStrength","bridgeCloseness","bridgeBetweenness")) %>%
  as.data.frame() %>%
  select(type,id,sample) %>%
  rename(measure = type,
         node = id,
         value = sample) %>%
  mutate(graph = "Males")
res_2 <- summary(net_female,statistics = c("bridgeStrength","bridgeCloseness","bridgeBetweenness")) %>%
  as.data.frame() %>%
  select(type,id,sample) %>%
  rename(measure = type,
         node = id,
         value = sample) %>%
  mutate(graph = "Females")

df <- rbind(res_1,res_2) %>%
  mutate(measure = renameMeasure(measure)) %>%
  mutate(measure = factor(measure,
                          c("Bridge Strength","Bridge Closeness","Bridge Betweenness"))) %>%
  mutate(node = refactorNode(node))


res_1 <- extractSigNCT_Node(net_NCT,index = "bridgeStrength")

res_2 <- extractSigNCT_Node(net_NCT,index = "bridgeCloseness")

res_3 <- extractSigNCT_Node(net_NCT,index = "bridgeBetweenness")

Res_BridgeSymptoms <- rbind(res_1,res_2,res_3) %>%
  mutate(measure = renameMeasure(measure)) %>%
  mutate(measure = factor(measure,
                          c("Bridge Strength","Bridge Closeness","Bridge Betweenness")))

# 合并数据框
sig_df <- df %>%
  left_join(Res_BridgeSymptoms, by = c("node" = "node", "measure" = "measure")) %>%
  mutate(node = forcats::fct_rev(node)) %>%
  mutate(color = case_when(
    grepl("PUS ", node) ~ "lightblue",
    grepl("PUSN ", node) ~ "lightsalmon",
    grepl("PUVG ", node) ~ "lightgreen",
    TRUE ~ "black"
  ))

p <- ggplot(data = sig_df, aes(x = node, y = value, group = graph)) +
  geom_line(aes(linetype = graph, color = graph), linewidth = 0.8) +
  geom_point(aes(shape = graph, color = graph), na.rm = TRUE, size = 2) +
  scale_linetype_manual(name = "Graph", values = c("Males" = "solid", "Females" = "dashed")) +
  scale_shape_manual(name = "Graph", values = c("Males" = 16, "Females" = 17)) +
  scale_color_manual(name = "Graph", values = c("Males" = "black", "Females" = "lightgrey"))+
  scale_x_discrete(labels = function(labels) {
    color_labels <- sig_df$color[match(labels, sig_df$node)]
    mapply(function(label, color) paste0("<span style='color:", color, ";'>", label, "</span>"), labels, color_labels)
  }) +
  coord_flip() +
  facet_grid(~measure, scales = "free") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10),   # Y轴文本大小
    plot.subtitle = element_blank(),        # 去除副标题
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(size = 12),   # facet 标签文本大小
    legend.text = element_text(size = 10)   # 图例文本大小
  ) +
  theme(axis.text.y = element_markdown()) +
  xlab("Node") +
  geom_text(aes(label = ifelse(!is.na(pval), "*", ""), y = value), color = "darkred",size = 9, vjust = 0.7,hjust = -0.5)

print(p)

ggsave(filename = "../Res_4_Reports/Figure_2_D.svg",
       plot = p,
       width = 10, height = 6, units = "in", device = "svg")

sink()
