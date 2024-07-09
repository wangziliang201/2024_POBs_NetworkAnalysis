library(bruceR)
library(bootnet)
library(qgraph)
set.wd()
source("../subfunctions.R")

sink(file = "../Res_1_Logs/Log_6_1_Res_Network_in_FullSample.txt",
     append = F,
     split = T)

# Network Visualization and reporting some details ------------------------

net <- import("../Res_2_Results/Obj_Net_Fullsample.rds")

fprintf("Smallest EBIC = %.2f correspoding lambda = %.2f * 10^-3\n",
        net$results$ebic[which.min(net$results$ebic)],
        net$results$lambda[which.min(net$results$ebic)]*1e3)

Num_NonZeroEdges = sum( net$graph < 0 ) / 2 + sum( net$graph > 0 ) / 2
Num_PosEdges = sum( net$graph > 0 ) / 2
Num_NegEdges = sum( net$graph < 0 ) / 2

fprintf("The number of Negative Edges: %d (%2.2f%%) | The number of Postive Edges: %d (%2.2f%%)\n",
        Num_NegEdges,Num_NegEdges/Num_NonZeroEdges*100,
        Num_PosEdges,Num_PosEdges/Num_NonZeroEdges*100)


Items <- data.frame("Item_label" = colnames(net$graph) )
Items$order <- seq_len(nrow(Items))

wording <- import("../Res_3_IntermediateData/QuestionnaireItemWording.xlsx")

Items <- merge(Items,wording,
               by.x = "Item_label",
               all.x = T)
Items <- Items[order(Items$order), ]
Items$Community <- Items$Item_label %>%
  stringr::str_remove_all("_\\d")


# Network Visualization ---------------------------------------------------

png(file = "../Res_4_Reports/Figure_1_A.png", width = 14, height = 8,units = "in",res = 600) 

plot_custom(net, Items,layout_type = "spring",legend.cex = 0.7)

dev.off()

# Visualization for redundant analysis results ----------------------------

Res_Redundant <- import("../Res_2_Results/Res_Item_RedundantAnalysis.rda")

library(reshape2)
matrix <- Res_Redundant$wto$matrix

# 将矩阵下对角阵提取出来
lower_triangle <- matrix
lower_triangle[upper.tri(lower_triangle)] <- NA

df <- melt(lower_triangle, na.rm = TRUE) 

# 为不同数值范围设置颜色
df$color <- cut(df$value,
                breaks = c(0.00, 0.2, 0.25, 0.30, Inf),
                labels = c("<0.20", "0.20-0.25", "0.25-0.30", ">0.30"),
                right = FALSE)
levels_order <- c(
  "PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
  "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
  "PUVG_1", "PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"
)

labels_order <- c(
  "PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
  "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
  "PUVG 1", "PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9"
)


# 创建一个包含所有颜色的虚拟数据框
dummy_data <- data.frame(
  Var1 = factor(rep("dummy", 4), levels = levels_order, labels = labels_order),
  Var2 = factor(rep("dummy", 4), levels = levels_order, labels = labels_order),
  value = c(NA, NA, NA, NA),
  color = factor(c("<0.20", "0.20-0.25", "0.25-0.30", ">0.30"), levels = c("<0.20", "0.20-0.25", "0.25-0.30", ">0.30"))
)

# 将虚拟数据添加到原始数据中
df <- rbind(df, dummy_data)

# 绘制矩阵下对角阵并逆时针旋转90度
p <- ggplot(df, aes(x = Var1, y = Var2, fill = color)) +
  geom_tile(color = "white", show.legend = TRUE) + # 去除网格线
  scale_fill_manual(values = c("<0.20" = "#EEEEEE", "0.20-0.25" = "lightcoral", "0.25-0.30" = "red", ">0.30" = "darkred"),
                    drop = FALSE) + # 确保图例中包含所有颜色
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(), # 去除左侧的 y 轴刻度标签
        axis.ticks.y = element_blank(), # 去除左侧的 y 轴刻度线
        axis.text.y.right = element_text(hjust = 0, vjust = 0.5), # 保持右侧标签水平
        panel.grid = element_blank(),
        legend.position = "top",
        plot.margin = unit(c(1, 4, 1, 1), "lines")) + # 增加右侧空白边距以显示标签
  labs(x = "", y = "", fill = "Weighted Topological Overlap") +
  scale_x_discrete(expand = c(0, 0)) + # 显示 x 轴标签
  scale_y_discrete(expand = c(0, 0), position = "right") # 显示 y 轴标签，并调整到右侧
ggsave(filename = "../Res_4_Reports/Figure_S2.svg",
       plot = p,
       width = 6, height = 6, units = "in", device = "svg")
# Network Stability Analysis & Visualization -------------------------------
if (file.exists("../Res_2_Results/Obj_Net_Stability_FullSample.rds")){
  Res_Stability <- import("../Res_2_Results/Obj_Net_Stability_FullSample.rds")
}else{
  Res_Stability <- bootnet(net,
                           nBoots = 5000,
                           nCores = 8,
                           type = "case",
                           caseMin = 0.05,
                           caseMax = 0.75,
                           statistics = c('edge',
                                          'strength',
                                          'betweenness',
                                          'closeness'),
                           nlambda = Num_lambda,
                           lambda.min.ratio = MinRatio_lambda,
                           threshold = Threshold_Flag
  )
  export(Res_Stability,
         "../Res_2_Results/Obj_Net_Stability_FullSample.rds")
}


corStability(Res_Stability,cor = 0.7)

p <- plot(Res_Stability,'all')
p_new <- p +
  ylim(0,1) +
  geom_hline(yintercept = 0.7, linetype = "solid",linewidth = 1, color = "gray") + 
  annotate("text", x = 6825, y = 0.75, label = expression(italic(r) == 0.7),
           hjust = 0, size = 20 / .pt , color = "gray") + 
  theme_bruce() + 
  theme(
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed",linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "gray", linetype = "dashed",linewidth = 0.25),
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed",linewidth = 0.5),
    panel.grid.minor = element_blank(),  # 移除次要Y轴网格线
    legend.position = "top"
  )
p_new
ggsave(filename = "../Res_4_Reports/Figure_S3_A.svg",
      plot = p_new,
      width = 4, height = 6, units = "in", device = "svg")

p <- plot(Res_Stability,'edge')
p_new <- p +
  ylim(0,1) +
  geom_hline(yintercept = 0.7, linetype = "solid",linewidth = 1, color = "gray") + 
  annotate("text", x = 6825, y = 0.75, label = expression(italic(r) == 0.7),
           hjust = 0, size = 20 / .pt , color = "gray") + 
  theme_bruce() + 
  theme(
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed",linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "gray", linetype = "dashed",linewidth = 0.25),
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed",linewidth = 0.5),
    panel.grid.minor = element_blank(),  # 移除次要Y轴网格线
    legend.position = "top"
  )
p_new
ggsave(filename = "../Res_4_Reports/Figure_S3_B.svg",
       plot = p_new,
       width = 4, height = 6, units = "in", device = "svg")

# rm(list = ls())
# gc()

net_boots <- import("../Res_2_Results/Obj_Net_Fullsample_bootstrap.rds")


p <- plot(net_boots,
          labels = F,
          order = "sample")

p <- p + 
  theme_bruce() + 
  theme(
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed", linewidth = 0.5), # 添加Y轴网格线
    panel.grid.minor.x = element_line(color = "grey", linetype = "dashed", linewidth = 0.25), # 添加Y轴网格线
    axis.ticks.y = element_line(linewidth = 0.1),
    axis.text.y = element_blank(),          # 去除Y轴的刻度标签
    strip.text = element_blank(),
    legend.position = "top",                 # 将图例位置移到图上方
    legend.text = element_text(size = 12) 
  ) + 
  xlab("Edge Weights") + 
  ylab("Edge No.")
ggsave(filename = "../Res_4_Reports/Figure_S3_C.svg",
       plot = p,
       width = 4, height = 6, units = "in", device = "svg")

# Core Symptoms Visualization ---------------------------------------
library(ggplot2)
library(dplyr)
library(forcats)
library(ggtext)

# 创建颜色映射
T_Centrality <- summary(net_boots, statistics = c("strength", "closeness", "betweenness")) %>%
  as.data.frame() %>%
  select(c(type, id, sample, mean, CIlower, CIupper)) %>%
  mutate(id = factor(id, levels = c(
    "PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
    "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
    "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
    labels = c(
      "PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
      "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
      "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9"))
  ) %>%
  mutate(id = forcats::fct_rev(id)) %>%
  mutate(color = case_when(
    grepl("PUS ", id) ~ "lightblue",
    grepl("PUSN ", id) ~ "lightsalmon",
    grepl("PUVG ", id) ~ "lightgreen",
    TRUE ~ "black"
  ))

# 绘图
p <- ggplot(T_Centrality, aes(x = mean, y = id)) +
  geom_bar(stat = "identity", aes(fill = color), width = 0.7) +
  geom_errorbar(aes(xmin = CIlower, xmax = CIupper),color = "#999999", width = 0.2) +
  geom_point(aes(x = sample), color = "#444444", size = 1.5) +
  facet_wrap(~ factor(type,
                      levels = c("strength", "closeness", "betweenness"),
                      labels = c("Strength","Closeness","Betweenness")),
             scales = "free", ncol = 3) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),   # Y轴文本大小
    plot.subtitle = element_blank(),        # 去除副标题
    legend.position = "top",                # 图例位置
    strip.text = element_text(size = 12),   # facet 标签文本大小
    legend.text = element_text(size = 14)   # 图例文本大小
  ) +
  scale_fill_identity() +                    # 直接使用定义的颜色
  scale_y_discrete(labels = function(labels) { 
    color_labels <- T_Centrality$color[match(labels, T_Centrality$id)]
    mapply(function(label, color) paste0("<span style='color:", color, ";'>", label, "</span>"), labels, color_labels)
  }) +
  labs(y = "Node") +
  theme(axis.text.y = element_markdown())    # 使用 element_markdown 来支持颜色

# 打印绘图对象
print(p)

ggsave(filename = "../Res_4_Reports/Figure_1_B.svg",
       plot = p,
       width = 7, height = 4, units = "in", device = "svg")
# Bridge Symptoms Visualization -------------------------------------------

# 创建颜色映射
T_Centrality <- summary(net_boots, statistics = c("bridgeStrength", "bridgeCloseness", "bridgeBetweenness")) %>%
  as.data.frame() %>%
  select(c(type, id, sample, mean, CIlower, CIupper)) %>%
  mutate(id = factor(id, levels = c(
    "PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
    "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
    "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
    labels = c(
      "PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
      "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
      "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9"))
  ) %>%
  mutate(id = forcats::fct_rev(id)) %>%
  mutate(color = case_when(
    grepl("PUS ", id) ~ "lightblue",
    grepl("PUSN ", id) ~ "lightsalmon",
    grepl("PUVG ", id) ~ "lightgreen",
    TRUE ~ "black"
  ))

# 绘图
p <- ggplot(T_Centrality, aes(x = mean, y = id)) +
  geom_bar(stat = "identity", aes(fill = color), width = 0.7) +
  geom_errorbar(aes(xmin = CIlower, xmax = CIupper),color = "#999999", width = 0.2) +
  geom_point(aes(x = sample), color = "#444444", size = 1.5) +
  facet_wrap(~ factor(type,
                      levels = c("bridgeStrength", "bridgeCloseness", "bridgeBetweenness"),
                      labels = c("Bridge Strength", "Bridge Closeness", "Bridge Betweenness")),
             scales = "free", ncol = 3) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),   # Y轴文本大小
    plot.subtitle = element_blank(),        # 去除副标题
    legend.position = "top",                # 图例位置
    strip.text = element_text(size = 12),   # facet 标签文本大小
    legend.text = element_text(size = 14)   # 图例文本大小
  ) +
  scale_fill_identity() +                    # 直接使用定义的颜色
  scale_y_discrete(labels = function(labels) { 
    color_labels <- T_Centrality$color[match(labels, T_Centrality$id)]
    mapply(function(label, color) paste0("<span style='color:", color, ";'>", label, "</span>"), labels, color_labels)
  }) +
  labs(y = "Node") +
  theme(axis.text.y = element_markdown()) 

print(p)

ggsave(filename = "../Res_4_Reports/Figure_1_C.svg",
       plot = p,
       width = 7, height = 4, units = "in", device = "svg")



# Compare Core and Bridge Symptom Indices ---------------------------------
node_names <- net_boots$sampleTable %>%
  as.data.frame() %>%
  filter(type == "strength") %>%
  filter(value != 0) %>%
  .$id 
node_pairs <- combn(node_names,2,simplify = T)
node_pairs_df <- data.table(
  x = node_pairs[1, ],
  y = node_pairs[2, ]
) %>%
  unique() %>%
  mutate(measures = NA,
         sig.test = NA,
         lowCI = NA,
         upCI = NA)
measures_list <- c("strength","closeness","betweenness",
                   "bridgeStrength","bridgeCloseness","bridgeBetweenness")
for (j in measures_list){
  for (i in 1:nrow(node_pairs_df)){
    tmp_res <- differenceTest(net_boots,
                              x = node_pairs_df$x[i],
                              y = node_pairs_df$y[i],
                              measure = j,
                              alpha = 0.001)
    node_pairs_df$measures[i] <- j
    node_pairs_df$sig.test[i] <- tmp_res$significant
    node_pairs_df$lowCI[i] <- tmp_res$lower
    node_pairs_df$upCI[i] <- tmp_res$upper
  }
  export(node_pairs_df,
         "../Res_2_Results/T_Res_DiffTest_Nodes_FullSample.xlsx",
         sheet = j)
}
all_node_pairs_df <- data.frame()
for (j in measures_list){
  node_pairs_df <- import("../Res_2_Results/T_Res_DiffTest_Nodes_FullSample.xlsx",
                          sheet = j) %>%
    filter(sig.test == TRUE)
  Flag_WithinPUSN_x <- node_pairs_df$x %>%
    str_count(pattern = "PUSN_") == 1
  Flag_WithinPUS_x <- node_pairs_df$x %>%
    str_count(pattern = "PUS_") == 1
  Flag_WithinPUVG_x <- node_pairs_df$x %>%
    str_count(pattern = "PUVG_") == 1
  Flag_WithinPUSN_y <- node_pairs_df$y %>%
    str_count(pattern = "PUSN_") == 1
  Flag_WithinPUS_y <- node_pairs_df$y %>%
    str_count(pattern = "PUS_") == 1
  Flag_WithinPUVG_y <- node_pairs_df$y %>%
    str_count(pattern = "PUVG_") == 1
  Flag_WithPOBComp <- (Flag_WithinPUSN_x & Flag_WithinPUSN_y) |
    (Flag_WithinPUS_x & Flag_WithinPUS_y) |
    (Flag_WithinPUVG_x & Flag_WithinPUVG_y)
  node_pairs_df[Flag_WithPOBComp, ] <- NA
  node_pairs_df <- na.omit(node_pairs_df)
  
  Flag_PUS <- str_detect(node_pairs_df$y,"PUS_\\d")
  temp <- node_pairs_df$x[ Flag_PUS ]
  node_pairs_df$x[ Flag_PUS ] <- node_pairs_df$y[ Flag_PUS ]
  node_pairs_df$y[ Flag_PUS ] <- temp
  print(arrange(node_pairs_df,desc(x)))
  all_node_pairs_df <- rbind(all_node_pairs_df,node_pairs_df)
}
export(all_node_pairs_df,
       "../Res_2_Results/T_Res_DiffTest_Nodes_FullSample_Filtered.xlsx")
# Compare Edges -----------------------------------------------------------

edge_names <- net_boots$sampleTable %>%
  as.data.frame() %>%
  filter(type == "edge") %>%
  filter(value != 0) %>%
  .$id 
edge_pairs <- combn(edge_names,2,simplify = T)
edge_pairs_df <- data.table(
  x = edge_pairs[1, ],
  y = edge_pairs[2, ]
) %>%
  unique() %>%
  mutate(sig.test = NA,
         lowCI = NA,
         upCI = NA)

for (i in 1:nrow(edge_pairs_df)){
  tmp_res <- differenceTest(net_boots,
                            x = edge_pairs_df$x[i],
                            y = edge_pairs_df$y[i],
                            measure = c("edge"),
                            alpha = 0.001)
  edge_pairs_df$sig.test[i] <- tmp_res$significant
  edge_pairs_df$lowCI[i] <- tmp_res$lower
  edge_pairs_df$upCI[i] <- tmp_res$upper
}

export(edge_pairs_df,
       "../Res_2_Results/T_Res_DiffTest_Edges_FullSample.xlsx")

edge_pairs_df <- edge_pairs_df %>%
  filter(sig.test == TRUE)
Flag_WithinPUSN_x <- edge_pairs_df$x %>%
  str_count(pattern = "PUSN_") == 2
Flag_WithinPUS_x <- edge_pairs_df$x %>%
  str_count(pattern = "PUS_") == 2
Flag_WithinPUVG_x <- edge_pairs_df$x %>%
  str_count(pattern = "PUVG_") == 2
Flag_WithinPUSN_y <- edge_pairs_df$y %>%
  str_count(pattern = "PUSN_") == 2
Flag_WithinPUS_y <- edge_pairs_df$y %>%
  str_count(pattern = "PUS_") == 2
Flag_WithinPUVG_y <- edge_pairs_df$y %>%
  str_count(pattern = "PUVG_") == 2
Exclude_Flag_WithinComp <- (Flag_WithinPUSN_x & Flag_WithinPUSN_y) |
  (Flag_WithinPUS_x & Flag_WithinPUS_y) |
  (Flag_WithinPUVG_x & Flag_WithinPUVG_y)
# edge_pairs_df[Exclude_Flag_WithinComp,] <- NA
Flag_PUSNComp <- (( Flag_WithinPUSN_x & !(Flag_WithinPUS_y | Flag_WithinPUVG_y) )|
  ( Flag_WithinPUSN_y & !(Flag_WithinPUS_x | Flag_WithinPUVG_x) )) &
  !Exclude_Flag_WithinComp
Flag_PUSComp <- (( Flag_WithinPUS_x & !(Flag_WithinPUSN_y | Flag_WithinPUVG_y) )|
  ( Flag_WithinPUS_y & !(Flag_WithinPUSN_x | Flag_WithinPUVG_x) )) &
  !Exclude_Flag_WithinComp
Flag_PUVGComp <- (( Flag_WithinPUVG_x & !(Flag_WithinPUS_y | Flag_WithinPUSN_y) )|
  ( Flag_WithinPUVG_y & !(Flag_WithinPUS_x | Flag_WithinPUSN_x) )) &
  !Exclude_Flag_WithinComp

c(edge_pairs_df[Flag_PUSNComp & (edge_pairs_df$upCI < 0),"x"],
  edge_pairs_df[Flag_PUSNComp & (edge_pairs_df$upCI < 0),"y"]) %>%
  unique() %>%
  .[str_count(.,"PUSN_") == 2] %>%
  length() %>%
  fprintf("%d (%2.2f) Edges within PUSN showed stronger weights than between-POB edges\n",
          .,
          ./(6*5/2)*100)
c(edge_pairs_df[Flag_PUSComp & (edge_pairs_df$upCI < 0),"x"],
  edge_pairs_df[Flag_PUSComp & (edge_pairs_df$upCI < 0),"y"]) %>%
  unique() %>%
  .[str_count(.,"PUS_") == 2] %>%
  length() %>%
  fprintf("%d (%2.2f) Edges within PUS showed stronger weights than between-POB edges\n",
          .,
          ./(6*5/2)*100)
c(edge_pairs_df[Flag_PUVGComp & (edge_pairs_df$upCI < 0),"x"],
  edge_pairs_df[Flag_PUVGComp & (edge_pairs_df$upCI < 0),"y"]) %>%
  unique() %>%
  .[str_count(.,"PUVG_") == 2] %>%
  length() %>%
  fprintf("%d (%2.2f) Edges within PUVG showed stronger weights than between-POB edges\n",
          .,
          ./(9*8/2)*100)

sink()
