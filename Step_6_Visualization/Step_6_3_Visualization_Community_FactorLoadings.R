library(bruceR)
library(ggplot2)
set.wd()


# Visualize Community Allocation ------------------------------------------


net_community <- import("../Res_2_Results/T_Res_CommunityIdentify.rda")

# 将data frame转换为长格式的同时，确保包含行名
net_community_long <- net_community %>%
  mutate(Row = rownames(.)) %>% # 添加一列来存储行名
  pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
  mutate(Row = factor(Row, levels = c(
    "PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
    "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
    "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
    labels = c(
      "PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
      "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
      "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9"))
  ) %>%
  mutate(Row = forcats::fct_rev(Row)) %>%
  mutate(Value = factor(Value, c(2,1,3)))

p <- ggplot(net_community_long, aes(x = Column, y = Row, fill = Value)) +
  geom_tile(color = "white", size = 0.5) + 
  scale_fill_manual(values = c("2" = "lightblue", "1" = "lightsalmon",  "3" = "lightgreen"),
                    labels = c("2" = "Community 1", "1" = "Community 2", "3" = "Community 3")) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5), 
    panel.grid = element_blank(), 
    legend.position = "top", # 将图例移到顶部
    legend.title = element_blank()
  ) +
  xlab("Community Identifying Algorithm") + 
  ylab("Nodes")

ggsave("../Res_4_Reports/Figure_2_E.svg",
       plot = p,
       width = 4, height = 4, units = "in", device = "svg")


# Visualize Factor Loadings -----------------------------------------------

EFA_loadings <- import("../Res_2_Results/Res_EFA_DiscoverySample.rda")
CFA_loadings <- import("../Res_2_Results/Res_CFA_ValidationSample.rda")

EFA_loadings_long <- EFA_loadings %>%
  as.data.frame() %>%
  mutate(Nodes = rownames(.)) %>% # 添加一列来存储行名
  naniar::replace_with_na_all(~.x < 0.4) %>%
  select(-Communality) %>%
  rename(`Factor 3` = PA2,
         `Factor 1` = PA3,
         `Factor 2` = PA1) %>%
  pivot_longer(cols = -Nodes, names_to = "Factors", values_to = "Value") %>%
  mutate(Nodes = factor(Nodes, levels = c(
    "PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
    "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
    "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
    labels = c(
      "PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
      "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
      "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9"))
  ) %>%
  mutate(Factors = factor(Factors, c("Factor 1","Factor 2","Factor 3"))) %>%
  na.omit() %>%
  mutate(Type = "EFA in Discovery Sample (n=3592)")

CFA_loadings_long <- CFA_loadings %>%
  filter(op == "=~") %>%
  select(c(lhs,rhs,est.std)) %>%
  rename(Factors = lhs,
         Nodes = rhs,
         Value = est.std) %>%
  mutate(Nodes = factor(Nodes, levels = c(
    "PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
    "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
    "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
    labels = c(
      "PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
      "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
      "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9"))
  ) %>%
  mutate(Factors = factor(Factors, 
                          c("PUS","PUSN","PUVG"),
                          c("Factor 1","Factor 2","Factor 3"))) %>%
  mutate(Type = "CFA in Validation Sample (n=3592)")

FA_loadings <- rbind(EFA_loadings_long,CFA_loadings_long) %>%
  mutate(Type = factor(Type,
                       c("CFA in Validation Sample (n=3592)",
                         "EFA in Discovery Sample (n=3592)")))
  

p <- ggplot(FA_loadings, aes(x = Nodes, y = Value, fill = Factors, color = Type)) +
  geom_col(position = position_dodge(width = 0.9),
           show.legend = F,
           width = 0.6,
           size = 0.8) +
  labs(x = "Nodes", y = "Value") +
  scale_fill_manual(values = c("Factor 1" = "lightblue", 
                               "Factor 2" = "lightsalmon", 
                               "Factor 3" = "lightgreen"),
                    name = "Factors") +
  scale_color_manual(values = c("CFA in Validation Sample (n=3592)" = "black",
                                "EFA in Discovery Sample (n=3592)" = "grey"), name = "Type") +
  
  # 修正geom_point中的position_dodge调用
  geom_point(aes(shape = Type), 
             position = position_dodge(width = 0.9), # 直接指定宽度值
             size = 2,
             stroke = 0.5,
             show.legend = T) + # 修改为TRUE，若希望在图例中显示形状
  
  scale_shape_manual(values = c("CFA in Validation Sample (n=3592)" = 19,  
                                "EFA in Discovery Sample (n=3592)" = 18),
                     name = "Type") +
  theme_bruce() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1,size = 8), 
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed", linewidth = 0.8), # 添加Y轴网格线
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    legend.position = "top", 
    legend.title = element_blank()
  ) + 
  scale_y_continuous(limits = c(0, 0.85), expand = c(0, 0))+
  xlab("Items") +
  ylab("Standardized Loadings")

print(p)


ggsave("../Res_4_Reports/Figure_2_F.svg",
       plot = p,
       width = 8, height = 4, units = "in", device = "svg")
