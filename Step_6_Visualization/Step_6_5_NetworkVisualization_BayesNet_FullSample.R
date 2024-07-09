library(bruceR)
library(BGGM)
set.wd()

communities = c("PUSN","PUSN","PUSN","PUSN","PUSN","PUSN",
                "PUS", "PUS", "PUS", "PUS", "PUS", "PUS",
                "PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG")


BayesNet <- import("../Res_2_Results/Obj_BayesNet_FullSample.rds")
Graph <- select(BayesNet,
                cred = 0.999,
                alternative = 'greater')

# 生成原始的图对象
p <- plot(Graph,
          edge_magnify = 15,
          node_size = 20,
          groups = communities,
          layout = 'fruchtermanreingold',
          pos_col = "salmon")$plt

# 修改颜色设置
p <- p + 
  scale_color_manual(breaks = c("PUS","PUSN","PUVG"),
                     values = c("lightblue", "lightsalmon", "lightgreen"))

# 提取 ggplot 对象中的数据
p_data <- ggplot_build(p)$data[[5]]

# 替换节点名称中的下划线为空格
p_data$label <- str_replace_all(p_data$label, "_", " ")

p$layers <- p$layers[-5]

# 移除旧的标签层（假设旧的标签层是第二个图层）
new_p <- p + geom_text(data = p_data, aes(x, y, label = label), size = 4, vjust = 0.5, hjust = 0.5) +
  theme(legend.position = "right") +
  labs(color = "Group") +
  theme_void() +
  theme(legend.position = 'none')

# 显示新的图
print(new_p)
