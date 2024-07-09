fprintf <- function(STR,...,end = '\n'){
  cat(paste(sprintf(STR,...),end,sep = ""))
}

fullfile <- function(...){
  return(normalizePath(paste(...,sep = .Platform$file.sep),mustWork = F))
}

# 自定义绘图函数
plot_custom <- function(net, Items, layout_type = "spring",
                        label.cex = 0.8,
                        label.prop = 1,
                        legend.cex = 0.4,
                        ...) {
  library(smacof)
  library(qgraph)
  # 获取网络的加权邻接矩阵
  adjMatrix <- net$graph
  
  # 计算节点的strength
  strength <- qgraph::centrality_auto(net)$node.centrality[["Strength"]]
  strength_scaled <- scales::rescale(strength, to = c(0, 1))
  
  # 自定义node和edge颜色
  # nodeColors <- colorRampPalette(c("white", "blue"))(100)[as.numeric(cut(strength_scaled, breaks = 100))]
  edgeColors <- ifelse(adjMatrix > 0, "salmon", "skyblue")

  
  # 计算布局
  if (length(layout_type) == 1){
    if (layout_type == "MDS") {
      dissimilarity <- sim2diss(adjMatrix, method = "rank")
      mds_result <- mds(dissimilarity, type = "interval")
      layout <- mds_result$conf
      
    } else if (layout_type == "Procrustes") {
      dissimilarity <- sim2diss(adjMatrix, method = "rank")
      mds_result <- mds(dissimilarity, type = "interval")
      fit_procrustes <- Procrustes(mds_result$conf, mds_result$conf)
      layout <- fit_procrustes$Yhat
      
    } else {
      # 其他布局类型直接传递给qgraph的layout参数
      layout <- layout_type
    }
  }else{
    layout <- layout_type
  }

  # 使用qgraph函数绘制网络图
  p <- qgraph(adjMatrix, 
              layout = layout,
              groups = Items$Community,
              color = c("lightblue", "lightsalmon", "lightgreen"),
              labels = stringr::str_replace_all(colnames(adjMatrix), "_", " "),
              label.cex = label.cex, 
              label.color = 'black', 
              label.prop = label.prop,
              negDashed = TRUE, 
              legend.cex = legend.cex, 
              legend.mode = 'style2', 
              nodeNames = Items$Dimension, 
              border.width = 1.5, 
              font = 2,
              pie = strength_scaled, 
              edge.color = edgeColors,
              ...)
  invisible(p$layout)
}

generateItemsdf <- function(net){
  cur_wd <- getwd()
  bruceR::set.wd()
  Items <- data.frame("Item_label" = colnames(net$graph) )
  Items$order <- seq_len(nrow(Items))
  wording <- import("../Res_3_IntermediateData/QuestionnaireItemWording.xlsx")
  Items <- merge(Items,wording,
                 by.x = "Item_label",
                 all.x = T)
  Items <- Items[order(Items$order), ]
  Items$Community <- Items$Item_label %>%
    stringr::str_remove_all("_\\d")
  setwd(cur_wd)
  return(Items)
}


compareCentrality <- function(net1, net2,
                              include = c("Strength",
                                          "Closeness",
                                          "Betweenness",
                                          "all",
                                          "All"),
                              orderBy = c("Strength",
                                          "Closeness",
                                          "Betweenness"),
                              decreasing = T,
                              legendName = '',
                              net1Name = 'Network 1',
                              net2Name = 'Network 2'){
  
  library(ggplot2)
  library(forcats)
  
  if (length(include) == 1){
    if(include == "All" | include == "all"){
      include = c("Strength",
                  "Closeness",
                  "Betweenness")
    }
  }

  df <- qgraph::centralityTable(net1, net2,
                        standardized = F,
                        relative = F) %>%
    filter(measure %in% include) %>%
    mutate(measure = factor(measure,orderBy))
  
  df <- df %>% 
    mutate(graph = case_when(graph == 'graph 1' ~ net1Name,
                             graph == 'graph 2' ~ net2Name),
           graph = as.factor(graph)) %>% 
    
    mutate(node = factor(node, 
                         levels = c("PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
                                    "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
                                    "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
                         labels = c("PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
                                    "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
                                    "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9")
                              )
    ) %>%
    mutate(node = forcats::fct_rev(node))  %>%
    mutate(color = case_when(
      grepl("PUS ", node) ~ "lightblue",
      grepl("PUSN ", node) ~ "lightsalmon",
      grepl("PUVG ", node) ~ "lightgreen",
      TRUE ~ "black"
    ))
  
  return(df)
}
refactorNode <- function(NodeNames){
  NodeNames <- factor(as.character(NodeNames), 
                      levels = c("PUS_1", "PUS_2", "PUS_3", "PUS_4", "PUS_5", "PUS_6",
                                 "PUSN_1", "PUSN_2", "PUSN_3", "PUSN_4", "PUSN_5", "PUSN_6",
                                 "PUVG_1","PUVG_2", "PUVG_3", "PUVG_4", "PUVG_5", "PUVG_6", "PUVG_7", "PUVG_8", "PUVG_9"),
                      labels = c("PUS 1", "PUS 2", "PUS 3", "PUS 4", "PUS 5", "PUS 6",
                                 "PUSN 1", "PUSN 2", "PUSN 3", "PUSN 4", "PUSN 5", "PUSN 6",
                                 "PUVG 1","PUVG 2", "PUVG 3", "PUVG 4", "PUVG 5", "PUVG 6", "PUVG 7", "PUVG 8", "PUVG 9")
                      )
  return(NodeNames)
}
renameMeasure <- function(measures){
  measures <- measures %>%
    stringr::str_replace_all("^strength$","Strength") %>%
    stringr::str_replace_all("^closeness$","Closeness") %>%
    stringr::str_replace_all("^betweenness$","Betweenness") %>%
    stringr::str_replace_all("bridgeStrength","Bridge Strength") %>%
    stringr::str_replace_all("bridgeCloseness","Bridge Closeness") %>%
    stringr::str_replace_all("bridgeBetweenness","Bridge Betweenness")
}
# extractCentrality <- function(net1,
#                               include = c("Strength",
#                                           "Closeness",
#                                           "Betweenness",
#                                           "ExpectedInfluence",
#                                           "all",
#                                           "All"),
#                               orderBy = c("Strength",
#                                           "Closeness",
#                                           "Betweenness",
#                                           "ExpectedInfluence"),
#                               decreasing = T){
#   
#   library(ggplot2)
#   library(forcats)
#   if (length(include) == 1){
#     if(include == "All" | include == "all"){
#       include = c("Strength",
#                   "Closeness",
#                   "Betweenness",
#                   "ExpectedInfluence")
#     }
#   }
#   
#   df <- centralityTable(net1) %>% filter(measure %in% include)
#   
#   df <- df %>% 
#     mutate(graph = case_when(graph == 'graph 1' ~ net1Name,
#                              graph == 'graph 2' ~ net2Name),
#            graph = as.factor(graph),
#            node = as.factor(node)) %>% 
#     
#     mutate(node = fct_reorder(node, value))
#   return(df)
# }
extractSigNCT_Node <- function(net_NCT,p_val_threshold = 0.05,index = "strength"){
  pval <- as.data.frame(net_NCT$diffcen.pval)[index]
  Res_Sig <- rownames(pval)[pval < p_val_threshold]
  fprintf("|Network Comparison Test|Centrality Index: %s|Statistically significant results: %s|",index,Res_Sig)
  if (length(Res_Sig) == 0){
    df <- na.omit(data.frame("node" = "",
                             "pval" = NA,
                             "measure" = ""))
  }else{
    df <- na.omit(data.frame("node" = Res_Sig,
                             "pval" = pval[pval < p_val_threshold],
                             "measure" = index))
  }

  df$node <- refactorNode(df$node)
  return(df)
}
extractSigNCT_Edge <- function(net_NCT,p_val_threshold = 0.05){
  Res_Edge <- as.data.frame(net_NCT$einv.pvals)
  pval <- Res_Edge["p-value"]
  Res_Sig <- Res_Edge[pval < p_val_threshold,c("Var1","Var2","p-value")] %>%
    mutate(EdgeName = stringr::str_c(Var1,Var2,sep = "-"))
  fprintf("|Network Comparison Test|Edge|Statistically significant results: %s|",Res_Sig$EdgeName)
  return(na.omit(Res_Sig))
}

extractTwoNet_Node <- function(net1,net2,
                                netName = c("net1","net2"),
                                type = "core"){
  library(dplyr)
  if (type == "core"){
    T_net1 <- summary(net1,statistics = c("strength", "closeness", "betweenness")) %>%
      as.data.frame() %>%
      select(c(type,id,mean,sample,CIlower,CIupper))
    T_net2 <- summary(net2,statistics = c("strength", "closeness", "betweenness")) %>%
      as.data.frame() %>%
      select(c(type,id,mean,sample,CIlower,CIupper))
    factor_levels <- c("strength", "closeness", "betweenness")
    factor_labels <- c("Strength", "Closeness", "Betweenness")
  }else if (type == "bridge"){
    T_net1 <- summary(net1,statistics = c("bridgeStrength","bridgeCloseness","bridgeBetweenness")) %>%
      select(c(type,id,mean,sample,CIlower,CIupper))
    T_net2 <- summary(net2,statistics = c("bridgeStrength","bridgeCloseness","bridgeBetweenness")) %>%
      select(c(type,id,mean,sample,CIlower,CIupper))
    factor_levels <- c("bridgeStrength","bridgeCloseness","bridgeBetweenness")
    factor_labels <- c("Bridge Strength", "Bridge Closeness", "Bridge Betweenness")
  }

  T_net1$group <- netName[1]
  T_net2$group <- netName[2]
  
  T_combined <-rbind(T_net1,T_net2)
  
  T_combined <- T_combined %>%
    mutate(type = factor(type,factor_levels,factor_labels)) %>%
    mutate(group = factor(group,netName)) %>%
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
    ),
    shape = case_when(
      group == netName[1] ~ 22, 
      group == netName[2] ~ 24 
    ),
    color_group = case_when(
      group == netName[1] ~ "#2A6EBBEE",
      group == netName[2] ~ "#69BE28EE"
    ))
  return(T_combined)
}

CompareBootstrapNet <- function(net_PUS,net_NonPUS){
  
  
  T_Combined_Node <- extractTwoNet_Node(net_PUS,net_NonPUS,
                                        netName = c("PUS","Non-PUS"),
                                        type = "core")
  library(ggtext)
  p <- ggplot(T_Combined_Node, aes(x = sample, y = id, fill = color, group = group)) +
    geom_errorbar(aes(xmin = CIlower, xmax = CIupper, color = color_group),
                  linewidth = 0.5, width = 0.5, position = position_dodge(0.7)) +
    geom_point(aes(x = sample, shape = shape, color = color_group), size = 2, position = position_dodge(0.7)) +
    facet_wrap(~ type, scales = "free", ncol = 3) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),   # Y轴文本大小
      plot.subtitle = element_blank(),        # 去除副标题
      legend.position = "top",                # 图例位置
      strip.text = element_text(size = 12),   # facet 标签文本大小
      legend.text = element_text(size = 14),   # 图例文本大小
      legend.title = element_blank()
    ) +
    scale_color_manual(values = c("#2A6EBBEE" = "#2A6EBBEE", "#69BE28EE" = "#69BE28EE"),
                       labels = c("#2A6EBBEE" = "PUS", "#69BE28EE" = "Non-PUS")) +
    scale_shape_identity() + 
    scale_fill_identity() +                    # 直接使用定义的颜色
    scale_y_discrete(labels = function(labels) { 
      color_labels <- T_Combined_Node$color[match(labels, T_Combined_Node$id)]
      mapply(function(label, color) paste0("<span style='color:", color, ";'>", label, "</span>"), labels, color_labels)
    }) +
    labs(x = "Mean", y = "Node") +
    theme(axis.text.y = element_markdown())    # 使用 element_markdown 来支持颜色
  
  # 打印绘图对象
  print(p)
  ggsave(filename = "../Res_2_Reports/Fig_Tmp.svg",
         plot = p,
         width = 12, height = 4, units = "in", device = "svg")
  
}
calc_strength <- function(x){
  library(igraph)
  obj <- igraph::graph_from_adjacency_matrix(abs(x),mode = 'undirected',diag = F,weighted = T)
  return(igraph::strength(obj,loops = F))
}
calc_closeness <- function(x){
  library(igraph)
  obj <- igraph::graph_from_adjacency_matrix(abs(x),mode = 'undirected',diag = F,weighted = T)
  return(igraph::closeness(obj))
}
calc_betweenness <- function(x){
  library(igraph)
  obj <- igraph::graph_from_adjacency_matrix(abs(x),mode = 'undirected',diag = F,weighted = T)
  return(igraph::betweenness(obj,cutoff = 1/1e-10))
}

calc_centrality <- function(x, metric, communities = NULL) {
  switch(metric,
         Strength = calc_strength(x),
         Closeness = calc_closeness(x),
         Betweenness = calc_betweenness(x),
         `Bridge Strength` = networktools::bridge(x, communities = communities)$`Bridge Strength`,
         `Bridge Closeness` = networktools::bridge(x, communities = communities)$`Bridge Closeness`,
         `Bridge Betweenness` = networktools::bridge(x, communities = communities)$`Bridge Betweenness`)
}

extractRollYourOwn <- function(ResList,cred=0.95,digits = 3){
  lb <- (1-cred) / 2
  ub <- 1 - lb
  dims <- dim(ResList$results)
  
  if(is.null(dims)){
    
    mu <- mean(ResList$results)
    
    scale <- sd(ResList$results)
    
    res <- data.frame(Post.mean = round(mean(ResList$results), 3),
                      Post.sd =    round(sd(ResList$results), 3),
                      Cred.lb = round(quantile(ResList$results, probs = lb), 3),
                      Cred.ub = round(quantile(ResList$results, probs = ub), 3) )
  } else {
    
    mu <-  apply( ResList$results, 1, mean, na.rm = T)
    p <- length(mu)
    scale <- apply( ResList$results, 1, sd, na.rm = T)
    ci_lb <- apply( ResList$results, 1, quantile, lb, na.rm = T)
    ci_ub <- apply( ResList$results, 1, quantile, ub, na.rm = T)
    
    res<- data.frame(Node = 1:p,
                     Post.mean = round(mu, 3),
                     Post.sd = round(scale, 3),
                     Cred.lb = round(ci_lb, 3),
                     Cred.ub = round(ci_ub, 3))
  }
  rownames(res) <- NULL
  
  return(res)
}

BayesComp_strength <- function(Yg1, Yg2){
  fit1 <-  BGGM::estimate(Yg1, analytic = TRUE)
  fit2 <-  BGGM::estimate(Yg2, analytic = TRUE)
  
  pcor1 <- BGGM::pcor_mat(fit1)
  pcor2 <- BGGM::pcor_mat(fit2)
  
  ind1 <- calc_strength(pcor1)
  ind2 <- calc_strength(pcor2)
  
  sum((ind1 - ind2)^2)
}


BayesNet_CentralityTable <- function(Obj_NayesNet){
  communities = c("PUSN","PUSN","PUSN","PUSN","PUSN","PUSN",
                  "PUS", "PUS", "PUS", "PUS", "PUS", "PUS",
                  "PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG","PUVG")
  centrality_metrics <- c("Strength","Closeness","Betweenness",
                          "Bridge Strength", "Bridge Closeness", "Bridge Betweenness")
  centrality_results <- list()
  
  for (metric in centrality_metrics) {
    f <- function(x, ...) calc_centrality(x, metric, communities)
    tmpres_ls <- roll_your_own(object = Obj_NayesNet,
                               FUN = f,
                               iter = 5000,
                               select = T,
                               cred = 0.95)
    tmpres_df <- extractRollYourOwn(tmpres_ls)
    tmpres_df$Node <- colnames(Obj_NayesNet$Y) %>%
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
  return(T_Centrality)
}
