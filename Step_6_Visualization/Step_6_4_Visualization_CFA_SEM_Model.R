library(bruceR)
library(semPlot)
library(svglite)
set.wd()

CFA_D <- import("../Res_2_Results/Obj_CFA_FullSample_DataDriven.rds")
svg(file = "../Res_4_Reports/Figure_S4.svg", width = 8, height = 8) 
semPaths(CFA_D, 
         what = "std",          # 标准化路径系数
         layout = "circle2",       # 布局方式
         intercepts = F,
         residuals = F,
         sizeMan = 6,
         sizeMan2 = 6,
         color = list("man" = "lightgrey","lat" = "black"),
         edge.color = "lightgrey",
         edge.label.cex = 1,    # 路径标签大小
         nCharNodes = 0,        # 节点名称字符数限制
         sizeLat = 8,          # 潜变量的节点大小
         # layoutSplit = T,
         curveAdjacent = 'cov',
         curvePivot = TRUE)     # 曲线路径枢轴
dev.off()


CFA_H <- import("../Res_2_Results/Obj_CFA_FullSample_HypothesisTest.rds")


semPaths(CFA_H, 
         what = "std",          # 标准化路径系数
         layout = "spring",       # 布局方式
         residuals = F,
         sizeMan = 6,
         sizeMan2 = 6,
         edge.color = "lightgrey",
         edge.label.cex = 0.8,    # 路径标签大小
         nCharNodes = 0,        # 节点名称字符数限制
         sizeLat = 8,          # 潜变量的节点大小
         curveAdjacent = 'cov',
         layoutSplit = T,
         curvePivot = TRUE)     # 曲线路径枢轴
