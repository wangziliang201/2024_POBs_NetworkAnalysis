library(caret)
library(bruceR)
library(nortest)
library(lavaan)
set.wd()

set.seed(123)

dat <- import("../Res_3_IntermediateData/prepared_data.rda") 


# Normality Testing -------------------------------------------------------
normality_results <- dat %>%
  select(starts_with('PU')) %>%
  sapply(., function(x) {
    ad_test <- ad.test(x)  # 执行Anderson-Darling检验
    ad_test$p.value}  # 提取p值
    )
print(normality_results)
DummyCoded_School <- dummy(dat$School,levelsToKeep = c('General High School','College')) %>%
  as.data.frame()

dat_dummy <- dat %>%
  mutate(
    School = NULL,
    GeneralHighSchool = DummyCoded_School$`General High School`,
    College = DummyCoded_School$College
  )
# Confirmatory Factor Analysis --------------------------------------------
model_specification = "
  PUSN =~ PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6
  PUS  =~ PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6
  PUVG =~ PUVG_1 + PUVG_2 + PUVG_3 + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9
  PUSN ~ AgeOfYear + GeneralHighSchool + College
  PUS  ~ AgeOfYear + GeneralHighSchool + College
  PUVG ~ AgeOfYear + GeneralHighSchool + College
"
CFA_Result <- dat_dummy %>%
  cfa(model = model_specification,
      data = .,
      # ordered = c("PUSN_1","PUSN_2","PUSN_3","PUSN_4","PUSN_5","PUSN_6",
      #             "PUS_1","PUS_2","PUS_3","PUS_4","PUS_5","PUS_6",
      #             "PUVG_1","PUVG_2","PUVG_3","PUVG_4","PUVG_5","PUVG_6","PUVG_7","PUVG_8","PUVG_9"),
      estimator = "MLR",
      orthogonal = F)

library(semTools)
semTools::fitmeasures(CFA_Result,fit.measures = c('cfi.robust','tli.robust','rmsea.robust','srmr_bentler'))
# model_summary(CFA_Result)

# Extract Factor Loadings and Visualization -------------------------------

CFA_loadings <- standardizedSolution(CFA_Result)

CFA_loadings_long <- CFA_loadings %>%
  filter(op == "=~") %>%
  select(c(lhs,rhs,est.std,se)) %>%
  rename(Factors = lhs,
         Nodes = rhs,
         Value = est.std,
         SE = se) %>%
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
                          c("Factor 1","Factor 2","Factor 3")))

p <- ggplot(CFA_loadings_long, aes(x = Nodes, y = Value, fill = Factors, color = Factors)) +
  geom_col(position = position_dodge(width = 0.9),
           show.legend = T,
           width = 0.6,
           size = 0.8) +
  scale_fill_manual(values = c("Factor 1" = "lightblue", 
                               "Factor 2" = "lightsalmon", 
                               "Factor 3" = "lightgreen"),
                    name = "Factors") +
  scale_color_manual(values = c("Factor 1" = "lightblue", 
                                "Factor 2" = "lightsalmon", 
                                "Factor 3" = "lightgreen"),
                     name = "Factors") +
  # 添加误差条
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE),
                width = 0.3, position = position_dodge(width = 0.9), 
                color = "black",size = 1,
                show.legend = F) +  # 设置误差条的颜色和大小
  # geom_point(aes(shape = Factors),
  #            color = "black",
  #            position = position_dodge(width = 0.9), 
  #            size = 1.5,
  #            stroke = 0.5,
  #            show.legend = T) +
  theme_bruce() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 8), 
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    legend.position = "top", 
    legend.title = element_blank()
  ) + 
  coord_cartesian(ylim = c(0.5, 0.8)) +  # 使用 coord_cartesian 来限制显示范围
  xlab("Items") +
  ylab("Standardized Loadings")

print(p)

ggsave("../Res_4_Reports/Figure_2_F_NEW.svg",
       plot = p,
       width = 12, height = 6, units = "in", device = "svg")

# CFA Model Visualization -------------------------------------------------

library(tidySEM)

get_layout(CFA_Result, layout_algorithm = "layout_on_grid")


graph_sem(CFA_Result)
# library(svglite)
# svglite(file = "../Res_4_Reports/CFAModel_OverallAnalysisSample.svg", width = 14, height = 6) 
# 
# dev.off()