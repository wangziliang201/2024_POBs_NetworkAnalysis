library(compareGroups)
library(bruceR)
set.wd()

source("../subfunctions.R")

sink(file = "../Res_1_Logs/Log_1_4_Res_SampleCharacteristics.txt",
     append = F,
     split = T)

dat <- import("../Res_3_IntermediateData/prepared_data.rda") %>%
  mutate(Total_PUSN = SUM(var = "PUSN_",items = 1:6,data = .)) %>%
  mutate(Total_PUVG = SUM(var = "PUVG_",items = 1:9,data = .)) %>%
  mutate(Total_PUS = SUM(var = "PUS_",items = 1:6,data = .)) %>%
  mutate(Group = factor(Total_PUS > 23,c(T,F),c('PUS','Non-PUS'))) %>%
  select(c(AgeOfYear,Sex,School,Group,starts_with("Total_"))) %>%
  rename(`Age (years)` = AgeOfYear,
         `Biological Sex` = Sex,
         `Current Educational Status` = School,
         PUSN = Total_PUSN,
         PUVG = Total_PUVG,
         PUS = Total_PUS)

fprintf("Age Range : %2.2f ~ %2.2f \n", min(dat$`Age (years)`), max(dat$`Age (years)`))

         # `Problematic Usage of Social Network site or media` = Total_PUSN,
         # `Problematic Usage of Video Game` = Total_PUVG,
         # `Problematic Usage of Smartphone` = Total_PUS)
# `Problematic Usage of Social Network site or media` + `Problematic Usage of Video Game` + `Problematic Usage of Smartphone`
Res_CompT <- compareGroups(`Biological Sex` ~ `Age (years)` + `Current Educational Status` + PUSN+PUS+PUVG + Group,
                           data = dat,
                           method = c(1,3,1,1,1,3))

T_Demographics <- createTable(Res_CompT,
                              digits = 2,
                              show.all = T,
                              all.last = F,
                              show.ci = F,
                              sd.type = 1)
print(T_Demographics)

export2xls(T_Demographics,
          file = "../Res_2_Results/T_Demographics_SampleCharacteristics.xlsx")


Describe(dat,all.as.numeric = F,
         file = "../Res_2_Results/T_DescriptiveStatistics.doc",
         upper.triangle = T,
         upper.smooth = T,
         plot.width = 12,
         plot.height = 12,
         plot.file = "../Res_2_Results/Fig_DescriptiveCorrelation.png",
         plot.dpi = 300)

sink()
