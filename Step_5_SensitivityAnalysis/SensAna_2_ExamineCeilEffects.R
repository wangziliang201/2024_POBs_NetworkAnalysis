library(bruceR)
library(compareGroups)
set.wd()
dat <- import("../Res_3_IntermediateData/prepared_data.rda")

Res_CompT <- compareGroups(Sex ~ PUS_1 + PUS_2 + PUS_3 + PUS_4 + PUS_5 + PUS_6 + 
                             PUSN_1 + PUSN_2 + PUSN_3 + PUSN_4 + PUSN_5 + PUSN_6 +
                             PUVG_1 + PUVG_2 + PUVG_3 + PUVG_4 + PUVG_5 + PUVG_6 + PUVG_7 + PUVG_8 + PUVG_9,
                           data = dat,
                           method = 1)

T_Sex <- createTable(Res_CompT,
                     show.p.overall = F,
                     digits = 2,
                     sd.type = 1)
print(T_Sex)
export2xls(T_Sex,
           file = "../Res_2_Results/T_SensAna_2_SexComp.xlsx")