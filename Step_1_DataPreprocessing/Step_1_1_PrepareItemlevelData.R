library(bruceR)
library(EGAnet)
set.wd()

sink(file = "../Res_1_Logs/Log_1_1_CovariatesRegressedOut_RedundantAnalysis.txt",
     append = F,
     split = T)


dat <- import("../Res_3_IntermediateData/prepared_data.rda")


# Regress out covariates ---------------------------------------------------

dat_regout <- dat %>%
  select(Sex)

item_var <- grep(pattern = "PU.*_\\d",colnames(dat),value = T)

for (iItem in item_var){
  mdl_regout <- lm(formula = as.formula(sprintf("%s ~ AgeOfYear + School",iItem)),
                       data = dat)
  # model_summary(mdl_regout)
  dat_regout[iItem] <- mdl_regout$residuals + mdl_regout$coefficients['(Intercept)']
}

export(dat_regout,file = "../Res_3_IntermediateData/regressed_data.rda",verbose = 1)
fprintf("UVA for data regressed out Age and Education: \n")
Res_RedundantAna <- dat_regout %>%
  select(starts_with("PU")) %>%
  UVA(data = .,
      uva.method = "MBR",
      reduce = F)
print(Res_RedundantAna)
export(Res_RedundantAna,"../Res_2_Results/Res_Item_RedundantAnalysis.rda")


for (iItem in item_var){
  mdl_regout <- lm(formula = as.formula(sprintf("%s ~ AgeOfYear + School + Sex",iItem)),
                   data = dat)
  # model_summary(mdl_regout)
  dat_regout[iItem] <- mdl_regout$residuals + mdl_regout$coefficients['(Intercept)']
}
fprintf("UVA for data regressed out Age, Education and Sex: \n")

export(dat_regout,file = "../Res_3_IntermediateData/regressed_data_sex.rda",verbose = 1)

Res_RedundantAna <- dat_regout %>%
  select(starts_with("PU")) %>%
  UVA(data = .,
      uva.method = "MBR",
      reduce = F)
print(Res_RedundantAna)
# export(Res_RedundantAna,"../Res_2_Results/Res_Item_RedundantAnalysis.rda")

sink()
