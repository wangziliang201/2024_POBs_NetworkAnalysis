library(bruceR)
set.wd()
source("../subfunctions.R")

sink(file = "../Res_1_Logs/Log_1_3_Res_Measurement_Reliability.txt",
     append = F,
     split = T)

dat <- import("../Res_3_IntermediateData/prepared_data.rda")


Res_alpha_PUSN <- dat %>%
  Alpha(var = "PUSN_",
        items = 1:6)

Res_alpha_PUS <- dat %>%
  Alpha(var = "PUS_",
        items = 1:6)


Res_alpha_PUVG <- dat %>%
  Alpha(var = "PUVG_",
        items = 1:9)

sink()