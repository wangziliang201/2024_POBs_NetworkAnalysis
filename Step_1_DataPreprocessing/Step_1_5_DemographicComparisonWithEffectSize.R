library(bruceR)
library(effectsize)
set.wd()

source("../subfunctions.R")

dat <- import("../Res_3_IntermediateData/prepared_data.rda") %>%
  mutate(Total_PUSN = SUM(var = "PUSN_",items = 1:6,data = .)) %>%
  mutate(Total_PUVG = SUM(var = "PUVG_",items = 1:9,data = .)) %>%
  mutate(Total_PUS = SUM(var = "PUS_",items = 1:6,data = .)) %>%
  mutate(Group = factor(Total_PUS > 23,c(T,F),c('PUS','Non-PUS'))) %>%
  select(c(AgeOfYear,Sex,School,Group,starts_with("Total_"))) %>%
  rename(Age = AgeOfYear,
         Sex = Sex,
         Educational = School,
         PUSN = Total_PUSN,
         PUVG = Total_PUVG,
         PUS = Total_PUS)

fprintf("Age Range : %2.2f ~ %2.2f \n", min(dat$`Age (years)`), max(dat$`Age (years)`))


# Res_CompT <- compareGroups(`Biological Sex` ~ `Age (years)` + `Current Educational Status` + PUSN+PUS+PUVG + Group,
#                            data = dat,
#                            method = c(1,3,1,1,1,3))

TTEST(dat,y = c("Age","PUS","PUSN","PUVG"),x="Sex")
# dat$`Age (years)`

cramers_v(dat$Educational,dat$Sex)
