library(dplyr)
library(ggplot2)
library(magrittr)

giver <- data.table::fread(here::here("Data/Raw Data/S1_time2.csv"),
                           header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100', !is.na(PROLIFIC_PID),!is.na(gender)) %>%
  dplyr::select(PROLIFIC_PID,Condition,dplyr::contains("_advice"),
                age,gender, ethnicity,education,"0.PROLIFIC_PID":"9.Guess" ) %>%
  dplyr::mutate(dplyr::across(dplyr::contains("_advice"),
                              ~ dplyr::case_when(stringr::str_starts(.,"Low") ~ 0,
                                                 stringr::str_starts(.,"High") ~ 1))) %>%
  dplyr::mutate_at(vars(contains("_advice")), as.numeric) %>%
  dplyr::mutate(ExpShow = ifelse(Condition == 'E',1,0)) %>%
  dplyr::rename('GiverID' = 'PROLIFIC_PID',"GiverGender" = 'gender')


# get advice from each Adviser
giverAdv <- giver %>% dplyr::select(dplyr::contains("_advice"))
targets <- giver %>% dplyr::select(dplyr::contains(".PROLIFIC_PID"))
conditions <- giver %>% dplyr::select(Condition,GiverID,GiverGender)

conditions <- conditions[rep(1:nrow(conditions), each = 10), ]
rownames(conditions) <- as.character(c(1:10000))

TargetList <- list()
AdviceList <- list()

for(i in 1:nrow(giver)){
  advList_i = as.list(giverAdv[i,])
  targetList_i = as.list(targets[i,])

  TargetList <- c(TargetList, targetList_i)
  AdviceList <- c(AdviceList,advList_i)

}

AdviceAll <- data_frame(Target = unlist(TargetList) ,Advice = unlist(AdviceList))
AdviceAll <- cbind(AdviceAll,conditions)


advisee <- readRDS(here::here("Data/Study1_time1.Rds"))%>%
  dplyr::select(PROLIFIC_PID,Guess, default,Score,Gender) %>%
  dplyr::rename('Target' = "PROLIFIC_PID", "intChoice" = 'Guess', 'Treatment' = 'default','TarGender' = 'Gender')

AdviceAll <- merge(AdviceAll,advisee,by.x = 'Target',by.y = "Target")


# find competitors
Competitors <- read.csv('Data/Raw Data/S1_Ability50.csv',
                        header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100' & Status == 'IP Address' & !is.na(QID3) & !is.na(Gender) ) %>%
  dplyr::rename('ProlificID' = 'QID3', 'score' = 'Score') %>%
  dplyr::mutate(Score = as.numeric(score)) %>%
  dplyr::select(ProlificID,ResponseId, Score,
                Gender, Age, Ethnicity,Education)

Competitors <- Competitors[order(-Competitors$Score),]

HighGroup <- Competitors[1:20,]
LowGroup <- Competitors[31:50,]

ReturnList <- list()

for(i in 1:nrow(AdviceAll)){
  performance <- AdviceAll$Performance[i]
  if (AdviceAll$Advice[i] == '1'){
    reward = 0.5
    group <- HighGroup
  }else{
    reward = 0.2
    group <- LowGroup
  }
  return_i = sum(performance >= group$Score) * reward / 20
  ReturnList <- c(ReturnList, return_i)
}

Return <- data_frame(ExpectedBonus = unlist(ReturnList))

AdviceAll <- cbind(AdviceAll, Return)

saveRDS(AdviceAll, file = here::here("Data/Study1_time2_advice.Rds"))
saveRDS(giver, file = here::here("Data/Study1_time2_giver.Rds"))

AdviceAll %>% write.csv("Data/Cleaned Data/S1_time2_advice_clean.csv")
giver %>% write.csv("Data/Cleaned Data/S1_time2_giver_clean.csv")

