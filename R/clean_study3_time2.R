library(dplyr)
library(ggplot2)
library(magrittr)

giver <- data.table::fread(here::here("Data/Raw Data/S3_time2.csv"),
                           header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100',gender != 'Other', !is.na(PROLIFIC_PID)) %>%
  dplyr::select(ResponseId, PROLIFIC_PID,Condition,dplyr::contains("_advice"),GoupNum,
                age,gender, ethnicity,education,"0.AdviseeID":"9.Treatment" ) %>%
  dplyr::mutate(dplyr::across(dplyr::contains("_advice"),
                              ~ dplyr::case_when(stringr::str_starts(.,"Low") ~ 0,
                                                 stringr::str_starts(.,"High") ~ 1))) %>%
  dplyr::mutate_at(vars(contains("_advice")), as.numeric) %>%
  dplyr::mutate_at(vars(contains('.Performance')), as.numeric) %>%
  dplyr::mutate_at(vars(contains('.Estimate')), as.numeric) %>%
  dplyr::rename('GroupNum' = 'GoupNum') %>%
  dplyr::mutate(GroupNum = as.numeric(GroupNum), age = as.numeric(age))

giver50 <- data.table::fread(here::here("Data/Raw Data/S1_time2_50.csv"),
                           header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100',gender != 'Other', !is.na(PROLIFIC_PID)) %>%
  dplyr::select(ResponseId, PROLIFIC_PID,Condition,dplyr::contains("_advice"),
                age,gender, ethnicity,education,"0.AdviseeID":"9.Treatment" ) %>%
  dplyr::mutate(dplyr::across(dplyr::contains("_advice"),
                              ~ dplyr::case_when(stringr::str_starts(.,"Low") ~ 0,
                                                 stringr::str_starts(.,"High") ~ 1))) %>%
  dplyr::mutate_at(vars(contains("_advice")), as.numeric) %>%
  dplyr::mutate_at(vars(contains('.Performance')), as.numeric) %>%
  dplyr::mutate_at(vars(contains('.Estimate')), as.numeric) %>%
  dplyr::mutate(age = as.numeric(age))


# get advice from each Adviser
giverAdv <- giver %>% dplyr::select(dplyr::contains("_advice"))
targets <- giver %>% dplyr::select(dplyr::contains(".AdviseeID"))
performance <- giver %>% dplyr::select(dplyr::contains(".Performance"))
Expectation <- giver %>% dplyr::select(dplyr::contains(".Estimate"))
condTime1 <- giver %>% dplyr::select(dplyr::contains(".Treatment"))
gender <- giver %>% dplyr::select(dplyr::contains(".Gender"))
conditions <- giver %>% dplyr::select(Condition,PROLIFIC_PID)


conditions <- conditions[rep(1:nrow(conditions), each = 10), ]
rownames(conditions) <- as.character(c(1:10000))


TargetList <- list()
AdviceList <- list()
ScoreList <- list()
GuessList <- list()
TreatmentList <- list()
GenderList <- list()


for(i in 1:nrow(giver)){
  advList_i = as.list(giverAdv[i,])
  targetList_i = as.list(targets[i,])
  score_i = as.list(performance[i,])
  guess_i = as.list(Expectation[i,])
  cond_i = as.list(condTime1[i,])
  gender_i = as.list(gender[i,])

  TargetList <- c(TargetList, targetList_i)
  AdviceList <- c(AdviceList,advList_i)
  ScoreList <- c(ScoreList,score_i)
  GuessList <- c(GuessList,guess_i)
  TreatmentList <- c(TreatmentList, cond_i)
  GenderList <- c(GenderList, gender_i)
}

AdviceAll <- data_frame(Target = unlist(TargetList) ,Advice = unlist(AdviceList), Performance = unlist(ScoreList), Guess = unlist(GuessList), Gender = unlist(GenderList), Treatment = unlist(TreatmentList))

AdviceAll <- cbind(AdviceAll,conditions)

AdviceAll <- AdviceAll %>% dplyr::rename('GiverID' = 'PROLIFIC_PID')

Competitors <- read.csv('Data/Raw Data/S3_Math50.csv') %>%
  dplyr::filter(Progress == '100' & Status == 'IP Address') %>%
  dplyr::filter(!is.na(QID3)) %>%
  dplyr::rename('ProlificID' = 'QID3') %>%
  dplyr::mutate(Score = as.numeric(SC0)) %>%
  dplyr::select(ProlificID, Score) %>%
  dplyr::filter(!is.na(Score))

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
    reward = 0.3
    group <- LowGroup
  }
  return_i = sum(performance >= group$Score) * reward / 20
  ReturnList <- c(ReturnList, return_i)
}

Return <- data_frame(ExpectedBonus = unlist(ReturnList))

AdviceAll <- cbind(AdviceAll, Return)

saveRDS(AdviceAll, file = here::here("Data/Study3_time2_advice.Rds"))
saveRDS(giver, file = here::here("Data/Study3_time2_giver.Rds"))
saveRDS(giver50, file = here::here("Data/Study3_time2_giver50.Rds"))

AdviceAll %>% write.csv("Data/Cleaned Data/S3_time2_advice_clean.csv")
giver %>% write.csv("Data/Cleaned Data/S3_time2_giver_clean.csv")
giver50 %>% write.csv("Data/Cleaned Data/S3_time2_giver50_clean.csv")

