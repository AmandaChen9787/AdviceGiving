library(dplyr)
library(ggplot2)
library(magrittr)

Advisee <- data.table::fread(here::here("Data/Raw Data/S3_time3.csv"),
                             header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100' & Status == 'IP Address') %>%
  dplyr::rename('ProlificID' = 'Q3', 'Advice' = '0.Advice',
                'AdviserID' = '0.AdviserID','Guess'='0.Guess','GiverCondition' = '0.Treatment') %>%
  dplyr::mutate(Bet = case_when(bet == 'Compete against the Low Performer Group' ~ 0,
                                bet == 'Compete against the High Performer Group' ~ 1)) %>%
  dplyr::select(ProlificID,ResponseId,Bet, Guess, Advice, AdviserID, GiverCondition) %>%
  dplyr::mutate(GiverCondition = case_when(GiverCondition == 'E' ~ 1,
                                           GiverCondition == 'None' ~ 0)) %>%
  dplyr::mutate(Guess = as.numeric(Guess) )

# randomly select one competitor
AdviseeScore <- data.table::fread(here::here("Data/Cleaned Data/S3_time1_clean.csv"),
                                  header = T,na.strings = '') %>%
  dplyr::select(ProlificID, count, Condition, Gender, Age) %>%
  dplyr::rename('AdviseeScore' = 'count', 'AdviseeCondition' = 'Condition')

Competitors <- data.table::fread(here::here("Data/Cleaned Data/S3_Math50_cleaned.csv"),
                                    header = T,na.strings = '') %>%
  dplyr::select(ProlificID, Score)

Competitors <- Competitors[order(-Competitors$Score),]

HighGroup <- Competitors[1:20,]
LowGroup <- Competitors[31:50,]


CompetitorList <- list()
ScoreList <- list()

set.seed(807)

for(i in 1:nrow(Advisee)){

  random_num <- sample(20, 1)

  if (Advisee[i,]$Bet == 1){
    ID_i <- HighGroup[random_num,"ProlificID"]
    Score_i <- HighGroup[random_num,"Score"]
  }
  if (Advisee[i,]$Bet == 0){
    ID_i <- LowGroup[random_num,"ProlificID"]
    Score_i <- LowGroup[random_num,"Score"]
  }

  CompetitorList <- c(CompetitorList, ID_i)
  ScoreList <- c(ScoreList,Score_i)

}

Competitors_Target <- data_frame(CompetitorID = unlist(CompetitorList) ,CompetitorScore = unlist(ScoreList))

Advisee <- cbind(Advisee,Competitors_Target)

AdviseeResult <- merge(Advisee, AdviseeScore, by = 'ProlificID')

AdviseeResult <- AdviseeResult %>% dplyr::mutate(win = case_when(AdviseeScore>CompetitorScore ~ 1,
                                                                 AdviseeScore==CompetitorScore ~ 1,
                                                                 AdviseeScore<CompetitorScore ~ 0)) %>%
  dplyr::mutate(bonus = ifelse(win == 1, ifelse(Bet == 1, 0.5,0.3), 0))


# calculate expected return

ReturnList <- list()

for(i in 1:nrow(AdviseeResult)){
  performance <- AdviseeResult$AdviseeScore[i]
  if (AdviseeResult$Bet[i] == '1'){
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

AdviseeResult <- cbind(AdviseeResult, Return)

saveRDS(AdviseeResult, file = here::here("Data/Study3_time3.Rds"))

AdviseeResult %>% write.csv("Data/Cleaned Data/S3_time3_clean.csv")


