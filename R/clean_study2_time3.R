library(dplyr)
library(ggplot2)
library(magrittr)


Target <- read.csv("Data/Cleaned Data/S2_time1_clean.csv")[-c(1,3)]

Receiver <- data.table::fread(here::here("Data/Raw Data/S2_time3.csv"),
                              header = T,na.strings = '')[-c(1,2)]  %>%
dplyr::filter(Progress == '100' & Status == 'IP Address') %>%
  dplyr::rename('ProlificID' = 'Q3', 'Advice' = '0.Advice','GiverID' = '0.GiverID','IntGuess' = 'Guess',
                'GiverCondition' = '0.Condition') %>%
  dplyr::mutate(Bet = case_when(bet == 'Compete against the Low Performer Group' ~ 0,
                                bet == 'Compete against the High Performer Group' ~ 1)) %>%
  dplyr::select(ProlificID,Bet, IntGuess, Advice, GiverID, GiverCondition) %>%
  dplyr::mutate(GiverCondition = case_when(GiverCondition == 'E' ~ 1,
                                           GiverCondition == 'None' ~ 0))

Receiver <- merge(Receiver,Target,by = 'ProlificID')


Competitors <- data.table::fread(here::here("Data/Raw Data/S2_Ability50.csv"),
                                 header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100' & Status == 'IP Address' & !is.na(QID3) & !is.na(Gender) ) %>%
  dplyr::rename('ProlificID' = 'QID3', 'score' = 'Score') %>%
  dplyr::mutate(Score = as.numeric(score)) %>%
  dplyr::select(ProlificID,ResponseId, Score,
                Gender, Age, Ethnicity,Education)

Competitors %>% write.csv("Data/Cleaned Data/S2_Ability50_clean.csv")


# Paired with competitors
Competitors <- Competitors[order(-Competitors$Score),] %>%
  dplyr::select(ProlificID, Score)

HighGroup <- Competitors[1:20,]
LowGroup <- Competitors[31:50,]

CompetitorList <- list()
ScoreList <- list()

set.seed(807)

for(i in 1:nrow(Receiver)){

  random_num <- sample(20, 1)

  if (Receiver[i,]$Bet == 1){
    ID_i <- HighGroup[random_num,"ProlificID"]
    Score_i <- HighGroup[random_num,"Score"]
  }
  if (Receiver[i,]$Bet == 0){
    ID_i <- LowGroup[random_num,"ProlificID"]
    Score_i <- LowGroup[random_num,"Score"]
  }

  CompetitorList <- c(CompetitorList, ID_i)
  ScoreList <- c(ScoreList,Score_i)

}

Competitors_Target <- data_frame(CompetitorID = unlist(CompetitorList) ,CompetitorScore = unlist(ScoreList))

Receiver <- cbind(Receiver,Competitors_Target)

Receiver <- Receiver %>% dplyr::mutate(win = case_when(Score>CompetitorScore ~ 1,
                                                       Score==CompetitorScore ~ 1,
                                                       Score<CompetitorScore ~ 0)) %>%
  dplyr::mutate(bonus = ifelse(win == 1, ifelse(Bet == 1, 0.5,0.3), 0))



saveRDS(Receiver, file = here::here("Data/Study2_time3.Rds"))
Receiver %>% write.csv("Data/Cleaned Data/S2_time3_clean.csv")
