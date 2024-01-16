library(dplyr)
library(ggplot2)
library(magrittr)

giver <- data.table::fread(here::here("Data/Raw Data/S2_time2.csv"),
                           header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100',gender != 'Other')%>%

  dplyr::select(ResponseId, PROLIFIC_PID,dplyr::starts_with("ranking_"),dplyr::contains("_advice"),Condition,
                gender, age, ethnicity,education, dplyr::starts_with("url.")) %>%
  tidyr::pivot_longer('1_advice':'10_advice') %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = gsub("st|nd|rd|th| rank| \\(most attractive\\)| \\(least attractive\\)", "", value),
                value = as.numeric(value))

duplicates <- apply(giver[,3:12], 1, function(x) any(duplicated(x)))

giver <- giver %>% dplyr::mutate(duplicates = duplicates) %>%
  dplyr::filter(duplicates == 'FALSE')

# clean the url
url <-  giver %>% dplyr::select(ResponseId,dplyr::starts_with("url."))

url <- apply(url[,2:11],2,function(x)
  {
    basename(x)
  })

url <- apply(url,2,function(x)
  {
    gsub("\\\\\\\\|.png$", "",x)
  })

url <- as.data.frame(url)

giver <- giver %>% dplyr::select(-dplyr::starts_with("url."))
giver <- cbind(giver,url)

#find the 7th participant
giverRank <- giver[,grepl('ranking_',names(giver))]

giverRank <- as.data.frame(apply(giverRank, 2, as.numeric))

for(i in 1:nrow(giver)){
    rankList = as.list(giverRank[i,])
    participant <- NA
    if (length(rankList)==length(unique(rankList))){
      urlList = as.list(url[i,])

      #if (7 %in% rankList == TRUE){
      targetNum <- which(rankList == 7)
      #if (length(targetNum)==1){
      participant <- dplyr::nth(urlList,targetNum[1])}
    giver[i,'Target'] <- participant
}
giverStat <- giver %>% dplyr::filter(!is.na(Target)) %>% dplyr::rename('Advice' = 'value')


# We then randomly select one piece of advice for each participant who upload selfies
#advice <- giverStat %>% dplyr::group_by(Target) %>% dplyr::slice_sample(n=1)  %>%
#  dplyr::select(Target,Advice,PROLIFIC_PID)%>% dplyr::rename('QID7_Id'='Target') %>% dplyr::rename('Giver_Id'='PROLIFIC_PID')

giverStat <- giverStat %>% dplyr::mutate(age = as.numeric(age))

saveRDS(giverStat, file = here::here("Data/Study2_time2.Rds"))
giverStat %>% write.csv("Data/Cleaned Data/S2_time2_clean.csv")


#calculate final ranking for each selfie
selfie <- readRDS(here::here("Data/Study2_time1_200.Rds"))

selfies <- as.data.frame(unique(selfie$QID7_Id))
selfies[,c('scores','raterNr')] <- 0

for(i in 1:nrow(giver)){
  rankList = as.list(giverRank[i,])
  urlList = as.list(url[i,])
  for (j in 1:10){
    photo = urlList[j]
    selfies[selfies$`unique(selfie$QID7_Id)`==photo,]$scores <- selfies[selfies$`unique(selfie$QID7_Id)`==photo,]$scores + as.numeric(rankList[j])
    selfies[selfies$`unique(selfie$QID7_Id)`==photo,]$raterNr <- selfies[selfies$`unique(selfie$QID7_Id)`==photo,]$raterNr +1
  }
}
selfies <- selfies %>% dplyr::mutate(finalRank = scores/raterNr, finalRank = round(finalRank)) %>% dplyr::rename('QID7_Id'='unique(selfie$QID7_Id)') %>%
  dplyr::filter(!is.na(finalRank))
selfieStst <- merge(selfie,selfies,by = 'QID7_Id') %>%
  dplyr::select(-c('scores','raterNr'))


saveRDS(selfieStst, file = here::here("Data/Study2_finalRank.Rds"))
selfieStst %>% write.csv("Data/Cleaned Data/S2_time2_finalRank.csv")
