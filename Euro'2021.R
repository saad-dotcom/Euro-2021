library("tidyverse")
library("readr")
library("dplyr")
library("ggplot2")
library("GGally")
library("skimr")
library("viridis")
library("gridExtra")
library("modelr")
library("broom")
library("ggrepel")
library("sqldf")

results=read.csv('Results.csv')
head(results)

winner=vector(mode = "list", length = length(results$home_team))
for (i in 1:length(results$home_team)) {
  if (results$home_score[i] > results$away_score[i]){
    winner[i]=results$home_team[i]
  } else if (results$home_score[i] < results$away_score[i]){
    winner[i]=results$away_team[i]
  } else {
    winner[i]='Draw'
  }
  
}
results$Winning_Team=winner
results$goal_difference=abs(results$home_score-results$away_score)

final_results = select(results, -c('date', 'home_score', 'away_score', 'tournament', 'city', 'country', 'goal_difference'))

# 2 if home-team wins, 0 if away-team wins and 1 for a Draw
for (i in 1:length(final_results$Winning_Team)){
  if (final_results$Winning_Team[i]==final_results$home_team[i]){
    final_results$Winning_Team[i]=2
  } else if (final_results$Winning_Team[i]==final_results$away_team[i]){
    final_results$Winning_Team[i]=0
  } else{
    final_results$Winning_Team[i]=1
  }
}

euro_2021_GS=data.frame(TeamA=c('Turkey','Wales','Denmark','Belgium','England',
                                'Austria','Netherlands','Scotland','Poland',
                                'Spain','Hungary','France','Finland','Turkey',
                                'Italy','Ukraine','Denmark','Netherlands',
                                'Sweden','Croatia','England','Hungary','Portugal',
                                'Spain','Italy','Switzerland','Ukraine','FYR Macedonia',
                                'Finland','Russia','Czech Republic','Croatia','Sweden',
                                'Slovakia','Germany','Portugal'),
                        TeamB=c('Italy','Switzerland','Finland','Russia','Croatia',
                                'FYR Macedonia','Ukraine','Czech Republic','Slovakia',
                                'Sweden','Portugal','Germany','Russia','Wales','Switzerland',
                                'FYR Macedonia','Belgium','Austria','Slovakia','Czech Republic',
                                'Scotland','France','Germany','Poland','Wales','Turkey','Austria',
                                'Netherlands','Belgium','Denmark','England','Scotland','Poland',
                                'Spain','Hungary','France'))


final_results=final_results %>% 
  filter(final_results$home_team==euro_2021_GS$TeamA |
           final_results$away_team==euro_2021_GS$TeamB)


install.packages('fastDummies')
library(fastDummies)
final_results <- dummy_cols(final_results, select_columns = c('home_team','away_team'))
names(final_results) <- make.names(names(final_results))

final_results = select(final_results, -c('home_team','away_team'))
final_results$Winning_Team=as.integer(final_results$Winning_Team)

set.seed(123)
train_index <- sample(seq_len(nrow(final_results)), size = floor(0.70 * nrow(final_results)))

training = final_results[train_index, ]
testing = final_results[-train_index, ]


library(nnet)

model1=multinom(Winning_Team ~ .,data=training)
summary(model1)


training$predicted <- predict(model1, training, "class")
ctable <- table(training$Winning_Team, training$predicted)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)


testing$predicted <- predict(model1, testing, "class")
ctable <- table(testing$Winning_Team, testing$predicted)
ctable
round((sum(diag(ctable))/sum(ctable))*100,2)





ranking=read.csv('fifa_rankings.csv')


#Function for data engineering and prediction
DE_and_predict=function(Final,ranking,final_results,model){
  
  for (i in 1:length(Final$TeamB)) {
    if (Final$TeamA_ranking[i]<Final$TeamB_ranking[i]){
      Final$home_team[i]=Final$TeamA[i]
      Final$away_team[i]=Final$TeamB[i]
    } else{
      Final$home_team[i]=Final$TeamB[i]
      Final$away_team[i]=Final$TeamA[i]
    }
  }
  
  Final = select(Final, -c('TeamB','TeamA','TeamA_ranking','TeamB_ranking'))
  prediction_set = dummy_cols(Final, select_columns = c('home_team','away_team'))
  #names(prediction_set) = make.names(names(prediction_set))
  
  missing_set=final_results[!colnames(final_results)%in%colnames(prediction_set)]
  missing_set=select(missing_set, -c('Winning_Team'))
  
  for (i in 1:length(missing_set)) {
    missing_set[i]=0
  }
  
  missing_set=missing_set[1:length(Final$home_team),]
  prediction_set=cbind(prediction_set,missing_set)
  
  prediction_set_backup=prediction_set
  prediction_set=select(prediction_set,-c('home_team','away_team'))
  
  predictions=predict(model, prediction_set, 'probs')
  for (i in 1:length(prediction_set_backup$home_team)) {
    cat(prediction_set_backup$home_team[i],'vs',prediction_set_backup$away_team[i])
    if (predictions[i,1]<predictions[i,3]){
      print(prediction_set_backup$home_team[i])
    } else if (predictions[i,1]>predictions[i,3]){
      print(prediction_set_backup$away_team[i])
    } else {
      print('Draw')
    }
  }
}

#Group-Stage Results
euro_GS_new1=merge(euro_2021_GS,ranking,by.x=c('TeamA'),by.y=c('Team'),all.x = TRUE)
euro_GS_new1$TeamA_ranking=euro_GS_new1$Position
euro_GS_new1 = select(euro_GS_new1, -c('Points','Position'))

euro_GS_new2=merge(euro_GS_new1,ranking,by.x=c('TeamB'),by.y=c('Team'),all.x = TRUE)
euro_GS_new2$TeamB_ranking=euro_GS_new2$Position
euro_GS_Final = select(euro_GS_new2, -c('Points','Position'))


DE_and_predict(euro_GS_Final,ranking,final_results,model1)


#euro_21_R16

euro_2021_R16=data.frame(TeamA=c('Italy','Wales','Denmark','Austria',
                                 'Scotland','Germany','Spain','England'),
                         TeamB=c('Netherlands','Belgium','Portugal','Poland',
                                 'Sweden','Ukraine','Croatia','France'))

euro_R16_new1=merge(euro_2021_R16,ranking,by.x=c('TeamA'),by.y=c('Team'),all.x = TRUE)
euro_R16_new1$TeamA_ranking=euro_R16_new1$Position
euro_R16_new1 = select(euro_R16_new1, -c('Points','Position'))

euro_R16_new2=merge(euro_R16_new1,ranking,by.x=c('TeamB'),by.y=c('Team'),all.x = TRUE)
euro_R16_new2$TeamB_ranking=euro_R16_new2$Position
euro_R16_Final = select(euro_R16_new2, -c('Points','Position'))

DE_and_predict(euro_R16_Final,ranking,final_results,model1)


#euro21_QF

euro_2021_QF=data.frame(TeamA=c('Scotland','Portugal','Poland','Spain'),
                        TeamB=c('Ukraine','Netherlands','Belgium','France'))

euro_QF_new1=merge(euro_2021_QF,ranking,by.x=c('TeamA'),by.y=c('Team'),all.x = TRUE)
euro_QF_new1$TeamA_ranking=euro_QF_new1$Position
euro_QF_new1 = select(euro_QF_new1, -c('Points','Position'))

euro_QF_new2=merge(euro_QF_new1,ranking,by.x=c('TeamB'),by.y=c('Team'),all.x = TRUE)
euro_QF_new2$TeamB_ranking=euro_QF_new2$Position
euro_QF_Final = select(euro_QF_new2, -c('Points','Position'))

DE_and_predict(euro_QF_Final,ranking,final_results,model1)


#euro21_SF

euro_2021_SF=data.frame(TeamA=c('Belgium','France'),
                        TeamB=c('Scotland','Portugal'))

euro_SF_new1=merge(euro_2021_SF,ranking,by.x=c('TeamA'),by.y=c('Team'),all.x = TRUE)
euro_SF_new1$TeamA_ranking=euro_SF_new1$Position
euro_SF_new1 = select(euro_SF_new1, -c('Points','Position'))

euro_SF_new2=merge(euro_SF_new1,ranking,by.x=c('TeamB'),by.y=c('Team'),all.x = TRUE)
euro_SF_new2$TeamB_ranking=euro_SF_new2$Position
euro_SF_Final = select(euro_SF_new2, -c('Points','Position'))

DE_and_predict(euro_SF_Final,ranking,final_results,model1)


#euro21_F

euro_2021_F=data.frame(TeamA=c('France'),
                       TeamB=c('Belgium'))

euro_F_new1=merge(euro_2021_F,ranking,by.x=c('TeamA'),by.y=c('Team'),all.x = TRUE)
euro_F_new1$TeamA_ranking=euro_F_new1$Position
euro_F_new1 = select(euro_F_new1, -c('Points','Position'))

euro_F_new2=merge(euro_F_new1,ranking,by.x=c('TeamB'),by.y=c('Team'),all.x = TRUE)
euro_F_new2$TeamB_ranking=euro_F_new2$Position
euro_F_Final = select(euro_F_new2, -c('Points','Position'))


for (i in 1:length(euro_F_Final$TeamB)) {
  if (euro_F_Final$TeamA_ranking[i]<euro_F_Final$TeamB_ranking[i]){
    euro_F_Final$home_team[i]=euro_F_Final$TeamA[i]
    euro_F_Final$away_team[i]=euro_F_Final$TeamB[i]
  } else{
    euro_F_Final$home_team[i]=euro_F_Final$TeamB[i]
    euro_F_Final$away_team[i]=euro_F_Final$TeamA[i]
  }
}

euro_F_Final = select(euro_F_Final, -c('TeamB','TeamA','TeamA_ranking','TeamB_ranking'))
prediction_set = dummy_cols(euro_F_Final, select_columns = c('home_team','away_team'))

missing_set=final_results[!colnames(final_results)%in%colnames(prediction_set)]
missing_set=select(missing_set, -c('Winning_Team'))

for (i in 1:length(missing_set)) {
  missing_set[i]=0
}

missing_set=missing_set[1:length(euro_F_Final$home_team),]
prediction_set=cbind(prediction_set,missing_set)

prediction_set_backup=prediction_set
prediction_set=select(prediction_set,-c('home_team','away_team'))

predictions=predict(model1, prediction_set, 'probs')
for (i in 1:length(prediction_set_backup$home_team)) {
  cat(prediction_set_backup$home_team[i],'vs',prediction_set_backup$away_team[i])
  if (predictions[i]<predictions[i+2]){
    print(prediction_set_backup$home_team[i])
  } else if (predictions[i]>predictions[i+2]){
    print(prediction_set_backup$away_team[i])
  } else {
    print('Draw')
  }
}

##The Model Predicts that France will win Euro'2021!!