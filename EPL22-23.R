#Installing necessary packages
install.packages("gmodels")
install.packages("NbClust")
install.packages('caret')
install.packages("tidyr")
install.packages("fastcluster")
install.packages("ggcorrplot")
install.packages('e1071')
install.packages("mice")
install.packages("dendextend")
install.packages("Hmisc")
install.packages('C50')
install.packages('tidyverse')
install.packages("gridExtra")
install.packages('e1071')
install.packages("ggplot2")
install.packages('pROC')
install.packages("VIM")
install.packages("randomForest")
install.packages("class")
install.packages("stringr")
install.packages("dplyr")
install.packages('ROCR')
install.packages("corrplot")
install.packages("ggrepel")

library(ROCR)
library(tidyr)
library(ggcorrplot)
library(ggrepel)
library(C50)
library(stringr)
library(gridExtra)
library(NbClust)
library(e1071)
library(dplyr)
library(caret)
library(tidyverse)
library(e1071)
library(corrplot)
library(ggplot2)
library(dendextend)
library(pROC)
library(fastcluster)
library(VIM)
library(class)
library(gmodels)
library(randomForest)
library(mice)
library(Hmisc)

rm(list=ls())

#Loading the csv
InputData <- read.csv("EPL22-23.csv",header = TRUE,stringsAsFactors = T, encoding = 'UTF-8')

#Checking the datatype of each Variable
str(InputData)

#Overall distribution of EPL footballers stats
summary(InputData)
table(InputData$Position)

#Missing values check
md.pattern(InputData, rotate.names = TRUE)

#Outliers Check
outliers <- function(InputData)
{
  InputData %>%  select_if(is.numeric) %>% map(~ boxplot.stats(.x)$out) 
}
outliers(InputData)

#-------------------------DESCRIPTIVE STATISTICS----------------------

#--------------------------FORWARDS-----------------------------------

# Filter forwards who played more than 500 minutes and rank by criteria
forwards_ranked <- InputData %>%
  filter(MinutesPlayed > 500 & Position == "Forward") %>%
  arrange(desc(Shots), desc(DuelsPer90))

# Select top 15 forwards
top_15_forwards <- head(forwards_ranked, 15)

# Create a scatter plot for DuelsPer90 vs. Shots for the top 15 forwards
ggplot(top_15_forwards, aes(x = DuelsPer90, y = Shots, label = FullName)) +
  geom_point(color = "blue", size = 4) +
  geom_text_repel(size = 3.5, nudge_y = 1.5) +
  theme_minimal() +
  labs(title = "DuelsPer90 vs. Shots for Top 15 Forwards",
       x = "Duels Per 90",
       y = "Number of Shots")


# Filter the data for forwards who played more than 500 minutes
forwards_ranked <- InputData %>%
  filter(MinutesPlayed > 500 & Position == "Forward") %>%
  arrange(desc(Dribbles), desc(KeyPasses))  # Sort by Dribbles and then by KeyPasses

# Select the top 15 forwards
top_15_forwards <- head(forwards_ranked, 15)

# Create the scatter plot for Dribbles vs. KeyPasses
ggplot(top_15_forwards, aes(x = Dribbles, y = KeyPasses)) +
  geom_point(color = "blue", size = 5, alpha = 0.7) +  # Adjust color, size, and transparency of points if needed
  geom_text_repel(aes(label = FullName), nudge_x = 0.5) +  # Use ggrepel to prevent text overlapping
  theme_minimal() +
  labs(title = "Dribbles vs. KeyPasses for Top 15 Forwards",
       x = "Dribbles",
       y = "KeyPasses")

#--------------------------MIDFIELDERS-----------------------------------
# Filter midfielders who played more than 500 minutes and rank by criteria
midfielders_ranked <- InputData %>%
  filter(MinutesPlayed > 500 & Position == "Midfielder") %>%
  arrange(desc(xA), desc(Crosses))

# Select top 15 midfielders
top_15_midfielders <- head(midfielders_ranked, 15)

# Create a scatter plot for Crosses vs. xA for the top 15 midfielders
ggplot(top_15_midfielders, aes(x = Crosses, y = xA, label = FullName)) +
  geom_point(color = "red", size = 4) +
  geom_text_repel(size = 3.5, nudge_y = 0.5) +
  theme_minimal() +
  labs(title = "Crosses vs. xA for Top 15 Midfielders",
       x = "Number of Crosses",
       y = "xA (Expected Assists)")

#--------------------------DEFENDERS-----------------------------------
# Filter defenders who played more than 500 minutes
defenders_ranked <- InputData %>%
  filter(MinutesPlayed > 500 & Position == "Defender") %>%
  arrange(desc(Tackles), desc(Passes))

# Select top 10 defenders
top_10_defenders <- head(defenders_ranked, 10)

# Create a bubble chart for Passes vs. Tackles
ggplot(top_10_defenders, aes(x = Passes, y = Tackles)) +
  geom_point(aes(color = FullName), size = 5, alpha = 0.6) +  # Setting alpha for a bit of transparency
  scale_color_brewer(palette = "Set3") +  # A different color palette for distinction
  geom_text_repel(aes(label = FullName), size = 4, nudge_x = 0.5) +  # You might need to adjust nudge_x to ensure text doesn't overlap too much
  theme_minimal() +
  labs(title = "Tackles vs. Passes for Top 10 Defenders",
       x = "Passes",
       y = "Number of Tackles")


#--------------------------GOALKEEPERS-----------------------------------
# Filter goalkeepers who played more than 500 minutes
goalkeepers_ranked <- InputData %>%
  filter(MinutesPlayed > 500 & Position == "Goalkeeper") %>%
  arrange(desc(SavePercentage), desc(PassCompletionRate))

# Select top 15 goalkeepers
top_15_goalkeepers <- head(goalkeepers_ranked, 15)

# Create a bubble chart for PassCompletionRate vs. SavePercentage
ggplot(top_15_goalkeepers, aes(x = PassCompletionRate, y = SavePercentage)) +
  geom_point(color = "orange", size = 4, alpha = 0.6) +  # All points will be orange and of the same size
  geom_text_repel(aes(label = FullName), nudge_x = 0.5) +  # Use ggrepel to prevent text overlapping
  theme_minimal() +
  labs(title = "Save Percentage vs. Pass Completion Rate for Top 15 Goalkeepers",
       x = "Pass Completion Rate",
       y = "Save Percentage")

#------------------------------------------------------------------------
#Datatype conversion of columns to numeric
Input_DataType <- InputData %>% mutate_at(c('MinutesPlayed',
                                            'Appearances',
                                            'Goals',
                                            'Assists',
                                            'PenaltyGoals',
                                            'PenaltyMisses',
                                            'Cleansheets',
                                            'Conceded',
                                            'YellowCards',
                                            'RedCards',
                                            'MinsPerMatch',
                                            'AverageRatingOverall',
                                            'PassesPer90',
                                            'Passes',
                                            'PassesCompletedPer90',
                                            'KeyPassesPer90',
                                            'KeyPasses',
                                            'TacklesPer90',
                                            'Tackles',
                                            'ShotsPer90',
                                            'Shots',
                                            'ShotsOnTarget',
                                            'ShotsOnTargetPer90',
                                            'Interceptions',
                                            'InterceptionsPer90',
                                            'Crosses',
                                            'CrossesPer90',
                                            'Dribbles',
                                            'DribblesPer90',
                                            'DribblesSuccessful',
                                            'DribblesSuccessfulPer90',
                                            'Saves',
                                            'SavesPer90',
                                            'ShotsFaced',
                                            'ShotsFacedPer90',
                                            'SavePercentage',
                                            'xG',
                                            'PassCompletionRate',
                                            'DribbledPastPer90',
                                            'DribbledPast',
                                            'InsideBoxSaves',
                                            'BlocksPer90',
                                            'Blocks',
                                            'ClearancesPer90',
                                            'Clearances',
                                            'PenaltiesCommitted',
                                            'Punches',	
                                            'PunchesPer90',	
                                            'Offisdes',
                                            'xA',
                                            'xAPer90',
                                            'nPxG',
                                            'xPxGPer90',
                                            'FoulsDrawn',
                                            'FoulsDrawnPer90',
                                            'FoulsCommittedPer90',
                                            'FoulsCommitted',
                                            'xGPer90',
                                            'AerialDuelsWon',
                                            'AerialDuelsWonPer90',
                                            'DuelsPer90',
                                            'Duels',
                                            'DuelsWon',
                                            'DuelsWonPer90',
                                            'Dispossesed',
                                            'DispossesedPer90',
                                            'AccurateCrosses',
                                            'AccurateCrossesPer90'), as.numeric)

str(Input_DataType)

#Variables for which lesser value is considered to be better, are inversed
Inv = c('PenaltyMisses',
        'Conceded',
        'YellowCards',
        'RedCards',
        'DribbledPast',
        'PenaltiesCommitted',	
        'Offisdes',
        'FoulsCommittedPer90',
        'FoulsCommitted',
        'Dispossesed',
        'DispossesedPer90')

Input_DataType[Inv] = 1/Input_DataType[Inv] 
Input_DataType <- replace(Input_DataType, Input_DataType==Inf, 0)

#Creating subsets based on positions
Input_DataFor <- subset(Input_DataType, Position == "Forward")
Input_DataMid <- subset(Input_DataType, Position == "Midfielder")
Input_DataDef <- subset(Input_DataType, Position == "Defender")
Input_DataGoal <- subset(Input_DataType, Position == "Goalkeeper")

#Normalizing values of variables to a single scale
Nor_ColsFor <- c('MinutesPlayed',
                 'Appearances',
                 'Goals',
                 'Assists',
                 'PenaltyMisses',
                 'YellowCards',
                 'RedCards',
                 'MinsPerMatch',
                 'AverageRatingOverall',
                 'PassesPer90',
                 'Passes',
                 'PassesCompletedPer90',
                 'KeyPassesPer90',
                 'KeyPasses',
                 'ShotsPer90',
                 'Shots',
                 'ShotsOnTarget',
                 'ShotsOnTargetPer90',
                 'Interceptions',
                 'InterceptionsPer90',
                 'Crosses',
                 'CrossesPer90',
                 'Dribbles',
                 'DribblesPer90',
                 'DribblesSuccessful',
                 'DribblesSuccessfulPer90',
                 'xG',
                 'Offisdes',
                 'xA',
                 'xAPer90',
                 'nPxG',
                 'xPxGPer90',
                 'FoulsDrawn',
                 'FoulsDrawnPer90',
                 'xGPer90',
                 'AerialDuelsWon',
                 'AerialDuelsWonPer90',
                 'DuelsPer90',
                 'Duels',
                 'DuelsWon',
                 'DuelsWonPer90')

Nor_ColsMid <- c('MinutesPlayed',
                 'Appearances',
                 'Goals',
                 'Assists',
                 'PenaltyMisses',
                 'YellowCards',
                 'RedCards',
                 'MinsPerMatch',
                 'AverageRatingOverall',
                 'PassesPer90',
                 'Passes',
                 'PassesCompletedPer90',
                 'KeyPassesPer90',
                 'KeyPasses',
                 'TacklesPer90',
                 'Tackles',
                 'ShotsPer90',
                 'Interceptions',
                 'InterceptionsPer90',
                 'Crosses',
                 'CrossesPer90',
                 'Dribbles',
                 'DribblesPer90',
                 'DribblesSuccessful',
                 'DribblesSuccessfulPer90',
                 'xG',
                 'PassCompletionRate',
                 'BlocksPer90',
                 'Blocks',
                 'PenaltiesCommitted',
                 'xA',
                 'xAPer90',
                 'nPxG',
                 'xPxGPer90',
                 'FoulsDrawn',
                 'FoulsDrawnPer90',
                 'xGPer90',
                 'AerialDuelsWon',
                 'AerialDuelsWonPer90',
                 'DuelsPer90',
                 'Duels',
                 'DuelsWon',
                 'DuelsWonPer90',
                 'Dispossesed',
                 'DispossesedPer90',
                 'AccurateCrosses',
                 'AccurateCrossesPer90')

Nor_ColsDef <- c('MinutesPlayed',
                 'Appearances',
                 'Goals',
                 'Assists',
                 'Cleansheets',
                 'Conceded',
                 'YellowCards',
                 'RedCards',
                 'MinsPerMatch',
                 'AverageRatingOverall',
                 'PassesPer90',
                 'Passes',
                 'PassesCompletedPer90',
                 'KeyPassesPer90',
                 'KeyPasses',
                 'TacklesPer90',
                 'Tackles',
                 'ShotsPer90',
                 'Shots',
                 'Interceptions',
                 'InterceptionsPer90',
                 'Crosses',
                 'CrossesPer90',
                 'Dribbles',
                 'DribblesPer90',
                 'DribblesSuccessful',
                 'DribblesSuccessfulPer90',
                 'xG',
                 'PassCompletionRate',
                 'DribbledPastPer90',
                 'DribbledPast',
                 'BlocksPer90',
                 'Blocks',
                 'ClearancesPer90',
                 'Clearances',
                 'PenaltiesCommitted',
                 'xA',
                 'xAPer90',
                 'nPxG',
                 'xPxGPer90',
                 'FoulsCommittedPer90',
                 'FoulsCommitted',
                 'xGPer90',
                 'AerialDuelsWon',
                 'AerialDuelsWonPer90',
                 'DuelsPer90',
                 'Duels',
                 'DuelsWon',
                 'DuelsWonPer90',
                 'Dispossesed',
                 'DispossesedPer90',
                 'AccurateCrosses',
                 'AccurateCrossesPer90')

Nor_ColsGoal <- c('MinutesPlayed',
                  'Appearances',
                  'Cleansheets',
                  'Conceded',
                  'YellowCards',
                  'RedCards',
                  'MinsPerMatch',
                  'AverageRatingOverall',
                  'Passes',
                  'KeyPasses',
                  'Interceptions',
                  'Saves',
                  'SavesPer90',
                  'ShotsFaced',
                  'ShotsFacedPer90',
                  'SavePercentage',
                  'PassCompletionRate',
                  'InsideBoxSaves',
                  'ClearancesPer90',
                  'Clearances',
                  'PenaltiesCommitted',
                  'Punches',	
                  'PunchesPer90',	
                  'FoulsCommittedPer90',
                  'FoulsCommitted',
                  'AerialDuelsWon',
                  'AerialDuelsWonPer90',
                  'DuelsPer90',
                  'Duels',
                  'DuelsWon',
                  'DuelsWonPer90')

NorDataFor <- as.data.frame(scale(Input_DataFor[Nor_ColsFor]))
NorDataMid <- as.data.frame(scale(Input_DataMid[Nor_ColsMid]))
NorDataDef <- as.data.frame(scale(Input_DataDef[Nor_ColsDef]))
NorDataGK <- as.data.frame(scale(Input_DataGoal[Nor_ColsGoal]))

#Normalized values combined with EPL Footballer Data 
InputNorFor <- cbind(Input_DataFor$FullName,Input_DataFor$Position,NorDataFor)
InputNorMid <- cbind(Input_DataMid$FullName,Input_DataMid$Position,NorDataMid)
InputNorDef <- cbind(Input_DataDef$FullName,Input_DataDef$Position,NorDataDef)
InputNorGK <- cbind(Input_DataGoal$FullName,Input_DataGoal$Position,NorDataGK)

colnames(InputNorFor)[1] ="FullName"
colnames(InputNorMid)[1] ="FullName"
colnames(InputNorDef)[1] ="FullName"
colnames(InputNorGK)[1] ="FullName"

#Correlation check- Forwards
ResFor <- InputNorFor[,c('Appearances',
                         'PenaltyMisses',
                         'YellowCards', 
                         'RedCards',
                         'MinsPerMatch',
                         'KeyPassesPer90',
                         'ShotsPer90',
                         'ShotsOnTargetPer90',
                         'InterceptionsPer90',
                         'CrossesPer90',
                         'DribblesSuccessfulPer90',
                         'xG',
                         'Offisdes',
                         'xA', 
                         'xGPer90',
                         'AerialDuelsWonPer90',
                         'DuelsPer90',
                         'DuelsWonPer90')]

round(ResFor,2)

#Calculating the Correlation
CorrFor= round(cor(ResFor), 2)

#Display Findings
ggcorrplot(CorrFor, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Correlation check- Midfielders
ResMid <- InputNorMid[,c('Appearances',
                          'Assists',
                          'YellowCards',
                          'RedCards',
                          'MinsPerMatch',
                          'PassesPer90',
                          'KeyPassesPer90',
                          'TacklesPer90',
                          'ShotsPer90',
                          'InterceptionsPer90',
                          'CrossesPer90',
                          'DribblesPer90',
                          'DribblesSuccessful',
                          'xG',
                          'PassCompletionRate',
                          'BlocksPer90',
                          'xAPer90',
                          'FoulsDrawnPer90',
                          'xGPer90',
                          'AerialDuelsWonPer90',
                          'DuelsPer90',
                          'DuelsWonPer90')]

round(ResMid,2)

#Calculating the Correlation
CorrMid = round(cor(ResMid), 2)

#Display Findings
ggcorrplot(CorrMid, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Correlation check- Defenders
ResDef <- InputNorDef[,c('Appearances',
                          'Assists',
                          'Cleansheets',
                          'Conceded',
                          'YellowCards',
                          'RedCards',
                          'MinsPerMatch',
                          'PassesPer90',
                          'KeyPassesPer90',
                          'TacklesPer90',
                          'ShotsPer90',
                          'InterceptionsPer90',
                          'CrossesPer90',
                          'DribblesPer90',
                          'PassCompletionRate',
                          'DribbledPastPer90',
                          'BlocksPer90',
                          'ClearancesPer90',
                          'xAPer90',
                          'FoulsCommittedPer90',
                          'xG',
                          'AerialDuelsWon',
                          'Duels',
                          'DuelsWonPer90',
                          'DispossesedPer90')]

round(ResDef,2)

#Calculating the Correlation
CorrDef = round(cor(ResDef), 2)

#Display Findings
ggcorrplot(CorrDef, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Correlation check- Goalkeepers
ResGK <- InputNorGK[,c('Cleansheets',
                          'Conceded',
                          'YellowCards',
                          'RedCards',
                          'MinsPerMatch',
                          'KeyPasses',
                          'Interceptions',
                          'SavesPer90',
                          'ShotsFacedPer90',
                          'SavePercentage',
                          'PassCompletionRate', 
                          'InsideBoxSaves',
                          'ClearancesPer90',
                          'FoulsCommitted',
                          'AerialDuelsWonPer90',
                          'DuelsPer90')]

round(ResGK,2)

#Calculating the Correlation
CorrGK = round(cor(ResGK), 2)

#Display Findings
ggcorrplot(CorrGK, hc.order = TRUE, type = "lower",
           lab = TRUE)

#KPI Identification: Position-wise
#Multiple Linear Regression- Forwards
MLRFor = lm(formula =  AverageRatingOverall ~ Appearances +
               PenaltyMisses +
               YellowCards +
               RedCards +
               MinsPerMatch +
               KeyPassesPer90 +
               ShotsPer90 +
               ShotsOnTargetPer90 +
               InterceptionsPer90 +
               CrossesPer90 +
               DribblesSuccessfulPer90 +
               xG +
               Offisdes +
               xA +
               xGPer90 +
               AerialDuelsWonPer90 +
               DuelsPer90 +
               DuelsWonPer90, data = InputNorFor)

#Display results
summary(MLRFor)

PredictedRatingsFor <- predict(MLRFor, newdata = ResFor)
PredictedRatingsFor

RatingsFor <- data.frame(InputNorFor$FullName,round(PredictedRatingsFor,2))

#Multiple Linear Regression- Midfielders
MLRMid = lm(formula =  AverageRatingOverall ~ Appearances +
               Assists +
               YellowCards +
               RedCards +
               MinsPerMatch +
               PassesPer90 +
               KeyPassesPer90 +
               TacklesPer90 +
               ShotsPer90 +
               InterceptionsPer90 +
               CrossesPer90 +
               DribblesPer90 +
               DribblesSuccessful +
               xG +
               PassCompletionRate +
               BlocksPer90 +
               xAPer90 +
               FoulsDrawnPer90 +
               xGPer90 +
               AerialDuelsWonPer90 +
               DuelsPer90, data = InputNorMid)

#Display results
summary(MLRMid)

PredictedRatingsMid <- predict(MLRMid, newdata = ResMid)
PredictedRatingsMid

RatingsMid <- data.frame(InputNorMid$FullName,round(PredictedRatingsMid,2))

#Multiple Linear Regression- Defenders
MLRDef = lm(formula =  AverageRatingOverall ~ Assists +
               Cleansheets +
               Conceded +
               YellowCards +
               RedCards +
               MinsPerMatch +
               PassesPer90 +
               KeyPassesPer90 +
               TacklesPer90 +
               ShotsPer90 +
               InterceptionsPer90 +
               CrossesPer90 +
               DribblesPer90 +
               PassCompletionRate +
               DribbledPastPer90 +
               BlocksPer90 +
               ClearancesPer90 +
               xAPer90 +
               FoulsCommittedPer90 +
               xG +
               AerialDuelsWon +
               DuelsWonPer90 +
               DispossesedPer90, data = InputNorDef)

#Display results
summary(MLRDef)

PredictedRatingsDef <- predict(MLRDef, newdata = ResDef)
PredictedRatingsDef

RatingsDef <- data.frame(InputNorDef$FullName,round(PredictedRatingsDef,2))

#Multiple Linear Regression- GK
MLRGK = lm(formula =  AverageRatingOverall ~ Cleansheets +
                Conceded +
                YellowCards +
                RedCards +
                MinsPerMatch +
                KeyPasses +
                Interceptions +
                SavesPer90 +
                SavePercentage +
                PassCompletionRate +
                InsideBoxSaves +
                ClearancesPer90 +
                FoulsCommitted +
                AerialDuelsWonPer90 +
                DuelsPer90, data = InputNorGK)

#Display results
summary(MLRGK)

PredictedRatingsGK <- predict(MLRGK, newdata = ResGK)
PredictedRatingsGK

RatingsGK <- data.frame(InputNorGK$FullName,round(PredictedRatingsGK,2))

#......................Multiple Linear Regression...............................

#Significant KPIs- Forwards
MLRForKPI = lm(formula =  AverageRatingOverall ~ MinsPerMatch +
                   ShotsPer90 +
                   DribblesSuccessfulPer90 +
                   DuelsPer90 +
                   DuelsWonPer90, data = InputNorFor)

#Display results
summary(MLRForKPI)

PredictedRatingsForKPI <- predict(MLRForKPI, newdata = ResFor)
PredictedRatingsForKPI

RatingsForKPI <- data.frame(InputNorFor$FullName,round(PredictedRatingsForKPI,2))

MAEForKPI <- mean(abs(InputNorFor$AverageRatingOverall - PredictedRatingsForKPI))
MAEForKPI

#Significant KPIs- Midfielders
MLRMidKPI = lm(formula =  AverageRatingOverall ~ MinsPerMatch +
                   PassesPer90 +
                   xGPer90 +
                   DuelsPer90 + YellowCards + 
                   AerialDuelsWonPer90, data = InputNorMid)

#Display results
summary(MLRMidKPI)

PredictedRatingsMidKPI <- predict(MLRMidKPI, newdata = ResMid)
PredictedRatingsMidKPI

RatingsMidKPI <- data.frame(InputNorMid$FullName,round(PredictedRatingsMidKPI,2))

MAEMidKPI <- mean(abs(InputNorMid$AverageRatingOverall - PredictedRatingsMidKPI))
MAEMidKPI

#Significant KPIs- Defenders
MLRDefKPI = lm(formula =  AverageRatingOverall ~ Cleansheets +
                   YellowCards +
                   MinsPerMatch +
                   TacklesPer90 +
                   CrossesPer90 +
                   ClearancesPer90 +
                   xG +
                   AerialDuelsWon, data = InputNorDef)

#Display results
summary(MLRDefKPI)

PredictedRatingsDefKPI <- predict(MLRDefKPI, newdata = ResDef)
PredictedRatingsDefKPI

RatingsDefKPI <- data.frame(InputNorDef$FullName,round(PredictedRatingsDefKPI,2))

MAEDefKPI <- mean(abs(InputNorDef$AverageRatingOverall - PredictedRatingsDefKPI))
MAEDefKPI

#Significant KPIs- GKs
MLRGkKPI = lm(formula =  AverageRatingOverall ~ Conceded +
                    SavePercentage + SavesPer90 + InsideBoxSaves +
                    MinsPerMatch, data = InputNorGK)

#Display results
summary(MLRGkKPI)

PredictedRatingsGKKPI <- predict(MLRGkKPI, newdata = ResGK)
PredictedRatingsGKKPI

RatingsGkKPI <- data.frame(InputNorGK$FullName,round(PredictedRatingsGKKPI,2))

MAEGkKPI <- mean(abs(InputNorGK$AverageRatingOverall - PredictedRatingsGKKPI))
MAEGkKPI

#...........................Random Forest...............................

#...................KPIs- Forwards.............................
#Data split- Traiing & Testing data

set.seed(12345)
TrainForCountRF <- sample(nrow(InputNorFor), 0.8 * nrow(InputNorFor))
TrainForRF <- InputNorFor[, c("AverageRatingOverall","MinsPerMatch","ShotsPer90","DribblesSuccessfulPer90","DuelsPer90","DuelsWonPer90")][TrainForCountRF, ]
TestForRF <- InputNorFor[, c("AverageRatingOverall","MinsPerMatch","ShotsPer90","DribblesSuccessfulPer90","DuelsPer90","DuelsWonPer90")][-TrainForCountRF, ]

#Training
RF_For <- randomForest(AverageRatingOverall ~ ., data = TrainForRF)

#Rank Prediction
TestForRF$PredictedRanking <- predict(RF_For, newdata = TestForRF)
InputNorFor$PredictedRankingRF <- predict(RF_For, newdata = InputNorFor)

RatingsForKPIRF <- data.frame(InputNorFor$FullName,round(InputNorFor$PredictedRankingRF,2))

#MAE Calculation
MAEForKpiRF <- mean(abs(TestForRF$AverageRatingOverall - TestForRF$PredictedRanking))  
MAEForKpiRF

#...................KPIs- Midfielders.............................
#Data split- Traiing & Testing data
TrainMidCountRF <- sample(nrow(InputNorMid), 0.8 * nrow(InputNorMid))
TrainMidRF <- InputNorMid[, c("AverageRatingOverall","MinsPerMatch","PassesPer90","xGPer90","DuelsPer90","YellowCards","AerialDuelsWonPer90")][TrainMidCountRF, ]
TestMidRF <- InputNorMid[, c("AverageRatingOverall","MinsPerMatch","PassesPer90","xGPer90","DuelsPer90","YellowCards","AerialDuelsWonPer90")][-TrainMidCountRF, ]

#Training
RF_Mid <- randomForest(AverageRatingOverall ~ ., data = TrainMidRF)

#Rank Prediction
TestMidRF$PredictedRanking <- predict(RF_Mid, newdata = TestMidRF)
InputNorMid$PredictedRankingRF <- predict(RF_Mid, newdata = InputNorMid)

RatingsMidKPIRF <- data.frame(InputNorMid$FullName,round(InputNorMid$PredictedRankingRF,2))

#MAE Calculation
MAEMidKpiRF <- mean(abs(TestMidRF$AverageRatingOverall - TestMidRF$PredictedRanking))  
MAEMidKpiRF

#...................KPIs- Defenders.............................
#Data split- Traiing & Testing data
TrainDefCountRF <- sample(nrow(InputNorDef), 0.8 * nrow(InputNorDef))
TrainDefRF <- InputNorDef[, c("AverageRatingOverall","Cleansheets","YellowCards","MinsPerMatch","TacklesPer90","CrossesPer90","ClearancesPer90","xG","AerialDuelsWon")][TrainDefCountRF, ]
TestDefRF <- InputNorDef[, c("AverageRatingOverall","Cleansheets","YellowCards","MinsPerMatch","TacklesPer90","CrossesPer90","ClearancesPer90","xG","AerialDuelsWon")][-TrainDefCountRF, ]

#Training
RF_Def <- randomForest(AverageRatingOverall ~ ., data = TrainDefRF)

#Rank Prediction
TestDefRF$PredictedRanking <- predict(RF_Def, newdata = TestDefRF)
InputNorDef$PredictedRankingRF <- predict(RF_Def, newdata = InputNorDef)

RatingsDefKPIRF <- data.frame(InputNorDef$FullName,round(InputNorDef$PredictedRankingRF,2))

#MAE Calculation
MAEDefKpiRF <- mean(abs(TestDefRF$AverageRatingOverall - TestDefRF$PredictedRanking))  
MAEDefKpiRF

#...................KPIs- GKs.............................
#Data split- Traiing & Testing data
TrainGKCountRF <- sample(nrow(InputNorGK), 0.8 * nrow(InputNorGK))
TrainGKRF <- InputNorGK[, c("AverageRatingOverall","Conceded","SavePercentage","MinsPerMatch","SavesPer90","InsideBoxSaves")][TrainGKCountRF, ]
TestGkRF <- InputNorGK[, c("AverageRatingOverall","Conceded","SavePercentage","MinsPerMatch","SavesPer90","InsideBoxSaves")][-TrainGKCountRF, ]

#Training
RF_GK <- randomForest(AverageRatingOverall ~ ., data = TrainGKRF)

#Rank Prediction
TestGkRF$PredictedRanking <- predict(RF_GK, newdata = TestGkRF)
InputNorGK$PredictedRankingRF <- predict(RF_GK, newdata = InputNorGK)

RatingsGkKPIRF <- data.frame(InputNorGK$FullName,round(InputNorGK$PredictedRankingRF,2))

#MAE Calculation
MAEGkKpiRF <- mean(abs(TestGkRF$AverageRatingOverall - TestGkRF$PredictedRanking))  
MAEGkKpiRF

