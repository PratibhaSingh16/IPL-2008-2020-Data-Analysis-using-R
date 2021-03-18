# IPL 2008-2020 Data Analysis using R
 
---
title: "INDIAN PREMIER LEAGUE"
author: "TEAM 12"
date: "2/19/2021"
output:
  word_document: default
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(sqldf)
library(tidyverse)
```

Source of Dataset : https://www.kaggle.com/patrickb1912/ipl-complete-dataset-20082020?select=IPL+Matches+2008-2020.csv

#DATASET DESCRIPTION


Column Name         |Type  |  Description
-----------         |------| -----------
id                  |int   |  Unique id alloted to each match
city                |char  |  City where match was played
date                |char  |  Date on which match was played
player_of_match     |char  |  Man of the Match
venue               |char  | Stadium where match was played
team1               |char  | First of the two teams competing
team2               |char  | Second of the two teams competing
toss_winner         |char  |  Team which was the toss
toss_decision       |char  |Batting or bowling decision made by the toss winning team
winner              |char  |Team that won the match
result              |char  |Winning Factor which is either by runs or wickets
result_margin       |int   |Won By how many runs or wikctes
eliminator          |int   |Y= if the type of match is eliminator otherwise N


This Dataset Contains Data of IPL Matches starting from 2008 to 2020.

The IPL is the most attended cricket league in the world and rank sixth among all sports leagues. In 2010 the IPL became the first sporting event in the world to be broadcasted live in YOUTUBE . The brand value of IPL was estimated to be US $3.2 billion in 2014. According to BCCI, the 2015 IPL Season contributed 11.5 million to the GDP of the Indian economy.

![](C:/Users/divya/Downloads/MicrosoftTeams-image.png)

Before we begin with the analysis, lets first load the data and have a quick look at it :

```{r }
ipl <- read.csv("C:/Users/divya/Downloads/IPL DATASET/IPL Matches 2008-2020.csv")
str(ipl)
attach(ipl)

```

We can clearly see that this dataset contains 17 columns which includes id, city, date, player_of_match, venue neutral, venue, team1, team2, toss_winner, toss_decision, winner, result, result_margin, eliminator, method, umpire1 and umpire2. 

## Data Cleaning

Only thing left before starting the analysis is cleaning the data by deleting unwanted columns and making minor adjustments in the data to be able to write and understand the code clearly.

```{r }

##Removing Irrelevant features
ipl <- select(ipl, -c(method,umpire1,umpire2,neutral_venue))

teams <- list("Kolkata Knight Riders" = "KKR","Chennai Super Kings"= "CSK","Rajasthan Royals"="RR","Royal Challengers Bangalore"="RCB","Deccan Chargers"="SRH",
              "Kings XI Punjab" = "KXIP","Delhi Daredevils" = "DC","Mumbai Indians"="MI","Kochi Tuskers Kerala"="KTK","Pune Warriors"="PW","Sunrisers Hyderabad"="SRH",
              "Rising Pune Supergiants" = "RPS","Gujarat Lions"="GL","Rising Pune Supergiant"="RPS","Delhi Capitals"="DC")

for ( i in seq(1,length(teams),1)){
  ipl <- ipl %>% mutate_all(funs(str_replace(.,names(teams)[i],teams[[names(teams)[i]]])))
}

ipl$result_margin <- as.numeric(ipl$result_margin)
```
We have successfully deleted the unwanted columns(“umpire1, umpire2, method,neutral_venue”). And also replaced the names of the Teams with their respective abbreviations. 

## Exploratory Data Analysis

Lets start with finding out total number of matches played from 2008 to 2020. 

How many matches we’ve got in the dataset?

```{r Point1}
length(ipl$id)
```

Our dataset says that there have been `r length(ipl$id)` circket matches played in the  history of IPL till now. That's a huge number.

How many seasons we’ve got in the dataset?

```{r Point2}
ipl <- ipl %>%  mutate(date = as.Date(date, format= "%Y-%m-%d") )

ipl <- ipl %>% 
  mutate(
    season = case_when(
      as.numeric(format(ipl$date, "%Y")) == 2008 ~ "1",
      as.numeric(format(ipl$date, "%Y")) == 2009 ~ "2",
      as.numeric(format(ipl$date, "%Y")) == 2010 ~ "3",
      as.numeric(format(ipl$date, "%Y")) == 2011 ~ "4",
      as.numeric(format(ipl$date, "%Y")) == 2012 ~ "5",
      as.numeric(format(ipl$date, "%Y")) == 2013 ~ "6",
      as.numeric(format(ipl$date, "%Y")) == 2014 ~ "7",
      as.numeric(format(ipl$date, "%Y")) == 2015 ~ "8",
      as.numeric(format(ipl$date, "%Y")) == 2016 ~ "9",
      as.numeric(format(ipl$date, "%Y")) == 2017 ~ "10",
      as.numeric(format(ipl$date, "%Y")) == 2018 ~ "11",
      as.numeric(format(ipl$date, "%Y")) == 2019 ~ "12",
      as.numeric(format(ipl$date, "%Y")) == 2020 ~ "13",
      TRUE ~ "other"
    )
  )

```

There are total of `r length(unique(ipl$season))` seasons with latest being played in 2020 amidst the pandemic. 


Now Lets find out which stadium hosts most number of matches.

Number of matches played in different stadiums 
```{r }
ggplot(ipl,aes(venue, rm.na=T)) + geom_bar(fill="Blue") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of Matches Played")

ggplot(ipl[which(!is.na(ipl$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Number of Matches Played") +
  guides(fill=FALSE) 
```
It is pretty evident that Eden Gardens, Feroz Shah Kotla, MChinnaswamy, PCA Mohali and Wankhede Stadium are top 5 stadium to host most number of matched over the years. This is probably because Eliminators and Finals are usually played in these major grounds. 

We all know the format of the tournament have changed over the years. So lets see which was the longest season. 

## Which Season had most number of matches?

```{r point 5}

library(ggplot2)
season_match_count <- ipl %>% group_by(season) %>% summarise(count = n()) 
print(season_match_count, row.names = FALSE)
ggplot(season_match_count, 
       aes(x = season, 
           y = count)) + 
  geom_bar(stat = "identity",fill = "cornflowerblue", color="black") +
  geom_text(aes(label = count), 
            vjust=-0.5) +
  labs(x = "Season", 
       y = "No. of matches", 
       title  = "Matches played per season")+
  theme_minimal() +
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )

most_wins <- ipl %>% group_by(season) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(row_number()==1)
print(paste0("Season ",most_wins$season, " had most number of matches, : " ,most_wins$count ))

```
After viewing the plot, it is evident that the management changed the format in the initial year but from season 7 to season 13 total number of matched are 60. 

Note: Seasons 8 and 10 do not have 60 matches probably because some matches might have been called off because of rain or any other reasons. 

It will be fun to see which team the match while batting first with the highest margin 

Which Team had won by maximum runs ?

```{r point3}
max_run_win <- ipl %>% group_by(result) %>% summarise(count = max(result_margin)) %>% filter(result == "runs")

team <- ipl %>% filter( result_margin == max_run_win$count) %>% select(winner,result,result_margin)
team

```
Which Team had won by maximum wicket?
```{r point4}
max_wick_win <- ipl %>% group_by(result) %>% summarise(count = max(result_margin)) %>% filter(result == "wickets")

team_wick <- ipl %>% filter( result_margin == max_wick_win$count & result == "wickets" ) %>% distinct(winner,result,result_margin)

team_wick

```


## Which IPL Team is more successful?

There could be multiple parameters on which we can decide which team is more successful. 

1)Most matches won by a team 
```{r point 6}
successful_team <- ipl %>% group_by(winner) %>% summarise(wins = n())  %>% arrange(desc(wins)) %>% top_n(n=1) 

successful_team

```

2)Match winning %

Overview of matches played, toss won, wins, and losses for each team in the IPL history.

```{r }
team1 <- ipl %>% group_by(team1) %>% summarise(count = n()) %>% arrange(team1)

team2 <- ipl %>% group_by(team2) %>% summarise(count = n()) %>% arrange(team2)

toss_win <- ipl %>% group_by(toss_winner) %>% summarise(count = n()) %>% arrange(toss_winner)

match_win <- ipl %>% group_by(winner) %>% summarise(count = n()) %>% arrange(winner)

team_summary <- sqldf("select a.team1, (a.count + b.count) as total_matches, c.count as toss_wins, d.count as match_wins,(a.count + b.count) - d.count as match_loss from team1 a inner join team2 b on a.team1=b.team2 inner join toss_win c on  a.team1 = c.toss_winner inner join match_win d on a.team1=d.winner" )

print(team_summary)

ggplot(team_summary, aes(x = team1, y = match_wins  , color = team1 , size = match_wins)) +
  geom_point()

```

3)Most number of Title wins 

List the winner of each season

```{r }
season_final <- ipl %>% group_by(season) %>% summarise(finale = max(date)) %>% arrange(season)

season_winner <- sqldf("select a.season, a.winner from ipl a inner join season_final b on a.date=b.finale")

print(season_winner)

```

top 3 most successful teams in IPL 
```{r }
top_teams <- sqldf("select winner, count(winner) as title_count from season_winner group by winner order by 2 desc limit 5")
top_teams

```

Does winning the toss has any advantage ?
```{r }
toss_stats <- ipl %>% filter( toss_winner == winner) %>% group_by(toss_winner) %>% summarise(count = n()) %>% summarise(total = sum(count))

ipl %>% select(id) %>% summarise(count = n()) %>% mutate( winning_prob = (toss_stats$total / count) * 100 )


ipl_stat <- ipl %>% select(toss_winner,winner)
ipl_stat$match_toss<-ifelse(as.character(ipl$toss_winner)==as.character(ipl$winner),"Won","Lost")

ggplot(ipl_stat[which(!is.na(ipl_stat$match_toss)),],aes(match_toss, fill = match_toss))+ 
  geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")


```

In 816 matches over the period of 13 years, 51.2% of the times the team who won the toss also won the match. This clearly says that winning the toss does not have much effect on winning chances. 


Data cleaning for applying models
```{r }
ipl <- ipl  %>% mutate( city = ifelse( is.na(city)  & venue == "Sharjah Cricket Stadium","Sharjah",ifelse(is.na(city) &  venue == "Dubai International Cricket Stadium","Dubai",city)))

```
RANDOM FOREST FOR PREDICTING WINNER
```{r }

ipl <- ipl  %>% mutate( city = ifelse( is.na(city)  & venue == "Sharjah Cricket Stadium","Sharjah",ifelse(is.na(city) &  venue == "Dubai International Cricket Stadium","Dubai",city)))

ipl <- ipl  %>% mutate(winner= replace_na(winner,"Draw"))
sum(is.na(matches))
matches <- ipl %>% select(team1,team2,city,toss_decision,toss_winner,venue,winner)
matches$toss_decision <- as.numeric(as.factor(matches$toss_decision))
matches$city <- as.numeric(as.factor(matches$city))
matches$venue <- as.numeric(as.factor(matches$venue))
matches$winner <- as.factor(matches$winner)

set.seed(123)
train_idx <- sample(nrow(matches), .70*nrow(matches))

matches_train <- matches[train_idx,]
matches_test <- matches[-train_idx,]


library(randomForest)

rf <- randomForest(winner ~ team1 + team2 + venue + toss_winner + city + toss_decision ,data=matches_train)
rf
summary(rf)
pred = predict(rf, matches_test, type ="response")
cm = table(matches_test$winner, pred)
cm
mean(matches_test$winner == pred)

```


## Appendix

Classification is the method of predicting the class of a given input data point. Classification problems fall under the Supervised learning method.

One such classification method is randomForest. 

randomForest function is a supervised classification and regression algorithm. As the name suggests, this algorithm randomly creates a forest with several trees. It can also be used in unsupervised mode for assessing proximities among data points.

Generally, the more trees in the forest the more robust the forest looks like. Similarly, in the random forest classifier, the higher the number of trees in the forest, greater is the accuracy of the results.Random forest builds multiple decision trees (called the forest) and glues them together to get a more accurate and stable prediction. The forest it builds is a collection of Decision Trees, trained with the bagging method.
based on the idea of bagging, which is used to reduce the variation in the predictions by combining the result of multiple Decision trees on different samples of the data set.
