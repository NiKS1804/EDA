install.packages('dplyr')
library(dplyr)
odi = read.csv(file.choose())
odi
dim(odi)
typeof(odi)
View(odi)
str(odi)

#unique countries
countries_unique=unique(odi$Country)
countries_unique
players_unique<-unique(odi$Player)
players_unique
length(players_unique)

#Numerical columns
min(odi$Runs,na.rm = TRUE)  #omitting na value
max(odi$Runs,na.rm = TRUE)
mean(odi$Runs,na.rm = TRUE)  #CALCULATING MEAN
sd(odi$Runs,na.rm = TRUE)    #CALCULATING STANDARD DEVIATION

min(odi$ScoreRate,na.rm = TRUE)
max(odi$ScoreRate,na.rm = TRUE)

View(odi)

#GROUPING IN R USING DPLYR PACKAGE
#calculate playerwise total runs

player_total_runs = odi %>% group_by(Player) %>% summarise(Total_Runs = sum(Runs, na.rm= TRUE))
player_total_runs
class(player_total_runs)
View(player_total_runs)

#playerwise average runs

player_avg_runs = odi %>% group_by(Player) %>% summarise(player_avg_runs=mean(Runs,na.rm=TRUE))
player_avg_runs
head(player_avg_runs)
player_avg_runs=player_avg_runs %>% arrange(-avg_runs)
player_avg_runs
View(player_avg_runs)


#R-markdown to create reports
install.packages("rmarkdown")

#learning packages
?group_by
?filter
??min
#How to create two summary statistics in a single summarise function

#top 10 players by total runs
top_players= odi %>% group_by(Player) %>% summarise(Total_runs=sum(Runs,na.rm=TRUE),
Country=first(Country))%>% arrange(-Total_runs)
View(top_players)
library(ggplot2)

ggplot(top_players,aes(x=Player,y=Total_runs))+ geom_bar(stat = 'identity')


#1top 10 players by total number of centuries
#2top 10 players by total number of ducks
#3top 10 countries by total number of players
#4top 10 grounds by total centuries

top_players= odi %>% group_by(Player) %>% filter(Runs,Runs>99)
top_players

#1top 10 players by total number of centuries
odi_centuries = filter(odi, Runs > 99)
odi_centuries
dim(odi_centuries)
View(odi_centuries)
players_centuries = odi_centuries %>% group_by(Player) %>% summarise(centuries =n())#to count the number of centuries
players_centuries=players_centuries %>% arrange(-centuries)
head(players_unique)

#2top 10 players by total number of ducks
odi_ducks = filter(odi, Runs< 1)
odi_ducks
players_ducks = odi_ducks %>% group_by(Player) %>% summarise(ducks =n())
players_ducks = players_ducks %>% arrange(-ducks)
players_ducks

#4 top 10 grounds by total centuries
odi_centuries = filter(odi, Runs > 99)
odi_centuries
dim(odi_centuries)
View(odi_centuries)
top_ground = odi_centuries %>% group_by(Ground) %>% summarise(centuries =n()) %>% arrange(-centuries)
top_ground
View(top_ground)

#3 top 10 countries by total number of players
top_country=odi %>% group_by(Country) %>% summarise(total_Players=length(unique(Player)))
top_country=top_country %>% arrange(-total_Players)
top_country




###EDA-2 on 22 aug 2017

#filter the data for sachin & identify the ducks sachin has scored

data_sachin=filter(odi,Player=='Sachin R Tendulkar')
data_sachin
rows_sachin_ducks=filter(data_sachin, Runs==0)
rows_sachin_ducks
View(rows_sachin_ducks)
dim(rows_sachin_ducks)

##maximum ducks against which country

data_sachin=filter(odi,Player=='Sachin R Tendulkar')
data_sachin
rows_sachin_ducks=filter(data_sachin, Runs==0)
versus_ducks =rows_sachin_ducks %>% group_by(Versus) %>% summarise(ducks=n()) %>% arrange(-ducks)
versus_ducks


#number of centuries against each country
data_sachin=filter(odi,Player=='Sachin R Tendulkar')
data_sachin
rows_sachin_century=filter(data_sachin, Runs>=100)
versus_century =rows_sachin_century %>% group_by(Year) %>% summarise(century=n()) %>% arrange(-century)
versus_century

#number of missed centuries

data_sachin=filter(odi,Player=='Sachin R Tendulkar')
data_sachin
rows_sachin_century=filter(data_sachin, Runs>=90 & Runs <=99)
missed_century =rows_sachin_century %>% group_by(Versus) %>% summarise(century=n()) %>% arrange(-century)
missed_century

#number of fifties
data_sachin=filter(odi,Player=='Sachin R Tendulkar')
data_sachin
rows_sachin_fifty=filter(data_sachin, Runs>=50 & Runs <=99)
total_fifty =rows_sachin_fifty %>% group_by(Versus) %>% summarise(fifty=n()) %>% arrange(-fifty)
total_fifty

#plotting ggplot
ggplot(odi,aes(x=Runs,y=ScoreRate)) + geom_point() 


#mutate function
odi = odi %>% mutate(ducks=if_else(Runs==0,1,0))
head(odi)
odi = odi %>% mutate(centuries=if_else(Runs>99,1,0))
odi
odi = odi %>% mutate(missed=if_else(Runs>90 & Runs<100,1,0))
odi
odi = odi %>% mutate(fifties=if_else(Runs>=50,1,0))
odi
View(head(odi))


players_summary = odi %>% group_by(Player) %>% summarise(Total_runs=sum(Runs,na.rm=TRUE),Centuries=sum(centuries,na.rm=TRUE),Ducks=sum(ducks,na.rm=TRUE),fifty=sum(fifties,na.rm=TRUE),missed_century=sum(missed,na.rm=TRUE)
%>% arrange(-Total_runs))
players_summary


#flipkart tweets data set questions

library(dplyr)
twee=read.csv("C://users/Administrator/Desktop//tweets_flipkart.csv")
View(twee)
dim(twee)
typeof(twee)
View(twee)
str(twee)

#1 Identify types of columns(locations,texts,groups etc)

str(twee)
colSums(is.na(twee))
View(twee[is.na(twee$retweets),])
library(dplyr)
avg_retweets=twee %>%summarise(avg_retweets=mean(retweets,na.rm='T'))
avg_retweets
twee[twee$retweets == 0.4050279,]
twee$retweets
is.na(twee$retweets)


#2 Identify top ten users based on number of tweets

top_ten = twee %>% group_by(user) %>% summarise(total_tweets = length(text)) %>% arrange(-total_tweets)
top_ten

#3 What is the percentage of tweets from each device (use source column to know about dev
pct_tweets =twee %>% group_by(source) %>% summarise(no_of_twts=n()) %>% arrange(-no_of_twts)
pct_tweets
twt_pct =(no_of_twts * 100/nrow(twee) %>% arrange(-twt_pct))
twt_pct



#4 which date has the maximum number of tweets

max_tweet = twee %>% group_by(created_at) %>% summarise(maximum = length(text)) %>% arrange(-maximum)
max_tweet

#5 compute number of characters in each tweets.create a new column for the same.how many tweets are having less than 50 characters

twee$text = as.character(twee$text)
twee = twee %>% mutate(characters=nchar(text))
View(twee)
twee=filter(twee,characters<=50)
twee


#USING CRICKET DATA
#1 convert matchdate column to date format(use as.datefunction,learn how to pass the date format from the documentation)
#2 create additional columns to the data 
#a. year
#b.month
#c. day
#d. day of week

#compute number of centuries by sachin across years

??as.Date



































