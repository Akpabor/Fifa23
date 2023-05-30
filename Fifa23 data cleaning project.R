
####Packages #####
library(dplyr)
library(tidyverse)
library(lubridate)
library(corrplot)


Fifa23_ds= read.csv("C:\\Users\\tiver\\OneDrive\\Documents\\Portfolio project\\Fifa23 data.csv")

str(Fifa23_ds)

###Transformation into correct format####

Fifa23_ds$Height= gsub('cm' , "",Fifa23_ds$Height)
Fifa23_ds$Weight = gsub('kg' , "",Fifa23_ds$Weight)



Fifa23_ds$Height =as.numeric(Fifa23_ds$Height)
Fifa23_ds$Weight =as.numeric(Fifa23_ds$Weight)

Fifa23_ds$Wage= gsub('K' , "",Fifa23_ds$Wage)
Fifa23_ds$Wage= gsub('€' , "",Fifa23_ds$Wage)
Fifa23_ds$Wage =as.integer(Fifa23_ds$Wage)

Fifa23_ds$Value= gsub('M' , "",Fifa23_ds$Value)
Fifa23_ds$Value= gsub('€' , "",Fifa23_ds$Value)
Fifa23_ds$Value =as.integer(Fifa23_ds$Value)

####remove Na from data
index=is.na(Fifa23_ds$Value)
Fifa23_ds$Value[index] = 0

#####seperate joined columns into Year Month and Day#######

Fifa23_ds$Joined =as.Date(Fifa23_ds$Joined, format = "%b %d,%Y")

library(lubridate)
Fifa23_ds$year <- year(ymd(Fifa23_ds$Joined))
Fifa23_ds$month <- month(ymd(Fifa23_ds$Joined)) 
Fifa23_ds$day <- day(ymd(Fifa23_ds$Joined))




######Dplyr package########

###only show players that make 6 figures or more a week#####

six_figure_players=Fifa23_ds %>%
  filter(Wage>= 100)

####show highest paid player by country###
six_figure_players=Fifa23_ds %>%
  group_by(Nationality)%>%
  summarise(Highest_paid_countries = max(Wage))








######Regression analysis#######

Fifa23_ds%>%
  select( Height ,  Weight,  Wage  , Overall, Potential,Skill.Moves,International_rep=International.Reputation, Value)

####Selects the columns of interest###



cor(Fifa23_ds%>%
      select( Height ,  Weight,   Wage  , Overall, Potential,Skill.Moves,International.Reputation , Value))   

corrplot(cor(Fifa23_ds%>%
               select( Height ,  Weight,  Wage  , Overall, Potential,Skill.Moves,International.Reputation, Value)), method = "circle") 
