
####Packages #####
library(dplyr)
library(tidyverse)
library(lubridate)
library(corrplot)

install.packages('tidyverse')

#####Step 1: Download and upload the Fifa23 dataset####
Fifa23_ds= read.csv("C:\\Users\\tiver\\OneDrive\\Documents\\Portfolio project\\Fifa23 data.csv")
Fifa_org=read.csv("C:\\Users\\tiver\\OneDrive\\Documents\\Portfolio project\\Fifa23 data.csv")

str(Fifa23_ds)
str(Fifa_org)
###Step 2: Transformation of the Height and Weight column ####

##removal of cm and kg##
Fifa23_ds$Height= gsub('cm' , "",Fifa23_ds$Height)
Fifa23_ds$Weight = gsub('kg' , "",Fifa23_ds$Weight)


##Tranformation of height and weight column to numeric
Fifa23_ds$Height =as.numeric(Fifa23_ds$Height)
Fifa23_ds$Weight =as.numeric(Fifa23_ds$Weight)

#####Step 3 :seperate joined columns into Year Month and Day#######

Fifa23_ds$Joined =as.Date(Fifa23_ds$Joined, format = "%b %d,%Y")
?as.Date
library(lubridate)
Fifa23_ds$year <- year(ymd(Fifa23_ds$Joined))
Fifa23_ds$month <- month(ymd(Fifa23_ds$Joined)) 
Fifa23_ds$day <- day(ymd(Fifa23_ds$Joined))


###Step 4: Transform the value, wage, and release clause columns into columns of integers.
 ####

##removal of K,€, and M## AND the transdformation of columns into integers
Fifa23_ds$Wage= gsub('K' , "",Fifa23_ds$Wage)
Fifa23_ds$Wage= gsub('€' , "",Fifa23_ds$Wage)
Fifa23_ds$Wage =as.integer(Fifa23_ds$Wage)

Fifa23_ds$Value= gsub('M' , "",Fifa23_ds$Value)
Fifa23_ds$Value= gsub('€' , "",Fifa23_ds$Value)
Fifa23_ds$Value =as.numeric(Fifa23_ds$Value)

Fifa23_ds$Release.Clause  = gsub('M' , "",Fifa23_ds$Release.Clause)
Fifa23_ds$Release.Clause  = gsub('€' , "",Fifa23_ds$Release.Clause)
Fifa23_ds$Release.Clause  =as.numeric(Fifa23_ds$Release.Clause)

distinct(Fifa23_ds$Value)
####remove Na from data###
index=is.na(Fifa23_ds$Value)
Fifa23_ds$Value[index] = 0

index=is.na(Fifa23_ds$Release.Clause)
Fifa23_ds$Release.Clause[index] = 0



######Dplyr package########

###Step 5: Determine the highest-paid players for each country.
####
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





x= c(600,500,550,559,650,670,700,575,520,650)
median(x)
