library(plyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(tidyverse)
library(magrittr)

#1

setwd("C:\\Users\\draks\\Desktop\\lab")

#read data
#na.strings = "N/A" in this dataset
data <- read.csv(file = "data.csv",na.strings = "N/A")
data <- as_tibble(data)
# 2-a) select is used
data_selected <- data %>% select(CountryID,Country.Name,World.Rank,X2019.Score,Tax.Burden...of.GDP,Population..Millions.,GDP..Billions..PPP.,Unemployment....,Inflation....,Region)

#change columns to lowercase
colnames(data_selected) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(data_selected)

#a) Check NA
print("The number of NA in the dataframe")
print(sum(is.na(data_selected)))


#b) Begin replacing or removing NA values
#There are two necessary columns that a country should NOT have as NA that have NA values in some rows
#world.rank x2019.score 
#As they are necessary, if a country has even one of them as a NA i decided to drop the country as it would not be of use 
#The reason is, we can't take avarage for world.rank and thus score
data_selected <- subset(data_selected, !is.na(world.rank))
data_selected <- subset(data_selected, !is.na(x2019.score))

#lets check if na omit is successful by checking the means of numerical data, if it was not successful it would return NA
mean(data_selected$world.rank)
mean(data_selected$x2019.score)

#Other columns can have NA values in them.
#However it is not necessary to delete them, for the sake of calculation, I will replace them with the mean of the column. 
#However as we can see we can not get the mean of columns that have NA values with mean() funcion, so we add another parameters to ignore the NA values while getting a mean
#Change the NA values with mean(ignoring the NA)
#Check if it is successful using mean() without parameters

#tax.burden...of.gdp
mean_to_replace <- mean(as.numeric(data_selected$tax.burden...of.gdp),na.rm=TRUE)
data_selected$tax.burden...of.gdp[is.na(data_selected$tax.burden...of.gdp)] <- mean_to_replace

#unemployment....
mean_to_replace <- mean(as.numeric(data_selected$unemployment....),na.rm=TRUE)
mean_to_replace <- round(mean_to_replace,2)
data_selected$unemployment....[is.na(data_selected$unemployment....)] <- mean_to_replace

#inflation....
mean_to_replace <- mean(as.numeric(data_selected$inflation....),na.rm=TRUE)
data_selected$inflation....[is.na(data_selected$inflation....)] <- mean_to_replace


#Lets check if there is any NA left in the entire data
#FALSE means no NA in any of the rows
apply(data_selected, 2, function(x) any(is.na(x)))

#a) Check NA again
print("The number of NA in the dataframe")
print(sum(is.na(data_selected)))

#2
#a) Select was used previously
#data_selected <- data %>% select(CountryID,Country.Name,World.Rank,X2019.Score,Tax.Burden...of.GDP,Population..Millions.,GDP..Billions..PPP.,Unemployment....,Inflation....)

#b)
#I prefer descending order
arrange(data_selected, data_selected$world.rank)

#c)
#mutate into number of unemployed citizens using ((population in millions * 10 ^ 6) / 100 * unemployment)
data_selected <- mutate(data_selected, unemployed.citizens = (as.numeric(data_selected$population..millions.) * 1000000) / 100 * as.numeric(data_selected$unemployment....) )

# d-e-f)
#detaching plyr to avoid errors with group_by function
detach(package:plyr)
data_selected %>% 
  group_by(data_selected$region) %>%
  summarise(Inflation_Mean=mean(inflation....), Unemployed_Sum =sum(unemployed.citizens)) %>%  
  top_n(3,Inflation_Mean)

#4
p <- ggplot(data_selected, aes(x =countryid , y = world.rank)) 
p+geom_point(aes(color = x2019.score))

p <- ggplot(data_selected, aes(x =countryid , y = inflation....)) 
p+geom_point(aes(color = world.rank))

p <- ggplot(data_selected, aes(x =countryid , y = unemployed.citizens)) 
p+geom_point(aes(color = region))






