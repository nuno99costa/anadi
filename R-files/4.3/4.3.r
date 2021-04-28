library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(nortest)

covid_data <- read_csv("../owid-covid-data.csv")


#4.3 Correlação

#lista de datas possíveis
date_list <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")

#4.3.a

#obter dados relativos ao problema (e remover NAs)
europe <- subset(covid_data, covid_data$continent == 'Europe' & covid_data$population>10000000 & date %in% date_list, select=c(location, reproduction_rate, population_density))
clean_europe <- na.omit(europe)

#filtrar dados de modo a obter a máxima taxa de transmissibilidade por país
countries_highest_rate <- group_by(clean_europe, location) %>% 
  filter(reproduction_rate == max(reproduction_rate)) %>% 
  filter(1:n() == 1)

## devemos remover outliers??

#shapiro ou anderson darling
ggqqplot(countries_highest_rate$reproduction_rate)
ggqqplot(countries_highest_rate$population_density)
shapiro.test(countries_highest_rate$reproduction_rate)
shapiro.test(countries_highest_rate$population_density)
ad.test(countries_highest_rate$reproduction_rate)
ad.test(countries_highest_rate$population_density)

cor.test (c(countries_highest_rate$population_density) ,c(countries_highest_rate$reproduction_rate) , alternative ="two.sided", method ="pearson")

#4.3.b

#obter dados
total_deaths_per_million_per_country <- subset(covid_data, covid_data$continent == 'Europe' & covid_data$population>10000000 & date %in% date_list, select=c(location,total_deaths_per_million, aged_65_older))

#filtrar dados de modo a obter os máximos por país
countries_highest_rate <- group_by(total_deaths_per_million_per_country, location) %>% 
  filter(total_deaths_per_million == max(total_deaths_per_million)) %>% 
  filter(1:n() == 1)

#shapiro ou anderson darling
ggqqplot(countries_highest_rate$total_deaths_per_million)
ggqqplot(countries_highest_rate$aged_65_older)
shapiro.test(countries_highest_rate$total_deaths_per_million)
shapiro.test(countries_highest_rate$aged_65_older)

#dados são normais
#teste de correlação
cor.test (c(countries_highest_rate$total_deaths_per_million) ,c(countries_highest_rate$aged_65_older), method ="pearson")
