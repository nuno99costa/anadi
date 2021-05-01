library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(nortest)
library(ggpubr)
library(car)
library(PMCMRplus)
covid_data <- read_csv("../owid-covid-data.csv")


###4.3 Correlação

#lista de datas possíveis
date_list <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")

##4.3.a

#obter dados relativos ao problema (e remover NAs)
europe <- subset(covid_data, 
                 covid_data$continent == 'Europe' & 
                 covid_data$population>10000000 & 
                 date %in% date_list, 
                 select=c(location, reproduction_rate, population_density))
clean_europe <- na.omit(europe)

#filtrar dados de modo a obter a máxima taxa de transmissibilidade por país
highest_rp_pd <- group_by(clean_europe, location) %>% 
  filter(reproduction_rate == max(reproduction_rate)) %>% 
  filter(1:n() == 1)

#testamos a normalidade das amostras (Shapiro)
plot_pd <- ggqqplot(highest_rp_pd$population_density)
png(filename="4.3.a_qqplot_pd.png")
plot(plot_pd)
dev.off()

plot_rd <- ggqqplot(highest_rp_pd$reproduction_rate)
png(filename="4.3.a_qqplot_rd.png")
plot(plot_rd)
dev.off()

shapiro.test(highest_rp_pd$reproduction_rate)
shapiro.test(highest_rp_pd$population_density)

#como a amostra em análise é pequena e uma das variavéis não é normalmente distríbuida, usamos test de Kendall
cor(c(highest_rp_pd$population_density) ,c(highest_rp_pd$reproduction_rate) , method ="kendall")

#coeficiente = -0.08
#assim, assumimos não existe nenhuma correlação significativa entre as duas variáveis

##4.3.b

#obter dados
total_deaths_per_million_per_country <- subset(covid_data, covid_data$continent == 'Europe' & covid_data$population>10000000 & date %in% date_list, select=c(location,total_deaths_per_million, aged_65_older))

#filtrar dados de modo a obter os máximos por país
highest_td_a65 <- group_by(total_deaths_per_million_per_country, location) %>% 
  filter(total_deaths_per_million == max(total_deaths_per_million)) %>% 
  filter(1:n() == 1)

#verificar 
shapiro.test(highest_td_a65$total_deaths_per_million)
plot_tdpm <- ggqqplot(highest_td_a65$total_deaths_per_million)
png(filename="4.3.a_qqplot_total_deaths_per_million.png")
plot(plot_tdpm)
dev.off()

shapiro.test(highest_td_a65$aged_65_older)
plot_age65 <- ggqqplot(highest_td_a65$aged_65_older)
png(filename="4.3.b_qqplot_population_density.png")
plot(plot_age65)
dev.off()

#dados seguem distribuição normal
#

#teste de correlação
cor.test(c(highest_td_a65$total_deaths_per_million) ,c(highest_td_a65$aged_65_older), method ="pearson")

ggscatter(highest_td_a65, x = "aged_65_older", y = "total_deaths_per_million", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
