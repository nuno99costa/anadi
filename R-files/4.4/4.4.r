library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(reshape2)
library(nortest)
library(car)
covid_data <- read_csv("../owid-covid-data.csv")

###4.4 Regressão

#lista de datas possíveis
date_list <- seq(as.Date("2020-04-01"), as.Date("2021-02-27"), by = "day")


##4.4.a

#obter dados relativos ao problema (e remover NAs)
portugal <- subset(covid_data, covid_data$iso_code == "PRT" & 
                   date %in% date_list, 
                   select = c(stringency_index,
                              new_deaths_per_million,
                              new_cases_per_million,
                              reproduction_rate,
                              date))
portugal<- na.omit(portugal)

#obter ano e mês
portugal$Month <- months(portugal$date)
portugal$Year <- format(portugal$date,format="%y")

#calcular médias
monthly_reproduction_rate <- aggregate( reproduction_rate ~ Month + Year , portugal , mean )
monthly_stringency_index <- aggregate( stringency_index ~ Month + Year , portugal , mean )
monthly_daily_cases <- aggregate( new_deaths_per_million ~ Month + Year , portugal , mean )
monthly_daily_deaths <- aggregate( new_cases_per_million ~ Month + Year , portugal , mean )

portugal <- subset(monthly_reproduction_rate, select=c(Month, reproduction_rate))
portugal <- merge(portugal, monthly_stringency_index[, c("Month", "stringency_index")], by="Month")
portugal <- merge(portugal, monthly_daily_cases[, c("Month", "new_deaths_per_million")], by="Month")
portugal <- merge(portugal, monthly_daily_deaths[, c("Month", "new_cases_per_million")], by="Month")

reg <- lm(portugal$stringency_index ~ portugal$new_deaths_per_million + portugal$new_cases_per_million + portugal$reproduction_rate)
summary(reg)
residual_shapiro <- shapiro.test(residuals(reg))

par(mfrow=c(1,2))
png(filename="4.4.a_residuals_fitted.png")
plot(reg, which = 1)
dev.off()
png(filename="4.4.a_normalqq.png")
plot(reg, which = 2)
dev.off()
png(filename="4.4.a_scale_location.png")
plot(reg, which = 3)
dev.off()
png(filename="4.4.a_residuals_leverage.png")
plot(reg, which = 5)
dev.off()

#4.4.b

#Multicolinariedade
vif <- vif(reg)
#como os VIF para todas as variáveis independentes são superiores a 3, existe multicolinariedade

#Homocedasticidade
#como a amostra é pequena (<30) e os resíduos não são distribuidos normalmente, não usamos o teste de Breusch-Pagan
png(filename="4.4.b_homocedasticidade.png")
plot(fitted(reg),residuals(reg),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)
dev.off()

#após análise gráfica, verificamos que os dados são aleatórios, logo a homocedasticidade é cumprida

#Autocorrelação
dbtest <- durbinWatsonTest(reg)
#de acordo com o p-value do durbin watson teste (>0.05), os dados são independentes.

##4.4.c
portugal <- data.frame(new_deaths_per_million=c(10), new_cases_per_million=c(460), reproduction_rate=c(1.1))

prediction <- predict(reg, portugal)

print(prediction)

#para os dados inseridos no cálculo da previsão, é esperado um valor para o IR de 74.43