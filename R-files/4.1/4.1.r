library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
covid_data <- read_csv("owid-covid-data.csv")

#4.1 Análise de Dados

##4.1-a

#captura dados relativos a continentes do dataset inicial
continent_data <- covid_data[grepl('(OWID_)(AFR|ASI|EUR|NAM|OCE|SAM)', covid_data$iso_code), ]
#captura subset de dados relativos aos casos totais
total_cases_by_continent <- subset(continent_data, select=c("location", "total_cases", "date"))
#remove NAs
result_df <- na.omit(total_cases_by_continent)

#passa nomes dos continentes para Português
result_df$location[result_df$location == "Africa"] <- "África"
result_df$location[result_df$location == "Asia"] <- "Ásia"
result_df$location[result_df$location == "Europe"] <- "Europa"
result_df$location[result_df$location == "North America"] <- "América do Norte"
result_df$location[result_df$location == "Oceania"] <- "Oceânia"
result_df$location[result_df$location == "South America"] <- "América do Sul"

#cria o gráfico
result <- ggplot(result_df, aes(x = date, y = total_cases)) + 
  geom_line(aes(color = location), size = 1,alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_discrete(name = "Continentes") +
  xlab("Tempo") +
  ylab("Nº de infetados") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

#guarda o gráfico num ficheiro
png(filename="4.1.a.png")
plot(result)
dev.off()

##4.1-b

#captura subset de dados relativos aos casos totais por milhão de habitantes (usando os dados capturados na alínea anterior)
total_cases_per_million_by_continent <- subset(continent_data, select=c("location", "total_cases_per_million", "date"))
#remove NAs
result_df <- na.omit(total_cases_per_million_by_continent)

#passa nomes dos continentes para Português
result_df$location[result_df$location == "Africa"] <- "África"
result_df$location[result_df$location == "Asia"] <- "Ásia"
result_df$location[result_df$location == "Europe"] <- "Europa"
result_df$location[result_df$location == "North America"] <- "América do Norte"
result_df$location[result_df$location == "Oceania"] <- "Oceânia"
result_df$location[result_df$location == "South America"] <- "América do Sul"

#cria o gráfico
result <- ggplot(result_df, aes(x = date, y = total_cases_per_million)) + 
  geom_line(aes(color = location), size = 1,alpha = 0.5, position = position_dodge(0.8)) +
  labs(x = "Tempo", y = "Nº de infetados/milhão de habitantes", color = "Continentes\n") +
  theme_minimal() + 
  scale_y_continuous(labels = comma)

#guarda o gráfico num ficheiro
png(filename="4.1.b.png")
plot(result)
dev.off()

##4.1-c

#captura subset de dados relativos ao número de mortes diárias por milhão de habitantes
portugal_daily_deaths_pm <- subset(covid_data, iso_code == "PRT", select=c("new_deaths_per_million"))
uk_daily_deaths_pm <- subset(covid_data, iso_code == "GBR", select=c("new_deaths_per_million"))
spain_daily_deaths_pm <- subset(covid_data, iso_code == "ESP", select=c("new_deaths_per_million"))
italy_daily_deaths_pm <- subset(covid_data, iso_code == "ITA", select=c("new_deaths_per_million"))

#limpar NAs
clean_portugal_ddpm <- na.omit(portugal_daily_deaths_pm)
clean_uk_ddpm <- na.omit(uk_daily_deaths_pm)
clean_spain_ddpm <- na.omit(spain_daily_deaths_pm)
clean_italy_ddpm <- na.omit(italy_daily_deaths_pm)

#obter dados para cálculo de outliers
#obter amplitude interquartil
prt_iqr <- IQR(clean_portugal_ddpm$new_deaths_per_million)
#obter quartis
prt_quartile <- quantile(clean_portugal_ddpm$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
#aplicar fórmula para remover outliers e guardar numa data frame
prt_df <- data.frame(group='Portugal', "new_deaths_per_million"=subset(clean_portugal_ddpm, clean_portugal_ddpm$new_deaths_per_million > prt_quartile[1]-1.5*prt_iqr & clean_portugal_ddpm$new_deaths_per_million < prt_quartile[2]+1.5*prt_iqr))

uk_iqr <- IQR(clean_uk_ddpm$new_deaths_per_million)
uk_quartile <- quantile(clean_uk_ddpm$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
uk_df <- data.frame(group='UK', "new_deaths_per_million"= subset(clean_uk_ddpm, clean_uk_ddpm$new_deaths_per_million > uk_quartile[1]-1.5*uk_iqr & clean_uk_ddpm$new_deaths_per_million < uk_quartile[2]+1.5*uk_iqr))

esp_iqr <- IQR(clean_spain_ddpm$new_deaths_per_million)
esp_quartile <- quantile(clean_spain_ddpm$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
esp_df <- data.frame(group='Espanha', "new_deaths_per_million"=subset(clean_spain_ddpm, clean_spain_ddpm$new_deaths_per_million > esp_quartile[1]-1.5*esp_iqr & clean_spain_ddpm$new_deaths_per_million < esp_quartile[2]+1.5*esp_iqr))

ita_iqr <- IQR(clean_italy_ddpm$new_deaths_per_million)
ita_quartile <- quantile(clean_italy_ddpm$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
ita_df <- data.frame(group='Itália', "new_deaths_per_million"=subset(clean_italy_ddpm, clean_italy_ddpm$new_deaths_per_million > ita_quartile[1]-1.5*ita_iqr & clean_italy_ddpm$new_deaths_per_million < ita_quartile[2]+1.5*ita_iqr))

#junta todas as data frames referentes a cada país
boxplot_data <- rbind(prt_df,uk_df,esp_df,ita_df)

result <- ggplot(boxplot_data, aes(x = group , y = new_deaths_per_million)) +
        geom_boxplot (aes(fill=group)) +
        labs(x = "Países", y = "Nº de mortes/milhão de habitantes") +
        theme(legend.position = "none")

#guarda o gráfico num ficheiro
png(filename="4.1.c.png")
plot(result)
dev.off()

##4.1-d

#captura dados relativos aos países do dataset inicial
albania_data <- data.frame(covid_data[grepl('(ALB)', covid_data$iso_code),])
denmark_data <- data.frame(covid_data[grepl('(DNK)', covid_data$iso_code), ])
germany_data <- data.frame(covid_data[grepl('(DEU)', covid_data$iso_code), ])
russia_data <- data.frame(covid_data[grepl('(RUS)', covid_data$iso_code), ])

#limpar NAs na informação importante
albania_data <- albania_data[!is.na(albania_data$total_deaths_per_million) & !is.na(albania_data$total_tests_per_thousand), ]
denmark_data <- denmark_data[!is.na(denmark_data$total_deaths_per_million) & !is.na(denmark_data$total_tests_per_thousand), ]
germany_data <- germany_data[!is.na(germany_data$total_deaths_per_million) & !is.na(germany_data$total_tests_per_thousand), ]
russia_data <- russia_data[!is.na(russia_data$total_deaths_per_million) & !is.na(russia_data$total_tests_per_thousand), ]

#captura a informação mais recente sobre cada país
albania_latest <- tail(albania_data, n=1)
denmark_latest <- tail(denmark_data, n=1)
germany_latest <- tail(germany_data, n=1)
russia_latest <-  tail(russia_data, n=1)

#junta toda a informação numa única variável
barplot_data <- rbind(albania_latest,denmark_latest,germany_latest,russia_latest)

#remove dados desnecessários e transforma numa tabela reconhecida para o ggplot barplot
barplot_data <- melt(barplot_data[,c('location','total_deaths_per_million','total_tests_per_thousand')],id.vars = 1)

#gera o gráfico
result <- ggplot(barplot_data,aes(x = location,y = value))  + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  scale_fill_discrete(name = "", labels = c("Mortes Totais/\nmilhão de habitantes", "Testes Totais/\nmil habitantes")) +
  xlab("Países") +
  ylab(NULL) + 
  scale_x_discrete(labels=c("Albânia", "Dinamarca", "Alemanha", "Rússia"))

#guarda o gráfico num ficheiro
png(filename="4.1.d.png")
plot(result)
dev.off()

#4.1-e

#captura dados relativos aos países do dataset inicial
capture_data <- covid_data[grepl('(Europe)', covid_data$continent), ]
europe_data <- capture_data %>% select(location, new_cases_per_million, date)

#remove NAs nas colunas afetas ao problema
clean_europe_data <- data.frame(europe_data[!is.na(europe_data$new_cases_per_million), ])

#ordena dados por novos casos por dia por milhão de habitantes
sorted_europe_data <- clean_europe_data[order(-clean_europe_data$new_cases_per_million), ]

#imprime primeiro elemento da lista
print(head(sorted_europe_data, n=1))

#4.1-f

#captura dados relativos aos países do dataset inicial
work_data <- covid_data %>% select(location, reproduction_rate, date)

#remove NAs nas colunas afetas ao problema
clean_data <- data.frame(work_data[!is.na(work_data$reproduction_rate), ])

#ordena dados por taxa de transmissibilidade
sorted_data <- clean_data[order(-clean_data$reproduction_rate), ]

#imprime primeiro elemento da lista
print(head(sorted_data, n=1))

#4.1-g

#captura dados relativos a continentes do dataset inicial
afr_data <- covid_data[grepl('(OWID_)(AFR)', covid_data$iso_code), ]
asi_data <- covid_data[grepl('(OWID_)(ASI)', covid_data$iso_code), ]
eur_data <- covid_data[grepl('(OWID_)(EUR)', covid_data$iso_code), ]
nam_data <- covid_data[grepl('(OWID_)(NAM)', covid_data$iso_code), ]
oce_data <- covid_data[grepl('(OWID_)(OCE)', covid_data$iso_code), ]
sam_data <- covid_data[grepl('(OWID_)(SAM)', covid_data$iso_code), ]

#captura subset de dados relativos aos casos totais
new_cases_by_afr <- subset(afr_data, select=c("new_deaths_per_million"))
new_cases_by_asi <- subset(asi_data, select=c("new_deaths_per_million"))
new_cases_by_eur <- subset(eur_data, select=c("new_deaths_per_million"))
new_cases_by_nam <- subset(nam_data, select=c("new_deaths_per_million"))
new_cases_by_oce <- subset(oce_data, select=c("new_deaths_per_million"))
new_cases_by_sam <- subset(sam_data, select=c("new_deaths_per_million"))

#remove NAs
new_cases_by_afr <- na.omit(new_cases_by_afr)
new_cases_by_asi <- na.omit(new_cases_by_asi)
new_cases_by_eur <- na.omit(new_cases_by_eur)
new_cases_by_nam <- na.omit(new_cases_by_nam)
new_cases_by_oce <- na.omit(new_cases_by_oce)
new_cases_by_sam <- na.omit(new_cases_by_sam)

#obter dados para cálculo de outliers
#obter amplitude interquartil
afr_iqr <- IQR(new_cases_by_afr$new_deaths_per_million)
#obter quartis
afr_quartile <- quantile(new_cases_by_afr$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
#aplicar fórmula para remover outliers e guardar numa darame
afr_df <- data.frame(group='África', "new_deaths_per_million"=subset(new_cases_by_afr, new_cases_by_afr$new_deaths_per_million > afr_quartile[1]-1.5*afr_iqr & new_cases_by_afr$new_deaths_per_million < afr_quartile[2]+1.5*afr_iqr))

asi_iqr <- IQR(new_cases_by_asi$new_deaths_per_million)
asi_quartile <- quantile(new_cases_by_asi$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
asi_df <- data.frame(group='Ásia', "new_deaths_per_million"=subset(new_cases_by_asi, new_cases_by_asi$new_deaths_per_million > asi_quartile[1]-1.5*asi_iqr & new_cases_by_asi$new_deaths_per_million < asi_quartile[2]+1.5*asi_iqr))

eur_iqr <- IQR(new_cases_by_eur$new_deaths_per_million)
eur_quartile <- quantile(new_cases_by_eur$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
eur_df <- data.frame(group='Europa', "new_deaths_per_million"=subset(new_cases_by_eur, new_cases_by_eur$new_deaths_per_million > eur_quartile[1]-1.5*eur_iqr & new_cases_by_eur$new_deaths_per_million < eur_quartile[2]+1.5*eur_iqr))

nam_iqr <- IQR(new_cases_by_nam$new_deaths_per_million)
nam_quartile <- quantile(new_cases_by_nam$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
nam_df <- data.frame(group='América\ndo Norte', "new_deaths_per_million"=subset(new_cases_by_nam, new_cases_by_nam$new_deaths_per_million > nam_quartile[1]-1.5*nam_iqr & new_cases_by_nam$new_deaths_per_million < nam_quartile[2]+1.5*nam_iqr))

oce_iqr <- IQR(new_cases_by_oce$new_deaths_per_million)
oce_quartile <- quantile(new_cases_by_oce$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
oce_df <- data.frame(group='Oceânia', "new_deaths_per_million"=subset(new_cases_by_oce, new_cases_by_oce$new_deaths_per_million > oce_quartile[1]-1.5*oce_iqr & new_cases_by_oce$new_deaths_per_million < oce_quartile[2]+1.5*oce_iqr))

sam_iqr <- IQR(new_cases_by_sam$new_deaths_per_million)
sam_quartile <- quantile(new_cases_by_sam$new_deaths_per_million, probs=c(.25, .75), na.rm = TRUE)
sam_df <- data.frame(group='América\ndo Sul', "new_deaths_per_million"=subset(new_cases_by_sam, new_cases_by_sam$new_deaths_per_million > sam_quartile[1]-1.5*sam_iqr & new_cases_by_sam$new_deaths_per_million < sam_quartile[2]+1.5*sam_iqr))

#junta todas as data frames referentes a cada país
boxplot_data <- rbind(afr_df,asi_df,eur_df,nam_df,oce_df,sam_df)

result <- ggplot(boxplot_data, aes(x = group , y = new_deaths_per_million)) +
      geom_boxplot (aes(fill=group),position=position_dodge(width=1.2)) +
      xlab("Continentes") +
      ylab("Mortes Diárias/milhão de habitantes") + 
      theme(legend.position = "none")

#guarda o gráfico num ficheiro
png(filename="4.1.g.png")
plot(result)
dev.off()
