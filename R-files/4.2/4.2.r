library(readr)
covid_data <- read_csv("../owid-covid-data.csv")

file<-file("result_4.2.a.txt", "w")

#4.2 Inferência Estatística

#lista de datas possíveis
date_list <- seq(as.Date("2020-04-01"), as.Date("2021-02-27"), by = "day")

#4.2.a

write("4.2.a", file)
#definir seed
set.seed(118)

#obter lista de dias a usar
sample_data <- sample(date_list, 30)

#obter taxa de transmissibilidade, por país, para as datas definidas
sample_prt <- subset(covid_data, date %in% sample_data & iso_code == "PRT", select=c(location, reproduction_rate))
sample_uk <- subset(covid_data, date %in% sample_data & iso_code == "GBR", select=c(location, reproduction_rate))

#verificamos a normalidade da distribuição dos dados (P-value)
shap_prt <- shapiro.test(sample_prt$reproduction_rate)
shap_uk <- shapiro.test(sample_uk$reproduction_rate)
write(c("Shapiro p-values","Portugal",shap_prt$p.value,"Reino Unido", shap_uk$p.value), file, append=TRUE)


##########################
##  FALTA FTEST????    ##
########################

#Shapiro P-values + tamanho da amostra (<=30) -> usamos T-test
#p-value = 0.1904 > 0.05
result <- t.test(sample_uk$reproduction_rate,sample_prt$reproduction_rate, alternative = "greater")

#as médias são semelhantes entre os países
write(c("T-test p-value", result$p.value,"Estimativa de média (Portugal, Reino Unido)",result$estimate,"A média do Reino Unido não é significativamente superior à média de Portugal"), file, append=TRUE)

#4.2.b

write("\n4.2.b", file, append=TRUE)
#definir seed
set.seed(115)

#obter lista de dias a usar
sample_dates <- sample(date_list, 15)

#obter taxa de transmissibilidade, para os países selecionados, para as datas definidas
all_samples <- subset(covid_data, date %in% sample_dates & grepl('(ESP|ITA|FRA|PRT)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

#testar normalidade dos dados
shap_prt <- shapiro.test(subset(all_samples, all_samples$location == "Portugal")$new_deaths_per_million)
shap_esp <- shapiro.test(subset(all_samples, all_samples$location == "Spain")$new_deaths_per_million)
shap_ita <- shapiro.test(subset(all_samples, all_samples$location == "Italy")$new_deaths_per_million)
shap_fra <- shapiro.test(subset(all_samples, all_samples$location == "France")$new_deaths_per_million)


write(c("Shapiro p-values","Portugal: ", shap_prt$p.value,"Espanha: ", shap_esp$p.value,"Itália: ", shap_ita$p.value,"França: ", shap_fra$p.value), file, append=TRUE)

#Dados não são normais
#Usamos o teste de Kruskal-Wallis

kruskal <- kruskal.test(new_deaths_per_million~location, data=all_samples)
kruskal
#Kruskal p-value= 0.5104
write("Distribuição dos dados não é normal", file, append=TRUE)
write(c("\nTeste de Kruskal p-value", kruskal$p.value, "De acordo com o p-value obtido, concluimos que não existe diferença significativa na variável em estudo (p-value > 0.05)"), file, append=TRUE)

#4.2.b

write("\n4.2.c", file, append=TRUE)

#definir seed e obter lista de dias a usar
set.seed(100)
sample_dates <- sample(date_list, 30)

#obter taxa de transmissibilidade, para os países selecionados, para as datas definidas
afr_samples <- subset(covid_data, grepl('(OWID_AFR)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(101)
sample_dates <- sample(date_list, 30)
asi_samples <- subset(covid_data, date %in% sample_dates & grepl('(OWID_ASI)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(102)
sample_dates <- sample(date_list, 30)
eur_samples <- subset(covid_data, grepl('(OWID_EUR)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(103)
sample_dates <- sample(date_list, 30)
nam_samples <- subset(covid_data, grepl('(OWID_NAM)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(104)
sample_dates <- sample(date_list, 30)
sam_samples <- subset(covid_data, grepl('(OWID_SAM)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

#testar normalidade dos dados
shap_afr <- shapiro.test(afr_samples$new_deaths_per_million)
shap_asi <- shapiro.test(asi_samples$new_deaths_per_million)
shap_eur <- shapiro.test(eur_samples$new_deaths_per_million)
shap_nam <- shapiro.test(nam_samples$new_deaths_per_million)
shap_sam <- shapiro.test(sam_samples$new_deaths_per_million)


write(c("Shapiro p-values","África: ", shap_afr$p.value,"Ásia: ", shap_asi$p.value,"Europa: ", shap_eur$p.value,"América do Norte: ", shap_nam$p.value,"América do Sul: ", shap_sam$p.value,"Apesar dos grupos de dados não serem todos normalmente distribuidos, a amostra é grande (>=30) logo usamos ANOVA"), file, append=TRUE)

#Usamos ANOVA
result <- t.test(sample_uk$reproduction_rate,sample_prt$reproduction_rate, alternative = "greater")

#as médias são semelhantes entre os países
write(c("T-test p-value", result$p.value,"Estimativa de média (Portugal, Reino Unido)",result$estimate,"A média do Reino Unido não é significativamente superior à média de Portugal"), file, append=TRUE)