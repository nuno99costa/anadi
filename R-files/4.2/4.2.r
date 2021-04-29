library(readr)
library(ggpubr)
library(car)
covid_data <- read_csv("../owid-covid-data.csv")

file<-file("result_4.2.txt", "w")

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

#verificamos a normalidade através de análise gráfica
qqplot_prt <- ggqqplot(sample_prt$reproduction_rate, ylab = "Taxa de Transmissibilidade", xlabel= "Teorético")
#guarda o gráfico num ficheiro
png(filename="4.2.a_portugal_qqplot.png")
plot(qqplot_prt)
dev.off()

qqplot_uk <- ggqqplot(sample_uk$reproduction_rate, ylab = "Taxa de Transmissibilidade", xlabel= "Teorético")
png(filename="4.2.a_reino_unido_qqplot.png")
plot(qqplot_uk)
dev.off()

#verificamos a normalidade da distribuição dos dados (P-value do teste de Shapiro)
shap_prt <- shapiro.test(sample_prt$reproduction_rate)
shap_uk <- shapiro.test(sample_uk$reproduction_rate)
write(c("Shapiro p-values","Portugal",shap_prt$p.value,"Reino Unido", shap_uk$p.value), file, append=TRUE)

#Análise gráfica + Shapiro P-values + tamanho da amostra (>=30) + dados emparelhados -> usamos T-test emparelhado
result <- t.test(sample_uk$reproduction_rate,sample_prt$reproduction_rate, paired = TRUE, alternative = "greater")

#as médias são semelhantes entre os países
write(c("T-test p-value", result$p.value,"De acordo com o p-value (>0.05), aceitamos a hipótese nula e inferimos que média do Reino Unido não é significativamente superior à média de Portugal"), file, append=TRUE)

##4.2.b
write("\n4.2.b", file, append=TRUE)

#definir seed
set.seed(115)

#obter lista de dias a usar
sample_dates <- sample(date_list, 15)

#obter taxa de transmissibilidade, para os países selecionados, para as datas definidas
all_samples <- subset(covid_data, date %in% sample_dates & grepl('(ESP|ITA|FRA|PRT)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

#verificamos a normalidade através de análise gráfica
qqplot_prt <- ggqqplot(subset(all_samples, all_samples$location == "Portugal")$new_deaths_per_million, 
		 ylab = "Mortes Diárias/Milhão de habitantes", 
		 xlabel= "Teorético")
#guarda o gráfico num ficheiro
png(filename="4.2.b_portugal_qqplot.png")
plot(qqplot_prt)
dev.off()

qqplot_esp <- ggqqplot(subset(all_samples, all_samples$location == "Spain")$new_deaths_per_million, 
		 ylab = "Mortes Diárias/Milhão de habitantes", 
		 xlabel= "Teorético")
png(filename="4.2.b_espanha_qqplot.png")
plot(qqplot_esp)
dev.off()

qqplot_ita <- ggqqplot(subset(all_samples, all_samples$location == "Italy")$new_deaths_per_million, 
		 ylab = "Mortes Diárias/Milhão de habitantes", 
		 xlabel= "Teorético")
png(filename="4.2.b_italia_qqplot.png")
plot(qqplot_ita)
dev.off()

qqplot_fra <- ggqqplot(subset(all_samples, all_samples$location == "France")$new_deaths_per_million, 
		 ylab = "Mortes Diárias/Milhão de habitantes", 
		 xlabel= "Teorético")
png(filename="4.2.b_frança_qqplot.png")
plot(qqplot_fra)
dev.off()

#Testamos normalidade dos dados
shap_prt <- shapiro.test(subset(all_samples, all_samples$location == "Portugal")$new_deaths_per_million)
shap_esp <- shapiro.test(subset(all_samples, all_samples$location == "Spain")$new_deaths_per_million)
shap_ita <- shapiro.test(subset(all_samples, all_samples$location == "Italy")$new_deaths_per_million)
shap_fra <- shapiro.test(subset(all_samples, all_samples$location == "France")$new_deaths_per_million)

write(c("Shapiro p-values","Portugal: ", shap_prt$p.value,
		"Espanha: ", shap_esp$p.value,
		"Itália: ", shap_ita$p.value,
		"França: ", shap_fra$p.value), file, append=TRUE)

#Dados não são normalmente distribuidos e são emparelhados
#Usamos o teste de Friedman
friedman <- friedmanTest(y=all_samples$new_deaths_per_million, groups=all_samples$location, blocks=all_samples$date)

write("Distribuição dos dados não é normal", file, append=TRUE)
write(c("\nTeste de Friedman p-value", friedman$p.value, "De acordo com o p-value obtido, rejeitamos a hipótese nula e inferimos que existe diferença significativa em no mínimo duas das amostras estudadas (p-value < 0.05)"), file, append=TRUE)

#Análise Post-hoc
boxplot <- ggplot(all_samples, aes(x=location, y=new_deaths_per_million)) + 
    geom_boxplot()
png(filename="4.2.b_boxplot.png")
plot(boxplot)
dev.off()

#usamos o teste de Nemenyi (https://www.worldcat.org/title/distribution-free-multiple-comparisons/oclc/39810544)

frdManyOneNemenyiTest(y=all_samples$new_deaths_per_million, groups=all_samples$location, blocks=all_samples$date)


##4.2.c
write("\n4.2.c", file, append=TRUE)

#definir seed e obter lista de dias a usar
set.seed(100)
sample_dates <- sample(date_list, 30)

#obter taxa de transmissibilidade, para os países selecionados, para as datas definidas
afr_samples <- subset(covid_data, date %in% sample_dates & grepl('(OWID_AFR)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(101)
sample_dates <- sample(date_list, 30)
asi_samples <- subset(covid_data, date %in% sample_dates & grepl('(OWID_ASI)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(102)
sample_dates <- sample(date_list, 30)
eur_samples <- subset(covid_data, date %in% sample_dates & grepl('(OWID_EUR)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(103)
sample_dates <- sample(date_list, 30)
nam_samples <- subset(covid_data, date %in% sample_dates & grepl('(OWID_NAM)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

set.seed(104)
sample_dates <- sample(date_list, 30)
sam_samples <- subset(covid_data, date %in% sample_dates & grepl('(OWID_SAM)', covid_data$iso_code), select=c(location,date,new_deaths_per_million))

#testar normalidade dos dados
ggqqplot(afr_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_afr <- shapiro.test(afr_samples$new_deaths_per_million)
ggqqplot(asi_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_asi <- shapiro.test(asi_samples$new_deaths_per_million)
ggqqplot(eur_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_eur <- shapiro.test(eur_samples$new_deaths_per_million)
ggqqplot(nam_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_nam <- shapiro.test(nam_samples$new_deaths_per_million)
ggqqplot(sam_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_sam <- shapiro.test(sam_samples$new_deaths_per_million)

all_samples <- rbind(afr_samples, asi_samples, eur_samples, nam_samples, sam_samples)
#testes de Levene (para verificar a variancia)
leveneTest(new_deaths_per_million ~ location, data=all_samples)

mean(nam_samples$new_deaths_per_million)
mean(asi_samples$new_deaths_per_million)
mean(afr_samples$new_deaths_per_million)
mean(eur_samples$new_deaths_per_million)
mean(sam_samples$new_deaths_per_million)

write(c("Shapiro p-values","África: ", shap_afr$p.value,"Ásia: ", shap_asi$p.value,"Europa: ", shap_eur$p.value,"América do Norte: ", shap_nam$p.value,"América do Sul: ", shap_sam$p.value,"Apesar dos grupos de dados não serem todos normalmente distribuidos, a amostra é grande (>=30) logo usamos ..."), file, append=TRUE)

#Usamos ...
all_samples <- rbind(afr_samples,asi_samples,eur_samples,nam_samples,sam_samples)
kruskal.test(new_deaths_per_million ~ location , data=all_samples)
oneway.test(new_deaths_per_million ~ location , data=all_samples)

#análise post hoc
t.test(afr_samples$new_deaths_per_million, asi_samples$new_deaths_per_million)
t.test(afr_samples$new_deaths_per_million, eur_samples$new_deaths_per_million)
t.test(afr_samples$new_deaths_per_million, nam_samples$new_deaths_per_million)
t.test(afr_samples$new_deaths_per_million, sam_samples$new_deaths_per_million)

t.test(asi_samples$new_deaths_per_million, eur_samples$new_deaths_per_million)
t.test(asi_samples$new_deaths_per_million, nam_samples$new_deaths_per_million)
t.test(asi_samples$new_deaths_per_million, sam_samples$new_deaths_per_million)

t.test(eur_samples$new_deaths_per_million, nam_samples$new_deaths_per_million)
t.test(eur_samples$new_deaths_per_million, sam_samples$new_deaths_per_million)

t.test(nam_samples$new_deaths_per_million, sam_samples$new_deaths_per_million)

# médias semelhantes
# Europa - América do Sul
# África - Ásia