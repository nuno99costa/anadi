library(readr)
library(ggpubr)
library(car)
library(scales)
library(reshape2)
library(PMCMRplus)

covid_data <- read_csv("../owid-covid-data.csv")

file<-file("4.2.txt", "w")

###4.2 Inferência Estatística

#lista de datas possíveis
date_list <- seq(as.Date("2020-04-01"), as.Date("2021-02-27"), by = "day")

##4.2.a
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
write(c("Paired T-test p-value", result$p.value,"De acordo com o p-value (>0.05), aceitamos a hipótese nula e inferimos que média do Reino Unido não é significativamente superior à média de Portugal"), file, append=TRUE)

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
friedman <- friedman.test(y=all_samples$new_deaths_per_million, groups=all_samples$location, blocks=all_samples$date)

write("Distribuição dos dados não é normal", file, append=TRUE)
write(c("\nTeste de Friedman p-value", friedman$p.value, "De acordo com o p-value obtido, aceitamos a hipótese nula que infere a não existência de diferenças significativas entre as amostras exploradas (p-value > 0.05)"), file, append=TRUE)


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

#testar distribuição normal dos dados
qqplot_afr <- ggqqplot(afr_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_afr <- shapiro.test(afr_samples$new_deaths_per_million)
png(filename="4.2.c_áfrica_qqplot.png")
plot(qqplot_afr)
dev.off()

qqplot_asi <- ggqqplot(asi_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_asi <- shapiro.test(asi_samples$new_deaths_per_million)
png(filename="4.2.c_ásia_qqplot.png")
plot(qqplot_asi)
dev.off()

qqplot_eur <- ggqqplot(eur_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_eur <- shapiro.test(eur_samples$new_deaths_per_million)
png(filename="4.2.c_europa_qqplot.png")
plot(qqplot_eur)
dev.off()

qqplot_nam <- ggqqplot(nam_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_nam <- shapiro.test(nam_samples$new_deaths_per_million)
png(filename="4.2.c_america_norte_qqplot.png")
plot(qqplot_nam)
dev.off()

qqplot_sam <- ggqqplot(sam_samples$new_deaths_per_million, ylab = "Mortes Diarias/Milhão de habitantes", xlabel= "Teorético")
shap_sam <- shapiro.test(sam_samples$new_deaths_per_million)
png(filename="4.2.c_américa_sul_qqplot.png")
plot(qqplot_sam)
dev.off()

all_samples <- rbind(afr_samples, asi_samples, eur_samples, nam_samples, sam_samples)


#testamos a variância
levene_test <- leveneTest(all_samples$new_deaths_per_million, group=all_samples$location)

levene_test

write(c("Shapiro p-values","África: ", shap_afr$p.value,
						   "Ásia: ", shap_asi$p.value,
						   "Europa: ", shap_eur$p.value,
						   "América do Norte: ", shap_nam$p.value,
						   "América do Sul: ", shap_sam$p.value,
		"Apesar dos grupos de dados não serem todos normalmente distribuidos, as amostras são grandes (>=30) e independentes. No entanto, as variâncias são significativamente diferentes logo usamos Kruskal-Wallis"), file, append=TRUE)

#Usamos Kruskal-Wallis
kruskal <- kruskal.test(new_deaths_per_million ~ location , data=all_samples)

kruskal
write(c("Kruskal-Wallis Hipótese Nula p-value:", "< 2.2e-16"), file, append = TRUE)

write(c("De acordo com o p-value obtido, rejeitamos a hipótese nula e inferimos que as médias das amostras são significativamente diferentes."), file, append= TRUE)

#análise post hoc

#p-value < 0.05, rejeitamos hipótese nula e inferimos que as varianças apresentam diferenças significativas entre elas
#assim, usamos o teste de Games Howell, porque as variâncias são diferentes
all_samples$location <- as.factor(all_samples$location)

ght <- gamesHowellTest(new_deaths_per_million~location, data=all_samples)

write(c("Após análise post-hoc, verificamos (através dos p-values do teste Games-Howell), que apenas os pares Ásia - África (p-value approx. 0.97), América do Sul - Europa (p-value approx. 0.58) e América do Sul - América do Norte (p-value approx. 0.19) tem médias semelhantes, sendo que os restantes pares são significativamente diferentes"), file, append= TRUE)
