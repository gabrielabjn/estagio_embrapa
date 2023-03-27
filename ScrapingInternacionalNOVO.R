# Código Internacional #

# ATENCAO: #
# PASSAR TODOS OS VALORES NA PLANILHA PARA VALOR (SÓ SELECIONAR TODOS E CLICAR #
# NO SIMBOLO DE "ALERTA" E ESCOLHER A OPCAO "CONVERTER EM NUMERO") #
# APOS A CONVERSAO, MULTIPLICAR OS VALORES DA SOJA (GRAO E OLEO), #
# MILHO E TRIGO POR 100 #

install.packages("RQuantLib")
install.packages("bizdays")
install.packages("lubridate")
install.packages("openxlsx")
install.packages("XML")
install.packages("httr")
install.packages("dplyr")
install.packages("rvest")
install.packages("readxl")

library(RQuantLib)
library(bizdays)
library(lubridate)
library(openxlsx)
library(XML)
library(httr)
library(dplyr)
library(rvest)
library(readxl)
options(OutDec = ",")

# Abrindo a planilha e descobrindo o último dia com dados #

setwd("Z:/user/Bdados/Setorial/Agricola/Pre�os")

planilha <- read_excel("Precos_diarios.xlsx",  sheet = "Internacional")
datas <- planilha[1]
ultimo_dia <- tail(datas,1)
ultimo_dia <- as.POSIXlt.Date(ultimo_dia)
dia_da_semana <- weekdays.POSIXt(ultimo_dia)
hoje <- Sys.Date()
ultimo_dia <- tail(datas, n=1)
ultimo_dia <- as.matrix.POSIXlt(ultimo_dia)
ano1 <- year(ultimo_dia)
ano2 <- year(hoje)
ultimo_dia <- tail(datas, n=1)
ultimo_dia <- as.matrix.POSIXlt(ultimo_dia)
variavel1 <- as.matrix.POSIXlt(variavel1)
load_rmetrics_calendars(ano1)
load_rmetrics_calendars(ano2)
bizdays.options$set(default.calendar = "Rmetrics/NYSE")
dias_uteis <- bizdays(ultimo_dia, hoje)
du <- bizdays(ultimo_dia, hoje)

# Pegando as cotacoes #

# ACUCAR #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/sucroenergetico/acucar-bolsa-de-nova-iorque-nybot'
site <- read_html(url)

acucar1 <- 0
acucar1 <- as.data.frame(acucar1)
acucar2 <- 0
acucar2 <- as.data.frame(acucar2)
acucar3 <- 0
acucar3 <- as.data.frame(acucar3)
datas <- "."
datas <- as.data.frame(datas)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  
  acucar1 <- rbind(acucar1, AJUSTE3[1,2])
  acucar2 <- rbind(acucar2, AJUSTE3[2,2])
  acucar3 <- rbind(acucar3, AJUSTE3[3,2])
  
  acucar1 <- as.data.frame(acucar1)
  acucar2 <- as.data.frame(acucar2)
  acucar3 <- as.data.frame(acucar3)
  
  du <- du - 1
  
}

du <- dias_uteis

datas <- datas[-1,]
datas <- as.data.frame(datas)

acucar1 <- acucar1[-1,]
acucar1 <- as.data.frame(acucar1)
acucar2 <- acucar2[-1,]
acucar2 <- as.data.frame(acucar2)
acucar3 <- acucar3[-1,]
acucar3 <- as.data.frame(acucar3)

# ALGODAO #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/algodao/algodao-bolsa-de-nova-iorque-nybot'
site <- read_html(url)

algodao1 <- 0
algodao1 <- as.data.frame(algodao1)
algodao2 <- 0
algodao2 <- as.data.frame(algodao2)
algodao3 <- 0
algodao3 <- as.data.frame(algodao3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  algodao1 <- rbind(algodao1, AJUSTE3[1,2])
  algodao2 <- rbind(algodao2, AJUSTE3[2,2])
  algodao3 <- rbind(algodao3, AJUSTE3[3,2])
  
  algodao1 <- as.data.frame(algodao1)
  algodao2 <- as.data.frame(algodao2)
  algodao3 <- as.data.frame(algodao3)
  
  du <- du - 1
  
}

du <- dias_uteis

algodao1 <- algodao1[-1,]
algodao1 <- as.data.frame(algodao1)
algodao2 <- algodao2[-1,]
algodao2 <- as.data.frame(algodao2)
algodao3 <- algodao3[-1,]
algodao3 <- as.data.frame(algodao3)

# CAFE NY #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/cafe/cafe-bolsa-de-nova-iorque-nybot'
site <- read_html(url)

cafe1 <- 0
cafe1 <- as.data.frame(cafe1)
cafe2 <- 0
cafe2 <- as.data.frame(cafe2)
cafe3 <- 0
cafe3 <- as.data.frame(cafe3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  cafe1 <- rbind(cafe1, AJUSTE3[1,2])
  cafe2 <- rbind(cafe2, AJUSTE3[2,2])
  cafe3 <- rbind(cafe3, AJUSTE3[3,2])
  
  cafe1 <- as.data.frame(cafe1)
  cafe2 <- as.data.frame(cafe2)
  cafe3 <- as.data.frame(cafe3)
  
  du <- du - 1
  
}

du <- dias_uteis

cafe1 <- cafe1[-1,]
cafe1 <- as.data.frame(cafe1)
cafe2 <- cafe2[-1,]
cafe2 <- as.data.frame(cafe2)
cafe3 <- cafe3[-1,]
cafe3 <- as.data.frame(cafe3)

# CAFE LONDON #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/cafe/cafe-bolsa-de-londres-liffe'
site <- read_html(url)

cafe4 <- 0
cafe4 <- as.data.frame(cafe4)
cafe5 <- 0
cafe5 <- as.data.frame(cafe5)
cafe6 <- 0
cafe6 <- as.data.frame(cafe6)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  cafe4 <- rbind(cafe4, AJUSTE3[1,2])
  cafe5 <- rbind(cafe5, AJUSTE3[2,2])
  cafe6 <- rbind(cafe6, AJUSTE3[3,2])
  
  cafe4 <- as.data.frame(cafe4)
  cafe5 <- as.data.frame(cafe5)
  cafe6 <- as.data.frame(cafe6)
  
  du <- du - 1
  
}

du <- dias_uteis

cafe4 <- cafe4[-1,]
cafe4 <- as.data.frame(cafe4)
cafe5 <- cafe5[-1,]
cafe5 <- as.data.frame(cafe5)
cafe6 <- cafe6[-1,]
cafe6 <- as.data.frame(cafe6)

# SOJA #

# Soja grao #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/soja/soja-bolsa-de-chicago-cme-group'
site <- read_html(url)

soja1 <- 0
soja1 <- as.data.frame(soja1)
soja2 <- 0
soja2 <- as.data.frame(soja2)
soja3 <- 0
soja3 <- as.data.frame(soja3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  soja1 <- rbind(soja1, ((AJUSTE3[1,2])))
  soja2 <- rbind(soja2, ((AJUSTE3[2,2])))
  soja3 <- rbind(soja3, ((AJUSTE3[3,2])))
  
  soja1 <- as.data.frame(soja1)
  soja2 <- as.data.frame(soja2)
  soja3 <- as.data.frame(soja3)
  
  du <- du - 1
  
}

du <- dias_uteis

soja1 <- soja1[-1,]
soja1 <- as.data.frame(soja1)
soja2 <- soja2[-1,]
soja2 <- as.data.frame(soja2)
soja3 <- soja3[-1,]
soja3 <- as.data.frame(soja3)

# Soja farelo #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/soja/farelo-de-soja-chicago-cbot'
site <- read_html(url)

soja4 <- 0
soja4 <- as.data.frame(soja4)
soja5 <- 0
soja5 <- as.data.frame(soja5)
soja6 <- 0
soja6 <- as.data.frame(soja6)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  soja4 <- rbind(soja4, AJUSTE3[1,2])
  soja5 <- rbind(soja5, AJUSTE3[2,2])
  soja6 <- rbind(soja6, AJUSTE3[3,2])
  
  soja4 <- as.data.frame(soja4)
  soja5 <- as.data.frame(soja5)
  soja6 <- as.data.frame(soja6)
  
  du <- du - 1
  
}

du <- dias_uteis

soja4 <- soja4[-1,]
soja4 <- as.data.frame(soja4)
soja5 <- soja5[-1,]
soja5 <- as.data.frame(soja5)
soja6 <- soja6[-1,]
soja6 <- as.data.frame(soja6)

#Soja oleo #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/soja/oleo-de-soja-chicago-cbot'
site <- read_html(url)

soja7 <- 0
soja7 <- as.data.frame(soja7)
soja8 <- 0
soja8 <- as.data.frame(soja8)
soja9 <- 0
soja9 <- as.data.frame(soja9)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  soja7 <- rbind(soja7, ((AJUSTE3[1,2])))
  soja8 <- rbind(soja8, ((AJUSTE3[2,2])))
  soja9 <- rbind(soja9, ((AJUSTE3[3,2])))
  
  soja7 <- as.data.frame(soja7)
  soja8 <- as.data.frame(soja8)
  soja9 <- as.data.frame(soja9)
  
  du <- du - 1
  
}

du <- dias_uteis

soja7 <- soja7[-1,]
soja7 <- as.data.frame(soja7)
soja8 <- soja8[-1,]
soja8 <- as.data.frame(soja8)
soja9 <- soja9[-1,]
soja9 <- as.data.frame(soja9)

# MILHO #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/milho/milho-bolsa-de-chicago-cme-group'
site <- read_html(url)

milho1 <- 0
milho1 <- as.data.frame(milho1)
milho2 <- 0
milho2 <- as.data.frame(milho2)
milho3 <- 0
milho3 <- as.data.frame(milho3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  milho1 <- rbind(milho1, ((AJUSTE3[1,2])))
  milho2 <- rbind(milho2, ((AJUSTE3[2,2])))
  milho3 <- rbind(milho3, ((AJUSTE3[3,2])))
  
  milho1 <- as.data.frame(milho1)
  milho2 <- as.data.frame(milho2)
  milho3 <- as.data.frame(milho3)
  
  du <- du - 1
  
}

du <- dias_uteis

milho1 <- milho1[-1,]
milho1 <- as.data.frame(milho1)
milho2 <- milho2[-1,]
milho2 <- as.data.frame(milho2)
milho3 <- milho3[-1,]
milho3 <- as.data.frame(milho3)

# TRIGO #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/trigo/trigo-bolsa-de-chicago-cme-group'
site <- read_html(url)

trigo1 <- 0
trigo1 <- as.data.frame(trigo1)
trigo2 <- 0
trigo2 <- as.data.frame(trigo2)
trigo3 <- 0
trigo3 <- as.data.frame(trigo3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  trigo1 <- rbind(trigo1, ((AJUSTE3[1,2])))
  trigo2 <- rbind(trigo2, ((AJUSTE3[2,2])))
  trigo3 <- rbind(trigo3, ((AJUSTE3[3,2])))
  
  trigo1 <- as.data.frame(trigo1)
  trigo2 <- as.data.frame(trigo2)
  trigo3 <- as.data.frame(trigo3)
  
  du <- du - 1
  
}

du <- dias_uteis

trigo1 <- trigo1[-1,]
trigo1 <- as.data.frame(trigo1)
trigo2 <- trigo2[-1,]
trigo2 <- as.data.frame(trigo2)
trigo3 <- trigo3[-1,]
trigo3 <- as.data.frame(trigo3)

# ETANOL #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/sucroenergetico/etanol-bolsa-de-chicago-cme-group'
site <- read_html(url)

etanol1 <- 0
etanol1 <- as.data.frame(etanol1)
etanol2 <- 0
etanol2 <- as.data.frame(etanol2)
etanol3 <- 0
etanol3 <- as.data.frame(etanol3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  etanol1 <- rbind(etanol1, AJUSTE3[1,2])
  etanol2 <- rbind(etanol2, AJUSTE3[2,2])
  etanol3 <- rbind(etanol3, AJUSTE3[3,2])
  
  etanol1 <- as.data.frame(etanol1)
  etanol2 <- as.data.frame(etanol2)
  etanol3 <- as.data.frame(etanol3)
  
  du <- du - 1
  
}

du <- dias_uteis

etanol1 <- etanol1[-1,]
etanol1 <- as.data.frame(etanol1)
etanol2 <- etanol2[-1,]
etanol2 <- as.data.frame(etanol2)
etanol3 <- etanol3[-1,]
etanol3 <- as.data.frame(etanol3)

# SUCO DE LARANJA #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/laranja/suco-de-laranja-bolsa-de-nova-iorque-nybot'
site <- read_html(url)

laranja1 <- 0
laranja1 <- as.data.frame(laranja1)
laranja2 <- 0
laranja2 <- as.data.frame(laranja2)
laranja3 <- 0
laranja3 <- as.data.frame(laranja3)

while(du >= 1) {
  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    .[du] %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[1]])
  
  AJUSTE <- lista_tabela[[1]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  laranja1 <- rbind(laranja1, AJUSTE3[1,2])
  laranja2 <- rbind(laranja2, AJUSTE3[2,2])
  laranja3 <- rbind(laranja3, AJUSTE3[3,2])
  
  laranja1 <- as.data.frame(laranja1)
  laranja2 <- as.data.frame(laranja2)
  laranja3 <- as.data.frame(laranja3)
  
  du <- du - 1
  
}

du <- dias_uteis

laranja1 <- laranja1[-1,]
laranja1 <- as.data.frame(laranja1)
laranja2 <- laranja2[-1,]
laranja2 <- as.data.frame(laranja2)
laranja3 <- laranja3[-1,]
laranja3 <- as.data.frame(laranja3)

# PETROLEO #

petroleo1 <- "Site"
petroleo1 <- as.data.frame(petroleo1)
petroleo2 <- "Site"
petroleo2 <- as.data.frame(petroleo2)
petroleo3 <- "Site"
petroleo3 <- as.data.frame(petroleo3)

while (du >= 1){
  petroleo1 <- rbind(petroleo1, "Site")
  petroleo1 <- as.data.frame(petroleo1)
  petroleo2 <- rbind(petroleo2, "Site")
  petroleo2 <- as.data.frame(petroleo2)
  petroleo3 <- rbind(petroleo3, "Site")
  petroleo3 <- as.data.frame(petroleo3)
  du <- du - 1
}

du <- dias_uteis

petroleo1 <- petroleo1[-1,]
petroleo1 <- as.data.frame(petroleo1)
petroleo2 <- petroleo2[-1,]
petroleo2 <- as.data.frame(petroleo2)
petroleo3 <- petroleo3[-1,]
petroleo3 <- as.data.frame(petroleo3)

# CRB #

url <- 'https://www.investing.com/indices/thomson-reuters-core-cmdty-crb-historical-data'
site <- read_html(url)

vetor1 <- 0
vetor1 <- as.data.frame(vetor1)

c <- 1

  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  
  head(lista_tabela[[2]])
  
  AJUSTE <- lista_tabela[[2]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  
  while(du >= 1){
    
  vetor1 <- rbind(vetor1, AJUSTE3[c,2])
  
  crb <- as.data.frame(vetor1)
  
  c <- c + 1
  du <- du - 1
    
  }

du <- dias_uteis

vetor1 <- vetor1[-1,]
vetor1 <- as.data.frame(vetor1)

crb <- 0
crb <- as.data.frame(crb)
parametro <- NROW(vetor1)

while(parametro >= 1){
  
  crb <- rbind(crb, vetor1[parametro,1])
  parametro = parametro - 1
  
}

crb <- crb[-1,]
crb <- as.data.frame(crb)

# CLASS III #

url <- 'https://www.investing.com/commodities/class-iii-milk-futures-historical-data'
site <- read_html(url)

vetor2 <- 0
vetor2 <- as.data.frame(vetor2)

c <- 1

  #Escolhe qual o elemento HTML para coletar - resultado em HTML #
  info_Ajuste_HTML <- html_nodes(site,'table')
  info_Ajuste <- html_text(info_Ajuste_HTML)
  head(info_Ajuste,20)
  head(info_Ajuste_HTML)
  lista_tabela <- site %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  str(lista_tabela)
  head(lista_tabela[[2]])
  
  AJUSTE <- lista_tabela[[2]]
  dataajuste <- AJUSTE[1]
  AJUSTE2 <- AJUSTE[2]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
 
   while(du >= 1) {
     
  datas <- rbind(datas, AJUSTE3[c,1])
  datas <- as.data.frame(datas)
  
  vetor2 <- rbind(vetor2, AJUSTE3[c,2])
  vetor2 <- as.data.frame(vetor2)
  
  c <- c + 1
  du <- du - 1
  
}

du <- dias_uteis

vetor2 <- vetor2[-1,]
vetor2 <- as.data.frame(vetor2)
parametro <- NROW(vetor2)

datasfinal <- 0
datasfinal <- as.data.frame(datasfinal)
classiii <- 0
classiii <- as.data.frame(classiii)

while (parametro >= 1){
  
  datasfinal <- rbind(datasfinal, datas[parametro,1])
  classiii <- rbind(classiii, vetor2[parametro,1])
  parametro = parametro - 1
}

datasfinal <- datasfinal[-1,]
datasfinal <- as.data.frame(datasfinal)
classiii <- classiii[-1,]
classiii <- as.data.frame(classiii)

# CLASS IV #

classiv <- "Site"
classiv <- as.data.frame(classiv)

while (du >= 1){
  
  classiv <- rbind(classiv, "Site")
  classiv <- as.data.frame(classiv)
  
  du <- du - 1
}

du <- dias_uteis

classiv <- classiv[-1,]
classiv <- as.data.frame(classiv)

# Juntando os dados #

tabela <- cbind(datasfinal, acucar1, acucar2, acucar3, algodao1, algodao2, algodao3, cafe1, cafe2, cafe3, cafe4, cafe5, cafe6,
                soja1, soja2, soja3, soja4, soja5, soja6, soja7, soja8, soja9, milho1, milho2, milho3, trigo1, trigo2, trigo3, etanol1,
                etanol2, etanol3, laranja1, laranja2, laranja3, petroleo1, petroleo2, petroleo3, crb, classiii, classiv)

colnames(tabela) <- c("Datas", "Acucar 1", "Acucar 2", "Acucar 3", "Algodao 1", "Algodao 2", "Algodao 3", "Cafe 1", "Cafe 2", "Cafe 3",
                      "Cafe 4", "Cafe 5", "Cafe 6", "Soja grao 1", "Soja grao 2", "Soja grao 3", "Soja Farelo 1", "Soja Farelo 2", "Soja Farelo 3",
                      "Soja Oleo 1", "Soja Oleo 2", "Soja Oleo 3", "Milho 1", "Milho 2", "Milho 3", "Trigo 1", "Trigo 2", "Trigo 3",
                      "Etanol 1", "Etanol 2", "Etanol 3", "Laranja 1", "Laranja 2", "Laranja 3", "Petroleo 1", "Petroleo 2", "Petroleo 3",
                      "CRB", "Class III", "Class IV")

tabela <- tabela[-1,]

setwd("Z:/user/Planilha calculo/Agricola/Precos/scraping")

write.xlsx(tabela, "Z:/user/Planilha calculo/Agricola/Precos/scraping/valorNOVO.xlsx", colNames = TRUE)

