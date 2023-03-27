# Código Doméstico #

# ATENCAO: #
# PASSAR TODOS OS VALORES NA PLANILHA PARA VALOR (SÓ SELECIONAR TODOS E CLICAR #
# NO SIMBOLO DE "ALERTA" E ESCOLHER A OPCAO "CONVERTER EM NUMERO") #
# PRECOS DO DERAL AINDA TEM QUE SER EXTRAIDOS NO SITE SEMANALMENTE #

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

planilha <- read_excel("Precos_diarios.xlsx",  sheet = "Domestico")
datas <- planilha[1]
ultimo_dia <- tail(datas,1)
ultimo_dia <- as.POSIXlt.Date(ultimo_dia)
dia_da_semana <- weekdays.POSIXt(ultimo_dia)
hoje <- Sys.Date()
ultimo_dia <- tail(datas, n=1)

url <- 'https://www.cepea.esalq.usp.br/br/indicador/acucar.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste,AJUSTE2)
parametro <- NROW(AJUSTE3)
p <- NROW(AJUSTE3)
datas1 = "0/0/0"
datas1 <- as.data.frame(datas1)
while (parametro >= 1){
datas1 <- rbind(datas1,AJUSTE3[parametro,1])
parametro <- parametro - 1
}
datas1 <- datas1[-1,]
  datas1 <- as.data.frame.Date(datas1)
  
  acucar1 = 0
  acucar1 <- as.data.frame(acucar1)
  while (p >= 1){
    acucar1 <- rbind(acucar1,AJUSTE3[p,2])
    p <- p - 1
  }
  
  acucar1 <- acucar1[-1,]
  acucar1 <- as.data.frame(acucar1)
  
 acucar <- cbind(datas1,acucar1)
variavel2 = NROW(acucar)

atualizacao <- variavel2 - dias_uteis

while (atualizacao >= 1){
  acucar <- acucar[-1,]
  atualizacao <- atualizacao - 1
}

datasfinal <- acucar[1]
acucar <- acucar[-1]

# ALGODAO EM PLUMA #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/algodao.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
algodao = 0
algodao <- as.data.frame(algodao)

while (p >= 1){
  algodao <- rbind(algodao,AJUSTE3[p,2])
  p <- p - 1
}

algodao <- algodao[-1,]
algodao <- as.data.frame(algodao)

variavel3 = NROW(algodao)

atualizacao2 <- variavel3 - dias_uteis

while (atualizacao2 >= 1){
  algodao <- algodao[-1,]
  algodao <- as.data.frame(algodao)
  atualizacao2 <- atualizacao2 - 1
}

# BOI GORDO #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/boi-gordo.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
boigordo = 0
boigordo <- as.data.frame(boigordo)

while (p >= 1){
  boigordo <- rbind(boigordo,AJUSTE3[p,2])
  p <- p - 1
}

boigordo <- boigordo[-1,]
boigordo <- as.data.frame(boigordo)

variavel4 = NROW(boigordo)

atualizacao3 <- variavel4 - dias_uteis

while (atualizacao3 >= 1){
  boigordo <- boigordo[-1,]
  boigordo <- as.data.frame(boigordo)
  atualizacao3 <- atualizacao3 - 1
}

# BOI B3 #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/boi-gordo/boi-gordo-b3-prego-regular'
site <- read_html(url)

boi1 <- 0
boi1 <- as.data.frame(boi1)
boi2 <- 0
boi2 <- as.data.frame(boi2)
boi3 <- 0
boi3 <- as.data.frame(boi3)

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
boi1 <- rbind(boi1, AJUSTE3[1,2])
boi2 <- rbind(boi2, AJUSTE3[2,2])
boi3 <- rbind(boi3, AJUSTE3[3,2])

boi1 <- as.data.frame(boi1)
boi2 <- as.data.frame(boi2)
boi3 <- as.data.frame(boi3)

du <- du - 1

}

du <- dias_uteis

boi1 <- boi1[-1,]
boi1 <- as.data.frame(boi1)
boi2 <- boi2[-1,]
boi2 <- as.data.frame(boi2)
boi3 <- boi3[-1,]
boi3 <- as.data.frame(boi3)

# BEZERRO #

# MS #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/bezerro.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
bezerroms = 0
bezerroms <- as.data.frame(bezerroms)

while (p >= 1){
  bezerroms <- rbind(bezerroms,AJUSTE3[p,2])
  p <- p - 1
}

bezerroms <- bezerroms[-1,]
bezerroms <- as.data.frame(bezerroms)

variavel5 = NROW(bezerroms)

atualizacao4 <- variavel5 - dias_uteis

while (atualizacao4 >= 1){
  bezerroms <- bezerroms[-1,]
  bezerroms <- as.data.frame(bezerroms)
  atualizacao4 <- atualizacao4 - 1
}

# Peso Medio #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/bezerro.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
pesomedio = 0
pesomedio <- as.data.frame(pesomedio)

while (p >= 1){
  pesomedio <- rbind(pesomedio,AJUSTE3[p,2])
  p <- p - 1
}

pesomedio <- pesomedio[-1,]
pesomedio <- as.data.frame(pesomedio)

variavel6 = NROW(pesomedio)

atualizacao5 <- variavel6 - dias_uteis

while (atualizacao5 >= 1){
  pesomedio <- pesomedio[-1,]
  pesomedio <- as.data.frame(pesomedio)
  atualizacao5 <- atualizacao5 - 1
}

# SP #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/bezerro-media-sao-paulo.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
bezerrosp = 0
bezerrosp <- as.data.frame(bezerrosp)

while (p >= 1){
  bezerrosp <- rbind(bezerrosp,AJUSTE3[p,2])
  p <- p - 1
}

bezerrosp <- bezerrosp[-1,]
bezerrosp <- as.data.frame(bezerrosp)

variavel7 = NROW(bezerrosp)

atualizacao6 <- variavel7 - dias_uteis

while (atualizacao6 >= 1){
  bezerrosp <- bezerrosp[-1,]
  bezerrosp <- as.data.frame(bezerrosp)
  atualizacao6 <- atualizacao6 - 1
}

# CAFE #

# ARABICA #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/cafe.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
cafearabica = 0
cafearabica <- as.data.frame(cafearabica)

while (p >= 1){
  cafearabica <- rbind(cafearabica,AJUSTE3[p,2])
  p <- p - 1
}

cafearabica <- cafearabica[-1,]
cafearabica <- as.data.frame(cafearabica)

variavel8 = NROW(cafearabica)

atualizacao7 <- variavel8 - dias_uteis

while (atualizacao7 >= 1){
  cafearabica <- cafearabica[-1,]
  cafearabica <- as.data.frame(cafearabica)
  atualizacao7 <- atualizacao7 - 1
}

# ROBUSTA #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/cafe.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
caferobusta = 0
caferobusta <- as.data.frame(caferobusta)

while (p >= 1){
  caferobusta <- rbind(caferobusta,AJUSTE3[p,2])
  p <- p - 1
}

caferobusta <- caferobusta[-1,]
caferobusta <- as.data.frame(caferobusta)

variavel9 = NROW(caferobusta)

atualizacao8 <- variavel9 - dias_uteis

while (atualizacao8 >= 1){
  caferobusta <- caferobusta[-1,]
  caferobusta <- as.data.frame(caferobusta)
  atualizacao8 <- atualizacao8 - 1
}

# CAFE B3 #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/cafe/cafe-arabica-4-5-b3-prego-regular'
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

# SOJA #

# DERAL 1 #

sojaderal1 <- "Deral"
sojaderal1 <- as.data.frame(sojaderal1)

while (du >= 1){
  sojaderal1 <- rbind(sojaderal1, "Deral")
  sojaderal1 <- as.data.frame(sojaderal1)
  du <- du - 1
}

du <- dias_uteis

sojaderal1 <- sojaderal1[-1,]
sojaderal1 <- as.data.frame(sojaderal1)

# PARANA #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/soja.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
sojaparana = 0
sojaparana <- as.data.frame(sojaparana)

while (p >= 1){
  sojaparana <- rbind(sojaparana,AJUSTE3[p,2])
  p <- p - 1
}

sojaparana <- sojaparana[-1,]
sojaparana <- as.data.frame(sojaparana)

variavel10 = NROW(sojaparana)

atualizacao9 <- variavel10 - dias_uteis

while (atualizacao9 >= 1){
  sojaparana <- sojaparana[-1,]
  sojaparana <- as.data.frame(sojaparana)
  atualizacao9 <- atualizacao9 - 1
}

# PARANAGUA #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/soja.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
sojaparanagua = 0
sojaparanagua <- as.data.frame(sojaparanagua)

while (p >= 1){
  sojaparanagua <- rbind(sojaparanagua,AJUSTE3[p,2])
  p <- p - 1
}

sojaparanagua <- sojaparanagua[-1,]
sojaparanagua <- as.data.frame(sojaparanagua)

variavel11 = NROW(sojaparanagua)

atualizacao10 <- variavel11 - dias_uteis

while (atualizacao10 >= 1){
  sojaparanagua <- sojaparanagua[-1,]
  sojaparanagua <- as.data.frame(sojaparanagua)
  atualizacao10 <- atualizacao10 - 1
}

# SOJA B3 #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/soja/soja-b3-pregao-regular'
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
  soja1 <- rbind(soja1, AJUSTE3[1,2])
  soja2 <- rbind(soja2, AJUSTE3[2,2])
  soja3 <- rbind(soja3, AJUSTE3[3,2])
  
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

# DERAL 2 #

sojaderal2 <- "Deral"
sojaderal2 <- as.data.frame(sojaderal2)

while (du >= 1){
  sojaderal2 <- rbind(sojaderal2, "Deral")
  sojaderal2 <- as.data.frame(sojaderal2)
  du <- du - 1
}

du <- dias_uteis

sojaderal2 <- sojaderal2[-1,]
sojaderal2 <- as.data.frame(sojaderal2)

# DERAL 3 #

sojaderal3 <- "Deral"
sojaderal3 <- as.data.frame(sojaderal3)

while (du >= 1){
  sojaderal3 <- rbind(sojaderal3, "Deral")
  sojaderal3 <- as.data.frame(sojaderal3)
  du <- du - 1
}

du <- dias_uteis

sojaderal3 <- sojaderal3[-1,]
sojaderal3 <- as.data.frame(sojaderal3)

# MILHO #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/milho.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
milhocepea = 0
milhocepea <- as.data.frame(milhocepea)

while (p >= 1){
  milhocepea <- rbind(milhocepea,AJUSTE3[p,2])
  p <- p - 1
}

milhocepea <- milhocepea[-1,]
milhocepea <- as.data.frame(milhocepea)

variavel12 = NROW(milhocepea)

atualizacao11 <- variavel12 - dias_uteis

while (atualizacao11 >= 1){
  milhocepea <- milhocepea[-1,]
  milhocepea <- as.data.frame(milhocepea)
  atualizacao11 <- atualizacao11 - 1
}

# MILHO B3 #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/milho/milho-b3-prego-regular'
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
  milho1 <- rbind(milho1, AJUSTE3[1,2])
  milho2 <- rbind(milho2, AJUSTE3[2,2])
  milho3 <- rbind(milho3, AJUSTE3[3,2])
  
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

# PR #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/trigo.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
trigopr = 0
trigopr <- as.data.frame(trigopr)

while (p >= 1){
  trigopr <- rbind(trigopr,AJUSTE3[p,2])
  p <- p - 1
}

trigopr <- trigopr[-1,]
trigopr <- as.data.frame(trigopr)

variavel13 = NROW(trigopr)

atualizacao12 <- variavel13 - dias_uteis

while (atualizacao12 >= 1){
  trigopr <- trigopr[-1,]
  trigopr <- as.data.frame(trigopr)
  atualizacao12 <- atualizacao12 - 1
}

# RS #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/trigo.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2]%>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
trigors = 0
trigors <- as.data.frame(trigors)

while (p >= 1){
  trigors <- rbind(trigors,AJUSTE3[p,2])
  p <- p - 1
}

trigors <- trigors[-1,]
trigors <- as.data.frame(trigors)

variavel14 = NROW(trigors)

atualizacao13 <- variavel14 - dias_uteis

while (atualizacao13 >= 1){
  trigors <- trigors[-1,]
  trigors <- as.data.frame(trigors)
  atualizacao13 <- atualizacao13 - 1
}

# FRANGO #

# Congelado #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/frango.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
frangocongelado = 0
frangocongelado <- as.data.frame(frangocongelado)

while (p >= 1){
  frangocongelado <- rbind(frangocongelado,AJUSTE3[p,2])
  p <- p - 1
}

frangocongelado <- frangocongelado[-1,]
frangocongelado <- as.data.frame(frangocongelado)

variavel15 = NROW(frangocongelado)

atualizacao14 <- variavel15 - dias_uteis

while (atualizacao14 >= 1){
  frangocongelado <- frangocongelado[-1,]
  frangocongelado <- as.data.frame(frangocongelado)
  atualizacao14 <- atualizacao14 - 1
}

# Resfriado #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/frango.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
frangoresfriado = 0
frangoresfriado <- as.data.frame(frangoresfriado)

while (p >= 1){
  frangoresfriado <- rbind(frangoresfriado,AJUSTE3[p,2])
  p <- p - 1
}

frangoresfriado <- frangoresfriado[-1,]
frangoresfriado <- as.data.frame(frangoresfriado)

variavel16 = NROW(frangoresfriado)

atualizacao15 <- variavel16 - dias_uteis

while (atualizacao15 >= 1){
  frangoresfriado <- frangoresfriado[-1,]
  frangoresfriado <- as.data.frame(frangoresfriado)
  atualizacao15 <- atualizacao15 - 1
}

# SUINO #

url <- 'https://www.noticiasagricolas.com.br/cotacoes/suinos/indicador-do-suino-vivo-cepea-esalq'
site <- read_html(url)

suinomg <- 0
suinomg <- as.data.frame(suinomg)
suinopr <- 0
suinopr <- as.data.frame(suinopr)
suinosp <- 0
suinosp <- as.data.frame(suinosp)

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
  AJUSTE2 <- AJUSTE[3]
  AJUSTE3 <- cbind(dataajuste, AJUSTE2)
  suinomg <- rbind(suinomg, AJUSTE3[1,2])
  suinopr <- rbind(suinopr, AJUSTE3[2,2])
  suinosp <- rbind(suinosp, AJUSTE3[5,2])
  
  suinomg <- as.data.frame(suinomg)
  suinopr <- as.data.frame(suinopr)
  suinosp <- as.data.frame(suinosp)
  
  du <- du - 1
  
}

du <- dias_uteis

suinomg <- suinomg[-1,]
suinomg <- as.data.frame(suinomg)
suinopr <- suinopr[-1,]
suinopr <- as.data.frame(suinopr)
suinosp <- suinosp[-1,]
suinosp <- as.data.frame(suinosp)

# LARANJA #

# Industria #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/citros.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
laranjaindustria = 0
laranjaindustria <- as.data.frame(laranjaindustria)

while (p >= 1){
  laranjaindustria <- rbind(laranjaindustria,AJUSTE3[p,2])
  p <- p - 1
}

laranjaindustria <- laranjaindustria[-1,]
laranjaindustria <- as.data.frame(laranjaindustria)

variavel17 = NROW(laranjaindustria)

atualizacao16 <- variavel17 - dias_uteis

while (atualizacao16 >= 1){
  laranjaindustria <- laranjaindustria[-1,]
  laranjaindustria <- as.data.frame(laranjaindustria)
  atualizacao16 <- atualizacao16 - 1
}


# In natura #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/citros.aspx'
site <- read_html(url)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)
p <- NROW(AJUSTE3)
laranjanatura = 0
laranjanatura <- as.data.frame(laranjanatura)

while (p >= 1){
  laranjanatura <- rbind(laranjanatura,AJUSTE3[p,2])
  p <- p - 1
}

laranjanatura <- laranjanatura[-1,]
laranjanatura <- as.data.frame(laranjanatura)

variavel18 = NROW(laranjanatura)

atualizacao17 <- variavel18 - dias_uteis

while (atualizacao17 >= 1){
  laranjanatura <- laranjanatura[-1,]
  laranjanatura <- as.data.frame(laranjanatura)
  atualizacao17 <- atualizacao17 - 1
}

# ETANOL #

# Anidro #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/etanol.aspx'
site <- read_html(url)

anidro <- 0
anidro <- as.data.frame(anidro)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[2]%>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)

c = NROW(acucar)

  while (c >= 1){
    anidro <- rbind(anidro, AJUSTE3[1,2])
    anidro <- as.data.frame(anidro)
    c = c -1
}

anidro <- anidro[-1,]
anidro <- as.data.frame(anidro)

# Hidratado #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/etanol.aspx'
site <- read_html(url)

hidratado <- 0
hidratado <- as.data.frame(hidratado)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)

c = NROW(acucar)

  while (c >= 1){
    hidratado <- rbind(hidratado, AJUSTE3[1,2])
    hidratado <- as.data.frame(hidratado)
    c = c -1
  }

hidratado <- hidratado[-1,]
hidratado <- as.data.frame(hidratado)

# Hidratado Industrial #

url <- 'https://www.cepea.esalq.usp.br/br/indicador/etanol.aspx'
site <- read_html(url)

hidratadoind <- 0
hidratadoind <- as.data.frame(hidratadoind)

#Escolhe qual o elemento HTML para coletar - resultado em HTML #
info_Ajuste_HTML <- html_nodes(site,'table')
info_Ajuste <- html_text(info_Ajuste_HTML)
head(info_Ajuste,20)
head(info_Ajuste_HTML)
lista_tabela <- site %>%
  html_nodes("table") %>%
  .[3]%>%
  html_table(fill = TRUE)

str(lista_tabela)

head(lista_tabela[[1]])

AJUSTE <- lista_tabela[[1]]
dataajuste <- AJUSTE[1]
AJUSTE2 <- AJUSTE[2]
AJUSTE3 <- cbind(dataajuste, AJUSTE2)

c = NROW(acucar)

  while (c >= 1){
    hidratadoind <- rbind(hidratadoind, AJUSTE3[1,2])
    hidratadoind <- as.data.frame(hidratadoind)
    c = c -1
  }

hidratadoind <- hidratadoind[-1,]
hidratadoind <- as.data.frame(hidratadoind)

# Juntando os dados #

tabela <- cbind(datasfinal, acucar, algodao, boigordo, boi1, boi2, boi3, bezerroms, pesomedio, bezerrosp, cafearabica, caferobusta,
                cafe1, cafe2, cafe3, sojaderal1, sojaparanagua, sojaparana, soja1, soja2, soja3, sojaderal2, sojaderal3, milhocepea,
                milho1, milho2, milho3, trigopr, trigors, frangocongelado, frangoresfriado, suinomg, suinopr, suinosp, laranjaindustria,
                laranjanatura, anidro, hidratado, hidratadoind)

colnames(tabela) <- c("Datas", "Acucar", "Algodao", "Boi gordo", "Boi 1", "Boi 2", "Boi 3", "Bezerro MS", "Peso Medio", "Bezerro SP",
                      "Cafe Arabica", "Cafe Robusta", "Cafe 1", "Cafe 2", "Cafe 3", "Deral", "Soja Paranagua", "Soja Parana", "Soja 1",
                      "Soja 2", "Soja 3", "Deral", "Deral", "Milho Cepea", "Milho 1", "Milho 2", "Milho 3", "Trigo PR", "Trigo RS",
                      "Frango Congelado", "Frango Resfriado", "Suino MG", "Suino PR", "Suino SP", "Laranja Industria", "Laranja Natura",
                      "Anidro", "Hidratado", "Hidratado Industria")

tabela <- tabela [-1,]

setwd("Z:/user/Planilha calculo/Agricola/Precos/scraping")

write.xlsx(tabela, "Z:/user/Planilha calculo/Agricola/Precos/scraping/cepeaNOVO.xlsx", colNames = TRUE)

