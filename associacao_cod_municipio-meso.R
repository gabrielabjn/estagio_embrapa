
setwd("Z:/user/Projetos/Papers/Correlacoes_Gabriela/demandas")

# Associacao entre codigos de municipios a codigos de mesorregiao
# IBGE

library(readxl)

# MINAS GERAIS -----------------------------------------------------------------

dados<- read_excel("Z:/user/Projetos/Papers/Correlacoes_Gabriela/demandas/relacao_distrito_mesorreg_MG.xlsx")
relacao<-read_excel("Z:/user/Projetos/Papers/Correlacoes_Gabriela/demandas/Relação de produtores Março2023.xlsx")

meso<-dados$mesorreg
municipio<-dados$municipio 

municipio_desejado<-relacao$`MUNICIPIO IBGE`

# Encontrar os índices correspondentes dos códigos de municípios no vetor de códigos
indices <- match(municipio_desejado, municipio)

# Obter os códigos de mesorregiões correspondentes aos códigos de municípios desejados
mesorregioes_associadas <- meso[indices]

mesorregioes_associadas

write.csv2(mesorregioes_associadas,"mesorreg_associadas.csv")

rm(list=ls())

# GOIÁS ------------------------------------------------------------------------

dados<- read_excel("Z:/user/Projetos/Papers/Correlacoes_Gabriela/demandas/relacao_distrito_mesorreg_GO.xlsx")
relacao<-read_excel("Z:/user/Projetos/Papers/Correlacoes_Gabriela/demandas/Relação de produtores Março2023.xlsx")

meso<-dados$mesorreg
municipio<-dados$municipio 

municipio_desejado<-relacao$`MUNICIPIO IBGE`[2822]

length(municipio_desejado)

# Encontrar os índices correspondentes dos códigos de municípios no vetor de códigos
indices <- match(municipio_desejado, municipio)

# Obter os códigos de mesorregiões correspondentes aos códigos de municípios desejados
mesorregioes_associadas <- meso[indices]

mesorregioes_associadas

write.csv2(mesorregioes_associadas,"mesorreg_associadas_GO.csv")

rm(list=ls())



