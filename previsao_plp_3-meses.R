
# Previsao PLP e Spot - 3 meses

# Observacoes

# Este modelo consiste em prever o spot via auto.arima() e, depois, utilizar esse dado como entrada em nosso melhor modelo de previsao do preco ao produtor.
# Esse procedimento eh automatizado para ser repetido 3 vezes, de forma que tenhamos, na saida, previsoes para os proximos 3 meses.
# A funcao auto.arima escolhe automaticamente os seus parametros com base na serie temporal fornecida.

# Aviso: Nao utilize neste codigo caracteres como acento circunflexo/agudo, til, c cedilha. 

setwd("Z:/user/Projetos/Projecoes/Preço ao produtor") # define o diretorio (onde estao os arquivos usados neste codigo)
library(readxl) # abre a biblioteca que le excel
library('forecast') # biblioteca com funcoes de previsao 

DADOS <- read_excel("Z:/user/Projetos/Projecoes/Preço ao produtor/TESTE-PRECO_AO_PRODUTOR.xlsx") # salvando dados da planilha PRECO_AO_PRODUTOR 

# O codigo abaixo se baseou na funcao auto.arima() para prever tanto spot quanto preco ao produtor, porem a previsao nao se mostrou eficiente.

#####################

# spot_icp_ts<-ts(DADOS$SPOT_ICP,start = c(2006,04),frequency = 12) #criando a serie temporal spot_icp_ts 
# plot.ts(spot_icp_ts)
# plot(decompose(spot_icp_ts))
# 
# prev<-auto.arima(spot_icp_ts)
# prev<-forecast(prev, h = 3)
# prev #previsao para spot (proximos 3 meses)
# prev_principal<- as.numeric(prev$mean)

######################

# preco_prod_ts<-ts(DADOS$`Preço ao produtor ICP`, start = c(2006,04),frequency = 12) # criando a serie temporal spot_icp_ts
# prev<-auto.arima(preco_prod_ts)
# prev<-forecast(prev, h = 5)
# prev

# glauco disse que apresentou uma queda muito forte. ele acredita que os precos ficarao mais estaveis...

######################

previsoes<-c(1:3) # vetor para armazenar as previsoes para PLP (preco do leite ao produtor)
i<-1 # contador

spot_icp_ts<-ts(DADOS$SPOT_ICP,start = c(2006,04),frequency = 12) # criando a serie temporal para  o spot_icp 
prev<-auto.arima(spot_icp_ts)
prev<-forecast(prev, h = 1)
prev #previsao para spot

x<-matrix(NA,3,2)

colnames(x) <- c("PLP", "SPOT")

spot_serie<-as.array(DADOS$SPOT_ICP)

plp_serie<-as.array(DADOS$`Preço ao produtor ICP`)

while (i<4){
  
  if (i!=1){
    
    spot_serie<-append(spot_serie,x[i-1,2])
    spot_serie
    
    plp_serie<-append(plp_serie,previsoes[i-1])
    plp_serie
    
    prec_prod_ICP<-ts(na.omit(plp_serie[2:length(plp_serie)]),start = c(2006,04),frequency = 12)
    length(prec_prod_ICP)
    SPOT_ICP<-ts(spot_serie[-c(length(spot_serie))],start = c(2006,04),frequency = 12)
    length(SPOT_ICP)
    
    
    fit<-Arima(prec_prod_ICP,xreg = SPOT_ICP,order = c(3,1,0),seasonal = list(order=c(1,1,1),frequency=12))
  
    forecast<-forecast(fit,xreg = SPOT_ICP[c(length(SPOT_ICP))],h= 1)
    
    previsoes[i]<-as.numeric(forecast$mean)
    
    spot_icp_ts<-ts(spot_serie, start = c(2006,04),frequency = 12) # criando a serie temporal spot_icp_ts 
    prev<-auto.arima(spot_icp_ts)
    prev<-forecast(prev, h = 1)
    prev #previsao para spot
    x[i,2]<- as.numeric(prev$mean)
  
  }
  
  else{

    prec_prod_ICP<-ts(na.omit( DADOS$`Preço ao produtor ICP`[2:length(DADOS$`Preço ao produtor ICP`)]),start = c(2006,04),frequency = 12)
    SPOT_ICP<-ts(DADOS$SPOT_ICP[-c(length(DADOS$SPOT_ICP))],start = c(2006,04),frequency = 12)
    fit<-Arima(prec_prod_ICP,xreg = SPOT_ICP,order = c(3,1,0),seasonal = list(order=c(1,1,1),frequency=12))
    forecast<-forecast(fit,xreg =DADOS$SPOT_ICP[c(length(DADOS$SPOT_ICP))],h= 1)

    previsoes[i]<-as.numeric(forecast$mean)
    
    spot_icp_ts<-ts(spot_serie, start = c(2006,04),frequency = 12) # criando a serie temporal spot_icp_ts 
    prev<-auto.arima(spot_icp_ts)
    prev<-forecast(prev, h = 1)
    prev #previsao para spot
    x[i,2]<- as.numeric(prev$mean)

  
  }


i<-i+1

}

previsoes

x[1,1]<-previsoes[1]
x[2,1]<- previsoes[2]
x[3,1]<-previsoes[3]
x

write.csv2(x,"pred_3-meses.csv")



