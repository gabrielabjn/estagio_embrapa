
### Modelo de Previsao do Preco Trimestral do Leite (PTL) 
### Brasil

### Carregar bibliotecas necessárias
library(readxl)
library(forecast)


### Definir o diretório

setwd("Z:/user/Projetos/Projecoes/PTL")

######## Mantenha os dados do ICP, preço ao produtor deflacionado e da PTL 
######## da planilha PROJEÇÃO PTL 
######## atualizados 


########


DADOS<- read_excel("Z:/user/Projetos/Projecoes/PTL/PROJECAO_PTL.xlsx")

#### define os regressores com lags e a variavel resposta
PTL<-ts(na.exclude(na.exclude( DADOS$`PTL br`)[-c(1:12)]),start = c(2007,04),frequency = 12)
PTL_lag12 <-ts(na.exclude( DADOS$`PTL br`),start = c(2007,04),frequency = 12)
ICP_lag3 <-ts(na.exclude( DADOS$ICP[-c(1:9)]),start = c(2007,04),frequency = 12)
farm_gat_lag3 <-ts(na.exclude( DADOS$`Preço ao produtor deflacionado icp`[-c(1:9)]),start = c(2007,04),frequency = 12)

datatrain<-ts.intersect(PTL,PTL_lag12,ICP_lag3,farm_gat_lag3,dframe = TRUE)

trendtrains<-1:length(datatrain$PTL)

datatrain$trendtrains<-trendtrains

#### estima o modelo
tsglm<-glm(PTL ~ 
            
             trendtrains
            
          + PTL_lag12
          + ICP_lag3
          + farm_gat_lag3
          
          
          ,
          family =  Gamma(link = "identity"),data = datatrain )



summary(tsglm)


##### define as variaveis para prever os próximos meses;
##### PREC AO PRODUTOR E ICP DEVEM TER O MESMO NÚMERO DE VARIAVEIS 

ICP_lag3prev <-ICP_lag3[(length(PTL)+1):(length(ICP_lag3))]
farm_gat_lag3prev <-farm_gat_lag3[(length(PTL)+1):(length(farm_gat_lag3))]
PTL_lag12prev <-PTL_lag12[(length(PTL) + 1):(length(farm_gat_lag3))]
trendprev<-(length(PTL)+1):(length(farm_gat_lag3))

matPrev<-matrix( c(ICP_lag3prev,farm_gat_lag3prev,PTL_lag12prev,trendprev),ncol = 4)
colnames(matPrev)<-c("ICP_lag3","farm_gat_lag3","PTL_lag12","trendtrains")
matPrev<-as.data.frame(matPrev)
pred<-predict.glm(tsglm,newdata =matPrev)
write.csv2(pred,"pred_br.csv")


ts.plot(pred,lty=c(1:2),col = c(1:2))

# ------------------------------------------------------------------------------

# TESTES ( DESCONSIDERAR )

plp <- DADOS$`Preço ao produtor deflacionado icp`[-c(189:204)]
ptl <- DADOS$`PTL br`[-c(189:204)]
icp <- DADOS$ICP[-c(189:204)]

lag1.plot(plp,12,corr=TRUE)

lag2.plot(plp, ptl, max.lag = 12, corr = TRUE)

lag2.plot(icp, ptl, max.lag = 12, corr = TRUE)
















