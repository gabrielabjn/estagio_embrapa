setwd("Z:/user/Projetos/Projecoes/Preço ao produtor")
library(readxl)
library('forecast')
DADOS <- read_excel("Z:/user/Projetos/Projecoes/Preço ao produtor/PREÇO_AO_PRODUTOR.xlsx")

prec_prod_ICP<-ts(na.omit( DADOS$`Preço ao produtor ICP`[2:length(DADOS$`Preço ao produtor ICP`)]),start = c(2006,04),frequency = 12)

SPOT_ICP<-ts(DADOS$SPOT_ICP[-c(length(DADOS$SPOT_ICP))],start = c(2006,04),frequency = 12)

#TestArimax(par=par2,Z=prec_prod_ICP,xreg = SPOT_ICP)

fit<-Arima(prec_prod_ICP,xreg = SPOT_ICP,order = c(3,1,0),seasonal = list(order=c(1,1,1),frequency=12))
forecast<-forecast(fit,xreg =DADOS$SPOT_ICP[c(length(DADOS$SPOT_ICP))],h=3 )
forecast
#O codigo abaixo estava no outro arquivo (prec_ao_prod.R)
#fit<-Arima(prec_prod,order = c(1,0,2),seasonal = list(order=c(2,1,0),frequenc=12),xreg =spot_defasadoICP,lambda = "auto" )
#forecast(fit,xreg = PROD$SPOT_ICP[length(PROD$SPOT_ICP)],h=1)

pred<-as.numeric(forecast$mean)
write.csv2(pred,"pred.csv")
