setwd(Z:/user/Projetos/Projecoes/Preço ao produtor")
library(readxl)
library('forecast')
PROD<- read_excel("PREÇO_AO_PRODUTOR.xlsx")
prec_prod<-PROD$`Preço ao produtor ICP`[-1]
spot_defasadoICP<-PROD$SPOT_ICP[-length(PROD$SPOT_ICP)]

##spot_defasado<-PROD$SPOTQUINZENAL_BR[-length(PROD$SPOTQUINZENAL_BR)]
##TestArimax(par2,Z=prec_prod,xreg = spot_defasadoICP)
##TestArimax(par2,Z=prec_prod,xreg = spot_defasadoICP,seasonal = NULL)
##TestArimax(par2,Z=prec_prod,xreg = spot_defasadoICP,seasonal = list(order=c(2,1,0),frequenc=12))
##TestArimax(par2,Z=prec_prod,xreg = spot_defasado)
##TestArimax(par2,Z=prec_prod,xreg = spot_defasado,seasonal = NULL)
##TestArimax(par2,Z=prec_prod,xreg = spot_defasado,seasonal = list(order=c(2,1,0),frequenc=12))


#codigo por joao pedro

fit<-Arima(prec_prod,order = c(1,0,2),seasonal = list(order=c(2,1,0),frequenc=12),xreg =spot_defasadoICP,lambda = "auto" )
  forecast(fit,xreg = PROD$SPOT_ICP[length(PROD$SPOT_ICP)],h=1)

#adicionado por gabriela (abaixo)
  
pred<-as.numeric(forecast$mean)
write.csv2(pred,"pred.csv")  

