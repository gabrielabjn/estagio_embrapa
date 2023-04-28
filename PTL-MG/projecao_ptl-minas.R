### Modelo de Previsao do Preco Trimestral do Leite (PTL)
### Minas Gerais

### Carregar bibliotecas necessárias
library(readxl)
library(forecast)

### Definir o diretório

setwd("Z:/user/Projetos/Projecoes/PTL/PTL-MG")

######## Mantenha os dados do ICP, preço ao produtor deflacionado e da PTL-MG 
######## da planilha PROJEÇÃO PTL MINAS
######## atualizados 

dados<- read_excel('Z:/user/Projetos/Projecoes/PTL/PTL-MG/PROJECAO_PTL_MINAS.xlsx')

ptl_mg<- dados$`PTL MG`

ptl_mg_ts<-ts(ptl_mg, frequency = 1) # criacao de serie temporal
ptl_mg_ts # frequencia eh o numero de observacoes por mes.

# ARIMA(1,1,1)(1,1,1)[12]
# erro de 15%

fit<-Arima(ptl_mg_ts, order = c(1,1,1), seasonal = c(1,1,1))
fit_fc<-forecast(fit, 3, level = 99.5)
plot(fit_fc)
fit_fc


# ARIMA(1,0,1)(2,1,2)[12]
# erro de 10%

fit2<-Arima(ptl_mg_ts, order = c(1,0,1), seasonal = c(2,1,2))
fit_fc2<-forecast(fit, 3, level = 99.5)
plot(fit_fc2)
fit_fc2


# Let's create a data frame for registering both predictions.

previsao1<-c(fit_fc$mean)
previsao2<-c(fit_fc2$mean)

df<-data.frame(previsao1,previsao2)
colnames(df)<-c("Arima(1,1,1)(1,1,1)[12]","Arima(1,0,1)(2,1,2)")

write.csv2(df,"prev_ptl-mg.csv")
