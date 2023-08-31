library(readxl)
library(forecast)

setwd("Z:/user/Projetos/Projecoes/Preço ao produtor")
dados <- read_excel("Z:/user/Projetos/Projecoes/Preço ao produtor/PREÇO_AO_PRODUTOR.xlsx")

names(dados)[1]<-"data"
dados$data<-as.Date(dados$data, format = "%Y-%m-%d")

spot_ts<-ts(dados$SPOT_ICP[1:198], start = dados$data[1],freq = 12) #199 // treino
plp_ts<- ts(dados$`Preço ao produtor ICP`[1:198],start = dados$data[1], freq = 12) #199 // treino


# autoarima --------------------------------------------------------------------

modelo<-auto.arima(plp_ts, xreg=spot_ts)
modelo

forecast(modelo, xreg=dados$SPOT_ICP[198])

# residuos

res<-modelo$residuals

par(mfrow = c(2,2))
qqnorm(res)
qqline(res, col = 'red', lwd = 1.5)

ks.test(res, 'pnorm')



# arima(1,1,0) -----------------------------------------------------------------

modelo2<-Arima(plp_ts, xreg=spot_ts, order= c(1,1,0))

forecast(modelo2, xreg=dados$SPOT_ICP[197])

# residuos

res2<-modelo2$residuals

qqnorm(res2)
qqline(res2, col = 'red', lwd = 1.5)

ks.test(res2, 'pnorm')


# Como aplicar diff sazonal ----------------------------------------------------

dados_diff_s <- diff(plp_ts, lag = 12)
acf(dados_diff_s)

pacf(dados_diff_s)

(64+67)/2
