install.packages('fpp3')
install.packages('readxl')
install.packages('TSA')
install.packages('forecast')
install.packages('rlang')
install.packages('astsa')
install.packages("patchwork")
install.packages('GGally')
install.packages('tseries')
install.packages('feasts')
install.packages("writexl")
install.packages("timetk")
#install.packages("digitize") #extrai dados de figuras 

library(fpp3)
library(readxl)
library(TSA)
library(forecast)
library(astsa)
library(GGally)
library(patchwork)
library(tseries)
library(slider)
library(feasts)
library(fabletools)
library(writexl)
library(timetk)
#library(digitize)

#----------------------------------------------------------------------------------------

setwd("Z:/user/Projetos/Projecoes/PTL/PTL-MG") # indica o local do computador onde estao os arquivos que serao trabalhados aqui.
dados<- read_excel('Z:/user/Projetos/Projecoes/PTL/PTL-MG/PROJECAO_PTL_MINAS.xlsx') # armazena informacoes do referido arquivo em 'dados'

#----------------------------------------------------------------------------------------

# NOTAS

# ctrl + shift + c [para (des)comentar]
# use ccf() para calcular correlacao com defasagens variadas de uma unica vez.
# argumento 'lag' indica o modulo da defasagem.



#----------------------------------------------------------------------------------------

# SOLICITACOES


# glauco solicitou fazer: ptl em funcao ptl -1, ptl -12 e preco ao prod -3, -4



# Criar variaveis------------------------------------------------------------------------

plp <- dados$`Preço ao produtor deflacionado icp`
ptl_mg<- dados$`PTL MG`
icp<- dados$ICP

ptl_mg_ts<-ts(ptl_mg, frequency = 1) # criacao de serie temporal
ptl_mg_ts # frequencia eh o numero de observacoes por mes.



# Analise inicial das variaveis----------------------------------------------------------


acf(ptl_mg_ts) # both trend and seasonal patterns
pacf(ptl_mg_ts) # nao entendi muito bem o que eh a parcial (tem relacao com media movel)

acf(plp) # both trend and seasonal
pacf(plp) 

acf(icp) # only trend
pacf(icp) 

# When data have a trend, the autocorrelations for small lags tend to be large
#and positive because observations nearby in time are also nearby in value. 
#So the ACF of a trended time series tends to have positive values that slowly
#decrease as the lags increase.

# When data are seasonal, the autocorrelations will be larger for the seasonal 
#lags (at multiples of the seasonal period) than for other lags.




# PLP x PTL MG --------------------------------------------------------------------------

# ccf(x,y), with 'y' being the predicted variable
# lag term is for the x-variable

grafico1<-ccf(plp, ptl_mg, type = "correlation", 
                   main = "Correlacao para Preço ao Produtor x PTL-MG") 

# CCF is affected by the time series structure of the x-variable and any 
#"in common" trends the x and y series may have over time.

grafico1 # valor negativo indica que preco ao prod fica atras do ptl-mg


#prewhitening eliminates autocorrelation bias of x

grafico1_pw<-TSA::prewhiten(plp,ptl_mg, main = 'Correlações com Defasagem Preço ao Produtor x PTL-MG')

grafico1_pw #lag -3


# ICP X PLP MG --------------------------------------------------------------------------

grafico2<- ccf(icp, ptl_mg, type = 'correlation',
                    main = 'Correlacao para ICP x PTL-MG')
grafico2

#prewhitening eliminates autocorrelation bias of x
grafico2_pw<-TSA::prewhiten(icp,ptl_mg, main = 'Correlações com Defasagem ICP x PTL-MG')
grafico2_pw #lag - 3 ou -9


# TSIBBLE -------------------------------------------------------------------------------

# Creating a tsibble object for PTL

# O tsibble eh uma estrutura moderna para armazenar e manipular series temporais.

ptl_mg_t <- tsibble(
  Month = yearmonth(dados$Data),
  PTLMG = dados$`PTL MG`,
  ICP = dados$ICP,
  plp = dados$`Preço ao produtor deflacionado icp`,
  index = Month
)
ptl_mg_t

# modificando os valores de PTLMG para ficarem em milhares de reais.
ptl_mg_t<- ptl_mg_t %>% mutate(PTLMG = PTLMG/1000)
ptl_mg_t


# FUNCOES UTEIS ------------------------------------------------------------------------

#try some dplyr objects: mutate(), filter(), select, summarise()

ptl_mg_t %>% filter(plp < 2) # use filter() for keeping only chosen rows

ptl_mg_t %>% filter(plp < 2) %>% select(Month) # use select() for keeping only chosen columns

ptl_mg_t %>% select(plp) # Index variable (time period) is always returned anyways

# use summarise() for modifying columns values
# use mutate() for creating new columns

ptl_mg_t %>% filter(plp < 2) -> new_t # for creating a new tsibble after piped functions
new_t #tsibble com valores plp < 2
remove(new_t)

# as_tsibble(key = c(),  index = )
# use the line of code above for transform csv into tsibble object.
# 'key' stands for the define columns
# 'index' is for time period associated to the time series
# the variable(s) we want to predict are neither key nor index.



# PLOTS --------------------------------------------------------------------------------

#install.packages("patchwork")
#library(patchwork)

# autoplot(tsibble_name, tsibble variable (if not specified, it's gonna return 'y' ))

p1<-autoplot(ptl_mg_t)
p2<-autoplot(ptl_mg_t, plp)
p3<-autoplot(ptl_mg_t, ICP)

p1+p2+p3 


# separated mini time plots for ptl_mg_t

ptl_mg_t %>%
  gg_subseries(PTLMG) + 
ptl_mg_t %>% gg_subseries(ICP) +
ptl_mg_t %>% gg_subseries(plp) +
ptl_mg_t %>% gg_subseries(plp)
# blue horizontal line indicates mean
  
# Use 'period' argument for selecting which seasonal pattern you want to display on graph

ptl_mg_t %>% gg_season(PTLMG, period = "year") + 
  labs(title="Yearly seazonal pattern for PTL MG")



#----------------------------------------------------------------------------------------------------
# Abaixo: graficos de dispersao (scatterplots) de PTL-MG x sua versao defasada 
# + correlacoes

lag1.plot(ptl_mg, 24, corr = TRUE)


# ICP X PTL --------------------------------------------------------------------

lag2.plot(icp, ptl_mg, max.lag = 12, corr = TRUE, smooth = TRUE)
#default argument for smooth is true


# PLP X PTL --------------------------------------------------------------------

lag2.plot(plp,ptl_mg,max.lag = 12, corr = TRUE, smooth = TRUE)

#use period() argument for selecting which seasonal plot is required



# Scatterplot matrix --------------------------------------------------------------------------------

# To see the relationships between time series, we can plot 
# each one of them against the others. 

# This requires GGally package.

ptl_mg_t %>%
  pivot_wider(values_from= ptl_mg_t, names_from= as.character(ptl_mg_t$Month)) %>%
  GGally::ggpairs(columns = 2:9)

# didn't figure out why code above doesn't work! (book fpp3 - 1 - 1.6)

#---------------------------------------------------------------------------------------------------

# 17 de Novembro de 2022

# fpp3 book
# 3.2 Time Series Components

# Additive x Multiplicative Decomposition ----------------------------------------------------------

# The additive decomposition is the most appropriate if the magnitude of the seasonal fluctuations,
# or the variation around the trend-cycle, does not vary with the level of the time series.

p1
ptl_mg_t

decompose <- ptl_mg_t %>%
  model(stl = STL(PTLMG))

components(decompose)
# The output above shows the components of an STL decomposition.

components(decompose) %>%
  as_tsibble() %>%
  autoplot(PTLMG, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00")

components(decompose) %>% autoplot()
# The remainder component shown in the bottom panel is what is left over when the seasonal and 
#trend-cycle components have been subtracted from the data.

# Each grey bar represents the same length but because the plots are on different scales, the bars vary in size.
# The large grey bar in the bottom panel shows that the variation in the remainder component is smallest compared 
#to the variation in the data.

# If we shrank the bottom three panels until their bars became the same size as that in the data panel, then all the 
#panels would be on the same scale.
# Note:  But how to do the above?



# Seasonally adjusted data --------------------------------------------------------------------------

components(decompose) %>%
  as_tsibble() %>%
  autoplot(PTLMG, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") 

# We call seasonally adjusted the data without seasonal component, which is given by  
# (yt-St) for additive decomposed data and (yt/St) for multiplicative ones.

# If the variation due to seasonality is not of primary interest, the seasonally adjusted series can be useful. 
# Note: Not our case.

# Note: On the last plot, it seems like it have seasonality even if it was seasonally adjusted. 
# It also appears to have a cycle (?).

#-------------------------------------------------------------------------------

# 4.2 ACF Features

ptl_mg_t %>% features(PTLMG, feat_acf)

# 1. the first autocorrelation coefficient from the original data;
# 2. the sum of squares of the first ten autocorrelation coefficients from the original data;
# 3. the first autocorrelation coefficient from the differenced data;
# 4. the sum of squares of the first ten autocorrelation coefficients from the differenced data;
# 5. the first autocorrelation coefficient from the twice differenced data;
# 6. the sum of squares of the first ten autocorrelation coefficients from the twice differenced data;
# OBS: for seasonal data, the autocorrelation coefficient at the first seasonal lag is also returned.


# 4.3 STL features

ptl_mg_t %>% features(PTLMG, feat_stl)


# 4.4 Other Features

#install.packages('feasts')
#library('feasts')

ptl_mg_t %>% features(PTLMG, feat_spectral) # A series which has strong trend and seasonality
#(and so is easy to forecast) will have entropy close to 0. 
# A series that is very noisy (and so is difficult to forecast) will have entropy close to 1.
# Note: a nossa ficou no meio termo, ora bolas!



#-------------------------------------------------------------------------------

# ARIMA modelling in {fable}

# Series needs to be stationary for applying ARIMA
# Tests of stationarity

# Is PTLMG stationary -----> Use KPSS test (null hypothesis is stationary)

kpss.test(ptl_mg_t$PTLMG) # p-value < 0.05 ---> reject null hypothesis ---> series is non-stationary.
ptl_mg_t %>% features(PTLMG, unitroot_kpss) # does the same test of the line of code above.

# Apply differencing methods (try it using 'transmute' and creating columns in ptl_mg_t for seasonal and simple differentiation next time)
# Differencing has nothing to do with derivatives. It is the opposite of sum in this context.   



# SIMPLE DIFFERENCING ----------------------------------------------------------

ptl_mg_t<-ptl_mg_t %>% mutate(PTLMG_diff = difference(PTLMG,1)) # simple diferentiation (for new column)
# diferenca entre cada termo e seu antecessor. nao havera valor correspondente para o primeiro termo.

P1 <- ptl_mg_t %>% autoplot(PTLMG_diff) # fev 2023: uma diff parece ser suficiente.
P1


ptl_mg_t %>% ACF(PTLMG_diff)%>% autoplot() #The right order of differencing is the minimum differencing required
#to get a near-stationary series which roams around a defined mean and the ACF plot reaches to zero fairly quick.

ptl_mg_t %>% PACF(PTLMG_diff)%>% autoplot()

ptl_mg_t %>% features(PTLMG,feat_acf) # ??

# In the event you can't really decide between two orders of differencing, then go with the order that gives the least standard deviation in the differenced series.



# SEASONAL DIFFERENCING --------------------------------------------------------

ptl_mg_t<-ptl_mg_t %>% mutate(PTLMG_diff = difference(PTLMG_diff, 12)) # adding seasonally differentiation to PTLMG_diff
# coluna com as diferencas entre cada termo e seu correspondente do ano antecessor.
# valores para o primeiro ano nao serao computados (pois ele nao possui antecessor).
ptl_mg_t %>% ACF(PTLMG_diff) %>% autoplot() ->a
ptl_mg_t %>% PACF(PTLMG_diff) %>% autoplot()-> b

a + b

# abaixo: verificacao de lag plots com correlacoes para PTLMG_diff

aux<-as_tibble(ptl_mg_t)

lag1.plot(aux$PTLMG_diff[14:189],10, corr=TRUE, na.omit=TRUE)

# nao seu por que codigo abaixo nao funciona (faz o mesmo que o acima, soh que
#manipulando diretamente no tsibble object).
# ptl_mg_t %>% filter(PTLMG_diff>13) %>% lag1.plot(PTLMG_diff,10,corr=TRUE)
# ptl_mg_t %>% filter(PTLMG_diff>13) %>% select(PTLMG_diff)

# -----------------------------------------------------------------------------------------------------------------------------------


# Vamos checar novamente a suposicao de estacionariedade da serie apos as diferenciacoes?

adf.test(na.omit(aux$PTLMG_diff)) # Augmented Dickey Fuller test

# hipotese nula rejeitada ---> a serie ainda eh estacionaria!
# bora fazer mais transformacoes!

kpss.test(ptl_mg_t$PTLMG_diff) # KPSS test

# p-value is greater than 0,05 ---> don't reject null hypothesis ---> series is stationary
# conclusao: bora fazer mais transformacoes p tornar a serie em nao estacionaria!


# Verificacao dos paramteros de Estacionariedade -------------------------------


# there is a function which computes the necessary number of first differences: unitroot_ndiffs

ptl_mg_t %>% features(PTLMG, unitroot_ndiffs)
ptl_mg_t %>% features(PTLMG_diff, unitroot_ndiffs)

# a similar feature for determining whether seasonal differencing is required is: unitroot_nsdiffs()

ptl_mg_t %>% features(PTLMG, unitroot_nsdiffs)
ptl_mg_t %>% features(PTLMG_diff, unitroot_nsdiffs)



# AR ---------------------------------------------------------------------------

ptl_mg_t %>% PACF(PTLMG_diff) %>% autoplot() #Any autocorrelation in a stationarized series can be rectified by adding enough AR terms. 

#So, we initially take the order of AR term to be equal to as many lags that crosses the significance limit in the PACF plot.



# MA ---------------------------------------------------------------------------

ptl_mg_t %>% ACF(PTLMG_diff) %>% autoplot() 

#Just like how we looked at the PACF plot for the number of AR terms, you can look at the ACF plot for the number of MA terms. 



# ---------------------------------------------------------------------------------------------------------------------------------------------------------------

# Comparacao de graficos plot, ACF e PACF para PTLMG (serie original) e PTLMG_diff 
# (serie diferenciada 2 vezes)

plot1<-ptl_mg_t %>% autoplot(PTLMG)
plot2<-ptl_mg_t %>% autoplot(PTLMG_diff) 

plot3<-ptl_mg_t%>%ACF(PTLMG)%>%autoplot()
plot4<-ptl_mg_t%>%ACF(PTLMG_diff)%>%autoplot() 

plot5<-ptl_mg_t%>%PACF(PTLMG)%>%autoplot()
plot6<-ptl_mg_t%>%PACF(PTLMG_diff)%>%autoplot()

plot1 + plot2 + plot3 + plot4 + plot5 + plot6


# AUTOMATIC ARIMA --------------------------------------------------------------

# Serie de testes de tudo quanto eh tipo com ARIMA

# Applying automatic ARIMA

fit <- ptl_mg_t %>%  model(ARIMA(PTLMG_diff))
report(fit)

# Determining which p, d, q parameters are the best for the model.

# autocorrelation plot
acfplot<-ptl_mg_t %>% ACF(PTLMG_diff) %>% autoplot()
acfplot

# partial autocorrelation plot
pacfplot<-ptl_mg_t %>% PACF(PTLMG_diff) %>% autoplot()
pacfplot

acfplot+pacfplot

# a convenient way to produce a time plot, ACF plot and PACF plot in one command is to use the gg_tsdisplay() function with plot_type = "partial".

ptl_mg_t %>% gg_tsdisplay(PTLMG_diff, plot_type = "partial")

# estimating moving average (recommend for monthly data with annual seasonality = 2 x 12 MA)

# We can also specify particular values of pdq() that ARIMA() can search for (fpp3 9.5)

ptl_mg_t %>% model(ARIMA(PTLMG))
ptl_mg_t %>% model(ARIMA(PTLMG_diff ~ pdq(p=1:12, d=0:4, q=0:5)))

#-------------------------------------------------------------------------------

# Testando o modelo ARIMA(1,0,0)(2,0,0)

# drop_na.tbl_ts <- function(ts) tsibble::as_tsibble(tidyr:::drop_na.data.frame(ts)) # for making drop_na works in a tsibble
# ptl_mg_t<-drop_na(ptl_mg_t) # removing NA rows from ptl_mg_t


ptlmg_fit = ptl_mg_t %>% model(arima = ARIMA(PTLMG ~ pdq(1,1,1)+ PDQ(1,1,1), period = 12, stepwise = FALSE, approx = FALSE))
ptlmg_fit


--------------------------------------------------------------------------------

# AAAAAAAAAAAAAAAAAAAAAAAA  
  
ptlmgfc = ptlmg_fit %>% forecast(h=3) # nao funciona aaaaa
ptlmgfc


ptlmgfc %>% autoplot(ptlmg_fit)




# ARIMA(1,1,1)(1,1,1)[12]-------------------------------------------------------

fit<-Arima(ptl_mg_ts, order = c(1,1,1), seasonal = c(1,1,1))
fit_fc<-forecast(fit, 3, level = 99.5)
fit_fc
plot(fit_fc)


# o codigo abaixo faz o mesmo que o acima so que inclui casa decimal na saidas 


df<-ptl_mg_t %>%select(PTLMG)
df<-as.data.frame(df)
df

df_ts<-ts(df$PTLMG, frequency = 1)
df_ts

modelo<-Arima(df_ts, c(1,1,1), c(1,1,1))

forecast(modelo, h = 3 )


# TESTE RESIDUOS 

augment(ptlmg_fit) %>%
  features(.resid, ljung_box) # p value greater than 0.05 ---> don't reject null hypothesis (residuals are independent and not correlated)

ptlmg_fit %>%
  gg_tsresiduals()

#-------------------------------------------------------------------------------

# 01/ 02/ 2023 # Alguns testes para se verificar a viabilidade de replicar o codigo
# do joao pedro para ptl_br em ptl_minas.

# escolhe-se a melhor defasagem para ptl_mg em funcao de plp
lag2.plot(plp, ptl_mg, max.lag = 12, corr = TRUE)
# lags 4, 3, 1

# escolhe-se a melhor defasagem para ptl_mg em funcao de plp
lag2.plot(icp, ptl_mg, max.lag = 12, corr = TRUE)
# lag 3 

# melhor defasagem de ptl em função de si própria
lag1.plot(ptl_mg, max.lag = 12, corr = TRUE)
# lag 12


# FEV 2023 ---------------------------------------------------------------------
# LIVRO MORETTIN

#desvio padrao ou variancia de PTL-MG eh proporcional a media?

var(ptl_mg)
mean(ptl_mg)

# qual transformacao eh mais adequada? logaritmica ou box-cox?

length(ptl_mg)

vet_mean<-c(rep(0,600))
vet_range<-c(rep(0,600))
cont<-1

while (cont<=600){
  
  subsample<-sample(ptl_mg,20, replace = TRUE)
  
  vet_mean[cont]<-mean(subsample)
  vet_range[cont]<-max(subsample)-min(subsample)
  cont<-cont+1
  
}
vet_mean
vet_range
  
plot(vet_mean,vet_range) #dados me parecem espalhados 

# testar com dados transformados logaritmicamente

ptl_mg_log<-log(ptl_mg)

vet_mean<-c(rep(0,600))
vet_range<-c(rep(0,600))
cont<-1

while (cont<=600){
  
  subsample<-sample(ptl_mg_log,20, replace = TRUE)
  
  vet_mean[cont]<-mean(subsample)
  vet_range[cont]<-max(subsample)-min(subsample)
  cont<-cont+1
  
}
vet_mean
vet_range

plot(vet_mean,vet_range)

par(mfrow=c(2,2))

hist(ptl_mg_log) 
hist(ptl_mg)

# box-cox transformation

ptl_mg_bc<-box_cox_vec(ptl_mg, lambda = "auto", silent = FALSE)





