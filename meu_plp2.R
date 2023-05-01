# Instale e carregue o pacote "readxl"
library(readxl) # ler arquivo excel (banco de dados)
library(fpp3) # criar objeto tsibble 
library(forecast) # fazer previsao
library(base) # para realizar diferenciacoes

# Defina o diretorio
setwd("C:/Users/55229/Documents/embrapa/series temporais")

# Importe os dados do Excel
dados <- read_excel("parametros_leite_gabriela - oficial.xlsx", range = cell_rows(2:218))

# Verifique se há valores ausentes nos dados
sum(is.na(dados))

# Selecionando apenas as colunas com plp e va's que quero usar como preditoras
dados <- select(dados, meses, `preço ao produtor (R$/L)...18`, `UHT - cepea (R$/L)...21`, 
                          `Leite em pó - cepea (R$/L)...22`, `Queijo Muçarela (R$/kg)...27`,
                          `Leite Cru (Spot)...30`)

summary(dados)

# Renomeando nomes das colunas

colnames(dados) <- c('meses','plp','uht', 'leite_po', 'mucarela', 'spot')
summary(dados)

# Alterando formato do index para ano/mes
dados<-dados%>% mutate(meses = yearmonth(meses))

# Criando estrutura tsibble (mais moderna aparentemente)

dados_t<-dados %>% as_tsibble(index=meses)

# Alguns graficos 

dados_t %>%
  autoplot(plp) +
  labs(x="Data", y="Valor em reais (R$)",
       title="Preco do leite ao produtor (Abr-2005 a Dez-2022)") 


# nao eh estacionario na media nem na variancia
# apresenta tendencia de crescimento e sazonalidade
# essas caracteristicas podem ser visualizada tambem em um grafico acf

# Funcao de Autocorrelacao

dados_t %>% ACF(plp, lag_max = 12)

# Grafico de Autocorrelacao

dados_t %>% ACF(plp, lag_max = 50) %>% autoplot()


# Diferenciacao de plp + graficos + analises

dados_t <- dados_t %>% mutate(plp, diff = difference(plp))
dados_t %>% autoplot(diff)

dados_t %>% ACF(diff, lag_max = 50) %>% autoplot() 
dados_t %>% ACF(diff, lag_max = 50) %>% autoplot() 

# presenca de tendencia: as autocorrelacoes p pequenos lags tendem a ser grandes
# e positivas 
# presenca de sazonalidade: as autocorrelacoes serao maiores em lags multiplos 
# da frequencia sazonal


# Se as barras no gráfico ACF diminuem gradualmente em direção a zero e, em seguida, ficam constantes, isso pode indicar que um modelo ARMA (autorregressivo de média móvel) seria apropriado. A ordem ARMA pode ser escolhida com base no número de lags em que a correlação é significativa e não diminui para zero.
#4

# A ordem do componente de média móvel pode ser determinada pelo número de picos significativos no gráfico ACF. Se houver um pico significativo no gráfico ACF na defasagem q, o parâmetro q pode ser definido como q = número de defasagens até o primeiro zero no gráfico ACF.
# 2

# Verificar se os dados sao estacionarios


# Residuos ---------------------------------------------------------------------

# Premissas: nao sao correlacionados. Caso sejam, ficaram informacoes nos residuos
# que deveriam estar no modelo.

# possuem media 0, caso nao seja entao as previsoes estao viesadas 

# augment(dados_fit)
# 
# augment(dados_fit) %>%
#   filter(.model == "Drift") %>%
#   autoplot(.resid)
# 
# augment(dados_fit) %>%
#   filter(.model == "Drift") %>%
#   ACF(.resid) %>%
#   autoplot()

# nao queremos rejeitar h0 : os residuos sao iid 

augment(dados_fit) %>%
  features(.resid, ljung_box) # p valor < 0.05 ---> rejeitar h0, descartar o modelo

# ------------------------------------------------------------------------------
# Ajustar modelo Arima ---------------------------------------------------------

plp_arima = dados_t %>% model(arima = ARIMA(plp))
plp_arima

# Olhando o modelo ajustado com mais detalhes ----------------------------------

report(plp_arima)

# Ajustar considerando dados mais recentes -------------------------------------

model = dados[-c(205:218),] # dados de treinamento (ate dez 2021)
plp_ts = as.ts(model$plp)

# Ajuste automatico do modelo (considerando dados ate dez - 2022) 
auto.arima(dados$plp, xreg = cbind(dados$uht, dados$spot, dados$leite_po, dados$mucarela), seasonal = 12)
fit #(0,1,0)

# Ajuste manual do modelo

fit<-Arima(as.ts(model$plp), order = c(0,1,0), xreg = cbind(model$uht, model$spot,model$leite_po, model$mucarela))

fit

a<-cbind(model$uht, model$spot,model$leite_po, model$mucarela)
ncol(a)

b<-cbind(dados$uht[205:216], dados$spot[205:216], dados$leite_po[205:216], dados$mucarela[205:216])
ncol(b)

# Previsao 

FC<-forecast(fit, xreg = cbind(dados$uht[205:216], dados$spot[205:216], dados$leite_po[205:216], dados$mucarela[205:216]))

write.csv2(FC, file = 'arima(0,1,0).csv')

# ---------------------------------------


fit<-Arima(as.ts(dados$plp), order = c(0,1,0), xreg = cbind(dados$uht, dados$spot, dados$leite_po, dados$mucarela))

FC<-forecast(fit, xreg = cbind(4.1820, 2.8470, 30.8124, 31.1744))
FC

