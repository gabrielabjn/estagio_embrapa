# Instale e carregue o pacote "readxl"
library(readxl) # ler arquivo excel (banco de dados)
library(fpp3) # criar objeto tsibble 
library(forecast) # fazer previsao

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

# presenca de tendencia: as autocorrelacoes p pequenos lags tendem a ser grandes
# e positivas 
# presenca de sazonalidade: as autocorrelacoes serao maiores em lags multiplos 
# da frequencia sazonal


# Se as barras no gráfico ACF diminuem gradualmente em direção a zero e, em seguida, ficam constantes, isso pode indicar que um modelo ARMA (autorregressivo de média móvel) seria apropriado. A ordem ARMA pode ser escolhida com base no número de lags em que a correlação é significativa e não diminui para zero.
#4

# A ordem do componente de média móvel pode ser determinada pelo número de picos significativos no gráfico ACF. Se houver um pico significativo no gráfico ACF na defasagem q, o parâmetro q pode ser definido como q = número de defasagens até o primeiro zero no gráfico ACF.
# 2

# Verificar se os dados sao estacionarios

# # Decomposicao STL -------------------------------------------------------------
# 
# # Robusta p outliers (se desejavel)
# 
# dados_t %>%
#   model(STL(plp)) %>%
#   components() %>% autoplot()
# 
# dados_t %>%
#   model(STL(plp)) %>%
#   components() 
# 
# dados_stl<-dados_t %>%
#   model(STL(plp)) %>%
#   components() 
# 
# 
# # Ajuste Sazonal
# 
# dados_t %>% 
#   autoplot(plp, col = "gray") +
#   autolayer(dados_stl, season_adjust, color = "blue")
# 
# 
# # Forca da tendencia e da sazonalidade
# 
# dados_t %>%
#   features(plp, feat_stl)
# 
# # Ajustes de modelos de benchmark (ver slides da Paula Macaira)
# 
# dados_fit = dados_t %>%
#   model(Seasonal_naive = SNAIVE(plp),
#         Naive = NAIVE(plp),
#         Drift = RW(plp ~ drift()),
#         Mean = MEAN(plp))
# dados_fit # mable eh uma tabela com os modelos ajustados
# 
# dados_fc = dados_fit %>%
#   forecast(h = 1)
# dados_fc # fable eh uma tabela de previsao com distribuicoes
# 
# 
# # Plotando as previsoes
# 
# dados_fc %>%
#   autoplot(dados_t, level = NULL)



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

# Lembre-se que a seleção dos parâmetros não é uma tarefa simples e depende das 
#características da série temporal e do objetivo da análise. É importante avaliar 
#diferentes modelos e compará-los com métricas de desempenho, como o erro médio 
#absoluto (MAE) e o erro quadrático médio (MSE), para escolher o melhor modelo para a previsão.

# Ajustar modelo Arima ---------------------------------------------------------

plp_arima = dados_t %>% model(arima = ARIMA(plp))
plp_arima

# Olhando o modelo ajustado com mais detalhes ----------------------------------

report(plp_arima)

# Realizando a previsao e plotando ---------------------------------------------

plp_fc = plp_arima %>% forecast(h = 12)
plp_fc
plp_fc %>% autoplot(dados_t)

# Quero prever para 2022 -------------------------------------------------------

train = dados[-c(205:218),]
plp_ts = as.ts(dados_ate_2021$plp)

plp_fc2<-arima(plp_ts, order = c(2,1,3))

fc<-forecast(plp_fc2, h = 12)
fc
forecast(plp_fc2, h = 12) %>% autoplot()

write.csv2(fc, file = 'arima(2,1,3).csv')

# Auto.arima com x reg ---------------------------------------------------------

fit <- auto.arima(train$plp, xreg = cbind(train$uht, train$spot, train$leite_po, train$mucarela), seasonal = 12)

fc2 <- forecast(fit, h = 12, xreg = cbind(dados$uht[205:216], dados$spot[205:216], dados$leite_po[205:216], dados$mucarela[205:216]))
fc2

write.csv2(fc2, file = 'arima(2,1,2).csv')













  
