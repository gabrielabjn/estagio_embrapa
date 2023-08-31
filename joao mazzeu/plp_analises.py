# Bibliotecas Necessarias -----------------------------------------------------

from pandas import read_excel
from matplotlib import pyplot
from numpy import log

import os

import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import plot_pacf

# Obtém o diretório atual -----------------------------------------------------
os.getcwd()

# Define o novo diretório de trabalho
os.chdir('Z:/user/Projetos/Papers/Correlacoes_Gabriela/previsao_plp')

# Verifica o novo diretório de trabalho
os.getcwd()

# criar variavel com nossa serie ----------------------------------------------
series = pd.read_excel('dados_prev_plp.xlsx', header = 0, index_col = 0,parse_dates=True).squeeze("columns") 
# Obs: o argumento squeeze = True em conjunto com header = 0, faz com que o data 
# frame seja lido como series (cada coluna, uma serie)

# Alterar formato de data para mes/ano
series.index = pd.to_datetime(series.index).strftime('%m-%Y')

# verificar classe (tipo) da variavel 'series'
type(series) # data frames pandas

# Grafico de 'series'
series.plot()

# Obtendo a referência para o objeto da legenda
legenda = plt.legend()

# Definindo as coordenadas da legenda
x_coord = 1.0  # Coordenada x
y_coord = 0.5  # Coordenada y

# Definindo a posição da legenda fora do gráfico
legenda.set_bbox_to_anchor((x_coord, y_coord))


# Grafico Sazonal
series.plot(subplots=True, figsize=(10, 8))

# LogTransform ----------------------------------------------------------------
log_series=log(series)

# Grafico de 'log_series'
log_series.plot()

# Obtendo a referência para o objeto da legenda
legenda = plt.legend()

# Definindo as coordenadas da legenda
x_coord = 1.0  # Coordenada x
y_coord = 0.5  # Coordenada y


# Definindo a posição da legenda fora do gráfico
legenda.set_bbox_to_anchor((x_coord, y_coord))


# Analise da variavel resposta ------------------------------------------------

preco_prod=series.iloc[:,0] # salvando coluna referente ao preco produtor

media = preco_prod.mean() # salvando media
round(media, 2) # printando media

mediana = preco_prod.median() # salvando mediana
round(mediana, 2) # printando mediana

desvio = preco_prod.std() # salvando desvio padrao
round(desvio,2) # printando desvio padrao 
   
minimo = preco_prod.min() # salvar minimo
round(minimo,2) # printando minimo

maximo = preco_prod.max() # salvar maximo
round(maximo,2) # printando maximo

# Histograma 
plt.hist(preco_prod)
# Adicione rótulos e título ao gráfico
plt.xlabel('Preco ao Produtor')
plt.ylabel('Frequência')
plt.title('Histograma Preco ao Produtor')

# Regressao Linear Simples ----------------------------------------------------

# UHT x Preco ao Produtor -----------------------------------------------------

# Adicionar uma constante aos dados de entrada
# A biblioteca statsmodel exige cte para estimar intercepcao
b = sm.add_constant(series.iloc[:,1])

# Criar o modelo de regressão linear
uht_model = sm.OLS(series.iloc[:,0], series.iloc[:,1])

# Ajustar o modelo aos dados
resultado = uht_model.fit()

# Obter os coeficientes (inclinação e interceptação)
coef_inclinacao_uht = resultado.params['UHT - cepea (R$/L)']
#coef_interceptacao_uht = resultado.params['const']

# Imprimir resumo do modelo
print(resultado.summary())

# Plotar os dados originais e a linha de regressão
plt.scatter(series.iloc[:,1], series.iloc[:,0], color='blue', label='Dados Originais')
plt.plot(series.iloc[:,1], resultado.fittedvalues, color='red', label='Regressão Linear')
# Adicionar rótulos e título ao gráfico
plt.xlabel('uht')
plt.ylabel('preco ao produtor')
plt.title('UHT X Preco ao Produtor')


# Leite em Pó x Preco ao Produtor ---------------------------------------------

# Adicionar uma constante aos dados de entrada
# A biblioteca statsmodel exige cte para estimar intercepcao
b = sm.add_constant(series.iloc[:,2])

# Criar o modelo de regressão linear
po_model = sm.OLS(series.iloc[:,0], series.iloc[:,2])

# Ajustar o modelo aos dados
resultado = po_model.fit()

# Obter os coeficientes (inclinação e interceptação)
coef_inclinacao_po = resultado.params['Leite em pó - cepea (R$/L)']
#coef_interceptacao_uht = resultado.params['const']

# Imprimir resumo do modelo
print(resultado.summary())

# Plotar os dados originais e a linha de regressão
plt.scatter(series.iloc[:,2], series.iloc[:,0], color='blue', label='Dados Originais')
plt.plot(series.iloc[:,2], resultado.fittedvalues, color='red', label='Regressão Linear')
# Adicionar rótulos e título ao gráfico
plt.xlabel('leite em po')
plt.ylabel('preco ao produtor')
plt.title('Leite Po X Preco ao Produtor')



# Mucarela x Preco ao Produtor ------------------------------------------------

# Adicionar uma constante aos dados de entrada
# A biblioteca statsmodel exige cte para estimar intercepcao
b = sm.add_constant(series.iloc[:,3])

# Criar o modelo de regressão linear
mu_model = sm.OLS(series.iloc[:,0], series.iloc[:,3])

# Ajustar o modelo aos dados
resultado = mu_model.fit()

# Obter os coeficientes (inclinação e interceptação)
coef_inclinacao_mu = resultado.params['Queijo Muçarela (R$/kg)']
#coef_interceptacao_uht = resultado.params['const']

# Imprimir resumo do modelo
print(resultado.summary())

# Plotar os dados originais e a linha de regressão
plt.scatter(series.iloc[:,3], series.iloc[:,0], color='blue', label='Dados Originais')
plt.plot(series.iloc[:,3], resultado.fittedvalues, color='red', label='Regressão Linear')
# Adicionar rótulos e título ao gráfico
plt.xlabel('leite em po')
plt.ylabel('preco ao produtor')
plt.title('Mucarela X Preco ao Produtor')



# Spot x Preco ao Produtor ----------------------------------------------------

# Adicionar uma constante aos dados de entrada
# A biblioteca statsmodel exige cte para estimar intercepcao
b = sm.add_constant(series.iloc[:,4])

# Criar o modelo de regressão linear
spot_model = sm.OLS(series.iloc[:,0], series.iloc[:,4])

# Ajustar o modelo aos dados
resultado = spot_model.fit()

# Obter os coeficientes (inclinação e interceptação)
coef_inclinacao_spot = resultado.params['Leite Cru (Spot)']
#coef_interceptacao_spot = resultado.params['const']

# Imprimir resumo do modelo
print(resultado.summary())

# Plotar os dados originais e a linha de regressão
plt.scatter(series.iloc[:,4], series.iloc[:,0], color='blue', label='Dados Originais')
plt.plot(series.iloc[:,4], resultado.fittedvalues, color='red', label='Regressão Linear')
# Adicionar rótulos e título ao gráfico
plt.xlabel('spot')
plt.ylabel('preco ao produtor')
plt.title('Spot X Preco ao Produtor')


# Fazer correlacao ------------------------------------------------------------

corr_uht=series.iloc[:,0].corr(series.iloc[:,1])
round(corr_uht,2) # uht x preco prod

corr_po = series.iloc[:,0].corr(series.iloc[:,2])
round(corr_po,2) # po x preco prod

corr_mucarela = series.iloc[:,0].corr(series.iloc[:,3])
round(corr_mucarela,2) # mucarela x preco prod

corr_spot = series.iloc[:,0].corr(series.iloc[:,4])
round(corr_spot,2) # spot x preco prod


# Fazer regressao multipla ----------------------------------------------------

# Separando as variáveis independentes e a variável dependente 

series.iloc[:,0]
X = series[['UHT - cepea (R$/L)', 'Leite em pó - cepea (R$/L)','Queijo Muçarela (R$/kg)', 'Leite Cru (Spot)']]
y = series['preço ao produtor (R$/L)']

# Calcular o índice que corresponde aos primeiros 80% dos elementos
index = int(len(X) * 0.8)

# Obter os primeiros 80% dos elementos da Series
X = X.iloc[:index,:]
y = y[:index]

len(y)
len(X)

# Adicionando uma constante aos dados independentes para estimar o termo de interceptação
X = sm.add_constant(X)

# Ajustando o modelo de regressão múltipla
model = sm.OLS(y, X)
results = model.fit()

# Imprimindo os resultados da regressão
print(results.summary())

# Plotando os dados e a linha de regressão
fig, ax = plt.subplots()
ax.scatter(y, results.fittedvalues, edgecolors=(0, 0, 0))
ax.plot([y.min(), y.max()], [y.min(), y.max()], 'k--', lw=2)
ax.set_xlabel('Valores Observados')
ax.set_ylabel('Valores Estimados')
plt.show()

# ACF/ PACF Plots -------------------------------------------------------------

# Preço Prod  -----------------------------------------------------------------
plot_acf(y)
plt.xlabel('Lag')
plt.ylabel('Autocorrelacao')
plt.title('Preço ao Produtor (ACF)')

plot_pacf(y)
plt.xlabel('Lag')
plt.ylabel('Autocorrelacao')
plt.title('Preço ao Produtor (PACF)')


# AJUSTE MODELO ---------------------------------------------------------------

# Separar os regressores (X) e a variável dependente (y)

# Criar o modelo SARIMAX
model = sm.tsa.SARIMAX(y, exog=X, order=(1, 1, 0))
model

# Ajustar o modelo aos dados
results = model.fit()
# leite em po nao se mostrou significativo --> retirar

# Criar nova variavel X sem coluna do leite em po
type(X)

X_novo= X.drop('Leite em pó - cepea (R$/L)', axis=1) # axis = 1 indica que quero remover coluna, e nao linha 
type(X_novo)

# Criar novo modelo SARIMAX
model = sm.tsa.SARIMAX(y, exog=X_novo, order=(1, 1, 0))
model

# Ajustar o modelo aos dados
results = model.fit()

# Imprimir os resultados
print(results.summary())

# Residuos --------------------------------------------------------------------

# QQPLOT
residuos=results.resid
sm.qqplot(residuos, line='s')

plt.hist(residuos, edgecolor='black')

# KS TEST
from scipy.stats import kstest

_, p_value = kstest(residuos, 'norm')
print('Valor-p do teste de Kolmogorov-Smirnov:', p_value) # rejeita'-se a hipotese nula da normalidade dos residuos

# Shapiro
from scipy.stats import shapiro

_, p_value = shapiro(residuos)
print('Valor-p do teste de Shapiro-Wilk:', p_value) # rejeita-se a hipotese nula da normalidade dos residuos


# AJUSTAR NOVO MODELO ---------------------------------------------------------

# Considerar apenas Spot como regressor

X.loc[:,'Leite Cru (Spot)']

# Criar o modelo SARIMAX
model = sm.tsa.SARIMAX(y, exog=X.loc[:,'Leite Cru (Spot)'], order=(1, 1, 0))
model

# Ajustar o modelo aos dados
results = model.fit()

# Imprimir os resultados
print(results.summary())

# Residuos 2 ------------------------------------------------------------------

# QQPLOT
residuos=results.resid
sm.qqplot(residuos, line='s')

plt.hist(residuos, edgecolor='black')

# KS TEST
from scipy.stats import kstest

_, p_value = kstest(residuos, 'norm')
print('Valor-p do teste de Kolmogorov-Smirnov:', p_value) # rejeita'-se a hipotese nula da normalidade dos residuos

# Shapiro
from scipy.stats import shapiro

_, p_value = shapiro(residuos)
print('Valor-p do teste de Shapiro-Wilk:', p_value) # rejeita-se a hipotese nula da normalidade dos residuos

# ainda nao esta bom o ajuste!

# AJUSTAR NOVO MODELO ---------------------------------------------------------

# Auto.arima
! pip install pmdarima
from pmdarima import auto_arima

model = auto_arima( y, exogenous= X.loc[:,'Leite Cru (Spot)'], seasonal=True, m=12)

# Ajustar o modelo aos dados'
results = model.fit(y, exogenous = X.loc[:,'Leite Cru (Spot)'])

# Imprimir os resultados
print(results.summary())

# Residuos --------------------------------------------------------------------

# QQPLOT
residuos=model.resid()
sm.qqplot(residuos, line='s')

plt.hist(residuos, edgecolor='black')

# KS TEST
from scipy.stats import kstest

_, p_value = kstest(residuos, 'norm')
print('Valor-p do teste de Kolmogorov-Smirnov:', p_value) # rejeita'-se a hipotese nula da normalidade dos residuos

# Shapiro
from scipy.stats import shapiro

_, p_value = shapiro(residuos)
print('Valor-p do teste de Shapiro-Wilk:', p_value) # rejeita-se a hipotese nula da normalidade dos residuos




