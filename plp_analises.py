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
model = sm.tsa.SARIMAX(y, exog=X, order=(1, 1, 0), seasonal_order=(1, 0, 0, 12))
model

# Ajustar o modelo aos dados
results = model.fit()

# Imprimir os resultados
print(results.summary())

# Residuos --------------------------------------------------------------------

# QQPLOT
residuos=results.resid
sm.qqplot(residuos, line='s')


# KS TEST

from scipy.stats import kstest

_, p_value = kstest(residuos, 'norm')
print('Valor-p do teste de Kolmogorov-Smirnov:', p_value)

# Shapiro

from scipy.stats import shapiro

_, p_value = shapiro(residuos)
print('Valor-p do teste de Shapiro-Wilk:', p_value)



# Checando possiveis parametros d e D -----------------------------------------

# ### Check stationarity with Augmented Dickey-Fuller test

import time
start = time.time()

# function to create all combinations of differencing orders as needed

def differencing(series, m):
    info = []
    
    # "general" differencing only (d = 0 or 1 or 2)
    for i in range(3):
        series.name = f"d{i}_D0_m0"
        info.append(series)
        series = series.diff()
    
    # seasonal differencing (D = 1) given d = 0 or 1 or 2
    for i in m:
        for j in range(3):
            series = info[j].diff(periods=i)
            series.name = f"d{j}_D1_m{i}"
            info.append(series)
    
    # seasonal differencing (D = 2) given d = 0 or 1 or 2
    for i in m:
        for j in range(3):
            series = info[j+3].diff(periods=i)
            series.name = f"d{j}_D2_m{i}"
            info.append(series)
        
    return pd.DataFrame(info).T


# create the differenced series
diff_series = differencing(series['preço ao produtor (R$/L)'], [12])
diff_series

# All the above series are then put through ADF test to see if stationarity is achieved.

# function to create a summary of test results of all the series
def adf_summary(diff_series):
    from statsmodels.tsa.stattools import adfuller
    summary = []
    
    for i in diff_series:
        # unpack the results
        a, b, c, d, e, f = adfuller(diff_series[i].dropna())
        g, h, i = e.values()
        results = [a, b, c, d, g, h, i]
        summary.append(results)
    
    columns = ["Test Statistic", "p-value", "#Lags Used", "No. of Obs. Used",
               "Critical Value (1%)", "Critical Value (5%)", "Critical Value (10%)"]
    index = diff_series.columns
    summary = pd.DataFrame(summary, index=index, columns=columns)
    
    return summary


# create the summary
summary = adf_summary(diff_series)


# filter away results that are not stationary
pd.set_option('display.max_columns', None) # mostrar todas as colunas
summary_passed = summary[summary["p-value"] < 0.05]
summary_passed
# The non-stationary series are filtered away, leaving those that passed the test.

# output indices as a list
index_list = pd.Index.tolist(summary_passed.index)

# use the list as a condition to select stationary time-series
passed_series = diff_series[index_list]

# sort the columns by names
ps = passed_series.sort_index(axis=1)(axis=1)



# ### Calculate ACF & PACF for the resulting series ---------------------------

# store all values of significant spikes by nlags in PACF plots
from statsmodels.tsa.stattools import pacf

# create an empty dataframe to store values
df_sp_p = pd.DataFrame()
AR = []
for i in ps:
    # unpack the results into PACF and their CI
    PACF, PACF_ci = pacf(ps[i].dropna(), alpha=0.05, method='ywm')
    
    # subtract the upper and lower limits of CI by ACF to centre CI at zero
    PACF_ci_ll = PACF_ci[:,0] - PACF
    PACF_ci_ul = PACF_ci[:,1] - PACF
    
    # find positions of significant spikes ~ possible value of p & P
    sp1 = np.where(PACF < PACF_ci_ll)[0]
    sp2 = np.where(PACF > PACF_ci_ul)[0]
    
    # PACF values of the significant spikes
    sp1_value = abs(PACF[PACF < PACF_ci_ll])
    sp2_value = PACF[PACF > PACF_ci_ul]
    
    # store values to dataframe
    sp1_series = pd.Series(sp1_value, index=sp1)
    sp2_series = pd.Series(sp2_value, index=sp2)
    df_sp_p = pd.concat((df_sp_p, sp1_series, sp2_series), axis=1)


# store all values of significant spikes by nlags in ACF plots
from statsmodels.tsa.stattools import acf

# create an empty dataframe to store values
df_sp_q = pd.DataFrame()
MA = []
for i in ps:
    # unpack the results into ACF and their CI
    ACF, ACF_ci = acf(ps[i].dropna(), alpha=0.05)
    
    # subtract the upper and lower limits of CI by ACF to centre CI at zero
    ACF_ci_ll = ACF_ci[:,0] - ACF
    ACF_ci_ul = ACF_ci[:,1] - ACF
    
    # find positions of significant spikes ~ possible value of q & Q
    sp1 = np.where(ACF < ACF_ci_ll)[0]
    sp2 = np.where(ACF > ACF_ci_ul)[0]
    
    # ACF values of the significant spikes
    sp1_value = abs(ACF[ACF < ACF_ci_ll])
    sp2_value = ACF[ACF > ACF_ci_ul]
    
    # store values to dataframe
    sp1_series = pd.Series(sp1_value, index=sp1)
    sp2_series = pd.Series(sp2_value, index=sp2)
    df_sp_q = pd.concat((df_sp_q, sp1_series, sp2_series), axis=1)


# visualize sums of values of significant spikes by nlags in ACF plots
df_sp_q.iloc[1:].T.sum().plot(kind='bar', title='Possible MA Terms', xlabel='nth lag', ylabel='Sum of ACF')

# visualize sums of values of significant spikes by nlags in PACF plots
df_sp_p.iloc[1:].T.sum().plot(kind='bar', title='Possible AR Terms', xlabel='nth lag', ylabel='Sum of PACF')


# ## Building the SARIMA models -----------------------------------------------

# create a function to generate combinations of input list of no.
def pdq_grid(p, d, q):
    pdq = []
    for i in p:
        for j in d:
            for k in q:
                pdq.append([i, j, k])
    return pdq

def PDQm_grid(P, D, Q, m):
    PDQm = []
    for i in P:
        for j in D:
            for k in Q:
                for l in m:
                    PDQm.append([i, j, k, l])
    return PDQm
    

# possible values of the parameters
p = [0, 2]
d = [1, 2]
q = [0, 12]
P = [0, 1]
D = [0, 1, 2]
Q = [0,1]
m = [12]

# create all combinations of possible values
pdq = pdq_grid(p, d, q)
PDQm = PDQm_grid(P, D, Q, m)

# create a function for semi-grid-searching SARIMA ----------------------------
def SARIMA_grid(endog, order, seasonal_order):

    # create an empty list to store values
    model_info = []
    
    # filter away errors & warnings due to failture to converge, LU decomposition errors, etc
    import warnings
    warnings.simplefilter("ignore")
    
    #fit the model
    from statsmodels.tsa.statespace.sarimax import SARIMAX
    from statsmodels.tools.eval_measures import rmse
    for i in order:
        for j in seasonal_order:
            try:
                model = SARIMAX(endog=endog, order=i, seasonal_order=j)
                result = model.fit()
                predict = result.predict()
            
                # calculate evaluation metrics: MAPE, RMSE, AIC & BIC
                MAPE = (abs((endog-predict)[1:])/(endog[1:])).mean()
                RMSE = rmse(endog[1:], predict[1:])
                AIC = result.aic
                BIC = result.bic
            
                # create a list of order, seasonal order & evaluation metrics
                info = [i, j, MAPE, RMSE, AIC, BIC]
                model_info.append(info)
                
            except:
                continue
            
    # create a dataframe to store info of all models
    columns = ["order", "seasonal_order", "MAPE", "RMSE", "AIC", "BIC"]
    model_info = pd.DataFrame(data=model_info, columns=columns)
    return model_info


# create train-test-split -----------------------------------------------------
train = series['preço ao produtor (R$/L)'].iloc[:int(len(series)*0.8)]
test = series['preço ao produtor (R$/L)'].iloc[int(len(series)*0.8):]

# fit all combinations into the model
model_info = SARIMA_grid(endog=train, order=pdq, seasonal_order=PDQm)
                         
end = time.time()
print(f'time required: {end - start}')

# save the results
model_info.to_csv(path_or_buf='sarima_semi_model.csv')

# 10 least MAPE models
least_MAPE = model_info.nsmallest(10, "MAPE")
least_MAPE

# Best models per metric ------------------------------------------------------

# 10 least RMSE models
least_RMSE = model_info.nsmallest(10, "RMSE")
least_RMSE

# 10 least AIC models
least_AIC = model_info.nsmallest(10, "AIC")
least_AIC

# 10 least BIC models
least_BIC = model_info.nsmallest(10, "BIC")
least_BIC # the 1st 6 least BIC models are the same as the 1st 6 least AIC models

L1 = model_info[model_info.MAPE == model_info.MAPE.min()]
L2 = model_info[model_info.RMSE == model_info.RMSE.min()]
L3 = model_info[model_info.AIC == model_info.AIC.min()]
L4 = least_BIC[least_BIC.MAPE == least_BIC.MAPE.min()]

# least mape among least aic/bic
pd.concat((L1, L2, L3, L4)) 


# -----------------------------------------------------------------------------

# Let’s plot the results of each model with the following codes and see how 
#their predictions compared to ground truth.

import pandas as pd
from statsmodels.tsa.statespace.sarimax import SARIMAX

# fit the models of least MAPE, least RMSE, least AIC/BIC & least MAPE among least AIC/BIC
order_list = [(0,1,0), (2,1,12), (2,1,0), (2,1,0)]
seasonal_order_list = [(0,0,0,12), (0,0,0,12), (1,0,1,12), (1,0,1,12)]
y = series['preço ao produtor (R$/L)']
pred_list = []
y1 = [] # prediction confidence interval lower boundary
y2 = [] # prediction confidence interval upper boundary

for i in range(4):
    model = SARIMAX(endog=train.dropna(), order=order_list[i],
                    seasonal_order=seasonal_order_list[i])
    result = model.fit()
    pred_summary = result.get_prediction(y.index[0], y.index[-1]).summary_frame()
    pred_list.append(pred_summary['mean'])
    y1.append(pred_summary['mean_ci_lower'][test.index])
    y2.append(pred_summary['mean_ci_upper'][test.index])
    




