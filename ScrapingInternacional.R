# Carregando pacotes
setwd("Z:/user/Planilha calculo/Agricola/Precos/scraping")

library(rvest)

# Dia da cotação

#cot-fisicas

hoje <- as.POSIXlt(Sys.Date())

if(hoje$wday == 1){
  dia <- Sys.Date() -3 # -3 (for mondays)
}else{
  dia <- Sys.Date() - 1 # -1 (for other days of the week)
}

rm(hoje)

# Algodao

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/algodao/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[2]]) #2
algodao <- as.numeric(gsub(tabela$`Investing (¢/lb)`[1:3], 
                      pattern = ',', replacement = '.'))
rm(link, tabela, tabelas)

# CafÃ©

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/cafe/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[3]]) #3
tabela2 <- html_table(tabelas[[5]]) #5
cafe1 <- as.numeric(gsub(tabela$`Fech. Investing (¢/lb)`[1:3], 
                         pattern = ',', replacement = '.'))
cafe2 <- as.numeric(gsub(substr(tabela2$`Fechamento (US$ / ton)`[1:3], 
                                start = 0, stop = 5), 
                         pattern = '\\.', replacement = ''))
cafe <- c(cafe1, cafe2)

rm(link, tabela, cafe1, cafe2, tabela2, tabelas)

# Suco de laranja

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/laranja/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[3]]) #3
sucoLaranja <- as.numeric(gsub(tabela$`Fechamento (c/US$/libra peso)`[1:3], 
                               pattern = ',', replacement = '.'))


rm(link, tabela, tabelas)

# Milho

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/milho/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[3]]) #3
milho <- as.numeric(gsub(tabela$`Fechamento (US$ / Bushel)`[1:3], 
                         pattern = ',', replacement = '.')) * 100
rm(link, tabela, tabelas)

# Soja - Farelo de soja - Óleo de soja

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/soja/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[3]])#3  
tabela2 <- html_table(tabelas[[7]])#7   
tabela3 <- html_table(tabelas[[10]])#10
soja <- as.numeric(gsub(tabela$`Fechamento (US$ / Bushel)`[1:3], 
                        pattern = ',', replacement = '.')) * 100
farelo <- as.numeric(gsub(tabela2$`Fechamento (US$/ton)`[1:3], 
                        pattern = ',', replacement = '.'))
oleo <- as.numeric(gsub(tabela3$`Fechamento (US$/lb)`[1:3], 
                        pattern = ',', replacement = '.')) * 100
soja <- c(soja, farelo, oleo)

rm(link, tabela, tabela2, tabela3, tabelas, farelo, oleo)

# AÃ§Ãºcar - Etanol

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/sucroenergetico/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[4]])#4
tabela2 <- html_table(tabelas[[5]])#5
acucar <- as.numeric(gsub(tabela$`Fechamento c/US$/libra peso`[1:3], 
                          pattern = ',', replacement = '.'))
etanol <- as.numeric(gsub(tabela2$`Fechamento (US$/gal)`[1:3], 
                          pattern = ',', replacement = '.'))

rm(link, tabela, tabela2, tabelas)

# Trigo

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/trigo/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[2]]) #2
trigo <- as.numeric(gsub(tabela$`Fechamento (US$ / Bushel)`[1:3], 
                         pattern = ',', replacement = '.')) * 100
rm(link, tabela, tabelas)

# Montando a matriz

valores <- c(acucar, algodao, cafe, soja, milho, trigo, etanol, sucoLaranja)
matriz <- matrix(valores, nrow = 1)
colnames(matriz) <- c('Acucar 1', 'Acucar 2', 'Acucar 3', 'Algodao 1', 
                      'Algodao 2', 'Algodao 3', 'Cafe 1', 'Cafe 2', 'Cafe 3',
                      'Cafe Londres 1', 'Cafe Londres 2', 'Cafe Londres 3',
                      'Soja 1', 'Soja 2', 'Soja 3', 'Farelo de soja 1',
                      'Farelo de soja 2', 'Farelo de soja 3', 'Óleo de soja 1',
                      'Óleo de soja 2', 'Óleo de soja 3', 'Milho 1', 'Milho 2',
                      'Milho 3', 'Trigo 1', 'Trigo 2', 'Trigo 3', 'Etanol 1',
                      'Etanol 2', 'Etanol 3', 'Suco 1', 'Suco 2', 'Suco 3')

rownames(matriz) <- c('Valores')

write.csv2(matriz, file = 'valor.csv')

rm(list = ls())

