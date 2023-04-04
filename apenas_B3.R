
# Carregando pacotes

library(rvest)

setwd("Z:/user/Planilha calculo/Agricola/Precos/scraping")

# Dia da cotação

#cot-fisicas

hoje <- as.POSIXlt(Sys.Date())

if(hoje$wday == 1){
  dia <- Sys.Date() - 3 # -3 (for mondays)
}else{
  dia <- Sys.Date() - 1 # -1 (for other days of the week)
}

rm(hoje)


# Coletando os dados do B3

# Boi Gordo B3------------------------------------------------------------------

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/boi-gordo/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[4]]) # 4
boigordo <- as.numeric(gsub(tabela$`Fechamento (R$/@)`[1:3], 
                            pattern = ',', replacement = '.'))
rm(link, tabela, tabelas)



# Café B3 ----------------------------------------------------------------------

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/cafe/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[4]]) # 4
cafearabica <- as.numeric(gsub(tabela$`Fechamento (US$/ Saca de 60kg)`[1:3], 
                               pattern = ',', replacement = '.'))
rm(link, tabela, tabelas)



# Soja B3 ----------------------------------------------------------------------

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/soja/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[5]]) # 5
soja <- as.numeric(gsub(tabela$`Fechamento (US$ / saca 60 kg)`[1:3], 
                        pattern = ',', replacement = '.'))
rm(link, tabela, tabelas)



# Milho B3 ---------------------------------------------------------------------

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/milho/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[2]]) # 1
milho <- as.numeric(gsub(tabela$`Fechamento (R$/ Saca de 60kg)`[1:3], 
                         pattern = ',', replacement = '.'))
rm(link, tabela, tabelas)



# Preparando para o excel ------------------------------------------------------


valores <- c(rep(NA, 3), boigordo, rep(NA, 5), cafearabica, 
             rep(NA, 3), soja, rep(NA,3), milho, rep(NA, 9))


matriz <- matrix(valores, nrow = 1)


colnames(matriz) <- c('Acucar', 'Algodao', 'Boi', 'Boi 1', 'Boi 2', 'Boi 3',
                      'Bezerro MS', 'Bezzero medio MS', 'Bezerro SP',
                      'Cafe arabica', 'Cafe robusta', 'Cafe 1', 'Cafe 2',
                      'Cafe 3', 'Soja paranagua', 'Soja parana', 'Soja 1',
                      'Soja 2', 'Soja 3','Milho', 'Milho 1', 'Milho 2', 'Milho 3',
                      'Trigo PR', 'Trigo RS', 'Frango congelado',
                      'Frango resfriado', 'Suino MG', 'Suino PR', 'Suino SP',
                      'Laranja industria', 'Laranja in natura',
                      'Etanol anidro', 'Etanol combustivel', 'Etanol industrial')

rownames(matriz) <- 'Valores'

write.csv2(matriz, file = "just_B3.csv")

rm(list = ls())




