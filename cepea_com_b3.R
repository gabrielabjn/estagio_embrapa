
# Eh o seguinte: este codigo eh uma versao do scraping domestico que puxa os
# dados da b3 do portal Noticias Agricolas; porem, notamos que os dados desse 
# portal destoam levemente do originais encontrados no site oficial da B3.

# Ainda assim, este codigo eh uma boa alternativa a quando precisarmos pegar os
# dados com urgencia, ainda que apresentem uma leve diferenca dos dados originais.

# Carregando pacotes -----------------------------------------------------------

library(rvest)

setwd("Z:/user/Planilha calculo/Agricola/Precos/scraping")

# Link base - Cepea

linkBase <- 'https://www.cepea.esalq.usp.br/br/indicador/'

# Produtos - Cepea

produtos <- c('acucar.aspx', 'algodao.aspx', 'boi-gordo.aspx', 'bezerro.aspx',
              'bezerro-media-sao-paulo.aspx', 'cafe.aspx', 'soja.aspx',
              'milho.aspx', 'trigo.aspx', 'frango.aspx', 'suino.aspx',
              'citros.aspx', 'etanol.aspx')


# Coletando os dados do Cepea --------------------------------------------------

valores <- numeric()
c <- 1

for(i in 1:13){
  pagina <- read_html(paste(linkBase, produtos[i], sep = ''))
  
  if(i == 11){
    tabela <- html_nodes(pagina, '#imagenet-indicador2')
    v <- html_table(tabela)[[1]][c(1, 2, 5), 3]$ValorVista # [c(1,2,5), 3]      # +5 each
    v <- as.numeric(gsub(v, pattern = ',', replacement = '.'))
    valores <- c(valores, v)
    c <- c + 3
  }else{
    tabela <- html_nodes(pagina, '#imagenet-indicador1')
    v <- gsub(html_table(tabela)[[1]][2, 2], pattern = '\\.', #[1,2]
              replacement = '')
    v <- as.numeric(gsub(v, pattern = ',', replacement = '.'))
    
    valores[c] <- v
    c <- c + 1
    
    if(i %in% c(4, 6, 7, 9, 10, 12, 13)){
      tabela <- html_nodes(pagina, '#imagenet-indicador2')
      v <- gsub(html_table(tabela)[[1]][2, 2], pattern = '\\.', #[1,2]
                replacement = '')
      v <- as.numeric(gsub(v, pattern = ',', replacement = '.'))
      
      valores[c] <- v
      c <- c + 1
    }
    
    if(i == 13){
      tabela <- html_nodes(pagina, '#imagenet-indicador3')
      v <- gsub(html_table(tabela)[[1]][2, 2], pattern = '\\.', #[1,2]
                replacement = '')
      v <- as.numeric(gsub(v, pattern = ',', replacement = '.'))
      
      valores[c] <- v
      c <- c + 1
    }
  }
}


#para soja cepea - 9 stands for paranagua, 10 stands for parana
#para etanois (na ordem da precos_diarios) - 21,22,23
order <- c(1:8, 9, 10, 11:20, 21, 22, 23)
Cepea <- valores[order]
Cepea

# Coletando os dados da B3 -----------------------------------------------------

# Dia da cotação

#cot-fisicas

hoje <- as.POSIXlt(Sys.Date())

if(hoje$wday == 1){
  dia <- Sys.Date() - 3 # -3 (for mondays)
}else{
  dia <- Sys.Date() - 1 # -1 (for other days of the week)
}

rm(hoje)

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



# Milho B3 ----------------------------------------------------------------------

link <- read_html(paste('https://www.noticiasagricolas.com.br/cotacoes/milho/',
                        dia, sep = ''))
tabelas <- html_nodes(link, '.cot-fisicas')
tabela <- html_table(tabelas[[2]]) # 1
milho <- as.numeric(gsub(tabela$`Fechamento (R$/ Saca de 60kg)`[1:3], 
                        pattern = ',', replacement = '.'))
rm(link, tabela, tabelas)

rm(dia)


# Criando matriz de dados ------------------------------------------------------


valores <- c(Cepea[1:3], boigordo, Cepea[4:8], cafearabica, 
             Cepea[9:10], soja,Cepea[11], milho, Cepea[12:23])

matriz <- matrix(valores, nrow = 1)


ctrl+shift+c
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

write.csv2(matriz, file = "cepea_com_b3.csv")

rm(list = ls())

