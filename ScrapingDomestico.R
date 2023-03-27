# Carregando pacotes

library(rvest)

setwd("Z:/user/Planilha calculo/Agricola/Precos/scraping")

# Link base - Cepea

linkBase <- 'https://www.cepea.esalq.usp.br/br/indicador/'

# Produtos - Cepea

produtos <- c('acucar.aspx', 'algodao.aspx', 'boi-gordo.aspx', 'bezerro.aspx',
              'bezerro-media-sao-paulo.aspx', 'cafe.aspx', 'soja.aspx',
              'milho.aspx', 'trigo.aspx', 'frango.aspx', 'suino.aspx',
              'citros.aspx', 'etanol.aspx')


# Coletando os dados do Cepea

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
    v <- gsub(html_table(tabela)[[1]][1, 2], pattern = '\\.', #[1,2]
              replacement = '')
    v <- as.numeric(gsub(v, pattern = ',', replacement = '.'))
    
    valores[c] <- v
    c <- c + 1
    
    if(i %in% c(4, 6, 7, 9, 10, 12, 13)){
      tabela <- html_nodes(pagina, '#imagenet-indicador2')
      v <- gsub(html_table(tabela)[[1]][1, 2], pattern = '\\.', #[1,2]
                replacement = '')
      v <- as.numeric(gsub(v, pattern = ',', replacement = '.'))
      
      valores[c] <- v
      c <- c + 1
    }
    
    if(i == 13){
      tabela <- html_nodes(pagina, '#imagenet-indicador3')
      v <- gsub(html_table(tabela)[[1]][1, 2], pattern = '\\.', #[1,2]
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

# Coletando os dados do B3

### manter codigo abaixo comentado
#ss quando tiver erro na b3, para facilitar na hora de copiar.

# link <- read_html('http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-ajustes-do-pregao-ptBR.asp')
# 
# 
# 
# trs <- html_nodes(link, 'tr')
# 
# nBoi <- grep('Boi', trs)
# nCafe <- grep('Cafi', trs)
# nSoja <- grep('Soja', trs)
# nMilho <- grep('Milho', trs)
# 
# 
# b3Tr <- trs[c(nBoi:(nBoi + 2), nCafe:(nCafe + 2), nSoja[1]:(nSoja[1] + 2), 
#               nMilho:(nMilho + 2))]
# b3Td <- html_nodes(b3Tr, 'td')
# b3 <- as.numeric(gsub(html_text(b3Td[c(4, 10, 16, 22, 28, 34, 
#                                        40, 46, 52, 58, 64, 70)]), 
#                       pattern = ',', replacement = '.'))


# Criando matriz de dados

#usar o abaixo quando site da b3 nao funcionar. usar o comentado caso contrario.
valores <- c(Cepea[1:3], NA, NA, NA, Cepea[4:8], NA , NA, NA, NA, Cepea[9:10], NA, NA,
             NA, NA, NA, Cepea[11], NA, NA, NA, Cepea[12:20])

Cepea

#valores <- c(Cepea[1:3], b3[1:3], Cepea[4:8], b3[4:6], Cepea[9:10], b3[7:9],Cepea[11], b3[10:12], Cepea[12:23])

matriz <- matrix(valores, nrow = 1)


#usar o abaixo quando site da b3 nao funcionar. usar o comentado caso contrario.
colnames(matriz) <- c('Acucar', 'Algodao', 'Boi','', '', '','Bezerro MS', 'Bezzero midio MS', 'Bezerro SP',
                      'Cafi arabica', 'Cafi robusta', '', '', '', 'Soja paranagua', 'Soja parana', '', '', '',
                      'Milho', '', '', '', 'Trigo PR', 'Trigo RS', 'Frango congelado',
                      'Frango resfriado', 'Sumno MG', 'Sumno PR', 'Sumno SP',
                      'Laranja indC:stria', 'Laranja in natura',
                      'Etanol anidro', 'Etanol combustmvel', 'Etanol industrial')

# ctrl+shift+c
# colnames(matriz) <- c('Acucar', 'Algodao', 'Boi', 'Boi 1', 'Boi 2', 'Boi 3',
#                       'Bezerro MS', 'Bezzero medio MS', 'Bezerro SP', 
#                       'Cafe arabica', 'Cafe robusta', 'Cafe 1', 'Cafe 2', 
#                       'Cafe 3', 'Soja paranagua', 'Soja parana', 'Soja 1', 
#                       'Soja 2', 'Soja 3','Milho', 'Milho 1', 'Milho 2', 'Milho 3',  
#                       'Trigo PR', 'Trigo RS', 'Frango congelado',
#                       'Frango resfriado', 'Suino MG', 'Suino PR', 'Suino SP',
#                       'Laranja industria', 'Laranja in natura', 
#                       'Etanol anidro', 'Etanol combustivel', 'Etanol industrial')

rownames(matriz) <- 'Valores'

write.csv2(matriz, file = "cepea.csv")

rm(list = ls())

