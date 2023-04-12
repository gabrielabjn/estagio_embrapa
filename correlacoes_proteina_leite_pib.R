
# 10/04/2023
# Trabalho de verificao da correlacao entre consumo de leite/carne e PIB
# Considerando diferentes paises.

setwd("Z:/user/Projetos/Papers/Consumo_Proteina_renda") # definindo diretorio
library(readxl)
dados<-read_excel("Z:/user/Projetos/Papers/Consumo_Proteina_renda/Leite-carne-PIB-per_capita.xlsx")  # extraindo os dados

class(dados) # exibindo a(s) classe(s) dos dados

df<-as.data.frame(dados) # colocando tudo formato de data frame para padronizar
class(df) # exibindo a nova classe dos dados


# PLOT 1 - LEITE X PIB -------------------------------------------------------------------------------------------------

plot1<-plot(y = log(df$`Consumo per capita leite - 2021`) , ylab = 'Consumo de Leite per capita (kg)',
            x = log(df$`PIB per capita - 2021`) , xlab = 'PIB per capita (dolares)', 
            col = 'darkblue', pch = 16, main = "Leite x PIB (ln)")
correlacao<-cor.test(df$`PIB per capita - 2021`, df$`Consumo per capita leite - 2021`)

p.valor<-format(correlacao$p.value, scientific = FALSE)

mtext(paste('corr:', format(round(correlacao$estimate, 2), nsmall = 2), '    p-valor:',
            p.valor, sep = ' ', side=3, line = -2))

lm1<-lm(formula =  log(df$`Consumo per capita leite - 2021`) ~ log(df$`PIB per capita - 2021`))
abline(lm1, col = 'red')


par(mfrow = c(2,2))

# PLOT 2 - BOI X CARNE -------------------------------------------------------------------------------------------------

plot2<-plot(y = log(df$boi) , ylab = 'Consumo de Carne Bovina per capita (kg)',
            x = log(df$`PIB per capita - 2021`) , xlab = 'PIB per capita (dolares)', 
            col = 'darkblue', pch = 16, main = "Carne Bovina x PIB (ln)")
correlacao<-cor.test(df$`PIB per capita - 2021`, df$boi)

p.valor<-format(correlacao$p.value, scientific = FALSE)

mtext(paste('corr:', format(round(correlacao$estimate, 2), nsmall = 2), '    p-valor:',
            p.valor, sep = ' ', side=3, line = -2))

lm2<-lm(formula =  log(df$boi) ~ log(df$`PIB per capita - 2021`))
abline(lm2, col = 'red')



# PLOT 3 - PORCO X CARNE -------------------------------------------------------------------------------------------------

plot3<-plot(y = df$porco , ylab = 'Consumo de Carne Suina per capita (kg)',
            x = df$`PIB per capita - 2021` , xlab = 'PIB per capita (dolares)', 
            col = 'darkblue', pch = 16, main = "Carne Suina x PIB")
correlacao<-cor.test(df$`PIB per capita - 2021`, df$porco)

p.valor<-format(correlacao$p.value, scientific = FALSE)

mtext(paste('corr:', format(round(correlacao$estimate, 2), nsmall = 2), '    p-valor:',
            p.valor, sep = ' ', side=3, line = -2))

lm3<-lm(formula =  df$porco ~ df$`PIB per capita - 2021`)
abline(lm3, col = 'red')


# PLOT 4 - FRANGO X CARNE -------------------------------------------------------------------------------------------------


plot4<-plot(y = log(df$frango) , ylab = 'Consumo de Carne de Frango per capita (kg)',
            x = log(df$`PIB per capita - 2021`) , xlab = 'PIB per capita (dolares)', 
            col = 'darkblue', pch = 16, main = "Carne Frango x PIB (ln)")
correlacao<-cor.test(df$`PIB per capita - 2021`, df$frango)

p.valor<-format(correlacao$p.value, scientific = FALSE)

mtext(paste('corr:', format(round(correlacao$estimate, 2), nsmall = 2), '    p-valor:',
            p.valor, sep = ' ', side=3, line = -2))

lm4<-lm(formula =  log(df$frango) ~ log(df$`PIB per capita - 2021`))
abline(lm4, col = 'red')


# PLOT 5 - OVELHA X CARNE -------------------------------------------------------------------------------------------------

plot5<-plot(y = log(df$ovelha) , ylab = 'Consumo de Carne de Ovelha per capita (kg)',
            x = log(df$`PIB per capita - 2021`) , xlab = 'PIB per capita (dolares)', 
            col = 'darkblue', pch = 16, main = "Carne Ovelha x PIB (ln)")
correlacao<-cor.test(df$`PIB per capita - 2021`, df$ovelha)

p.valor<-format(correlacao$p.value, scientific = FALSE)

mtext(paste('corr:', format(round(correlacao$estimate, 2), nsmall = 2), '    p-valor:',
            p.valor, sep = ' ', side=3, line = -2))

lm5<-lm(formula =  log(df$ovelha) ~ log(df$`PIB per capita - 2021`))
abline(lm5, col = 'red')

rm(correlacao)



# Dados ficaram muito dispersos no scatterplot. Glauco solicitou trocar pra dotplot -------------------------------------- 


consumo_sobre_pib<- df$`Consumo per capita leite - 2021`/df$`PIB per capita - 2021`

# Grafico

par(mfrow= c(2,2))

par(mar = c(6, 4, 6, 1))

dotchart(consumo_sobre_pib, df$Pais[1:35], pch = 21, bg = "steelblue1",
         main = "Consumo de Leite por País- 2021",cex.main = 1.8,pt.cex = 1.5)
title(xlab = "Consumo em kg/capita", line = 4, cex.lab = 1.5) 

# Ainda nao ficou bom


# Vamos trabalhar com o data frame anterior organizado por ordem
#descrescente do PIB 

df_PIB_rank<-df[order(df$`PIB per capita - 2021`,decreasing=TRUE),] # Cria novo df
df_PIB_rank$`PIB per capita - 2021`


consumo_sobre_pib_rank<- df_PIB_rank$`Consumo per capita leite - 2021`/df_PIB_rank$`PIB per capita - 2021`

# Novo teste (grafico)

par(mar = c(6, 4, 6, 1))

dotchart(consumo_sobre_pib_rank[1:35], df_PIB_rank$Pais[1:35], pch = 21, bg = "steelblue1",
         main = "Consumo de Leite por País- 2021",cex.main = 1.8,pt.cex = 1.5)
title(xlab = "Consumo em kg per capita / PIB", line = 4, cex.lab = 1.5) 

# Agora, em ordem crescente

df_PIB_rank_as<-df[order(df$`PIB per capita - 2021`,decreasing=FALSE),] # Cria novo df
df_PIB_rank_as$`PIB per capita - 2021`


consumo_sobre_pib_as<- df_PIB_rank_as$`Consumo per capita leite - 2021`/df_PIB_rank_as$`PIB per capita - 2021`


par(mar = c(6, 4, 6, 1))

dotchart(consumo_sobre_pib_as[1:35], df_PIB_rank_as$Pais[1:35], pch = 21, bg = "steelblue1",
         main = "Consumo de Leite por País- 2021",cex.main = 1.8, pt.cex = 1.5)
title(xlab = "Consumo em kg per capita / PIB", line = 4, cex.lab = 1.5) 


rm(df_PIB_rank, df_PIB_rank_as, consumo_sobre_pib, consumo_sobre_pib_as, consumo_sobre_pib_rank)



# Eu tinha entendido errado. Eh para organizar de acordo com a ordem crescente do leite/PIB -----------------------------
# e nao apenas do PIB.

# Vamos fazer o seguinte: adicionar uma nova coluna a df, contendo os dados de consumo sobre renda

# Inicialmente, para o leite. Depois vamos replicar para as outras variaveis.

leite_pib<-(df$`Consumo per capita leite - 2021`/df$`PIB per capita - 2021`)

df <- cbind(df, leite_pib)

rm(leite_pib)

# Criar novo dataframe e organizar dados por ordem decrescente de leite/pib 

df_rankeado<-df[order(df$leite_pib,decreasing=FALSE),] 
df_rankeado

# Criar dotplot

dev.off()

# Use graphics.off() for non-interactive sessions

par(mar = c(6, 4, 6, 1))

dotchart(df_rankeado$leite_pib[1:34], df_rankeado$Pais[1:34], pch = 21, bg = "steelblue1",
         main = "Consumo de Leite por País- 2021",cex.main = 1.8, pt.cex = 1.5)
title(xlab = "Consumo em kg per capita / PIB per capita", line = 4, cex.lab = 1.5) 

# Grafico ficou de dificil interpretacao. Vamos fazer assim: dividir pib por mil
# antes de fazer a conta em leite/pib. Dai a interpretacao fica a seguinte:
# Quanto cada pessoa consome de leite por 1000 dolares de renda do pais. ------------------------------------------------

rm(df_rankeado)

pib_sobre_mil<- (df$`PIB per capita - 2021`)/1000

leite_pib<-(df$`Consumo per capita leite - 2021`/pib_sobre_mil)

leite_pib

df$leite_pib <- leite_pib 

df$leite_pib

rm(leite_pib, df_rankeado)

df_rankeado1<-df[order(df$leite_pib,decreasing=FALSE),] 
df_rankeado1

dev.off()

par(mar = c(6, 4, 6, 4))

df_rankeado1$Pais

dotchart(df_rankeado1$leite_pib[1:34], df_rankeado1$Pais[1:34], pch = 21, bg = c(rep("steelblue", 25),"green2",rep("steelblue", 8)),
         main = "Consumo de Leite por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 25),"green4",rep("black", 8)))

axis(side=1, at=seq(0, 80, by=10))

title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


# Replicar para as carnes ----------------------------------------------------------------------------------------------

# BOI

boi_pib<-(df$boi/pib_sobre_mil)

boi_pib

df <- cbind(df, boi_pib)

df$boi_pib

rm(boi_pib)

df_rankeado2<-df[order(df$boi_pib,decreasing=FALSE),] 
df_rankeado2

dev.off()

par(mar = c(6, 4, 6, 4))

df_rankeado2$Pais

dotchart(df_rankeado2$boi_pib[1:34], df_rankeado2$Pais[1:34], pch = 21, bg = c(rep("steelblue", 32),"green2","steelblue"),
         main = "Consumo de Carne Bovina por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 32), "green4", "black"))

title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 



# PORCO

porco_pib<-(df$porco/pib_sobre_mil)

porco_pib

df <- cbind(df, porco_pib)

df$porco_pib

rm(porco_pib)

df_rankeado3<-df[order(df$porco_pib,decreasing=FALSE),] 
df_rankeado3

dev.off()

par(mar = c(6, 4, 6, 4))

df_rankeado3$Pais

dotchart(df_rankeado3$porco_pib[1:34], df_rankeado3$Pais[1:34], pch = 21, bg = c(rep("steelblue", 28),"green2",rep("steelblue",5)),
         main = "Consumo de Carne Suína por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 28), "green4", rep("black",5)))

title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


# FRANGO

frango_pib<-(df$frango/pib_sobre_mil)

frango_pib

df <- cbind(df, frango_pib)

df$frango_pib

rm(frango_pib)

df_rankeado4<-df[order(df$frango_pib,decreasing=FALSE),] 
df_rankeado4

dev.off()

par(mar = c(6, 4, 6, 4))

df_rankeado4$Pais

dotchart(df_rankeado4$frango_pib[1:34], df_rankeado4$Pais[1:34], pch = 21, bg = c(rep("steelblue", 33),"green2"),
         main = "Consumo de Carne de Frango por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 33), "green4"))

title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


# OVELHA


ovelha_pib<-(df$ovelha/pib_sobre_mil)

ovelha_pib

df <- cbind(df, ovelha_pib)

df$ovelha_pib

rm(ovelha_pib)

df_rankeado5<-df[order(df$ovelha_pib,decreasing=FALSE),] 
df_rankeado5

dev.off()

par(mar = c(6, 4, 6, 4))

df_rankeado5$Pais

dotchart(df_rankeado5$ovelha_pib[1:34], df_rankeado5$Pais[1:34], pch = 21, bg = c(rep("steelblue", 13),"green2", rep("steelblue",20)),
         main = "Consumo de Carne de Ovelha por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 13), "green4", rep("black",20)))

title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 

dev.off()

# Junta tudo

par(mfrow=c(2,3), mar = c(6, 4, 6, 4))

dotchart(df_rankeado1$leite_pib[1:34], df_rankeado1$Pais[1:34], pch = 21, bg = c(rep("steelblue", 25),"green2",rep("steelblue", 8)),
         main = "Consumo de Leite por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 25),"green4",rep("black", 8)))
axis(side=1, at=seq(0, 80, by=10))
title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 

dotchart(df_rankeado2$boi_pib[1:34], df_rankeado2$Pais[1:34], pch = 21, bg = c(rep("steelblue", 32),"green2","steelblue"),
         main = "Consumo de Carne Bovina por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 32), "green4", "black"))
title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


dotchart(df_rankeado3$porco_pib[1:34], df_rankeado3$Pais[1:34], pch = 21, bg = c(rep("steelblue", 28),"green2",rep("steelblue",5)),
         main = "Consumo de Carne Suína por País - 2021",cex.main = 1.8, pt.cex = 1.5,
         col = c(rep("black", 28), "green4", rep("black",5)))
title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


dot4<-dotchart(df_rankeado4$frango_pib[1:34], df_rankeado4$Pais[1:34], pch = 21, bg = c(rep("steelblue", 33),"green2"),
               main = "Consumo de Carne de Frango por País - 2021",cex.main = 1.8, pt.cex = 1.5,
               col = c(rep("black", 33), "green4"))
title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


dot5<-dotchart(df_rankeado5$ovelha_pib[1:34], df_rankeado5$Pais[1:34], pch = 21, bg = c(rep("steelblue", 13),"green2", rep("steelblue",20)),
               main = "Consumo de Carne de Ovelha por País - 2021",cex.main = 1.8, pt.cex = 1.5,
               col = c(rep("black", 13), "green4", rep("black",20)))
title(xlab = "Consumo em kg / $1000 PIB (ambos per capita) ", line = 4, cex.lab = 1.5) 


