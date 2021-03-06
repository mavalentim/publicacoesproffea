library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(haven)
library(tidyr)
library(reshape2)


#ABRINDO A BASE DEIXADA NO REPOSIT�RIO
publi <- read_excel("C:/Users/Matheus/Desktop/coisa x em r/professores da fea publicam quando.xlsx", 
                                                  col_types = c("text", "text", "numeric", 
                                                                       "numeric", "skip"))

#CRIANDO VARI�VEIS DE INTERESSE: DISTANCIA ENTRE O ANO DE GRADUA��O E ANO DE OBTEN��O DO MESTRADO
#E A PRIMEIRA PUBLICA��O EXPRESSA NO LATTES
publi <- publi%>%
  mutate(ano_primeira_publicacao = as.double(ano_primeira_publicacao),
         ano_graduacao = as.double(ano_graduacao), ano_mestrado = as.double(ano_mestrado))%>%
  mutate(diferenca_grad = ano_primeira_publicacao - ano_graduacao,
         diferenca_mest = ano_primeira_publicacao - ano_mestrado)
  
#FAZENDO OS GR�FICOS DAS VARI�VEIS DE INTERESSE
ggplot(publi, aes(x=ano_graduacao, y= diferenca_grad), color = area) + geom_point(size=6)+
  labs(x = "Ano de Obten��o da Gradua��o", y = "Diferen�a de anos entre gradua��o e primeira publica��o", 
       title = "Diferen�a entre ano de obten��o da gradua��o e ano da primeira publica��o ao longo dos anos")

ggplot(publi, aes(x=ano_mestrado, y= diferenca_mest), color = blue) + geom_point(size=6)+
  labs(x = "Ano de Obten��o do Mestrado", y = "Diferen�a de anos entre fim do mestrado e primeira publica��o", 
       title = "Diferen�a entre �ltimo ano do mestrado e ano da primeira publica��o ao longo dos anos")


#CRIANDO ESTAT�STICAS DESCRITIVAS DAS VARI�VEIS
publi%>%
  summarise(mediadist_grad = mean(diferenca_grad, na.rm = TRUE), medianadist_grad = median(diferenca_grad, na.rm = TRUE),
            sddist_grad = sd(diferenca_grad, na.rm = TRUE),
            mediadist_mest = mean(diferenca_mest, na.rm = TRUE), medianadist_mest = median(diferenca_mest, na.rm = TRUE),
            sddist_mest = sd(diferenca_mest, na.rm = TRUE))
  
    
    
    
    
    
    

