library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(haven)
library(tidyr)
library(reshape2)


#ABRINDO A BASE DEIXADA NO REPOSITÓRIO
publi <- read_excel("C:/Users/Matheus/Desktop/coisa x em r/professores da fea publicam quando.xlsx", 
                                                  col_types = c("text", "text", "numeric", 
                                                                       "numeric", "skip"))

#CRIANDO VARIÁVEIS DE INTERESSE: DISTANCIA ENTRE O ANO DE GRADUAÇÃO E ANO DE OBTENÇÃO DO MESTRADO
#E A PRIMEIRA PUBLICAÇÃO EXPRESSA NO LATTES
publi <- publi%>%
  mutate(ano_primeira_publicacao = as.double(ano_primeira_publicacao),
         ano_graduacao = as.double(ano_graduacao), ano_mestrado = as.double(ano_mestrado))%>%
  mutate(diferenca_grad = ano_primeira_publicacao - ano_graduacao,
         diferenca_mest = ano_primeira_publicacao - ano_mestrado)
  

ggplot(publi, aes(x=ano_graduacao, y= diferenca_grad), color = area) + geom_point(size=6)+
  labs(x = "Ano de Obtenção da Graduação", y = "Diferença de anos entre graduação e primeira publicação", 
       title = "Diferença entre ano de obtenção da graduação e ano da primeira publicação ao longo dos anos")

ggplot(publi, aes(x=ano_mestrado, y= diferenca_mest), color = blue) + geom_point(size=6)+
  labs(x = "Ano de Obtenção do Mestrado", y = "Diferença de anos entre fim do mestrado e primeira publicação", 
       title = "Diferença entre último ano do mestrado e ano da primeira publicação ao longo dos anos")



publi%>%
  summarise(mediadist_grad = mean(diferenca_grad, na.rm = TRUE), medianadist_grad = median(diferenca_grad, na.rm = TRUE),
            sddist_grad = sd(diferenca_grad, na.rm = TRUE),
            mediadist_mest = mean(diferenca_mest, na.rm = TRUE), medianadist_mest = median(diferenca_mest, na.rm = TRUE),
            sddist_mest = sd(diferenca_mest, na.rm = TRUE))
  
    
    
    
    
    
    

