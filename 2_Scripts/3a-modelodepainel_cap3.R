#Modelos de painel - cap 3


# Diretório ----

wd = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield"


# Pacotes e configurações ----
options(scipen = 999)

library(fst) # carregar banco de dados 
library(tidyverse) # pipe e operações
library(dplyr) # operações de tratamento como filtro # REDUNDANTE 
library(weights) # médias ponderadas  # A PRINCÍPIO NÃO PRECISA
library(Hmisc) #medias ponderadas
library(segregation)  # indice do ben
library(rio) # funcao import
library(plm)  # modelos de painel
library(fixest) # modelos de painel
library(modelsummary) # comparar regressões bonitinhas


# 1. Formatando base de dados em painel -----------------------------------

cursos_painel =  read.csv(paste0(wd,"/lf_painel.csv")) 

#calcular variações com função diff para homens, brancos, n 

cursos_painel<- cursos_painel %>% 
  arrange(lf, ano) %>% 
  group_by(lf) %>% 
  mutate(ano2 = case_when(ano ==  1980 ~ 1
                          ,ano == 1991 ~ 2
                          ,ano == 2000 ~ 3
                          ,ano == 2010 ~ 4
                          
                          
  )) 

#colocar n em log para ver o efeito da variação percentual
# se colocar só n vai ser a mudança de uma unidade(uma pessoa entrando na área de estudo)
# por isso o efeito é pequeninho quase inexistente

cursos_painel_geral = cursos_painel %>% 
  filter(is.na(sexo)) %>%
  mutate (delta_renda = c(NA, diff(renda_media)),
          delta_n = c(NA, diff(n)),
          delta_logn = c(NA, diff(log(n))),
          delta_homens = c(NA, diff(perc_homens)),
          delta_mulheres = c(NA, diff(perc_mulheres)),
          delta_brancos= c(NA, diff(perc_brancos)),
          delta_negros = c(NA, diff(perc_negros))
  ) %>%
  ungroup()


cursos_painel_mulheres = cursos_painel %>% 
  filter( sexo == 0 ) %>% 
  mutate(delta_renda = c(NA, diff(renda_media)))


cursos_painel_mulheres$delta_n = cursos_painel_geral$delta_n
cursos_painel_mulheres$delta_logn = cursos_painel_geral$delta_logn

cursos_painel_homens = cursos_painel %>% 
  filter( sexo == 1 ) %>% 
  mutate(delta_renda = c(NA, diff(renda_media)))

cursos_painel_homens$delta_n = cursos_painel_geral$delta_n
cursos_painel_homens$delta_logn = cursos_painel_geral$delta_logn


cursos_painel = bind_rows(cursos_painel_geral, cursos_painel_homens, cursos_painel_mulheres) 


cursos_painel = cursos_painel %>% 
  arrange(lf, ano2) %>% 
  mutate(id = paste0(lf,sexo)) %>% 
  arrange(id, ano2)


# R recomendou essa função abaixo:
cursos_painel = pdata.frame(cursos_painel, index = c("id", "ano2"))



# 2. Modelo----------------------------------------------

## Efeitos fixos

felinkage_geral = ls ~ log(n) + delta_logn +  perc_mulheres +  delta_mulheres +  as.factor(ano) | as.factor(lf)

felinkagemulheres = ls ~ log(n)  + delta_logn + as.factor(ano)  |as.factor(lf)

felinkagehomens = ls ~ log(n)  + delta_logn + as.factor(ano)  | as.factor(lf)

## deu colinearidade com percentual mulheres e delta mulheres, mas manteve homens, 
#aí tirei pq acho que não era
## problema interno, mas entre homens e mulheres (soma daria 1, faz sentido)
## continua só com as factor 2010 e aí eu n entendi

# 2010 está como categoria de referência :o aí sua dummy "vai embora" (foi criada uma cópia)


feolslinkage = feols(felinkage_geral,
                     data = subset(cursos_painel, is.na(sexo)),
                     cluster = "lf")


feolslinkageh = feols(felinkagehomens,
                      data = subset(cursos_painel, sexo == 0),
                      cluster = "lf")

feolslinkagem = feols(felinkagemulheres,
                      data = subset(cursos_painel, sexo == 1),
                      cluster = "lf")

modelsummary(models = list(geral = feolslinkage,
                           homem = feolslinkageh, 
                           mulher =  feolslinkagem), stars = T)


