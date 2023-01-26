# Participação mulheres - modelo logístico - capítulo 3

# Diretório

# raw, desocupados
wd = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\censo com desocupados"

#processed, linkages
wd2 = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield"

# Pacotes e configuração
options(scipen = 999) #notação cientifica

library(rio)
library(tidyverse)
library(fst)
library(dplyr)
library(segregation)
library(weights)
library(Hmisc) #medias ponderadas
library(plm)  # modelos de painel
library(fixest) # modelos de painel
library(modelsummary) # comparar regressões bonitinhas
library(data.table)
library(questionr) # 
#Tentando plotar junto (tem de aperfeiçoar para unificar certas partes dos gráficos)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


# Abrindo censos e importando dados de linkage por levelfield do indivíduo

c91 = import(paste0(wd, "\\censodesocup1991.fst"))
c00 = import(paste0(wd, "\\censodesocup2000.fst"))
c10 = import(paste0(wd, "\\censodesocup2010.fst"))

l80 = import(paste0(wd2, "\\linkage_g1980.xlsx")) %>% select(lf, ls)
l91 = import(paste0(wd2, "\\linkage_g1991.xlsx"))%>% select(lf, ls)
l00 = import(paste0(wd2, "\\linkage_g2000.xlsx"))%>% select(lf, ls)

c91 = c91 %>% 
  left_join(l80, by = "lf")                    

c00 = c00 %>% 
  left_join(l91, by = "lf")                    

c10 = c10 %>% 
  left_join(l00, by = "lf")                    

gc()


# Função invlogit ---- 
invlogit = function(x) {
  
  1 / (1 + exp(-x))
  
}

# Ajustes de raça nos bancos ----

c91 = c91 %>% 
  mutate(raça = case_when(raça == 1 ~ 1,
                          raça == 2 ~ 0,
                          raça == 3 ~ 0))

c00 = c00 %>% 
  mutate(raça = case_when(raça == 1 ~ 1,
                          raça == 2 ~ 0,
                          raça == 3 ~ 0))
c10 = c10 %>% 
  mutate(raça = case_when(raça == 1 ~ 1,
                          raça == 2 ~ 0,
                          raça == 3 ~ 0))

gc()


# Participação modelo rev. Rogerio----
#1991
fit1 = glm (econActivity ~ ls*as.factor(sexo) + poly(idade, 2)*as.factor(sexo) + as.factor(raça), family=binomial(link="logit"),
            data = c91, weights = peso)
gc()


min(l80$ls)
max(l80$ls)


dados_inventados = expand.grid(ls = seq(0.08, 5.8, .1), sexo = c(0,1), idade = 35, raça = 1)

dados_inventados$valores_preditos_em_logODDs = predict(fit1, newdata = dados_inventados)

dados_inventados$prob_predita = invlogit(dados_inventados$valores_preditos_em_logODDs)




fig1 = 
  dados_inventados %>% 
  mutate(sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem'))) %>% 
  ggplot(aes(y = prob_predita,
             x = ls,
             color = as.factor(sexo))) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  # scale_x_continuous(breaks = seq(1980, 2010, 10),
  #                    minor_breaks = NULL) +
  # 
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(.3, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  labs(
    color = "Sexo",
    y = "Probabilidade de participação no mercado de trabalho",
    x = "Medida M",
    title = "Probabilidade de participação no mercado de trabalho",
    subtitle = "Brasil, 1991",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 8,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") 



ggsave(plot     = fig1,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf12.png",
       device = "png", dpi = 600, width = 8, height = 6)



#2000
fit2 = glm (econActivity ~ ls*as.factor(sexo) + poly(idade, 2)*as.factor(sexo) + as.factor(raça), family=binomial(link="logit"),
            data = c00, weights = peso)
gc()

min(l00$ls)
max(l00$ls)

dados_inventados2 = expand.grid(ls = seq(0.11, 4.53, .1), sexo = c(0,1), idade = 35, raça = 1)

dados_inventados2$valores_preditos_em_logODDs = predict(fit2, newdata = dados_inventados2)

dados_inventados2$prob_predita = invlogit(dados_inventados2$valores_preditos_em_logODDs)

fig2 = dados_inventados2 %>% 
  mutate(sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem'))) %>% 
  ggplot(aes(y = prob_predita,
             x = ls,
             color = as.factor(sexo))) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  # scale_x_continuous(breaks = seq(1980, 2010, 10),
  #                    minor_breaks = NULL) +
  # 
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(.3, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  labs(
    color = "Sexo",
    y = "Probabilidade de participação no mercado de trabalho",
    x = "Medida M",
    title = "Probabilidade de participação no mercado de trabalho",
    subtitle = "Brasil, 2000",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1991,2000)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 8,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") 



ggsave(plot     = fig2,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf13.png",
       device = "png", dpi = 600, width = 8, height = 6)


#2010
fit3 = glm (econActivity ~ ls*as.factor(sexo) + poly(idade, 2)*as.factor(sexo) + as.factor(raça), family=binomial(link="logit"),
            data = c10, weights = peso)
gc()


min(l00$ls)
max(l00$ls)

dados_inventados3 = expand.grid(ls = seq(0.11, 4.54, .1), sexo = c(0,1), idade = 35, raça = 1)

dados_inventados3$valores_preditos_em_logODDs = predict(fit3, newdata = dados_inventados3)

dados_inventados3$prob_predita = invlogit(dados_inventados3$valores_preditos_em_logODDs)

fig3 = dados_inventados3 %>% 
  mutate(sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem'))) %>% 
  ggplot(aes(y = prob_predita,
             x = ls,
             color = as.factor(sexo))) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  # scale_x_continuous(breaks = seq(1980, 2010, 10),
  #                    minor_breaks = NULL) +
  # 
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(.3, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  labs(
    color = "Sexo",
    y = "Probabilidade de participação no mercado de trabalho",
    x = "Medida M",
    title = "Probabilidade de participação no mercado de trabalho",
    subtitle = "Brasil, 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (2000,2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 8,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") 



ggsave(plot     = fig3,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf14.png",
       device = "png", dpi = 600, width = 8, height = 6)


#Model Summary ----

modelsummary(models = list( "1991" = fit1,
                            "2000" = fit2, 
                            "2010" = fit3), stars = T,
             output = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\part_modelo_att.docx")



## Filtrando apenas Ensino Superior para "purificar" efeito do linkage----

es_c91 = c91 %>% 
  filter(educ == 5)

es_c00 = c00 %>% 
  filter(educ == 5)

es_c10 = c10 %>% 
  filter(educ == 5)


#1991
fit4 = glm (econActivity ~ ls*sexo + poly(idade, 2)*sexo + raça, family=binomial(link="logit"),
            data = es_c91, weights = peso)


gc()


min()
dados_inventados4 = expand.grid(ls = seq(1.35, 5.79, .1), sexo = c(0,1), idade = 35, raça = 1)

dados_inventados4$valores_preditos_em_logODDs = predict(fit4, newdata = dados_inventados4)

dados_inventados4$prob_predita = invlogit(dados_inventados4$valores_preditos_em_logODDs)

fig4 = dados_inventados4 %>% 
  mutate(sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem'))) %>% 
  ggplot(aes(y = prob_predita,
             x = ls,
             color = as.factor(sexo))) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  # scale_x_continuous(breaks = seq(1980, 2010, 10),
  #                    minor_breaks = NULL) +
  # 
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(.8, 1),
                     breaks = seq(0, 1, .1),
                     minor_breaks = F) +
  
  labs(
    color = "Sexo",
    y = "Probabilidade de participação no mercado de trabalho",
    x = "Medida M",
    title = "",
    subtitle = "Brasil, 1991",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 8,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") 



ggsave(plot     = fig4,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf15.png",
       device = "png", dpi = 600, width = 8, height = 6)


#2000
fit5 = glm (econActivity ~ ls*sexo + poly(idade, 2)*sexo + raça, family=binomial(link="logit"),
            data = es_c00, weights = peso)

betas = round(coefficients(fit5), digits = 3)

probs = (exp(betas) -1 )*100

beprob = cbind(betas, probs)

export(beprob, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\beprob2000_es.xlsx" )

gc()

dados_inventados5 = expand.grid(ls = seq(1, 4.76, .1), sexo = c(0,1), idade = 35, raça = 1)

dados_inventados5$valores_preditos_em_logODDs = predict(fit5, newdata = dados_inventados5)

dados_inventados5$prob_predita = invlogit(dados_inventados5$valores_preditos_em_logODDs)

fig5 = dados_inventados5 %>% 
  mutate(sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem'))) %>% 
  ggplot(aes(y = prob_predita,
             x = ls,
             color = as.factor(sexo))) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  # scale_x_continuous(breaks = seq(1980, 2010, 10),
  #                    minor_breaks = NULL) +
  # 
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(.8, 1),
                     breaks = seq(0, 1, .1),
                     minor_breaks = F) +
  
  labs(
    color = "Sexo",
    y = "Probabilidade de participação no mercado de trabalho",
    x = "Medida M",
    title = "",
    subtitle = "Brasil, 2000",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1991,2000)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 8,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") 



ggsave(plot     = fig5,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf16.png",
       device = "png", dpi = 600, width = 8, height = 6)



#2010
fit6 = glm (econActivity ~ ls*sexo + poly(idade, 2)*sexo + raça, family=binomial(link="logit"),
            data = es_c10, weights = peso)


gc()
dados_inventados6 = expand.grid(ls = seq(1.1, 4.53, .1), sexo = c(0,1), idade = 35, raça = 1)

dados_inventados6$valores_preditos_em_logODDs = predict(fit6, newdata = dados_inventados6)

dados_inventados6$prob_predita = invlogit(dados_inventados6$valores_preditos_em_logODDs)

fig6 = dados_inventados5 %>% 
  mutate(sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem'))) %>% 
  ggplot(aes(y = prob_predita,
             x = ls,
             color = as.factor(sexo))) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  # scale_x_continuous(breaks = seq(1980, 2010, 10),
  #                    minor_breaks = NULL) +
  # 
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(.8, 1),
                     breaks = seq(0, 1, .1),
                     minor_breaks = F) +
  
  labs(
    color = "Sexo",
    y = "Probabilidade de participação no mercado de trabalho",
    x = "Medida M",
    title = "",
    subtitle = "Brasil, 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (2000,2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 8,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") 



ggsave(plot     = fig6,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf17.png",
       device = "png", dpi = 600, width = 8, height = 6)



#Model Summary ----

modelsummary(models = list( "1991" = fit4,
                            "2000" = fit5, 
                            "2010" = fit6), stars = T,
             output = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\part_modelo_ES.docx")

fig1 = import("C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf15.png")


p = grid.arrange(fig1, fig2, fig3,  nrow = 1, ncol = 3)
q = grid.arrange(fig4, fig5, fig6,  nrow = 1, ncol = 3)


ggsave(plot     = p,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf18.png",
       device = "png", dpi = 600, width = 10, height = 6)

ggsave(plot     = q,
       filename = "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\graf19.png",
       device = "png", dpi = 600, width = 10, height = 6)
