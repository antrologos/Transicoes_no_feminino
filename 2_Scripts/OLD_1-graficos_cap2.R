# Capítulo 2 Dissertação (gráficos bonitos)

# Pacotes e configuração ----

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
library(questionr) # wtd.table 
# Script do Rogério
library(data.table)
library(janitor)
library(IC2)
library(scales)
library(ggrepel) 
library(sf)
library(zoo)
library(cowplot)
library(lubridate)

# Caracterizando níveis-área em femininos, masculinos, neutros e separando Ensino básico ----

lf2 = import(here("../1_Data/processed/lf_painel.fst")) %>% 
  filter(is.na(sexo)) %>% 
  mutate(def = case_when(perc_mulheres > 0.6 ~ "Feminino",
                         perc_homens > 0.6 ~ "Masculino",
                         TRUE ~ "Neutro"
  ))

lf3 = lf2 %>% 
  arrange(lf, ano) %>% 
  group_by(lf, def) %>% 
  summarise(total_count=n(),.groups = 'drop') 

feminino = lf3 %>% 
  filter(def == "Feminino" & total_count >= 3)

masculino = lf3 %>% 
  filter(def == "Masculino" & total_count >= 3)

neutro = lf3 %>% 
  filter(def == "Neutro" & total_count >= 3)

def = bind_rows(feminino, masculino, neutro)

def = def %>% 
  mutate(def = if_else(lf %in% c("1NA", "2NA", "3NA", "4NA"),"Ensino básico", def))

lf = import(here("../1_Data/processed/lf_painel.fst")) %>%  
  filter(is.na(sexo)) %>% 
  left_join(def, by = "lf")

rm(lf3, lf2, feminino, masculino, neutro)


#Labels de level field ----

lf_labels = import(here("lf_labels.xlsx")) %>% 
  mutate(label = factor(label, 
                        levels = c( 'Não foi a escola',
                                    'E. F. incompleto',
                                    'E. F. completo',
                                    'E. M. completo',
                                    'Ciências da educação',
                                    'Formação de professores',
                                    'Artes',
                                    'Humanidades e letras',
                                    'Religião',
                                    'Língua materna (vernácula)',
                                    'História e arqueologia',
                                    'Filosofia e ética',
                                    'Ciências sociais e comportamentais',
                                    'Psicologia',
                                    'Economia',
                                    'Comunicação social',
                                    'Biblioteconomia',
                                    'Contabilidade e tributação',
                                    'Administração',
                                    'Direito',
                                    'Biologia e bioquímica',
                                    'Ciências físicas',
                                    'Física',
                                    'Química',
                                    'Matemática',
                                    'Estatística',
                                    'Ciência da computação',
                                    'Engenharia  (cursos gerais)',
                                    'Engenharia mecânica',
                                    'Eletricidade e energia',
                                    'Engenharia de processos',
                                    'Arquitetura e urbanismo',
                                    'Engenharia civil',
                                    'Agricultura',
                                    'Veterinária',
                                    'Saúde',
                                    'Medicina',
                                    'Enfermagem',
                                    'Odontologia',
                                    'Farmácia',
                                    'Serviço social',
                                    'Setor militar e de defesa',
                                    'Outros, não sabe e não especificado'
                        )))


# Gráfico 1 : Participação geral no mercado de trabalho -----


part_geral = import(here("../1_Data/processed/part_geral.xlsx")) %>% 
  mutate(partp = (part/100),
         part = round(part, digits = 1))

participacao_geral = 
  part_geral %>% 
  ggplot(aes(x = ano,
             y = partp)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0.5, 0.8),
                     breaks = seq(0, 1, .1),
                     minor_breaks = F) +
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=-0.7, size = 4) +
  labs(
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 11,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom")
participacao_geral


ggsave(plot     = participacao_geral,
       filename = here("../3_Outputs/graf1.png"),   # to salvando fora do compartilhado, para cada um gerar o seu 
       device = "png", dpi = 600, width = 5, height = 4)


# Gráfico 2.1: Participação no mercado de trabalho por nível-área (femininos) -----

part_lf = import(here("../1_Data/processed/part_lf.xlsx")) %>% 
  mutate(partp = (part/100),
         part = round(part, digits = 1))

part_lf = left_join(part_lf, lf_labels, by = 'lf')

part_lf = left_join(part_lf, def, by = 'lf')



participacao_lf_fem = 
  part_lf %>% 
  filter(def == "Feminino") %>% 
  ggplot(aes(x = ano,
             y = partp)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0.4, 1),
                     breaks = seq(0, 1, .2),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.3, size = 2.5) +
  labs(
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (feminino)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

participacao_lf_fem


ggsave(plot     = participacao_lf_fem,
       filename = here("../3_Outputs/graf2-1.png"),
       device = "png", dpi = 600, width = 9, height = 6)

#Gráfico 2.2: Participação no mercado de trabalho por nível-área (masculino) -----

participacao_lf_mas = 
  part_lf %>% 
  filter(def == 'Masculino') %>% 
  ggplot(aes(x = ano,
             y = partp)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0.4, 1),
                     breaks = seq(0, 1, .2),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 2.5) +
  labs(
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (masculino)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)



ggsave(plot     = participacao_lf_mas,
       filename = here("../3_Outputs/graf2-2.png"),
       device = "png", dpi = 600, width = 9, height = 6)

#Gráfico 2.3: Participação no mercado de trabalho por nível-área (neutro) -----

participacao_lf_neu = 
  part_lf %>% 
  filter(def == "Neutro") %>% 
  ggplot(aes(x = ano,
             y = partp)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0.4, 1),
                     breaks = seq(0, 1, .2),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 2.5) +
  labs(
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (neutro)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)


ggsave(plot     = participacao_lf_neu,
       filename = here("../3_Outputs/graf2-3.png"),
       device = "png", dpi = 600, width = 9, height = 6)


#Gráfico 2.3: Participação no mercado de trabalho por nível-área (basico) -----

participacao_lf_bas = 
  part_lf %>% 
  filter(def == "Ensino básico") %>% 
  ggplot(aes(x = ano,
             y = partp)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0.4, 1),
                     breaks = seq(0, 1, .2),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 3) +
  labs(
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (Ensino básico)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
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
        
        legend.position = "bottom") +
  facet_wrap(~label)


ggsave(plot     = participacao_lf_bas,
       filename = here("../3_Outputs/graf2-4.png"),
       device = "png", dpi = 600, width = 8, height = 6)

# Gráfico 3: Medida M geral ----

linkage_geral = import(here("../1_Data/processed/linkage_tg.xlsx")) %>% 
  filter(stat == "M") %>% 
  mutate(est = round(est, digits = 3))

M_geral = 
  linkage_geral %>% 
  ggplot(aes(x = ano,
             y = est)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(0.35, 0.4),
    minor_breaks = F) +
  geom_text(aes(label= est), position=position_dodge(width=0.9), vjust=-0.7, hjust = 0.1, size = 3) +
  labs(
    y = "Medida M",
    x = "Períodos",
    title = "Medida M",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 11,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom")

M_geral


ggsave(plot     = M_geral,
       filename = here("../3_Outputs/graf3.png"),
       device = "png", dpi = 600, width = 6, height = 4)


#Gráfico 4 :Medida M por nível-área ----

lf = import(here("../1_Data/processed/lf_painel.fst")) %>%  
        filter(is.na(sexo)) %>% 
        left_join(def, by = "lf") %>% 
  mutate(ls = round(ls, digits = 2))


# Gráfico 4.1 (femininos)---- 
linkage_lf_fem = 
  lf %>% 
  filter(def == "Feminino") %>% 
  ggplot(aes(x = ano,
             y = ls)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(1, 7),
    breaks = seq(0, 7, 1),
    minor_breaks = F) +
  
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.3, hjust = 0.3, size = 2.5) +
  labs(
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (feminino)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

linkage_lf_fem


ggsave(plot     = linkage_lf_fem,
       filename = here("../3_Outputs/graf4-1.png"),
       device = "png", dpi = 600, width = 9, height = 6)



# Gráfico 4.2 (masculinos)---- 
linkage_lf_mas = 
  lf %>% 
  filter(def == "Masculino") %>% 
  ggplot(aes(x = ano,
             y = ls)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  
  scale_y_continuous(
    limits = c(0.5, 6),
    breaks = seq(0, 6, 1),
    minor_breaks = F) +
  
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.3, hjust = 0.3, size = 3) +
  labs(
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (masculino)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

linkage_lf_mas


ggsave(plot     = linkage_lf_mas,
       filename = here("../3_Outputs/graf4-2.png"),
       device = "png", dpi = 600, width = 9, height = 6)


# Gráfico 4.3 (neutros)---- 
linkage_lf_neu = 
  lf %>% 
  filter(def == "Neutro") %>% 
  ggplot(aes(x = ano,
             y = ls)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(0.5, 5),
    breaks = seq(0, 5, 1),
    minor_breaks = F) +
  
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.3, hjust = 0.3, size = 3) +
  labs(
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (Neutro)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

linkage_lf_neu


ggsave(plot     = linkage_lf_neu,
       filename = here("../3_Outputs/graf4-3.png"),
       device = "png", dpi = 600, width = 9, height = 6)

# Gráfico 4.4 (basico)---- 
linkage_lf_bas = 
  lf %>% 
  filter(def == "Ensino básico") %>% 
  ggplot(aes(x = ano,
             y = ls)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .2),
    minor_breaks = F) +
  
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.3, hjust = 0.3, size = 3) +
  labs(
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (Ensino básico)",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

linkage_lf_neu


ggsave(plot     = linkage_lf_bas,
       filename = here("../3_Outputs/graf4-4.png"),
       device = "png", dpi = 600, width = 9, height = 6)

# Gráfico 5.1 : Participação no mercado de trabalho por sexo e nível área (femininos) -----


part_lf_sexo = import(here("../1_Data/processed/part_lfs.xlsx")) %>% 
  mutate(part = round(part, digits = 1),
         sexo = factor(sexo, 
                       labels = c('Mulher', 'Homem')))


part_lf_sexo = left_join(part_lf_sexo, lf_labels, by = 'lf') 

part_lf_sexo = left_join(part_lf_sexo, def, by = 'lf') 


participacao_lf_sexo_fem = 
  part_lf_sexo %>% 
  filter(def == "Feminino") %>% 
  ggplot(aes(x = ano,
             y = partp,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 3, check_overlap = T) +
  labs(
    color = 'Sexo',
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (feminino) e gênero",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)



ggsave(plot     = participacao_lf_sexo_fem,
       filename = here("../3_Outputs/graf5-1.png"),
       device = "png", dpi = 600, width = 9, height = 7)

# Gráfico 5.2 : Participação no mercado de trabalho por sexo e nível área (masculinos) -----

participacao_lf_sexo_mas =
  part_lf_sexo %>% 
  filter(def == "Masculino") %>% 
  ggplot(aes(x = ano,
             y = partp,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 3, check_overlap = T) +
  labs(
    color = 'Sexo',
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (masculino) e gênero",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)



ggsave(plot     = participacao_lf_sexo_mas,
       filename = here("../3_Outputs/graf5-2.png"),
       device = "png", dpi = 600, width = 9, height = 7)

# Gráfico 5.3 : Participação no mercado de trabalho por sexo e nível área (neutros) -----

participacao_lf_sexo_neu = 
  part_lf_sexo %>% 
  filter(def == "Neutro") %>% 
  ggplot(aes(x = ano,
             y = partp,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 3, check_overlap = T) +
  labs(
    color = 'Sexo',
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (neutro) e gênero",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

participacao_lf_sexo


ggsave(plot     = participacao_lf_sexo_neu,
       filename = here("../3_Outputs/graf5-3.png"),
       device = "png", dpi = 600, width = 9, height = 7)
# Gráfico 5.3 : Participação no mercado de trabalho por sexo e nível área (basicos) -----

participacao_lf_sexo_bas = 
  part_lf_sexo %>% 
  filter(def == "Ensino básico") %>% 
  ggplot(aes(x = ano,
             y = partp,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 1.2) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(labels = \(x) percent(x = x ,
                                           accuracy     = .1,
                                           big.mark     = ".",
                                           decimal.mark = ","),
                     limits = c(0, 1),
                     breaks = seq(0, 1, .25),
                     minor_breaks = F) +
  
  geom_text(aes(label= (percent(x = partp ,
                                accuracy     = .1,
                                big.mark     = ".",
                                decimal.mark = ","))), position=position_dodge(width=0.9), vjust=1.2, hjust = 0.4, size = 3, check_overlap = T) +
  labs(
    color = 'Sexo',
    y = "Percentual",
    x = "Períodos",
    title = "Participação no mercado de trabalho por nível-área (Ensino básico) e gênero",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)

participacao_lf_sexo


ggsave(plot     = participacao_lf_sexo_bas,
       filename = here("../3_Outputs/graf5-4.png"),
       device = "png", dpi = 600, width = 8, height = 6)


# Gráfico 6 : Medida M por sexo ----


linkage_tf = import(here("../1_Data/processed/linkage_tf.xlsx"))
linkage_tm = import(here("../1_Data/processed/linkage_tm.xlsx"))

linkage_tf$sexo = 0
linkage_tm$sexo = 1  

linkage_total = bind_rows(linkage_tm, linkage_tf) %>% 
  filter( stat == "M") %>% 
  mutate(sexo = factor (sexo, labels = c("Mulher", "Homem")),
         est = round(est, digits = 2))

M_geral_sexo = 
  linkage_total %>% 
  ggplot(aes(x = ano,
             y = est,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(0.2, 0.6),
    breaks = seq(0.2, 0.8, .1),
    minor_breaks = F) +
  geom_text(aes(label= est), position=position_dodge(width=0.9), vjust=-0.7, hjust = 0.1, size = 4) +
  labs(
    color = "Sexo",
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por sexo",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom")

M_geral_sexo


ggsave(plot     = M_geral_sexo,
       filename = here("../3_Outputs/graf6.png"),
       device = "png", dpi = 600, width = 5, height = 4)



# Gráfico 7 .1: Medida M por nível-área e sexo (femininos) ----

linkage_f = import(here("../1_Data/processed/linkage_f.xlsx"))
linkage_m = import(here("../1_Data/processed/linkage_m.xlsx"))

linkage_f$sexo = 0
linkage_m$sexo = 1  


linkage = bind_rows(linkage_m, linkage_f) %>% 
  left_join(lf_labels, by = "lf") %>% 
  mutate(sexo = factor (sexo, labels = c("Mulher", "Homem")),
         ls = round(ls, digits = 2))

linkage = left_join(linkage, def, by = 'lf') 


M_sexo_area_fem = 
  linkage %>% 
  filter(def == "Feminino") %>% 
  ggplot(aes(x = ano,
             y = ls,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  # scale_y_continuous(labels = \(x) percent(x = x ,
  #                                          accuracy     = .1,
  #                                          big.mark     = ".",
  #                                          decimal.mark = ","),
  #                    limits = c(0.5, 0.8),
  #                    breaks = seq(0, 1, .1),
  #                    minor_breaks = F) +
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.7, hjust = 0.1, size = 3, check_overlap = T) +
  labs(
    color = "Sexo",
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (feminino) e sexo",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)


ggsave(plot     = M_sexo_area_fem,
       filename = here("../3_Outputs/graf7.png"),
       device = "png", dpi = 600, width = 9, height = 6)

# Gráfico 7 .2: Medida M por nível-área e sexo (masculinos) -----
M_sexo_area_mas = 
  linkage %>% 
  filter(def == "Masculino") %>% 
  ggplot(aes(x = ano,
             y = ls,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  # scale_y_continuous(labels = \(x) percent(x = x ,
  #                                          accuracy     = .1,
  #                                          big.mark     = ".",
  #                                          decimal.mark = ","),
  #                    limits = c(0.5, 0.8),
  #                    breaks = seq(0, 1, .1),
  #                    minor_breaks = F) +
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.7, hjust = 0.1, size = 3, check_overlap = T) +
  labs(
    color = "Sexo",
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (masculino) e sexo",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)



ggsave(plot     = M_sexo_area_mas,
       filename = here("../3_Outputs/graf7-2.png"),
       device = "png", dpi = 600, width = 9, height = 6)


# Gráfico 7.3 : Medida M por nível-área e sexo (neutros) -----
M_sexo_area_neu = 
  linkage %>% 
  filter(def == "Neutro") %>% 
  ggplot(aes(x = ano,
             y = ls,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(0, 6),
    breaks = seq(0, 6, 1),
    minor_breaks = F) +
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.7, hjust = 0.1, size = 3, check_overlap = T) +
  labs(
    color = "Sexo",
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (neutro) e sexo",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 6,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~label)



ggsave(plot     = M_sexo_area_neu,
       filename = here("../3_Outputs/graf7-3.png"),
       device = "png", dpi = 600, width = 9, height = 6)



M_sexo_area_bas = 
  linkage %>% 
  filter(def == "Ensino básico") %>% 
  ggplot(aes(x = ano,
             y = ls,
             color = sexo)) +
  
  geom_line(lwd   = 1, alpha = .8) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = seq(1980, 2010, 10),
                     minor_breaks = NULL) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .3),
    minor_breaks = F) +
  geom_text(aes(label= ls), position=position_dodge(width=0.9), vjust=-0.7, hjust = 0.1, size = 3, check_overlap = T) +
  labs(
    color = "Sexo",
    y = "Medida M",
    x = "Períodos",
    title = "Medida M por nível-área (Ensino básico) e sexo",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
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
        
        legend.position = "bottom") +
  facet_wrap(~label)



ggsave(plot     = M_sexo_area_bas,
       filename = here("../3_Outputs/graf7-4.png"),
       device = "png", dpi = 600, width = 8, height = 6)





# Gráfico 8 : Relação ligação de homens e mulheres ----



linkage_f = import(here("../1_Data/processed/linkage_f.xlsx"))
linkage_m = import(here("../1_Data/processed/linkage_m.xlsx"))

# linkage_f$sexo = 0
# linkage_m$sexo = 1  

linkage_m = linkage_m %>% 
  rename(LSM = ls)

df2 = linkage_f %>% 
  rename(LSF = ls) %>% 
  left_join(linkage_m, by = c("lf", "ano")) %>% 
  left_join(lf_labels, by = "lf") %>% 
  mutate(LSF = round(LSF, digits = 3),
         LSM = round(LSM, digits = 3))


c2 = cor(df2$LSF, df2$LSM) # 0.84


cor_ls = df2 %>% 
  ggplot(aes(x = LSF,
             y = LSM)) +
  geom_point(size = 3, alpha = .5) +
  
  labs(
    x = "Medida M para mulheres",
    y = "Medida M para homens",
    title = "Relação entre a ligação de homens e mulheres",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 11,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~ano)

cor_ls


ggsave(plot     = cor_ls,
       filename = here("../3_Outputs/graf8.png"),
       device = "png", dpi = 600, width = 6, height = 5)



# Gráfico 9 : Relação entre tamanho e ligação geral ----

lf = lf = import(here("../1_Data/processed/lf_painel.fst")) %>% 
  filter(is.na(sexo)) 

tamanho_ls = lf %>% 
  ggplot(aes(x = log(n),
             y = ls)) +
  geom_point(size = 3, alpha = .5) +
  
  labs(
    y = "Medida M do nível-área",
    x = "Tamanho do nível-área (em log)",
    title = "Relação entre o tamanho e a ligação do nível-área",
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 11,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~ano)

tamanho_ls


ggsave(plot     = tamanho_ls,
       filename = here("../3_Outputs/graf9.png"),
       device = "png", dpi = 600, width = 6, height = 5)



# Gráfico 10: Relação entre  percentual de mulheres e ls -----

percentual_ls = lf %>% 
  ggplot(aes(x = perc_mulheres,
             y = ls)) +
  geom_point(size = 3, alpha = .5) +
  
  labs(
    x = "Percentual de mulheres no nível-área",
    y = "Medida M do nível-área",
    title = paste0("Relação entre o percentual de mulheres", "\n", "e a ligação do nível-área"),
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 11,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~ano)

percentual_ls


ggsave(plot     = percentual_ls,
       filename = here("../3_Outputs/graf10.png"),
       device = "png", dpi = 600, width = 6, height = 5)



# Gráfico 11: Relação entre participação e ls ----

lf = import(here("../1_Data/processed/lf_painel.fst")) %>% 
  filter(is.na(sexo)) 

part = import(here("../1_Data/processed/part_lf.xlsx")) %>%   
  filter(sexo == 1) 

lf = lf %>% 
  select(lf, ano, ls) %>% 
  left_join(part, lf, by = c("lf", "ano"))

part_ls = lf %>% 
  ggplot(aes(x = part,
             y = ls)) +
  geom_point(size = 3, alpha = .5) +
  
  labs(
    x = "Participação de mulheres no nível-área",
    y = "Medida M do nível-área",
    title = paste0("Relação entre a participação de mulheres","\n", "e a ligação do nível-área"),
    subtitle = "Brasil, 1980 a 2010",
    caption = paste("Fonte: IBGE, Microdados dos Censos Demográficos (1980,1991,2000 e 2010)",
                    "\n",
                    "Elaboração Própria")) +
  theme_bw() +
  #theme_bw(base_family = "Roboto Condensed") +
  theme(axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10,
                                        angle = 90),
        
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(size = 11,
                                        face = "bold", 
                                        colour = "white"),
        
        plot.title       = element_text(size = 14,
                                        face = "bold"),
        plot.subtitle    = element_text(size = 12),
        plot.caption     = element_text(size = 8),
        
        legend.position = "bottom") +
  facet_wrap(~ano)

part_ls


ggsave(plot     = part_ls,
       filename = here("../3_Outputs/graf11.png"),
       device = "png", dpi = 600, width = 6, height = 5)




