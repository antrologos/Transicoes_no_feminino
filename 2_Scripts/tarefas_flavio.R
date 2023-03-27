# Tabelas Flávio

# 0. Packages and Setup --------------------------------------------------------

library(rio)
library(tidyverse)
library(tidylog)
library(segregation)
library(Hmisc)
library(here)

#rm(list = ls());gc()
options(scipen = 999)

data_wd = "../1_Data/processed/"

if(!dir.exists(here("3_Output"))){
        dir.create(here("3_Output"))
}

# 1. Opening Data --------------------------------------------------------------

d_stacked <- import(here(data_wd, "stacked_census.fst"))
d_aggreg  <- import(here(data_wd, "data_levelfields_by_gender.fst"))

gc()


# Tarefa 1: M por grupo ------
#Rodar M com sexo no grupo, exemplo acima. Rodar para 1980 e 2010 apenas

m_grupo <- d_stacked %>%
        group_by(year) %>% ##aqui pode ser filtro dependendo de como estão os dados, se empilhados ou não
        do(mutual_total(., 
                        group = c("gender", "lf"), # aqui não parece importar a ordem
                        unit = "ocup3dig", 
                        weight = "wgt")) 

export(m_grupo, here(data_wd, "m_grupo.xlsx"))


# Criar um objeto pra homens e outro pra mulheres. Linhas: credenciais educacionais. Colunas: local linkage e proporção por ano ----


homens = d_aggreg%>% 
        filter(gender == "Male") %>% 
        select(lf, ls, year, gender) %>% 
        left_join(d_aggreg %>% filter(gender == "All") %>% select(lf, year, perc_women, perc_men), by = c("lf", "year"))

export(homens, here(data_wd, "homens.xlsx"))


mulheres = d_aggreg%>% 
        filter(gender == "Female") %>% 
        select(lf, ls, year, gender) %>% 
        left_join(d_aggreg %>% filter(gender == "All") %>% select(lf, year, perc_women, perc_men), by = c("lf", "year"))

export(mulheres, here(data_wd, "mulheres.xlsx"))

#Criar um objeto com as top 10 ocupações ordenadas de forma decrescente dentro de cada credencial educacional -----

#aproveitei e fiz para gênero tbm

ocup_labels <- import(here("1_Auxiliary_Data", "isco_label_3dig.xlsx")) %>% 
        rename(ocup3dig = isco88_3digit)

export(ocup_labels, here("1_Auxiliary_Data", "isco_label_3dig.xlsx"))

top10_geral = d_stacked %>% 
        group_by(year, lf, ocup3dig) %>% 
        summarise(contagem = sum(wgt)) %>% 
        arrange(desc(contagem), .by_group = T) %>% 
        slice_max(contagem, n = 10) %>% 
        left_join(ocup_labels, by = "ocup3dig")


top10_male = d_stacked %>% 
        filter(gender == "Male") %>% 
        group_by(year, lf, ocup3dig) %>% 
        summarise(contagem = sum(wgt)) %>% 
        arrange(desc(contagem), .by_group = T) %>% 
        slice_max(contagem, n = 10) %>% 
        left_join(ocup_labels, by = "ocup3dig")

top10_female = d_stacked %>% 
        filter(gender == "Female") %>% 
        group_by(year, lf, ocup3dig) %>% 
        summarise(contagem = sum(wgt)) %>% 
        arrange(desc(contagem), .by_group = T) %>% 
        slice_max(contagem, n = 10) %>% 
        left_join(ocup_labels, by = "ocup3dig")


export(top10_geral, here(data_wd, "top10_geral.xlsx"))
export(top10_male, here(data_wd, "top10_male.xlsx"))
export(top10_female, here(data_wd, "top10_female.xlsx"))
