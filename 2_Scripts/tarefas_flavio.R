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

m_grupo_sex <- d_stacked %>%
        group_by(year, gender) %>% ##aqui pode ser filtro dependendo de como estão os dados, se empilhados ou não
        do(mutual_total(., 
                        group = c("gender", "lf"), # aqui não parece importar a ordem
                        unit = "ocup3dig", 
                        weight = "wgt")) 

export(m_grupo_sex, here(data_wd, "m_grupo_sex.xlsx"))


ls_sexo = d_aggreg %>% 
        select(lf, ls, year, gender, n, perc_women, perc_men)

export(ls_sexo, here(data_wd, "ls_sexo.xlsx"))

# Criar um objeto pra homens e outro pra mulheres. Linhas: credenciais educacionais. Colunas: local linkage e proporção por ano ----


homens = d_aggreg%>% 
        filter(gender == "Male") %>% 
        select(lf, ls, year, gender) %>% 
        left_join(d_aggreg %>% filter(gender == "All") %>% select(lf, year, perc_women, perc_men), by = c("lf", "year")) %>% 
        rename(ls_homem = ls)

export(homens, here(data_wd, "homens.xlsx"))


mulheres = d_aggreg%>% 
        filter(gender == "Female") %>% 
        select(lf, ls, year, gender) %>% 
        left_join(d_aggreg %>% filter(gender == "All") %>% select(lf, year), by = c("lf", "year")) %>% 
        rename(ls_mulher = ls)

export(mulheres, here(data_wd, "mulheres.xlsx"))

comp = homens %>% 
        left_join(mulheres, by = c("lf", "year")) %>% 
        select(lf, year, ls_homem, ls_mulher, perc_men, perc_women)

export(comp, here(data_wd, "comp_ls_prop.xlsx"))


#Criar um objeto com as top 10 ocupações ordenadas de forma decrescente dentro de cada credencial educacional -----

#aproveitei e fiz para gênero tbm

ocup_labels <- import(here("1_Auxiliary_Data", "isco_label_3dig.xlsx")) %>% 
        rename(ocup3dig = isco88_3digit)

export(ocup_labels, here("1_Auxiliary_Data", "isco_label_3dig.xlsx"))

t1 = d_stacked %>% 
        group_by(year, lf, ocup3dig) %>% 
        summarise(contagem = sum(wgt))

t2 = t1 %>% 
        group_by(year, lf) %>% 
        summarise(total = sum(contagem))

t1 = t1 %>% left_join(t2, by = c("year", "lf")) %>% 
        mutate(prop = (contagem/total) *100)

top10_geral = t1 %>% 
        arrange(desc(contagem), .by_group = T) %>% 
        slice_max(contagem, n = 11) %>% 
        left_join(ocup_labels, by = "ocup3dig")




#Homens


t1 = d_stacked %>% 
        filter(gender == "Male") %>% 
        group_by(year, lf, ocup3dig) %>% 
        summarise(contagem = sum(wgt))

t2 = t1 %>% 
        group_by(year, lf) %>% 
        summarise(total = sum(contagem))

t1 = t1 %>% left_join(t2, by = c("year", "lf")) %>% 
        mutate(prop = (contagem/total) *100)

top10_male = t1 %>% 
        arrange(desc(contagem), .by_group = T) %>% 
        slice_max(contagem, n = 11) %>% 
        left_join(ocup_labels, by = "ocup3dig")



# Mulheres 

t1 = d_stacked %>% 
        filter(gender == "Female") %>% 
        group_by(year, lf, ocup3dig) %>% 
        summarise(contagem = sum(wgt))

t2 = t1 %>% 
        group_by(year, lf) %>% 
        summarise(total = sum(contagem))

t1 = t1 %>% left_join(t2, by = c("year", "lf")) %>% 
        mutate(prop = (contagem/total) *100)

top10_female = t1 %>% 
        arrange(desc(contagem), .by_group = T) %>% 
        slice_max(contagem, n = 11) %>% 
        left_join(ocup_labels, by = "ocup3dig")


export(top10_geral, here(data_wd, "top10_geral_prop.xlsx"))
export(top10_male, here(data_wd, "top10_male_prop.xlsx"))
export(top10_female, here(data_wd, "top10_female_prop.xlsx"))


# Decomposição dinâmica adicionando marginais ocupacionais ( p homem e p mulher ?)
# Atualizar pacote

c80 = import(here("../1_Data/processed/CensusProcessed_1980_18-65y.fst"))
c91 = import(here("../1_Data/processed/CensusProcessed_1991_18-65y.fst"))
c00 = import(here("../1_Data/processed/CensusProcessed_2000_18-65y.fst"))
c10 = import(here("../1_Data/processed/CensusProcessed_2010_18-65y.fst"))

## 1980-2010----
dif1 = mutual_difference(c80, c10, "ocup3dig", "lf", weight = "wgt")


# stat           est
# 1:             M1  0.2422805296
# 2:             M2  0.2572144284
# 3:           diff  0.0149338987
# 4:      additions -0.0003072113
# 5:       removals  0.0000000000
# 6: group_marginal  0.0551764588
# 7:  unit_marginal  0.0669016476
# 8:     structural -0.1068369963

rm(c80)
gc()
##1991 -2010----

c91 = import(here("../1_Data/processed/CensusProcessed_1991_18-65y.fst"))

dif2 = mutual_difference(c91, c10, "ocup3dig", "lf", weight = "wgt")

# stat                       est
# 1:             M1  0.2554952945040188105885
# 2:             M2  0.2572144283628715610845
# 3:           diff  0.0017191338588527504960
# 4:      additions  0.0009875940110600778254
# 5:       removals -0.0000000000000001665335
# 6: group_marginal  0.0485690835045495816757
# 7:  unit_marginal  0.0249770466577860505986
# 8:     structural -0.0728145903145427930703

rm(c91)
gc()

## 2000-2010 ----

c00 = import(here("../1_Data/processed/CensusProcessed_2000_18-65y.fst"))


dif3 = mutual_difference(c00, c10, "ocup3dig", "lf", weight = "wgt")

# stat          est
# 1:             M1  0.248106902
# 2:             M2  0.257214428
# 3:           diff  0.009107527
# 4:      additions  0.000000000
# 5:       removals  0.000000000
# 6: group_marginal  0.035582362
# 7:  unit_marginal  0.024466470
# 8:     structural -0.050941306


rm(c00, c10)
gc()

