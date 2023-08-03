# Prepara dados

# Pacotes e configuração ----

install.packages("segregation")

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
library(here)


# 1. Abrindo arquivos ----

#Criando vetor anos e objeto com caminho para bases de dados

        # dá para importar selecionando as variáveis (colunas) já
        #se tiver em csv, o argumento é select 

a <- read_fst(here("1_Data/Censos/Census_2000_Harmonized_AllVariables.fst"))
b <- read_fst(here("1_Data/Censos/Census_2010_Harmonized_AllVariables.fst"))

        
        # 1.2 - Recodificando variaveis ---- 
        a <- a %>%
                rename ("ano" = year,
                        "peso" = wgtperson,
                        "sexo" = male, 
                        "raça" = race, 
                        "idade" = age, 
                        "estudo" = levelattnd,
                        "educ" = educationAttainment,
                        "areadeestudo" = fieldsOfStudyAggreg,
                        "status_ocup" = occupationalStatus, 
                        "classe_trab" = classWorker, 
                        "ocupacao" = isco88,
                        "renda_trab" = MainJobIncome2010Values) %>% 
                filter(idade >= 18,
                       idade <= 65) %>%                   
                mutate(ocupacao = as.numeric(ocupacao),     # Criando variáveis de ocupações agregada e desagregada
                       ocup3dig = trunc(ocupacao/10),
                       ocup2dig =  trunc(ocupacao/100),
                       ocup1dig = trunc(ocupacao/1000),
                       educ = case_when( educ == 1 ~ 1      # Agregando níveis educacionais
                                         ,educ == 2 ~ 2
                                         ,educ == 3 ~ 2
                                         ,educ == 4 ~ 2
                                         ,educ == 5 ~ 3
                                         ,educ == 6 ~ 3
                                         ,educ == 7 ~ 4
                                         ,educ == 8 ~ 4
                                         ,educ == 9 ~ 5
                                         ,educ == 99 ~ 9),
                       lf = paste0(educ,areadeestudo))         # Criando variável level-field como junção de nível e área de estudo

b <- b %>%
      rename ("ano" = year,
              "peso" = wgtperson,
              "sexo" = male, 
              "raça" = race, 
              "idade" = age, 
              "estudo" = levelattnd,
              "educ" = educationAttainment,
              "areadeestudo" = fieldsOfStudyAggreg,
              "status_ocup" = occupationalStatus, 
              "classe_trab" = classWorker, 
              "ocupacao" = isco88,
              "renda_trab" = MainJobIncome2010Values) %>% 
      filter(idade >= 18,
             idade <= 65) %>%                   
      mutate(ocupacao = as.numeric(ocupacao),     # Criando variáveis de ocupações agregada e desagregada
             ocup3dig = trunc(ocupacao/10),
             ocup2dig =  trunc(ocupacao/100),
             ocup1dig = trunc(ocupacao/1000),
             educ = case_when( educ == 1 ~ 1      # Agregando níveis educacionais
                               ,educ == 2 ~ 2
                               ,educ == 3 ~ 2
                               ,educ == 4 ~ 2
                               ,educ == 5 ~ 3
                               ,educ == 6 ~ 3
                               ,educ == 7 ~ 4
                               ,educ == 8 ~ 4
                               ,educ == 9 ~ 5
                               ,educ == 99 ~ 9),
             lf = paste0(educ,areadeestudo))         # Criando variável level-field como junção de nível e área de estudo



c80 = import(here("../1_Data/processed/CensusProcessed_1980_18-65y.fst"))
c91 = import(here("../1_Data/processed/CensusProcessed_1991_18-65y.fst"))
c00 = import(here("../1_Data/processed/CensusProcessed_2000_18-65y.fst"))
c10 = import(here("../1_Data/processed/CensusProcessed_2010_18-65y.fst"))



# Calculando quantas mulheres de cada nível educacional há em cada ocupação nos anos 2000

sexo0_2000 <- c00 %>%
      drop_na(gender, lf, ocup3dig) %>%
      filter(gender== "Female") %>% 
      group_by(lf, ocup3dig)%>%
      summarise(sum(wgt)) %>%
      ungroup()

# Calculando quantos homens de cada nível educacional há em cada ocupação nos anos 2000


sexo1_2000 <- c00 %>%
        drop_na(gender, lf, ocup3dig) %>%
        filter(gender== "Male") %>% 
        group_by(lf, ocup3dig)%>%
        summarise(sum(wgt)) %>%
        ungroup()

# Calculando quantas mulheres de cada nível educacional há em cada ocupação nos anos 2010


sexo0_2010 <- c10 %>%
        drop_na(gender, lf, ocup3dig) %>%
        filter(gender== "Female") %>% 
        group_by(lf, ocup3dig)%>%
        summarise(sum(wgt)) %>%
        ungroup()

# Calculando quantos homens de cada nível educacional há em cada ocupação nos anos 2010

sexo1_2010 <- c10 %>%
        drop_na(gender, lf, ocup3dig) %>%
        filter(gender== "Male") %>% 
        group_by(lf, ocup3dig)%>%
        summarise(sum(wgt)) %>%
        ungroup()

rm(c00, c10)

########################################################
###############################3. Decomposição dinâmica#######
##############################################################


estrutural_sexo0 <- mutual_difference(sexo0_2000, sexo0_2010,
                                group="lf", unit="ocup3dig", weight = "sum(wgt)",
                                method = "shapley_detailed")


# marginal, grupo
marginal_grupo_sexo0 <- mutual_difference(sexo0_2000, sexo0_2010,
                                      group="ocup3dig", unit="lf", weight = "sum(wgt)",
                                      method = "shapley_detailed_marginal")

# marginal, unidade
marginal_unidade_sexo0 <- mutual_difference(sexo0_2000, sexo0_2010,
                                    group="lf", unit="ocup3dig", weight = "sum(wgt)",
                                    method = "shapley_detailed_marginal")

#Juntando os três
bc <- bind_rows(estrutural_sexo0, marginal_grupo_sexo0, marginal_unidade_sexo0)


export()
