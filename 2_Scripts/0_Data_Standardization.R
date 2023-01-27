# Script for data preparation
# Authors: Carolina Medeiros, Rogerio Barbosa, Flavio Carvalhaes

# Packages ---------------------------------------------------------------------

library(rio)
library(tidyverse)
library(tidylog)
library(segregation)
library(Hmisc)
library(here)

#library(weights)
#library(data.table)
#library(questionr) # wtd.table 

# 0. Setup ---------------------------------------------------------------------

#rm(list = ls());gc()
options(scipen = 999)

raw_data_wd       = "../1_Data/Censos/"
processed_data_wd = "../1_Data/processed/"

if(dir.exists(processed_data_wd)){
        unlink(here(processed_data_wd), recursive = T, force = T, expand = T)
}else{
        dir.create(processed_data_wd)
}

anos <- c(1980,1991,2000,2010)


# 1. Processing Microdata ------------------------------------------------------

#i = 1
stacked_census = tibble()
for (i in 1: length(anos)){
        
        ano_i = anos[i]
        print(ano_i)
        
        file_to_open_i = paste0("Census_", ano_i, "_Harmonized_AllVariables.fst", sep = "")
        file_to_save_i = paste0("CensusProcessed_", ano_i, "_18-65y.fst", sep = "")
        
        ## 1.1 - Opening file ----
        print("--- Opening file")
        
        c = import(here(raw_data_wd, file_to_open_i),
                   columns = c("year",
                               "stateMinimumComparable",
                               "regionMinimumComparable",
                               "wgtperson",
                               "male",
                               "race", 
                               "age", 
                               "levelattnd",
                               "educationAttainment",
                               "fieldsOfStudy",
                               "fieldsOfStudyAggreg",
                               "econActivity",
                               "occupationalStatus", 
                               "classWorker", 
                               "isco88",
                               "MainJobIncome2010Values")
                   )
        
        gc() 
                
        ## 1.2 - Recoding ----
        print("--- Recoding")
        c <- c %>%
                rename ("ano" = year,
                        "peso" = wgtperson,
                        "sexo" = male, 
                        "raca" = race, 
                        "idade" = age, 
                        "estado" = stateMinimumComparable,
                        "regiao" = regionMinimumComparable,
                        "educ" = educationAttainment,
                        "areadeestudo" = fieldsOfStudy,
                        "status_ocup" = occupationalStatus, 
                        "classe_trab" = classWorker, 
                        "ocupacao" = isco88,
                        "renda_trab" = MainJobIncome2010Values) %>% 
                filter(idade >= 18,
                       idade <= 65) %>%
                mutate(ocupacao = as.numeric(ocupacao),
                       ocup3dig = trunc(ocupacao/10),
                       ocup2dig = trunc(ocupacao/100),
                       ocup1dig = trunc(ocupacao/1000),
                       educ     = case_when(educ == 1 ~ 1,
                                            educ == 2 ~ 2,
                                            educ == 3 ~ 2,
                                            educ == 4 ~ 2,
                                            educ == 5 ~ 3,
                                            educ == 6 ~ 3,
                                            educ == 7 ~ 4,
                                            educ == 8 ~ 4,
                                            educ == 9 ~ 5,
                                            educ == 99 ~ 9),
                       
                       lf       = paste0(educ,areadeestudo))
        
        ## 1.3 - Saving the processed file ----
        print("--- Saving the processed file")
        export(c, here(processed_data_wd, file_to_save_i))
        
        ## 1.4 - Stacking ----
        print("--- Stacking")
        stacked_census = bind_rows(stacked_census, c)
        
        # 1.5 - Cleaning memory
        print("--- Cleaning memory")
        rm(c); gc()
        
}
 
gc();Sys.sleep(1);gc()

# 2. Saving the stacked data ---------------------------------------------------

export(stacked_census, here(processed_data_wd, "stacked_census.fst"))
gc();Sys.sleep(1);gc()


# 3. Aggregating data ----------------------------------------------------------
       
## 3.1 - Calculating local linkage by gender, all years ----

localLinkage_all = 
        map_dfr(anos, 
            .f = function(ano_i){
                    print(ano_i)
                    
                    gc() 
                    
                    gender_number = list(c(0,1), c(0), c(1))
                    gender_label  = list("all", "female", "male")
                    
                    map_dfr(.x = 1:3,
                            function(j){
                                    print(paste("----", gender_label[j]))
                                    
                                    gc() 
                                    
                                    stacked_census %>%
                                            filter(!is.na(ocupacao),
                                                   !is.na(lf),
                                                   ano == ano_i,
                                                   sexo %in% unlist(gender_number[j])) %>%
                                            mutual_local(group = "ocup3dig",
                                                         unit = "lf",
                                                         weight = "peso", 
                                                         wide = T) %>%
                                            mutate(year   = ano_i,
                                                   gender = gender_label[j])
                            })
            })
        
localLinkage_all <- tibble(localLinkage_all) %>% unnest(cols = gender)

gc();Sys.sleep(1);gc()
        

## 3.2 - Labor market participation by level-fields, gender, all years -------- 

participation = bind_rows(stacked_census %>%
                                  group_by(ano, lf) %>%
                                  summarise(part = wtd.mean(econActivity, peso)*100) %>%
                                  mutate(gender = "all") %>%
                                  ungroup() %>%
                                  rename(year = ano),
                          
                          stacked_census %>%
                                  mutate(gender = ifelse(sexo == 1, "male", "female")) %>%
                                  group_by(ano, lf, gender) %>%
                                  summarise(part = wtd.mean(econActivity, peso)*100) %>%
                                  ungroup() %>%
                                  rename(year = ano))


        
## 3.3 - Descriptives by level-field, by gender, all years ----
        
descriptives <- bind_rows(
        stacked_census %>% 
                group_by(ano, lf) %>% 
                summarise(n = sum(peso),
                          perc_homens   = wtd.mean(sexo, peso),
                          perc_mulheres = wtd.mean(sexo == 0, peso),
                          perc_brancos  = wtd.mean(raca == 1, peso),
                          perc_negros   = wtd.mean(raca == 3, peso),
                          renda_media   = wtd.mean(renda_trab, peso),
                          renda_var     = wtd.var(renda_trab, peso),
                          idade_media   = wtd.mean(idade, peso)) %>%
                ungroup() %>%
                mutate(gender = "all") %>%
                rename(year = ano),
        
        stacked_census %>% 
                mutate(gender = ifelse(sexo == 1, "male", "female")) %>%
                group_by(ano, lf, gender) %>% 
                summarise(n = sum(peso),
                          perc_homens   = NA_real_,
                          perc_mulheres = NA_real_,
                          perc_brancos  = wtd.mean(raca == 1, peso),
                          perc_negros   = wtd.mean(raca == 3, peso),
                          renda_media   = wtd.mean(renda_trab, peso),
                          renda_var     = wtd.var(renda_trab, peso),
                          idade_media   = wtd.mean(idade, peso)) %>%
                ungroup() %>%
                rename(year = ano)
        )

## 3.5 - Gathering all the aggregate datasets

d <- localLinkage_all %>%
        left_join(participation, by = c("year", "gender", "lf")) %>%
        left_join(descriptives, by = c("year", "gender", "lf"))

export(d, here(processed_data_wd, "data_levelfields_by_gender.fst"))



