# Script for data preparation:
# 1) Recodes and filters microdata (and saves yearly and stacked datasets)
# 2) Produces a level-field aggregated and stacked dataset

# Authors: Carolina Medeiros, Rogerio Barbosa, Flavio Carvalhaes


# 0. Packages and Setup ---------------------------------------------------------------------

library(rio)
library(tidyverse)
library(tidylog)
library(segregation)
library(Hmisc)
library(here)

#rm(list = ls());gc()
options(scipen = 999)

raw_data_wd       = "../1_Data/Censos/"
processed_data_wd = "../1_Data/processed/"

if(dir.exists(processed_data_wd)){
        unlink(here(processed_data_wd), recursive = T, force = T, expand = T)
        dir.create(processed_data_wd)
}else{
        dir.create(processed_data_wd)
}

years <- c(1980,1991,2000,2010)

isced_labels <- import(here("1_Auxiliary_Data", "isced3_labels.xlsx")) %>%
        select(fieldsOfStudy = isced_level3, 
               isced_labels  = isced_level3_label_en)

# 1. Processing Microdata ------------------------------------------------------

#i = 1
stacked_census = tibble()
for (i in 1: length(years)){
        
        year_i = years[i]
        print(year_i)
        
        file_to_open_i = paste0("Census_", year_i, "_Harmonized_AllVariables.fst", sep = "")
        file_to_save_i = paste0("CensusProcessed_", year_i, "_18-65y.fst", sep = "")
        
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
                rename(wgt      = wgtperson,
                       state    = stateMinimumComparable,
                       region   = regionMinimumComparable,
                       educ     = educationAttainment,
                       gender   = male,
                       earnings = MainJobIncome2010Values) %>%
                
                filter(age >= 18,
                       age <= 65) %>%
                
                left_join(isced_labels) %>%
        
                mutate(isco88 = as.numeric(isco88),
                       ocup3dig = trunc(isco88/10),
                       ocup2dig = trunc(isco88/100),
                       ocup1dig = trunc(isco88/1000),
                       
                       gender = ifelse(gender == 0, "Female", "Male"),
                       
                       race = case_when(race %in% 1 ~ "White",
                                        race %in% 2 ~ "Mixed Race",
                                        race %in% 3 ~ "Black",
                                        TRUE ~ NA_character_),
                       
                       educ_aggreg = case_when(educ %in% 1  ~ "1. None",         # 1. None
                                               educ %in% 2  ~ "1. None",         # 2. Primary Incomplete
                                               educ %in% 3  ~ "2. Primary",      # 3. Primary Complete
                                               educ %in% 4  ~ "2. Primary",      # 4. Mid School Incomplete
                                               educ %in% 5  ~ "3. Mid School",   # 5. Mid School Complete
                                               educ %in% 6  ~ "3. Mid School",   # 6. High School Incomplete
                                               educ %in% 7  ~ "4. High School",  # 7. High School Complete
                                               educ %in% 8  ~ "4. High School",  # 8. Higher Ed. Incomplete
                                               educ %in% 9  ~ "5. Higher Ed.",   # 9. Higher Ed. Complete
                                               educ %in% 99 ~ NA_character_),    #99. Other levels or unknown
                       
                       lf       = paste(educ_aggreg, 
                                        isced_labels,
                                        sep = " - "),
                       
                       lf       = str_remove(lf, " - NA"))
        
        
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
        map_dfr(years, 
            .f = function(year_i){
                    print(year_i)
                    
                    gc() 
                    
                    gender_cat   = list(c("Female", "Male"), "Female", "Male")
                    gender_label = list("All", "Female", "Male")
                    
                    map_dfr(.x = 1:3,
                            function(j){
                                    print(paste("----", gender_label[j]))
                                    
                                    gc() 
                                    
                                    stacked_census %>%
                                            filter(!is.na(isco88),
                                                   !is.na(lf),
                                                   year == year_i,
                                                   gender %in% unlist(gender_cat[j])) %>%
                                            mutual_local(group = "ocup3dig",
                                                         unit = "lf",
                                                         weight = "wgt", 
                                                         wide = T) %>%
                                            mutate(year   = year_i,
                                                   gender = gender_label[j])
                            })
            })
        
localLinkage_all <- tibble(localLinkage_all) %>% unnest(cols = gender)

gc();Sys.sleep(1);gc()
        

## 3.2 - Descriptives by level-field, by gender, all years ----
        
descriptives <- bind_rows(
        stacked_census %>% 
                mutate(log_earnings = log(earnings),
                       log_earnings = ifelse(!is.finite(log_earnings), NA_real_, log_earnings),
                       log_earnings = ifelse(is.nan(log_earnings),     NA_real_, log_earnings)) %>%
                group_by(year, lf) %>% 
                summarise(n = sum(wgt),
                          perc_women      = wtd.mean(gender %in% "Female", wgt),
                          perc_men        = wtd.mean(gender %in% "Male", wgt),
                          perc_white      = wtd.mean(race %in% "White", wgt),
                          perc_nonWhite   = wtd.mean(!(race %in% "White"), wgt), # havia um erro aqui
                          perc_laborMarket = wtd.mean(econActivity, wgt),
                          perc_occupied    = wtd.mean(occupationalStatus, wgt),
                          mean_earnings   = wtd.mean(earnings, wgt),
                          logVar_earnings = wtd.var(log_earnings, wgt),
                          mean_age        = wtd.mean(age, wgt)) %>%
                ungroup() %>%
                mutate(gender = "All") %>%
                rename(year = year),
        
        stacked_census %>% 
                mutate(log_earnings = log(earnings),
                       log_earnings = ifelse(!is.finite(log_earnings), NA_real_, log_earnings),
                       log_earnings = ifelse(is.nan(log_earnings),     NA_real_, log_earnings)) %>%
                group_by(year, lf, gender) %>% 
                summarise(n = sum(wgt),
                          perc_women       = NA_real_,
                          perc_men         = NA_real_,
                          perc_white       = wtd.mean(race %in% "White", wgt),
                          perc_nonWhite    = wtd.mean(!(race %in% "White"), wgt), # havia um erro aqui
                          perc_laborMarket = wtd.mean(econActivity, wgt),
                          perc_occupied    = wtd.mean(occupationalStatus, wgt),
                          mean_earnings    = wtd.mean(earnings, wgt),
                          logVar_earnings = wtd.var(log_earnings, wgt),
                          mean_age         = wtd.mean(age, wgt)) %>%
                ungroup() %>%
                rename(year = year))

gc() 

## 3.3 - Gathering all the aggregate datasets ----

d <- localLinkage_all %>%
        left_join(descriptives, by = c("year", "gender", "lf"))

export(d, here(processed_data_wd, "data_levelfields_by_gender.fst"))


gc() 
