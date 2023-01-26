# Prepara dados

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
library(here)


# 1. Abrindo arquivos ----

#Criando vetor anos e objeto com caminho para bases de dados

anos <- c(1980,1991,2000,2010)

i = 1
for (i in 1: length(anos)){
        print(i)
        
        ano_i = anos[i]
        
        # 1.1 - Abrindo o  censo ----
        file_to_open_i = paste0("Census_", ano_i, "_Harmonized_AllVariables.fst", sep = "")
        file_to_save_i = paste0("censo", ano_i, ".fst", sep = "") # 
        
        
        
        
        # dá para importar selecionando as variáveis (colunas) já
        #se tiver em csv, o argumento é select 
        
        c = read_fst(here("../1_Data/Censos/", file_to_open_i),
                   columns = c("year",
                               "wgtperson",
                               "male",
                               "race", 
                               "age", 
                               "levelattnd",
                               "econActivity",
                               "educationAttainment",
                               "fieldsOfStudyAggreg",
                               "occupationalStatus", 
                               "classWorker", 
                               "isco88",
                               "MainJobIncome2010Values"), to = 1000 )
        
        
        # 1.2 - Recodificando variaveis ---- 
        c <- c %>%
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
        
        
        # Calculando total linkage geral, para homens e para mulheres e salvando em separado
        
        indsegtg <- mutual_total(c,
                                 group = "ocup3dig",
                                 unit = "lf",
                                 weight = "peso")
        indsegtg$ano = as.numeric(ano_i)
        
        
        cm <- c %>% 
                filter(sexo == 1)
        
        
        indsegtm <- mutual_total(cm,
                                 group = "ocup3dig",
                                 unit = "lf",
                                 weight = "peso")
        indsegtm$ano = as.numeric(ano_i)
        
        cf <- c %>% 
                filter(sexo == 0)
        
        
        indsegtf <- mutual_total(cf,
                                 group = "ocup3dig",
                                 unit = "lf",
                                 weight = "peso")
        indsegtf$ano = as.numeric(ano_i)
        
        
        linkage_tg = paste0("\\linkage_tg", ano_i, ".xlsx", sep = "")
        linkage_tf = paste0("\\linkage_tf", ano_i, ".xlsx", sep = "")
        linkage_tm = paste0("\\linkage_tm", ano_i, ".xlsx", sep = "")
        
        export(indsegtg, paste0(wd3, linkage_tg))
        export(indsegtf, paste0(wd3, linkage_tf))
        export(indsegtm, paste0(wd3, linkage_tm))
        
        
        # 1.3 - Calculando  local linkage por nível-área ----
        indseg <- mutual_local(c, 
                               group = "ocup3dig", 
                               unit = "lf", 
                               weight = "peso", 
                               wide = TRUE)
        
        
        # 1.4 - Produzindo dados descritivos por nível-área ----
        curso <- c %>% 
                group_by(lf) %>% 
                summarise( n = sum(peso),
                           perc_homens   = wtd.mean(sexo, peso),
                           perc_mulheres = wtd.mean(sexo == 0, peso),
                           perc_brancos  = wtd.mean(raça == 1, peso),
                           perc_negros   = wtd.mean(raça == 3, peso),
                           renda_media   = wtd.mean(renda_trab, peso),
                           renda_var     = Hmisc::wtd.var(renda_trab, peso),
                           idade_media   = wtd.mean(idade, peso),
                           idade_var     = Hmisc::wtd.var(idade, peso)
                ) %>%
                ungroup()
        
        # 1.5 - Juntando local linkage com dados por nível-área ----
        curso <- curso %>% 
                left_join(indseg, by = "lf")
        
        
        
        # 1.6 - Produzindo dados descritivos por nível-área e SEXO ----
        curso_sexo <- c %>% 
                group_by(lf, sexo) %>% 
                summarise( n = sum(peso),
                           perc_homens   = wtd.mean(sexo, peso),
                           perc_mulheres = wtd.mean(sexo == 0, peso),
                           perc_brancos  = wtd.mean(raça == 1, peso),
                           perc_negros   = wtd.mean(raça == 3, peso),
                           renda_media   = wtd.mean(renda_trab, peso),
                           renda_var     = Hmisc::wtd.var(renda_trab, peso),
                           idade_media   = wtd.mean(idade, peso),
                           idade_var     = Hmisc::wtd.var(idade, peso)
                ) %>%
                ungroup()
        
        
        
        
        # 1.7 - Calculando local linkage para homens ---- 
        cm <- c %>% 
                filter(sexo == 1)
        
        indseg2 <- mutual_local(cm, "ocup3dig", "lf", weight = "peso", wide = TRUE)
        indseg2$sexo = 1
        
        
        # 1.7 - Calculando local linkage para mulheres ---- 
        cf <- c %>% 
                filter(sexo == 0)
        
        indseg3 <- mutual_local(cf, "ocup3dig", "lf", weight = "peso", wide = TRUE)
        indseg3$sexo = 0
        
        
        # 1.8 - Juntando dados descritivo por nível-área-sexo + local linkage
        curso_sexo = left_join(curso_sexo, bind_rows(indseg2, indseg3), by = c("lf", "sexo"))
        
        curso = curso %>% 
                mutate(sexo = NA_real_)
        
        curso = bind_rows(curso, curso_sexo)
        
        curso$ano = as.numeric(ano_i)
        
        # Extra: salvando local linkages em separado (ineficiente, mas já tinha feito assim, tenho de adaptar os gráficos)
        
        linkage_g = paste0("\\linkage_g", ano_i, ".xlsx", sep = "")
        linkage_f = paste0("\\linkage_f", ano_i, ".xlsx", sep = "")
        linkage_m = paste0("\\linkage_m", ano_i, ".xlsx", sep = "")
        
        export(indseg, paste0(wd3, linkage_g))
        export(indseg2, paste0(wd3, linkage_f))
        export(indseg3, paste0(wd3, linkage_m))
        
        
        rm(c)
        
        export(curso, paste0(wd3, file_to_save_i))
        
        gc()
}


# 2. Empilhando censos----

curso80 = read.fst(paste0(wd3,"/censo1980.fst"))
curso91 = read.fst(paste0(wd3,"/censo1991.fst"))
curso00 = read.fst(paste0(wd3,"/censo2000.fst"))
curso10 = read.fst(paste0(wd3,"/censo2010.fst"))


cursos_painel = bind_rows(curso80,curso91,curso00,curso10) 

export(cursos_painel, paste0(wd3,"/lf_painel.fst")) ## lembrar desse nome depois para os modelos  

linkage_tg1980 = import(paste0(wd3,"/linkage_tg1980.xlsx"))
linkage_tg1991 = import(paste0(wd3,"/linkage_tg1991.xlsx"))
linkage_tg2000 = import(paste0(wd3,"/linkage_tg2000.xlsx"))
linkage_tg2010 = import(paste0(wd3,"/linkage_tg2010.xlsx"))

linkage_tg = bind_rows(linkage_tg1980,
                       linkage_tg1991,
                       linkage_tg2000,
                       linkage_tg2010)

export(linkage_tg, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield\\linkage_tg.xlsx")


linkage_tf1980 = import(paste0(wd3,"/linkage_tf1980.xlsx"))
linkage_tf1991 = import(paste0(wd3,"/linkage_tf1991.xlsx"))
linkage_tf2000 = import(paste0(wd3,"/linkage_tf2000.xlsx"))
linkage_tf2010 = import(paste0(wd3,"/linkage_tf2010.xlsx"))

linkage_tf = bind_rows(linkage_tf1980,
                       linkage_tf1991,
                       linkage_tf2000,
                       linkage_tf2010)
export(linkage_tf, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield\\linkage_tf.xlsx")


linkage_tm1980 = import(paste0(wd3,"/linkage_tm1980.xlsx"))
linkage_tm1991 = import(paste0(wd3,"/linkage_tm1991.xlsx"))
linkage_tm2000 = import(paste0(wd3,"/linkage_tm2000.xlsx"))
linkage_tm2010 = import(paste0(wd3,"/linkage_tm2010.xlsx"))

linkage_tm = bind_rows(linkage_tm1980,
                       linkage_tm1991,
                       linkage_tm2000,
                       linkage_tm2010)


export(linkage_tm, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield\\linkage_tm.xlsx")

# linkages local segregation
linkage_g1980 = import(paste0(wd3,"/linkage_g1980.xlsx"))
linkage_g1991 = import(paste0(wd3,"/linkage_g1991.xlsx"))
linkage_g2000 = import(paste0(wd3,"/linkage_g2000.xlsx"))
linkage_g2010 = import(paste0(wd3,"/linkage_g2010.xlsx"))

linkage_g = bind_rows(         linkage_g1980,
                               linkage_g1991,
                               linkage_g2000,
                               linkage_g2010)

export(linkage_g, 
       "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield\\linkage_g.xlsx")

linkage_f1980 = import(paste0(wd3,"/linkage_f1980.xlsx"))
linkage_f1991 = import(paste0(wd3,"/linkage_f1991.xlsx"))
linkage_f2000 = import(paste0(wd3,"/linkage_f2000.xlsx"))
linkage_f2010 = import(paste0(wd3,"/linkage_f2010.xlsx"))

linkage_f = bind_rows(         linkage_f1980,
                               linkage_f1991,
                               linkage_f2000,
                               linkage_f2010)

export(linkage_f, 
       "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield\\linkage_f.xlsx")

linkage_m1980 = import(paste0(wd3,"/linkage_m1980.xlsx"))
linkage_m1991 = import(paste0(wd3,"/linkage_m1991.xlsx"))
linkage_m2000 = import(paste0(wd3,"/linkage_m2000.xlsx"))
linkage_m2010 = import(paste0(wd3,"/linkage_m2010.xlsx"))

linkage_m = bind_rows(         linkage_m1980,
                               linkage_m1991,
                               linkage_m2000,
                               linkage_m2010)

export(linkage_m, 
       "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\levelfield\\linkage_m.xlsx")



# Tratamento extra para dados de participação geral ----
# Censos com desocupados e econ activity

anos <- c(1980,1991,2000,2010)
for (i in 1: length(anos))
{
        print(i)
        ano_i = anos[i]
        
        #para importar o censo com desocupados
        file_to_open_i = paste0("\\Census_", ano_i, "_Harmonized_AllVariables.fst", sep = "")
        file_to_save_i = paste0("\\censodesocup", ano_i, ".fst", sep = "") # 
        
        
        c = import(paste0(wd2, file_to_open_i))
        
        c <- c %>%
                select(year,
                       wgtperson,
                       male,
                       race, 
                       age, 
                       stateMinimumComparable,
                       regionMinimumComparable,
                       educationAttainment,
                       fieldsOfStudy,
                       occupationalStatus, 
                       econActivity,
                       classWorker, 
                       isco88,
                       MainJobIncome2010Values) %>% 
                rename ("ano" = year,
                        "peso" = wgtperson,
                        "sexo" = male, 
                        "raça" = race, 
                        "idade" = age, 
                        "estado" = stateMinimumComparable,
                        "região" = regionMinimumComparable,
                        "educ" = educationAttainment,
                        "areadeestudo" = fieldsOfStudy,
                        "status_ocup" = occupationalStatus, 
                        "ocupacao" = isco88,
                        "renda_trab" = MainJobIncome2010Values) %>% 
                filter(idade >= 18,
                       idade <= 65) %>%
                mutate(ocupacao = as.numeric(ocupacao),
                       ocup3dig = trunc(ocupacao/10),
                       ocup2dig =  trunc(ocupacao/100),
                       ocup1dig = trunc(ocupacao/1000),
                       educ = case_when( educ == 1 ~ 1 
                                         ,educ == 2 ~ 2
                                         ,educ == 3 ~ 2
                                         ,educ == 4 ~ 2
                                         ,educ == 5 ~ 3
                                         ,educ == 6 ~ 3
                                         ,educ == 7 ~ 4
                                         ,educ == 8 ~ 4
                                         ,educ == 9 ~ 5
                                         ,educ == 99 ~ 9),
                       lf = paste0(educ,areadeestudo))
        
        part_geral = paste0("\\part_geral", ano_i, ".xlsx", sep = "")
        part_lf = paste0("\\part_lf", ano_i, ".xlsx", sep = "")
        
        #para salvar os resultados 
        part_sexo = paste0("\\part_sexo", ano_i, ".xlsx", sep = "")
        part_educ = paste0("\\part_educ", ano_i, ".xlsx", sep = "")
        part_area = paste0("\\part_area", ano_i, ".xlsx", sep = "")
        part_lf = paste0("\\part_lf", ano_i, ".xlsx", sep = "")
        part_lfs = paste0("\\part_lfs", ano_i, ".xlsx", sep = "")
        
        # Percentuais de participação no merc de trabalho -----
        
        # Participação geral  
        part <- c %>%
                summarise(part = wtd.mean(econActivity, peso)*100) %>%
                ungroup()
        
        part$ano = as.numeric(ano_i)
        
        export(part, paste0(wd3, part_geral))
        
        #participação geral no merc de trabalho por sexo
        part_s <- c %>%
                group_by(sexo) %>%
                summarise(part = wtd.mean(econActivity, peso)*100
                ) %>%
                ungroup()
        
        part_s$ano = as.numeric(ano_i)
        
        export(part_s, paste0(wd3, part_sexo))
        
        # participação no merc de trabalho por sexo e nível educacional 
        
        peduc <- c %>%
                group_by(sexo, educ) %>%
                summarise(part = wtd.mean(econActivity, peso)*100
                ) %>%
                ungroup()
        
        peduc$ano = as.numeric(ano_i)
        export(peduc, paste0(wd3, part_educ))
        
        
        # participação no merc de trabalho por sexo e área de estudo 
        
        parea <- c %>%
                filter(educ == 5) %>%
                group_by(sexo, areadeestudo) %>%
                summarise(part = wtd.mean(econActivity, peso)*100
                ) %>%
                ungroup()
        
        parea$ano = as.numeric(ano_i)
        export(parea, paste0(wd3, part_area))
        
        #participação no merc de trabalho  levelfield
        plf <- c %>%
                group_by(lf) %>%
                summarise(part = wtd.mean(econActivity, peso)*100
                ) %>%
                ungroup()
        
        plf$ano = as.numeric(ano_i)
        
        export(plf, paste0(wd3, part_lf))
        
        #participação no merc de trabalho por sexo e levelfield
        plfs <- c %>%
                group_by(sexo, lf) %>%
                summarise(part = wtd.mean(econActivity, peso)*100
                ) %>%
                ungroup()
        
        plfs$ano = as.numeric(ano_i)
        
        
        export(plfs, paste0(wd3, part_lfs))
        
        export(c, paste0(wd3, file_to_save_i))
        
        rm(c)  
        gc()
}

# Empilhando resultados ---- 

# Participação geral no mercado de trabalho


part_geral1980 = import(paste0(wd,"/part_geral1980.xlsx"))
part_geral1991 = import(paste0(wd,"/part_geral1991.xlsx"))
part_geral2000 = import(paste0(wd,"/part_geral2000.xlsx"))
part_geral2010 = import(paste0(wd,"/part_geral2010.xlsx"))

part_geral = bind_rows(part_geral1980,
                       part_geral1991,
                       part_geral2000,
                       part_geral2010)


export(part_geral, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\censo com desocupados\\part_geral.xlsx")


# Participação no mercado de trabalho por sexo

part_sexo1980 = import(paste0(wd4,"/part_sexo1980.xlsx"))
part_sexo1991 = import(paste0(wd4,"/part_sexo1991.xlsx"))
part_sexo2000 = import(paste0(wd4,"/part_sexo2000.xlsx"))
part_sexo2010 = import(paste0(wd4,"/part_sexo2010.xlsx"))

part_sexo = bind_rows(part_sexo1980,
                      part_sexo1991,
                      part_sexo2000,
                      part_sexo2010)


export(part_sexo, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\part_sexo.xlsx")


#participação por nível de ensino

part_educ1980 = import(paste0(wd4,"/part_educ1980.xlsx"))
part_educ1991 = import(paste0(wd4,"/part_educ1991.xlsx"))
part_educ2000 = import(paste0(wd4,"/part_educ2000.xlsx"))
part_educ2010 = import(paste0(wd4,"/part_educ2010.xlsx"))

part_educ = bind_rows(part_educ1980,
                      part_educ1991,
                      part_educ2000,
                      part_educ2010)

export(part_educ, 
       "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\part_educ.xlsx")



#participação por área de estudo

part_area1980 = import(paste0(wd4,"/part_area1980.xlsx"))
part_area1991 = import(paste0(wd4,"/part_area1991.xlsx"))
part_area2000 = import(paste0(wd4,"/part_area2000.xlsx"))
part_area2010 = import(paste0(wd4,"/part_area2010.xlsx"))

part_area = bind_rows(part_area1980,
                      part_area1991,
                      part_area2000,
                      part_area2010)

export(part_area, 
       "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\part_area.xlsx")


#participação por level field

part_lf1980 = import(paste0(wd4,"/part_lf1980.xlsx"))
part_lf1991 = import(paste0(wd4,"/part_lf1991.xlsx"))
part_lf2000 = import(paste0(wd4,"/part_lf2000.xlsx"))
part_lf2010 = import(paste0(wd4,"/part_lf2010.xlsx"))

part_lf = bind_rows(  part_lf1980,
                      part_lf1991,
                      part_lf2000,
                      part_lf2010)


export(part_lf, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\part_lf.xlsx")

#participação por level field e sexo

part_lfs1980 = import(paste0(wd4,"/part_lfs1980.xlsx"))
part_lfs1991 = import(paste0(wd4,"/part_lfs1991.xlsx"))
part_lfs2000 = import(paste0(wd4,"/part_lfs2000.xlsx"))
part_lfs2010 = import(paste0(wd4,"/part_lfs2010.xlsx"))

part_lfs = bind_rows( part_lfs1980,
                      part_lfs1991,
                      part_lfs2000,
                      part_lfs2010)


export(part_lf, "C:\\Users\\demed\\Dropbox\\PC\\Documents\\Carol\\Dissertação\\Pesquisa\\Dissertacao_CarolinaMedeiros\\1_Data\\arquivos finais\\part_lfs.xlsx")


