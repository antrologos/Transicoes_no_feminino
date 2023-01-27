## Decomposição linkage

#Pacotes e configuração -----

options(scipen = 999) #notação cientifica

library(rio)
library(tidyverse)
library(fst)
library(dplyr)
library(segregation)
library(weights)
library(Hmisc) #medias ponderadas


# Abrindo censos processados ---- 


c80 = import(here("../1_Data/processed/censo1980.fst"))
c91 = import(here("../1_Data/processed/censo1991.fst"))
c00 = import(here("../1_Data/processed/censo2000.fst"))
c10 = import(here("../1_Data/processed/censo2010.fst"))


gc()



# Segregação total entre as ocupações e níveis área ----

total80 <- mutual_total(c80, "ocup3dig", "lf", weight = "peso")

# stat       est
# 1:    M 0.3926896
# 2:    H 0.1181553

total91 <- mutual_total(c91, "ocup3dig", "lf", weight = "peso")

# stat       est
# 1:    M 0.3857239
# 2:    H 0.1108845


total00 <- mutual_total(c00, "ocup3dig", "lf", weight = "peso")

# stat       est
# 1:    M 0.3830861
# 2:    H 0.1048113

total10 <- mutual_total(c10, "ocup3dig", "lf", weight = "peso")

# stat       est
# 1:    M 0.3722458
# 2:    H 0.1005862

#entre níveis educacionais ----

between80 <- mutual_total(c80, "ocup3dig", "educ", weight = "peso")
# stat       est
# 1:    M 0.3483201
# 2:    H 0.1048051

between91 <- mutual_total(c91, "ocup3dig", "educ", weight = "peso")
# stat        est
# 1:    M 0.32238375
# 2:    H 0.09267601

between00 <- mutual_total(c00, "ocup3dig", "educ", weight = "peso")
# stat        est
# 1:    M 0.31342127
# 2:    H 0.08575115

between10 <- mutual_total(c10, "ocup3dig", "educ", weight = "peso")
# stat        est
# 1:    M 0.26762954
# 2:    H 0.07231739

# intra níveis educacionais -----

##1980 ----
(mutual_total(c80, "ocup3dig", "lf", within = "educ", weight = "peso"))
# stat        est
# 1:    M 0.04436946
# 2:    H 0.01335020

# vê os de cada componente (aqui nível educacional)
within80 <- mutual_within(c80, "ocup3dig", "lf",
                         within = "educ", weight = "peso", wide = TRUE)


# educ         M          p         H ent_ratio
# 1:    1 0.0000000 0.24040830 0.0000000 0.5790462
# 2:    2 0.0000000 0.51295784 0.0000000 0.9905554
# 3:    3 0.0000000 0.09932761 0.0000000 1.0756347
# 4:    4 0.0000000 0.10244605 0.0000000 0.9537814
# 5:    5 0.9890607 0.04486020 0.3081559 0.9657312


# merge into a vector
components <- c(between80$est[1], within80$M * within80$p)
signif(100 * components / total80$est[1], 3)

##1991----
(mutual_total(c91, "ocup3dig", "lf", within = "educ", weight = "peso"))
# stat        est
# 1:    M 0.06334017
# 2:    H 0.01820847

within91 <- mutual_within(c91, "ocup3dig", "lf",
                         within = "educ", weight = "peso", wide = TRUE)


# educ         M          p         H ent_ratio
# 1:    1 0.0000000 0.15376516 0.0000000 0.6062341
# 2:    2 0.0000000 0.48427147 0.0000000 0.9307385
# 3:    3 0.0000000 0.13318609 0.0000000 1.0472440
# 4:    4 0.0000000 0.15995523 0.0000000 0.9824398
# 5:    5 0.9203471 0.06882204 0.2727799 0.9699146

# merge into a vector
components <- c(between91$est[1], within91$M * within91$p)
signif(100 * components / total91$est[1], 3)

## 2000----
(mutual_total(c00, "ocup3dig", "lf", within = "educ", weight = "peso"))
# stat        est
# 1:    M 0.06966487
# 2:    H 0.01906011

within00 <- mutual_within(c00, "ocup3dig", "lf",
                         within = "educ", weight = "peso", wide = TRUE)


# educ         M          p         H ent_ratio
# 1:    1 0.0000000 0.07885829 0.0000000 0.6145112
# 2:    2 0.0000000 0.45208992 0.0000000 0.8774949
# 3:    3 0.0000000 0.16398073 0.0000000 1.0252821
# 4:    4 0.0000000 0.22564040 0.0000000 1.0090010
# 5:    5 0.8770526 0.07943067 0.2600818 0.9226291

# merge into a vector
components <- c(between00$est[1], within00$M * within00$p)
signif(100 * components / total00$est[1], 3)


##2010 ----

(mutual_total(c10, "ocup3dig", "lf", within = "educ", weight = "peso"))
# stat        est
# 1:    M 0.10461621
# 2:    H 0.02826882

within10 <- mutual_within(c10, "ocup3dig", "lf",
                         within = "educ", weight = "peso", wide = TRUE)


# educ         M          p         H ent_ratio
# 1:    1 0.0000000 0.05130047 0.0000000 0.7074293
# 2:    2 0.0000000 0.31617161 0.0000000 0.8490738
# 3:    3 0.0000000 0.17879871 0.0000000 0.9776689
# 4:    4 0.0000000 0.32081935 0.0000000 1.0153899
# 5:    5 0.7871215 0.13290987 0.2310007 0.9207402

# merge into a vector
components <- c(between10$est[1], within10$M * within10$p)
signif(100 * components / total10$est[1], 3)



# Diferenças ao longo do tempo -----


## 1980-2010----
dif1 = mutual_difference(c80, c10, "ocup3dig", "lf", weight = "peso")


# stat         est
# 1:             M1  0.39268960
# 2:             M2  0.37224575
# 3:           diff -0.02044384
# 4:      additions -0.02987652
# 5:       removals  0.00000000
# 6: group_marginal  0.05710188
# 7:  unit_marginal  0.10609852
# 8:     structural -0.15376773

##1991 -2010----

dif2 = mutual_difference(c91, c10, "ocup3dig", "lf", weight = "peso")

# stat         est
# 1:             M1  0.38572392
# 2:             M2  0.37224575
# 3:           diff -0.01347817
# 4:      additions -0.02635755
# 5:       removals  0.00000000
# 6: group_marginal  0.05867648
# 7:  unit_marginal  0.05221657
# 8:     structural -0.09801367

## 2000-2010 ----

dif3 = mutual_difference(c00, c10, "ocup3dig", "lf", weight = "peso")

# stat          est
# 1:             M1  0.383086144
# 2:             M2  0.372245753
# 3:           diff -0.010840391
# 4:      additions  0.000000000
# 5:       removals  0.000000000
# 6: group_marginal  0.009875445
# 7:  unit_marginal  0.051099170
# 8:     structural -0.071815007



