###########################################################
##### Topic: Analysis of the work's database: 
##### Uncovering research patterns of a middle-income country: 
##### A Comprehensive Study of Peru's Specialized Health Institutes' Scientific Output (1991-2021)

##### Developed by: Frank Zela-Coila
##### Date: December 8, 2022
###########################################################

# 1) Open packages----
library(tidyverse)
library(openxlsx)
library(dplyr)
library(ggpubr)
library(haven)
library(Hmisc)
library(compareGroups)
library(scales) 
library(compareGroups)




# 2) TABLES----

## Table 1. General characteristics of scientific output from specialized institutes in Peru.----
IN_FINAL <- read.xlsx("data/bd_bibliometric_institutes_v1.xlsx")

data_tabla1_IN_1 <- compareGroups(IN ~ idioma + financiado + tipo_art + 
                                    tipo_stu + quartil + pais_rev +
                                    transversal_2, 
                                  data=IN_FINAL, 
                                  byrow = F, 
                                  max.ylev=10)

tabla1_IN_1.1 <- createTable(data_tabla1_IN_1, 
                             show.all=TRUE, 
                             digits = 2) 

tabla1_IN_1.1

export2xls(tabla1_IN_1.1, "tables/table1.xlsx")

## Table 2. Journal characteristics and specialization by specialized health institutes in Peru.----
IN_revistas_FINAL <- read.xlsx("data/bd_bibliometric_institutes_v2.xlsx")

### Global ----
names(IN_revistas_FINAL)

tab_revistas <-as.data.frame.matrix(table(IN_revistas_FINAL$revista))

table(IN_revistas_FINAL$revista)

tab_revistas <- IN_revistas_FINAL %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/3020)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)  


tab_revistas
write.xlsx(tab_revistas, "tables/table2.0.xlsx")

### Top by Institutes----
#### 1) INR----
IN_revistas_INR <- IN_revistas_FINAL %>% 
  filter(IN == "INR") 
IN_revistas_INR <- IN_revistas_INR %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/34)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)  

IN_revistas_INR

write.xlsx(IN_revistas_INR, "tables/table2.1.xlsx")
#### 2) INO----
IN_revistas_INO <- IN_revistas_FINAL %>% 
  filter(IN == "INO") 
IN_revistas_INO <- IN_revistas_INO %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/40)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)  

IN_revistas_INO

write.xlsx(IN_revistas_INO, "tables/table2.2.xlsx")

#### 3) INSN-SB----
IN_revistas_SANBORJA <- IN_revistas_FINAL %>% 
  filter(IN == "INSN-SAN BORJA") 

IN_revistas_SANBORJA <- IN_revistas_SANBORJA %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/106)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)  

IN_revistas_SANBORJA

write.xlsx(IN_revistas_SANBORJA, "tables/table2.3.xlsx")

#### 4) INMP----
IN_revistas_INMP <- IN_revistas_FINAL %>% 
  filter(IN == "INMP") 

IN_revistas_INMP <- IN_revistas_INMP %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/113)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev) 

IN_revistas_INMP

write.xlsx(IN_revistas_INMP, "tables/table2.4.xlsx")


#### 5) INSM----
IN_revistas_INSM <- IN_revistas_FINAL %>% 
  filter(IN == "INSM") 

IN_revistas_INSM <- IN_revistas_INSM %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/153)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)

IN_revistas_INSM

write.xlsx(IN_revistas_INSM, "tables/table2.5.xlsx")


#### 6) INSN-BRENA----
IN_revistas_BRENA <- IN_revistas_FINAL %>% 
  filter(IN == "INSN-BREÑA") 

IN_revistas_BRENA <- IN_revistas_BRENA %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/390)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)

IN_revistas_BRENA

write.xlsx(IN_revistas_BRENA, "tables/table2.6.xlsx")



#### 7) INCN----
IN_revistas_INCN <- IN_revistas_FINAL %>% 
  filter(IN == "INCN") 

IN_revistas_INCN <- IN_revistas_INCN %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/413)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)

IN_revistas_INCN

write.xlsx(IN_revistas_INCN, "tables/table2.7.xlsx")


#### 8) INEN----
IN_revistas_INEN <- IN_revistas_FINAL %>% 
  filter(IN == "INEN") 

IN_revistas_INEN <- IN_revistas_INEN %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/722)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)

IN_revistas_INEN

write.xlsx(IN_revistas_INEN, "tables/table2.8.xlsx")

#### 9) INS----
IN_revistas_INS <- IN_revistas_FINAL %>% 
  filter(IN == "INS") 

IN_revistas_INS <- IN_revistas_INS %>% 
  group_by(revista, pais_rev, quartil) %>% 
  count(revista, sort = T) %>% 
  mutate(porcentaje = (n/1049)*100, 
         porcentaje = round(porcentaje, 2)) %>% 
  select(revista, n, porcentaje, quartil, pais_rev)

IN_revistas_INS

write.xlsx(IN_revistas_INS, "tables/table2.9.xlsx")


## Table 3. Production indicators for each institute.----

### Productivity variation rate  ----
## VP=(Dt+1−Dt/Dt)x100
## VP: Variation of production 
## Dt: Number of articles published in X year
## Dt+1: Number of articles published in the next year of the X year.

# Total production in 2020
IN_var_prod <- IN_FINAL %>% 
  select(code, IN, year) %>% 
  filter(year == 2020) 
table(IN_var_prod$year) # 344 

# Total production in 2021
IN_var_prod <- IN_FINAL %>% 
  select(code, IN, year) %>% 
  filter(year == 2021) 
table(IN_var_prod$year) # 376

# filter 2020 and 2021
años <- c(2020,2021)

IN_var_prod <- IN_FINAL %>% 
  select(code, IN, year) %>% 
  filter(year %in% años) %>% 
  mutate(year=case_when(year%in%2020~"prod2020",
                        year%in%2021~"prod2021")) %>% 
  glimpse()
IN_var_prod

table_produc <- table(IN_var_prod$IN, IN_var_prod$year)
table_produc <- as.data.frame.matrix(table_produc)
table_produc

table_produc <- table_produc %>% 
  mutate(tasa_var_prod = ((prod2021-prod2020)/prod2020)*100, 
         tasa_var_prod = round(tasa_var_prod, 2))
table_produc

# export
write.xlsx(table_produc, "tables/table3.1.xlsx")

### Productivity index by Institute----
ind_produc_IN <- IN_FINAL %>% 
  group_by(IN) %>% 
  count(IN, sort = T) %>% 
  select(IN, n)

ind_produc_IN 
ind_produc_IN$log <- log(ind_produc_IN$n)
ind_produc_IN 

write.xlsx(ind_produc_IN, "tables/table3.2.xlsx")

### Productivity index by gender.----
# IPG= TAS(F) ̸ TAS(M).  
# IPG: Productivity index by gender. 
# TAS(F)= Total female first/last author 
# TAS(M)= Total male first/last author.

table(IN_FINAL$primer_au_inst)
# First author

# a. Filter first authorship
IP_gen_primerau <- IN_FINAL %>% 
  select(IN, gen_primer_au, primer_au_inst) %>% 
  filter(primer_au_inst == "Yes") %>% 
  select(IN, gen_primer_au)

IPC_gen_primerau <- table(IP_gen_primerau$IN,IP_gen_primerau$gen_primer_au)
IPC_gen_primerau

IPC_gen_primerau<-as.data.frame.matrix(IPC_gen_primerau)

# b. Calculate index
IPC_gen_primerau <- IPC_gen_primerau %>% 
  mutate(IPG = Female/Male, IPG = round(IPG, 2)) 

IPC_gen_primerau

IPC_gen_primerau$variable <- rownames(IPC_gen_primerau)
rownames(IPC_gen_primerau) <- NULL

IPC_gen_primerau <- IPC_gen_primerau %>% 
  select(variable, Female, Male, IPG) 

IPC_gen_primerau

write.xlsx(IPC_gen_primerau, "tables/table3.3.xlsx")

# Last author

# a. Filter last authorship
IP_gen_senior <- IN_FINAL %>% 
  select(IN, genero_senior, senior_instituto_sino) %>% 
  filter(senior_instituto_sino == "Si") %>% 
  select(IN, genero_senior)

IP_gen_senior

IP_gen_senior <- table(IP_gen_senior$IN,IP_gen_senior$genero_senior)

IP_gen_senior

IP_gen_senior <- as.data.frame.matrix(IP_gen_senior)

# b. Calculate index
IP_gen_senior <- IP_gen_senior %>% 
  mutate(IPG = Female/Male, IPG = round(IPG, 2)) 

IP_gen_senior

IP_gen_senior$variable <- rownames(IP_gen_senior)
rownames(IP_gen_senior) <- NULL

IP_gen_senior <- IP_gen_senior %>% 
  select(variable, Female, Male, IPG) 

IP_gen_senior

write.xlsx(IP_gen_senior, "tables/table3.4.xlsx")




## Table 4. Means and medians of authors, both national and international, by institute.----

# Total number of authors
IN_FINAL$n_au_totales <- as.numeric(IN_FINAL$n_au_totales)

# N° of authors per document from Institutes
IN_FINAL$n_au_institutos <- as.numeric(IN_FINAL$n_au_institutos)

# N° of authors per document from Peru
IN_FINAL$n_au_peruanos <- as.numeric(IN_FINAL$n_au_peruanos)

# N° of authors per document Foreign
IN_FINAL$n_au_extranjeros <- as.numeric(IN_FINAL$n_au_extranjeros)


Ind_impacto <- IN_FINAL %>% 
  select(IN, year, gen_primer_au,primer_au_inst,
         genero_senior, senior_instituto_sino,
         n_au_totales, n_au_extranjeros, n_au_peruanos, 
         n_au_institutos)

# duplicates of variables cause calculate mean and median
Ind_impacto$n_au_totales2 <- as.numeric(Ind_impacto$n_au_totales)
Ind_impacto$n_au_extranjeros2 <- as.numeric(Ind_impacto$n_au_extranjeros)
Ind_impacto$n_au_peruanos2 <- as.numeric(Ind_impacto$n_au_peruanos)
Ind_impacto$n_au_institutos2 <- as.numeric(Ind_impacto$n_au_institutos)


# Create table
tabla3_IN_1 <- compareGroups(IN ~ n_au_totales + 
                               n_au_extranjeros + n_au_peruanos + 
                               n_au_institutos + n_au_totales2 + 
                               n_au_extranjeros2 + n_au_peruanos2 + 
                               n_au_institutos2,
                             data=Ind_impacto, 
                             method = c(n_au_totales2 = 2, 
                                        n_au_extranjeros2 = 2,
                                        n_au_peruanos2 = 2,
                                        n_au_institutos2 = 2),
                             max.ylev=11)
tabla3_IN_1<-createTable(tabla3_IN_1, 
                         show.all=TRUE)

tabla3_IN_1
export2xls(tabla3_IN_1, "tables/table4.xlsx")




## Table 5. Stratified gender analysis of first and last authorship.----

### First authorship----
tabla3_IN_1_gen <- compareGroups(gen_primer_au ~ IN,
                                 data = Ind_impacto,
                                 byrow = T)

tabla3_IN_1_gen <- createTable(tabla3_IN_1_gen,  
                               show.p.overall = FALSE)

tabla3_IN_1_gen
export2xls(tabla3_IN_1_gen, "tables/table5.1")

tabla3_IN_1_au <- compareGroups(primer_au_inst ~ IN,
                                data = Ind_impacto,
                                byrow = T)

tabla3_IN_1_au <- createTable(tabla3_IN_1_au,  
                              show.p.overall = FALSE)

tabla3_IN_1_au
export2xls(tabla3_IN_1_au, "tables/table5.2")
# Filter data with "No" in primer_au_inst
Ind_impacto_no <- Ind_impacto[Ind_impacto$primer_au_inst == "No", ]
Ind_impacto_no
# Compare results = table
tabla3_IN_1_gen_no <- compareGroups(gen_primer_au ~ IN, 
                                    data = Ind_impacto_no)
tabla3_IN_1_1auno <- createTable(tabla3_IN_1_gen_no, 
                                 show.p.overall = FALSE)
tabla3_IN_1_1auno
export2xls(tabla3_IN_1_1auno, "tables/table5.3")

# Filter data with "Yes" in primer_au_inst
Ind_impacto_si <- Ind_impacto[Ind_impacto$primer_au_inst == "Yes", ]

# Compare results = table
tabla3_IN_1_gen_si <- compareGroups(gen_primer_au ~ IN, 
                                    data = Ind_impacto_si)

tabla3_IN_1_1ausi <- createTable(tabla3_IN_1_gen_si, 
                                 show.p.overall = FALSE)
tabla3_IN_1_1ausi
export2xls(tabla3_IN_1_1ausi, "tables/table5.4")

## Last authorship ----
gen_primer_au + 
  primer_au_inst + genero_senior +
  senior_instituto_sino
class(Ind_impacto$IN)
class(Ind_impacto$primer_au_inst)
names(Ind_impacto)
tabla3_IN_1_gen_sen <- compareGroups(genero_senior ~ IN,
                                     data = Ind_impacto,
                                     byrow = T)

tabla3_IN_1_gen_sen <- createTable(tabla3_IN_1_gen_sen,  
                                   show.p.overall = FALSE)

tabla3_IN_1_gen_sen
export2xls(tabla3_IN_1_gen_sen, "tables/table5.3.xlsx")

tabla3_IN_1_senior <- compareGroups(senior_instituto_sino ~ IN,
                                    data = Ind_impacto,
                                    byrow = T)

tabla3_IN_1_senior <- createTable(tabla3_IN_1_senior,  
                                  show.p.overall = FALSE)

tabla3_IN_1_senior
export2xls(tabla3_IN_1_senior, "tables/table5.4.xlsx")

# Filter data with "No" de senior_instituto_sino
Ind_impacto$senior_instituto_sino 
Ind_impacto_senior_no <- Ind_impacto %>% 
  select(IN, genero_senior, 
         senior_instituto_sino) %>%
  na.omit() %>% 
  filter(senior_instituto_sino == "No")

Ind_impacto_senior_no

# Create table
tabla3_IN_1_gen_sen_no <- compareGroups(genero_senior ~ IN, 
                                        data = Ind_impacto_senior_no)

tabla3_IN_1_sen_no <- createTable(tabla3_IN_1_gen_sen_no, 
                                  show.p.overall = FALSE)
tabla3_IN_1_sen_no

export2xls(tabla3_IN_1_sen_no, "tables/table5.5.xlsx")

# Filter data with "Yes" de senior_instituto_sino
Ind_impacto_senior_si <- Ind_impacto %>% 
  select(IN, genero_senior, 
         senior_instituto_sino) %>%
  na.omit() %>% 
  filter(senior_instituto_sino == "Yes")

Ind_impacto_senior_si
# Create table
tabla3_IN_1_gen_sen_si <- compareGroups(genero_senior ~ IN, 
                                        data = Ind_impacto_senior_si)
tabla3_IN_1_gen_sen_si <- createTable(tabla3_IN_1_gen_sen_si, 
                                      show.p.overall = FALSE)
tabla3_IN_1_gen_sen_si
export2xls(tabla3_IN_1_gen_sen_si, "tables/table5.6.xlsx")


## Table 6. Impact indicators for each specialized institute in Peru.----


IN_impacto <- IN_FINAL %>% 
  select(IN, year, citas) %>% 
  glimpse()
table(IN_impacto$IN)
head(IN_impacto) # VERIFICAMOS NAs en citas

#### 1) Total number of citations per institute----
IN_sumacitas <- IN_FINAL %>% 
  select(IN, year, citas) %>% 
  na.omit() %>% 
  group_by(IN) %>% 
  summarise(citas_totales = sum(citas)) %>% 
  glimpse()
IN_sumacitas
#### 2) Number of citations by document per institute----
##### Minimun case----
# NAs as 0
IN_impacto$citas[is.na(IN_impacto$citas)] = 0

table(IN_impacto$citas)

# Calculate mean and median
IN_sumacitas <- IN_impacto %>% 
  group_by(IN) %>% 
  summarise(citas_totales = sum(citas), 
            Citas.x.instituto1 = mean(citas), 
            SD.instituto1 = sd(citas), 
            Citas.x.instituto2 = median(citas), 
            IQR.instituto2 = IQR(citas)) %>%
  glimpse()

IN_sumacitas

# export
write.xlsx(IN_sumacitas, "tables/table6.1.xlsx")

##### Normal case (avoiding NAs)----
IN_impacto2.0 <- IN_FINAL %>% 
  select(IN, year, citas) %>% 
  glimpse()

# Calculate mean and median
IN_sumacitas2.0 <- IN_impacto2.0 %>% 
  group_by(IN) %>% 
  summarise(Citas.x.instituto1 = mean(citas, na.rm = T),
            SD.instituto1 = sd(citas, na.rm = T),
            Citas.x.instituto2 = median(citas, na.rm = T),
            IQR.instituto2 = IQR(citas, na.rm = T)) %>% 
  glimpse()

IN_sumacitas2.0

# export
write.xlsx(IN_sumacitas2.0, "tables/table6.2.xlsx")


##### Maximun case----

# Alternative analysis and reorder of categories of the variables
citas_INS_max$IN <- 0
citas_INEN_max$IN <- 1
citas_INCN_max$IN <- 2
citas_BRENA_max$IN <- 3 
citas_INSM_max$IN <- 4
citas_INMP_max$IN <- 5
citas_BORJA_max$IN <- 6
citas_INO_max$IN <- 7 
citas_INR_max$IN <- 8  
names(citas_INS_max)
names(citas_INEN_max)
names(citas_INCN_max)
names(citas_BRENA_max)
names(citas_INSM_max)
names(citas_INMP_max)
names(citas_BORJA_max)
names(citas_INO_max)
names(citas_INR_max)

# Bind of databases  
citas_maximas_IN <- bind_rows(
  citas_INR_max,
  citas_INO_max,
  citas_BORJA_max,
  citas_INMP_max,
  citas_INSM_max,
  citas_BRENA_max,
  citas_INCN_max,
  citas_INEN_max,
  citas_INS_max)
citas_maximas_IN$IN <- as.factor(citas_maximas_IN$IN)

citas_maximas_IN$IN <- factor(citas_maximas_IN$IN,
                              levels = c("0","1", "2", "3","4","5","6","7","8"),
                              labels = c("INS","INEN", "INCN", "INSN-BREÑA",
                                         "INSM", "INMP", "INSN-SAN BORJA", "INO", "INR"))


citas_maximas_IN$citas2 <- citas_maximas_IN$citas
tabla_impacto3 <- compareGroups(IN ~ citas + 
                                  citas2,
                                data=citas_maximas_IN, 
                                method = c(citas2 = 2),
                                max.ylev=11)
tabla_impacto3<-createTable(tabla_impacto3, 
                            show.all=TRUE)
export2xls(tabla_impacto3, "tables/table6.3.xlsx")

# Calcule by institute
####### a) INR 13 cites----
INR_FINAL
citas_INR_max <- INR_FINAL %>% 
  select(code, citas)

citas_INR_max$citas[is.na(citas_INR_max$citas)] = 13

sum(is.na(citas_INR_max$citas))
citas_INR_max
citas_INR_max <- citas_INR_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INR_max, "tablas/citas_INR_max.xlsx")

####### b) INO 96 cites----
INO_FINAL

citas_INO_max <- INO_FINAL %>% 
  select(code, citas)
citas_INO_max$citas[is.na(citas_INO_max$citas)] = 96
sum(is.na(citas_INO_max$citas))
citas_INO_max
citas_INO_max <- citas_INO_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INO_max, "tablas/citas_INO_max.xlsx")

####### c) INSN-BORJA 45 cites----
INSN_BORJA_FINAL
citas_BORJA_max <- INSN_BORJA_FINAL %>% 
  select(code, citas)
citas_BORJA_max$citas[is.na(citas_BORJA_max$citas)] = 45
sum(is.na(citas_BORJA_max$citas))
citas_BORJA_max
citas_BORJA_max <- citas_BORJA_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_BORJA_max, "tablas/citas_BORJA_max.xlsx")

####### d) INMP 101 cites----
INMP_FINAL
citas_INMP_max <- INMP_FINAL %>% 
  select(code, citas)

citas_INMP_max$citas[is.na(citas_INMP_max$citas)] = 101
sum(is.na(citas_INMP_max$citas))
citas_INMP_max
citas_INMP_max <- citas_INMP_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INMP_max, "tablas/citas_INMP_max.xlsx")

####### e) INSM 304 cites----
INSM_FINAL
citas_INSM_max <- INSM_FINAL %>% 
  select(code, citas)

citas_INSM_max$citas[is.na(citas_INSM_max$citas)] = 304 
sum(is.na(citas_INSM_max$citas))
citas_INSM_max
citas_INSM_max <- citas_INSM_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INSM_max, "tablas/citas_INSM_max.xlsx")

####### f) INSN-BREÑA 837 cites----
INSN_BRENA_FINAL
citas_BRENA_max <- INSN_BRENA_FINAL %>% 
  select(code, citas)

citas_BRENA_max$citas[is.na(citas_BRENA_max$citas)] = 837
sum(is.na(citas_BRENA_max$citas))
citas_BRENA_max
citas_BRENA_max <- citas_BRENA_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_BRENA_max, "tablas/citas_BRENA_max.xlsx")

####### g) INCN 646 cites----
INCN_FINAL
citas_INCN_max <- INCN_FINAL %>% 
  select(code, citas)

citas_INCN_max$citas[is.na(citas_INCN_max$citas)] = 646
sum(is.na(citas_INCN_max$citas))
citas_INCN_max
citas_INCN_max <- citas_INCN_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INCN_max, "tablas/citas_INCN_max.xlsx")


####### h) INEN 1823 cites----
INEN_FINAL
citas_INEN_max <- INEN_FINAL %>% 
  select(code, citas)

citas_INEN_max$citas[is.na(citas_INEN_max$citas)] = 1823
sum(is.na(citas_INEN_max$citas))
citas_INEN_max
citas_INEN_max <- citas_INEN_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INEN_max, "tablas/citas_INEN_max.xlsx")


####### i) INS  5908 cites----
INS_FINAL
citas_INS_max <- INS_FINAL %>% 
  select(code, citas)

citas_INS_max$citas[is.na(citas_INS_max$citas)] = 5908
sum(is.na(citas_INS_max$citas))
citas_INS_max
citas_INS_max <- citas_INS_max %>% 
  summarise(Citas.x.instituto1 = mean(citas),
            SD.instituto1 = sd(citas),
            Citas.x.instituto2 = median(citas),
            IQR.instituto2 = IQR(citas)) %>% 
  glimpse()
write.xlsx(citas_INS_max, "tablas/citas_INS_max.xlsx")


# union maximos
citas_max <- rbind (citas_INS_max, citas_INEN_max, citas_INCN_max,
                    citas_BRENA_max, citas_INSM_max, citas_INMP_max,
                    citas_BORJA_max, citas_INO_max, citas_INR_max)
write.xlsx(citas_max, "tablas/citas_max.xlsx")

#### 3) Number of highly cited publications ----
IN_mas_citadas <- IN_impacto %>% 
  filter(citas >= 10) %>% 
  glimpse()
IN_mas_citadas <- IN_impacto %>% 
  filter(citas >= 10) %>% 
  glimpse() %>% 
  group_by(IN) %>% 
  count(IN, sort = T) %>% 
  select(IN, n)
IN_mas_citadas
write.xlsx(IN_mas_citadas, "tablas/tables/table6.4.xlsx") # muy citadas

#### 4) Percentage of highly cited publications (highly cited)----
IN_citas_total <- IN_impacto %>% 
  group_by(IN) %>% 
  count(IN, sort = T) %>% 
  select(IN, n)
IN_citas_total
write.xlsx(IN_citas_total, "tables/table6.5.xlsx") # total


#### 5) Number of publications ever cited----
IN_alguna_citadas <- IN_impacto %>% 
  filter(citas >= 1) %>% 
  glimpse() %>% 
  group_by(IN) %>% 
  count(IN, sort = T) %>% 
  select(IN, n)
IN_alguna_citadas
write.xlsx(IN_alguna_citadas, "tablas/tabla4_IN_4.xlsx") 

table(IN$quartil)
#### 6) Cite score and H-index----
### H-INDEX
## hindex
h_index = function(cites) {
  if(max(cites) == 0) return(0) # assuming this is reasonable
  cites = cites[order(cites, decreasing = TRUE)]
  tail(which(cites >= seq_along(cites)), 1)
}
h_index(a2)
a1 = c(10,8, 5, 4, 3)
a2 = c(10, 9, 7, 1, 1)

## Just considering Scopus cites
IN_FINAL_citas <- IN_FINAL %>% 
  select(IN, year, citas) %>% 
  na.omit()

sum(is.na(IN_FINAL_citas$citas))

# Calculate cite score
IN_FINAL_citescore <- IN_FINAL_citas %>% 
  group_by(IN) %>% 
  summarise(citas_totales = sum(citas),
            doc_citables = n()) %>% 
  mutate(citescore = citas_totales/doc_citables,
         citescore = round(citescore,2)) 
IN_FINAL_citescore

# Calculate hindex global
IN_FINAL_hindex <- IN_FINAL_citas %>% 
  group_by(IN) %>% 
  summarise(hindex = h_index(citas)) 
#  hindex  2017 - 2021 (5 years)

IN_FINAL_hindex_5 <- IN_FINAL_citas %>% 
  filter(year >= 2017) %>% 
  group_by(IN) %>% 
  summarise(hindex5 = h_index(citas)) 

IN_FINAL_hindex_5

#  hindex  2012 - 2021 (10 years)

IN_FINAL_hindex_10 <- IN_FINAL_citas %>% 
  filter(year >= 2012) %>% 
  group_by(IN) %>% 
  summarise(hindex10 = h_index(citas)) 

IN_FINAL_hindex_10



# 3) FIGURES ----
# Figure 1 and 2 were created in word and excel respectively

# Figure 3. National and international collaboration by specialized institutes of health in Peru.----


bar_colab <- IN_colab1 %>%
  group_by(IN, X6) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct),
         porcentaje = cbind()) 

bar_colab

# edit manually n and lbl in just one cell = n(%)
write.xlsx(bar_colab, "tables/tabla_colab_por_editar.xlsx")

# usar formula =C2&" ("&E2&")" para unir celdas
bar_colab1 <- read.xlsx("tables/tabla_colab_editado.xlsx")
ordencolab_ingles <- c("Peruvian and foreign", "Foreign",
                       "Peruvian", "Individual") 
nombresIN <- c("INS","INEN", "INCN", "INSN-BREÑA","INSM", 
               "INMP", "INSN-SB", "INO", "INR")
figure3 <- ggplot(bar_colab1, 
       aes(x = factor(IN,
                      levels = ordenIN),
           y = pct,
           fill = factor(X6_ingles,
                         levels = ordencolab_ingles))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_x_discrete(labels = nombresIN)+
  geom_text(aes(label = porcentaje), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = colores_colab) +
  labs(y = "Porcentage (%)", 
       fill = "Collaboration",
       x = "Institutes") +
  theme_minimal()
figure3

