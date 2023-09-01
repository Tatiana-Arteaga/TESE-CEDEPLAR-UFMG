################################################################################
####################TESE DE DOUTORADO - CAPÍTULO 1####################
###############AUTORA: TATIANA CUNHA E SILVA ARTEAGA#############################
################################################################################

rm(list = ls(all = TRUE))


# PACOTES -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(here)
library(ggpubr)


# LEITURA DAS BASES -------------------------------------------------------

TV_MALE <-read_excel("TV-MALE.xlsx")
TV_FEMALE <-read_excel("TV-FEMALE.xlsx")
base_paises <- read_excel ("paises_base.xlsx")
base_populaçao <- read_excel("pop_UN.xlsx")



# RENOMEANDO AS VARIÁVEIS -------------------------------------------------

M_names<- c("M_index", "M_variant", "M_region", "M_notes", "location_code", "M_iso3", "M_iso2",
          "M_smdx", "type", "M_parent_code", "year", "x","n", "M_m(x,n)", "M_q(x,n)", 
          "M_p(x,n)", "M_l(x)", "M_d(x,n)","M_L(x,n)", "M_S(x,n)", "M_T(x)", "M_e(x)", "M_a(x,n)")
F_names<- c("F_index", "F_variant", "F_region", "F_notes", "location_code", "F_iso3", "F_iso2",
          "F_smdx", "type", "F_parent_code", "year", "x","n", "F_m(x,n)", "F_q(x,n)", 
          "F_p(x,n)", "F_l(x)", "F_d(x,n)","F_L(x,n)", "F_S(x,n)", "F_T(x)", "F_e(x)", "F_a(x,n)")

colnames(TV_MALE) <- M_names
colnames(TV_FEMALE) <- F_names



# MERGE DAS DUAS BASES DE DADOS (MALE E FEMALE) ---------------------------

  
TV_M_F <- full_join(TV_MALE, TV_FEMALE)


# Agregando o nível de desenvolvimento dos países -------------------------

TV_M_F_v2 <- left_join(TV_M_F, base_paises)

# Retirando países pequenos, regiões e anos que não interessam #

anos_interesse <- c(1980,1985,1990,1995,2000,2005,2010,2015,2020)
TV_M_F_v3 <- TV_M_F_v2 %>%
  filter(type == "Country/Area",
         year %in%  anos_interesse,
         is.na(Pop_menor90k))

# TRansformando variáveis de base_população em numéricas#

base_populaçao$`0-4`<- as.numeric(base_populaçao$`0-4`)
base_populaçao$`5-9`<- as.numeric(base_populaçao$`5-9`)
base_populaçao$`10-14`<- as.numeric(base_populaçao$`10-14`)
base_populaçao$`15-19`<- as.numeric(base_populaçao$`15-19`)
base_populaçao$`20-24`<- as.numeric(base_populaçao$`20-24`)
base_populaçao$`25-29`<- as.numeric(base_populaçao$`25-29`)
base_populaçao$`30-34`<- as.numeric(base_populaçao$`30-34`)
base_populaçao$`35-39`<- as.numeric(base_populaçao$`35-39`)
base_populaçao$`40-44`<- as.numeric(base_populaçao$`40-44`)
base_populaçao$`45-49`<- as.numeric(base_populaçao$`45-49`)
base_populaçao$`50-54`<- as.numeric(base_populaçao$`50-54`)
base_populaçao$`55-59`<- as.numeric(base_populaçao$`55-59`)
base_populaçao$`60-64`<- as.numeric(base_populaçao$`60-64`)
base_populaçao$`65-69`<- as.numeric(base_populaçao$`65-69`)
base_populaçao$`70-74`<- as.numeric(base_populaçao$`70-74`)
base_populaçao$`75-79`<- as.numeric(base_populaçao$`75-79`)
base_populaçao$`80-84`<- as.numeric(base_populaçao$`80-84`)
base_populaçao$`85-89`<- as.numeric(base_populaçao$`85-89`)
base_populaçao$`90-94`<- as.numeric(base_populaçao$`90-94`)
base_populaçao$`95-99`<- as.numeric(base_populaçao$`95-99`)
base_populaçao$`100+`<- as.numeric(base_populaçao$`100+`)


base_populaçao <- base_populaçao %>% 
  filter(Type == "Country/Area",
         year %in%  anos_interesse,
         total >= 500000)

vetor_pais <- base_populaçao %>% 
  distinct(location_code) %>% 
  pull(location_code)


TV_M_F_v4 <- TV_M_F_v3 %>% 
  filter(location_code %in% vetor_pais)


#####################Filtrando a base ########################


# 0-1-base ----------------------------------------------------------------


TV_M_F_filt_0a1 <- TV_M_F_v4 %>%
  filter(x == 0) %>% 
  mutate(RSPM = `M_q(x,n)`/ `F_q(x,n)`) %>% 
  select(M_region, location_code, x, n, year, `M_l(x)`, `F_l(x)`, `F_e(x)`, `M_e(x)`, RSPM, class)


# 1 a 4- base-----------------------------------------------------------------------

TV_M_F_filt_1a4 <- TV_M_F_v4 %>%
  filter(x == 1) %>% 
  mutate(RSPM = `M_q(x,n)`/ `F_q(x,n)`) %>% 
  select(M_region, location_code, x, n, year, `M_l(x)`, `F_l(x)`, `F_e(x)`, `M_e(x)`, RSPM, class)

# 5-15-base ---------------------------------------------------------------

TV_M_F_v5 <- TV_M_F_v4 %>% 
  group_by(location_code, year) %>%
  filter((x == 5 | x == 10) & n == 5) %>% 
  select(M_region, location_code, x, n, year, `M_l(x)`, `F_l(x)`, `F_e(x)`, `M_e(x)`, class)

TV_M_F_filt_5a15_prob <- TV_M_F_v4 %>%
  group_by(location_code, year) %>%
  filter((x == 5 | x == 10) & n == 5) %>% 
  select(M_region, x, n, year, `M_l(x)`, `F_l(x)`) %>% 
  pivot_wider(names_from = x, values_from = c( `M_l(x)`, `F_l(x)`)) %>% 
  mutate(prob_M_5_15 = (`M_l(x)_5`-`M_l(x)_10`)/`M_l(x)_5`,
         prob_F_5_15 = (`F_l(x)_5`-`F_l(x)_10`)/`F_l(x)_5`,
         RSPM = prob_M_5_15/prob_F_5_15 ) %>% 
  select(M_region, location_code, year, prob_F_5_15, prob_M_5_15, RSPM)

TV_M_F_filt_5a15_prob <- TV_M_F_filt_5a15_prob %>% 
  left_join(TV_M_F_v5, by = c("location_code", "year", "M_region"))



# 15-50-base --------------------------------------------------------------

TV_M_F_v6 <- TV_M_F_v4 %>% 
  filter(x == 15) %>% 
  select(M_region, location_code, x, year, `M_l(x)`, `F_l(x)`, `F_e(x)`, `M_e(x)`, class)

  
TV_M_F_filt_15a50_prob <- TV_M_F_v4 %>%
  group_by(location_code, year) %>%
  filter((x == 15 | x == 20 | x==25 | x==30 | x==35 |x==40 |x==45) & n == 5) %>% 
  select(M_region, x, n, `M_l(x)`, `F_l(x)`) %>% 
  pivot_wider(names_from = x, values_from = c( `M_l(x)`, `F_l(x)`)) %>% 
  mutate(prob_M_30_15 = (`M_l(x)_15`-`M_l(x)_45`)/`M_l(x)_15`,
         prob_F_30_15 = (`F_l(x)_15`-`F_l(x)_45`)/`F_l(x)_15`,
         RSPM = prob_M_30_15/prob_F_30_15) %>% 
  select(M_region, location_code, year, prob_F_30_15, prob_M_30_15, RSPM)

TV_M_F_filt_15a50_prob <- TV_M_F_filt_15a50_prob %>% 
  left_join(TV_M_F_v6, by = c("location_code", "year", "M_region"))

# 60-80-base -------------------------------------------------------------

TV_M_F_v7 <- TV_M_F_v4 %>% 
  filter(x == 60) %>% 
  select(M_region, location_code, x, year, `M_l(x)`, `F_l(x)`, `F_e(x)`, `M_e(x)`, class)
  
  
TV_M_F_filt_60a80_prob <- TV_M_F_v4 %>%
  group_by(location_code, year) %>%
  filter((x == 60 | x == 65 | x==70 | x==75 | x==80 |x==85 |x==90 |x==95 |x==100)) %>% 
  select(M_region, x, n, `M_l(x)`, `F_l(x)`) %>% 
  pivot_wider(names_from = x, values_from = c( `M_l(x)`, `F_l(x)`)) %>% 
  mutate(across(`M_l(x)_60`:`F_l(x)_100`, ~replace_na(., 0))) %>% 
  mutate(prob_M_20_60 = (`M_l(x)_60`-`M_l(x)_80`)/`M_l(x)_60`,
         prob_F_20_60 = (`F_l(x)_60`-`F_l(x)_80`)/`F_l(x)_60`,
         RSPM = prob_M_20_60/prob_F_20_60) %>% 
  select(M_region, location_code, year, prob_F_20_60, prob_M_20_60, RSPM)

TV_M_F_filt_60a80_prob <- TV_M_F_filt_60a80_prob %>% 
  left_join(TV_M_F_v7, by = c("location_code", "year", "M_region"))

# GRÁFICOS ----------------------------------------------------------------

############## 0 a 1 ano ###############

#########Identificando países com RSPM menor do que 1,1 ######
 

vetor_mort_fem_maior_0a1 <- TV_M_F_filt_0a1 %>% 
  filter(year==2020, RSPM<=1.1) %>% 
  distinct(M_region) %>% 
  pull(M_region)

vetor_mort_fem_maior_0a1


graf1<- ggplot() +
  geom_point(data = TV_M_F_filt_0a1, aes(x = `M_e(x)`, y = `RSPM`, colour = class)) +
  geom_text_repel(data = filter(TV_M_F_filt_0a1, `RSPM` <= 1.1),
            aes(x = `M_e(x)`, y = `RSPM`, label = M_region))+
  geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
  geom_smooth(data = TV_M_F_filt_0a1, aes(x = `M_e(x)`, y = `RSPM`), method = "lm", color = "darkgreen") +
  stat_regline_equation() +
  facet_wrap(~year)+
  labs(x = "M_ex",
       y = "RSPM",
       color = "class", 
       title = "Razão de Sexo da Probabilidade de Morte de 0 a 1 ano pela esperança de vida masculina")


############## 1 a 4 anos ###############

#########Identificando países com RSPM menor do que 1,0 ######


vetor_mort_fem_maior_1a4 <- TV_M_F_filt_1a4 %>% 
  filter(year==2020,RSPM<=1) %>% 
  distinct(M_region) %>% 
  pull(M_region)

vetor_mort_fem_maior_1a4

TV_M_F_filt_1a4 <- TV_M_F_filt_1a4 %>% 
  filter(!(M_region == "Denmark" & year == 2020))

graf2<- ggplot() +
  geom_point(data = TV_M_F_filt_1a4, aes(x = `M_e(x)`, y = `RSPM`, colour = class)) +
  geom_text_repel(data = filter(TV_M_F_filt_1a4, 
                                `RSPM` <= 1),
                  aes(x = `M_e(x)`, y = `RSPM`, label = M_region))+
  geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
  geom_smooth(data = TV_M_F_filt_1a4, aes(x = `M_e(x)`, y = `RSPM`), method = "lm", color = "darkgreen") +
  stat_regline_equation() +
  facet_wrap(~year)+
  labs(x = "M_ex",
       y = "RSPM",
       color = "class", 
       title = "Razão de Sexo da Probabilidade de Morte de 1 a 4 anos pela esperança de vida masculina")

############## 15 a 50 anos ###############

#########Identificando países com RSPM menor do que 1,1 ######

vetor_mort_fem_maior_15a50 <- TV_M_F_filt_15a50_prob %>% 
  filter(year==2020,RSPM<=1.1) %>% 
  distinct(M_region) %>% 
  pull(M_region)

vetor_mort_fem_maior_15a50


graf3<- ggplot() +
  geom_point(data = TV_M_F_filt_15a50_prob,
             aes(x =`M_e(x)`, y =`RSPM`, colour =`class`)) +
  geom_text_repel(data = filter(TV_M_F_filt_15a50_prob, `RSPM` <= 1),
                  aes(x = `M_e(x)`, y = `RSPM`, label = `M_region`))+
  geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
  geom_smooth(data = TV_M_F_filt_15a50_prob, aes(x = `M_e(x)`, y = `RSPM`), method = "lm", color = "darkgreen") +
  stat_regline_equation() +
  facet_wrap(~year)+
  labs(x = "M_ex",
       y = "RSPM",
       color = "class", 
       title = "Razão de Sexo da Probabilidade de Morte de 15 a 50 anos pela esperança de vida masculina")


############## 60 anos ou mais ###############

#########Identificando países com RSPM menor do que 1,1 ######


vetor_mort_fem_maior_60a80 <- TV_M_F_filt_60a80_prob %>% 
  filter(year==2020,RSPM<=1.1) %>% 
  distinct(M_region) %>% 
  pull(M_region)

vetor_mort_fem_maior_60a80


graf4<- ggplot() +
  geom_point(data = TV_M_F_filt_60a80_prob, aes(x = `M_e(x)`, y = `RSPM`, colour = class)) +
  geom_text_repel(data = filter(TV_M_F_filt_60a80_prob, 
                                `RSPM` <= 1),
                  aes(x = `M_e(x)`, y = `RSPM`, label = M_region))+
  geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
  geom_smooth(data = TV_M_F_filt_60a80_prob, aes(x = `M_e(x)`, y = `RSPM`), method = "lm", color = "darkgreen") +
  stat_regline_equation() +
  facet_wrap(~year)+
  labs(x = "M_ex",
       y = "RSPM",
       color = "class", 
       title = "Razão de Sexo da Probabilidade de Morte de 60 a 80 anos pela esperança de vida masculina")

####################################Salvando os gráficos########################

ggsave(filename = "GRÁFICOS-CAP1/graf1.pdf", # Nome do arquivo com extensão (.png, .jpg, .jpeg, .pdf etc)
       plot = graf1, # Qual objeto é para salvar
       height = 15, # Altura
       width = 25, # Largura
       units = "cm", # Unidade de medida
       dpi = 500) # Resolução


ggsave(filename = "GRÁFICOS-CAP1/graf2.pdf", # Nome do arquivo com extensão (.png, .jpg, .jpeg, .pdf etc)
       plot = graf2, # Qual objeto é para salvar
       height = 15, # Altura
       width = 25, # Largura
       units = "cm", # Unidade de medida
       dpi = 500) # Resolução


ggsave(filename = "GRÁFICOS-CAP1/graf3.pdf", # Nome do arquivo com extensão (.png, .jpg, .jpeg, .pdf etc)
       plot = graf3, # Qual objeto é para salvar
       height = 15, # Altura
       width = 25, # Largura
       units = "cm", # Unidade de medida
       dpi = 500) # Resolução


ggsave(filename = "GRÁFICOS-CAP1/graf4.pdf", # Nome do arquivo com extensão (.png, .jpg, .jpeg, .pdf etc)
       plot = graf4, # Qual objeto é para salvar
       height = 15, # Altura
       width = 25, # Largura
       units = "cm", # Unidade de medida
       dpi = 500) # Resolução


