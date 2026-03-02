rm(list=ls())

# ---------------------------PROCESAMIENTO --------------------------------

# Cargar librerías --------------------------------------------------------

library(pacman)

pacman::p_load(tidyverse, sjmisc, dplyr, haven, summarytools, car, magrittr)

# Abrir bases de datos ----------------------------------------------------

sindtrab <- readRDS("input/syt.rds")
autoaplicado <- readRDS("input/aut.rds")
empleadores <- readRDS("input/emp.rds")

# Procesar datos ----------------------------------------------------------

## Ordenar ID -------------------------------------------------------------

autoaplicado$id <- as.factor(autoaplicado$id)
empleadores$id <- as.factor(empleadores$id)
sindtrab$id <- as.factor(sindtrab$id)

autoaplicado <- autoaplicado %>% arrange(id)
empleadores <- empleadores %>% arrange(id)
sindtrab <- sindtrab %>% arrange(id)

identical(autoaplicado$id, empleadores$id) 
identical(autoaplicado$id, sindtrab$id)

# Seleccionar variables ---------------------------------------------------

#empleadores <- empleadores %>% rename_with(~ paste0(.x,"_a"), starts_with("l1"))
#sindtrab <- sindtrab %>% rename_with(~ paste0(.x,"_b"), starts_with("l1"))

proc <- bind_cols(autoaplicado %>% dplyr::select(k2,k3_3, starts_with("m3"),actividad_economica,starts_with("a3"),starts_with("a4"),a7,starts_with("b1"),
                                                 starts_with("c2"),starts_with("c3"),c10,
                                                 starts_with("f1")),
                  empleadores %>% dplyr::select(j4,starts_with("j5"),starts_with("l1")),
                  sindtrab %>% dplyr::select(sindicato, starts_with("k21"),starts_with("k22"), starts_with("k23")))

# Crear y transformar variables -------------------------------------------

proc <- proc %>% mutate(
  #Dependientes
  camara = case_when(
    j4 == 1 ~ 1,
    j4 == 2 ~ 0,
    TRUE ~ NA_real_),
  gremio = case_when(
    if_any(c(j5_88, j5_99), ~ .x == 1) ~ 0,
    if_any(c(starts_with("j5_"), -any_of(c("j5_88", "j5_99"))),~ .x %in% c("88", "96", "99")) ~ 0, # No hay 88, 99 y 96 asi que esta linea da lo mismo
    if_all(c(starts_with("j5_"), -any_of(c("j5_88", "j5_99"))),~ .x == 2) ~ 0,
    if_any(c(starts_with("j5_"), -any_of(c("j5_88", "j5_99"))),~ .x == 1) ~ 1,
    TRUE ~ NA_real_)) %>% 
  
       # Variables j5 tienen 3384 NA: lapply(proc %>% select(starts_with("j5_")), table, useNA = "ifany")lapply(proc %>% select(starts_with("j5_")), table, useNA = "ifany")
  
  #independientes
  
  mutate(union = sindicato,
         union_afiliation = case_when(b1_3_7 == 0 ~ NA_real_,
                                      TRUE ~ k3_3/b1_3_7),
         union_afiliation = case_when(union_afiliation < 0 ~ NA_real_,
                                      TRUE ~ union_afiliation),
         fragmentation = case_when(k2 == 0 ~ NA_real_,
                                   TRUE ~ union_afiliation / k2),
         across(starts_with("m3_"), ~ case_when(.x == 96 ~ NA_real_,
                                               TRUE ~ .x)),
         collective_bargaining_coverage = m3_1_1 + m3_2_1 + m3_1_2 + m3_2_2,
         union_power = case_when(collective_bargaining_coverage == 0 ~ NA_real_,
                                 TRUE ~ as.numeric(union_afiliation/collective_bargaining_coverage)),
         confederation = case_when(k21_22_23_sind == 2 ~ 0,
                                   k21_22_23_sind == 1 ~ 1,
                                   TRUE ~ NA_real_),
         legal_mobilization = as.integer(rowSums(across(l1_01:l1_04, ~ .x == 1), na.rm = TRUE) > 0),
         strikes = as.integer(rowSums(across(l1_05:l1_10, ~ .x == 1), na.rm = TRUE) > 0)) %>% 
  
  #Controles
  
  mutate(economic_industry = factor(actividad_economica, 
                                      labels = c("Agricultura, ganadería, silvicultura y pesca",
                                                 "Minería",
                                                 "Industria manufacturera",
                                                 "Suministro de electricidad, gas y agua; Gestión de desechos",
                                                 "Construcción",
                                                 "Comercio",
                                                 "Transporte, almacenamiento, información y comunicaciones",
                                                 "Alojamiento y servicio de comidas",
                                                 "Actividades financieras y de seguros; Actividades inmobiliarias",
                                                 "Actividades profesionales y técnicas; Servicios administrativos y de apoyo",
                                                 "Enseñanza",
                                                 "Salud y asistencia social",
                                                 "Actividades artísticas y recreativas; Otros servicios")),
         company_size = case_when(b1_3_7 <= 49 ~ 1,
                                  b1_3_7 >= 50 & b1_3_7 <= 199 ~ 2,
                                  b1_3_7 >= 200 ~ 3)
         )
         
# Exportar datos ----------------------------------------------------------

saveRDS(proc, file = "output/proc.rds")
