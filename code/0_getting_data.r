# library(plyr)
library(tidyverse)
library(httr)
library(jsonlite)

source(file = "code/aux_fun.r")


# Mosqlimate / Infodengue data (Suspected) --------------------------------

# Epiweek 41/2015 starts on 2015-10-11

# # Slow for the whole country and depends on internet connection
# df <- get_cases(start_date = "2015-10-11", 
#                 end_date = today(), 
#                 disease = 'dengue',
#                 # uf=NULL, # RJ
#                 # muncode= 3304557,
#                 # per_page = 100
#                )

# saveRDS(object = df, file = "~/Data/df.info.rds")
# df <- readRDS(file = "~/Data/df.info.rds")

df.clean <- df |>
  select(data_iniSE, municipio_geocodigo, starts_with("casos")) |>
  mutate(
    data_iniSE = ymd(data_iniSE),
    ID_MN_RESI = floor(municipio_geocodigo/10)
    )


# SINAN ftp data (Probable) -----------------------------------------------

# denv <- list()
# 
# denv[[1]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR15.csv.zip")
# denv[[2]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR16.csv.zip")
# denv[[3]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR17.csv.zip")
# denv[[4]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR18.csv.zip")
# denv[[5]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR19.csv.zip")
# denv[[6]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR20.csv.zip")
# denv[[7]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR21.csv.zip")
# denv[[8]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR22.csv.zip")
# denv[[9]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR23.csv.zip")
# denv[[10]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR24.csv.zip")
# denv[[11]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR25.csv.zip")
# 
# df.sinan <- lapply(
#   denv,
#   FUN = function(x){
#     x |> select(ID_MN_RESI, DT_SIN_PRI, SEM_PRI, CLASSI_FIN)
#   }
# ) |> bind_rows()
# 
# 
# df.sinan <- df.sinan |>
#   filter(DT_SIN_PRI >= "2015-10-11", DT_SIN_PRI < "2025-12-31") |>
#   drop_na(ID_MN_RESI) |>
#   mutate(
#     DT_SIN_PRI.sun = DT_SIN_PRI - as.numeric(format(DT_SIN_PRI, "%w"))
#   )
# 
# df3 <- df.sinan |>
#   filter(CLASSI_FIN != 5 | is.na(CLASSI_FIN)) |>
#   group_by(data_iniSE = DT_SIN_PRI.sun, ID_MN_RESI) |>
#   summarise(
#     casos_prov = n()
#   )
# 
# saveRDS(object = df3, file = "~/Data/df.sinan.rds")
# df3 <- readRDS(file = "~/Data/df.info.rds")






# Joining suspected and probable ------------------------------------------


dengue.df <- df.clean |>
  left_join(df3, by = join_by(data_iniSE, ID_MN_RESI)) |>
  ungroup()

dengue.df$casos_prov = replace_na(dengue.df$casos_prov, replace = 0)

# write_csv(x = dengue.df, file = "data/cases.csv.gz", progress = T)

dengue.df <- vroom::vroom("data/cases.csv.gz")

nmissing <- function(x) sum(is.na(x))
plyr::colwise(nmissing)(dengue.df)


dengue.df |> 
  group_by(data_iniSE) |> 
  summarise(
    casos_susp = sum(casos),
    casos_prov = sum(casos_prov),
  ) |> 
  ggplot(aes(x = data_iniSE)) +
  geom_line(aes(y=casos_susp, color="Supected (infodengue)")) +   
  geom_line(mapping = aes(y=casos_prov, color="Probable (SINAN)") ) +   
  theme_bw()


dengue.df |> 
  group_by(data_iniSE) |> 
  summarise(
    casos_susp = sum(casos),
    casos_prov = sum(casos_prov),
  ) |> 
  mutate(
    prop_desc = casos_prov / casos_susp 
  ) |> 
  ggplot(aes(x = data_iniSE, y = prop_desc)) +   
  geom_line() +   
  theme_bw()
