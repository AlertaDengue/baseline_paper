library(tidyverse)
library(httr)
library(jsonlite)
library(INLA)

# 
source(file = "code/aux_fun.r")


# Mosqlimate / Infodengue data (Suspected) --------------------------------

# Epiweek 41/2015 starts on 2015-10-11

mun.code = 3304557 # Rio
# mun.code = 3550308 # Sao Paulo
# mun.code = 4205407 # Floripa
# mun.code = 3548906 # Sao Carlos - SP


df = get_infodengue_cases(geocode = mun.code) |> 
  add_column(municipio_geocodigo = mun.code)

# # # Slow for the whole country and depends on internet connection
# df <- get_cases(start_date = "2015-10-11",
#                 end_date = today(),
#                 disease = 'dengue',
#                 # uf=NULL, # RJ
#                 muncode= mun.code,
#                 # per_page = 100
#                )



# Prepare data

df.aux <- df |> 
  # mutate(data_iniSE = ymd(data_iniSE)) |> 
  prepare.data(suspected_cases = T, muncode = mun.code) 

# # Excluindo a temporada 2024-2025
# df.floripa.inla <- df.floripa |> 
#   filter(date < "2024-10-06") |> 
#   prepare.data.4.inla()

# Excluindo a temporada 2025-2026
df.aux.inla <- df.aux |> 
  filter(date < "2025-10-05") |> 
  prepare.data.4.inla()

# Estimando (Se quiser a amostra Monte Carlo das previsoes: MC=T)
teste <- forecasting.inla(data.inla = df.aux.inla, MC = F) 

# teste$pred |> View()

tbl.season <- tibble(date = min(df.aux$date) + 7*(0:800)) |> 
  mutate(
    week = week.season(date),
    year = season(date)
  )

# Plot feioso
teste$pred <- teste$pred |> left_join(tbl.season, by = c("week", "year")) 

ggplot( mapping = aes(x = date) ) + 
  geom_line(aes(y = `0.5quant`, color = "P50"),
            data = teste$pred, linetype = "dashed"
  ) + 
  geom_line(aes(y = `0.75quant`, color = "P75"),
            data = teste$pred,
            linetype = "dashed"
  ) + 
  geom_line(aes(y = `0.25quant`, color = "P25"),
            data = teste$pred,
            linetype = "dashed"
  ) + 
  geom_line(aes(y = `0.9quant`, color = "P90"),
            data = teste$pred,
            linetype = "dashed"
  ) + 
  geom_pointrange(mapping = aes(y = casos_est, 
                                ymin = casos_est_min, 
                                ymax = casos_est_max, 
                                color = "nowcasting"), 
             data =    df |> 
               mutate(
                 date = ymd(data_iniSE),
                 year = season(data_iniSE)) |> 
               filter(year == "2025-2026") 
  ) + 
  geom_point(mapping = aes(y = cases, color = "Observed"), 
             data =  df.aux |>  
               filter(year == "2025-2026") 
  ) + 
  labs(
    colour = "Predicted cases",
    y = "Cases",
    x = "Week",
    title = "Season 2025-2026"
  ) + 
  theme_bw()


