library(tidyverse)
library(httr)
library(jsonlite)

# 
source(file = "code/aux_fun.r")


# Mosqlimate / Infodengue data (Suspected) --------------------------------

# Epiweek 41/2015 starts on 2015-10-11

# # Slow for the whole country and depends on internet connection
df <- get_cases(start_date = "2015-10-11",
                end_date = today(),
                disease = 'dengue',
                # uf=NULL, # RJ
                muncode= 4205407,
                # per_page = 100
               )

library(INLA)


# Prepare data

df.floripa <- df |> 
  mutate(data_iniSE = ymd(data_iniSE)) |> 
  prepare.data(suspected_cases = T, muncode = 4205407) 

# Excluindo a temporada 2024-2025
df.floripa.inla <- df.floripa |> 
  filter(date < "2024-10-06") |> 
  prepare.data.4.inla()

# Estimando (Se quiser a amostra Monte Carlo das previsoes: MC=T)
teste <- forecasting.inla(data.inla = df.floripa.inla, MC = F) 

teste$pred |> View()


# Plot feioso

ggplot( mapping = aes(x = week) ) + 
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
  geom_point(mapping = aes(y = cases, color = "Observed"), 
            data =  df.floripa |>  
              filter(year == "2024-2025") 
  ) + 
  labs(
    colour = "Predictive percentile",
    y = "Cases"
  ) + 
  theme_bw()


