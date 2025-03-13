library(tidyverse)
library(INLA)
source("code/aux_fun.r")


# dengue <- read_csv("~/Dropbox/Research/Modelagem/Baseline_forecasts/data/dengue.csv.gz")
dengue.df <- vroom::vroom("data/cases.csv.gz")
spatial.tbl <- vroom::vroom("data/spatial.tbl.csv")

dengue.df <- dengue.df |> 
  left_join(spatial.tbl |> 
              select(geocode, uf, macroregional, macroregional_geocode), 
            by = c("municipio_geocodigo"="geocode") )



nmissing <- function(x) sum(is.na(x))
plyr::colwise(nmissing)(dengue.df)

# Dados por macroregional - casos supeitos
dados.macro <- dengue.df |> prepare.data(suspected_cases = T)

# Dados por macroregional - casos provaveis
dados.prov.macro <- dengue.df |> prepare.data(suspected_cases = F)


# selufs <- c("SP", "RJ", "MG")
# selufs <- sort(unique(dengue.df$uf))


set.seed(123456)



# Criando as listas

macros <- unique(dados.macro$macroregional_geocode)



list.train.susp.24_25 = vector(mode = "list", length = length(macros))
names(list.train.susp.24_25) = macros

list.train.prov.24_25 = list.train.susp.24_25

list.train.susp.23_24 = list.train.susp.24_25
list.train.susp.22_23 = list.train.susp.24_25

list.train.prov.23_24 = list.train.susp.24_25
list.train.prov.22_23 = list.train.susp.24_25


#k = 1
for(k in 1:length(macros)){
  
  # Predict season 2024-2025
  data.susp.macro.k = 
    dados.macro |> 
    filter(geocode == macros[k], date < "2024-10-06") |> 
    prepare.data.4.inla()

  UF = data.susp.macro.k$uf[1]
  
    
  data.prov.macro.k = 
    dados.prov.macro |> 
    filter(geocode == macros[k], date < "2024-10-06") |> 
    prepare.data.4.inla()
  
  
  
  list.train.susp.24_25[[k]] <- forecasting.inla(data.inla = data.susp.macro.k, MC =T) |>    
    output.treat.macro( UF=UF, geocode = macros[k])
  
  list.train.prov.24_25[[k]] <- forecasting.inla(data.inla = data.prov.macro.k, MC =T) |>    
    output.treat.macro( UF=UF, geocode = macros[k])
  
  
  # Predict season 2023-2024
  data.susp.macro.k = 
    dados.macro |> 
    filter(geocode == macros[k], date < "2023-10-08") |> 
    prepare.data.4.inla()
  
  data.prov.macro.k = 
    dados.prov.macro |> 
    filter(geocode == macros[k], date < "2023-10-08") |> 
    prepare.data.4.inla()
  
  list.train.susp.23_24[[k]] <- forecasting.inla(data.inla = data.susp.macro.k, MC =T) |>    
    output.treat.macro( UF=UF, geocode = macros[k])
  
  list.train.prov.23_24[[k]] <- forecasting.inla(data.inla = data.prov.macro.k, MC =T) |>    
    output.treat.macro( UF=UF, geocode = macros[k])

  
  # Predict season 2022-2023
  data.susp.macro.k = 
    dados.macro |> 
    filter(geocode == macros[k], date < "2022-10-09") |> 
    prepare.data.4.inla()
  
  data.prov.macro.k = 
    dados.prov.macro |> 
    filter(geocode == macros[k], date < "2022-10-09") |> 
    prepare.data.4.inla()
  
  list.train.susp.22_23[[k]] <- forecasting.inla(data.inla = data.susp.macro.k, MC =T) |>    
    output.treat.macro( UF=UF, geocode = macros[k])
  
  list.train.prov.22_23[[k]] <- forecasting.inla(data.inla = data.prov.macro.k, MC =T) |>    
    output.treat.macro( UF=UF, geocode = macros[k])
  
  cat(k, UF, macros[k], "\n")
  
}


df.prob.22_23 <- list.train.prov.22_23 |> map(.f = function(x) x$MC) |> bind_rows()
df.susp.22_23 <- list.train.susp.22_23 |> map(.f = function(x) x$MC) |> bind_rows()

df.prob.23_24 <- list.train.prov.23_24 |> map(.f = function(x) x$MC) |> bind_rows()
df.susp.23_24 <- list.train.susp.23_24 |> map(.f = function(x) x$MC) |> bind_rows()

df.prob.24_25 <- list.train.prov.24_25 |> map(.f = function(x) x$MC) |> bind_rows()
df.susp.24_25 <- list.train.susp.24_25 |> map(.f = function(x) x$MC) |> bind_rows()


df.prob.22_23 |> write_csv(file = "samples/macro.prob.22_23.csv.gz")
df.susp.22_23 |> write_csv(file = "samples/macro.susp.22_23.csv.gz")

df.prob.23_24 |> write_csv(file = "samples/macro.prob.23_24.csv.gz")
df.susp.23_24 |> write_csv(file = "samples/macro.susp.23_24.csv.gz")

df.prob.24_25 |> write_csv(file = "samples/macro.prob.24_25.csv.gz")
df.susp.24_25 |> write_csv(file = "samples/macro.susp.24_25.csv.gz")

