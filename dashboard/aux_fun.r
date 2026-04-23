# Auxiliar functions 
# Leo Bastos

get_cases <- function(  
    start_date=today()-11*7, 
    end_date = today(), 
    disease = 'dengue',
    uf=NULL, # RJ
    muncode=NULL, # 3304557
    per_page = 100
){
  
  infodengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/"
  page <- "1"
  pagination <- paste0("?page=", page, "&per_page=", per_page, "&")
  
  
  filters <- paste0("disease=", disease, "&start=", start_date, 
                    "&end=", end_date)
  
  filters.extra <- case_when(
    !is.null(muncode) ~  paste0("&geocode=", muncode),
    (is.null(muncode) & !is.null(uf)) ~ paste0("&uf=", uf),
    TRUE ~ NA
  )
  
  if(!is.na(filters.extra)){
    filters <- paste0(filters, filters.extra)
  }
  
  url <- paste0(infodengue_api, pagination, filters)
  
  resp <- GET(url)
  
  content <- content(resp, "text")
  json_content <- fromJSON(content)
  items <- json_content$items
  
  pagination_data <- json_content$pagination
  
  PAGES <- pagination_data$total_pages
  
  if(PAGES>1){
    
    for(k in 2:PAGES){
      
      pagination <- paste0("?page=", k, "&per_page=", per_page, "&")
      
      json_content <- paste0(
        infodengue_api, 
        pagination, 
        filters) |> 
        GET() |> 
        content("text") |> 
        fromJSON() 
      
      items.k <- json_content$items
      items <- bind_rows(items, items.k)
    }
    
  }
  
  items
}




# library(INLA)
# library(lubridate)





prepare.data <- function(dado, 
                         muncode = NULL, 
                         suspected_cases = T # probable if FALSE 
                         ){
  
  # Dado organizado por municipio e salvo em algum canto (formato fixo)
  
  # > str(dengue.df)
  # spc_tbl_ [2,709,420 × 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
  # $ data_iniSE           : Date[1:2709420], format: "2025-02-02" "2025-02-02" "2025-02-02" ...
  # $ municipio_geocodigo  : num [1:2709420] 1200104 1200054 1200401 1200708 1200203 ...
  # $ casos_est            : num [1:2709420] 11 0 5372 10 129 ...
  # $ casos_est_min        : num [1:2709420] 2 0 1662 1 11 ...
  # $ casos_est_max        : num [1:2709420] 32 0 NA 70 1136 ...
  # $ casos                : num [1:2709420] 0 0 141 0 1 0 0 19 0 0 ...
  # $ ID_MN_RESI           : num [1:2709420] 120010 120005 120040 120070 120020 ...
  # $ casos_prov           : num [1:2709420] 0 0 141 0 1 0 0 19 0 0 ...
  # $ uf                   : chr [1:2709420] "AC" "AC" "AC" "AC" ...
  # $ macroregional        : chr [1:2709420] "Acre" "Acre" "Acre" "Acre" ...
  # $ macroregional_geocode: num [1:2709420] 1201 1201 1201 1201 1201 ...
  

  # Define the outcome (Suspected or probable cases)
  if(suspected_cases){
    dado$cases = dado$casos
  }else{
    dado$cases = dado$casos_prov 
  }
    
  
  # Filtering by municipality
  if(!is.null(muncode)){
    if(muncode > 999999){
      aux <- dado |> filter(municipio_geocodigo == muncode)  
    }else{
      aux <- dado |> filter(ID_MN_RESI == muncode)
    }
    
    aux$geocode = muncode

    if(nrow(aux)==0){
      stop("Invalid muncode.")
    }
    
    aux <- aux |> 
      transmute(
        # uf = uf,
        # macroregional = macroregional, 
        # macroregional_geocode = macroregional_geocode,
        date = data_iniSE,
        cases = cases,
        geocode = geocode,
      )
    
  }
  # By health macroregion (for a loop)
  else{
    aux <- dado |> 
      drop_na(uf, macroregional, macroregional_geocode) |> 
      group_by(uf, macroregional, macroregional_geocode, date = data_iniSE) |> 
      summarise(
        cases = sum(cases)
      ) |> 
      mutate(
        geocode = macroregional_geocode
      )
  }
  
  aux <- aux %>% 
    mutate(
      #Pop = Pop,
      week = week.season(date),
      # week = ifelse(week == 53, 52, week),
      year = season(date),
      target = F
    )
  
  # if(forecast.one.year.ahead){
  #   aux.y = aux$year |> substr(start = 1, stop = 4) |> as.numeric() |> max()
  #   new.season = season(epiweek = 1, epiyear = aux.y + 2)
  #   
  #   aux <- aux |> 
  #     bind_rows(
  #       tibble(
  #         week = 1:52,
  #         year = new.season,
  #         target = T
  #       )
  #     )
  # }
  
  aux
}


prepare.data.4.inla = function( dado, forecast.one.year.ahead = T ){

  # Dado - output of prepare.data filtered for only one region
  
  if(length(unique(dado$geocode)) != 1){
    stop("There are more than one geocode in the data")
  }
  
  if(forecast.one.year.ahead){
    aux.y = dado$year |> substr(start = 1, stop = 4) |> as.numeric() |> max()
    new.season = season(epiweek = 1, epiyear = aux.y + 2)

    suppressWarnings(
      aux <- dado |>
        bind_rows(
          tibble(
            week = 1:52,
            year = new.season,
            geocode = dado$geocode[1],
            target = T
          )
        )
    )
  }
  
  aux
  
}


forecasting.inla <- function(data.inla,   # dados - Data containing columns (geocode, cases, week, year, target)
                             #Year2forecast = 2023,
                             MC = FALSE, M = 2000,
                             # Changing the first week and redefine year as seasons
                             start = 41, 
                             # make week 53 as week 52 (POG!)
                             week53 = T,
                             quantiles = c(0.05, 0.25, 0.5, 0.75, 0.9, 0.95),
                             output.only = F,
                             likelihood = "nbinomial",
                             timeRE = "rw2",
                             cyclic = T,
                             WAIC = F){

  
  formula.q <- cases ~ 1 +
    f(week, model = timeRE, constr = T, cyclic = cyclic,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    ) + 
    f(year, model = "iid", constr = T,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    )
  
  # # Adding forecasting component
  # data.inla <- data.inla %>% 
  #   add_row(week = 1:52, Year = Year2forecast) %>% 
  #   mutate(year = Year - min(Year) + 1)
  
  linear.term.year.cur <- which(data.inla$target == T)
  
  
  output.mean <- inla(formula = formula.q, 
                      data = data.inla %>% 
                        mutate(
                          cases = ifelse(target==F, cases, NA)
                        ),
                      control.predictor = list(link = 1, compute = T, 
                                               quantiles = quantiles),
                      family = likelihood, 
                      # offset = log(Pop / 1e5),
                      # control.family = list(
                      #   control.link = list(
                      #     model = "quantile",
                      #     quantile = 0.5
                      #     )
                      #   ),
                      # control.fixed = control.fixed(prec.intercept = 1),
                      control.compute = list(config = MC, waic = WAIC)
  )
  
  out = NULL
  
  out$inla = output.mean
  
  out$pred = data.inla %>% ungroup() |> filter(target == T) %>% select(geocode, week, year) |> 
    bind_cols( output.mean$summary.fitted.values[linear.term.year.cur,] )
  
  if(MC){
    param.samples <- inla.posterior.sample(output.mean, n = M)
    
    samples.MC <- param.samples %>%
      map(.f = function(xxx, idx = linear.term.year.cur){
        rnbinom(
          n = idx, 
          mu = exp(xxx$latent[idx]), 
          size = xxx$hyperpar[1]
        )} ) 
    
    names(samples.MC) <- 1:M
    
    samples.MC <- samples.MC %>%   
      bind_rows(.id = "samples") %>% 
      rowid_to_column(var = "week") %>% 
      gather(key = "samples", value = "values", -week) 
    
    
    out$MC = samples.MC
  }
  
  out 
}


output.treat.macro = function(x, UF, geocode){
  
  x$pred$uf = UF
  x$pred$macrocode = geocode
  
  x$MC$uf = UF
  x$MC$macrocode = geocode
  
  x
  
}




threshold.MC <- function(samples.MC){
  thresholdw.data <- samples.MC %>%  
    group_by(week) %>% 
    summarise(
      Q1 = quantile(probs = 0.25, values),
      Q2 = quantile(probs = 0.50, values),
      Q3 = quantile(probs = 0.75, values),
      P90 = quantile(probs = 0.9, values),
    )
  thresholdw.data
} 



season <- function(x, epiweek=NULL, epiyear=NULL, start = 41){
  
  if( is.null(epiweek) | is.null(epiyear)){
    if(!is.Date(x)) stop("Not Date format")
    
    ew = epiweek(x)
    ey = epiyear(x)
    
  }else{
    ew = epiweek
    ey = epiyear
  }
  
  ifelse(ew < start, paste(ey-1, ey, sep = "-"), paste(ey, ey+1, sep = "-"))
}

# season(today())

week.season  <- function(x, epiweek=NULL, start = 41, week53 = T){
  
  if( is.null(epiweek) ){
    
    if(!is.Date(x)) stop("Not Date format")
    
    ew = epiweek(x)
  }else{
    ew = epiweek
  }
  
  if(week53 & any(ew==53)) ew[ew==53] = 52
  
  # 1 = start, 2 = start + 1, 3 = start + 2,...
  ifelse(ew >= start, ew - start + 1, 52 - start + 1 + ew)
}

# Funcao auxiliar para rodar os tres cenarios
run_inla_forecasting = function(dados, macro, uf){
  aux = forecasting.inla(dados = dados, MC =T)
  
  aux$pred$uf = uf
  aux$pred$macrocode = macro
  
  aux$MC$uf = uf
  aux$MC$macrocode = macro
  
  aux  
}
