
# Loading data --------------------------------------------------------------------------------

dengue.df <- vroom::vroom("data/cases.csv.gz")
spatial.tbl <- vroom::vroom("data/spatial.tbl.csv")

dengue.df <- dengue.df |> 
  left_join(spatial.tbl |> 
              select(geocode, uf, macroregional, macroregional_geocode), 
            by = c("municipio_geocodigo"="geocode") )

dados.macro <- dengue.df |> 
  prepare.data(suspected_cases = F)

observed <- dados.macro |> 
  rename(season = year) |> 
  mutate(year.s.first = as.numeric(str_sub(season, 1, 4))) |> 
  filter(season >= 2022)


# Wrangling Brazil data -----------------------------------------------------------------------

df4BRplot <- function(obj, ano) {
  
  observed.tmp <- observed |> 
    filter(year.s.first == ano) |> 
    group_by(week, season, year.s.first, date) |> 
    summarise(cases = sum(cases, na.rm = TRUE)) 
  
  temp <- obj |> 
    group_by(week, samples) |> 
    summarise(values = sum(values)) |> 
    ungroup() |> 
    group_by(week) |> 
    summarise(q50 = quantile(values, probs = 0.5), 
              q75 = quantile(values, probs = 0.75),
              q90 = quantile(values, probs = 0.9),
              q100 = Inf) 
  
  tmp1 <- temp |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'maxvalues'
    )
  
  tmp2 <- temp |> 
    mutate(q100 = q90,
           q90 = q75,
           q75 = q50,
           q50 = 0) |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'minvalues'
    )
  
  tmp <- tmp1 |> 
    left_join(tmp2, by = c('week', 'quantile')) |> 
    ungroup() |> 
    left_join(observed.tmp, by = 'week') |> 
    mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12), 
           epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
           date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
  
  tmp$quantile <- factor(tmp$quantile, 
                         levels = c('q50','q75','q90','q100'),
                         labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
                                    'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
  return(tmp)
}


# Wrangling UF data ---------------------------------------------------------------------------

df4UFplot <- function(obj, ano) {
  
  observed.tmp <- observed |> 
    filter(year.s.first == ano) |> 
    group_by(week, season, year.s.first, uf) |> 
    summarise(cases = sum(cases, na.rm = TRUE)) 
  
  temp <- obj |> 
    group_by(week, samples, uf) |> 
    summarise(values = sum(values)) |> 
    ungroup() |> 
    group_by(week, uf) |> 
    summarise(q50 = quantile(values, probs = 0.5), 
              q75 = quantile(values, probs = 0.75), 
              q90 = quantile(values, probs = 0.9),
              q100 = Inf) 
  
  tmp1 <- temp |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'maxvalues'
    )
  
  tmp2 <- temp |> 
    mutate(q100 = q90,
           q90 = q75,
           q75 = q50,
           q50 = 0) |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'minvalues'
    )
  
  tmp <- tmp1 |> 
    left_join(tmp2, by = c('week', 'uf', 'quantile')) |> 
    ungroup() |> 
    left_join(observed.tmp, by = c('week', 'uf')) |> 
    mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12), 
           epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
           date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
  
  tmp$quantile <- factor(tmp$quantile, 
                         levels = c('q50','q75','q90','q100'),
                         labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
                                    'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
  return(tmp)
}


# Wrangling Health District data --------------------------------------------------------------

df4HDplot <- function(obj, ano) {
  
  observed.tmp <- observed |> 
    filter(year.s.first == ano) |> 
    group_by(week, season, year.s.first, uf, macroregional, macroregional_geocode) |> 
    summarise(cases = sum(cases, na.rm = TRUE)) 
  
  temp <- obj |> 
    group_by(week, samples, uf, macrocode) |> 
    summarise(values = sum(values)) |> 
    ungroup() |> 
    group_by(week, uf, macrocode) |> 
    summarise(q50 = quantile(values, probs = 0.5), 
              q75 = quantile(values, probs = 0.75), 
              q90 = quantile(values, probs = 0.9),
              q100 = Inf) 
  
  tmp1 <- temp |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'maxvalues'
    )
  
  tmp2 <- temp |> 
    mutate(q100 = q90,
           q90 = q75,
           q75 = q50,
           q50 = 0) |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'minvalues'
    )
  
  tmp <- tmp1 |> 
    left_join(tmp2, by = c('week', 'uf', 'quantile', 'macrocode')) |> 
    ungroup() |> 
    left_join(observed.tmp, by = c('week', 'uf', 'macrocode' = 'macroregional_geocode')) |> 
    mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12), 
           epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
           date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
  
  tmp$quantile <- factor(tmp$quantile, 
                         levels = c('q50','q75','q90','q100'),
                         labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
                                    'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
  return(tmp)
}
