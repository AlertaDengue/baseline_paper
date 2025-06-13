# library(plyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(plotly)
library(INLA)
library(tidyverse)
library(geofacet)
library(MetBrewer)
library(ggpubr)
library(scales)
library(ggnewscale)
library(sf)
library(colorspace)
library(geobr)

source(file = "../code/aux_fun.r")


### 

# Aesthetics  -------------------------------------------------------------

# Creating the palette for the plots: 
pal <- met.brewer('Hiroshige', 5)[1:3]
pal2 <- c(met.brewer('Hiroshige', 7)[1], pal)


# Loading data and results ------------------------------------------------

dengue.df <- vroom::vroom("../data/cases.csv.gz")
spatial.tbl <- vroom::vroom("../data/spatial.tbl.csv")

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

df.prob.22_23 <- read_csv(file = "../samples/macro.prob.22_23.csv.gz")
df.prob.23_24 <- read_csv(file = "../samples/macro.prob.23_24.csv.gz")
df.prob.24_25 <- read_csv(file = "../samples/macro.prob.24_25.csv.gz")


# Table 2 -----------------------------------------------------------------


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

t1br <- df4BRplot(df.prob.22_23, ano = 2022)

max_valor <- max(t1br$maxvalues[is.finite(t1br$maxvalues)], t1br$cases, na.rm = TRUE)
t1br_max_round <- ceiling(max_valor / 50) * 50
t1br_max_round2 <- t1br_max_round + (t1br_max_round * 0.1)

t1br2 <- t1br |> 
  mutate(
    maxvalues = ifelse(is.infinite(maxvalues), t1br_max_round2, maxvalues)
  )

plot_grafico_artigo <- function(df, palette) {
  
  quants = rev(unique(df$quantile))
  colors = palette[seq_along(quants)]
  
  plot_ly() |>
    add_trace(
      data = df |> filter(quantile == quants[1]), 
      type = 'scatter', 
      mode = 'lines', 
      x = ~date, 
      y = ~maxvalues,
      fill = 'tozeroy',
      line = list(color = colors[1]),
      fillcolor = colors[1],
      name = quants[1],
      hovertemplate = paste0(
        "<b>", quants[1], "</b>",
        "<extra></extra>"
      )
    ) |> 
    add_trace(
      data = df |> filter(quantile == quants[2]), 
      type = 'scatter', 
      mode = 'lines', 
      x = ~date, 
      y = ~maxvalues,
      fill = 'tozeroy',
      line = list(color = colors[2]),
      fillcolor = colors[2],
      name = quants[2],
      hovertemplate = paste0(
        "<b>", quants[2], "</b>",
        "<extra></extra>"
      )
    ) |> 
    add_trace(
      data = df |> filter(quantile == quants[3]), 
      type = 'scatter', 
      mode = 'lines', 
      x = ~date, 
      y = ~maxvalues,
      fill = 'tozeroy',
      line = list(color = colors[3]),
      fillcolor = colors[3],
      name = quants[3],
      hovertemplate = paste0(
        "<b>", quants[3], "</b>",
        "<extra></extra>"
      )
    ) |> 
    add_trace(
      data = df |> filter(quantile == quants[4]), 
      type = 'scatter', 
      mode = 'lines', 
      x = ~date, 
      y = ~maxvalues,
      fill = 'tozeroy',
      line = list(color = colors[4]),
      fillcolor = colors[4],
      name = quants[4],
      hovertemplate = paste0(
        "<b>Below the median, typical</b>",
        "<extra></extra>"
      )
    ) |> 
    add_trace(
      data = df,
      type = 'scatter',
      mode = 'lines',
      x = ~date,
      y = ~cases,
      line = list(color = "black"),
      name = "Cases",
      hovertemplate = paste0(
        "<b>Cases</b>: %{y:,.0f}", 
        "<extra></extra>"
      )
    ) |> 
    layout(
      hovermode = "x unified",
      xaxis = list(
        title    = "Date",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title    = "Dengue cases",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      legend = list(
        itemclick       = FALSE,  # impede esconder traces com um clique :contentReference[oaicite:0]{index=0}
        itemdoubleclick = FALSE   # impede isolar um trace com duplo clique :contentReference[oaicite:1]{index=1}
      )
    )
  
}

fig <- plot_grafico_artigo(t1br2, pal2)
fig
