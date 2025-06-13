# app.R

library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(geobr)
library(aweek)
library(vroom)

source("aux_fun.r")

# Palette
pal <- met.brewer('Hiroshige', 5)[1:3]
pal2 <- c(met.brewer('Hiroshige', 7)[1], pal)

# Função plot BR
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

# Seu objeto t1br2 para Brasil
t1br <- df4BRplot(df.prob.22_23, ano = 2022)
max_valor <- max(t1br$maxvalues[is.finite(t1br$maxvalues)], t1br$cases, na.rm = TRUE)
t1br_max_round <- ceiling(max_valor / 50) * 50
t1br_max_round2 <- t1br_max_round + (t1br_max_round * 0.1)
t1br2 <- t1br |> mutate(maxvalues = ifelse(is.infinite(maxvalues), t1br_max_round2, maxvalues))

# Função de plotagem
plot_grafico_artigo <- function(df, palette) {
  quants  <- rev(unique(df$quantile))
  colors  <- palette[seq_along(quants)]
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig %>% add_trace(
      data      = df |> filter(quantile == quants[i]),
      type      = 'scatter', mode = 'lines',
      x         = ~date, y = ~maxvalues,
      fill      = 'tozeroy',
      line      = list(color = colors[i]),
      fillcolor = colors[i],
      name      = quants[i],
      hovertemplate = paste0("<b>", quants[i],"</b><extra></extra>")
    )
  }
  fig %>%
    add_trace(
      data      = df,
      type      = 'scatter', mode = 'lines',
      x         = ~date, y = ~cases,
      line      = list(color = "black"),
      name      = "Cases",
      hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
    ) %>%
    layout(
      hovermode = "x unified",
      xaxis = list(title="Date",    showgrid=FALSE, zeroline=FALSE),
      yaxis = list(title="Dengue cases", showgrid=FALSE, zeroline=FALSE),
      legend = list(itemclick=FALSE, itemdoubleclick=FALSE)
    )
}



# UI ------------------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Previsão de Dengue"),
  tabsetPanel(
    tabPanel("Brasil",
             plotlyOutput("plotBR", height = "600px")
    ),
    tabPanel("UF",
             sidebarLayout(
               sidebarPanel(
                 selectInput("uf", "Escolha a UF:", choices = sort(unique(dengue.df$uf)))
               ),
               mainPanel(
                 plotlyOutput("plotUF", height = "600px")
               )
             )
    ),
    tabPanel("Macrorregião",
             sidebarLayout(
               sidebarPanel(
                 selectInput("macro", "Escolha a macrorregião:", choices = sort(unique(dengue.df$macroregional)))
               ),
               mainPanel(
                 plotlyOutput("plotMacro", height = "600px")
               )
             )
    )
  )
)


# SERVER --------------------------------------------------------------------------------------


server <- function(input, output, session) {
  
  # 1) Brasil
  output$plotBR <- renderPlotly({
    plot_grafico_artigo(t1br2, pal2)
  })
  
  # 2) UF
  output$plotUF <- renderPlotly({
    
  })
  
  # 3) Macrorregião
  output$plotMacro <- renderPlotly({
    
  })
  
}


# RUN APP -------------------------------------------------------------------------------------

shinyApp(ui, server)
