# app.R

library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(geobr)
library(aweek)
library(vroom)
library(MetBrewer)


# Auxiliary functions -------------------------------------------------------------------------

source("aux_fun.r")
source("data_fun.r")
source("graf_fun.r")


# Palette -------------------------------------------------------------------------------------

pal <- met.brewer('Hiroshige', 5)[1:3]
pal2 <- c(met.brewer('Hiroshige', 7)[1], pal)


# Reading and manipulating data ---------------------------------------------------------------

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


# Datasets for Brazil, UFs and Health Regions -------------------------------------------------

t1br <- df4BRplot(df.prob.22_23, ano = 2022)
t2br <- df4BRplot(df.prob.23_24, ano = 2023)

t1uf <- df4UFplot(df.prob.22_23, ano = 2022)
t2uf <- df4UFplot(df.prob.23_24, ano = 2023)



# UI ------------------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Dashboard - Freitas et al. 2025"),
  
  tabsetPanel(
    
    tabPanel("Brasil",
             column(
               width = 6,
               plotlyOutput("plotBR1", height = "600px")
             ),
             column(
               width = 6,
               plotlyOutput("plotBR2", height = "600px")
             )
    ),
    
    tabPanel("UF",
             selectInput(
               inputId = "uf", 
               label   = "Escolha a UF:", 
               choices = sort(unique(dengue.df$uf)), 
               selected = sort(unique(dengue.df$uf))[1]
             ),
             fluidRow(
               column(width = 6,
                      plotlyOutput("plotUF1", height = "600px")
               ),
               column(width = 6,
                      plotlyOutput("plotUF2", height = "600px")
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
  output$plotBR1 <- renderPlotly({
    plot_grafico_artigo(t1br, pal2)
  })
  
  output$plotBR2 <- renderPlotly({
    plot_grafico_artigo(t2br, pal2)
  })
  
  
  # 2) UF
  output$plotUF1 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t1uf, uf, pal2)
  })
  
  output$plotUF2 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t2uf, uf, pal2)
  })
  
  # 3) Macrorregião
  output$plotMacro <- renderPlotly({
    
  })
  
}


# RUN APP -------------------------------------------------------------------------------------

shinyApp(ui, server)
