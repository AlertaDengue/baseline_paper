library(tidyverse)
# library(httr)
# library(jsonlite)
library(INLA)


# Packages for the plotting
library(MetBrewer)



# 
source(file = "code/aux_fun.r")


# Mosqlimate / Infodengue data (Suspected) --------------------------------

# Epiweek 41/2015 starts on 2015-10-11

mun.code = 3304557 # Rio
# mun.code = 3550308 # Sao Paulo
# mun.code = 4205407 # Floripa
# mun.code = 3548906 # Sao Carlos - SP
mun.code = 5208707 # Goiania

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


# Plot bonito (I hope)

# Aesthetics  -------------------------------------------------------------

# Creating the palette for the plots: 
pal <- met.brewer('Hiroshige', 5)[1:3]
pal2 <- c(met.brewer('Hiroshige', 7)[1], pal)

theme_legend_right <-  theme(text = element_text(family = 'arial'),
                             panel.border = element_blank(),
                             panel.grid.minor = element_blank(),
                             plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
                             axis.title.y = element_text(face = 'bold', margin = margin(r = .3, unit = 'cm')),
                             plot.margin = margin(t = 0.2, b = 0.2, r = 0.2, l = 0.2, unit = 'cm'),
                             legend.box.margin = margin(t = -0.2, b = -0.2, l = -0.2, r = 0.1, unit = 'cm'),
                             legend.position = 'right', 
                             legend.spacing.y = unit(-0.4, "cm"),
                             legend.text = element_text(size = 12), 
                             legend.key.spacing.y = unit(0.3, "cm"),
                             legend.title = element_text(size = 13, face = 'bold')) 

guides_2legends <- guides(fill = guide_legend(order = 1, override.aes = list(linetype = 0), 
                                              title.position = "top", 
                                              theme = theme(rect = element_rect(fill = NA),
                                                            legend.key = element_rect(fill = NA),
                                                            legend.title = element_text(hjust = 0.5, margin = margin(b = .3, unit = 'cm')))),
                          color = guide_legend(order = 0, 
                                               title = element_blank(), 
                                               title.position = "top", 
                                               theme = theme(rect = element_rect(fill = NA), 
                                                             legend.key = element_rect(fill = NA),
                                                             legend.text = element_text(face = 'bold'))))

guides_legends_chart <- guides(fill = guide_legend(order = 1, override.aes = list(linetype = 0), 
                                                   title.position = "top", 
                                                   theme = theme(rect = element_rect(fill = NA),
                                                                 legend.key = element_rect(fill = NA),
                                                                 legend.text = element_text(face = 'bold'))),
                               color = guide_legend(order = 0, 
                                                    override.aes = list(linetype = c('dashed', 'solid')),
                                                    title = element_blank(), 
                                                    title.position = "top", 
                                                    theme = theme(rect = element_rect(fill = NA), 
                                                                  legend.key = element_rect(fill = NA),
                                                                  legend.text = element_text(face = 'bold'))))




df4plot <- function(obj, df, ano) {
  
  observed <- df |> 
    mutate(
      date = ymd(data_iniSE),
      year = season(data_iniSE)) |> 
    filter(year == "2025-2026") 
    
  
  temp <- obj |> 
    transmute(date = date,
              # epiweek = epiweek(date),
              # epiyear = epiyear(date),
              q50 = `0.5quant`, 
              q75 = `0.75quant`,
              q90 = `0.9quant`,
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
    left_join(tmp2, by = c('date', 'quantile')) |> 
    ungroup() |> 
    left_join(observed, by = 'date') # |> 
    # mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12), 
    #        epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
    #        date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
  
  tmp$quantile <- factor(tmp$quantile, 
                         levels = c('q50','q75','q90','q100'),
                         labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
                                    'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
  return(tmp)
}


tmp = df4plot(obj = teste$pred, df = df)

ggplot(tmp, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
  geom_line(aes(y = casos_est, color = 'black'), linewidth = 0.5, show.legend = T) +
  geom_point(aes(y = casos)) +
  # geom_line(aes(y = casos_est, color = 'red'), linewidth = 0.5, show.legend = T) +
  # scale_color_manual(values = c('black', 'red'), name = '', labels = c('Observed cases', 'Delay corrected')) + 
  scale_color_manual(values = 'black', name = '', labels = 'Cases (delay corrected)') + 
  geom_ribbon(aes(y = casos_est, ymin = casos_est_min, ymax = casos_est_max), fill = "gray", alpha = 0.4) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    title = '2025-2026',
    x = "",
    y = "Number of cases"
  ) + theme_legend_right +
  theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.5, l = 0.2, unit = 'cm')) +
  guides_2legends +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = .5))
