# Visualising results

# LPF and LSB
# Last update 13-Mar-2025

rm(list = ls())
Sys.setlocale("LC_ALL","en_US.utf8")


# Packages ----------------------------------------------------------------
library(tidyverse)
library(geofacet)
library(MetBrewer)
library(ggpubr)
library(scales)
library(ggnewscale)
library(sf)
library(colorspace)
library(geobr)

source("code/aux_fun.r")


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



# Loading data and results ------------------------------------------------

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

df.prob.22_23 <- read_csv(file = "samples/macro.prob.22_23.csv.gz")
df.prob.23_24 <- read_csv(file = "samples/macro.prob.23_24.csv.gz")
df.prob.24_25 <- read_csv(file = "samples/macro.prob.24_25.csv.gz")



# Table 2 -----------------------------------------------------------------

totalSeasons <- function(obj, season.years){
  
  tmp <- obj |> 
    group_by(samples) |> 
    summarise(values = sum(values)) |> 
    mutate(season = season.years) |> 
    ungroup() |> 
    group_by(season) |> 
    summarise(q50 = quantile(values, probs = 0.5), 
              q75 = quantile(values, probs = 0.75),
              q90 = quantile(values, probs = 0.9)) |> 
    pivot_longer(
      cols = c(q50, q75, q90),
      names_to = 'quantile',
      values_to = 'values'
    ) |> 
    bind_rows(observed |> 
                filter(season == season.years) |> 
                group_by(season) |> 
                summarise(values = sum(cases, na.rm = TRUE)) |> 
                mutate(quantile = 'Observed')) 
  return(tmp)
}
  

totalSeasons(df.prob.22_23, season.years = '2022-2023')
totalSeasons(df.prob.23_24, season.years = '2023-2024')
totalSeasons(df.prob.24_25, season.years = '2024-2025')


# Fig 1 -------------------------------------------------------------------

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
t2br <- df4BRplot(df.prob.23_24, ano = 2023)


gt1br <- ggplot(t1br, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
  scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    title = '2022-2023',
    x = "",
    y = "Number of cases"
  ) + 
  theme_legend_right +
  theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.5, l = 0.2, unit = 'cm')) +
  guides_2legends +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = .5))

gt2br <- ggplot(t2br, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
  scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    title = '2023-2024',
    x = "",
    y = ""
  ) + 
  theme_legend_right +
  guides_2legends +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = .5))

ggarrange(gt1br, gt2br, common.legend = TRUE, legend = 'right', labels = "AUTO")


# Peak week
t1br |> 
  group_by(quantile) |> 
  filter(maxvalues != 'Inf', maxvalues == max(maxvalues)) |> 
  select(week, quantile, maxvalues)
t2br |> 
  group_by(quantile) |> 
  filter(maxvalues != 'Inf', maxvalues == max(maxvalues)) |> 
  select(week, quantile, maxvalues)


# Fig 2 -------------------------------------------------------------------

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

t1uf <- df4UFplot(df.prob.22_23, ano = 2022)
t2uf <- df4UFplot(df.prob.23_24, ano = 2023)

gt1uf <- ggplot(t1uf, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.4, show.legend = T) +
  scale_color_manual(values = 'black', name = '', labels = 'Observed') + 
  facet_geo(~uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    title = '2022-2023',
    x = "",
    y = "Number of cases"
  ) + 
  theme_legend_right +
  guides_2legends +
  theme(strip.text = element_text(face = 'bold'),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16, face = 'bold'))

gt2uf <- ggplot(t2uf, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.4, show.legend = T) +
  scale_color_manual(values = 'black', name = '', labels = 'Observed') + 
  facet_geo(~uf, grid = "br_states_grid1", scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b", expand = c(0,0)) +
  theme_bw(base_size = 16) +
  labs(
    title = '2023-2024',
    x = "",
    y = ""
  ) + 
  theme_legend_right +
  guides_2legends +
  theme(strip.text = element_text(face = 'bold'),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16, face = 'bold'))

ggarrange(gt1uf, gt2uf, nrow = 1, common.legend = TRUE, legend = 'none', labels = 'AUTO')


# Fig 3 -------------------------------------------------------------------

shape <- read_sf('data/shapefile_macro.json')

ufshape <- read_state(
  year = 2020, 
  showProgress = FALSE
)
ufshape <- ufshape |> 
  st_crop(xmin = -73.9904, ymin = -33.7511, xmax = -33.2476, ymax = 5.2718) 

shape <- st_set_crs(shape, st_crs(ufshape))

df4maps <- function(obj, ano) {
  
  observed.tmp <- observed |> 
    filter(year.s.first == ano) |> 
    group_by(macroregional_geocode) |> 
    summarise(cases = sum(cases, na.rm = TRUE)) 
  
  temp <- obj |> 
    group_by(samples, macrocode) |> 
    summarise(values = sum(values)) |> 
    ungroup() |> 
    group_by(macrocode) |> 
    summarise(q50 = quantile(values, probs = 0.5), 
              q75 = quantile(values, probs = 0.75), 
              q90 = quantile(values, probs = 0.9)) 
  
  tmp <- temp |> 
    ungroup() |> 
    left_join(observed.tmp, by = c('macrocode' = 'macroregional_geocode')) |> 
    mutate(validation = case_when(
      cases <= q50 ~ 'Below 50%',
      cases > q50 & cases <= q75 ~ 'Within 50-75%',
      cases > q75 & cases <= q90 ~ 'Within 75-90%',
      cases > q90 ~ 'Above 90%'
    ))
  
  
  tmp$validation <- factor(tmp$validation,
                           levels = c('Below 50%', 'Within 50-75%', 'Within 75-90%', 'Above 90%'),
                           labels = c('Below the median,\ntypical','Moderately high,\nfairly typical','Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
  return(tmp)
}

t1map <- df4maps(obj = df.prob.22_23, ano = 2022)
t1map

t1shape <- shape |> 
  left_join(t1map, by = c('code_macro' = 'macrocode'))

ufshape2 <- ufshape %>%
  mutate(my_nudge_y=ifelse(abbrev_state=='DF',0.5,0))

gt1map <- ggplot() +
  geom_sf(data = t1shape, aes(fill = validation), alpha = 0.7, linewidth = 0.1,  color = 'grey95') +
  scale_fill_manual(values = rev(pal2), name = 'Observed cases compared with probabilistic epidemic bands') +
  geom_sf(data = ufshape, fill = NA, linewidth = 0.5) +
  geom_sf_label(data = ufshape, aes(label = abbrev_state), label.size  = NA, alpha = 0.5, size = 2) +
  labs(title = '2022-2023') +
  theme_void() + 
  theme(legend.position = 'bottom', 
        legend.key.spacing.x = unit(20, 'pt'),
        legend.title.position = 'top', legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = 'bold')) 


t2map <- df4maps(obj = df.prob.23_24, ano = 2023)
t2map

t2shape <- shape |> 
  left_join(t2map, by = c('code_macro' = 'macrocode'))

gt2map <- ggplot() +
  geom_sf(data = t2shape, aes(fill = validation), alpha = 0.7, linewidth = 0.1, color = 'grey95') +
  scale_fill_manual(values = rev(pal2), name = 'Observed cases compared with probabilistic epidemic bands') +
  geom_sf(data = ufshape, fill = NA, linewidth = 0.5) +
  geom_sf_label(data = ufshape, aes(label = abbrev_state), label.size  = NA, alpha = 0.5, size = 2) +
  labs(title = '2023-2024') +
  theme_void() + 
  theme(legend.position = 'bottom', 
        legend.key.spacing.x = unit(20, 'pt'),
        legend.title.position = 'top', legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = 'bold')) 
gt2map

ggarrange(gt1map, gt2map, common.legend = TRUE, legend = 'bottom', labels = 'AUTO')


# Fig 4 -------------------------------------------------------------------

t3br <- df4BRplot(df.prob.24_25, ano = 2024)
t3uf <- df4UFplot(df.prob.24_25, ano = 2024)

t3br <- t3br |> 
  mutate(cases = if_else(date > ymd('2025-03-30'), true = NA, false = cases))
t3uf <- t3uf |> 
  mutate(cases = if_else(uf == 'ES' & is.na(cases), true = 0, false = cases),
         cases = if_else(date > ymd('2025-03-30'), true = NA, false = cases))


gt3br <- ggplot(t3br, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic bands') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
  scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    title = '2024-2025\nBrazil',
    x = "",
    y = "Number of cases"
  ) + 
  theme_legend_right +
  theme(legend.position = 'bottom', legend.title.position = 'top', legend.title = element_text(hjust = .5)) +
  guides_2legends +
  theme(plot.margin = margin(t = 1.5, r = 1.5, l = .5, b = 1.5, unit = 'cm'), 
        legend.margin = margin(t = 1.5, unit = 'cm')) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(linetype = 0)))

gt3br

gt3uf <- ggplot(t3uf, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic bands') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
  scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
  facet_geo(~uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    title = '2024-2025',
    x = "",
    y = ""
  ) + 
  theme_legend_right +
  guides_2legends +
  theme(strip.text = element_text(face = 'bold'),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5))

ggarrange(gt3br, gt3uf, common.legend = FALSE, 
          labels = "AUTO", widths = c(8,10))


# SM Fig2 -----------------------------------------------------------------

dadosBR <- dados.macro |> 
  group_by(date) |> 
  summarise(cases = sum(cases, na.rm = TRUE)) |> 
  filter(date <= ymd('2025-03-30'))


temporalBR <- dadosBR |> 
  ggplot() +
  geom_line(aes(x = date, y = cases), linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::number_format(big.mark = ",", decimal.mark = '.')) +
  labs(y = 'Number of cases', x = '', title = "Brazil") +
  theme_pubclean() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .5),
        text = element_text(size = 16, family = "Arial"), plot.margin = margin(t = 4, r = 16, l = 4, b = 4))


dadosRegiao <- dados.macro |> 
  mutate(region = str_sub(as.numeric(macroregional_geocode), 1, 1)) |> 
  group_by(date, region) |> 
  summarise(cases = sum(cases, na.rm = TRUE)) |> 
  filter(date <= ymd('2025-03-30'))

dadosRegiao$region <- factor(dadosRegiao$region, levels = c("1", "2", "3", "4", "5"),
                             labels = c("North", "Northeast", "Southeast", "South", "Mid-West"))


temporalR <- dadosRegiao |> 
  ggplot() +
  geom_line(aes(x = date, y = cases, colour = region), linewidth = .7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::number_format(big.mark = ",", decimal.mark = '.')) +
  labs(y = 'Number of cases', x = '') +
  theme_pubclean() +
  theme(legend.position = 'none',
        text = element_text(size = 14,family = 'Arial'), 
        plot.margin = margin(t = 4, r = 16, l = 4, b = 4),
        strip.text = element_text(size = 14)) +
  facet_wrap(.~region, scale = "free_y", ncol = 3)


ggarrange(ggarrange(NULL, temporalBR, NULL, widths = c(3,10,3), nrow = 1), 
          temporalR, nrow = 2)


# SM Fig3-30 --------------------------------------------------------------

nomesuf <- dados.macro |> 
  ungroup() |> 
  select(uf) |> 
  unique() |> 
  mutate(nome = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará",
                  "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão",
                  "Minas Gerais", "Mato Grosso do Sul", "Mato Grosso",
                  "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná",
                  "Rio de Janeiro", "Rio Grande do Norte", "Rondônia",
                  "Roraima", "Rio Grande do Sul", "Santa Catarina",
                  "Sergipe", "São Paulo", "Tocantins"),
         label = str_c(nome, " (", uf, ")"))

tuf <- bind_rows(t1uf, t2uf, t3uf |> 
                   mutate(year.s.first = 2024,
                          season = '2024-2025')) |> 
  left_join(nomesuf)


gera_plots_uf <- function(UF) {
  
  tmp <- bind_rows(t1uf, t2uf, t3uf |> mutate(year.s.first = 2024)) |> 
    filter(uf == UF)
  
  max_valor <- max(tmp$maxvalues[is.finite(tmp$maxvalues)], tmp$cases, na.rm = TRUE)
  max_round <- ceiling(max_valor / 50) * 50
  
  g1 <- ggplot(tmp |> filter(year.s.first == '2022'), aes(x = date, fill = quantile)) +
    geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
    scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
    geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
    scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
    scale_y_continuous(labels = scales::comma, limits = c(0,max_round)) +
    theme_bw(base_size = 18) +
    labs(
      title = '2022-2023',
      x = "",
      y = "Number of cases"
    ) + 
    theme_legend_right +
    guides_2legends +
    theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.6, l = 0.2, unit = 'cm'),
          axis.text.x = element_text(angle = 45, vjust = .5)) 
  
  g2 <- ggplot(tmp |> filter(year.s.first == '2023'), aes(x = date, fill = quantile)) +
    geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
    scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
    geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
    scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
    scale_y_continuous(labels = scales::comma, limits = c(0,max_round)) +
    theme_bw(base_size = 18) +
    labs(
      title = '2023-2024',
      x = "",
      y = ""
    ) + 
    theme_legend_right +
    theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.5, l = 0.2, unit = 'cm'),
          axis.text.x = element_text(angle = 45, vjust = .5)) +
    guides_2legends
  
  g3 <- ggplot(tmp |> filter(year.s.first == '2024'), aes(x = date, fill = quantile)) +
    geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
    scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic bands') +
    geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
    scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
    scale_y_continuous(labels = scales::comma, limits = c(0,max_round)) +
    theme_bw(base_size = 18) +
    labs(
      title = '2024-2025',
      x = "",
      y = ""
    ) + 
    theme_legend_right +
    guides_2legends +
    theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.5, l = 0.2, unit = 'cm'),
          axis.text.x = element_text(angle = 45, vjust = .5))
  
  
  fig <- ggarrange(g1, g2, g3, nrow = 1,
                   common.legend = TRUE, legend = 'bottom', labels = "AUTO")
  
  annotate_figure(fig, top = text_grob(unique(tmp$label), 
                                       face = "bold", size = 18))

}


vufs <- nomesuf$uf  

walk2(vufs, 3:29, function(uf, num) {
  p <- gera_plots_uf(uf)  
  
  ggsave(
    filename = sprintf("SM_Fig%02d.png", num),
    path = '~/ownCloud/Baseline_forecasts/Papers/Epidemic threshold/IDM/v2/SM/',
    plot = p,
    width = 12, height = 4.5, scale = 1.2,
    dpi = 300, 
    bg = 'white'
  )
})



# Control chart -----------------------------------------------------------

# Brazil: 

df4diagram <- function(obj, ano, target) {
  
  tmp1 <- obj |> 
    mutate(year.s.first = as.numeric(str_sub(year, 1, 4))) |> 
    filter(year.s.first >= ano - 5 & year.s.first <= ano - 1) |> 
    group_by(week, year, year.s.first, date) |> 
    summarise(cases = sum(cases, na.rm = TRUE)) |> 
    ungroup() |> 
    group_by(week) |> 
    mutate(mediana = median(cases), 
           Q1 = quantile(cases, .25), 
           Q3 = quantile(cases, .75),
           season = target) |> 
    select(season, week, mediana, Q1, Q3) |> 
    unique() 
  
  tmp2 <- observed |> 
    filter(year.s.first == ano) |> 
    group_by(week, season, date, year.s.first) |> 
    summarise(cases = sum(cases, na.rm = TRUE))
  
  tmp1 |> left_join(tmp2)
  
}


# 2022-2023

dc1br <- df4diagram(obj = dados.macro, ano = 2022, target = '2022-2023')

gdc1br <- ggplot(dc1br, aes(x = date)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = 'grey60'), alpha = 0.7) + 
  scale_fill_manual(values = 'grey60', name = '', labels = 'Endemic channel') +
  geom_line(aes(y = cases, color = 'Observed cases'), linetype = 'solid', linewidth = 0.7, show.legend = T) +
  geom_line(aes(y = mediana, color = 'Median'),  linetype = 'dashed', linewidth = .7, show.legend = T) +
  scale_color_manual(values = c('Observed cases' = 'black', 'Median' = 'blue'), name = '') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "Number of cases"
  ) + 
  theme_legend_right +
  guides_legends_chart 

# 2023-2024

dc2br <- df4diagram(obj = dados.macro, ano = 2023, target = '2023-2024')

gdc2br <- ggplot(dc2br, aes(x = date)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = 'grey60'), alpha = 0.7) + 
  scale_fill_manual(values = 'grey60', name = '', labels = 'Endemic channel') +
  geom_line(aes(y = cases, color = 'Observed cases'), linetype = 'solid', linewidth = 0.7, show.legend = T) +
  geom_line(aes(y = mediana, color = 'Median'),  linetype = 'dashed', linewidth = .7, show.legend = T) +
  scale_color_manual(values = c('Observed cases' = 'black', 'Median' = 'blue'), name = '') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "Number of cases"
  ) + 
  theme_legend_right +
  guides_legends_chart 

# 2024-2025

dc3br <- df4diagram(obj = dados.macro, ano = 2024, target = '2024-2025') |> 
  select(-date) |> 
  left_join(t3br |> select(week, date) |> unique(), by = 'week') |> 
  mutate(cases = if_else(date > ymd('2025-03-30'), true = NA, false = cases))

gdc3br <- ggplot(dc3br, aes(x = date)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = 'grey60'), alpha = 0.7) + 
  scale_fill_manual(values = 'grey60', name = '', labels = 'Endemic channel') +
  geom_line(aes(y = cases, color = 'Observed cases'), linetype = 'solid', linewidth = 0.7, show.legend = T) +
  geom_line(aes(y = mediana, color = 'Median'),  linetype = 'dashed', linewidth = .7, show.legend = T) +
  scale_color_manual(values = c('Observed cases' = 'black', 'Median' = 'blue'), name = '') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 16) +
  labs(
    x = "",
    y = "Number of cases"
  ) + 
  theme_legend_right +
  guides_legends_chart 



## Comparing with bands ---------------------------------------------------

# To compare, we will consider a warning signal when the number of observed cases is:
## - higher than the upper limit of the endemic channel (75% percentile of the distribution of cases 
## in the previous 5 years) for the control chart
## - higher than the "Moderately high, fairly typical" band (75% percentile of the samples
## distribution) for the probabilistic epidemic bands

# 2022-2023

alertas1br <- t1br |> 
  filter(quantile == 'Moderately high,\nfairly typical') |> 
  mutate(alerta = if_else(cases>maxvalues, true = 1, false = 0)) |> 
  select(date, alerta) |> 
  mutate(method = 'Probabilistic epidemic\nbands') |> 
  bind_rows(dc1br |> 
              mutate(alerta = if_else(cases>Q3, true = 1, false = 0)) |>
              ungroup() |> 
              select(date, alerta) |> 
              mutate(method = 'Control chart')) 

galerta1br <- ggplot(alertas1br, aes(x = date, y = alerta, group = method)) +
  geom_point(aes(color = method)) +
  geom_line(aes(color = method)) +
  scale_y_continuous(breaks = c(0,1), name = 'Warning', labels = c('No', 'Yes')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0), name ='') +
  scale_color_manual(values = c('#2F5CD7', '#ef8a47'),  name = '') +
  theme_bw(base_size = 16) +
  theme_legend_right +
  theme(legend.title = element_blank())


g22 <- ggarrange(gt1br + theme(axis.text.x = element_text(angle = 0),
                        axis.title.y = element_text(size = 16)),
          gdc1br,
          galerta1br,
          nrow = 3, 
          heights = c(7,7,3),
          legend = 'none',
          align = 'hv')


# 2023-2024

alertas2br <- t2br |> 
  filter(quantile == 'Moderately high,\nfairly typical') |> 
  mutate(alerta = if_else(cases>maxvalues, true = 1, false = 0)) |> 
  select(date, alerta) |> 
  mutate(method = 'Probabilistic bands') |> 
  bind_rows(dc2br |> 
              mutate(alerta = if_else(cases>Q3, true = 1, false = 0)) |>
              ungroup() |> 
              select(date, alerta) |> 
              mutate(method = 'Control chart')) 

galerta2br <- ggplot(alertas2br, aes(x = date, y = alerta, group = method)) +
  geom_point(aes(color = method)) +
  geom_line(aes(color = method)) +
  scale_y_continuous(breaks = c(0,1), name = '', labels = c('No', 'Yes')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0), name ='') +
  scale_color_manual(values = c('#2F5CD7', '#ef8a47'),  name = '') +
  theme_bw(base_size = 16) +
  theme_legend_right +
  theme(legend.title = element_blank())

g23 <- ggarrange(gt2br + 
                   theme(axis.text.x = element_text(angle = 0),
                         axis.title.y = element_text(size = 16)),
                 gdc2br + labs(y = ''),
                 galerta2br,
                 nrow = 3, 
                 heights = c(7,7,3),
                 align = 'hv', legend = 'none')


# 2024-2025

alertas3br <- t3br |> 
  filter(quantile == 'Moderately high,\nfairly typical') |> 
  mutate(alerta = if_else(cases>maxvalues, true = 1, false = 0)) |> 
  select(date, alerta) |> 
  mutate(method = 'Probabilistic\nepidemic bands') |> 
  bind_rows(dc3br |> 
              mutate(alerta = if_else(cases>Q3, true = 1, false = 0)) |>
              ungroup() |> 
              select(date, alerta) |> 
              mutate(method = 'Control chart')) 

galerta3br <- ggplot(alertas3br, aes(x = date, y = alerta, group = method)) +
  geom_point(aes(color = method)) +
  geom_line(aes(color = method)) +
  scale_y_continuous(breaks = c(0,1), name = '', labels = c('No', 'Yes')) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0), name ='') +
  scale_color_manual(values = c('#2F5CD7', '#ef8a47'),  name = '') +
  theme_bw(base_size = 16) +
  theme_legend_right +
  theme(legend.title = element_blank())

g24 <- ggarrange(gt3br +
            labs(title = '2024-2025', y = '') +
            theme_legend_right +
            theme(axis.text.x = element_text(angle = 0),
                  axis.title.y = element_text(size = 16)) +
            guides_2legends ,
          gdc3br + labs(y = ''),
          galerta3br,
          nrow = 3, 
          heights = c(7,7,3),
          align = 'hv')


ggarrange(g22, g23, g24, 
          ncol = 3,
          widths = c(5,5,7.25),
          align = 'hv')


# Date of first "warning":

t2br |> 
  filter(quantile == 'Moderately high,\nfairly typical') |> 
  mutate(alerta = if_else(cases>maxvalues, true = 1, false = 0)) |> 
  filter(alerta == 1) |> 
  arrange(date)

dc2br |> 
  mutate(alerta = if_else(cases>Q3, true = 1, false = 0)) |> 
  filter(alerta == 1) |> 
  arrange(date)


## Comparing peaks -------------------------------------------------------

t1br |> 
  group_by(quantile) |> 
  filter(maxvalues != 'Inf', maxvalues == max(maxvalues)) |> 
  select(week, quantile, maxvalues)
dc1br |> 
  arrange(desc(mediana))
dc1br |> 
  arrange(desc(cases))

t2br |> 
  group_by(quantile) |> 
  filter(maxvalues != 'Inf', maxvalues == max(maxvalues)) |> 
  select(week, quantile, maxvalues)
dc2br |> 
  arrange(desc(mediana))
dc2br |> 
  arrange(desc(cases))

t3br |> 
  group_by(quantile) |> 
  filter(maxvalues != 'Inf', maxvalues == max(maxvalues)) |> 
  select(week, quantile, maxvalues)
dc3br |> 
  arrange(desc(mediana))



# Plots for dashboard -----------------------------------------------------

# Health districts (HD) = macroregional

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

t1hd <- df4HDplot(df.prob.22_23, ano = 2022)
t2hd <- df4HDplot(df.prob.23_24, ano = 2023)
t3hd <- df4HDplot(df.prob.24_25, ano = 2024)

thd <- bind_rows(t1hd, t2hd, 
                 t3hd |> 
                   mutate(year.s.first = 2024,
                          season = '2024-2025') |>
                   select(-macroregional) |> 
                   left_join(dados.macro |> 
                               ungroup() |> 
                               select(macroregional, macroregional_geocode) |> 
                               unique(), 
                             by = c('macrocode' = 'macroregional_geocode'))) |> 
  left_join(nomesuf) |> 
  rename(UF_label = label) |> 
  mutate(label = str_c(macroregional, " (", uf, ")"))

tbr <- bind_rows(t1br, t2br, 
                 t3br |> 
                   mutate(year.s.first = 2024,
                          season = '2024-2025')) |> 
  mutate(label = 'Brazil')

gera_plots <- function(obj, local, ano) {
  
  if(is.numeric(local)){
  tmp <- obj |> 
    filter(macrocode == local,
           year.s.first == ano)
  }
  if(is.character(local)){
    if(local != 'BR'){
    tmp <- obj |> 
      filter(uf == local,
             year.s.first == ano)
    }
    else{
      tmp <- obj |> 
        filter(year.s.first == ano)
    }
  }
 
  if(nrow(tmp)==0){stop('Empty dataset, fix local.')}
  
  max_valor <- max(tmp$maxvalues[is.finite(tmp$maxvalues)], tmp$cases, na.rm = TRUE)
  max_round <- ceiling(max_valor / 50) * 50
  
  gtmp <- ggplot(tmp, aes(x = date, fill = quantile)) +
    geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
    scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
    geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
    scale_color_manual(values = 'black', name = '', labels = 'Observed cases') + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
    scale_y_continuous(labels = scales::comma, limits = c(0,max_round)) +
    theme_bw(base_size = 18) +
    theme_legend_right +
    guides_2legends +
    theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.6, l = 0.2, unit = 'cm'),
          axis.text.x = element_text(angle = 45, vjust = .5),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 16, hjust = .5)) 
  
  g1 <- gtmp +
    labs(
      title = unique(tmp$label),
      subtitle = unique(tmp$season),
      x = "",
      y = "Number of cases"
    ) 
  return(g1)
  
}

gera_plots(thd, local = 1701, 2024)
gera_plots(tuf, local = 'TO', 2024)
gera_plots(tbr, local = 'BR', 2024)



# Health districts map ----------------------------------------------------

library(leaflet)

shape <- read_sf('data/shapefile_macro.json') |> 
  left_join(spatial.tbl |> 
              select(uf, macroregional, macroregional_geocode) |> 
              unique() |> 
              rename(uf_name = uf), by = c('code_macro' = 'macroregional_geocode'))

ufshape <- read_state(
  year = 2020, 
  showProgress = FALSE
)
ufshape <- ufshape |> 
  st_crop(xmin = -73.9904, ymin = -33.7511, xmax = -33.2476, ymax = 5.2718) 

ufshape <- st_transform(ufshape, st_crs(shape))

regions_en <- tibble(
  name_region = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro Oeste'),
  region_en = c("North", "Northeast", "Southeast", "South", "Mid-West")
)
ufshape <- ufshape |> 
  left_join(regions_en, by = "name_region") |> 
  mutate(name_state = if_else(name_state == 'Amazônas', true = 'Amazonas', false = name_state))

shape <- shape |> 
  left_join(ufshape |> 
              st_drop_geometry() |> 
              select(code_state, region_en), by = c('uf' = 'code_state')) 

# Paleta de cores semelhante à do seu mapa
region_colors <- c(
  "North" = "#3CAACF",
  "Northeast" = "#D4EC88",
  "Mid-West" = "#66C07E",
  "Southeast" = "#A383D9",
  "South" = "#DC8E4B"
)

pal <- colorFactor(palette = region_colors, domain = shape$region_en)

uf_labels <- ufshape %>%
  mutate(centroid = st_centroid(geom)) %>%
  st_drop_geometry() %>%
  bind_cols(st_coordinates(st_centroid(ufshape))) %>%
  select(name_state, X, Y)  

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = shape,
              color = "white",
              weight = 1,
              fillColor = ~pal(region_en),
              fillOpacity = 0.7,
              label = ~paste("Health district:", macroregional, "/", uf_name)) %>%
  addPolygons(data = ufshape,
              fill = FALSE,
              color = "black",
              weight = 2) %>%
  addLabelOnlyMarkers(data = uf_labels,
                      lng = ~X,
                      lat = ~Y,
                      label = ~name_state,
                      labelOptions = labelOptions(
                        noHide = TRUE,
                        direction = "center",
                        textOnly = TRUE,
                        style = list(
                          "color" = "black",
                          "font-size" = "12px",
                          "font-weight" = "bold"
                        )
                      ))

