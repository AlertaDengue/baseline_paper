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
  guides_2legends

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
  theme(plot.margin = margin(t = 0.2, b = 0.2, r = 0.5, l = 0.2, unit = 'cm')) +
  guides_2legends

ggarrange(gt1br, gt2br, common.legend = TRUE, legend = 'right', labels = "AUTO")


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
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
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
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5))

gt2uf <- ggplot(t2uf, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic band') +
  geom_line(aes(y = cases, color = 'black'), linewidth = 0.5, show.legend = T) +
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
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5))

ggarrange(gt1uf, gt2uf, nrow = 1, common.legend = TRUE, legend = 'right', labels = 'AUTO')


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

gt3br <- ggplot(t3br, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic bands') +
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
  guides(fill = guide_legend(ncol = 2)) 

gt3br

gt3uf <- ggplot(t3uf, aes(x = date, fill = quantile)) +
  geom_ribbon(aes(ymin = minvalues, ymax = maxvalues), alpha = 0.7) + 
  scale_fill_manual(values = rev(pal2), name = 'Probabilistic epidemic bands') +
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
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5))


ggarrange(gt3br, gt3uf, common.legend = FALSE, 
          labels = "AUTO", widths = c(8,10))



# SM Fig2 -----------------------------------------------------------------

dadosBR <- dados.macro |> 
  group_by(date) |> 
  summarise(cases = sum(cases, na.rm = TRUE)) |> 
  filter(year(date)<2025) 


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
  filter(year(date)<2025) 

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
        text = element_text(size = 14,family = 'Arial'), plot.margin = margin(t = 4, r = 16, l = 4, b = 4)) +
  facet_wrap(.~region, scale = "free_y", ncol = 3)


ggarrange(ggarrange(NULL, temporalBR, NULL, widths = c(3,10,3), nrow = 1), 
          temporalR, nrow = 2)
