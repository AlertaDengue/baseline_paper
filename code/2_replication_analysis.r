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

# Dados por macroregional - casos supeitos
dados.macro <- dengue.df |> prepare.data(suspected_cases = T)

# Dados por macroregional - casos provaveis
dados.prov.macro <- dengue.df |> prepare.data(suspected_cases = F)


df.prob.22_23 <- read_csv(file = "samples/macro.prob.22_23.csv.gz")
df.susp.22_23 <- read_csv(file = "samples/macro.susp.22_23.csv.gz")

df.prob.23_24 <- read_csv(file = "samples/macro.prob.23_24.csv.gz")
df.susp.23_24 <- read_csv(file = "samples/macro.susp.23_24.csv.gz")

df.prob.24_25 <- read_csv(file = "samples/macro.prob.24_25.csv.gz")
df.susp.24_25 <- read_csv(file = "samples/macro.susp.24_25.csv.gz")

# save.image(file =  "~/Desktop/baseline.RData")


# Brazil ------------------------------------------------------------------


BR.probable = df.prob.22_23 |> 
  group_by(week, samples) |> 
  summarise(cases = sum(values)) |> 
  group_by(week) |> 
  summarise(
    P25 = quantile(cases, probs = 0.25),
    P50 = quantile(cases, probs = 0.5),
    P75 = quantile(cases, probs = 0.75),
    P90 = quantile(cases, probs = 0.9),
    P95 = quantile(cases, probs = 0.95)
  ) |> add_column(season = "2022-2023", .before = "week" ) |> 
  bind_rows(
    df.prov.23_24 |> 
      group_by(week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2023-2024", .before = "week" )
  ) |> 
  bind_rows(
    df.prov.24_25 |> 
      group_by(week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2024-2025", .before = "week" )
  ) |> ungroup()


# BR.probable |> write_csv(file = "data/BR.probable.csv")


BR.suspect = df.susp.22_23 |> 
  group_by(week, samples) |> 
  summarise(cases = sum(values)) |> 
  group_by(week) |> 
  summarise(
    P25 = quantile(cases, probs = 0.25),
    P50 = quantile(cases, probs = 0.5),
    P75 = quantile(cases, probs = 0.75),
    P90 = quantile(cases, probs = 0.9),
    P95 = quantile(cases, probs = 0.95)
  ) |> add_column(season = "2022-2023", .before = "week" ) |> 
  bind_rows(
    df.susp.23_24 |> 
      group_by(week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2023-2024", .before = "week" )
  ) |> 
  bind_rows(
    df.susp.24_25 |> 
      group_by(week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2024-2025", .before = "week" )
  ) |> ungroup()


# BR.suspect |> write_csv(file = "data/BR.suspect.csv")


# UF ------------------------------------------------------------------


UF.probable = df.prov.22_23 |> 
  group_by(uf, week, samples) |> 
  summarise(cases = sum(values)) |> 
  group_by(uf, week) |> 
  summarise(
    P25 = quantile(cases, probs = 0.25),
    P50 = quantile(cases, probs = 0.5),
    P75 = quantile(cases, probs = 0.75),
    P90 = quantile(cases, probs = 0.9),
    P95 = quantile(cases, probs = 0.95)
  ) |> add_column(season = "2022-2023", .before = "week" ) |> 
  bind_rows(
    df.prov.23_24 |> 
      group_by(uf, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(uf, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2023-2024", .before = "week" )
  ) |> 
  bind_rows(
    df.prov.24_25 |> 
      group_by(uf, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(uf, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2024-2025", .before = "week" )
  ) |> ungroup()


# UF.probable |> write_csv(file = "data/UF.probable.csv")


UF.suspect = df.susp.22_23 |> 
  group_by(uf, week, samples) |> 
  summarise(cases = sum(values)) |> 
  group_by(uf, week) |> 
  summarise(
    P25 = quantile(cases, probs = 0.25),
    P50 = quantile(cases, probs = 0.5),
    P75 = quantile(cases, probs = 0.75),
    P90 = quantile(cases, probs = 0.9),
    P95 = quantile(cases, probs = 0.95)
  ) |> add_column(season = "2022-2023", .before = "week" ) |> 
  bind_rows(
    df.susp.23_24 |> 
      group_by(uf, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(uf, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.55),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2023-2024", .before = "week" )
  ) |> 
  bind_rows(
    df.susp.24_25 |> 
      group_by(uf, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(uf, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2024-2025", .before = "week" )
  ) |> ungroup()


UF.suspect |> write_csv(file = "data/UF.suspect.csv")


# Macro ------------------------------------------------------------------


Macro.probable = df.prov.22_23 |> 
  group_by(macrocode, week, samples) |> 
  summarise(cases = sum(values)) |> 
  group_by(macrocode, week) |> 
  summarise(
    P25 = quantile(cases, probs = 0.25),
    P50 = quantile(cases, probs = 0.5),
    P75 = quantile(cases, probs = 0.75),
    P90 = quantile(cases, probs = 0.9),
    P95 = quantile(cases, probs = 0.95)
  ) |> add_column(season = "2022-2023", .before = "week" ) |> 
  bind_rows(
    df.prov.23_24 |> 
      group_by(macrocode, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(macrocode, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2023-2024", .before = "week" )
  ) |> 
  bind_rows(
    df.prov.24_25 |> 
      group_by(macrocode, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(macrocode, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2024-2025", .before = "week" )
  ) |> ungroup()


# Macro.probable |> write_csv(file = "data/Macro.probable.csv")


Macro.suspect = df.susp.22_23 |> 
  group_by(macrocode, week, samples) |> 
  summarise(cases = sum(values)) |> 
  group_by(macrocode, week) |> 
  summarise(
    P25 = quantile(cases, probs = 0.25),
    P50 = quantile(cases, probs = 0.5),
    P75 = quantile(cases, probs = 0.75),
    P90 = quantile(cases, probs = 0.9),
    P95 = quantile(cases, probs = 0.95)
  ) |> add_column(season = "2022-2023", .before = "week" ) |> 
  bind_rows(
    df.susp.23_24 |> 
      group_by(macrocode, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(macrocode, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2023-2024", .before = "week" )
  ) |> 
  bind_rows(
    df.susp.24_25 |> 
      group_by(macrocode, week, samples) |> 
      summarise(cases = sum(values)) |> 
      group_by(macrocode, week) |> 
      summarise(
        P25 = quantile(cases, probs = 0.25),
        P50 = quantile(cases, probs = 0.5),
        P75 = quantile(cases, probs = 0.75),
        P90 = quantile(cases, probs = 0.9),
        P95 = quantile(cases, probs = 0.95)
      ) |> add_column(season = "2024-2025", .before = "week" )
  ) |> ungroup()


# Macro.suspect |> write_csv(file = "data/Macro.suspect.csv")



# Totals ------------------------------------------------------------------

BR.totals <- df.susp.22_23 |> 
  group_by(samples) |> 
  summarise(values = sum(values)) |> 
  summarise(
    estimate = quantile(values, probs = 0.5),
    LI = quantile(values, probs = 0.025),
    LS = quantile(values, probs = 0.975),
    P50 = quantile(values, probs = 0.5),
    P75 = quantile(values, probs = 0.75),
    P90 = quantile(values, probs = 0.9),
  ) |> add_column(season = "2022-2023", .before = "estimate") |> 
  bind_rows(
    df.susp.23_24 |> 
      group_by(samples) |> 
      summarise(values = sum(values)) |> 
      summarise(
        estimate = quantile(values, probs = 0.5),
        LI = quantile(values, probs = 0.025),
        LS = quantile(values, probs = 0.975),
        P50 = quantile(values, probs = 0.5),
        P75 = quantile(values, probs = 0.75),
        P90 = quantile(values, probs = 0.9),
      ) |> add_column(season = "2023-2024", .before = "estimate") 
  ) |> 
  bind_rows(
    df.susp.24_25 |> 
      group_by(samples) |> 
      summarise(values = sum(values)) |> 
      summarise(
        estimate = quantile(values, probs = 0.5),
        LI = quantile(values, probs = 0.025),
        LS = quantile(values, probs = 0.975),
        P50 = quantile(values, probs = 0.5),
        P75 = quantile(values, probs = 0.75),
        P90 = quantile(values, probs = 0.9),
      ) |> add_column(season = "2024-2025", .before = "estimate") 
    
  ) |> bind_cols(
    df.prov.22_23 |> 
      group_by(samples) |> 
      summarise(values = sum(values)) |> 
      summarise(
        estimate.prob = quantile(values, probs = 0.5),
        LI.prob = quantile(values, probs = 0.025),
        LS.prob = quantile(values, probs = 0.975),
        P50 = quantile(values, probs = 0.5),
        P75 = quantile(values, probs = 0.75),
        P90 = quantile(values, probs = 0.9),
      ) |> bind_rows(
        df.prov.23_24 |> 
          group_by(samples) |> 
          summarise(values = sum(values)) |> 
          summarise(
            estimate.prob = quantile(values, probs = 0.5),
            LI.prob = quantile(values, probs = 0.025),
            LS.prob = quantile(values, probs = 0.975),
            P50 = quantile(values, probs = 0.5),
            P75 = quantile(values, probs = 0.75),
            P90 = quantile(values, probs = 0.9),
          )
      ) |> 
      bind_rows(
        df.prov.24_25 |> 
          group_by(samples) |> 
          summarise(values = sum(values)) |> 
          summarise(
            estimate.prob = quantile(values, probs = 0.5),
            LI.prob = quantile(values, probs = 0.025),
            LS.prob = quantile(values, probs = 0.975),
            P50 = quantile(values, probs = 0.5),
            P75 = quantile(values, probs = 0.75),
            P90 = quantile(values, probs = 0.9),
          )    
      )
  )

# BR.totals |> write_csv(file = "data/BR.totals.csv")


BR.totals |> 
  ggplot() + 
  geom_pointrange(aes(x = season, y = estimate, ymin = LI, ymax = LS)) + 
  geom_point( data = dengue.df |> 
                filter(data_iniSE >= "2022-10-09") |> 
                mutate(season = season(data_iniSE)) |> 
                group_by(season) |> 
                summarise(cases = sum(casos)),
              mapping = aes(x = season, y = cases), color = "red"
  ) + 
  theme_bw()






UF.totals <- df.susp.22_23 |> 
  group_by(uf, samples) |> 
  summarise(values = sum(values)) |> 
  group_by(uf) |> 
  summarise(
    estimate = quantile(values, probs = 0.5),
    LI = quantile(values, probs = 0.025),
    LS = quantile(values, probs = 0.975),
  ) |> add_column(season = "2022-2023", .before = "estimate") |> 
  bind_rows(
    df.susp.23_24 |> 
      group_by(uf, samples) |> 
      summarise(values = sum(values)) |> 
      group_by(uf) |> 
      summarise(
        estimate = quantile(values, probs = 0.5),
        LI = quantile(values, probs = 0.025),
        LS = quantile(values, probs = 0.975),
      ) |> add_column(season = "2023-2024", .before = "estimate") 
  ) |> 
  bind_rows(
    df.susp.24_25 |> 
      group_by(uf, samples) |> 
      summarise(values = sum(values)) |> 
      group_by(uf) |> 
      summarise(
        estimate = quantile(values, probs = 0.5),
        LI = quantile(values, probs = 0.025),
        LS = quantile(values, probs = 0.975),
      ) |> add_column(season = "2024-2025", .before = "estimate") 
    
  ) |> left_join(
    df.prov.22_23 |> 
      group_by(uf, samples) |> 
      summarise(values = sum(values)) |> 
      group_by(uf) |> 
      summarise(
        estimate.prob = quantile(values, probs = 0.5),
        LI.prob = quantile(values, probs = 0.025),
        LS.prob = quantile(values, probs = 0.975),
      ) |> add_column(season = "2022-2023") |> 
      bind_rows(
        df.prov.23_24 |> 
          group_by(uf, samples) |> 
          summarise(values = sum(values)) |> 
          group_by(uf) |> 
          summarise(
            estimate.prob = quantile(values, probs = 0.5),
            LI.prob = quantile(values, probs = 0.025),
            LS.prob = quantile(values, probs = 0.975)
          ) |> add_column(season = "2023-2024")
      ) |> 
      bind_rows(
        df.prov.24_25 |> 
          group_by(uf, samples) |> 
          summarise(values = sum(values)) |> 
          group_by(uf) |> 
          summarise(
            estimate.prob = quantile(values, probs = 0.5),
            LI.prob = quantile(values, probs = 0.025),
            LS.prob = quantile(values, probs = 0.975),
          ) |> add_column(season = "2024-2025")    
      )
  )

# UF.totals |> write_csv(file = "data/UF.totals.csv")


UF.totals |> 
  ggplot() + 
  geom_pointrange(aes(x = season, y = estimate, ymin = LI, ymax = LS)) + 
  geom_point( data = dengue.df |> 
                drop_na(uf) |> 
                filter(data_iniSE >= "2022-10-09") |> 
                mutate(season = season(data_iniSE)) |> 
                group_by(uf, season) |> 
                summarise(cases = sum(casos)),
              mapping = aes(x = season, y = cases), color = "red"
  ) + 
  theme_bw() + 
  facet_wrap(~uf, scales= "free_y" )


# 
# 
# 
# 
# # save.image(file = "~/Desktop/sprint.RData")
# # saveRDS(df.train.1, file = "~/Desktop/train1.rds")
# # saveRDS(df.train.2, file = "~/Desktop/train2.rds")
# 
# # Training 1
# 
# tbl.total.uf.train1 <- df.train.1 %>%
#   group_by(uf, samples) %>%
#   summarise(
#     values = sum(values)
#   ) %>% group_by(uf) %>%
#   summarise(
#     est = median(values),
#     li = quantile(values, probs = 0.05),
#     lu = quantile(values, probs = 0.95),
#   ) %>% left_join(
#     dengue %>%
#       filter(target_1 == T) %>%
#       group_by(uf) %>%
#       summarise(cases = sum(casos)), by = "uf"
#   ) #%>%
# #   bind_rows(
# #     tibble(uf = "BR") %>% bind_cols(df.train.1 %>% 
# #                                       group_by(samples) %>% 
# #                                       summarise(
# #                                         values = sum(values)
# #                                       ) %>% #group_by(uf) %>% 
# #                                       summarise(
# #                                         est = median(values),
# #                                         li = quantile(values, probs = 0.05),
# #                                         lu = quantile(values, probs = 0.95),
# #                                       ),
# #                                     dengue %>% 
# #                                       filter(target_1 == T) %>% 
# #                                       # group_by(uf) %>% 
# #                                       summarise(cases = sum(casos))
# #     )
# #   )
# 
# 
# tbl.total.uf.week.train1 <- df.train.1 %>% 
#   group_by(uf, week, samples) %>% 
#   summarise(
#     values = sum(values)
#   ) %>% group_by(uf, week) %>% 
#   summarise(
#     est = median(values),
#     li = quantile(values, probs = 0.05),
#     lu = quantile(values, probs = 0.95),
#   ) %>% left_join(
#     dengue %>% 
#       filter(target_1 == T) %>% 
#       mutate(
#         week = week.season(date)
#       ) %>% 
#       group_by(uf, week) %>% 
#       summarise(cases = sum(casos)), by = c("uf", "week")
#   ) %>% 
#   left_join(dt_train1)
# # bind_rows(
# #   tibble(uf = "BR") %>% 
# #     bind_cols(df.train.1 %>% 
# #                 group_by(week, samples) %>% 
# #                 summarise(
# #                   values = sum(values)
# #                 ) %>% group_by(week) %>% 
# #                 summarise(
# #                   est = median(values),
# #                   li = quantile(values, probs = 0.05),
# #                   lu = quantile(values, probs = 0.95),
# #                 ) %>% left_join(
# #                   dengue %>% 
# #                     filter(target_1 == T) %>% 
# #                     mutate(
# #                       week = week.season(date)
# #                     ) %>% 
# #                     group_by(week) %>%
# #                     summarise(cases = sum(casos)), by = "week"
# #                 )
# #     )
# # )
# 
# 
# 
# 
# tbl.total.uf.train2 <- df.train.2 %>%
#   group_by(uf, samples) %>%
#   summarise(
#     values = sum(values)
#   ) %>% group_by(uf) %>%
#   summarise(
#     est = median(values),
#     li = quantile(values, probs = 0.05),
#     lu = quantile(values, probs = 0.95),
#   ) %>% left_join(
#     dengue %>%
#       filter(target_2 == T) %>%
#       group_by(uf) %>%
#       summarise(cases = sum(casos)), by = "uf"
#   ) #%>%
# #   bind_rows(
# #     tibble(uf = "BR") %>% bind_cols(df.train.2 %>% 
# #                                       group_by(samples) %>% 
# #                                       summarise(
# #                                         values = sum(values)
# #                                       ) %>% #group_by(uf) %>% 
# #                                       summarise(
# #                                         est = median(values),
# #                                         li = quantile(values, probs = 0.05),
# #                                         lu = quantile(values, probs = 0.95),
# #                                       ),
# #                                     dengue %>% 
# #                                       filter(target_2 == T) %>% 
# #                                       # group_by(uf) %>% 
# #                                       summarise(cases = sum(casos))
# #     )
# #   )
# 
# 
# tbl.total.uf.week.train2 <- df.train.2 %>% 
#   group_by(uf, week, samples) %>% 
#   summarise(
#     values = sum(values)
#   ) %>% group_by(uf, week) %>% 
#   summarise(
#     est = median(values),
#     li = quantile(values, probs = 0.05),
#     lu = quantile(values, probs = 0.95),
#   ) %>% left_join(
#     dengue %>% 
#       filter(target_2 == T) %>% 
#       mutate(
#         week = week.season(date)
#       ) %>% 
#       group_by(uf, week) %>% 
#       summarise(cases = sum(casos)), by = c("uf", "week")
#   ) %>%  
#   left_join(dt_train2)
# # bind_rows(
# #   tibble(uf = "BR") %>% 
# #     bind_cols(df.train.2 %>% 
# #                 group_by(week, samples) %>% 
# #                 summarise(
# #                   values = sum(values)
# #                 ) %>% group_by(week) %>% 
# #                 summarise(
# #                   est = median(values),
# #                   li = quantile(values, probs = 0.05),
# #                   lu = quantile(values, probs = 0.95),
# #                 ) %>% left_join(
# #                   dengue %>% 
# #                     filter(target_2 == T) %>% 
# #                     mutate(
# #                       week = week.season(date)
# #                     ) %>% 
# #                     group_by(week) %>%
# #                     summarise(cases = sum(casos)), by = "week"
# #                 )
# #     )
# # )
# 
# 
# tbl.total.uf.week.train2 <- df.train.2 %>% 
#   group_by(uf, week, samples) %>% 
#   summarise(
#     values = sum(values)
#   ) %>% group_by(uf, week) %>% 
#   summarise(
#     est = median(values),
#     li = quantile(values, probs = 0.05),
#     lu = quantile(values, probs = 0.95),
#   ) %>% left_join(
#     dengue %>% 
#       filter(target_2 == T) %>% 
#       mutate(
#         week = week.season(date)
#       ) %>% 
#       group_by(uf, week) %>% 
#       summarise(cases = sum(casos)), by = c("uf", "week")
#   ) %>%  
#   left_join(dt_train2)
# 
# 
# # write_csv(tbl.total.uf.train1, file = "~/Desktop/tbl.total.uf.train1.csv")
# # write_csv(tbl.total.uf.train2, file = "~/Desktop/tbl.total.uf.train2.csv")
# write_csv(tbl.total.uf.week.train1, file = "~/Desktop/tbl.total.uf.week.train1.csv")
# write_csv(tbl.total.uf.week.train2, file = "~/Desktop/tbl.total.uf.week.train2.csv")
# 
# 
# tbl.total.uf.train1 %>% 
#   ggplot(aes(y = uf)) + 
#   geom_pointrange(aes(x = est, xmin = li, xmax = lu, color = "Estimates")) +
#   geom_point(aes(x = cases, color = "Observed")) + 
#   scale_x_log10() + 
#   theme_bw()
# 
# tbl.total.uf.train2 %>% 
#   ggplot(aes(y = uf)) + 
#   geom_pointrange(aes(x = est, xmin = li, xmax = lu, color = "Estimates")) +
#   geom_point(aes(x = cases, color = "Observed")) + 
#   scale_x_log10() + 
#   theme_bw()
# 
