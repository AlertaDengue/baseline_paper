# 
# denv <- list()
# 
# denv[[1]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR15.csv.zip")
# denv[[2]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR16.csv.zip")
# denv[[3]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR17.csv.zip")
# denv[[4]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR18.csv.zip")
# denv[[5]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR19.csv.zip")
# denv[[6]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR20.csv.zip")
# denv[[7]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR21.csv.zip")
# denv[[8]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR22.csv.zip")
# denv[[9]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR23.csv.zip")
# denv[[10]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR24.csv.zip")
# denv[[11]] <- vroom::vroom(file = "~/Data/Sinan/Dengue/DENGBR25.csv.zip")
# 
# df.sinan <- lapply(
#   denv, 
#   FUN = function(x){
#     x |> select(ID_MN_RESI, DT_SIN_PRI, SEM_PRI, CLASSI_FIN)
#   }
#   ) |> bind_rows() 
# 
# 
# df.sinan <- df.sinan |> 
#   filter(DT_SIN_PRI >= "2015-10-11", DT_SIN_PRI < "2025-12-31") |> #nrow()
#   drop_na(ID_MN_RESI) |> 
#   mutate(
#     DT_SIN_PRI.sun = DT_SIN_PRI - as.numeric(format(DT_SIN_PRI, "%w"))
#     ) 
# 
# df3 <- df.sinan |> 
#   filter(CLASSI_FIN != 5 | is.na(CLASSI_FIN)) |> 
#   group_by(data_iniSE = DT_SIN_PRI.sun, ID_MN_RESI) |> 
#   summarise(
#     casos_prov = n()
#   )
#   
# 
# 
# saveRDS(object = df, file = "~/Data/df.sinan.rds")
