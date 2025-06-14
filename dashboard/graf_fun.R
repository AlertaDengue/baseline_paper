# Plotly for Country --------------------------------------------------------------------------

plot_grafico_artigo <- function(df, palette) {
  
  quants <- rev(unique(df$quantile))
  colors <- palette[seq_along(quants)]
  
  max_valor <- max(df$maxvalues[is.finite(df$maxvalues)], df$cases, na.rm = TRUE)
  df_max_round <- ceiling(max_valor / 50) * 50
  df_max_round2 <- df_max_round + (df_max_round * 0.1)
  df2 <- df |> mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_round2, maxvalues))
  
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig |> add_trace(
      data = df2 |> filter(quantile == quants[i]),
      type = 'scatter', 
      mode = 'lines',
      x = ~date, 
      y = ~maxvalues,
      fill = 'tozeroy',
      line = list(color = colors[i]),
      fillcolor = colors[i],
      name = quants[i],
      hovertemplate = paste0("<b>", quants[i],"</b><extra></extra>")
    )
  }
  
  fig |>
    add_trace(
      data = df2,
      type = 'scatter', mode = 'lines',
      x = ~date, y = ~cases,
      line = list(color = "black"),
      name = "Cases",
      hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
    ) |>
    layout(
      hovermode = "x unified",
      xaxis = list(title="Date", showgrid=FALSE, zeroline=FALSE),
      yaxis = list(title="Dengue cases", showgrid=FALSE, zeroline=FALSE),
      legend = list(itemclick=FALSE, itemdoubleclick=FALSE)
    )
}


# Plotly for UF -------------------------------------------------------------------------------

plot_grafico_artigo_uf <- function(df, UF, palette) {
  
  quants <- rev(unique(df$quantile))
  colors <- palette[seq_along(quants)]
  
  max_valor <- df |> filter(uf == UF, is.finite(df$maxvalues)) |> select(maxvalues, cases) |> max()
  df_max_round <- ceiling(max_valor / 50) * 50
  df_max_round2 <- df_max_round + (df_max_round * 0.1)
  df2 <- df |> filter(uf == UF) |> mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_round2, maxvalues))
  
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig |> add_trace(
      data = df2 |> filter(quantile == quants[i]),
      type = 'scatter', 
      mode = 'lines',
      x = ~date, 
      y = ~maxvalues,
      fill = 'tozeroy',
      line = list(color = colors[i]),
      fillcolor = colors[i],
      name = quants[i],
      hovertemplate = paste0("<b>", quants[i],"</b><extra></extra>")
    )
  }
  
  fig |>
    add_trace(
      data = df2,
      type = 'scatter', mode = 'lines',
      x = ~date, y = ~cases,
      line = list(color = "black"),
      name = "Cases",
      hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
    ) |>
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "Dengue cases", showgrid = FALSE, zeroline = FALSE),
      legend = list(itemclick = FALSE, itemdoubleclick = FALSE)
    )
}
