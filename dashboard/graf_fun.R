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
      hovertemplate = paste0("<b>", quants[i],"</b>: %{y:,.0f}<extra></extra>")
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
      hovertemplate = paste0("<b>", quants[i],"</b>: %{y:,.0f}<extra></extra>")
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


# Plotly for HD -------------------------------------------------------------------------------

plot_grafico_artigo_macro_por_uf <- function(df, UF, palette) {

    df_uf <- df |> filter(uf == UF)
  regs <- sort(unique(df_uf$macroregional))
  n_regs <- length(regs)
  if(n_regs == 0) stop("Nenhuma macrorregião encontrada para ", UF)
  
  plots <- vector("list", n_regs)
  for (j in seq_along(regs)) {
    reg_j <- regs[j]
    df_reg <- df_uf |> filter(macroregional == reg_j)
    
    quants <- rev(unique(df_reg$quantile))
    colors <- palette[seq_along(quants)]
    
    max_valor <- df_reg |>
      filter(is.finite(maxvalues)) |>
      summarise(m = max(maxvalues, cases, na.rm = TRUE)) |>
      pull(m)
    df_max_rnd2 <- ceiling(max_valor / 50) * 50 * 1.1
    
    df2 <- df_reg |>
      mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_rnd2, maxvalues))
    
    fig_i <- plot_ly()
    for (i in seq_along(quants)) {
      fig_i <- fig_i |> add_trace(
        data = df2 |> filter(quantile == quants[i]),
        type = 'scatter',
        mode = 'lines',
        x = ~date,
        y = ~maxvalues,
        fill = 'tozeroy',
        line = list(color = colors[i]),
        fillcolor = colors[i],
        name = quants[i],
        hovertemplate = paste0("<b>", quants[i],"</b>: %{y:,.0f}<extra></extra>")
      )
    }
    fig_i <- fig_i |>
      add_trace(
        data = df2,
        type = 'scatter',
        mode = 'lines',
        x = ~date,
        y = ~cases,
        line = list(color = "black"),
        name = "Cases",
        hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
      ) |>
      layout(
        margin = list(t = 20, b = 20, l = 20, r = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, title = NULL),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, title = NULL),
        showlegend = FALSE
      )
    
    plots[[j]] <- fig_i
  }
  
  ncol <- ceiling(sqrt(n_regs))
  nrow <- ceiling(n_regs / ncol)
  
  p <- subplot(
    plotlist = plots,
    nrows = nrow,
    margin = 0.02,
    shareX = TRUE,
    # shareY = TRUE,
    titleX = FALSE,
    titleY = FALSE
  )
  
  title_hd <- vector("list", n_regs)
  for (i in seq_along(regs)) {
    coli <- (i-1) %% ncol
    rowi <- floor((i-1) / ncol)
    x0 <- coli / ncol
    x1 <- (coli+1) / ncol
    xc <- (x0 + x1) / 2
    y1 <- 1 - rowi / nrow
    # texto logo acima do topo da célula
    title_hd[[i]] <- list(
      x = xc, 
      y = y1 - 0.01,
      xref = "paper", 
      yref = "paper",
      text = regs[i],
      showarrow= FALSE,
      xanchor = "center",
      yanchor = "bottom",
      font = list(size = 12)
    )
  }
  
  p |> layout(
    hovermode = "x unified",
    annotations = title_hd
  )
}
