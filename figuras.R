# =============================================================================
# Comparacion de previsiones con datos observados
# DNPRMF
#
# Fabian Villarreal Sosa
# =============================================================================

# Librerias
pacman::p_load(tidyverse, readxl, openxlsx, dplyr, ggplot2, ggfortify, foreign, 
               timeDate, forecast, xts, urca, tseries, lubridate, stringi, 
               stringr, reshape2, expsmooth, seasonal, Metrics, highcharter)

# Colores
paleta <- c('#1E4976', '#5FA3BF', '#BBBE64', '#EFCB68', '#E5825E', '#D05022')

# Directorio
setwd('D:/DNPRMF/Programacion Sector Real/Previsiones/Comparacion')

# Datos
load('Resultados/dfs.RData')

# Opciones para hchart
hcoptslang <- getOption('highcharter.lang')
hcoptslang$decimalPoint <- ','
hcoptslang$thousandsSep <- '.'
options(highcharter.lang = hcoptslang)

dataLabels = list(enabled = TRUE, 
                 format = '{y:.1f}')

### Componentes de Equilibrio Oferta-Utilizacion

fig_com <- list() 
attach(df_com)

for (com in unique(componente)) {
  fig_com[[com]] <- 
    highchart() %>% 
    hc_add_series(df_com[componente == com & clase == 'Cierre',],
                  hcaes(x = anio, y = valor),
                  name = 'Cierre',
                  type = 'column',
                  color = paleta[1],
                  dataLabels = list(enabled = TRUE, 
                                    format = '{y:.1f}',
                                    style = list(fontSize = 9)),
                  borderColor = 'transparent') %>% 
    hc_add_series(df_com[componente == com & clase != 'Cierre',],
                  hcaes(x = anio, y = valor, group = clase),
                  type = 'scatter',
                  color = paleta[2:length(unique(clase))],
                  marker = list(symbol = 'diamond',
                                radius = 5)) %>% 
    hc_title(text = com,
             style = list(fontSize = '12px', 
                          fontWeight = 'bold')) %>%
    hc_xAxis_multiples(list(
      title = list(text = ''),
      tickWidth = 0,
      lineColor = '#ccd6eb',
      lineWidth = 1,
      labels = list(step = 1),
      tickInterval = 1)) %>% 
    hc_yAxis_multiples(list(
      title = list(text = 'Porcentaje',
                   style = list(fontSize = '11px',
                                fontWeight = 'bold',
                                color = '#333333')),
      plotLines = list(list(value = 0, color = 'grey', width = 1)),
      gridLineColor= '#transparent',
      lineColor = '#ccd6eb',
      lineWidth = 1)) %>% 
    hc_legend(itemStyle = list(fontSize = '11px')) %>% 
    hc_tooltip(pointFormat = "AC1o: {point.x} <br> Tasa: {point.y:.1f}") %>% 
    hc_size(width = 750, height = 400)
    
}

detach(df_com)


### Industria

fig_ind <- list() 
attach(df_ind)

for (ind in unique(industria)) {
  fig_ind[[ind]] <- 
    highchart() %>% 
    hc_add_series(df_ind[industria == ind & clase == 'Cierre',],
                  hcaes(x = anio, y = valor),
                  name = 'Cierre',
                  type = 'column',
                  color = paleta[1],
                  dataLabels = list(enabled = TRUE, 
                                    format = '{y:.1f}',
                                    style = list(fontSize = 9)),
                  borderColor = 'transparent') %>% 
    hc_add_series(df_ind[industria == ind & clase != 'Cierre',],
                  hcaes(x = anio, y = valor, group = clase),
                  type = 'scatter',
                  color = paleta[2:length(unique(clase))],
                  marker = list(symbol = 'diamond',
                                radius = 5)) %>% 
    hc_title(text = ind,
             style = list(fontSize = '12px', 
                          fontWeight = 'bold')) %>%
    hc_xAxis_multiples(list(
      title = list(text = ''),
      tickWidth = 0,
      lineColor = '#ccd6eb',
      lineWidth = 1,
      tickInterval = 1)) %>% 
    hc_yAxis_multiples(list(
      title = list(text = 'Porcentaje',
                   style = list(fontSize = '11px',
                                fontWeight = 'bold',
                                color = '#333333')),
      plotLines = list(list(value = 0, color = 'grey', width = 1)),
      gridLineColor= '#transparent',
      lineColor = '#ccd6eb',
      lineWidth = 1)) %>% 
    hc_legend(itemStyle = list(fontSize = '11px')) %>% 
    hc_tooltip(pointFormat = "AC1o: {point.x} <br> Tasa: {point.y:.1f}") %>% 
    hc_size(width = 750, height = 400)
  
}

detach(df_ind)


### EMOE

fig_emoe <- list() 
attach(df_emoe)

for (ind in unique(industria)) {
  fig_emoe[[ind]] <- 
    highchart() %>% 
    hc_add_series(df_emoe[industria == ind & clase == 'Cierre',],
                  hcaes(x = anio, y = valor),
                  name = 'Cierre',
                  type = 'column',
                  color = paleta[1],
                  dataLabels = list(enabled = TRUE, 
                                    format = '{y:.1f}',
                                    style = list(fontSize = 9)),
                  borderColor = 'transparent') %>% 
    hc_add_series(df_emoe[industria == ind & clase != 'Cierre',],
                  hcaes(x = anio, y = valor, group = clase),
                  type = 'scatter',
                  color = paleta[2:length(unique(clase))],
                  marker = list(symbol = 'diamond',
                                radius = 5)) %>% 
    hc_title(text = ind,
             style = list(fontSize = '12px', 
                          fontWeight = 'bold')) %>%
    hc_xAxis_multiples(list(
      title = list(text = ''),
      tickWidth = 0,
      lineColor = '#ccd6eb',
      lineWidth = 1,
      tickInterval = 1)) %>% 
    hc_yAxis_multiples(list(
      title = list(text = 'Porcentaje',
                   style = list(fontSize = '11px',
                                fontWeight = 'bold',
                                color = '#333333')),
      plotLines = list(list(value = 0, color = 'grey', width = 1)),
      gridLineColor= '#transparent',
      lineColor = '#ccd6eb',
      lineWidth = 1)) %>% 
    hc_legend(itemStyle = list(fontSize = '11px')) %>% 
    hc_tooltip(pointFormat = "AC1o: {point.x} <br> Tasa: {point.y:.1f}") %>% 
    hc_size(width = 750, height = 400)
  
}

detach(df_emoe)

save(fig_com,  file = 'Resultados/fig_com.RData')
save(fig_ind,  file = 'Resultados/fig_ind.RData')
save(fig_emoe, file = 'Resultados/fig_emoe.RData')
