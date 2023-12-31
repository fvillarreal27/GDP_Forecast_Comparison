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

# Archivo
archivo <- 'Datos/Comparacion Crecimiento - Previsiones.xlsx'



### Componentes de Equilibrio Oferta-Utilizacion -----

# Datos 
df <- data.frame(read_excel(archivo,
                            sheet = 'Oferta-Utilizacion', 
                            col_names = FALSE))

colnames(df) <- NULL
df <- df[c(1:12), ]

# Tratamiento
x <- t(df[1, ])
x <- na.locf(x)

df[1, ] <- t(x)
df[1, ] <- paste0(df[1, ], '_', df[2, ])

colnames(df) <- df[1, ]
df <- df[-c(1:2), ]

df <- df %>% 
  pivot_longer(cols = 2:ncol(df),
               names_to = 'clase',
               values_to = 'valor')

df <- df %>% 
  separate(clase, sep = '_', 
           c('anio', 'clase'))

df[, 'valor'] <- as.numeric(unlist(df[, 'valor']))
df[, 'anio'] <- as.numeric(unlist(df[, 'anio']))

names(df)[1] <- 'componente'

df_com <- df

## RMSE (en puntos porcentuales) y numero de sobre/subestimaciones
df_rmse1 <- df[df$clase == 'Cierre',]
df_rmse1$clase <- NULL
df_rmse1 <- df_rmse1[df_rmse1$anio >= 2019,]
names(df_rmse1)[3] <- 'cierre'

df_rmse2 <- df[df$clase != 'Cierre',]
df_rmse2 <- df_rmse2[df_rmse2$anio >= 2019,]

df_rmse <- merge(df_rmse2, df_rmse1,
                 by = c('componente', 'anio'),
                 all = TRUE, 
                 sort = FALSE)

df_rmse <- df_rmse[df_rmse$anio != 2020,] # Descartar anio de pandemia
df_rmse <- df_rmse[!is.na(df_rmse$cierre),]
df_rmse <- df_rmse[df_rmse$clase != 'Previsión 1',] # Solo enfocarse en la 2da y 3ra prevision (abril y septiembre)

a <- unique(df_rmse$componente)
b <- unique(df_rmse$clase)
criterio <- 0.1

  # Periodo 2019-2022
  matrix_1 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones
  
  n <- 1
  for (i in a) {
    aux <- df_rmse[df_rmse$componente == i,]
    
    # RMSE 
    aux$dif <- (aux$cierre - aux$valor)**2
    rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
    z <- c(i, 'RMSE (p.p.)', rmse)
    matrix_1[n,] <- z
    
    # Sobre/subestimaciones
    aux$sobre <- aux$valor > aux$cierre + criterio
    z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
    matrix_1[n+1,] <- z
    
    aux$sub <- aux$valor < aux$cierre + criterio
    z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
    matrix_1[n+2,] <- z

    n <- n + 3
  }

  # Prevision 2 (Abril)
  matrix_2 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones
    
  n <- 1
  for (i in a) {
    aux <- df_rmse[df_rmse$componente == i & df_rmse$clase == 'Previsión 2',]
    
    # RMSE 
    aux$dif <- (aux$cierre - aux$valor)**2
    rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
    z <- c(i, 'RMSE (p.p.)', rmse)
    matrix_2[n,] <- z
    
    # Sobre/subestimaciones
    aux$sobre <- aux$valor > aux$cierre + criterio
    z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
    matrix_2[n+1,] <- z
    
    aux$sub <- aux$valor < aux$cierre + criterio
    z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
    matrix_2[n+2,] <- z
    
    n <- n + 3
  }

  # Prevision 3 (Septiembre)
  matrix_3 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones
  
  n <- 1
  for (i in a) {
    aux <- df_rmse[df_rmse$componente == i & df_rmse$clase == 'Previsión 3',]
    
    # RMSE 
    aux$dif <- (aux$cierre - aux$valor)**2
    rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
    z <- c(i, 'RMSE (p.p.)', rmse)
    matrix_3[n,] <- z
    
    # Sobre/subestimaciones
    aux$sobre <- aux$valor > aux$cierre + criterio
    z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
    matrix_3[n+1,] <- z
    
    aux$sub <- aux$valor < aux$cierre + criterio
    z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
    matrix_3[n+2,] <- z
    
    n <- n + 3
  }
  
matrix_1 <- data.frame(matrix_1)
matrix_2 <- data.frame(matrix_2)
matrix_3 <- data.frame(matrix_3)

colnames(matrix_1) <- c('Componente', 'Medida', paste0(min(df_rmse$anio), '-', max(df_rmse$anio)))
colnames(matrix_2) <- c('Componente', 'Medida', 'Prev 2 (Abr)')
colnames(matrix_3) <- c('Componente', 'Medida', 'Prev 3 (Sep)')

matrix_1$`2019-2022` <- as.numeric(matrix_1$`2019-2022`)
matrix_2$`Prev 2 (Abr)` <- as.numeric(matrix_2$`Prev 2 (Abr)`)
matrix_3$`Prev 3 (Sep)` <- as.numeric(matrix_3$`Prev 3 (Sep)`)

matrix_total <- merge(matrix_1, matrix_2,
                      by = c('Componente', 'Medida'),
                      all = TRUE,
                      sort = FALSE)

matrix_total <- merge(matrix_total, matrix_3,
                      by = c('Componente', 'Medida'),
                      all = TRUE,
                      sort = FALSE)

matrix_total$`2019-2022` <- round(matrix_total$`2019-2022`, 2)
matrix_total$`Prev 2 (Abr)` <- round(matrix_total$`Prev 2 (Abr)`, 2)
matrix_total$`Prev 3 (Sep)` <- round(matrix_total$`Prev 3 (Sep)`, 2)
tabla_com <- matrix_total



### Industria -----

# Datos 
df <- data.frame(read_excel(archivo,
                            sheet = 'Industrias', 
                            col_names = FALSE))

colnames(df) <- NULL
df <- df[c(1:20), ]

# Tratamiento
x <- t(df[1, ])
x <- na.locf(x)

df[1, ] <- t(x)
df[1, ] <- paste0(df[1, ], '_', df[2, ])

colnames(df) <- df[1, ]
df <- df[-c(1:2), ]

df <- df %>% 
  pivot_longer(cols = 2:ncol(df),
               names_to = 'clase',
               values_to = 'valor')

df <- df %>% 
  separate(clase, sep = '_', 
           c('anio', 'clase'))

df[, 'valor'] <- as.numeric(unlist(df[, 'valor']))
df[, 'anio'] <- as.numeric(unlist(df[, 'anio']))

names(df)[1] <- 'industria'

df_ind <- df

## RMSE (en puntos porcentuales) y numero de sobre/subestimaciones
df_rmse1 <- df[df$clase == 'Cierre',]
df_rmse1$clase <- NULL
df_rmse1 <- df_rmse1[df_rmse1$anio >= 2019,]
names(df_rmse1)[3] <- 'cierre'

df_rmse2 <- df[df$clase != 'Cierre',]
df_rmse2 <- df_rmse2[df_rmse2$anio >= 2019,]

df_rmse <- merge(df_rmse2, df_rmse1,
                 by = c('industria', 'anio'),
                 all = TRUE, 
                 sort = FALSE)

df_rmse <- df_rmse[df_rmse$anio != 2020,] # Descartar anio de pandemia
df_rmse <- df_rmse[!is.na(df_rmse$cierre),]
df_rmse <- df_rmse[df_rmse$clase != 'Previsión 1',] # Solo enfocarse en la 2da y 3ra prevision (abril y septiembre)

a <- unique(df_rmse$industria)
b <- unique(df_rmse$clase)
criterio <- 0.1

# Periodo 2019-2022
matrix_1 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones

n <- 1
for (i in a) {
  aux <- df_rmse[df_rmse$industria == i,]
  
  # RMSE 
  aux$dif <- (aux$cierre - aux$valor)**2
  rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
  z <- c(i, 'RMSE (p.p.)', rmse)
  matrix_1[n,] <- z
  
  # Sobre/subestimaciones
  aux$sobre <- aux$valor > aux$cierre + criterio
  z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
  matrix_1[n+1,] <- z
  
  aux$sub <- aux$valor < aux$cierre + criterio
  z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
  matrix_1[n+2,] <- z
  
  n <- n + 3
}

# Prevision 2 (Abril)
matrix_2 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones

n <- 1
for (i in a) {
  aux <- df_rmse[df_rmse$industria == i & df_rmse$clase == 'Previsión 2',]
  
  # RMSE 
  aux$dif <- (aux$cierre - aux$valor)**2
  rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
  z <- c(i, 'RMSE (p.p.)', rmse)
  matrix_2[n,] <- z
  
  # Sobre/subestimaciones
  aux$sobre <- aux$valor > aux$cierre + criterio
  z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
  matrix_2[n+1,] <- z
  
  aux$sub <- aux$valor < aux$cierre + criterio
  z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
  matrix_2[n+2,] <- z
  
  n <- n + 3
}

# Prevision 3 (Septiembre)
matrix_3 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones

n <- 1
for (i in a) {
  aux <- df_rmse[df_rmse$industria == i & df_rmse$clase == 'Previsión 3',]
  
  # RMSE 
  aux$dif <- (aux$cierre - aux$valor)**2
  rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
  z <- c(i, 'RMSE (p.p.)', rmse)
  matrix_3[n,] <- z
  
  # Sobre/subestimaciones
  aux$sobre <- aux$valor > aux$cierre + criterio
  z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
  matrix_3[n+1,] <- z
  
  aux$sub <- aux$valor < aux$cierre + criterio
  z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
  matrix_3[n+2,] <- z
  
  n <- n + 3
}

matrix_1 <- data.frame(matrix_1)
matrix_2 <- data.frame(matrix_2)
matrix_3 <- data.frame(matrix_3)

colnames(matrix_1) <- c('Industria', 'Medida', paste0(min(df_rmse$anio), '-', max(df_rmse$anio)))
colnames(matrix_2) <- c('Industria', 'Medida', 'Prev 2 (Abr)')
colnames(matrix_3) <- c('Industria', 'Medida', 'Prev 3 (Sep)')

matrix_1$`2019-2022` <- as.numeric(matrix_1$`2019-2022`)
matrix_2$`Prev 2 (Abr)` <- as.numeric(matrix_2$`Prev 2 (Abr)`)
matrix_3$`Prev 3 (Sep)` <- as.numeric(matrix_3$`Prev 3 (Sep)`)

matrix_total <- merge(matrix_1, matrix_2,
                      by = c('Industria', 'Medida'),
                      all = TRUE,
                      sort = FALSE)

matrix_total <- merge(matrix_total, matrix_3,
                      by = c('Industria', 'Medida'),
                      all = TRUE,
                      sort = FALSE)

matrix_total$`2019-2022` <- round(matrix_total$`2019-2022`, 2)
matrix_total$`Prev 2 (Abr)` <- round(matrix_total$`Prev 2 (Abr)`, 2)
matrix_total$`Prev 3 (Sep)` <- round(matrix_total$`Prev 3 (Sep)`, 2)
tabla_ind <- matrix_total



### EMOE -----

# Datos 
df <- data.frame(read_excel(archivo,
                            sheet = 'EMOE', 
                            col_names = FALSE))

colnames(df) <- NULL
df <- df[c(1:11), ]

# Tratamiento
x <- t(df[1, ])
x <- na.locf(x)

df[1, ] <- t(x)
df[1, ] <- paste0(df[1, ], '_', df[2, ])

colnames(df) <- df[1, ]
df <- df[-c(1:2), ]

df <- df %>% 
  pivot_longer(cols = 2:ncol(df),
               names_to = 'clase',
               values_to = 'valor')

df <- df %>% 
  separate(clase, sep = '_', 
           c('anio', 'clase'))

df[, 'valor'] <- as.numeric(unlist(df[, 'valor']))
df[, 'anio'] <- as.numeric(unlist(df[, 'anio']))

names(df)[1] <- 'industria'

df_emoe <- df

## RMSE (en puntos porcentuales) y numero de sobre/subestimaciones
df_rmse1 <- df[df$clase == 'Cierre',]
df_rmse1$clase <- NULL
df_rmse1 <- df_rmse1[df_rmse1$anio >= 2019,]
names(df_rmse1)[3] <- 'cierre'

df_rmse2 <- df[df$clase != 'Cierre',]
df_rmse2 <- df_rmse2[df_rmse2$anio >= 2019,]

df_rmse <- merge(df_rmse2, df_rmse1,
                 by = c('industria', 'anio'),
                 all = TRUE, 
                 sort = FALSE)

df_rmse <- df_rmse[df_rmse$anio != 2020,] # Descartar anio de pandemia
df_rmse <- df_rmse[!is.na(df_rmse$cierre),]

a <- unique(df_rmse$industria)
b <- unique(df_rmse$clase)
criterio <- 0.1

# Periodo 2019-2022
matrix_1 <- matrix(NA, nrow = length(a)*3, ncol = 3) # 3 debido a que calcula RMSE, sobre y subestimaciones

n <- 1
for (i in a) {
  aux <- df_rmse[df_rmse$industria == i,]
  
  # RMSE 
  aux$dif <- (aux$cierre - aux$valor)**2
  rmse <- sqrt(sum(aux$dif, na.rm = TRUE)/sum(!is.na(aux$dif)))
  z <- c(i, 'RMSE (p.p.)', rmse)
  matrix_1[n,] <- z
  
  # Sobre/subestimaciones
  aux$sobre <- aux$valor > aux$cierre + criterio
  z <- c(i, '# Sobreestimación', sum(aux$sobre, na.rm = TRUE))
  matrix_1[n+1,] <- z
  
  aux$sub <- aux$valor < aux$cierre + criterio
  z <- c(i, '# Subestimación', sum(aux$sub, na.rm = TRUE))
  matrix_1[n+2,] <- z
  
  n <- n + 3
}

matrix_1 <- data.frame(matrix_1)
colnames(matrix_1) <- c('Industria', 'Medida', 'Resultado')
matrix_1$Resultado <- as.numeric(matrix_1$Resultado)
matrix_1$Resultado <- round(matrix_1$Resultado, 2)
tabla_emoe <- matrix_1


# Resultados
save(df_com, df_ind, df_emoe, 
     file = 'Resultados/dfs.RData')

save(tabla_com, file = 'Resultados/tabla_com.RData')
save(tabla_ind, file = 'Resultados/tabla_ind.RData')
save(tabla_emoe, file = 'Resultados/tabla_emoe.RData')
