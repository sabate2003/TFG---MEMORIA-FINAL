library(readxl)   # Para leer archivos Excel
library(dplyr)    # Para manipulación de datos
library(purrr)    # Para trabajar con listas fácilmente
library(xts)      # Para convertir a serie temporal

# Cargar el archivo Excel
archivo_excel <- "BASE DE DATOS ! .xlsx"

# Obtener los nombres de todas las hojas (que corresponden a las empresas)
nombres_hojas <- excel_sheets(archivo_excel)

# Leer cada hoja y guardarla en una lista
datos_empresas <- map(nombres_hojas, ~ read_excel(archivo_excel, sheet = .x))

# Asignar nombres a la lista (para identificar cada empresa)
names(datos_empresas) <- nombres_hojas

head(datos_empresas$BBVA)

datos_empresas <- map(datos_empresas, ~ mutate(.x,`Exchange Date`  = as.Date(`Exchange Date`)))

# Convertir cada empresa en serie temporal
series_empresas <- map(datos_empresas, ~ xts(.x$`Close`, order.by = .x$`Exchange Date`))

# Ver la serie temporal de BBVA
head(series_empresas$BBVA)

source("bds-recursive-inference-functions_v3REAL.R")

exists("bds_recurV3")
# Aplicar bds_recurV3 a todas las empresas
resultados_bds <- map(series_empresas, ~ bds_recurV3(.x, N0=100, m=6, nstd=0.50, lapso=1, tracebar=TRUE))

# Ver la estructura general de los resultados
str(resultados_bds)

# Aplicar bds_recur_stchg a todas las empresas
resultados_bds_stchg <- map(series_empresas, ~ bds_recur_stchg(.x, N0=100, m=6, lapso=1, type="OLS-CUSUM"))

# Ver los resultados de IBEX 35
resultados_bds_stchg$`IBEX 35`

# Ver el gráfico generado para IBEX 35
resultados_bds_stchg$`IBEX 35`$g_bds

# Ver los resultados del test de cambios estructurales para IBEX 35
resultados_bds_stchg$`IBEX 35`$salida.bds.strchg


# Ver los resultados de Acciona
resultados_bds_stchg$`Acciona `

# Ver el gráfico generado para Acciona
resultados_bds_stchg$`Acciona `$g_bds

# Ver los resultados del test de cambios estructurales para Acciona
resultados_bds_stchg$`Acciona `$salida.bds.strchg


# Ver los resultados de Acerinox
resultados_bds_stchg$`Acerinox `

# Ver el gráfico generado para Acerinox
resultados_bds_stchg$`Acerinox `$g_bds

# Ver los resultados del test de cambios estructurales para Acerinox
resultados_bds_stchg$`Acerinox `$salida.bds.strchg


# Ver los resultados de ACS
resultados_bds_stchg$`ACS `

# Ver el gráfico generado para ACS
resultados_bds_stchg$`ACS `$g_bds

# Ver los resultados del test de cambios estructurales para ACS
resultados_bds_stchg$`ACS `$salida.bds.strchg


# Ver los resultados de ArcelorMittal SA
resultados_bds_stchg$`ArcelorMittal SA`

# Ver el gráfico generado para ArcelorMittal SA
resultados_bds_stchg$`ArcelorMittal SA`$g_bds

# Ver los resultados del test de cambios estructurales para ArcelorMittal SA
resultados_bds_stchg$`ArcelorMittal SA`$salida.bds.strchg


# Ver los resultados de Banco Sabadell
resultados_bds_stchg$`Banco Sabadell`

# Ver el gráfico generado para Banco Sabadell
resultados_bds_stchg$`Banco Sabadell`$g_bds

# Ver los resultados del test de cambios estructurales para Banco Sabadell
resultados_bds_stchg$`Banco Sabadell`$salida.bds.strchg


# Ver los resultados de Banco Santander
resultados_bds_stchg$`Banco Santander`

# Ver el gráfico generado para Banco Santander
resultados_bds_stchg$`Banco Santander`$g_bds

# Ver los resultados del test de cambios estructurales para Banco Santander
resultados_bds_stchg$`Banco Santander`$salida.bds.strchg


# Ver los resultados de Bankinter
resultados_bds_stchg$`Bankinter`

# Ver el gráfico generado para Bankinter
resultados_bds_stchg$`Bankinter`$g_bds

# Ver los resultados del test de cambios estructurales para Bankinter
resultados_bds_stchg$`Bankinter`$salida.bds.strchg


# Ver los resultados de BBVA
resultados_bds_stchg$`BBVA `

# Ver el gráfico generado para BBVA
resultados_bds_stchg$`BBVA `$g_bds

# Ver los resultados del test de cambios estructurales para BBVA
resultados_bds_stchg$`BBVA `$salida.bds.strchg


# Ver los resultados de Caixabank
resultados_bds_stchg$`Caixabank `

# Ver el gráfico generado para Caixabank
resultados_bds_stchg$`Caixabank `$g_bds

# Ver los resultados del test de cambios estructurales para Caixabank
resultados_bds_stchg$`Caixabank `$salida.bds.strchg


# Ver los resultados de Colonial
resultados_bds_stchg$` Colonial`

# Ver el gráfico generado para Colonial
resultados_bds_stchg$` Colonial`$g_bds

# Ver los resultados del test de cambios estructurales para Colonial
resultados_bds_stchg$` Colonial`$salida.bds.strchg


# Ver los resultados de Enagas
resultados_bds_stchg$`Enagas`

# Ver el gráfico generado para Enagas
resultados_bds_stchg$`Enagas`$g_bds

# Ver los resultados del test de cambios estructurales para Enagas
resultados_bds_stchg$`Enagas`$salida.bds.strchg


# Ver los resultados de Endesa
resultados_bds_stchg$`Endesa `

# Ver el gráfico generado para Endesa
resultados_bds_stchg$`Endesa `$g_bds

# Ver los resultados del test de cambios estructurales para Endesa
resultados_bds_stchg$`Endesa `$salida.bds.strchg


# Ver los resultados de Ferrovial
resultados_bds_stchg$`Ferrovial `

# Ver el gráfico generado para Ferrovial
resultados_bds_stchg$`Ferrovial `$g_bds

# Ver los resultados del test de cambios estructurales para Ferrovial
resultados_bds_stchg$`Ferrovial `$salida.bds.strchg


# Ver los resultados de Fluidra
resultados_bds_stchg$`Fluidra`

# Ver el gráfico generado para Fluidra
resultados_bds_stchg$`Fluidra`$g_bds

# Ver los resultados del test de cambios estructurales para Fluidra
resultados_bds_stchg$`Fluidra`$salida.bds.strchg


# Ver los resultados de Grifols
resultados_bds_stchg$`Grifols `

# Ver el gráfico generado para Grifols
resultados_bds_stchg$`Grifols `$g_bds

# Ver los resultados del test de cambios estructurales para Grifols
resultados_bds_stchg$`Grifols `$salida.bds.strchg


# Ver los resultados de Iberdrola
resultados_bds_stchg$`Iberdrola `

# Ver el gráfico generado para Iberdrola
resultados_bds_stchg$`Iberdrola `$g_bds

# Ver los resultados del test de cambios estructurales para Iberdrola
resultados_bds_stchg$`Iberdrola `$salida.bds.strchg


# Ver los resultados de Inditex
resultados_bds_stchg$`Inditex`

# Ver el gráfico generado para Inditex
resultados_bds_stchg$`Inditex`$g_bds

# Ver los resultados del test de cambios estructurales para Inditex
resultados_bds_stchg$`Inditex`$salida.bds.strchg


# Ver los resultados de Indra
resultados_bds_stchg$`Indra `

# Ver el gráfico generado para Indra
resultados_bds_stchg$`Indra `$g_bds

# Ver los resultados del test de cambios estructurales para Indra
resultados_bds_stchg$`Indra `$salida.bds.strchg


# Ver los resultados de Mapfre
resultados_bds_stchg$`Mapfre`

# Ver el gráfico generado para Mapfre
resultados_bds_stchg$`Mapfre`$g_bds

# Ver los resultados del test de cambios estructurales para Mapfre
resultados_bds_stchg$`Mapfre`$salida.bds.strchg


# Ver los resultados de Naturgy
resultados_bds_stchg$`Naturgy `

# Ver el gráfico generado para Naturgy
resultados_bds_stchg$`Naturgy `$g_bds

# Ver los resultados del test de cambios estructurales para Naturgy
resultados_bds_stchg$`Naturgy `$salida.bds.strchg


# Ver los resultados de Redeia
resultados_bds_stchg$`Redeia`

# Ver el gráfico generado para Redeia
resultados_bds_stchg$`Redeia`$g_bds

# Ver los resultados del test de cambios estructurales para Redeia
resultados_bds_stchg$`Redeia`$salida.bds.strchg


# Ver los resultados de Sacyr
resultados_bds_stchg$`Sacyr`

# Ver el gráfico generado para Sacyr
resultados_bds_stchg$`Sacyr`$g_bds

# Ver los resultados del test de cambios estructurales para Sacyr
resultados_bds_stchg$`Sacyr`$salida.bds.strchg


# Ver los resultados de Solaria
resultados_bds_stchg$`Solaria `

# Ver el gráfico generado para Solaria
resultados_bds_stchg$`Solaria `$g_bds

# Ver los resultados del test de cambios estructurales para Solaria
resultados_bds_stchg$`Solaria `$salida.bds.strchg


# Ver los resultados de Telefonica
resultados_bds_stchg$`Telefonica`

# Ver el gráfico generado para Telefonica
resultados_bds_stchg$`Telefonica`$g_bds

# Ver los resultados del test de cambios estructurales para Telefonica
resultados_bds_stchg$`Telefonica`$salida.bds.strchg



