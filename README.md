Análisis de registros de *Canna indica* L.
================

``` r
# Paquetes
library(tidyverse)
library(readxl)
library(stringr)
library(stringi)

# Leer datos
datos_orig <- read_excel("Cantidad de Registros biologicos - incluye tipos de suelo.xlsx", skip = 1)
datos <- datos_orig %>% filter(`Numero de Registro`>0)
datos <- datos %>% filter(`Numero de Registro` < 20)
datos <- datos %>% select(-`Color representativo`)
table(datos$`Tipo de suelo predominante`)
```

    ## 
    ## Arcilloso - Rojizo/Rocoso        Arcilloso/Calcáreo          Arcilloso/Limoso 
    ##                         1                         1                         1 
    ##                  Calcáreo     Franco Fino/Pedregoso          Franco/Arcilloso 
    ##                         1                         1                         6 
    ##       Gravilloso/Calcáreo  Latosólico - Rojo/Rocoso 
    ##                         1                         1

``` r
# Refactorizar
datos <- datos %>%
  mutate(
    suelo_raw  = `Tipo de suelo predominante`,
    # normaliza: minúsculas, sin acentos, espacios únicos, unifica guiones y barras
    suelo_norm = suelo_raw |>
      stringr::str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      str_replace_all("[-/]", " ") |>
      str_squish(),
    grupo_suelo = case_when(
      str_detect(suelo_norm, "calcare") ~ "Calcáreo",
      str_detect(suelo_norm, "lato|rojo|rocos|pedreg|gravill") ~ "Fino pedregoso",
      TRUE ~ "Fino no calcáreo"
    )
  )
dplyr::count(datos, suelo_raw, grupo_suelo, sort = TRUE)
```

    ## # A tibble: 8 × 3
    ##   suelo_raw                 grupo_suelo          n
    ##   <chr>                     <chr>            <int>
    ## 1 Franco/Arcilloso          Fino no calcáreo     6
    ## 2 Arcilloso - Rojizo/Rocoso Fino pedregoso       1
    ## 3 Arcilloso/Calcáreo        Calcáreo             1
    ## 4 Arcilloso/Limoso          Fino no calcáreo     1
    ## 5 Calcáreo                  Calcáreo             1
    ## 6 Franco Fino/Pedregoso     Fino pedregoso       1
    ## 7 Gravilloso/Calcáreo       Calcáreo             1
    ## 8 Latosólico - Rojo/Rocoso  Fino pedregoso       1

``` r
# Evaluación por Chi cuadrado
tabla <- datos %>% group_by(grupo_suelo) %>%
  summarise(suma = sum(`Numero de Registro`))
chisq.test(tabla$suma)
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  tabla$suma
    ## X-squared = 0.14286, df = 2, p-value = 0.9311

No existe evidencia estadísticamente significativa de que la frecuencia
de registros difiera entre los tipos de suelo predominante en la
provincia.

Esto podría sugerir que la planta es generalista y que existe
subregsitros en otras provincias.

En resumen, la ausencia de diferencias significativas sugiere que *Canna
indica* podría comportarse como una especie edáficamente generalista, y
el patrón observado podría estar influido por desigualdades en el
esfuerzo de muestreo entre provincias.
