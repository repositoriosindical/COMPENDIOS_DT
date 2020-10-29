library(tidyverse)
library(ggrepel)
library(openxlsx)
library(tibble)


# Cuadro 1 ----------------------------------------------------------------

## Datos DT desde github
cuadro1<-read.xlsx("https://github.com/nicolasrattor/COMPENDIOS_DT/raw/main/Output/Cuadros/1.%20OOSS/cuadro1.xlsx",colNames = FALSE)
names(cuadro1)<-c("ano",
      "sindicatos_activos",
      "poblacion_afiliada",
      "ft_ocupada1",
      "tasa_sind1",
      "ft_ocupada2",
      "tasa_sind2",
      "poblacion_afiliada_sind_dep",
      "ft_ocupada3",
      "tasa_sind3")

cuadro1
