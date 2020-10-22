

## Datos de los Compendios Estadísticos de la Dirección del Trabajo en formato amigable

La Dirección del Trabajo publica interesante información estadística sobre sindicatos, negociaciones colectivas y huelgas en sus [compendios estadísticos](https://www.dt.gob.cl/portal/1629/w3-propertyvalue-22777.html) para los años 1990-2018. Sin embargo, las tablas se encuentran en formato `pdf`, lo que dificulta su uso y análisis. Con el objeto de hacerle la vida más fácil a los y las colegas, y a nosotros mismos cuando queremos usar los datos, elaboramos este repositorio.

El archivo *Script para leer principales tablas DT* produce los principales cuadros y gráficos de los compendios estadísticos, los cuáles se encuentran guardados en la ruta `Output/Cuadros` o `Output/Graficos`. La numeración de cuadros y gráficos corresponde a la de los documentos publicados por la misma DT. Los datos de los archivos `pdf` se pasaron a `R` mediante dos formas alternativas: (1) se copiaron y pegaron las tablas como texto en el script del programa, o (2) Con la aplicación online `I love pdf` se transformaron a `xlsx` los archivos `pdf`. Mediante los paquetes `readxl` y `openxlsx` estos archivos excel fueron leídos y posteriormente ordenados.

En el archivo *Script correcciones* se proponen formas alternativas de calcular las tasas de sindicalización y otros indicadores en base a otros datos novedosos o datos actualizados. Los resultados de esto se presentan en la ruta `Output/Extra`.

El resumen y descarga directa de los cuadros más relevantes se presenta aquí:

### 1. Organizaciones Sindicales (OOSS)

[Cuadro1](https://github.com/nicolasrattor/COMPENDIOS_DT/blob/main/Output/Cuadros/1.%20OOSS/cuadro1.xlsx). Cantidad de sindicatos activos, poblacion afiliada a sindicatos activos, fuerza de trabajo y tasas de sindicalización.

[Cuadro2](https://github.com/nicolasrattor/COMPENDIOS_DT/blob/main/Output/Cuadros/1.%20OOSS/cuadro2.xlsx). Cantidad de trabajadores afiliados a sindicatos activos, fuerza de trabajo ocupada, y tasa de sindicalización, según sexo.

[Cuadro3](https://github.com/nicolasrattor/COMPENDIOS_DT/blob/main/Output/Cuadros/1.%20OOSS/cuadro3.xlsx). Cantidad de sindicatos activos a nivel nacional, por rama de actividad económica.

### 2. Negociaciones colectivas

### 3. Huelgas