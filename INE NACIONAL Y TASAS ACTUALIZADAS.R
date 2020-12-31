library(tidyverse)
library(ggrepel)
library(openxlsx)
library(tibble)

#### Tabulados ocupación y categoría ocupación INE ####

## Fuerza de trabajo y ocupados

pob_edad_trabajar<-read.xlsx("http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/cuadros-estadisticos/series-de-tiempo-nueva-calibraci%C3%B3n-proyecciones-de-poblaci%C3%B3n-censo-2017/poblaci%C3%B3n-en-edad-de-trabajar-por-situaci%C3%B3n-en-la-fuerza-de-trabajo.xlsx?sfvrsn=afbd6d15_22",
                             sheet = 2,startRow = 6)
nombres<-names(pob_edad_trabajar)
pob_edad_trabajar<-read.xlsx("http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/cuadros-estadisticos/series-de-tiempo-nueva-calibraci%C3%B3n-proyecciones-de-poblaci%C3%B3n-censo-2017/poblaci%C3%B3n-en-edad-de-trabajar-por-situaci%C3%B3n-en-la-fuerza-de-trabajo.xlsx?sfvrsn=afbd6d15_22",
                             sheet = 2,startRow = 7)
pob_edad_trabajar<-pob_edad_trabajar[1:(nrow(pob_edad_trabajar)-3),c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28)]
nombres<-nombres[c(1,2,4-1,6-1,8-1,10-1,12-1,14-1,16-1,18-1,20-1,22-1,24-1,26-1,28-1)]
names(pob_edad_trabajar)<-nombres


## Dependientes y asalariados sector privado

categoria_ocupacion<-read.xlsx("http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/cuadros-estadisticos/series-de-tiempo-nueva-calibraci%C3%B3n-proyecciones-de-poblaci%C3%B3n-censo-2017/serie-categor%C3%ADa-en-la-ocupaci%C3%B3n.xlsx?sfvrsn=70dc54db_24",
                               sheet = 2,startRow = 6)
nombres<-names(categoria_ocupacion)
categoria_ocupacion<-read.xlsx("http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/cuadros-estadisticos/series-de-tiempo-nueva-calibraci%C3%B3n-proyecciones-de-poblaci%C3%B3n-censo-2017/serie-categor%C3%ADa-en-la-ocupaci%C3%B3n.xlsx?sfvrsn=70dc54db_24",
                             sheet = 2,startRow = 7)
categoria_ocupacion<-categoria_ocupacion[1:(nrow(categoria_ocupacion)-9),c(1,2,4,6,8,10,12,14,16,18,20,22,24,26)]
nombres<-nombres[c(1,2,4-1,6-1,8-1,10-1,12-1,14-1,16-1,18-1,20-1,22-1,24-1,26-1)]
names(categoria_ocupacion)<-nombres

# Tabulados ocupación INE censo 2002 (desde 1990) --------------------------------------

categoria_2002<-read.xlsx("Input/INE/1986-2009/ocupados-según-categoría---total-país(2).xlsx",
                                  sheet = 1,startRow = 9)
names(categoria_2002)<-c("Año","Trimestre","Total","Independientes.(Empleadores)","Independientes.(Trabajadores.por.cuenta.propia)",
                         "Asalariados.(Total)./4","Personal.de.servicio.doméstico.(Total)./6","Independientes.(Familiares.no.remunerados)")


pob_edad_trabajar_2002<-read.xlsx("Input/INE/1986-2009/total-pais---poblacion-de-15-anos-o-mas-segun-situacion-en-la-fuerza-de-trabajo-1986-2009(1).xlsx",
                          sheet = 1,startRow = 9)
pob_edad_trabajar_2002<-pob_edad_trabajar_2002[,c(1:6,9:12)]
names(pob_edad_trabajar_2002)<-c("Año","Trimestre","Población.en.edad.de.trabajar.(Total)",
                                 "Fuerza.de.trabajo.(Total)","Ocupados.(Total)","Desocupados.(Total)",
                                 "Inactivos","Tasa.de.desocupación","Tasa.de.ocupación","Tasa.de.participación"  )

## Filtrar OND y combinar

a<-pob_edad_trabajar %>% filter(Trimestre=="Oct - Dic" |
                               (Trimestre=="Sep - Nov" & Año==2020)) %>%  ## Mientras no hay dato
   select(Año,`Fuerza.de.trabajo.(Total)`,`Ocupados.(Total)`)

b<-categoria_ocupacion %>% filter(Trimestre=="Oct - Dic" |
                                     (Trimestre=="Sep - Nov" & Año==2020)) %>% select(`Asalariados.(Total)./4`,
                                                                     `Personal.de.servicio.doméstico.(Total)./6`,
                                                                     `Asalariados.(Sector.privado)`)
base2010_2020<-cbind(a,b)


a<-pob_edad_trabajar_2002 %>% 
   filter(Trimestre=="  Oct-Dic") %>% 
   select(`Fuerza.de.trabajo.(Total)`,`Ocupados.(Total)`) %>% 
   mutate(Año=1986:2009)

b<-categoria_2002 %>% filter(Trimestre=="  Oct-Dic") %>% select(`Asalariados.(Total)./4`,
                                                             `Personal.de.servicio.doméstico.(Total)./6`)

base1986_2009<-cbind(a,b)
base1986_2009<-base1986_2009 %>% mutate(`Asalariados.(Sector.privado)`=NA)  ## ocupar para pre 2010 los sector privados DT


base<-rbind(base1986_2009,base2010_2020)

as.numeric(base$`Fuerza.de.trabajo.(Total)`)
as.numeric(base$`Ocupados.(Total)`)
as.numeric(base$`Asalariados.(Sector.privado)`)

base$`Asalariados.(Total)./4`<-gsub(" ", "", base$`Asalariados.(Total)./4`, fixed = TRUE)
base$`Asalariados.(Total)./4`<-gsub(",", ".", base$`Asalariados.(Total)./4`, fixed = TRUE)
base$`Asalariados.(Total)./4`<-as.numeric(base$`Asalariados.(Total)./4`)

base$`Personal.de.servicio.doméstico.(Total)./6`<-gsub(" ", "", base$`Personal.de.servicio.doméstico.(Total)./6`, fixed = TRUE)
base$`Personal.de.servicio.doméstico.(Total)./6`<-gsub(",", ".", base$`Personal.de.servicio.doméstico.(Total)./6`, fixed = TRUE)
base$`Personal.de.servicio.doméstico.(Total)./6`<-as.numeric(base$`Personal.de.servicio.doméstico.(Total)./6`)

base<-base %>% mutate(Dependientes=`Personal.de.servicio.doméstico.(Total)./6`+`Asalariados.(Total)./4`) %>% 
   filter(Año>=1990&Año<=2020)

library(writexl)
write_xlsx(list("INE_nacional"=base),"Output/Nuevo/INE_nacional.xlsx", 
           col_names = TRUE,format_headers = TRUE)



#### Pegar cuadro INE con Cuadro DT ####

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

base<-base %>% filter(Año<=2018)

base<-cuadro1 %>% select(poblacion_afiliada,poblacion_afiliada_sind_dep,ft_ocupada3) %>% 
   cbind(base)

base<-base %>% mutate(`Asalariados.(Sector.privado)`=case_when(is.na(`Asalariados.(Sector.privado)`) ~ 
                                                            (ft_ocupada3-`Personal.de.servicio.doméstico.(Total)./6`)/1000,
                                                         TRUE~`Asalariados.(Sector.privado)` ))

base<-base %>% mutate(tasa1=poblacion_afiliada/`Fuerza.de.trabajo.(Total)`/10,
                tasa2=poblacion_afiliada/`Ocupados.(Total)`/10,
                tasa3=poblacion_afiliada_sind_dep/Dependientes/10,               ## considera sp y tcp
                tasa4=poblacion_afiliada_sind_dep/`Asalariados.(Total)./4`/10,   ## considera sector público
                tasa5=poblacion_afiliada_sind_dep/`Asalariados.(Sector.privado)`/10,  ## solo sector privado
                tasa6=poblacion_afiliada_sind_dep/(`Asalariados.(Sector.privado)`+`Personal.de.servicio.doméstico.(Total)./6`)/10) ## sector privado y tcp (tasa que tmb calcula DT)

base

base %>% mutate(Año=as.numeric(as.character(Año))) %>% 
   select(Año,tasa1,tasa2,tasa3,tasa4,tasa5,tasa6) %>% 
   gather(tipo,tasa,-Año) %>% mutate(tasa=as.numeric(tasa)) %>% 
   ggplot(aes(x=Año,y=tasa,color=tipo)) + geom_line() + geom_point() + theme_bw() + 
   labs(title = "Evolución de las tasas de sindicalizacion de la poblacion afiliada a sindicatos activos",
        subtitle = "Datos de ocupación de INE actualizados a Censo 2017 ",
        x="Anos",
        y = "Porcentaje",
        caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
   theme(legend.position = "bottom",plot.title = element_text(size=12),
         plot.subtitle = element_text(size=10),
         plot.caption = element_text(size=8)) + 
   scale_x_continuous(breaks = c(1991,1995,2000,2004,2009,2015,2018),limits = c(1990,2018))+
   geom_text_repel(aes(label=ifelse(Año%in%c(1991,1995,2000,2004,2009,2015,2018),
                                    format(paste0(round(tasa,1),"%")),"")), vjust=-1,colour="black")


ggsave(
   plot = last_plot(),
   filename = "Output/Nuevo/Evolucion de las tasas de sindicalizacion de la poblacion afiliada a sindicatos activos.png",
   device = "png",
   dpi = "retina",
   units = "cm",
   width = 33,
   height = 17
)

tasas<-c("tasa1=poblacion_afiliada/`Fuerza.de.trabajo.(Total)`/10,",
         "tasa2=poblacion_afiliada/`Ocupados.(Total)`/10,",
         "tasa3=poblacion_afiliada_sindicatos_dependientes/Dependientes/10,               ## considera sector privado, publico y servicio domestico",
         "tasa4=poblacion_afiliada_sindicatos_dependientes/`Asalariados.(Total)./4`/10,   ## considera sector público y privado",
         "tasa5=poblacion_afiliada_sindicatos_dependientes/`Asalariados.(Sector.privado)`/10,  ## solo sector privado",
         "tasa6=poblacion_afiliada_sindicatos_dependientes/(`Asalariados.(Sector.privado)`+`Personal.de.servicio.doméstico.(Total)./6`")
tasas<-tasas %>% as.data.frame()


#### Guardar tablas ####
library(writexl)
write_xlsx(list("tabla"=base,
                "tasas"=tasas),"Output/Nuevo/Tasas_actualizadas.xlsx", col_names = TRUE,format_headers = TRUE)
