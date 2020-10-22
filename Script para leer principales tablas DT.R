library(tidyverse)
library(ggrepel)
library(openxlsx)
library(tibble)


# 1. OOSS -----------------------------------------------------------------



#### Cuadro1. Cantidad de sindicatos activos, poblacion afiliada a sindicatos activos, fuerza de trabajo y tasas de sindicalización, años 1990 a 1999 ####

f0<-c("ano",
      "sindicatos_activos",
      "poblacion_afiliada",
      "ft_ocupada1",
      "tasa_sind1",
      "ft_ocupada2",
      "tasa_sind2",
      "poblacion_afiliada_sind_dep",
      "ft_ocupada3",
      "tasa_sind3")

cuadro1<-rbind(
c(1990 ,NA   ,606812 ,4525530 ,13.4, 3745599, 16.2, 515825 ,2692459, 19.2),
c(1991 ,7707 ,701355 ,4630670 ,15.1, 3862016, 18.2, 586716 ,2765416, 21.2),
c(1992 ,8323 ,724065 ,4877430 ,14.8, 4054999, 17.9, 602435 ,2909679, 20.7),
c(1993 ,7974 ,684361 ,5109290 ,13.4, 4280082, 16.0, 569066 ,3067602, 18.6),
c(1994 ,7891 ,661966 ,5122760 ,12.9, 4305265, 15.4, 547862 ,3038135, 18.0),
c(1995 ,7505 ,637570 ,5174410 ,12.3, 4364825, 14.6, 518094 ,3100665, 16.7),
c(1996 ,NA   , NA     ,5298680 ,NA   ,4525632 ,NA   ,NA      ,3243452,  NA),
c(1997 ,7446 ,617761 ,5380190 ,11.5, 4643794, 13.3, 501386 ,3350654, 15.0),
c(1998 ,7439 ,611535 ,5432350 ,11.3, 4657376, 13.1, 489957 ,3276276, 15.0),
c(1999 ,7057 ,579996 ,5404480 ,10.7, 4552804, 12.7, 463071 ,3192874, 14.5),
c(2000, 7659, 595495, 5381460, 11.1, 4528339, 13.2, 467835, 3170319, 14.8),
c(2001, 7410, 599610, 5479390, 10.9, 4631937, 12.9, 466138, 3187987, 14.6),
c(2002, 8149, 618930, 5531260, 11.2, 4723018, 13.1, 481749, 3253178, 14.8),
c(2003, 8967, 669507, 5675130, 11.8, 4849564, 13.8, 519687, 3326674, 15.6),
c(2004, 9416, 680351, 5862900, 11.6, 4979719, 13.7, 533175, 3423609, 15.6),
c(2005, 9148, 676368, 5904999, 11.5, 5054172, 13.4, 539326, 3581293, 15.1),
c(2006, 9424, 703706, 6410982, 11.0, 5477846, 12.8, 568856, 3933474, 14.5),
c(2007, 9365, 724606, 6567241, 11.0, 5622264, 12.9, 607190, 4099492, 14.8),
c(2008, 9340, 801251, 6740408, 11.9, 5782781, 13.9, 685763, 4251018, 16.1),
c(2009, 9776, 837055, 6710990, 12.5, 5747152, 14.6, 712814, 4131857, 17.3),
c(2010, 9871  ,858571,  7353835, 11.7, 6172536, 13.9, 734495  ,4646299, 15.8),
c(2011 ,10310 ,892365,  7564346, 11.8, 6348945, 14.1, 765138  ,4863073, 15.7),
c(2012 ,10585 ,940603,  7699431, 12.2, 6441986, 14.6, 821041  ,4960005, 16.6),
c(2013 ,10634 ,940222,  7904048, 11.9, 6600648, 14.2, 823722  ,5024782, 16.4),
c(2014 ,11162 ,985770,  8013717, 12.3, 6686276, 14.7, 859093  ,5061592, 17.0),
c(2015 ,11433 ,1048234, 8136356, 12.9, 6815314, 15.4, 919315  ,5133964, 17.9),
c(2016 ,11653 ,1139955 ,8216865 ,13.9 ,6901108 ,16.5 ,1009255 ,5142873, 19.6),
c(2017 ,11916 ,1179445 ,8406527 ,14.0 ,6930930 ,17.0 ,1043865 ,5099920, 20.5),
c(2018 ,11920 ,1174346 ,8414056 ,14.0 ,6927523 ,17.0 ,1045300 ,5068372, 20.6)
)

cuadro1<-cuadro1 %>% as.data.frame()
names(cuadro1)<-f0
cuadro1


# Grafico 1
cuadro1 %>% select(ano,tasa_sind1,tasa_sind2,tasa_sind3) %>% 
  gather(tipo,tasa,-ano) %>% mutate(tasa=as.numeric(tasa)) %>% 
  ggplot(aes(x=ano,y=tasa,color=tipo)) + geom_line() + geom_point() + theme_bw() + 
  labs(title = "Gráfico 1. Evolución de las tasas de sindicalizacion de la poblacion afiliada a sindicatos activos",
       x="Anos",
       y = "Porcentaje",
       caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
  theme(legend.position = "bottom",plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(size=6)) + 
  geom_text_repel(aes(label=ifelse(ano%in%c(1991,1995,2000,2004,2009,2015,2018),format(round(tasa,1)),"")), vjust=-1,colour="black")

ggsave(
  plot = last_plot(),
  filename = "Output/Graficos/1. OOSS/grafico1.Evolucion de las tasas de sindicalizacion de la poblacion afiliada a sindicatos activos.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 17
)


#### Cuadro 2. Cantidad de trabajadores afiliados a sindicatos activos, fuerza de trabajo ocupada, y tasa de sindicalización, según sexo ####

cuadro2<-rbind(
c(2002, 497272, 121658, 618930 , 3241454, 1481566, 4723020, 15.3, 8.2  ,13.1),
c(2003, 531238, 138269, 669507 , 3298663, 1550891, 4849554, 16.1, 8.9  ,13.8),
c(2004, 522271, 158080, 680351 , 3335792, 1643938, 4979730, 15.7, 9.6  ,13.7),
c(2005, 515114, 161254, 676368 , 3376302, 1677870, 5054172, 15.3, 9.6  ,13.4),
c(2006, 532661, 171045, 703706 , 3627100, 1850746, 5477846, 14.7, 9.2  ,12.8),
c(2007, 537651, 186955, 724606 , 3683805, 1938459, 5622264, 14.6, 9.6  ,12.9),
c(2008, 562172, 239079, 801251 , 3738207, 2044574, 5782781, 15.0, 11.7 ,13.9),
c(2009, 580795, 256260, 837055 , 3661636, 2085516, 5747152, 15.9, 12.3 ,14.6),
c(2010, 585384, 273187, 858571 , 3822903, 2349633, 6172536, 15.3, 11.6 ,13.9),
c(2011, 593203, 299162, 892365 , 3883465, 2465480, 6348945, 15.3, 12.1 ,14.1),
c(2012, 618610, 321993, 940603 , 3930414, 2511571, 6441986, 15.7, 12.8 ,14.6),
c(2013, 610485, 329737, 940222 , 4005377, 2595271, 6600648, 15.2, 12.7 ,14.2),
c(2014, 630512, 355258, 985770 , 4035930, 2650346, 6686276, 15.6, 13.4 ,14.7),
c(2015, 646093, 402141, 1048234, 4081413, 2733898, 6815311, 15.8, 14.7 ,15.4),
c(2016, 683516, 455528, 1139044, 4153796, 2747312, 6901108, 16.5, 16.6 ,16.5),
c(2017, 691381, 488064, 1179445, 4157795, 2773138, 6930933, 16.6, 17.6 ,17.0),
c(2018, 681399, 492947, 1174346, 4149084, 2778439, 6927523, 16.4, 17.7 ,17.0)
) %>% as.data.frame()

names(cuadro2)<-c("ano","afiliados_hombres","afiliados_mujeres","afiliados_total",
                  "ft_hombres","ft_mujeres","ft_total","ft_hombres", "ft_mujeres","ft_total")


#### Cuadro 3. Cantidad de sindicatos activos a nivel nacional, por rama de actividad económica ####

# CIIU rev 2 1991-2010
cuadro3_1<-cbind(
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 5,rows=c(1:12),cols=c(1:8),na.strings = "**"),
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 6,rows=c(1:12),cols=c(2:8),na.strings = "**"),
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 7,rows=c(2:13),cols=c(2:8),na.strings = "**")
)

# CIIU rev 3 2011-2017
cuadro3_2<-read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 8,rows=c(2:21),cols=c(1:8),na.strings = "**")

# CIIU rev 4 2018
cuadro3_3<-read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 9,rows=c(2:25),cols=c(1:2),na.strings = "**")


cuadro3_2 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                      `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                      `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                      `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                      `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                      `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                      `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                      `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
              add_row(Rama.Actividad.Económica="Minería") %>%
              add_row(Rama.Actividad.Económica="Industria") %>%
              add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua") %>%
              add_row(Rama.Actividad.Económica="Construcción") %>%
              add_row(Rama.Actividad.Económica="Comercio") %>%
              add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones") %>%
              add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros") %>%
              add_row(Rama.Actividad.Económica="Servicios") %>%
              add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas")
  
cuadro3<-merge(cuadro3_1,cuadro3_2,by="Rama.Actividad.Económica",all.x = TRUE)
cuadro3<-merge(cuadro3,cuadro3_3,by="Rama.Actividad.Económica",all.x = TRUE)


#### Cuadro 4. Cantidad de sindicatos activos a nivel nacional, por rama de actividad económica ####






#### Guardar tablas ####
library(writexl)
write_xlsx(cuadro1,"Output/Cuadros/1. OOSS/cuadro1.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro2,"Output/Cuadros/1. OOSS/cuadro2.xlsx", col_names = TRUE,format_headers = TRUE)

