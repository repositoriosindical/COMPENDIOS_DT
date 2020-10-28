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
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) + 
  geom_text_repel(aes(label=ifelse(ano%in%c(1991,1995,2000,2004,2009,2015,2018),format(round(tasa,1)),"")), vjust=-1,colour="black") + 
  

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


cuadro3_2_r<-cuadro3_2 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                      `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                      `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                      `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                      `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                      `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                      `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                      `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
              add_row(Rama.Actividad.Económica="Minería",
                      `2011`=sum(.$`2011`[3]),
                      `2012`=sum(.$`2012`[3]),
                      `2013`=sum(.$`2013`[3]),
                      `2014`=sum(.$`2014`[3]),
                      `2015`=sum(.$`2015`[3]),
                      `2016`=sum(.$`2016`[3]),
                      `2017`=sum(.$`2017`[3])) %>%
  
              add_row(Rama.Actividad.Económica="Industria",
                      `2011`=sum(.$`2011`[4]),
                      `2012`=sum(.$`2012`[4]),
                      `2013`=sum(.$`2013`[4]),
                      `2014`=sum(.$`2014`[4]),
                      `2015`=sum(.$`2015`[4]),
                      `2016`=sum(.$`2016`[4]),
                      `2017`=sum(.$`2017`[4])) %>%
  
              add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
                      `2011`=sum(.$`2011`[5]),
                      `2012`=sum(.$`2012`[5]),
                      `2013`=sum(.$`2013`[5]),
                      `2014`=sum(.$`2014`[5]),
                      `2015`=sum(.$`2015`[5]),
                      `2016`=sum(.$`2016`[5]),
                      `2017`=sum(.$`2017`[5])) %>%
  
              add_row(Rama.Actividad.Económica="Construcción",
                      `2011`=sum(.$`2011`[6]),
                      `2012`=sum(.$`2012`[6]),
                      `2013`=sum(.$`2013`[6]),
                      `2014`=sum(.$`2014`[6]),
                      `2015`=sum(.$`2015`[6]),
                      `2016`=sum(.$`2016`[6]),
                      `2017`=sum(.$`2017`[6])) %>%
  
              add_row(Rama.Actividad.Económica="Comercio",
                      `2011`=sum(.$`2011`[7]+.$`2011`[8]),
                      `2012`=sum(.$`2012`[7]+.$`2012`[8]),
                      `2013`=sum(.$`2013`[7]+.$`2013`[8]),
                      `2014`=sum(.$`2014`[7]+.$`2014`[8]),
                      `2015`=sum(.$`2015`[7]+.$`2015`[8]),
                      `2016`=sum(.$`2016`[7]+.$`2016`[8]),
                      `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
              add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
                      `2011`=sum(.$`2011`[9]),
                      `2012`=sum(.$`2012`[9]),
                      `2013`=sum(.$`2013`[9]),
                      `2014`=sum(.$`2014`[9]),
                      `2015`=sum(.$`2015`[9]),
                      `2016`=sum(.$`2016`[9]),
                      `2017`=sum(.$`2017`[9])) %>%
  
              add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
                      `2011`=sum(.$`2011`[10]+.$`2011`[11]),
                      `2012`=sum(.$`2012`[10]+.$`2012`[11]),
                      `2013`=sum(.$`2013`[10]+.$`2013`[11]),
                      `2014`=sum(.$`2014`[10]+.$`2014`[11]),
                      `2015`=sum(.$`2015`[10]+.$`2015`[11]),
                      `2016`=sum(.$`2016`[10]+.$`2016`[11]),
                      `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
              add_row(Rama.Actividad.Económica="Servicios",
                      `2011`=sum(.$`2011`[12]+.$`2011`[13]+.$`2011`[14]+.$`2011`[15]+.$`2011`[16]+.$`2011`[17]),
                      `2012`=sum(.$`2012`[12]+.$`2012`[13]+.$`2012`[14]+.$`2012`[15]+.$`2012`[16]+.$`2012`[17]),
                      `2013`=sum(.$`2013`[12]+.$`2013`[13]+.$`2013`[14]+.$`2013`[15]+.$`2013`[16]+.$`2013`[17]),
                      `2014`=sum(.$`2014`[12]+.$`2014`[13]+.$`2014`[14]+.$`2014`[15]+.$`2014`[16]+.$`2014`[17]),
                      `2015`=sum(.$`2015`[12]+.$`2015`[13]+.$`2015`[14]+.$`2015`[15]+.$`2015`[16]+.$`2015`[17]),
                      `2016`=sum(.$`2016`[12]+.$`2016`[13]+.$`2016`[14]+.$`2016`[15]+.$`2016`[16]+.$`2016`[17]),
                      `2017`=sum(.$`2017`[12]+.$`2017`[13]+.$`2017`[14]+.$`2017`[15]+.$`2017`[16]+.$`2017`[17])) %>%
  
              add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
                      `2011`=sum(.$`2011`[18]),
                      `2012`=sum(.$`2012`[18]),
                      `2013`=sum(.$`2013`[18]),
                      `2014`=sum(.$`2014`[18]),
                      `2015`=sum(.$`2015`[18]),
                      `2016`=sum(.$`2016`[18]),
                      `2017`=sum(.$`2017`[18]))


cuadro3_3_r<-cuadro3_3 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                   `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[15]+.$`2018`[16]+.$`2018`[17]+.$`2018`[18]+.$`2018`[19]+.$`2018`[20]+.$`2018`[21])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2018`=sum(.$`2018`[22]))

  
cuadro3<-merge(cuadro3_1,cuadro3_2_r,by="Rama.Actividad.Económica",all.x = TRUE)
cuadro3<-merge(cuadro3,cuadro3_3_r,by="Rama.Actividad.Económica",all.x = TRUE)

cuadro3<-unique( cuadro3 )
cuadro3<-cuadro3[-3,]

# Grafico 3
cuadro3 %>% filter(Rama.Actividad.Económica!="Total") %>% 
  gather(ano,sindicatos,-Rama.Actividad.Económica) %>% mutate(tasa=as.numeric(sindicatos),
                                                              ano=as.numeric(ano)) %>% 
  ggplot(aes(x=ano,y=sindicatos,color=Rama.Actividad.Económica)) + geom_line() + geom_point() + theme_bw() + 
  labs(title = "Gráfico 3. Cantidad de sindicatos activos a nivel nacional, por rama de actividad económica",
       x="Anos",
       y = "Sindicatos activos",
       caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) + scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(1990, 2018),breaks = c(1990,2000,2010,2018))

ggsave(
  plot = last_plot(),
  filename = "Output/Graficos/1. OOSS/grafico3.Cantidad de sindicatos activos a nivel nacional, por rama de actividad económica.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 17
)


#### Cuadro 3 solo 2018 (ramas desagregadas)

cuadro3_3 %>% filter(Rama.Actividad.Económica!="Total") %>% 
  ggplot(aes(x=as.factor(reorder(Rama.Actividad.Económica,`2018`)),y=`2018`))+
  geom_bar(stat="identity", fill="#3399FF", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(`2018`, big.mark = ".", scientific = FALSE)), hjust=-0.5, colour = "black", size=3.0) +
  labs(title="Cantidad de sindicatos activos a nivel nacional, por rama de actividad económica en 2018",
       x="Ramas de actividad económica", 
       y = "Sindicatos")



#### Cuadro 4. y 5. Cantidad de afiliados ####

## 1990 a 2001
cuadro4_1<-cbind(
  read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 10,rows=c(1:12),cols=c(1:7),na.strings = "**"),
  read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 11,rows=c(2:13),cols=c(2:7),na.strings = "**")
)

## 2002 a 2010

a<-read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 12,rows=c(4:14),cols=c(1:10),na.strings = "**",colNames = FALSE)
b<-read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 13,rows=c(3:13),cols=c(2:10),na.strings = "**",colNames = FALSE)
c<-read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 14,rows=c(3:13),cols=c(2:10),na.strings = "**",colNames = FALSE)
names(a)<-c("Rama.Actividad.Económica","h2","m2","t2","h3","m3","t3","h4","m4","t4")
names(b)<-c("h5","m5","t5","h6","m6","t6","h7","m7","t7")
names(c)<-c("h8","m8","t8","h9","m9","t9","h10","m10","t10")
cuadro4_2_sexo<-cbind(a,b,c)

cuadro4_2<-cuadro4_2_sexo %>% select(1,4,7,10,13,16,19,22,25,28)

## 2011 a 2017
cuadro4_3_sexo<-cbind(
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 15,rows=c(4:22),cols=c(1:7),na.strings = "**",colNames = FALSE),
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 16,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 17,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 18,rows=c(4:22),cols=c(2:4),na.strings = "**",colNames = FALSE)
)

names(cuadro4_3_sexo)<-c("Rama.Actividad.Económica","h11","m11","t11","h12","m12","t12","h13","m13","t13","h14","m14","t14",
                               "h15","m15","t15","h16","m16","t16","h17","m17","t17")

cuadro4_3<-cuadro4_3_sexo %>% select(1,4,7,10,13,16,19,22)


## 2018
cuadro4_4_sexo<-read.xlsx("Input/Ilovepdf/1. OOSS.xlsx",sheet = 19,rows=c(3:26),cols=c(1:4),na.strings = "**")
names(cuadro4_4_sexo)<-c("Rama.Actividad.Económica","h18","m18","t18")
cuadro4_4<-cuadro4_4_sexo %>% select(1,4)

## A. Primero sin sexo

cuadro4_1
cuadro4_2
colnames(cuadro4_2)<-c("Rama.Actividad.Económica",2002,2003,2004,2005,2006,2007,2008,2009,2010)

colnames(cuadro4_3)<-c("Rama.Actividad.Económica",2011,2012,2013,2014,2015,2016,2017)

cuadro4_3<-cuadro4_3 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                   `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                   `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                   `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                   `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                   `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                   `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                   `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2011`=sum(.$`2011`[12]+.$`2011`[13]+.$`2011`[14]+.$`2011`[15]+.$`2011`[16]+.$`2011`[17]),
          `2012`=sum(.$`2012`[12]+.$`2012`[13]+.$`2012`[14]+.$`2012`[15]+.$`2012`[16]+.$`2012`[17]),
          `2013`=sum(.$`2013`[12]+.$`2013`[13]+.$`2013`[14]+.$`2013`[15]+.$`2013`[16]+.$`2013`[17]),
          `2014`=sum(.$`2014`[12]+.$`2014`[13]+.$`2014`[14]+.$`2014`[15]+.$`2014`[16]+.$`2014`[17]),
          `2015`=sum(.$`2015`[12]+.$`2015`[13]+.$`2015`[14]+.$`2015`[15]+.$`2015`[16]+.$`2015`[17]),
          `2016`=sum(.$`2016`[12]+.$`2016`[13]+.$`2016`[14]+.$`2016`[15]+.$`2016`[16]+.$`2016`[17]),
          `2017`=sum(.$`2017`[12]+.$`2017`[13]+.$`2017`[14]+.$`2017`[15]+.$`2017`[16]+.$`2017`[17])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2011`=sum(.$`2011`[18]),
          `2012`=sum(.$`2012`[18]),
          `2013`=sum(.$`2013`[18]),
          `2014`=sum(.$`2014`[18]),
          `2015`=sum(.$`2015`[18]),
          `2016`=sum(.$`2016`[18]),
          `2017`=sum(.$`2017`[18]))



colnames(cuadro4_4)<-c("Rama.Actividad.Económica",2018)

cuadro4_4<-cuadro4_4 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                   `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[15]+.$`2018`[16]+.$`2018`[17]+.$`2018`[18]+.$`2018`[19]+.$`2018`[20]+.$`2018`[21])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2018`=sum(.$`2018`[22]))

cuadro4<-merge(cuadro4_1,cuadro4_2,by="Rama.Actividad.Económica")

cuadro4<-merge(cuadro4,cuadro4_3,by="Rama.Actividad.Económica",all.x = TRUE)
cuadro4<-merge(cuadro4,cuadro4_4,by="Rama.Actividad.Económica",all.x = TRUE)

cuadro4<-unique( cuadro4 )
cuadro4<-cuadro4[-3,]

cuadro4 %>% filter(Rama.Actividad.Económica!="Total") %>% 
  gather(ano,afiliados,-Rama.Actividad.Económica) %>% mutate(tasa=as.numeric(afiliados),
                                                              ano=as.numeric(ano)) %>% 
  ggplot(aes(x=ano,y=afiliados,color=Rama.Actividad.Económica)) + geom_line() + geom_point() + theme_bw() + 
  labs(title = "Gráfico 4. Cantidad de afiliados activos a nivel nacional, por rama de actividad económica",
       x="Anos",
       y = "Trabajadores sindicalizados",
       caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) + scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(1990, 2018),breaks = c(1990,2000,2010,2018))



ggsave(
  plot = last_plot(),
  filename = "Output/Graficos/1. OOSS/grafico4.Cantidad de afiliados a nivel nacional, por rama de actividad económica.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 17
)


## B. Con sexo (cuadro 5) (solo desde 2002)

# hombres
cuadro4_2<-cuadro4_2_sexo %>% select(1,4-2,7-2,10-2,13-2,16-2,19-2,22-2,25-2,28-2)
cuadro4_3<-cuadro4_3_sexo %>% select(1,4-2,7-2,10-2,13-2,16-2,19-2,22-2)
cuadro4_4<-cuadro4_4_sexo %>% select(1,4-2)

colnames(cuadro4_2)<-c("Rama.Actividad.Económica",2002,2003,2004,2005,2006,2007,2008,2009,2010)

colnames(cuadro4_3)<-c("Rama.Actividad.Económica",2011,2012,2013,2014,2015,2016,2017)

cuadro4_3<-cuadro4_3 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                 `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                 `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                 `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                 `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                 `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                 `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                 `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2011`=sum(.$`2011`[12]+.$`2011`[13]+.$`2011`[14]+.$`2011`[15]+.$`2011`[16]+.$`2011`[17]),
          `2012`=sum(.$`2012`[12]+.$`2012`[13]+.$`2012`[14]+.$`2012`[15]+.$`2012`[16]+.$`2012`[17]),
          `2013`=sum(.$`2013`[12]+.$`2013`[13]+.$`2013`[14]+.$`2013`[15]+.$`2013`[16]+.$`2013`[17]),
          `2014`=sum(.$`2014`[12]+.$`2014`[13]+.$`2014`[14]+.$`2014`[15]+.$`2014`[16]+.$`2014`[17]),
          `2015`=sum(.$`2015`[12]+.$`2015`[13]+.$`2015`[14]+.$`2015`[15]+.$`2015`[16]+.$`2015`[17]),
          `2016`=sum(.$`2016`[12]+.$`2016`[13]+.$`2016`[14]+.$`2016`[15]+.$`2016`[16]+.$`2016`[17]),
          `2017`=sum(.$`2017`[12]+.$`2017`[13]+.$`2017`[14]+.$`2017`[15]+.$`2017`[16]+.$`2017`[17])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2011`=sum(.$`2011`[18]),
          `2012`=sum(.$`2012`[18]),
          `2013`=sum(.$`2013`[18]),
          `2014`=sum(.$`2014`[18]),
          `2015`=sum(.$`2015`[18]),
          `2016`=sum(.$`2016`[18]),
          `2017`=sum(.$`2017`[18]))

colnames(cuadro4_4)<-c("Rama.Actividad.Económica",2018)

cuadro4_4<-cuadro4_4 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                 `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[15]+.$`2018`[16]+.$`2018`[17]+.$`2018`[18]+.$`2018`[19]+.$`2018`[20]+.$`2018`[21])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2018`=sum(.$`2018`[22]))

cuadro5_hombres<-merge(cuadro4_2,cuadro4_3,by="Rama.Actividad.Económica",all.x = TRUE)
cuadro5_hombres<-merge(cuadro5_hombres,cuadro4_4,by="Rama.Actividad.Económica",all.x = TRUE)

cuadro5_hombres<-unique( cuadro5_hombres )
cuadro5_hombres<-cuadro5_hombres[-3,]


# mujeres
cuadro4_2<-cuadro4_2_sexo %>% select(1,4-1,7-1,10-1,13-1,16-1,19-1,22-1,25-1,28-1)
cuadro4_3<-cuadro4_3_sexo %>% select(1,4-1,7-1,10-1,13-1,16-1,19-1,22-1)
cuadro4_4<-cuadro4_4_sexo %>% select(1,4-1)

colnames(cuadro4_2)<-c("Rama.Actividad.Económica",2002,2003,2004,2005,2006,2007,2008,2009,2010)

colnames(cuadro4_3)<-c("Rama.Actividad.Económica",2011,2012,2013,2014,2015,2016,2017)

cuadro4_3<-cuadro4_3 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                 `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                 `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                 `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                 `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                 `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                 `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                 `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2011`=sum(.$`2011`[12]+.$`2011`[13]+.$`2011`[14]+.$`2011`[15]+.$`2011`[16]+.$`2011`[17]),
          `2012`=sum(.$`2012`[12]+.$`2012`[13]+.$`2012`[14]+.$`2012`[15]+.$`2012`[16]+.$`2012`[17]),
          `2013`=sum(.$`2013`[12]+.$`2013`[13]+.$`2013`[14]+.$`2013`[15]+.$`2013`[16]+.$`2013`[17]),
          `2014`=sum(.$`2014`[12]+.$`2014`[13]+.$`2014`[14]+.$`2014`[15]+.$`2014`[16]+.$`2014`[17]),
          `2015`=sum(.$`2015`[12]+.$`2015`[13]+.$`2015`[14]+.$`2015`[15]+.$`2015`[16]+.$`2015`[17]),
          `2016`=sum(.$`2016`[12]+.$`2016`[13]+.$`2016`[14]+.$`2016`[15]+.$`2016`[16]+.$`2016`[17]),
          `2017`=sum(.$`2017`[12]+.$`2017`[13]+.$`2017`[14]+.$`2017`[15]+.$`2017`[16]+.$`2017`[17])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2011`=sum(.$`2011`[18]),
          `2012`=sum(.$`2012`[18]),
          `2013`=sum(.$`2013`[18]),
          `2014`=sum(.$`2014`[18]),
          `2015`=sum(.$`2015`[18]),
          `2016`=sum(.$`2016`[18]),
          `2017`=sum(.$`2017`[18]))

colnames(cuadro4_4)<-c("Rama.Actividad.Económica",2018)

cuadro4_4<-cuadro4_4 %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                 `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[15]+.$`2018`[16]+.$`2018`[17]+.$`2018`[18]+.$`2018`[19]+.$`2018`[20]+.$`2018`[21])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
          `2018`=sum(.$`2018`[22]))

cuadro5_mujeres<-merge(cuadro4_2,cuadro4_3,by="Rama.Actividad.Económica",all.x = TRUE)
cuadro5_mujeres<-merge(cuadro5_mujeres,cuadro4_4,by="Rama.Actividad.Económica",all.x = TRUE)

cuadro5_mujeres<-unique( cuadro5_mujeres )
cuadro5_mujeres<-cuadro5_mujeres[-3,]

## combinar y crear cuadro 5

cuadro5_hombres<-cuadro5_hombres %>% mutate(sexo="hombres")
cuadro5_mujeres<-cuadro5_mujeres %>% mutate(sexo="mujeres")
cuadro5<-rbind(cuadro5_hombres,cuadro5_mujeres)

cuadro5 %>% filter(Rama.Actividad.Económica!="Total") %>% 
  gather(ano,afiliados,-Rama.Actividad.Económica,-sexo) %>% mutate(afiliados=as.numeric(afiliados),
                                                             ano=as.numeric(ano)) %>% 
  ggplot(aes(x=ano,y=afiliados,color=Rama.Actividad.Económica)) + geom_line() + geom_point() + theme_bw() + facet_wrap(~sexo) + 
  labs(title = "Gráfico 5. Cantidad de afiliados activos a nivel nacional, por rama de actividad económica y sexo",
       x="Anos",
       y = "Trabajadores sindicalizados",
       caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) + scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(2002, 2018),breaks = c(2002,2010,2018))



ggsave(
  plot = last_plot(),
  filename = "Output/Graficos/1. OOSS/grafico5.Cantidad de afiliados a nivel nacional, por rama de actividad económica y sexo.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 17
)



#### Cuadro 6. Cantidad de sindicatos activos, según tipo de sindicato, años 1991 a 2009 ####






#### Guardar tablas ####
library(writexl)
write_xlsx(cuadro1,"Output/Cuadros/1. OOSS/cuadro1.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro2,"Output/Cuadros/1. OOSS/cuadro2.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro3,"Output/Cuadros/1. OOSS/cuadro3.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro4,"Output/Cuadros/1. OOSS/cuadro4.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro5,"Output/Cuadros/1. OOSS/cuadro5.xlsx", col_names = TRUE,format_headers = TRUE)

