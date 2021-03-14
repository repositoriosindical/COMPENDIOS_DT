library(tidyverse)
library(ggrepel)
library(openxlsx)
library(tibble)


# 3. HUELGAS -----------------------------------------------------------------



#### Cuadro1.  ####

f0<-c("ano",
      "huelgas_aprobadas",
      "trabajadores_aprobadas",
      "huelgas_efectuadas",
      "trabajadores_efectuadas")

cuadro1<-rbind(
c(1997 ,401 ,39351 ,179 , 19278),
c(1998 ,239 ,26308 ,121 , 12608),
c(1999 ,280 ,28236 ,108 , 10667),
c(2000 ,300 ,31645 ,125 , 13227),
c(2001 ,251 ,26463 ,85 ,  11591),
c(2002 ,361 ,38708 ,117 , 14662),
c(2003 ,379 ,32722 ,92  , 10443),
c(2004 ,398 ,36352 ,125 , 13013),
c(2005 ,444 ,38093 ,101 , 11209),
c(2006 ,538 ,55017 ,134 , 15602),
c(2007 ,625 ,77659 ,146 , 17294),
c(2008 ,611 ,61721 ,159 , 17473),
c(2009 ,626 ,81125 ,171 , 21915),
c(2010 ,672 ,81940 ,174 , 31799),
c(2011 ,735 ,88617 ,183 , 22698),
c(2012 ,772 ,101137, 161, 30035),
c(2013 ,832 ,110929, 201, 30638),
c(2014 ,824 ,129334, 214, 41939),
c(2015 ,841 ,127458, 174, 24325),
c(2016 ,800 ,115064, 204, 36876),
c(2017 ,747 ,110158, 148, 19425),
c(2018 ,698 ,169087, 143, 15969),
c(2019, 845, 124250, 153, 32128)
)

cuadro1<-cuadro1 %>% as.data.frame()
names(cuadro1)<-f0
cuadro1


# Grafico 1
cuadro1 %>% select(ano,huelgas_aprobadas,huelgas_efectuadas) %>% 
  gather(tipo,huelgas,-ano) %>% mutate(huelgas=as.numeric(huelgas)) %>% 
  ggplot(aes(x=ano,y=huelgas,color=tipo)) + geom_line() + geom_point() + theme_bw() + 
  labs(title = "Gráfico 1. Cantidad de huelgas, por estado de huelga",
       x="Anos",
       y = "Huelgas",
       caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) + 
  geom_text_repel(aes(label=ifelse(ano%in%c(1997,2000,2004,2009,2015,2019),format(round(huelgas,1)),"")), vjust=-1,colour="black") +
  scale_x_continuous(limits = c(1997, 2019),breaks = c(1997,2001,2005,2009,2014,2019))
  
  
  ggsave(
    plot = last_plot(),
    filename = "Output/Graficos/3. HUELGAS/grafico1.A.Cantidad de huelgas, por estado de huelga.png",
    device = "png",
    dpi = "retina",
    units = "cm",
    width = 33,
    height = 17
  )


  cuadro1 %>% select(ano,trabajadores_aprobadas,trabajadores_efectuadas) %>% 
    gather(tipo,huelgas,-ano) %>% mutate(huelgas=as.numeric(huelgas)) %>% 
    ggplot(aes(x=ano,y=huelgas,color=tipo)) + geom_line() + geom_point() + theme_bw() + 
    labs(title = "Gráfico 1. Cantidad trabajadores involucrados en huelgas, por estado de huelga",
         x="Anos",
         y = "Trabajadores",
         caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
    theme(legend.position = "bottom",plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          plot.caption = element_text(size=8)) + 
    geom_text_repel(aes(label=ifelse(ano%in%c(1997,2001,2005,2009,2012,2014,2019),format(round(huelgas,1),big.mark = ".", scientific = FALSE),"")), vjust=-1,colour="black") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
    scale_x_continuous(limits = c(1997, 2019),breaks = c(1997,2001,2005,2009,2014,2019))
  
  
  ggsave(
    plot = last_plot(),
    filename = "Output/Graficos/3. HUELGAS/grafico1.B.Cantidad de trabajadores involucrados en huelgas, por estado de huelga.png",
    device = "png",
    dpi = "retina",
    units = "cm",
    width = 33,
    height = 17
  )
  

  #### Cuadro4.  ####
  
  f0<-c("ano",
        "huelgas_terminadas" ,
        "trabajadores" ,
        "dias" ,
        "promedio dias",
        "dias trabajador",
        "dias promedio por trabajador")
  
  cuadro2<-rbind(
  
    c(1997, 179 ,19278 ,1850, 10.3 ,250599 ,NA),
    c(1998, 121 ,12608 ,1204, 10.0 ,123507 ,NA),
    c(1999, 108 ,10667 ,1308, 12.0 ,103232 ,NA),
    c(2000, 125 ,13227 ,1121,  9.0  ,114306 ,NA),
    c(2001, 86  ,11591,  805,  9.4  ,127157 ,NA),
    c(2002, 117 ,14662 ,1363, 11.6 ,207224 ,NA),
    c(2003, 92  ,10443,  802,  8.7 ,  73467 ,NA),
    c(2004, 125 ,13013 ,1586, 12.7 ,172858 ,NA),
    c(2005, 101 ,11209 ,1131, 11.2 , 99931  ,NA),
    c(2006, 134 ,15602 ,1501, 11.2 ,195344 ,NA),
    c(2007, 146 ,17294 ,1593, 10.9 ,163770 ,NA),
    c(2008, 159 ,17473 ,1701, 10.7 ,202178 ,NA),
    c(2009, 171 ,21915 ,1961, 11.5 ,242508 ,NA),
    c(2010, 174 ,31799 ,2178, 12.5 ,333822 ,NA),
    c(2011, 183 ,22698 ,2227, 12.2 ,257591 ,NA),
    c(2012, 161 ,30035 ,1886, 11.7 ,252316 ,NA),
    c(2013, 201 ,30638 ,2756, 13.7 ,412379 ,NA),
    c(2014, 214 ,41939 ,2463, 11.5 ,443517 ,NA),
    c(2015, 174 ,24325 ,2524, 14.5 ,330509 ,NA),
    c(2016, 204 ,36876 ,3279, 16.1 ,606590 ,NA),
    c(2017, 148 ,19425 ,2406, 16.3 ,383738 ,NA),
    c(2018, 143 ,15969 ,2233, 15.6 ,242902 ,15.2),
    c(2019, 153, 32128, 2287, 14.9, 382083, 11.9)
  
)
  
  cuadro2<-cuadro2 %>% as.data.frame()
  names(cuadro2)<-f0
  cuadro2
  
  
  cuadro2 %>% mutate(`promedio dias`=as.numeric(`promedio dias`)) %>% 
    ggplot(aes(x=ano,y=`promedio dias`)) + geom_line() + geom_point() + theme_bw() + 
    labs(title = "Gráfico 4. Duración promedio",
         x="Anos",
         y = "Duración (días)",
         caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
    theme(legend.position = "bottom",plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          plot.caption = element_text(size=8)) + 
    geom_text_repel(aes(label=ifelse(ano%in%c(1997,2001,2005,2009,2012,2014,2019),format(round(`promedio dias`,1),big.mark = ".", scientific = FALSE),"")), vjust=-1,colour="black") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
    scale_x_continuous(limits = c(1997, 2019),breaks = c(1997,2001,2005,2009,2014,2019))
  

  ggsave(
    plot = last_plot(),
    filename = "Output/Graficos/3. HUELGAS/grafico4.Duración promedio.png",
    device = "png",
    dpi = "retina",
    units = "cm",
    width = 33,
    height = 17
  )
  
  
  #### Cuadro por sectores (no está en compendio, construido en base a anuario 2018 y archivo excel en PC)
  
  cuadro3_2018_sexo<-read.xlsx("Input/Ilovepdf/3. HUELGAS (ANUARIO 2018).xlsx",sheet = 3,rows=c(3:26),cols=c(1:5),na.strings = "**")
  cuadro3_2018_huelgas<-cuadro3_2018_sexo %>% select(1,2)
  names(cuadro3_2018_huelgas)<-c("Rama.Actividad.Económica","huelgas_terminadas")
  
  cuadro3_2018_huelgas<-cuadro3_2018_huelgas %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                     `huelgas_terminadas`=sum(.$`huelgas_terminadas`[1])) %>% 
    
    add_row(Rama.Actividad.Económica="Minería",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[2])) %>%
    
    add_row(Rama.Actividad.Económica="Industria",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[3])) %>%
    
    add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[4]+.$`huelgas_terminadas`[5])) %>%
    
    add_row(Rama.Actividad.Económica="Construcción",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[6])) %>%
    
    add_row(Rama.Actividad.Económica="Comercio",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[7]+.$`huelgas_terminadas`[9])) %>%
    
    add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[8]+.$`huelgas_terminadas`[10])) %>%
    
    add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[11]+.$`huelgas_terminadas`[12])) %>%
    
    add_row(Rama.Actividad.Económica="Servicios",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[13]+.$`huelgas_terminadas`[14]+.$`huelgas_terminadas`[15]+.$`huelgas_terminadas`[16]+.$`huelgas_terminadas`[17]+.$`huelgas_terminadas`[18]+.$`huelgas_terminadas`[19]+.$`huelgas_terminadas`[20]+.$`huelgas_terminadas`[21])) %>%
    
    add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[22]))
  
  
  
  cuadro3_2019_sexo<-read.xlsx("Input/Ilovepdf/5.huelgas_2019.xlsx",sheet = 3,rows=c(3:26),cols=c(1:5),na.strings = "**")
  cuadro3_2019_huelgas<-cuadro3_2019_sexo %>% select(1,2)
  names(cuadro3_2019_huelgas)<-c("Rama.Actividad.Económica","huelgas_terminadas")
  
  cuadro3_2019_huelgas<-cuadro3_2019_huelgas %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                                         `huelgas_terminadas`=sum(.$`huelgas_terminadas`[1])) %>% 
    
    add_row(Rama.Actividad.Económica="Minería",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[2])) %>%
    
    add_row(Rama.Actividad.Económica="Industria",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[3])) %>%
    
    add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[4]+.$`huelgas_terminadas`[5])) %>%
    
    add_row(Rama.Actividad.Económica="Construcción",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[6])) %>%
    
    add_row(Rama.Actividad.Económica="Comercio",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[7]+.$`huelgas_terminadas`[9])) %>%
    
    add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[8]+.$`huelgas_terminadas`[10])) %>%
    
    add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[11]+.$`huelgas_terminadas`[12])) %>%
    
    add_row(Rama.Actividad.Económica="Servicios",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[13]+.$`huelgas_terminadas`[14]+.$`huelgas_terminadas`[15]+.$`huelgas_terminadas`[16]+.$`huelgas_terminadas`[17]+.$`huelgas_terminadas`[18]+.$`huelgas_terminadas`[19]+.$`huelgas_terminadas`[20]+.$`huelgas_terminadas`[21])) %>%
    
    add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
            `huelgas_terminadas`=sum(.$`huelgas_terminadas`[22]))
  
  
  
  
  
  cuadro3_2018_trabajadores<-cuadro3_2018_sexo %>% select(1,5)
  names(cuadro3_2018_trabajadores)<-c("Rama.Actividad.Económica","trabajadores")
  
  cuadro3_2018_trabajadores<-cuadro3_2018_trabajadores %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                                         `trabajadores`=sum(.$`trabajadores`[1])) %>% 
    
    add_row(Rama.Actividad.Económica="Minería",
            `trabajadores`=sum(.$`trabajadores`[2])) %>%
    
    add_row(Rama.Actividad.Económica="Industria",
            `trabajadores`=sum(.$`trabajadores`[3])) %>%
    
    add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
            `trabajadores`=sum(.$`trabajadores`[4]+.$`trabajadores`[5])) %>%
    
    add_row(Rama.Actividad.Económica="Construcción",
            `trabajadores`=sum(.$`trabajadores`[6])) %>%
    
    add_row(Rama.Actividad.Económica="Comercio",
            `trabajadores`=sum(.$`trabajadores`[7]+.$`trabajadores`[9])) %>%
    
    add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
            `trabajadores`=sum(.$`trabajadores`[8]+.$`trabajadores`[10])) %>%
    
    add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
            `trabajadores`=sum(.$`trabajadores`[11]+.$`trabajadores`[12])) %>%
    
    add_row(Rama.Actividad.Económica="Servicios",
            `trabajadores`=sum(.$`trabajadores`[13]+.$`trabajadores`[14]+.$`trabajadores`[15]+.$`trabajadores`[16]+.$`trabajadores`[17]+.$`trabajadores`[18]+.$`trabajadores`[19]+.$`trabajadores`[20]+.$`trabajadores`[21])) %>%
    
    add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
            `trabajadores`=sum(.$`trabajadores`[22]))
  
  
  
  cuadro3_2019_trabajadores<-cuadro3_2019_sexo %>% select(1,5)
  names(cuadro3_2019_trabajadores)<-c("Rama.Actividad.Económica","trabajadores")
  
  cuadro3_2019_trabajadores<-cuadro3_2019_trabajadores %>% add_row(Rama.Actividad.Económica="Agricultura  y  pesca",
                                                                   `trabajadores`=sum(.$`trabajadores`[1])) %>% 
    
    add_row(Rama.Actividad.Económica="Minería",
            `trabajadores`=sum(.$`trabajadores`[2])) %>%
    
    add_row(Rama.Actividad.Económica="Industria",
            `trabajadores`=sum(.$`trabajadores`[3])) %>%
    
    add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
            `trabajadores`=sum(.$`trabajadores`[4]+.$`trabajadores`[5])) %>%
    
    add_row(Rama.Actividad.Económica="Construcción",
            `trabajadores`=sum(.$`trabajadores`[6])) %>%
    
    add_row(Rama.Actividad.Económica="Comercio",
            `trabajadores`=sum(.$`trabajadores`[7]+.$`trabajadores`[9])) %>%
    
    add_row(Rama.Actividad.Económica="Transporte  y  comunicaciones",
            `trabajadores`=sum(.$`trabajadores`[8]+.$`trabajadores`[10])) %>%
    
    add_row(Rama.Actividad.Económica="Establecimientos  ftnancieros",
            `trabajadores`=sum(.$`trabajadores`[11]+.$`trabajadores`[12])) %>%
    
    add_row(Rama.Actividad.Económica="Servicios",
            `trabajadores`=sum(.$`trabajadores`[13]+.$`trabajadores`[14]+.$`trabajadores`[15]+.$`trabajadores`[16]+.$`trabajadores`[17]+.$`trabajadores`[18]+.$`trabajadores`[19]+.$`trabajadores`[20]+.$`trabajadores`[21])) %>%
    
    add_row(Rama.Actividad.Económica="Actividades  no  especiftcadas",
            `trabajadores`=sum(.$`trabajadores`[22]))
  
  
  

  cuadro3_2018_trabajadores<-cuadro3_2018_trabajadores[c(24:33),]
  cuadro3_2018_huelgas     <-cuadro3_2018_huelgas[c(24:33),]
  
  cuadro3_2019_trabajadores<-cuadro3_2019_trabajadores[c(24:33),]
  cuadro3_2019_huelgas     <-cuadro3_2019_huelgas[c(24:33),]
  
  names(cuadro3_2018_trabajadores)<-c("Sector","2018")
  names(cuadro3_2018_huelgas)     <-c("Sector","2018")
  
  names(cuadro3_2019_trabajadores)<-c("Sector","2019")
  names(cuadro3_2019_huelgas)     <-c("Sector","2019")

  
  
  cuadro3<-read.xlsx("Input/Huelgas legales chile DT por sector.xlsx",sheet = 2,rows=c(1:253),cols=c(1:4),na.strings = "**")
  cuadro3<-cuadro3 %>% rename(ano=Año) %>% mutate(ano=round(ano))

  cuadro3_TC<-cuadro3 %>% 
    group_by(ano) %>%
    mutate(row = row_number()) %>% 
    select(Sector,ano,row,TC) %>% 
    tidyr::pivot_wider(names_from = ano,values_from = TC) %>% select(-c(row)) %>% add_row(Sector="Actividades  no  especiftcadas")
  
  cuadro3_huelgas<-cuadro3 %>% 
    group_by(ano) %>%
    mutate(row = row_number()) %>% 
    select(Sector,ano,row,huelgas_efectuadas) %>% 
    tidyr::pivot_wider(names_from = ano,values_from = huelgas_efectuadas) %>% select(-c(row)) %>% add_row(Sector="Actividades  no  especiftcadas")
  
  cuadro3_TC <-cuadro3_TC %>% arrange(Sector)
  cuadro3_2018_trabajadores<-cuadro3_2018_trabajadores %>% arrange(Sector)
  cuadro3_2018_trabajadores<-cuadro3_2018_trabajadores %>% select(`2018`)
  
  cuadro3_2019_trabajadores<-cuadro3_2019_trabajadores %>% arrange(Sector)
  cuadro3_2019_trabajadores<-cuadro3_2019_trabajadores %>% select(`2019`)
  
  cuadro3_TC<-cbind(cuadro3_TC,cuadro3_2018_trabajadores,cuadro3_2019_trabajadores)
  
  cuadro3_huelgas <-cuadro3_huelgas %>% arrange(Sector)
  cuadro3_2018_huelgas<-cuadro3_2018_huelgas %>% arrange(Sector)
  cuadro3_2018_huelgas<-cuadro3_2018_huelgas %>% select(`2018`)
  
  cuadro3_2019_huelgas<-cuadro3_2019_huelgas %>% arrange(Sector)
  cuadro3_2019_huelgas<-cuadro3_2019_huelgas %>% select(`2019`)
  
  cuadro3_huelgas<-cbind(cuadro3_huelgas,cuadro3_2018_huelgas,cuadro3_2019_huelgas)
  
  
  
  cuadro3_huelgas %>%  
    gather(ano,huelgas,-Sector) %>% mutate(huelgas=as.numeric(huelgas),
                                           ano=as.numeric(ano)) %>% 
    ggplot(aes(x=ano,y=huelgas,color=Sector)) + geom_line() + geom_point() + theme_bw() + 
    labs(title = "Gráfico 3. Huelgas legales por sector económico",
         x="Anos",
         y = "Huelgas",
         caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
    theme(legend.position = "bottom",plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          plot.caption = element_text(size=8)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
    scale_x_continuous(breaks = c(1990,1993,1997,2001,2005,2009,2014,2019))
  
  
  ggsave(
    plot = last_plot(),
    filename = "Output/Graficos/3. HUELGAS/grafico5.Huelgas_por_sector.png",
    device = "png",
    dpi = "retina",
    units = "cm",
    width = 33,
    height = 17
  )
  
  cuadro3_TC %>%  
    gather(ano,trabajadores,-Sector) %>% mutate(trabajadores=as.numeric(trabajadores),
                                           ano=as.numeric(ano)) %>% 
    ggplot(aes(x=ano,y=trabajadores,color=Sector)) + geom_line() + geom_point() + theme_bw() + 
    labs(title = "Gráfico 3. Trabajadores comprometidos en huelgas legales por sector económico",
         x="Anos",
         y = "trabajadores",
         caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
    theme(legend.position = "bottom",plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          plot.caption = element_text(size=8)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
    scale_x_continuous(breaks = c(1990,1993,1997,2001,2005,2009,2014,2019))
  
  ggsave(
    plot = last_plot(),
    filename = "Output/Graficos/3. HUELGAS/grafico6.TC_por_sector.png",
    device = "png",
    dpi = "retina",
    units = "cm",
    width = 33,
    height = 17
  )
  
  cuadro3_TC %>% filter(Sector!="Actividades  no  especiftcadas") %>% 
    gather(ano,trabajadores,-Sector) %>% mutate(trabajadores=as.numeric(trabajadores),
                                                ano=as.numeric(ano)) %>% 
    ggplot(aes(x=ano,y=trabajadores,color=Sector)) + geom_line() + geom_point() + theme_bw() + facet_wrap(~Sector) + 
    labs(title = "Gráfico 3. Trabajadores comprometidos en huelgas legales por sector económico",
         x="Anos",
         y = "trabajadores",
         caption = "Fuente: Compendio estadístico Dirección del Trabajo") + 
    theme(legend.position = "bottom",plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          plot.caption = element_text(size=8)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
    scale_x_continuous(breaks = c(1990,2005,2019))
  
  ggsave(
    plot = last_plot(),
    filename = "Output/Graficos/3. HUELGAS/grafico7.TC_por_sector_facet.png",
    device = "png",
    dpi = "retina",
    units = "cm",
    width = 33,
    height = 17
  )


#### Guardar tablas ####
library(writexl)
write_xlsx(cuadro1,"Output/Cuadros/3. HUELGAS/cuadro1.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro2,"Output/Cuadros/3. HUELGAS/cuadro2.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro3_TC,"Output/Cuadros/3. HUELGAS/cuadro3_TC.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro3_huelgas,"Output/Cuadros/3. HUELGAS/cuadro3_huelgas.xlsx", col_names = TRUE,format_headers = TRUE)
