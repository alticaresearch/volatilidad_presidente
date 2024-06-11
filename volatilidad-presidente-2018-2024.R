setwd("~/volatilidad_secciones-main")
###----------------
library(pacman)
p_load(purrr, dplyr,tidyverse, haven, tibble, sjlabelled,janitor, 
       readr, ggplot2, readxl, openxlsx, foreign, esaps)
#-------------------------------------------
pres_2018 <- read_xlsx("presidencia.xlsx")
pres_2024 <- read_xlsx("presidente-2024.xlsx")

names(pres_2018)
##########Limpieza de datos
#--Datos 2018:
#--Agregamos a nivel sección
pres_2018 <- pres_2018|>group_by(NOMBRE_ESTADO, SECCION)|>
  summarize(PAN=sum(PAN,na.rm = T),
            PRI=sum(PRI,na.rm = T),
            PRD=sum(PRD,na.rm = T),
            PVEM=sum(PVEM,na.rm = T),
            PT=sum(PT,na.rm = T),
            MC=sum(MC,na.rm = T),
            PANAL=sum(PANAL,na.rm = T),
            MORENA=sum(MORENA,na.rm = T),
            PES=sum(PES,na.rm = T),
            PRI_PVEM_NA=sum(PRI_PVEM_NA,na.rm=T),
            PRI_PVEM=sum(PRI_PVEM,na.rm = T),
            PRI_NA=sum(PRI_NA,na.rm = T),
            PVEM_NA=sum(PVEM_NA,na.rm = T),
            PAN_PRD_MC=sum(PAN_PRD_MC,na.rm = T),
            PAN_PRD=sum(PAN_PRD,na.rm = T),
            PAN_MC=sum(PAN_MC,na.rm = T),
            PRD_MC=sum(PRD_MC,na.rm = T),
            PT_MORENA_PES=sum(PT_MORENA,na.rm = T),
            PT_MORENA=sum(PT_MORENA,na.rm = T),
            MORENA_PES=sum(MORENA_PES,na.rm = T),
            PT_PES=sum(PT_PES,na.rm = T),
            CAND_IND_01=sum(CAND_IND_01,na.rm = T),
            CAND_IND_02=sum(CAND_IND_02,na.rm = T),
            CNR=sum(CNR,na.rm = T),
            VN=sum(VN,na.rm = T),
            TOTAL_VOTOS_CALCULADOS=sum(TOTAL_VOTOS_CALCULADOS,na.rm = T),.groups = "drop")

##Creamos variables de coaliciones y votacion efectiva
pres_2018 <- pres_2018|>mutate(C_1=1,##Coalicion Ricardo Anaya
                               C_2=1,##Coalicion J.A. Meade
                               C_3=1)#Coalicion AMLO

pres_2018 <- pres_2018|>mutate(TOTAL_VOTO_EFECTIVO=TOTAL_VOTOS_CALCULADOS-(VN+CNR))
names(pres_2018)
#--------
pres_2018 <- pres_2018|>mutate(CIND=CAND_IND_01+CAND_IND_02)
pres_2018 <- pres_2018|>select(1:23,26:33)
#----------------
names(pres_2024)

pres_2024 <- pres_2024|>group_by(ENTIDAD,SECCION)|>
  summarize(PAN=sum(PAN,na.rm = T),
            PRI=sum(PRI,na.rm = T),
            PRD=sum(PRD,na.rm = T),
            PVEM=sum(PVEM,na.rm = T),
            PT=sum(PT,na.rm = T),
            MC=sum(MC,na.rm = T),
            MORENA=sum(MORENA,na.rm = T),
            PAN_PRI_PRD=sum(PAN_PRI_PRD,na.rm = T),
            PAN_PRI=sum(PAN_PRI,na.rm = T),
            PAN_PRD=sum(PAN_PRD,na.rm = T),
            PRI_PRD=sum(PRI_PRD,na.rm = T),
            PVEM_PT_MORENA=sum(PVEM_PT_MORENA,na.rm = T),
            PVEM_PT=sum(PVEM_PT,na.rm = T),
            PVEM_MORENA=sum(PVEM_MORENA,na.rm = T),
            PT_MORENA=sum(PT_MORENA,na.rm = T),
            CNR=sum(CNR,na.rm = T),
            VN=sum(VN,na.rm = T),
            TOTAL_VOTOS_CALCULADO=sum(TOTAL_VOTOS_CALCULADOS,na.rm = T),
            LISTA_NOMINAL=sum(LISTA_NOMINAL,na.rm = T),.groups = "drop")

#---
pres_2024 <- pres_2024|>mutate(C_1=1,#Coalicion Xóchitl Gálvez
                               C_2=1)#Coalición Claudia Sheinbaum

pres_2024 <- pres_2024|>mutate(TOTAL_VOTO_EFECTIVO=TOTAL_VOTOS_CALCULADO-(VN+CNR))

#--Función que desagrega el voto x coalición 
voto_coalicion <- function(df, col1,col2 ,col3,col4,col5, col6,col7, col8){ 
  
  #Para cada sección dentro de la base de datos cuando la exista alianza en el distrito 
  for (i in 1:nrow(df)) {
    if (df[i,col1] == 1) {
      
      #-Calculo los votos enteros y el resto de cada división para las diferentes combinaciones de la coalición  
      #-Votos para el partido 1
      votos_1 <- df[i,col5]%/%3 + df[i,col6]%/%2 +df[i,col7]%/%2
      resto_1 <- df[i,col5]%%3 + df[i,col6]%%2 +df[i,col7]%%2
      #-Votos para el partido 2
      votos_2 <- df[i,col5]%/%3 + df[i,col6]%/%2  + df[i,col8]%/%2
      resto_2 <- df[i,col5]%%3 + df[i,col6]%%2  + df[i,col8]%%2
      #-Votos para el partido 3
      votos_3 <- df[i,col5]%/%3  +df[i,col7]%/%2 + df[i,col8]%/%2
      resto_3 <- df[i,col5]%%3  +df[i,col7]%%2 + df[i,col8]%%2
      
      #Asigno los votos al partido 1 según los posibles supuestos, si hay empate en votos entre los tres al partido 1 le asigno el resto
      df[i,col2] <- case_when(
        df[i,col2]>df[i,col3]&df[i,col2]>df[i,col4]~df[i,col2]+votos_1+resto_1,
        df[i,col2]<df[i,col3]|df[i,col2]<df[i,col4]~df[i,col2]+votos_1,
        df[i,col2]==df[i,col3]&df[i,col2]==df[i,col4]&df[i,col3]==df[i,col4]~df[i,col2]+votos_1+resto_1,
        TRUE~NA)
      #Asigno los votos al partido 2 según los posibles supuestos
      df[i,col3] <- case_when(
        df[i,col3]>df[i,col2]&df[i,col3]>df[i,col4]~df[i,col3]+votos_2+resto_2,
        df[i,col3]<df[i,col2]|df[i,col3]<df[i,col4]~df[i,col3]+votos_2,
        TRUE~NA)
      #Asigno los votos al partido 3 según los posibles supuestos
      df[i,col4] <- case_when(
        df[i,col4]>df[i,col2]&df[i,col4]>df[i,col3]~df[i,col4]+votos_3+resto_3,
        df[i,col4]<df[i,col2]|df[i,col4]<df[i,col3]~df[i,col4]+votos_3,
        TRUE~NA)
      
    }
  }
  
  return(df)  
}

#---------------------------- 
pres_2018
pres_2018 <- voto_coalicion(pres_2018,"C_1","PAN","PRD","MC","PAN_PRD_MC","PAN_PRD","PAN_MC","PRD_MC")#Coalición Al Frente por México
pres_2018 <- voto_coalicion(pres_2018,"C_2","PRI","PVEM","PANAL","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA")#Coalición Todos por México
pres_2018 <- voto_coalicion(pres_2018,"C_3","MORENA","PT","PES","PT_MORENA_PES","PT_MORENA","MORENA_PES","PT_PES")#Coalición Juntos Haremos Historia

pres_2024
pres_2024 <- voto_coalicion(pres_2024,"C_1","PAN","PRI","PRD","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD")#Coalición Al Frente por México
pres_2024 <- voto_coalicion(pres_2024,"C_2","MORENA","PVEM","PT","PVEM_PT_MORENA","PVEM_PT","PVEM_MORENA","PT_MORENA")#Coalición Todos por México

#-----------------------------------------
write.xlsx(pres_2018, "pres-2018-voto sin coalicion.xlsx")
write.xlsx(pres_2024, "pres-2024-voto sin coalicion.xlsx")
#-----------------
colnames(pres_2018)
colnames(pres_2024)

#--Seleccionamos únicamente las columnas de los partidos
desgob_2018 <- pres_2018|>select(NOMBRE_ESTADO,SECCION,
                                 PAN,PRI,PRD,PVEM,PT,MC,PANAL,MORENA,PES,CIND,CNR,VN,
                                 TOTAL_VOTOS_CALCULADOS,TOTAL_VOTO_EFECTIVO)



desgob_2024 <- pres_2024|>select(ENTIDAD,SECCION,PAN,PRI,PRD,PVEM,PT,MC,MORENA,
                                 CNR,VN,TOTAL_VOTOS_CALCULADO,
                                 TOTAL_VOTO_EFECTIVO)
#--------------
colnames(desgob_2018)
colnames(desgob_2024)

#---Calculamos porcentaje efectivo solo para partidos
desgob_2018[3:12] <- desgob_2018[3:12]*100/desgob_2018$TOTAL_VOTO_EFECTIVO
desgob_2024[3:9] <- desgob_2024[3:9]*100/desgob_2024$TOTAL_VOTO_EFECTIVO
############------Ahora calculamos la volatilidad tipo A : partidos nuevos y partidos que desaparecieron 
exit <- setdiff(names(desgob_2018[3:12]),names(desgob_2024[3:9]))
enter <- setdiff(names(desgob_2024[3:9]),names(desgob_2018[3:12]))


desgob_2018 <- desgob_2018|>rename(ENTIDAD="NOMBRE_ESTADO")
#---
colnames(desgob_2024)
volatilidad_A <-  desgob_2018|>select(ENTIDAD,SECCION,all_of(exit))
volatilidad_A
volatilidad_A <- left_join(volatilidad_A,select(desgob_2024,ENTIDAD,SECCION,all_of(enter)),by = c("ENTIDAD","SECCION"))
volatilidad_A[is.na(volatilidad_A)] <- 0
volatilidad_A$VOLATILIDAD_A <- apply(volatilidad_A[3:5],1,sum)
volatilidad_A$VOLATILIDAD_A <- volatilidad_A$VOLATILIDAD_A/2

hist(volatilidad_A$VOLATILIDAD_A)
mean(volatilidad_A$VOLATILIDAD_A)
sd(volatilidad_A$VOLATILIDAD_A)

#------Volatilidad B: partidos estables
###Identificacmos a los partidos que no  cambiaron entre eleciones
stable <- intersect(names(desgob_2018[3:12]),names(desgob_2024[3:9]))
stable

names(desgob_2018)
#---Elección tiempo 0
a <- desgob_2018|>select(ENTIDAD,SECCION,all_of(stable)&!starts_with("TOTAL"))
colnames(a)
a <- a|>gather(PARTIDO,VOTO_2018,3:9)
a
#---Elección tiempo 1
b <- desgob_2024|>select(ENTIDAD,SECCION,all_of(stable)&!starts_with("TOTAL"))
colnames(b)
b <- b|>gather(PARTIDO,VOTO_2024,3:9)
b
#-------
volatilidad_B <- left_join(b, a, by = c("ENTIDAD","SECCION","PARTIDO"))
volatilidad_B[is.na(volatilidad_B)] <- 0
volatilidad_B <- volatilidad_B|>mutate(VOLATILIDAD_B=VOTO_2024-VOTO_2018)

colnames(volatilidad_B)
volatilidad_B <- volatilidad_B|>select(!starts_with("VOTO"))
volatilidad_B <- volatilidad_B|>spread(PARTIDO,VOLATILIDAD_B)
write.xlsx(volatilidad_B, "volatilidad_b_secciones-pres.xlsx")
#-------------------
volatilidad_B
volatilidad_B[3:9] <- apply(volatilidad_B[3:9],2,abs)
volatilidad_B$VOLATILIDAD_B <- apply(volatilidad_B[3:9],1,sum)
volatilidad_B$VOLATILIDAD_B <- volatilidad_B$VOLATILIDAD_B/2

hist(volatilidad_B$VOLATILIDAD_B)
mean(volatilidad_B$VOLATILIDAD_B)
sd(volatilidad_B$VOLATILIDAD_B)
rm(a,b)

#--------------
colnames(volatilidad_total)
volatilidad_total <- inner_join(volatilidad_A,volatilidad_B,by=c("ENTIDAD","SECCION"))
volatilidad_total[is.na(volatilidad_total)]<-0

volatilidad_total <- volatilidad_total|>mutate(VOLATILIDAD_TOTAL=VOLATILIDAD_A+VOLATILIDAD_B)
volatilidad_total
hist(volatilidad_total$VOLATILIDAD_TOTAL)
mean(volatilidad_total$VOLATILIDAD_TOTAL)
sd(volatilidad_total$VOLATILIDAD_TOTAL)
#-----
mean(volatilidad_total$VOLATILIDAD_A)
sd(volatilidad_total$VOLATILIDAD_A)

mean(volatilidad_total$VOLATILIDAD_B)
sd(volatilidad_total$VOLATILIDAD_B)
#----------------------------------
write.xlsx(volatilidad_total, "volatilidad_total_secciones-pres.xlsx")