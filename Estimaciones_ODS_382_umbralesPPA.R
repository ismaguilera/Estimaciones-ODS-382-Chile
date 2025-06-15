#### CONTACTO ####
#   Ismael Aguilera
#   Profesional Depto. Economía de la Salud, DIPLAS, SSP, Minsal
#   Consultas a: ismael.aguilera@minsal.cl o al anexo: 240484
#   Última actualización: 15/Junio/2025

#### Paquetes ####
list_of_packages <- c("data.table","dplyr", "scales", "readxl", "writexl","laeken","jsonlite","installr")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}
lapply(list_of_packages, require, character.only = TRUE)
options(timeout = max(10000, getOption("timeout")))

#### Programa para descomprimir archivos a descargar ####
# instalar 7zip para descomprimir desde R los datos de las EPF V y VI en formato rar.
# También, es posible crear una carpeta con nombre "Datos" desde la actual ubicación,
# luego descargar mediante navegador y descomprimir con otro programa.
# En dicho caso, comentar líneas de código
# Es importante mantener los nombres de los archivos descomprimidos a utilizar en dicha carpeta

ruta_7zip<-"C:/Program Files/7-Zip/7Z.exe"
if(!file.exists(ruta_7zip)){
  install.7zip()
}
unzip<-shQuote(ruta_7zip)


#### Funciones ####
#Lectura bases con formato estándar (EPF9, EPF8 y EPF7)
BASE_ODS382_FORMATO<- function(url_gastos,url_personas,var_hogar,var_gasto){
  EPF_GASTOS<-fread(url_gastos)
  colnames(EPF_GASTOS)<-toupper(colnames(EPF_GASTOS))
  EPF_GASTOS[,GASTO2:=as.numeric(gsub(",", ".", GASTO))]
  GASTOS_HOGAR_EPF<-EPF_GASTOS[,.(.N,GT=sum(GASTO2,na.rm=TRUE),GS=sum(GASTO2[D=="6"],na.rm=TRUE)),by=var_hogar]
  
  EPF_PERSONAS<-fread(url_personas)
  if(any(like(colnames(EPF_PERSONAS),"sprincipal",ignore.case = TRUE))){
    colnames(EPF_PERSONAS)[colnames(EPF_PERSONAS)=="sprincipal"]<-"JHOGAR"
  }
  colnames(EPF_PERSONAS)<-toupper(colnames(EPF_PERSONAS))
  EPF_PERSONAS[,FE2:=as.numeric(gsub(",", ".", FE))]
  JEFE_HOGAR_EPF<-EPF_PERSONAS[JHOGAR=="1"]
  
  BASE_ODS382_EPF<-JEFE_HOGAR_EPF[GASTOS_HOGAR_EPF,,on=var_hogar]
  BASE_ODS382_EPF<-BASE_ODS382_EPF[!is.na(FE2)]
  
  expr<-paste0("GT:=as.numeric(gsub(',', '.', ",var_gasto,"))")
  return(BASE_ODS382_EPF[,eval(parse(text =expr)),by=var_hogar])
}


#### DATO PPA ####
PPP2024<-as.data.table(readxl::read_xlsx(path="C:/Users/ismaelaguilera/OneDrive - SUBSECRETARIA DE SALUD PUBLICA/2025/ODS 382 Consulta/Fuentes/PPPfactors20241012_onlinetool_CHL.xlsx"))

# url_PPA_2017_json<-"http://api.worldbank.org/v2/countries/CHL/indicators/PA.NUS.PRVT.PP?format=json&date=2017"
# PPA_2017<-fromJSON(url_PPA_2017_json)[[2]]$value
#PPA_2017 == 463.231413
# otro_dato_PPA_2017<- 485.313
# desde "https://databank.worldbank.org/embed/ICP-2017-Cycle/id/4add74e?inf=n&country=CHL&Series=9260000:INDIVIDUAL%20CONSUMPTION%20EXPENDITURE%20BY%20HOUSEHOLDS%20WITHOUT%20HOUSING"


#### EPF IX ####
url_epf9_gastos<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/ix-epf-(octubre-2021---septiembre-2022)/base-gastos-ix-epf-(formato-csv).csv"
url_epf9_personas<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/ix-epf-(octubre-2021---septiembre-2022)/base-personas-ix-epf-(formato-csv).csv"

BASE_ODS382_EPF9<-BASE_ODS382_FORMATO(url_epf9_gastos,url_epf9_personas,"FOLIO","GASTOT_HD_AI")
BASE_ODS382_EPF9[JHOGAR==1,sum(NPERSONAS*FE2)]

#### EPF VIII ####
url_epf8_gastos<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/viii-epf---(junio-2016---julio-2017)/base-gastos-viii-epf-(formato-csv).csv"
url_epf8_personas<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/viii-epf---(junio-2016---julio-2017)/base-personas-viii-epf-(formato-csv).csv"

BASE_ODS382_EPF8<-BASE_ODS382_FORMATO(url_epf8_gastos,url_epf8_personas,"FOLIO","GASTOT_HD_AI")
BASE_ODS382_EPF8[,sum(GS*FE2/NPERSONAS*12/365.25)]/BASE_ODS382_EPF8[,sum(FE2)]


#### EPF VII ####
url_epf7_gastos<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/vii-epf---(noviembre-2011---octubre-2012)/base-gastos-vii-epf-(formato-csv).csv"
url_epf7_personas<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/vii-epf---(noviembre-2011---octubre-2012)/base-personas-vii-epf-(formato-csv).csv"

BASE_ODS382_EPF7<-BASE_ODS382_FORMATO(url_epf7_gastos,url_epf7_personas,"FOLIO","GASTOT_FNR_AI")
BASE_ODS382_EPF7[GS>0,sum(NPERSONA*FE2)] / BASE_ODS382_EPF7[,sum(NPERSONA*FE2)]
BASE_ODS382_EPF7[,sum(GS*FE2)]/BASE_ODS382_EPF7[,sum(FE2)]

#### EPF VI ####

url_epf6 <- "https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/vi-epf---(noviembre-2006---octubre-2007)/base-de-datos-vi-epf---9-divisiones---formato-csv.rar"
#crear carpeta
dirEPF<-paste0(getwd(),"/Datos/")
dir.create(file.path(dirEPF))
#descargar web
destfile_EPF6<-paste0(dirEPF,"EPF6.rar")
download.file(url_epf6,destfile_EPF6,mode="wb")

#descomprimir en pc
epf6_archivo_gastos<-"Gasto_QIng_Nacional_Real.csv"
epf6_archivo_personas<-"Ingreso_Qing_Hogares_Nacional_Real.csv"
cmd <- paste(unzip, "x",shQuote(destfile_EPF6),"-aot -ssc",paste0("-o",shQuote(dirEPF)),epf6_archivo_gastos,epf6_archivo_personas,"-r")
system(cmd)

file.rename(paste0(dirEPF,epf6_archivo_gastos),paste0(dirEPF,"EPF6_",epf6_archivo_gastos))
EPF6_GASTOS<- fread(paste0(dirEPF,"EPF6_",epf6_archivo_gastos))
EPF6_GASTOS[,GASTO2:=as.numeric(gsub(",", ".", Gasto_Real))]
EPF6_GASTOS[,FE_G:=as.numeric(gsub(",", ".", Factor_Expansion_Anual))]
GASTOS_HOGAR_EPF6<-EPF6_GASTOS[,.(.N,GTsAI=sum(GASTO2),GS=sum(GASTO2[CodP01=="5000"]),FE_G2=mean(FE_G)),by=clave_hogar]

file.rename(paste0(dirEPF,epf6_archivo_personas),paste0(dirEPF,"EPF6_",epf6_archivo_personas))
EPF6_PERSONAS<-fread(paste0(dirEPF,"/EPF6_",epf6_archivo_personas))
EPF6_PERSONAS[,FE2:=as.numeric(gsub(",", ".", Factor_Expansion_Anual))]

BASE_ODS382_EPF6<-GASTOS_HOGAR_EPF6[EPF6_PERSONAS,,on=c(clave_hogar="Clave_hogar")]
BASE_ODS382_EPF6<-BASE_ODS382_EPF6[!is.na(FE2)]
BASE_ODS382_EPF6[,GT:=GTsAI+Arriendo_Imputado,by=clave_hogar]

BASE_ODS382_EPF6[GS>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)]
BASE_ODS382_EPF6[,sum(GS*FE2)]/BASE_ODS382_EPF6[,sum(FE2)]



#### EPF V ####
options(timeout = max(1000, getOption("timeout")))
url_epf5 <- "https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/v-epf---(agosto-1996---julio-1997)/base-de-datos-v-epf---formato-csv.rar"

#descargar web
destfile_EPF5<-paste0(dirEPF,"EPF5.rar")
download.file(url_epf5,destfile_EPF5,mode="wb")

#descomprimir en pc
epf5_archivo_gastos<-"Gasto.csv"
epf5_archivo_personas<-"Personas.csv"
epf5_archivo_fe<-"Factor_expansion.csv"
cmd <- paste(unzip, "x",shQuote(destfile_EPF5),"-aou -ssc",paste0("-o",shQuote(dirEPF)),
             epf5_archivo_gastos, epf5_archivo_personas,epf5_archivo_fe,"-r")
system(cmd)
epf5_archivo_personas<-grep("^Personas",list.files(dirEPF),value = TRUE)

file.rename(paste0(dirEPF,epf5_archivo_gastos),paste0(dirEPF,"EPF5_",epf5_archivo_gastos))
EPF5_GASTOS<- fread(paste0(dirEPF,"EPF5_",epf5_archivo_gastos))
GASTOS_HOGAR_EPF5<-EPF5_GASTOS[,.(.N,GTsAI=sum(Gasto),GS=sum(Gasto[like(Codigo_producto,"^5")])),by=Codigo_hogar]


file.rename(paste0(dirEPF,epf5_archivo_personas),paste0(dirEPF,"EPF5_",epf5_archivo_personas))
EPF5_PERSONAS<-fread(paste0(dirEPF,"EPF5_",epf5_archivo_personas[1]))
HOGARES_EPF5<-EPF5_PERSONAS[,.(max_Factor=min(Clave_factor_expansion),npersonas=.N,Arriendo_Imputado=sum(Arriendo_imputado_vivienda,na.rm=TRUE)),by=Numero_hogar]

file.rename(paste0(dirEPF,epf5_archivo_fe),paste0(dirEPF,"EPF5_",epf5_archivo_fe))
EPF5_FE<-fread(paste0(dirEPF,"EPF5_",epf5_archivo_fe),encoding = "Latin-1")
EPF5_FE[,FE2:=as.numeric(gsub(",", ".", Factor_expansion_año))]

HOGARES_EPF5<-HOGARES_EPF5[EPF5_FE[,c("Codigo_factor_expansion","FE2")],,on=c(max_Factor="Codigo_factor_expansion")]
BASE_ODS382_EPF5<-HOGARES_EPF5[GASTOS_HOGAR_EPF5,,on=c(Numero_hogar="Codigo_hogar")]
BASE_ODS382_EPF5<-BASE_ODS382_EPF5[!is.na(FE2)]
BASE_ODS382_EPF5[,GT:=GTsAI+Arriendo_Imputado,by=Numero_hogar]

BASE_ODS382_EPF5[GS>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5[,sum(npersonas*FE2)]
BASE_ODS382_EPF5[,sum(GS*FE2)]/BASE_ODS382_EPF5[,sum(FE2)]


#### Umbrales ----

# CHL_UHC<-readxl::read_xlsx(path="C:/Users/ismaelaguilera/OneDrive - SUBSECRETARIA DE SALUD PUBLICA/2025/ODS 382 Consulta/CHL_UHC financial protection indicators 2025_ES.xlsx",
#                   sheet = "Estadísticas auxiliares",
#                   range = "L16:L21",col_names = FALSE)

Umbrales<-data.table(EPF=c(5:9),PPA_215=PPP2024[Year %in% c(1996,2006,2011,2016,2021),c(6)][[1]],
                     PPA_115=PPP2024[Year %in% c(1996,2006,2011,2016,2021),c(11)][[1]])
# Umbrales<-data.table(EPF=c(5:9),u=c(1339.05346679687,
#                                     2574.34252929687,
#                                     3156.75122070312,
#                                     4895.537109375,
#                                     7411.6943359375))

BASE_ODS382_EPF9[,gs_pc:=GS/NPERSONAS*12/365.25]
BASE_ODS382_EPF9[,gtot_pc:=GT/NPERSONAS*12/365.25]
EPF9_u_115<-BASE_ODS382_EPF9[,0.5*weightedMedian(gtot_pc-gs_pc,FE2)]+Umbrales[EPF==9,PPA_115]
BASE_ODS382_EPF9[,umbral:=ifelse(gs_pc>0.4*(gtot_pc-EPF9_u_115)&gs_pc>0,1,0)]

BASE_ODS382_EPF8[,gs_pc:=GS/NPERSONAS*12/365.25]
BASE_ODS382_EPF8[,gtot_pc:=GT/NPERSONAS*12/365.25]
EPF8_u_115<-BASE_ODS382_EPF8[,0.5*weightedMedian(gtot_pc-gs_pc,FE2)]+Umbrales[EPF==8,PPA_115]
BASE_ODS382_EPF8[,umbral:=ifelse(gs_pc>0.4*(gtot_pc-EPF8_u_115)&gs_pc>0,1,0)]

BASE_ODS382_EPF7[,gs_pc:=GS/NPERSONA*12/365.25]
BASE_ODS382_EPF7[,gtot_pc:=GT/NPERSONA*12/365.25]
EPF7_u_115<-BASE_ODS382_EPF7[,0.5*weightedMedian(gtot_pc-gs_pc,FE2)]+Umbrales[EPF==7,PPA_115]
BASE_ODS382_EPF7[,umbral:=ifelse(gs_pc>0.4*(gtot_pc-EPF7_u_115)&gs_pc>0,1,0)]

BASE_ODS382_EPF6[,gs_pc:=GS/PersonasXHogar*12/365.25]
BASE_ODS382_EPF6[,gtot_pc:=GT/PersonasXHogar*12/365.25]
EPF6_u_115<-BASE_ODS382_EPF6[,0.5*weightedMedian(gtot_pc-gs_pc,FE2)]+Umbrales[EPF==6,PPA_115]
BASE_ODS382_EPF6[,umbral:=ifelse(gs_pc>0.4*(gtot_pc-EPF6_u_115)&gs_pc>0,1,0)]

BASE_ODS382_EPF5[,gs_pc:=GS/npersonas*12/365.25]
BASE_ODS382_EPF5[,gtot_pc:=GT/npersonas*12/365.25]
EPF5_u_115<-BASE_ODS382_EPF5[,0.5*weightedMedian(gtot_pc-gs_pc,FE2)]+Umbrales[EPF==5,PPA_115]
BASE_ODS382_EPF5[,umbral:=ifelse(gs_pc>0.4*(gtot_pc-EPF5_u_115)&gs_pc>0,1,0)]


## -- Quintilización --------------------------------------------------------

quintilizar <- function(x){
  x[,ing_pc:=as.numeric(ING_DISP_HOG_HD_PC_AI)]
  # Se genera un vector con la variable de interés
  variable_orden <- "gtot_pc"       # Ingreso del hogar sin ai
  variable_reemplazo <- "ing_pc" 
  base_personas_quin <- x %>%
    # El id_missing se construye con el ingreso del hogar correspondiente 
    mutate(id_missing = if_else(ing_pc <= 0, 1, 0)) %>%
    # Se utiliza el gasto total del hogar cuando id missing es 1
    mutate(vector_orden = case_when(id_missing == 1 ~ !!sym(variable_reemplazo), 
                                    TRUE ~ !!sym(variable_orden)))
  # Ordenamiento de los hogares
  quin_pc_cai <- base_personas_quin %>%
    arrange(vector_orden, !!sym(variable_orden), !!sym(variable_reemplazo), FOLIO)
  # Cálculo de la suma acumulada (fe_acum) y 
  # Participación acumulada del FE (part_acum)
  quin_pc_cai <- quin_pc_cai %>% mutate(fe_acum = cumsum(FE2*NPERSONAS),
                                        part_acum = fe_acum/max(fe_acum))
  # Identificador del FE hogares en el límite de quintiles (etiqueta_hogar) y 
  # distancia de la suma acumulada respecto al máximo (dist) 
  quin_pc_cai <- quin_pc_cai %>%
    mutate(etiqueta_hogar = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ 1,
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ 1,
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ 1,
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ 1,
      T ~ 0)) %>%
    mutate(distancia = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ fe_acum - (20/100*max(fe_acum)),
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ fe_acum - (40/100*max(fe_acum)), 
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ fe_acum - (60/100*max(fe_acum)),
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ fe_acum - (80/100*max(fe_acum))
    ))
  # fe - distancia de la suma acumulada respecto al máximo (distancia)
  quin_pc_cai <- quin_pc_cai %>% mutate(distancia_2 = FE2*NPERSONAS - distancia,
                                        id = row_number())
  # Expandir observaciones que generan la división
  hogares_limite <- quin_pc_cai %>%
    filter(etiqueta_hogar == 1) %>%
    pull(id)
  quin_pc_cai <- quin_pc_cai %>%
    bind_rows(quin_pc_cai %>%
                filter(id %in% hogares_limite)) %>%
    arrange(id)
  # Reemplazo de pesos para las observaciones divisorias (1)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 1 & !is.na(distancia_2), distancia_2, FE2*NPERSONAS)) %>%
    ungroup()
  # Reemplazo de pesos para las observaciones divisorias (2)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 2, distancia, FE_QUINTIL_PC_CAI)) %>%
    ungroup()
  # Suma acumulada y frecuencia acumulada de nuevos freq. w.
  quin_pc_cai <- quin_pc_cai %>% mutate(suma = cumsum(FE_QUINTIL_PC_CAI),
                                        pct_acum = suma / max(suma))
  # Asignación quintiles
  quin_pc_cai <- quin_pc_cai %>%
    mutate(QUINTIL_PC_CAI = case_when(
      pct_acum <= 0.2000000001 ~ 1,
      pct_acum <= 0.4000000001 & pct_acum > 0.2000000001 ~ 2,
      pct_acum <= 0.6000000001 & pct_acum > 0.4000000001 ~ 3,
      pct_acum <= 0.8000000001 & pct_acum > 0.6000000001 ~ 4,
      pct_acum <= 1.000000001 & pct_acum > 0.8000000001 ~ 5)
    )
  EPF_QUIN<-as.data.table(quin_pc_cai)
  EPF_QUIN<-setDT(unlist(list(EPF_QUIN[umbral>0,.(pob_u=sum(NPERSONAS*FE2)),by=QUINTIL_PC_CAI],EPF_QUIN[,.(pobt=sum(NPERSONAS*FE2)),by=QUINTIL_PC_CAI][,c(2)]),recursive = FALSE),
        check.names = TRUE)
  EPF_QUIN[,u_q:=pob_u/pobt]
  return(EPF_QUIN)
}

quintilizar_epf7 <- function(x){
  x[,ing_pc:=as.numeric(INGDHOG_HD_AI)/NPERSONA]
  # Se genera un vector con la variable de interés
  variable_orden <- "gtot_pc"       # Ingreso del hogar sin ai
  variable_reemplazo <- "ing_pc" 
  base_personas_quin <- x %>%
    # El id_missing se construye con el ingreso del hogar correspondiente 
    mutate(id_missing = if_else(ing_pc <= 0, 1, 0)) %>%
    # Se utiliza el gasto total del hogar cuando id missing es 1
    mutate(vector_orden = case_when(id_missing == 1 ~ !!sym(variable_reemplazo), 
                                    TRUE ~ !!sym(variable_orden)))
  # Ordenamiento de los hogares
  quin_pc_cai <- base_personas_quin %>%
    arrange(vector_orden, !!sym(variable_orden), !!sym(variable_reemplazo), FOLIO)
  # Cálculo de la suma acumulada (fe_acum) y 
  # Participación acumulada del FE (part_acum)
  quin_pc_cai <- quin_pc_cai %>% mutate(fe_acum = cumsum(FE2*NPERSONA),
                                        part_acum = fe_acum/max(fe_acum))
  # Identificador del FE hogares en el límite de quintiles (etiqueta_hogar) y 
  # distancia de la suma acumulada respecto al máximo (dist) 
  quin_pc_cai <- quin_pc_cai %>%
    mutate(etiqueta_hogar = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ 1,
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ 1,
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ 1,
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ 1,
      T ~ 0)) %>%
    mutate(distancia = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ fe_acum - (20/100*max(fe_acum)),
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ fe_acum - (40/100*max(fe_acum)), 
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ fe_acum - (60/100*max(fe_acum)),
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ fe_acum - (80/100*max(fe_acum))
    ))
  # fe - distancia de la suma acumulada respecto al máximo (distancia)
  quin_pc_cai <- quin_pc_cai %>% mutate(distancia_2 = FE2*NPERSONA - distancia,
                                        id = row_number())
  # Expandir observaciones que generan la división
  hogares_limite <- quin_pc_cai %>%
    filter(etiqueta_hogar == 1) %>%
    pull(id)
  quin_pc_cai <- quin_pc_cai %>%
    bind_rows(quin_pc_cai %>%
                filter(id %in% hogares_limite)) %>%
    arrange(id)
  # Reemplazo de pesos para las observaciones divisorias (1)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 1 & !is.na(distancia_2), distancia_2, FE2*NPERSONA)) %>%
    ungroup()
  # Reemplazo de pesos para las observaciones divisorias (2)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 2, distancia, FE_QUINTIL_PC_CAI)) %>%
    ungroup()
  # Suma acumulada y frecuencia acumulada de nuevos freq. w.
  quin_pc_cai <- quin_pc_cai %>% mutate(suma = cumsum(FE_QUINTIL_PC_CAI),
                                        pct_acum = suma / max(suma))
  # Asignación quintiles
  quin_pc_cai <- quin_pc_cai %>%
    mutate(QUINTIL_PC_CAI = case_when(
      pct_acum <= 0.2000000001 ~ 1,
      pct_acum <= 0.4000000001 & pct_acum > 0.2000000001 ~ 2,
      pct_acum <= 0.6000000001 & pct_acum > 0.4000000001 ~ 3,
      pct_acum <= 0.8000000001 & pct_acum > 0.6000000001 ~ 4,
      pct_acum <= 1.000000001 & pct_acum > 0.8000000001 ~ 5)
    )
  EPF_QUIN<-as.data.table(quin_pc_cai)
  EPF_QUIN<-setDT(unlist(list(EPF_QUIN[umbral>0,.(pob_u=sum(NPERSONA*FE2)),by=QUINTIL_PC_CAI],EPF_QUIN[,.(pobt=sum(NPERSONA*FE2)),by=QUINTIL_PC_CAI][,c(2)]),recursive = FALSE),
                  check.names = TRUE)
  EPF_QUIN[,u_q:=pob_u/pobt]
  return(EPF_QUIN)
}

quintilizar_epf6 <- function(x){
  x[,ing_pc:=as.numeric(Ingreso_Total)/PersonasXHogar]
  # Se genera un vector con la variable de interés
  variable_orden <- "gtot_pc"       # Ingreso del hogar sin ai
  variable_reemplazo <- "ing_pc" 
  base_personas_quin <- x %>%
    # El id_missing se construye con el ingreso del hogar correspondiente 
    mutate(id_missing = if_else(ing_pc <= 0, 1, 0)) %>%
    # Se utiliza el gasto total del hogar cuando id missing es 1
    mutate(vector_orden = case_when(id_missing == 1 ~ !!sym(variable_reemplazo), 
                                    TRUE ~ !!sym(variable_orden)))
  # Ordenamiento de los hogares
  quin_pc_cai <- base_personas_quin %>%
    arrange(vector_orden, !!sym(variable_orden), !!sym(variable_reemplazo), clave_hogar)
  # Cálculo de la suma acumulada (fe_acum) y 
  # Participación acumulada del FE (part_acum)
  quin_pc_cai <- quin_pc_cai %>% mutate(fe_acum = cumsum(FE2*PersonasXHogar),
                                        part_acum = fe_acum/max(fe_acum))
  # Identificador del FE hogares en el límite de quintiles (etiqueta_hogar) y 
  # distancia de la suma acumulada respecto al máximo (dist) 
  quin_pc_cai <- quin_pc_cai %>%
    mutate(etiqueta_hogar = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ 1,
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ 1,
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ 1,
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ 1,
      T ~ 0)) %>%
    mutate(distancia = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ fe_acum - (20/100*max(fe_acum)),
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ fe_acum - (40/100*max(fe_acum)), 
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ fe_acum - (60/100*max(fe_acum)),
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ fe_acum - (80/100*max(fe_acum))
    ))
  # fe - distancia de la suma acumulada respecto al máximo (distancia)
  quin_pc_cai <- quin_pc_cai %>% mutate(distancia_2 = FE2*PersonasXHogar - distancia,
                                        id = row_number())
  # Expandir observaciones que generan la división
  hogares_limite <- quin_pc_cai %>%
    filter(etiqueta_hogar == 1) %>%
    pull(id)
  quin_pc_cai <- quin_pc_cai %>%
    bind_rows(quin_pc_cai %>%
                filter(id %in% hogares_limite)) %>%
    arrange(id)
  # Reemplazo de pesos para las observaciones divisorias (1)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 1 & !is.na(distancia_2), distancia_2, FE2*PersonasXHogar)) %>%
    ungroup()
  # Reemplazo de pesos para las observaciones divisorias (2)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 2, distancia, FE_QUINTIL_PC_CAI)) %>%
    ungroup()
  # Suma acumulada y frecuencia acumulada de nuevos freq. w.
  quin_pc_cai <- quin_pc_cai %>% mutate(suma = cumsum(FE_QUINTIL_PC_CAI),
                                        pct_acum = suma / max(suma))
  # Asignación quintiles
  quin_pc_cai <- quin_pc_cai %>%
    mutate(QUINTIL_PC_CAI = case_when(
      pct_acum <= 0.2000000001 ~ 1,
      pct_acum <= 0.4000000001 & pct_acum > 0.2000000001 ~ 2,
      pct_acum <= 0.6000000001 & pct_acum > 0.4000000001 ~ 3,
      pct_acum <= 0.8000000001 & pct_acum > 0.6000000001 ~ 4,
      pct_acum <= 1.000000001 & pct_acum > 0.8000000001 ~ 5)
    )
  EPF_QUIN<-as.data.table(quin_pc_cai)
  EPF_QUIN<-setDT(unlist(list(EPF_QUIN[umbral>0,.(pob_u=sum(PersonasXHogar*FE2)),by=QUINTIL_PC_CAI],EPF_QUIN[,.(pobt=sum(PersonasXHogar*FE2)),by=QUINTIL_PC_CAI][,c(2)]),recursive = FALSE),
                  check.names = TRUE)
  EPF_QUIN[,u_q:=pob_u/pobt]
  return(EPF_QUIN)
}


quintilizar_epf5 <- function(x){
  # Se genera un vector con la variable de interés
  variable_orden <- "gtot_pc"       # Ingreso del hogar sin ai
  variable_reemplazo <- "gtot_pc" 
  base_personas_quin <- x %>%
    # El id_missing se construye con el ingreso del hogar correspondiente 
    mutate(id_missing = if_else(gtot_pc <= 0, 1, 0)) %>%
    # Se utiliza el gasto total del hogar cuando id missing es 1
    mutate(vector_orden = case_when(id_missing == 1 ~ !!sym(variable_reemplazo), 
                                    TRUE ~ !!sym(variable_orden)))
  # Ordenamiento de los hogares
  quin_pc_cai <- base_personas_quin %>%
    arrange(vector_orden, !!sym(variable_orden), !!sym(variable_reemplazo), Numero_hogar)
  # Cálculo de la suma acumulada (fe_acum) y 
  # Participación acumulada del FE (part_acum)
  quin_pc_cai <- quin_pc_cai %>% mutate(fe_acum = cumsum(FE2*npersonas),
                                        part_acum = fe_acum/max(fe_acum))
  # Identificador del FE hogares en el límite de quintiles (etiqueta_hogar) y 
  # distancia de la suma acumulada respecto al máximo (dist) 
  quin_pc_cai <- quin_pc_cai %>%
    mutate(etiqueta_hogar = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ 1,
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ 1,
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ 1,
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ 1,
      T ~ 0)) %>%
    mutate(distancia = case_when(
      part_acum >= 20/100 & lag(part_acum) < 20/100 ~ fe_acum - (20/100*max(fe_acum)),
      part_acum >= 40/100 & lag(part_acum) < 40/100 ~ fe_acum - (40/100*max(fe_acum)), 
      part_acum >= 60/100 & lag(part_acum) < 60/100 ~ fe_acum - (60/100*max(fe_acum)),
      part_acum >= 80/100 & lag(part_acum) < 80/100 ~ fe_acum - (80/100*max(fe_acum))
    ))
  # fe - distancia de la suma acumulada respecto al máximo (distancia)
  quin_pc_cai <- quin_pc_cai %>% mutate(distancia_2 = FE2*npersonas - distancia,
                                        id = row_number())
  # Expandir observaciones que generan la división
  hogares_limite <- quin_pc_cai %>%
    filter(etiqueta_hogar == 1) %>%
    pull(id)
  quin_pc_cai <- quin_pc_cai %>%
    bind_rows(quin_pc_cai %>%
                filter(id %in% hogares_limite)) %>%
    arrange(id)
  # Reemplazo de pesos para las observaciones divisorias (1)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 1 & !is.na(distancia_2), distancia_2, FE2*npersonas)) %>%
    ungroup()
  # Reemplazo de pesos para las observaciones divisorias (2)
  quin_pc_cai <- quin_pc_cai %>%
    group_by(id) %>%
    mutate(FE_QUINTIL_PC_CAI = ifelse(row_number() == 2, distancia, FE_QUINTIL_PC_CAI)) %>%
    ungroup()
  # Suma acumulada y frecuencia acumulada de nuevos freq. w.
  quin_pc_cai <- quin_pc_cai %>% mutate(suma = cumsum(FE_QUINTIL_PC_CAI),
                                        pct_acum = suma / max(suma))
  # Asignación quintiles
  quin_pc_cai <- quin_pc_cai %>%
    mutate(QUINTIL_PC_CAI = case_when(
      pct_acum <= 0.2000000001 ~ 1,
      pct_acum <= 0.4000000001 & pct_acum > 0.2000000001 ~ 2,
      pct_acum <= 0.6000000001 & pct_acum > 0.4000000001 ~ 3,
      pct_acum <= 0.8000000001 & pct_acum > 0.6000000001 ~ 4,
      pct_acum <= 1.000000001 & pct_acum > 0.8000000001 ~ 5)
    )
  EPF_QUIN<-as.data.table(quin_pc_cai)
  EPF_QUIN<-setDT(unlist(list(EPF_QUIN[umbral>0,.(pob_u=sum(npersonas*FE2)),by=QUINTIL_PC_CAI],EPF_QUIN[,.(pobt=sum(npersonas*FE2)),by=QUINTIL_PC_CAI][,c(2)]),recursive = FALSE),
                  check.names = TRUE)
  EPF_QUIN[,u_q:=pob_u/pobt]
  return(EPF_QUIN)
}

EPF9_QUIN<-quintilizar(BASE_ODS382_EPF9)
EPF9_QUIN[,mean(u_q)]
EPF8_QUIN<-quintilizar(BASE_ODS382_EPF8)
EPF8_QUIN[,mean(u_q)]
EPF7_QUIN<-quintilizar_epf7(BASE_ODS382_EPF7)
EPF7_QUIN[,mean(u_q)]
EPF6_QUIN<-quintilizar_epf6(BASE_ODS382_EPF6)
EPF6_QUIN[,mean(u_q)]
EPF5_QUIN<-quintilizar_epf5(BASE_ODS382_EPF5)
EPF5_QUIN[,mean(u_q)]



BASE_ODS382_EPF5_SEX<-EPF5_PERSONAS[Parentesco==1,.(Numero_hogar,Sexo,Parentesco,Grupo_edad)][BASE_ODS382_EPF5,,on=c("Numero_hogar")]

epf6_archivo_personas_2<-"PERSONAS.csv"
cmd <- paste(unzip, "x",shQuote(destfile_EPF6),"-aot -ssc",paste0("-o",shQuote(dirEPF)),epf6_archivo_personas_2,"-r")
system(cmd)

file.rename(paste0(dirEPF,epf6_archivo_personas_2),paste0(dirEPF,"EPF6_",epf6_archivo_personas_2))
EPF6_PERSONAS2<-fread(paste0(dirEPF,"/EPF6_",epf6_archivo_personas_2))
BASE_ODS382_EPF6_SEX<-EPF6_PERSONAS2[Parentesco==1,.(Clave_Hogar,Sexo,Parentesco,Grupo_edad)][BASE_ODS382_EPF6,,on=c("Clave_Hogar"="clave_hogar")]



#### Tipo de Hogar ----
EPF9_PERSONAS<-fread(url_epf9_personas)
#Los adultos tienen entre 20 y 59 años.
#Los niños son los menores de 10 años.
#Los adolescentes tienen entre 10 y 19 años.
#Las personas mayores (o adultos mayores) son las que tienen 60 años o más.

EPF9_PERSONAS[, c("hay_adultos","hay_adolescentes","hay_ninos","hay_adultosmayores") :=
                list(as.integer(any(edad >= 20 & edad <= 59)),
                     as.integer(any(edad >= 10 & edad <= 19)),
                     as.integer(any(edad < 10)),
                     as.integer(any(edad >= 60)))
              , by = folio]
#a. Solo adultos
EPF9_PERSONAS[,solo_adultos:=hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*(1-hay_adultosmayores)]
#b. Adultos con niños y adolescentes
EPF9_PERSONAS[,solo_adultos_y_nna:=hay_adultos*(hay_adolescentes+hay_ninos>0)*(1-hay_adultosmayores)]

#c. Hogares multigeneracionales
#Los hogares multigeneracionales son los que incluyen:
#1) tres generaciones, con al menos un adulto, una persona/adulto mayor y al menos un niño o adolescente, o
#2) dos generaciones, con al menos una persona/adulto mayor y al menos un niño o adolescente.
EPF9_PERSONAS[,multigeneracional:=(hay_adolescentes + hay_ninos>0)*hay_adultosmayores]

#d. Adultos con personas mayores
EPF9_PERSONAS[,solo_adultos_y_mayores:=hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores]

#e. Solo adultos mayores
EPF9_PERSONAS[,solo_adultosmayores:=(1-hay_adultos)*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores]

BASE_ODS382_EPF9_THOGAR<-EPF9_PERSONAS[sprincipal==1,.(solo_adultos,solo_adultos_y_nna,multigeneracional,solo_adultos_y_mayores,solo_adultosmayores),by=folio][BASE_ODS382_EPF9,,on=c("folio"="FOLIO")]



EPF8_PERSONAS<-fread(url_epf8_personas)
EPF8_PERSONAS[, c("hay_adultos","hay_adolescentes","hay_ninos","hay_adultosmayores") :=
                list(as.integer(any(EDAD >= 20 & EDAD <= 59)),
                     as.integer(any(EDAD >= 10 & EDAD <= 19)),
                     as.integer(any(EDAD < 10)),
                     as.integer(any(EDAD >= 60)))
              , by = FOLIO]

EPF8_PERSONAS[,c("solo_adultos","solo_adultos_y_nna","multigeneracional","solo_adultos_y_mayores","solo_adultosmayores") :=
                list(hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*(1-hay_adultosmayores),
                     hay_adultos*(hay_adolescentes+hay_ninos>0)*(1-hay_adultosmayores),
                     (hay_adolescentes + hay_ninos>0)*hay_adultosmayores,
                     hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores,
                     (1-hay_adultos)*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores)]


BASE_ODS382_EPF8_THOGAR<-EPF8_PERSONAS[SPRINCIPAL==1,.(solo_adultos,solo_adultos_y_nna,multigeneracional,solo_adultos_y_mayores,solo_adultosmayores),by=FOLIO][BASE_ODS382_EPF8,,on=c("FOLIO")]


EPF7_PERSONAS<-fread(url_epf7_personas)

EPF7_PERSONAS[, c("hay_adultos","hay_adolescentes","hay_ninos","hay_adultosmayores") :=
                list(as.integer(any(EDAD >= 20 & EDAD <= 59)),
                     as.integer(any(EDAD >= 10 & EDAD <= 19)),
                     as.integer(any(EDAD < 10)),
                     as.integer(any(EDAD >= 60)))
              , by = FOLIO]

EPF7_PERSONAS[,c("solo_adultos","solo_adultos_y_nna","multigeneracional","solo_adultos_y_mayores","solo_adultosmayores") :=
                list(hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*(1-hay_adultosmayores),
                     hay_adultos*(hay_adolescentes+hay_ninos>0)*(1-hay_adultosmayores),
                     (hay_adolescentes + hay_ninos>0)*hay_adultosmayores,
                     hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores,
                     (1-hay_adultos)*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores)]

BASE_ODS382_EPF7_THOGAR<-EPF7_PERSONAS[SPRINCIPAL==1,.(solo_adultos,solo_adultos_y_nna,multigeneracional,solo_adultos_y_mayores,solo_adultosmayores),by=FOLIO][BASE_ODS382_EPF7,,on=c("FOLIO")]




EPF6_PERSONAS2[, c("hay_adultos","hay_adolescentes","hay_ninos","hay_adultosmayores") :=
                 list(as.integer(any(Grupo_edad >= 5 & Grupo_edad < 13)),
                      as.integer(any(Grupo_edad >= 3 & Grupo_edad < 5)),
                      as.integer(any(Grupo_edad < 3)),
                      as.integer(any(Grupo_edad >= 13)))
               , by = Clave_Hogar]
EPF6_PERSONAS2[,c("solo_adultos","solo_adultos_y_nna","multigeneracional","solo_adultos_y_mayores","solo_adultosmayores") :=
                list(hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*(1-hay_adultosmayores),
                     hay_adultos*(hay_adolescentes+hay_ninos>0)*(1-hay_adultosmayores),
                     (hay_adolescentes + hay_ninos>0)*hay_adultosmayores,
                     hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores,
                     (1-hay_adultos)*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores)]

BASE_ODS382_EPF6_THOGAR<-EPF6_PERSONAS2[Parentesco==1,.(solo_adultos,solo_adultos_y_nna,multigeneracional,solo_adultos_y_mayores,solo_adultosmayores),by=Clave_Hogar][BASE_ODS382_EPF6,,on=c("Clave_Hogar"="clave_hogar")]



EPF5_PERSONAS[, c("hay_adultos","hay_adolescentes","hay_ninos","hay_adultosmayores") :=
                list(as.integer(any(Grupo_edad >= 5 & Grupo_edad < 13)),
                     as.integer(any(Grupo_edad >= 3 & Grupo_edad < 5)),
                     as.integer(any(Grupo_edad < 3)),
                     as.integer(any(Grupo_edad >= 13)))
              , by = Numero_hogar]

EPF5_PERSONAS[,c("solo_adultos","solo_adultos_y_nna","multigeneracional","solo_adultos_y_mayores","solo_adultosmayores") :=
                 list(hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*(1-hay_adultosmayores),
                      hay_adultos*(hay_adolescentes+hay_ninos>0)*(1-hay_adultosmayores),
                      (hay_adolescentes + hay_ninos>0)*hay_adultosmayores,
                      hay_adultos*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores,
                      (1-hay_adultos)*(1-hay_adolescentes)*(1-hay_ninos)*hay_adultosmayores)]


BASE_ODS382_EPF5_THOGAR<-EPF5_PERSONAS[Parentesco==1,.(solo_adultos,solo_adultos_y_nna,multigeneracional,solo_adultos_y_mayores,solo_adultosmayores),by=Numero_hogar][BASE_ODS382_EPF5,,on=c("Numero_hogar")]


#### Estadísticas ----
new_ODS382<-data.table(version=character(),U_Nac=numeric(),U_C1=numeric(),U_C2=numeric(),
                       Q1=numeric(),Q2=numeric(),Q3=numeric(),Q4=numeric(),Q5=numeric(),
                       solo_adultos=numeric(),solo_adultos_y_nna=numeric(),multigeneracional=numeric(),solo_adultos_y_mayores=numeric(),solo_adultosmayores=numeric(),
                       Jefe_H=numeric(),Jefe_M=numeric(),Jefe_60menos=numeric(),Jefe_60mas=numeric())
new_ODS382<-rbind(new_ODS382,append(append(list("EPF5",BASE_ODS382_EPF5[umbral>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5[,sum(npersonas*FE2)],
                                                BASE_ODS382_EPF5[umbral>0&(gtot_pc>EPF5_u_115)&gs_pc>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5[,sum(npersonas*FE2)],
                                                BASE_ODS382_EPF5[umbral>0&(gtot_pc<EPF5_u_115)&gs_pc>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5[,sum(npersonas*FE2)]),
                                           EPF5_QUIN[[4]]),
                                    list(BASE_ODS382_EPF5_THOGAR[umbral>0&solo_adultos>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5_THOGAR[solo_adultos>0,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_THOGAR[umbral>0&solo_adultos_y_nna>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5_THOGAR[solo_adultos_y_nna>0,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_THOGAR[umbral>0&multigeneracional>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5_THOGAR[multigeneracional>0,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_THOGAR[umbral>0&solo_adultos_y_mayores>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5_THOGAR[solo_adultos_y_mayores>0,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_THOGAR[umbral>0&solo_adultosmayores>0,sum(npersonas*FE2)]/BASE_ODS382_EPF5_THOGAR[solo_adultosmayores>0,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_SEX[umbral>0&Sexo==1,sum(npersonas*FE2)]/BASE_ODS382_EPF5_SEX[Sexo==1,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_SEX[umbral>0&Sexo==2,sum(npersonas*FE2)]/BASE_ODS382_EPF5_SEX[Sexo==2,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_SEX[umbral>0&Grupo_edad<13,sum(npersonas*FE2)]/BASE_ODS382_EPF5_SEX[Grupo_edad<13,sum(npersonas*FE2)],
                                         BASE_ODS382_EPF5_SEX[umbral>0&Grupo_edad>=13,sum(npersonas*FE2)]/BASE_ODS382_EPF5_SEX[Grupo_edad>=13,sum(npersonas*FE2)])
)
)
new_ODS382<-rbind(new_ODS382,append(append(list("EPF6",BASE_ODS382_EPF6[umbral>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)],
                                                BASE_ODS382_EPF6[umbral>0&(gtot_pc>EPF6_u_115)&gs_pc>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)],
                                                BASE_ODS382_EPF6[umbral>0&(gtot_pc<EPF6_u_115)&gs_pc>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)]),
                                           EPF6_QUIN[[4]]),
                                    list(BASE_ODS382_EPF6_THOGAR[umbral>0&solo_adultos>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_THOGAR[solo_adultos>0,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_THOGAR[umbral>0&solo_adultos_y_nna>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_THOGAR[solo_adultos_y_nna>0,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_THOGAR[umbral>0&multigeneracional>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_THOGAR[multigeneracional>0,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_THOGAR[umbral>0&solo_adultos_y_mayores>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_THOGAR[solo_adultos_y_mayores>0,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_THOGAR[umbral>0&solo_adultosmayores>0,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_THOGAR[solo_adultosmayores>0,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_SEX[umbral>0&Sexo==1,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_SEX[Sexo==1,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_SEX[umbral>0&Sexo==2,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_SEX[Sexo==2,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_SEX[umbral>0&Grupo_edad<13,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_SEX[Grupo_edad<13,sum(PersonasXHogar*FE2)],
                                         BASE_ODS382_EPF6_SEX[umbral>0&Grupo_edad>=13,sum(PersonasXHogar*FE2)]/BASE_ODS382_EPF6_SEX[Grupo_edad>=13,sum(PersonasXHogar*FE2)])
)
)
new_ODS382<-rbind(new_ODS382,append(append(list("EPF7",BASE_ODS382_EPF7[umbral>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[,sum(NPERSONA*FE2)],
                                                BASE_ODS382_EPF7[umbral>0&(gtot_pc>EPF7_u_115)&gs_pc>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[,sum(NPERSONA*FE2)],
                                                BASE_ODS382_EPF7[umbral>0&(gtot_pc<EPF7_u_115)&gs_pc>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[,sum(NPERSONA*FE2)]),
                                           EPF7_QUIN[[4]]),
                                    list(BASE_ODS382_EPF7_THOGAR[umbral>0&solo_adultos>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7_THOGAR[solo_adultos>0,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7_THOGAR[umbral>0&solo_adultos_y_nna>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7_THOGAR[solo_adultos_y_nna>0,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7_THOGAR[umbral>0&multigeneracional>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7_THOGAR[multigeneracional>0,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7_THOGAR[umbral>0&solo_adultos_y_mayores>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7_THOGAR[solo_adultos_y_mayores>0,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7_THOGAR[umbral>0&solo_adultosmayores>0,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7_THOGAR[solo_adultosmayores>0,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7[umbral>0&SEXO==1,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[SEXO==1,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7[umbral>0&SEXO==2,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[SEXO==2,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7[umbral>0&EDAD<60,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[EDAD<60,sum(NPERSONA*FE2)],
                                         BASE_ODS382_EPF7[umbral>0&EDAD>=60,sum(NPERSONA*FE2)]/BASE_ODS382_EPF7[EDAD>=60,sum(NPERSONA*FE2)])
)
)
new_ODS382<-rbind(new_ODS382,append(append(list("EPF8",BASE_ODS382_EPF8[umbral>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[,sum(NPERSONAS*FE2)],
                                                BASE_ODS382_EPF8[umbral>0&(gtot_pc>EPF8_u_115)&gs_pc>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[,sum(NPERSONAS*FE2)],
                                                BASE_ODS382_EPF8[umbral>0&(gtot_pc<EPF8_u_115)&gs_pc>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[,sum(NPERSONAS*FE2)]),
                                           EPF8_QUIN[[4]]),
                                    list(BASE_ODS382_EPF8_THOGAR[umbral>0&solo_adultos>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8_THOGAR[solo_adultos>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8_THOGAR[umbral>0&solo_adultos_y_nna>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8_THOGAR[solo_adultos_y_nna>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8_THOGAR[umbral>0&multigeneracional>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8_THOGAR[multigeneracional>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8_THOGAR[umbral>0&solo_adultos_y_mayores>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8_THOGAR[solo_adultos_y_mayores>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8_THOGAR[umbral>0&solo_adultosmayores>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8_THOGAR[solo_adultosmayores>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8[umbral>0&SEXO==1,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[SEXO==1,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8[umbral>0&SEXO==2,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[SEXO==2,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8[umbral>0&EDAD<60,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[EDAD<60,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF8[umbral>0&EDAD>=60,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF8[EDAD>=60,sum(NPERSONAS*FE2)])
)
)
new_ODS382<-rbind(new_ODS382,append(append(list("EPF9",BASE_ODS382_EPF9[umbral>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[,sum(NPERSONAS*FE2)],
                                                BASE_ODS382_EPF9[umbral>0&(gtot_pc>EPF9_u_115)&gs_pc>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[,sum(NPERSONAS*FE2)],
                                                BASE_ODS382_EPF9[umbral>0&(gtot_pc<EPF9_u_115)&gs_pc>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[,sum(NPERSONAS*FE2)]),
                                           EPF9_QUIN[[4]]),
                                    list(BASE_ODS382_EPF9_THOGAR[umbral>0&solo_adultos>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9_THOGAR[solo_adultos>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9_THOGAR[umbral>0&solo_adultos_y_nna>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9_THOGAR[solo_adultos_y_nna>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9_THOGAR[umbral>0&multigeneracional>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9_THOGAR[multigeneracional>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9_THOGAR[umbral>0&solo_adultos_y_mayores>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9_THOGAR[solo_adultos_y_mayores>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9_THOGAR[umbral>0&solo_adultosmayores>0,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9_THOGAR[solo_adultosmayores>0,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9[umbral>0&SEXO==1,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[SEXO==1,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9[umbral>0&SEXO==2,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[SEXO==2,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9[umbral>0&EDAD<60,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[EDAD<60,sum(NPERSONAS*FE2)],
                                         BASE_ODS382_EPF9[umbral>0&EDAD>=60,sum(NPERSONAS*FE2)]/BASE_ODS382_EPF9[EDAD>=60,sum(NPERSONAS*FE2)])
)
)

#### Auxiliares ----

new_ODS382_aux<-data.table(version=character(),Prop_Nac=numeric(),GS_pp=numeric(),prop_GS_GT=numeric(),
                       med_GT=numeric(),prom_GT=numeric(),PPA_1_15_50med=numeric(),PPA_2_15=numeric(),
                       solo_adultos=numeric(),solo_adultos_y_nna=numeric(),multigeneracional=numeric(),solo_adultos_y_mayores=numeric(),solo_adultosmayores=numeric(),
                       Jefe_H=numeric(),Jefe_M=numeric(),Jefe_60menos=numeric(),Jefe_60mas=numeric())

new_ODS382_aux<-rbind(new_ODS382_aux,append(list("EPF5",BASE_ODS382_EPF5[,weightedMean((GS>0)*1,npersonas*FE2)],
                                              BASE_ODS382_EPF5[,weightedMean(GS/npersonas*12/365.25,FE2)],
                                              BASE_ODS382_EPF5[,weightedMean(GS/GT,FE2)],
                                              BASE_ODS382_EPF5[,weightedMedian(GT/npersonas*12/365.25,FE2)],
                                              BASE_ODS382_EPF5[,weightedMean(GT/npersonas*12/365.25,FE2)],
                                              EPF5_u_115,
                                              Umbrales[EPF==5,PPA_215]),
                                            c(BASE_ODS382_EPF5_THOGAR[solo_adultos>0,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_THOGAR[solo_adultos_y_nna>0,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_THOGAR[multigeneracional>0,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_THOGAR[solo_adultos_y_mayores>0,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_THOGAR[solo_adultosmayores>0,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_SEX[Sexo==1,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_SEX[Sexo==2,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_SEX[Grupo_edad<13,sum(npersonas*FE2)],
                                              BASE_ODS382_EPF5_SEX[Grupo_edad>=13,sum(npersonas*FE2)])/BASE_ODS382_EPF5[,sum(npersonas*FE2)]
                                            )
                      )

new_ODS382_aux<-rbind(new_ODS382_aux,append(list("EPF6",BASE_ODS382_EPF6[,weightedMean((GS>0)*1,PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6[,weightedMean(GS/PersonasXHogar*12/365.25,FE2)],
                                              BASE_ODS382_EPF6[,weightedMean(GS/GT,FE2)],
                                              BASE_ODS382_EPF6[,weightedMedian(GT/PersonasXHogar*12/365.25,FE2)],
                                              BASE_ODS382_EPF6[,weightedMean(GT/PersonasXHogar*12/365.25,FE2)],
                                              EPF6_u_115,
                                              Umbrales[EPF==6,PPA_215]),
                                            c(BASE_ODS382_EPF6_THOGAR[solo_adultos>0,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_THOGAR[solo_adultos_y_nna>0,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_THOGAR[multigeneracional>0,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_THOGAR[solo_adultos_y_mayores>0,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_THOGAR[solo_adultosmayores>0,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_SEX[Sexo==1,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_SEX[Sexo==2,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_SEX[Grupo_edad<13,sum(PersonasXHogar*FE2)],
                                              BASE_ODS382_EPF6_SEX[Grupo_edad>=13,sum(PersonasXHogar*FE2)])/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)])
                      )
new_ODS382_aux<-rbind(new_ODS382_aux,append(list("EPF7",BASE_ODS382_EPF7[,weightedMean((GS>0)*1,NPERSONA*FE2)],
                                              BASE_ODS382_EPF7[,weightedMean(GS/NPERSONA*12/365.25,FE2)],
                                              BASE_ODS382_EPF7[,weightedMean(GS/GT,FE2)],
                                              BASE_ODS382_EPF7[,weightedMedian(GT/NPERSONA*12/365.25,FE2)],
                                              BASE_ODS382_EPF7[,weightedMean(GT/NPERSONA*12/365.25,FE2)],
                                              EPF7_u_115,
                                              Umbrales[EPF==7,PPA_215]),
                                            c(BASE_ODS382_EPF7_THOGAR[solo_adultos>0,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7_THOGAR[solo_adultos_y_nna>0,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7_THOGAR[multigeneracional>0,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7_THOGAR[solo_adultos_y_mayores>0,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7_THOGAR[solo_adultosmayores>0,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7[SEXO==1,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7[SEXO==2,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7[EDAD<60,sum(NPERSONA*FE2)],
                                              BASE_ODS382_EPF7[EDAD>=60,sum(NPERSONA*FE2)])/BASE_ODS382_EPF7[,sum(NPERSONA*FE2)])
                      )

new_ODS382_aux<-rbind(new_ODS382_aux,append(list("EPF8",BASE_ODS382_EPF8[,weightedMean((GS>0)*1,NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8[,weightedMean(GS/NPERSONAS*12/365.25,FE2)],
                                              BASE_ODS382_EPF8[,weightedMean(GS/GT,FE2)],
                                              BASE_ODS382_EPF8[,weightedMedian(GT/NPERSONAS*12/365.25,FE2)],
                                              BASE_ODS382_EPF8[,weightedMean(GT/NPERSONAS*12/365.25,FE2)],
                                              EPF8_u_115,
                                              Umbrales[EPF==8,PPA_215]),
                                            c(BASE_ODS382_EPF8_THOGAR[solo_adultos>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8_THOGAR[solo_adultos_y_nna>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8_THOGAR[multigeneracional>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8_THOGAR[solo_adultos_y_mayores>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8_THOGAR[solo_adultosmayores>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8[SEXO==1,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8[SEXO==2,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8[EDAD<60,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF8[EDAD>=60,sum(NPERSONAS*FE2)])/BASE_ODS382_EPF8[,sum(NPERSONAS*FE2)])
                      )

new_ODS382_aux<-rbind(new_ODS382_aux,append(list("EPF9",BASE_ODS382_EPF9[,weightedMean((GS>0)*1,NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9[,weightedMean(GS/NPERSONAS*12/365.25,FE2)],
                                              BASE_ODS382_EPF9[,weightedMean(GS/GT,FE2)],
                                              BASE_ODS382_EPF9[,weightedMedian(GT/NPERSONAS*12/365.25,FE2)],
                                              BASE_ODS382_EPF9[,weightedMean(GT/NPERSONAS*12/365.25,FE2)],
                                              EPF9_u_115,
                                              Umbrales[EPF==9,PPA_215]),
                                            c(BASE_ODS382_EPF9_THOGAR[solo_adultos>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9_THOGAR[solo_adultos_y_nna>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9_THOGAR[multigeneracional>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9_THOGAR[solo_adultos_y_mayores>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9_THOGAR[solo_adultosmayores>0,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9[SEXO==1,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9[SEXO==2,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9[EDAD<60,sum(NPERSONAS*FE2)],
                                              BASE_ODS382_EPF9[EDAD>=60,sum(NPERSONAS*FE2)])/BASE_ODS382_EPF9[,sum(NPERSONAS*FE2)])
                      )

writexl::write_xlsx(list("Indicador 3.8.2 revisado"=new_ODS382,"Estadísticas auxiliares"=new_ODS382_aux),
                    "C:/Users/ismaelaguilera/OneDrive - SUBSECRETARIA DE SALUD PUBLICA/2025/ODS 382 Consulta/Revision_ODS382_con_datos_web_2025_2.xlsx")

# BASE_ODS382_EPF9[,Anio:=2021]
# BASE_ODS382_EPF9[,Periodo:=2]
# BASE_ODS382_EPF9[,PPA_215:=1168.71496582031*2.15]
# BASE_ODS382_EPF9[,PPP:=1168.71496582031]
# 
# readr::write_csv(BASE_ODS382_EPF9[,.(Anio,FE,Periodo,NPERSONAS,GT,GS,PPA_215,PPP)],"C:/Users/ismaelaguilera/OneDrive - SUBSECRETARIA DE SALUD PUBLICA/2025/ODS 382 Consulta/EPF9.csv")