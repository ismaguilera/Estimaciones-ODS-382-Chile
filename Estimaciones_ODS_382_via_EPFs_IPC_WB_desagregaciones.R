#### CONTACTO ####
#   Ismael Aguilera
#   Profesional Depto. Economía de la Salud, DIPLAS, SSP, Minsal
#   Consultas a: ismael.aguilera@minsal.cl o al anexo: 240484
#   Última actualización: 29/Enero/2024

#### Paquetes ####
list_of_packages <- c("data.table", "scales", "readxl", "writexl","laeken","jsonlite","installr")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}
lapply(list_of_packages, require, character.only = TRUE)
options(timeout = max(1000, getOption("timeout")))

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
eval_base<- function(BASE_ODS,pob_epf,expr,cond){
  return(BASE_ODS[eval(parse(text =cond)),eval(parse(text =expr))]/pob_epf)
}
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

#Umbrales 10% y 25% e indicadores auxiliares
ODS382<-function(BASE_ODS,var_npersonas,var_factor_exp,var_hogar){
  suma_ponderada<-paste0("sum(",var_npersonas,"*",var_factor_exp,")")
  pob_epf<-eval_base(BASE_ODS,1,expr=suma_ponderada,cond=paste0("!is.na(",var_factor_exp,")"))
  
  BASE_ODS[,('GCS_cA'):=GS/GT,by=var_hogar]  
  ODS382_datos<-percent(c(eval_base(BASE_ODS,pob_epf,expr=suma_ponderada,cond="GCS_cA>0.10"),
                    eval_base(BASE_ODS,pob_epf,expr=suma_ponderada,cond="GCS_cA>0.25")),accuracy = 0.001)
  BASE_ODS[,('GSpc_d'):=GS*12/365/get(var_npersonas),by=var_hogar]
  gs<-c(percent(eval_base(BASE_ODS,pob_epf,expr=suma_ponderada,cond="GS>0"),accuracy = 0.001), 
        BASE_ODS[,weighted.mean(GSpc_d,get(var_factor_exp))],                       #promedio
        BASE_ODS[,weightedMedian(GSpc_d,get(var_factor_exp))])                      #mediana
  porc_ppto<-percent(BASE_ODS[,mean(GCS_cA)],accuracy = 0.001)
  BASE_ODS[,('GTpc_d'):=GT*12/365/get(var_npersonas),by=var_hogar]
  Consumoh<-c(BASE_ODS[,weightedMedian(GTpc_d,get(var_factor_exp))],                #promedio
              BASE_ODS[,weighted.mean(GTpc_d,get(var_factor_exp))])                 #mediana
  return(list(ODS382_datos,gs,porc_ppto,Consumoh))
}

#Medidas de pobreza relacionadas a gastos en salud
FGT<-function(BASE_ODS,var_npersonas,var_factor_exp){
  suma_ponderada<-paste0("sum(",var_npersonas,"*",var_factor_exp)
  pob_epf<-eval_base(BASE_ODS,1,expr=paste0(suma_ponderada,")"),cond=paste0("!is.na(",var_factor_exp,")"))
  formula_hh_expcapdN<-"*as.numeric(GTpc_d-GSpc_d<Z)*((Z-(GTpc_d-GSpc_d))/Z)^"
  formula_hh_expcapdG<-"*as.numeric(GTpc_d<Z)*((Z-GTpc_d)/Z)^"
  condicion_basal<-paste0("!is.na(",var_factor_exp,")")
  FGT_0<-eval_base(BASE_ODS,pob_epf,expr=paste0(suma_ponderada,formula_hh_expcapdN,"0)"),cond=condicion_basal)-
    eval_base(BASE_ODS,pob_epf,expr=paste0(suma_ponderada,formula_hh_expcapdG,"0)"),cond=condicion_basal)
  FGT_1<-eval_base(BASE_ODS,pob_epf,expr=paste0(suma_ponderada,formula_hh_expcapdG,"0)"),cond=paste0("GSpc_d>0"))
  FGT_2<-eval_base(BASE_ODS,pob_epf,expr=paste0(suma_ponderada,formula_hh_expcapdN,"1)"),cond=condicion_basal)-
    eval_base(BASE_ODS,pob_epf,expr=paste0(suma_ponderada,formula_hh_expcapdG,"1)"),cond=condicion_basal)
  return(percent(c(FGT_0,FGT_1,FGT_2),accuracy = 0.01))
}

#Indicadores de empobrecimiento por gastos en salud
EMP_FGT<- function(BASE_ODS,var_npersonas,var_factor_exp,LP_EPF){
  BASE_ODS[,('Z'):=LP_EPF[3]]
  EMP_FGT_60<-FGT(BASE_ODS,var_npersonas,var_factor_exp)
  BASE_ODS[,('Z'):=LP_EPF[2]]
  EMP_FGT_PPP365<-FGT(BASE_ODS,var_npersonas,var_factor_exp)
  BASE_ODS[,('Z'):=LP_EPF[1]]
  EMP_FGT_PPP215<-FGT(BASE_ODS,var_npersonas,var_factor_exp)
  return(list(EMP_FGT_60,EMP_FGT_PPP365,EMP_FGT_PPP215))
}

#Ajustar formatos a planilla excel de resultado
formato_base<-function(BASE,indicador){
  return(rbind(names(BASE[[indicador]]),gsub("\\.", ",", unname(sapply(BASE[[indicador]], as.character))),rep('', ncol(BASE[[indicador]]))))
}

# Datos de Línea de Pobreza desde web o tabla OPS
DATOS_EXTRAIDOS<-TRUE
LP<-function(ipc_anio,ODS382, datos_ref, son_extraidos = DATOS_EXTRAIDOS, Base_datos_ipc = Base_ipc,PPA_ref = PPA_2017){
  datos_lp<-if (son_extraidos){
    c(c(2.15,3.65)*PPA_ref/Base_datos_ipc[Año==2022,Promedio_base_2018_2]*Base_datos_ipc[Año==ipc_anio,Promedio_base_2018_2],ODS382[[4]][1]*0.6)
  }  else {
    datos_ref
  }
  return(datos_lp)
}

#### DATO PPA ####
url_PPA_2017_json<-"http://api.worldbank.org/v2/countries/CHL/indicators/PA.NUS.PRVT.PP?format=json&date=2017"
PPA_2017<-fromJSON(url_PPA_2017_json)[[2]]$value
#PPA_2017 == 463.231413
# otro_dato_PPA_2017<- 485.313
# desde "https://databank.worldbank.org/embed/ICP-2017-Cycle/id/4add74e?inf=n&country=CHL&Series=9260000:INDIVIDUAL%20CONSUMPTION%20EXPENDITURE%20BY%20HOUSEHOLDS%20WITHOUT%20HOUSING"


#### DATOS IPC ####
url_ipc_empalme2009_2023<-"https://www.ine.gob.cl/docs/default-source/%C3%ADndice-de-precios-al-consumidor/cuadros-estadisticos/series-empalmadas-y-antecedentes-historicos/series-empalmadas-diciembre-2009-a-la-fecha/serie-hist%C3%B3rica-empalmada-ipc-diciembre-2009-a-la-fecha-csv.csv"
url_ipc_empalme1928_2009<-"https://www.ine.gob.cl/docs/default-source/%C3%ADndice-de-precios-al-consumidor/cuadros-estadisticos/series-empalmadas-y-antecedentes-historicos/series-historicas-empalmadas-1928-al-2009/serie-hist%C3%B3rica-empalmada-ipc-general-(%C3%ADndices)-1928---2009.xls"

#Base_ipc<-fread(url_ipc_empalme2009_2023,encoding = "Latin-1",skip=3)
Base_ipc<-fread(url_ipc_empalme2009_2023,encoding = "Latin-1")
Base_ipc[,IPC:=as.numeric(gsub(",", ".", Índice))]

#crear carpeta
dirEPF<-paste0(getwd(),"/Datos/")
dir.create(file.path(dirEPF))

#descargar web
destfile_xls<-paste0(dirEPF,"IPC_1928_2009.xls")
download.file(url_ipc_empalme1928_2009,destfile_xls,mode="wb")

Base_ipc_2<-as.data.table(read_xls(destfile_xls,skip=4))
colnames(Base_ipc_2)[1]<-"Año"
Base_ipc_2[, Promedio_base_2008 := rowSums(.SD)/12, .SDcols = 2:13]

Base_ipc<-merge(Base_ipc_2[, c(1,15)],Base_ipc[,.(.N,Promedio_base_2018=mean(IPC)),by="Año"],all=TRUE)
Base_ipc[,Promedio_base_2018_2:=ifelse(is.na(Promedio_base_2008),Promedio_base_2018,Base_ipc[Año==2009,Promedio_base_2018/Promedio_base_2008]*Promedio_base_2008)]

#### EPV IX ####
url_epf9_gastos<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/ix-epf-(octubre-2021---septiembre-2022)/base-gastos-ix-epf-(formato-csv).csv"
url_epf9_personas<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/ix-epf-(octubre-2021---septiembre-2022)/base-personas-ix-epf-(formato-csv).csv"

BASE_ODS382_EPF9<-BASE_ODS382_FORMATO(url_epf9_gastos,url_epf9_personas,"FOLIO","GASTOT_HD_AI")
ODS382_EPF9<-ODS382(BASE_ODS382_EPF9,"NPERSONAS","FE2","FOLIO")

# Según datos extraídos u OPS
LP_EPF9<-LP(2021,ODS382_EPF9,c(1100,1850,5600))

EMP_FGT_EPF9<-EMP_FGT(BASE_ODS382_EPF9,"NPERSONAS","FE2",LP_EPF9)

#Respaldo local archivos
download.file(url_epf9_gastos,paste0(dirEPF,"EPF9_Gastos.csv"),mode="wb")
download.file(url_epf9_personas,paste0(dirEPF,"EPF9_Personas.csv"),mode="wb")

#### EPV VIII ####
url_epf8_gastos<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/viii-epf---(junio-2016---julio-2017)/base-gastos-viii-epf-(formato-csv).csv"
url_epf8_personas<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/viii-epf---(junio-2016---julio-2017)/base-personas-viii-epf-(formato-csv).csv"

BASE_ODS382_EPF8<-BASE_ODS382_FORMATO(url_epf8_gastos,url_epf8_personas,"FOLIO","GASTOT_HD_AI")
ODS382_EPF8<-ODS382(BASE_ODS382_EPF8,"NPERSONAS","FE2","FOLIO")

# Según datos extraídos u OPS
LP_EPF8<-LP(2016,ODS382_EPF8,c(1018.91,1729.77,5458))
  
EMP_FGT_EPF8<-EMP_FGT(BASE_ODS382_EPF8,"NPERSONAS","FE2",LP_EPF8)

#Respaldo local archivos
download.file(url_epf8_gastos,paste0(dirEPF,"EPF8_Gastos.csv"),mode="wb")
download.file(url_epf8_personas,paste0(dirEPF,"EPF8_Personas.csv"),mode="wb")

#### EPV VII ####
url_epf7_gastos<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/vii-epf---(noviembre-2011---octubre-2012)/base-gastos-vii-epf-(formato-csv).csv"
url_epf7_personas<-"https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/vii-epf---(noviembre-2011---octubre-2012)/base-personas-vii-epf-(formato-csv).csv"

BASE_ODS382_EPF7<-BASE_ODS382_FORMATO(url_epf7_gastos,url_epf7_personas,"FOLIO","GASTOT_FNR_AI")
ODS382_EPF7<-ODS382(BASE_ODS382_EPF7,"NPERSONA","FE2","FOLIO")

# Según datos OPS
LP_EPF7<-LP(2011,ODS382_EPF7,c(854.63,1450.88,3373))

EMP_FGT_EPF7<-EMP_FGT(BASE_ODS382_EPF7,"NPERSONA","FE2",LP_EPF7)

#Respaldo local archivos
download.file(url_epf7_gastos,paste0(dirEPF,"EPF7_Gastos.csv"),mode="wb")
download.file(url_epf7_personas,paste0(dirEPF,"EPF7_Personas.csv"),mode="wb")

#### EPV VI ####

url_epf6 <- "https://www.ine.gob.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/vi-epf---(noviembre-2006---octubre-2007)/base-de-datos-vi-epf---9-divisiones---formato-csv.rar"

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

ODS382_EPF6<-ODS382(BASE_ODS382_EPF6,"PersonasXHogar","FE2","clave_hogar")

# Según datos OPS
LP_EPF6<-LP(2006,ODS382_EPF6,c(710.01,1205.37,2737))

EMP_FGT_EPF6<-EMP_FGT(BASE_ODS382_EPF6,"PersonasXHogar","FE2",LP_EPF6)


#### EPV V ####
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

ODS382_EPF5<-ODS382(BASE_ODS382_EPF5,"npersonas","FE2","Numero_hogar")

# Según datos OPS
LP_EPF5<-LP(1999,ODS382_EPF5,c(591.8891542,1004.835076,1339.4))

EMP_FGT_EPF5<-EMP_FGT(BASE_ODS382_EPF5,"npersonas","FE2",LP_EPF5)


#### Compendio Datos ODS 382 ####
BASE_ODS382<-list(data.table(Umbrales=c("Gastos en salud mayores al 10% del total de gastos o ingresos de los hogares",
                                   "Gastos en salud mayores al 25% del total de gastos o ingresos de los hogares"),
                        "1997"=ODS382_EPF5[[1]],
                        "2007"=ODS382_EPF6[[1]],
                        "2012"=ODS382_EPF7[[1]],
                        "2017"=ODS382_EPF8[[1]],
                        "2021"=ODS382_EPF9[[1]]),
                  data.table("Gastos en salud y pobreza, por año, usando un umbral de pobreza relativo: 60% del total del consumo familiar diario per cápita por día" = c("(Nacional) Empobrecidos (% población)",
                                                                                                                                                                          "(Nacional) Más Empobrecidos (% población)",
                                                                                                                                                                          "(Nacional) Incremento en la brecha de la pobreza debido a gastos familiares en salud (puntos porcentuales)"),
                             "1997"=EMP_FGT_EPF5[[1]],
                             "2007"=EMP_FGT_EPF6[[1]],
                             "2012"=EMP_FGT_EPF7[[1]],
                             "2017"=EMP_FGT_EPF8[[1]],
                             "2021"=EMP_FGT_EPF9[[1]]),
                  data.table("Gasto de los hogares en salud y pobreza, en el umbral de pobreza extrema de PPA2017 $3,65 al día per cápita" = c("(Nacional) Empobrecidos (% población)",
                                                                                                                                               "(Nacional) Más Empobrecidos (% población)",
                                                                                                                                               "(Nacional) Incremento en el recuento de la pobreza debidos a gastos familiares en salud (puntos porcentuales)"),
                             "1997"=EMP_FGT_EPF5[[2]],
                             "2007"=EMP_FGT_EPF6[[2]],
                             "2012"=EMP_FGT_EPF7[[2]],
                             "2017"=EMP_FGT_EPF8[[2]],
                             "2021"=EMP_FGT_EPF9[[2]]),
                  data.table("Gasto de los hogares en salud y pobreza, en el umbral de pobreza extrema de PPA2017 $2.15 al día per cápita" = c("(Nacional) Empobrecidos (% población)",
                                                                                                                                               "(Nacional) Más Empobrecidos (% población)",
                                                                                                                                               "(Nacional) Incremento en la brecha de la pobreza debido a gastos familiares en salud (puntos porcentuales)"),
                             "1997"=EMP_FGT_EPF5[[3]],
                             "2007"=EMP_FGT_EPF6[[3]],
                             "2012"=EMP_FGT_EPF7[[3]],
                             "2017"=EMP_FGT_EPF8[[3]],
                             "2021"=EMP_FGT_EPF9[[3]]))


TABLA_EXCEL_ODS382<-as.data.frame(rbind(c("Proporción de la población nacional con grandes gastos sanitarios (ODS 3.8.2))",rep('', ncol(BASE_ODS382[[1]])-1)),
           formato_base(BASE_ODS382,1), formato_base(BASE_ODS382,2),
           c("Gasto en salud empobrecedor (Indicadores relacionados a los ODS)",rep('', ncol(BASE_ODS382[[2]])-1)),
           formato_base(BASE_ODS382,3),formato_base(BASE_ODS382,4)
           ))

BASE_ODS382_aux<-list(data.table("Gastos en salud"=c("(Nacional) Población con algún gasto familiar en salud, (%)",
                                   "(Nacional) Gasto de los hogares en salud, promedio (per cápita, por día, unidades corrientes locales nominales)",
                                   "(Nacional) Gasto de los hogares en salud, mediana (per cápita, por día, unidades corrientes locales nominales)"),
                        "1997"=ODS382_EPF5[[2]],
                        "2007"=ODS382_EPF6[[2]],
                        "2012"=ODS382_EPF7[[2]],
                        "2017"=ODS382_EPF8[[2]],
                        "2021"=ODS382_EPF9[[2]]),
                      data.table("Porcentaje del presupuesto"=c("(Nacional) Gasto de los hogares en salud como proporción del gasto total en consumo de los hogares, media (%)"),
                                 "1997"=ODS382_EPF5[[3]],
                                 "2007"=ODS382_EPF6[[3]],
                                 "2012"=ODS382_EPF7[[3]],
                                 "2017"=ODS382_EPF8[[3]],
                                 "2021"=ODS382_EPF9[[3]]),
                      data.table("Consumo o ingresos del hogar"=c("(Nacional) Gastos de consumo de los hogares, mediana (per cápita, por día, unidades corrientes locales nominales)",
                                                                  "(Nacional) Gastos de consumo de los hogares, promedio (per cápita, por día, unidades corrientes locales nominales)"),
                                 "1997"=ODS382_EPF5[[4]],
                                 "2007"=ODS382_EPF6[[4]],
                                 "2012"=ODS382_EPF7[[4]],
                                 "2017"=ODS382_EPF8[[4]],
                                 "2021"=ODS382_EPF9[[4]]),
                      data.table("Líneas de pobreza"=c("Umbral de pobreza de la PPA2017$2,15 al día per cápita en moneda local nominal",
                                                       "Umbral de pobreza de la PPA2017$3,65 al día per cápita en moneda local nominal",
                                                       "Umbral de pobreza del 60% del gasto de consumo medio en moneda local nominal"),
                                 "1997"=LP_EPF5,
                                 "2007"=LP_EPF6,
                                 "2012"=LP_EPF7,
                                 "2017"=LP_EPF8,
                                 "2021"=LP_EPF9))

TABLA_EXCEL_ODS382_aux<-as.data.frame(rbind(c("Indicadores auxiliares",rep('', ncol(BASE_ODS382_aux[[1]])-1)),
                                        formato_base(BASE_ODS382_aux,1), formato_base(BASE_ODS382_aux,2),formato_base(BASE_ODS382_aux,3),formato_base(BASE_ODS382_aux,4)
                                        ))


#### Planilla excel ####
write_xlsx(list("ODS 3.8.2 e indic. relacionados" = TABLA_EXCEL_ODS382, "Indicadores auxiliares" = TABLA_EXCEL_ODS382_aux),"Resultados_ODS382_con_datos_web_2024.xlsx",col_names = FALSE)
# write_xlsx(list("ODS 3.8.2 e indic. relacionados" = TABLA_EXCEL_ODS382, "Indicadores auxiliares" = TABLA_EXCEL_ODS382_aux),"Resultados_ODS382_con_LP-OPS.xlsx",col_names = FALSE)

#### Comprimir archivos descargados y limpiar carpeta ####
file.remove(destfile_EPF6)
file.remove(destfile_EPF5)
system(paste(unzip, "a -r Respaldo_Datos382_IPC_EPF5a9.zip",shQuote("./Datos/*",type="cmd")," -mx=9"))
unlink("Datos",recursive = TRUE)

### Nuevo análisis ####

#Como los valores de la población en algunos casos toman valores cercanos 0,5%, no se utilizó el Intervalo de confianza de Wald
#que es el habitual. Así, se utiliza el método de Pearson-Klopper para estimar los intervalos de confianza de la librería "binom"
Intervalo_confianza_Wald<-function(prop,muestra){
  EE<-1.96*(prop*(1-prop)/muestra)^(1/2)
  return(paste("[",prop-EE,",",prop+EE,"]"))
}

library(binom)

resultado<-function(Intervalo_C){
  return(paste(Intervalo_C$mean,"IC: [",Intervalo_C$lower,",",Intervalo_C$upper,"]"))
}


desagregacion<-function(EPF_PERSONAS){
  ### Población general ###
  pob<-EPF_PERSONAS[,sum(FE2)]
  #sexo
  IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1,sum(FE2)], n = pob,methods = "exact")
  lista_desagregacion<-c(paste("umbral 10%:",resultado(IC_u10)))
  IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1,sum(FE2)], n = pob,methods = "exact")
  lista_desagregacion[2]<-c(paste("umbral 25%:",resultado(IC_u25)))
  lista_desagregacion[3]<-"SEXO"
  for (i in EPF_PERSONAS[,sum(FE2),by=SEXO][[1]]){
    texto <- switch(i,"Hombres","Mujeres")
    pob_x<-EPF_PERSONAS[SEXO==i,sum(FE2)]
    IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&SEXO==i,sum(FE2)], n = pob_x,methods = "exact")
    IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&SEXO==i,sum(FE2)], n = pob_x,methods = "exact")
    lista_desagregacion<-append(lista_desagregacion,paste(texto, pob_x/EPF_PERSONAS[,sum(FE2)], resultado(IC_u10),resultado(IC_u25)))
  }
  
  # pob_u_10<-EPF_PERSONAS[u_10==1,sum(FE2)]
  # pob_u_25<-EPF_PERSONAS[u_25==1,sum(FE2)]
  # prop_sexo<-list(EPF_PERSONAS[,.(sum(FE2),p_tot=sum(FE2)/pob),by=SEXO],
  #                 EPF_PERSONAS[u_10==1,.(sum(FE2),ps_u10=sum(FE2)/pob_u_10),by=SEXO],
  #                 EPF_PERSONAS[u_25==1,.(sum(FE2),ps_u25=sum(FE2)/pob_u_25),by=SEXO])
  #tramo etario indicado
  lista_desagregacion[6]<-"TRAMO ETARIO"
  EPF_PERSONAS[, tramo_edad := fcase(EDAD < 15, "0-14", EDAD < 25, "15-24", EDAD < 45, "25-44", EDAD < 65, "45-64", default = "65+")]
  for (texto in EPF_PERSONAS[,sum(FE2),by=tramo_edad][[1]]){
    pob_x<-EPF_PERSONAS[tramo_edad==texto,sum(FE2)]
    IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&tramo_edad==texto,sum(FE2)], n = pob_x,methods = "exact")
    IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&tramo_edad==texto,sum(FE2)], n = pob_x,methods = "exact")
    lista_desagregacion<-append(lista_desagregacion,paste(texto, pob_x/EPF_PERSONAS[,sum(FE2)], resultado(IC_u10),resultado(IC_u25)))
  }
  
  
  pob_sobre_15<-round(EPF_PERSONAS[tramo_edad!="0-14",sum(FE2)],0)
  pob_u_10_sobre_15<-round(EPF_PERSONAS[u_10==1&tramo_edad!="0-14",sum(FE2)],0)
  pob_u_25_sobre_15<-round(EPF_PERSONAS[u_25==1&tramo_edad!="0-14",sum(FE2)],0)
  
  tramo_etario<-list(EPF_PERSONAS[tramo_edad!="0-14",.(sum(FE2),pte=sum(FE2)/pob_sobre_15),by=tramo_edad],
                     EPF_PERSONAS[u_10==1&tramo_edad!="0-14",.(sum(FE2),pte_u10=sum(FE2)/pob_u_10_sobre_15),by=tramo_edad],
                     EPF_PERSONAS[u_25==1&tramo_edad!="0-14",.(sum(FE2),pte_u25=sum(FE2)/pob_u_25_sobre_15),by=tramo_edad])
  #rangos de años de escolaridad indicados 
  
  lista_desagregacion<-append(lista_desagregacion,"Población Adulta")
  pob_adulta<-EPF_PERSONAS[EDAD>17,sum(FE2)]
  IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&EDAD>17,sum(FE2)], n = pob_adulta,methods = "exact")
  IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&EDAD>17,sum(FE2)], n = pob_adulta,methods = "exact")
  lista_desagregacion<-append(lista_desagregacion,paste("Población adulta",pob_adulta/EPF_PERSONAS[,sum(FE2)],resultado(IC_u10),resultado(IC_u25)))
  
  lista_desagregacion<-append(lista_desagregacion,"Población Menor de Edad")
  pob_x<-EPF_PERSONAS[EDAD<18,sum(FE2)]
  IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&EDAD<18,sum(FE2)], n = pob_x,methods = "exact")
  IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&EDAD<18,sum(FE2)], n = pob_x,methods = "exact")
  lista_desagregacion<-append(lista_desagregacion,paste("Población Menor de Edad",pob_x/EPF_PERSONAS[,sum(FE2)],resultado(IC_u10),resultado(IC_u25)))
  
  lista_desagregacion<-append(lista_desagregacion,"ESCOLARIDAD en Adultos")
  EPF_PERSONAS[, anios_educ := fcase(EDUE < 8, "0-7", EDUE < 13, "8-12", default = "12+")]
  
  for (texto in EPF_PERSONAS[,.N,by=anios_educ][[1]]) {
    pob_x<-EPF_PERSONAS[anios_educ==texto&EDAD>17,sum(FE2)]
    IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&anios_educ==texto&EDAD>17,sum(FE2)], n = pob_x,methods = "exact")
    IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&anios_educ==texto&EDAD>17,sum(FE2)], n = pob_x,methods = "exact")    
    lista_desagregacion<-append(lista_desagregacion,paste(texto,pob_x/EPF_PERSONAS[,sum(FE2)],resultado(IC_u10),resultado(IC_u25)))
  }
  
  
  # pob_bajo8<-EPF_PERSONAS[anios_educ=="0-7"&EDAD>17,sum(FE2)]
  # EPF_PERSONAS[u_10==1&anios_educ=="0-7"&EDAD>17,.(.N,sum(FE2),sum(FE2)/pob_bajo8)]
  # EPF_PERSONAS[u_25==1&anios_educ=="0-7"&EDAD>17,.(.N,sum(FE2),sum(FE2)/pob_bajo8)]
  # pob_entre8y12<-EPF_PERSONAS[anios_educ=="8-12"&EDAD>17,sum(FE2)]
  # EPF_PERSONAS[u_10==1&anios_educ=="8-12"&EDAD>17,.(.N,sum(FE2),sum(FE2)/pob_entre8y12)]
  # EPF_PERSONAS[u_25==1&anios_educ=="8-12"&EDAD>17,.(.N,sum(FE2),sum(FE2)/pob_entre8y12)]
  # pob_mayor12<-EPF_PERSONAS[anios_educ=="12+"&EDAD>17,sum(FE2)]
  # EPF_PERSONAS[u_10==1&anios_educ=="12+"&EDAD>17,.(.N,sum(FE2),sum(FE2)/pob_mayor12)]
  # EPF_PERSONAS[u_25==1&anios_educ=="12+"&EDAD>17,.(.N,sum(FE2),sum(FE2)/pob_mayor12)]
  # 
  # rango_escolaridad<-list(EPF_PERSONAS[,.(.N,sum(FE2),pe=sum(FE2)/pob), by=anios_educ],
  #                         EPF_PERSONAS[u_10==1,.(.N,sum(FE2),pe_u10=sum(FE2)/pob_u_10), by=anios_educ],
  #                         EPF_PERSONAS[u_25==1,.(.N,sum(FE2),pe_u25=sum(FE2)/pob_u_25), by=anios_educ])
  #Zona
  lista_desagregacion<-append(lista_desagregacion,"ZONA")
  
  for (i in EPF_PERSONAS[,sum(FE2),by=ZONA][[1]]){
    texto <- switch(i,"Gran Santiago","Capitales regionales")
    pob_x<-EPF_PERSONAS[ZONA==i,sum(FE2)]
    IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&ZONA==i,sum(FE2)], n = pob_x,methods = "exact")
    IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&ZONA==i,sum(FE2)], n = pob_x,methods = "exact")  
    lista_desagregacion<-append(lista_desagregacion,paste(texto, pob_x/EPF_PERSONAS[,sum(FE2)],
                                                          resultado(IC_u10),resultado(IC_u25)))
  }
  # zona<-list(EPF_PERSONAS[,.(.N,sum(FE2),pz=sum(FE2)/pob), by=ZONA],
  #            EPF_PERSONAS[u_10==1,.(.N,sum(FE2),pz_u10=sum(FE2)/pob_u_10), by=ZONA],
  #            EPF_PERSONAS[u_25==1,.(.N,sum(FE2),pz_u25=sum(FE2)/pob_u_25), by=ZONA])
  #Prevision
  lista_desagregacion<-append(lista_desagregacion,"PREVISION")
  EPF_PERSONAS[, previ := fcase(SP02 < 0, "NA/NR/NS", SP02 < 6, "Fonasa", SP02 == 6, "SS FFAA y Orden", SP02 == 7,
                                "ISAPRE", SP02 == 8, "Otro", default = "Sin Info")]
  for (texto in EPF_PERSONAS[,sum(FE2),by=previ][[1]]){
    pob_x<-EPF_PERSONAS[previ==texto&EDAD>17,sum(FE2)]
    IC_u10<-binom.confint(x = EPF_PERSONAS[u_10==1&previ==texto&EDAD>17,sum(FE2)], n = pob_x,methods = "exact")
    IC_u25<-binom.confint(x = EPF_PERSONAS[u_25==1&previ==texto&EDAD>17,sum(FE2)], n = pob_x,methods = "exact") 
    lista_desagregacion<-append(lista_desagregacion,paste(texto, pob_x/EPF_PERSONAS[EDAD>17,sum(FE2)],
                                                          resultado(IC_u10),resultado(IC_u25)))
  }
  return(lista_desagregacion)
}

print_EPF<-function(url_epf_personas,BASE_ODS){
  EPF_PERSONAS<-fread(url_epf_personas)
  EPF_PERSONAS[,FE2:=as.numeric(gsub(",", ".", FE))]
  
  BASE_ODS[,u_10 := (GCS_cA>0.1)]
  BASE_ODS[,u_25 := (GCS_cA>0.25)]
  EPF_PERSONAS_2<-BASE_ODS[,.(FOLIO,u_10,u_25)][EPF_PERSONAS,,on="FOLIO"]
  pob_grandesgastos<-desagregacion(EPF_PERSONAS_2)
  cat(pob_grandesgastos,sep="\n")
}

print_EPF(url_epf8_personas,BASE_ODS382_EPF8)
print_EPF(url_epf7_personas,BASE_ODS382_EPF7)


epf6_archivo_gastos<-"PERSONAS.csv"
epf6_archivo_personas<-"Ingreso_Qing_Hogares_Nacional_Real.csv"
cmd <- paste(unzip, "x",shQuote(destfile_EPF6),"-aot -ssc",paste0("-o",shQuote(dirEPF)),epf6_archivo_gastos,epf6_archivo_personas,"-r")
system(cmd)

EPF6_PERSONAS_ind<-fread("./Datos/EPF6/PERSONAS.csv")
EPF6_PERSONAS_EDAD<-fread("./Datos/EPF6/GRUPO_EDAD.csv",encoding = "Latin-1")
EPF6_PERSONAS_EDUC<-fread("./Datos/EPF6/EDUCACION.csv",encoding = "Latin-1")
EPF6_PERSONAS_ZONA<-fread("./Datos/EPF6/AREA_GEOGRAFICA.csv",encoding = "Latin-1")
EPF6_PERSONAS_ind2<-EPF6_PERSONAS_EDAD[EPF6_PERSONAS_ind,,on="Grupo_edad"]
EPF6_PERSONAS_ind2<-EPF6_PERSONAS_EDUC[EPF6_PERSONAS_ind2,,on=c(Codigo_educacion="Educacion")]
EPF6_PERSONAS2<-EPF6_PERSONAS_ZONA[EPF6_PERSONAS,,on=c(Codigo="Area_Goegrafica")]

str(EPF7_PERSONAS)
str(EPF6_PERSONAS)
str(EPF5_PERSONAS)