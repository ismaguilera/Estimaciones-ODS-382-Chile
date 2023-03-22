#### CONTACTO ####
#   Ismael Aguilera
#   Profesional Depto. Economía de la Salud, DIPLAS, SSP, Minsal
#   Consultas a: ismael.aguilera@minsal.cl o al anexo: 240484
#   Última actualización: 21/Marzo/2023

source("Estimaciones_ODS_382_via_EPFs_IPC_WB.R")

#### VALIDACIÓN CON RESULTADOS OFICIALES ####

### EPF 8
ifelse(round(BASE_ODS382_EPF8[,sum(FE2)],0) == 3373786 &
         all(round(BASE_ODS382_EPF8[,sum(FE2),by=ZONA][1:2,V1],0) == c(1881649,1492137)),
       "Número de hogares igual EPF VIII","distinto a resultados oficiales")
pob_epf8<-BASE_ODS382_EPF8[,sum(NPERSONAS*FE2)]
ifelse(round(pob_epf8,0) == 11095466 &
         all(round(BASE_ODS382_EPF8[,sum(NPERSONAS*FE2),by=ZONA][1:2,V1],0) ==c(6224057,4871409)),
       "Número de personas igual EPF VIII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF8[,weighted.mean(as.numeric(gsub(",", ".", GASTOT_HD)),FE2)],0) == 1121925,
"Promedio Gasto igual EPF VIII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF8[,weightedMedian(as.numeric(gsub(",", ".", GASTOT_HD)),FE2)],0) == 810315,
       "Gasto mediano igual EPF VIII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF8[!is.na(FE2),weighted.mean(GS,FE2)],0) == 84791,
       "Gasto en Salud igual EPF VIII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF8[,weightedMedian(as.numeric(gsub(",", ".", GS)),FE2)],0) == 24982,
       "Gasto mediano igual EPF VIII","distinto a resultados oficiales")
round(BASE_ODS382_EPF8[!is.na(FE2),weighted.mean(GS,FE2)]/ BASE_ODS382_EPF8[,weighted.mean(as.numeric(gsub(",", ".", GASTOT_HD)),FE2)]*100,1) == 7.6
round(BASE_ODS382_EPF8[,weighted.mean(as.numeric(gsub(",", ".", GASTOT_HD))/NPERSONAS,FE2)],0) == 397819


### EPF 7
ifelse(round(BASE_ODS382_EPF7[,sum(FE2)],0) == 3009720,
       "Número de hogares igual EPF VII","distinto a resultados oficiales")
pob_epf7<-BASE_ODS382_EPF7[,sum(NPERSONA*FE2)]
ifelse(round(pob_epf7,0) == 10516225 &
         all(round(BASE_ODS382_EPF7[,sum(NPERSONA*FE2),by=ZONA][1:2,V1],0) ==c(4329313,6186912)),
       "Número de personas igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF7[,weighted.mean(as.numeric(gsub(",", ".", GASTOT_FNR)),FE2)],0) == 807409,
"Promedio Gasto igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF7[!is.na(FE2),weighted.mean(GS,FE2)],0) == 50657,
       "Gasto en Salud igual EPF VII","distinto a resultados oficiales")
round(BASE_ODS382_EPF7[!is.na(FE2),weighted.mean(GS,FE2)]/ BASE_ODS382_EPF7[,weighted.mean(as.numeric(gsub(",", ".", GASTOT_FNR)),FE2)]*100,2) == 6.27
round(BASE_ODS382_EPF7[,weighted.mean(as.numeric(gsub(",", ".", GASTOT_FNR))/NPERSONA,FE2)],0) == 269859

### EPF 6
ifelse(round(BASE_ODS382_EPF6[,sum(FE2)],0) == 2650757,
       "Número de hogares igual EPF VII","distinto a resultados oficiales")
pob_epf6<-BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)]
ifelse(round(pob_epf6,0) == 9433750,
       "Número de personas igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF6[,weighted.mean( GTsAI,FE2)],0) == 682967,
       "Promedio Gasto igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF6[,weighted.mean(GT,FE2)],0) /791013 - 1 < 0.005,
      "Promedio Gasto igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF6[,weighted.mean(GS,FE2)],0) == 36915,
       "Gasto en Salud igual EPF VII","distinto a resultados oficiales")
round(BASE_ODS382_EPF6[,weighted.mean(GS,FE2)]/ BASE_ODS382_EPF6[,weighted.mean(as.numeric(gsub(",", ".", GTsAI)),FE2)]*100,2) == 5.41
round(BASE_ODS382_EPF6[,weighted.mean( GTsAI,FE2)]/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)]*BASE_ODS382_EPF6[,sum(FE2)],0) == 191905
round(BASE_ODS382_EPF6[,weighted.mean(GT,FE2)]/BASE_ODS382_EPF6[,sum(PersonasXHogar*FE2)]*BASE_ODS382_EPF6[,sum(FE2)],0) / 222264 - 1 < 0.005

### EPF 5
ifelse(round(BASE_ODS382_EPF5[,sum(FE2)],0) == 1363706,
       "Número de hogares igual EPF VII","distinto a resultados oficiales")
pob_epf5<-BASE_ODS382_EPF5[,sum(npersonas*FE2)]
ifelse(round(pob_epf5,0) == 5233796,
       "Número de personas igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF5[,weighted.mean( GTsAI,FE2)],0) / 445637 - 1 < 0.005,
       "Promedio Gasto igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF5[,weighted.mean( GT,FE2)],0) / 536596 - 1 < 0.005,
       "Promedio Gasto igual EPF VII","distinto a resultados oficiales")
ifelse(round(BASE_ODS382_EPF5[,weighted.mean(GS,FE2)],0) / 24535 - 1 < 0.005,
       "Gasto en Salud igual EPF VII","distinto a resultados oficiales")
round(BASE_ODS382_EPF5[,weighted.mean(GS,FE2)]/ BASE_ODS382_EPF5[,weighted.mean(as.numeric(gsub(",", ".", GTsAI)),FE2)]*100,2) == 5.51 

round(BASE_ODS382_EPF5[,weighted.mean( GTsAI,FE2)]/BASE_ODS382_EPF5[,sum(npersonas*FE2)]*BASE_ODS382_EPF5[,sum(FE2)],0) / 116114 - 1 < 0.005
round(BASE_ODS382_EPF5[,weighted.mean(GT,FE2)]/BASE_ODS382_EPF5[,sum(npersonas*FE2)]*BASE_ODS382_EPF5[,sum(FE2)],0) / 139814 - 1 < 0.005



