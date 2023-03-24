# Estimaciones del indicador ODS 3.8.2 Chile 1997 a 2017

Este proyecto realiza las estimaciones de los indicadores de los Objetivos de Desarrollo Sustentable (ODS) 3.8.2 de Chile para los años 2000, 2006, 2011 y 2016 mediante lenguaje de programación R, tomando como fuente de información directamente las bases de datos disponibles en la web de: las [Encuestas de Presupuestos Familiares (EPF)](https://www.ine.gob.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares) desde su versión V a las VIII, los [Índices de Precios al Consumidor (IPC)](https://www.ine.gob.cl/estadisticas/economia/indices-de-precio-e-inflacion/indice-de-precios-al-consumidor) y la estimación de [Paridad del Poder Adquisitivo (PPA)](https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=CL) publicadas por el Banco Mundial acorde al ciclo 2017 del [Programa de Comparación Internacional (ICP)](https://www.worldbank.org/en/programs/icp).

## Descripción

Este proyecto cuenta con dos archivos de secuencias o scripts. En el primer archivo, [Estimaciones_ODS...](Estimaciones_ODS_382_via_EPFs_IPC_WB.R), tiene los siguientes propósitos:

i.  Realizar las estimaciones conforme a las bases públicas por las instituciones que entregan las fuentes primarias de información,

ii. Entregar como resultado una planilla Excel [Resultados_ODS382_con_datos_web](Resultados_ODS382_con_datos_web.xlsx) que entrega los indicadores principales del ODS 3.8.2, así como los indicadores auxiliares y,

iii. Generar un respaldo de las bases de descargadas en un archivo comprimido llamado [Respaldo_Datos_IPC_EPF5a8.zip](Respaldo_Datos382_IPC_EPF5a8.zip).

Por otra parte, el segundo archivo, [Validacion_de_Bases...](Validacion_de_Bases_y_Estimaciones_con_resultados_EPFs.R), ejecuta el primer archivo y con las tablas de datos en el entorno de desarrollo se realizan las validaciones según los datos publicados en los informes de [cada EPF](https://www.ine.gob.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares).

## Instalación

Este proyecto requiere descargar los scripts en R para su ejecución y tener instalado ![](https://img.shields.io/badge/R>%3D-4.2.0-blue.svg)

El archivo descargará las librerías necesarias para su funcionamiento que son:

![](https://img.shields.io/badge/data.table-1.14.2-blue) ![](https://img.shields.io/badge/scales-1.2.0-blue) ![](https://img.shields.io/badge/readxl-1.4.0-blue) ![](https://img.shields.io/badge/writexl-1.4.0-blue) ![](https://img.shields.io/badge/laeken-0.5.2-blue) ![](https://img.shields.io/badge/jsonlite-1.8.0-blue) ![](https://img.shields.io/badge/installr-0.23.4-blue)

Además, se necesita descargar un programa para descomprimir las bases de datos a descargar. En este caso, se utiliza el archivador de ficheros <a href="https://7-zip.org/download.html"><img src="https://7-zip.org/7ziplogo.png" title="Descargar 7zip" width="30"></a> que es un software libre. Al ejecutar el script es posible instalar dicho software.

## Resultados

Este proyecto entrega las siguientes estimaciones de los ODS 3.8.2 para Chile.

**Tabla 1: Proporción de la población nacional con grandes gastos sanitarios (ODS 3.8.2)**

| Umbrales                                                                     | 1997  | 2007  |  2012  |  2017  |
|------------------------------------------------------------------------------|:-----:|:-----:|:------:|:------:|
| Gastos en salud mayores al 10% del total de gastos o ingresos de los hogares | 8,98% | 9,38% | 11,33% | 14,61% |
| Gastos en salud mayores al 25% del total de gastos o ingresos de los hogares | 1,49% | 1,93% | 2,10%  | 2,09%  |

Fuente: Elaboración propia

## Contribuciones

Primero, gracias por considerar aportar a este proyecto. Las personas como tú hacen que nuestras estimaciones sean fidedignas a las fuentes de informaciones que tenemos y podamos convertir a este proyecto una gran herramienta. Queremos que contribuir a este proyecto sea lo más fácil y transparente posible, ya sea:

i.  Informar un error

ii. Discutir sobre consideraciones en el código y los resultados de las estimaciones

iii. Enviar una corrección

iv. Proponer nuevas funciones

Por cualquiera de esos motivos u otros, como proyectos afines, estemos en contacto.

## Contacto

Ismael Aguilera, Profesional [Deptartamento de Economía de la Salud](http://desal.minsal.cl/), DIPLAS, Subsecretaría de Salud Pública

Consultas a: [ismael.aguilera\@minsal.cl](mailto:ismael.aguilera@minsal.cl)

<a href="https://www.minsal.cl/"><img src="https://i0.wp.com/diplas.minsal.cl/wp-content/uploads/2018/11/logo-minsal.png?ssl=1" title="Ir a MINSAL" width="20%"></a>


