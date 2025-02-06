# Estimaciones del indicador ODS 3.8.2 Chile 1997 a 2021

Este proyecto realiza las estimaciones de los indicadores de los Objetivos de Desarrollo Sustentable (ODS) 3.8.2 de Chile para los años 2000, 2006, 2011, 2016 y, recientemente, 2021 mediante lenguaje de programación R, tomando como fuente de información directamente las bases de datos disponibles en la web de: las [Encuestas de Presupuestos Familiares (EPF)](https://www.ine.gob.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares) desde su versión V a las IX, los [Índices de Precios al Consumidor (IPC)](https://www.ine.gob.cl/estadisticas/economia/indices-de-precio-e-inflacion/indice-de-precios-al-consumidor) y la estimación de [Paridad del Poder Adquisitivo (PPA)](https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=CL) publicadas por el Banco Mundial acorde al ciclo 2017 del [Programa de Comparación Internacional (ICP)](https://www.worldbank.org/en/programs/icp).

## Descripción

Este proyecto cuenta con dos archivos de secuencias o scripts. En el primer archivo, [Estimaciones_ODS...](Estimaciones_ODS_382_via_EPFs_IPC_WB.R), tiene los siguientes propósitos:

i.  Realizar las estimaciones conforme a las bases públicas por las instituciones que entregan las fuentes primarias de información,

ii. Entregar como resultado una planilla Excel [Resultados_ODS382_con_datos_web_2024](Resultados_ODS382_con_datos_web_2024.xlsx) que entrega los indicadores principales del ODS 3.8.2, así como los indicadores auxiliares y,

iii. Generar un respaldo de las bases de descargadas en un archivo comprimido llamado [Respaldo_Datos_IPC_EPF5a9.zip](Respaldo_Datos382_IPC_EPF5a9.zip).

<details>
<summary><b>Pseudocódigo de Estimaciones_ODS_382_via_EPFs_IPC_WB.R</b> </summary>
<pre>
INICIALIZAR
  1. Cargar paquetes de librerías en R: si no están instalados, descargar e instalar
  2. Cargar programa 7zip para descomprimir archivos a descargar, 
    si no está instalado en la ruta específica, descargar e instalar el programa 7zip
  3. Generar funciones para facilitar ejecución:
    <b>Function 1</b> <i>Evaluar base como porcentaje de la población</i> (Base, poblacion, expresion, condicion)
    <b>Function 2</b> <i>Construir base según formatos estándares</i> (url_gastos, url_personas, hogar, gasto)
    <b>Function 3</b> <i>Estimar indicadores ODS 3.8.2</i> (Base, npersonas, factor_expansion, hogar)
    <b>Function 4</b> <i>Evaluar fórmula de medición de pobreza según Líneas de pobreza</i> (Base, npersonas, factor_expansion)
    <b>Function 5</b> <i>Estimar indicadores de empobrecimiento ODS 3.8.2</i> (Base, npersonas, factor_expansion, Linea_Pobreza)
    <b>Function 6</b> <i>Ajustar formatos de tablas de datos a planilla Excel con resultados</i> (Base, indicador)
    <b>Function 7</b> <i>Establecer líneas de Pobreza</i> (ipc_anio,Base_ODS, datos_OMS, son_extraidos, Base_datos_ipc, PPA_ref)
                      <b>if</b> (son_extraidos) <b>then</b> <i>usa datos online de PPA e IPC</i>
                      <b>else</b> <i>usa datos OPS</i> <b>end if</b>
  4. Extraer datos online:
    a) <i>Paridad de Poder Adquisitivo (PPA) desde sitio del Banco Mundial</i>
    b) <i>IPC desde sitio INE</i>
<br>
ALGORITMO ESTIMACIÓN PARA EPF 5 hasta 9
  1. <i>Descargar cada base según URL oficial en INE</i>
  2. Si (EPF == 7 | EPF == 8 | EPF == 9) ejecutar F2(), <i>sino construir base ad hoc</i>
  3. Ejecutar F3()
  4. Ejecutar F7()
  5. Ejecutar F5()
  6. <i>Guardar bases en carpeta</i> Datos

<br>
ALGORITMO ENTREGA DE COMPENDIO DE RESULTADOS ODS 3.8.2
  1. <i>Construir compendio de resultados de las estimaciones</i> F3() por cada EPF
  2. <i>Construir tabla de datos ODS 3.8.2</i> ejecutando F6()
  3. <i>Construir compendio de resultados</i> de las estimaciones F4() por cada EPF
  4. <i>Construir tabla de datos ODS 3.8.2 auxiliares</i> ejecutando F6()
  5. <i>Guardar planilla con tablas de datos ODS 3.8.2 y auxiliares</i>
<br>
PROCEDIMIENTO DE RESPALDO Y LIMPIEZA
  1. <i>Eliminar archivos comprimidos</i>
  2. <i>Comprimir bases en carpeta Datos</i>
  3. <i>Eliminar carpeta Datos</i>
  
  
</pre>

> Este pseudocódigo busca identificar las acciones de las secuencias y la relación con las funciones utilizadas
</details>

<details>
    <summary><b>Nombres de bases de datos y de variables utilizadas</b></summary>
		<table>
			<tr>
			<td>	<b>Bases y Variables</b>	</td>
			<td>	<b>EPF V</b> 	</td>
			<td>	<b>EPF VI</b>	</td>
			<td>	<b>EPF VII</b>	</td>
			<td>	<b>EPF VIII</b>	</td>
			<td>	<b>EPF IX</b>	</td>
			</tr>
			<tr>
			<td>	Base Personas (BP)	</td>
			<td>	Personas.csv	</td>
			<td>	Ingreso_Qing_Hogares_Nacional_Real.csv	</td>
			<td>	base-personas-vii-epf-(formato-csv).csv	</td>
			<td>	base-personas-viii-epf-(formato-csv).csv	</td>
			<td>	base-personas-ix-epf-(formato-csv).csv	</td>
			</tr>
			<tr>
			<td>	Base Gasto (BG)	</td>
			<td>	Gasto.csv	</td>
			<td>	Gasto_QIng_Nacional_Real.csv	</td>
			<td>	base-gastos-vii-epf-(formato-csv).csv	</td>
			<td>	base-gastos-viii-epf-(formato-csv).csv	</td>
			<td>	base-gastos-ix-epf-(formato-csv).csv	</td>
			</tr>
			<tr>
			<td>	Base Factor Expansión (BFE)	</td>
			<td>	Factor_expansion.csv	</td>
			<td>		</td>
			<td>		</td>
			<td>		</td>
			<td>		</td>
			</tr>
			<tr>
			<td>	Identificador hogar	</td>
			<td>	Codigo_hogar en BG y Numero_hogar en BP	</td>
			<td>	Clave_hogar en BP y clave_hogar en BG	</td>
			<td>	FOLIO en BP y BG	</td>
			<td>	FOLIO en BP y BG	</td>
			<td>	folio en BP y BG	</td>
			</tr>
			<tr>
			<td>	Número de personas	</td>
			<td>	Número de filas por Numero_hogar en BP	</td>
			<td>	PersonasXHogar en BP	</td>
			<td>	NPERSONA en BP	</td>
			<td>	NPERSONAS en BP	</td>
			<td>	npersonas en BP	</td>
			</tr>
			<tr>
			<td>	Factor de expansión	</td>
			<td>	Factor_expansion_año en BFE	</td>
			<td>	Factor_Expansion_Anual en BP	</td>
			<td>	FE en BP	</td>
			<td>	FE en BP	</td>
			<td>	fe en BP	</td>
			</tr>
			<tr>
			<td>	Consumo del hogar	</td>
			<td>	Gasto en BG + Arriendo_imputado_vivienda en BP	</td>
			<td>	Gasto_Real en BG + Arriendo_Imputado en BP	</td>
			<td>	GASTOT_FNR_AI en BP	</td>
			<td>	GASTOT_HD_AI en BP	</td>
			<td>	gatot_hd_ai en BP	</td>
			</tr>
			<tr>
			<td>	Gastos en salud	</td>
			<td>	Gasto si Codigo_producto comienza con 5 en BG	</td>
			<td>	Gasto_Real si CodP01=="5000" en BG	</td>
			<td>	Gasto si [D=="6"] en BG	</td>
			<td>	Gasto si [D=="6"] en BG	</td>
			<td>	gasto si [D=="6"] en BG	</td>
			</tr>
		</table>
</details>



Por otra parte, el segundo archivo, [Validacion_de_Bases...](Validacion_de_Bases_y_Estimaciones_con_resultados_EPFs.R), ejecuta el primer archivo y con las tablas de datos en el entorno de desarrollo se realizan las validaciones según los datos publicados en los informes de [cada EPF](https://www.ine.gob.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares).

## Instalación

Este proyecto requiere descargar los scripts en R para su ejecución y tener instalado ![](https://img.shields.io/badge/R>%3D-4.2.0-blue.svg)

El archivo descargará las librerías necesarias para su funcionamiento que son:

![](https://img.shields.io/badge/data.table-1.14.2-blue) ![](https://img.shields.io/badge/scales-1.2.0-blue) ![](https://img.shields.io/badge/readxl-1.4.0-blue) ![](https://img.shields.io/badge/writexl-1.4.0-blue) ![](https://img.shields.io/badge/laeken-0.5.2-blue) ![](https://img.shields.io/badge/jsonlite-1.8.0-blue) ![](https://img.shields.io/badge/installr-0.23.4-blue)

Además, se necesita descargar un programa para descomprimir las bases de datos a descargar. En este caso, se utiliza el archivador de ficheros <a href="https://7-zip.org/download.html"><img src="https://7-zip.org/7ziplogo.png" title="Descargar 7zip" width="30"></a> que es un software libre. Al ejecutar el script es posible instalar dicho software.

## Resultados

Este proyecto entrega las siguientes estimaciones de los ODS 3.8.2 para Chile.

**Tabla 1: Proporción de la población nacional con grandes gastos sanitarios (ODS 3.8.2)**

| Umbrales                                                                     | 1997  | 2007  |  2012  |  2017  |  2022  |
|------------------------------------------------------------------------------|:-----:|:-----:|:------:|:------:|:------:|
| Gastos en salud mayores al 10% del total de gastos o ingresos de los hogares | 8,98% | 9,38% | 11,33% | 14,61% | 17,75% |
| Gastos en salud mayores al 25% del total de gastos o ingresos de los hogares | 1,49% | 1,93% | 2,10%  | 2,09%  |2,67%  |

Fuente: Elaboración propia

## Contribuciones

Primero, gracias por considerar aportar a este proyecto. Las personas como tú hacen que nuestras estimaciones sean fidedignas a las fuentes de informaciones que tenemos y podamos convertir a este proyecto una gran herramienta. Queremos que contribuir a este proyecto sea lo más fácil y transparente posible, ya sea:

i.  Informar un error

ii. Discutir sobre consideraciones en el código y los resultados de las estimaciones

iii. Enviar una corrección

iv. Proponer nuevas funciones

Por cualquiera de esos motivos u otros, como proyectos afines, estemos en contacto.

## Contacto

Ismael Aguilera, Profesional [Departamento de Economía de la Salud](http://desal.minsal.cl/), DIPLAS, Subsecretaría de Salud Pública

Consultas a: [ismael.aguilera\@minsal.cl](mailto:ismael.aguilera@minsal.cl)

<a href="https://www.minsal.cl/"><img src="https://i0.wp.com/diplas.minsal.cl/wp-content/uploads/2018/11/logo-minsal.png?ssl=1" title="Ir a MINSAL" width="20%"></a>


