![Build Status](https://img.shields.io/static/v1?label=Autoría&message=Intersecta&color=EC1E2D) 
![Build Status](https://img.shields.io/github/stars/IntersectaOrg/amicus_scjn_detenciones?&color=yellow)
![Build Status](https://img.shields.io/github/directory-file-count/IntersectaOrg/amicus_scjn_detenciones) 
![Build Status](https://img.shields.io/tokei/lines/github/IntersectaOrg/amicus_scjn_detenciones)
![Build Status](https://img.shields.io/twitter/follow/intersectaorg?label=Follow&style=social)
![Build Status](https://img.shields.io/github/followers/IntersectaOrg?style=social)

<p align="center">
<img src = "logo.jpg" alt="foo" width="300">
</p> 


# Amicus curiae para la Acción de Inconstitucionalidad 63/2019 :scroll:

Este repositorio contiene los códigos con los que se generaron las figuras y estadísticas presentadas en el **Amicus curiae para la Acción de Inconstitucionalidad 63/2019. La Ley Nacional del Registro de Detenciones y las detenciones de las Fuerzas Armadas**.

En *Intersecta* hemos dedicado parte de nuestro trabajo a documentar y visibilizar las violencias que viven las personas —particularmente aquellas que pertenecen a grupos y comunidades históricamente discriminadas— en contextos militarizados. La evidencia presentada en este *Amicus* está basada en este trabajo, y muestra que las detenciones que realizan las Fuerzas Armadas se caracterizan por la arbitrariedad y la violencia, además de la falta de un registro adecuado y transparente de su actuación.

## Fuentes de datos :books:

Los datos utilizados están disponibles en: 

- [Encuesta Nacional de Población Privada de la Libertad (ENPOL)](https://www.inegi.org.mx/programas/enpol/2021/), publicada por el INEGI. 

- Respuesta de la Sedena a la solicitud de acceso a la información pública con folio número [0000700203820.](https://tinyurl.com/2hvqnpyr) 

- Respuesta de la Sedena a la solicitud de acceso a la información pública con folio número [0000700008921.](https://tinyurl.com/2zvlo6zw)

- Respuesta de la Semar a la solicitud de acceso a la información pública con folio número [0001300006321.](https://tinyurl.com/2edxcue3)

- Respuesta de la Semar a la solicitud de acceso a la información pública con folio número [0001300078321.](https://tinyurl.com/2pb9pdcn)

- Respuesta de la Semar a la solicitud de acceso a la información pública con folio número [330026622000463.](https://tinyurl.com/2opchkaa)


## Observaciones técnicas :gear:
1. Debido al peso de los archivos `ENPOL2021_5.dbf` y `ENPOL2021_7.dbf`, están almacenados dentro del repositorio dentro de la carpeta `ENPOL2021_5_7.zip`. Para poder correr el código `04_01_enpol_limpieza_base.R`, es necesario descomprimir esta carpeta zip en la copia local del repositorio.
