#------------------------------------------------------------------------------#
# Proyecto:                   AMICUS para la SCJN sobre LNRD 
# Objetivo:                   Características sociodemográficas de las personas
#                             detenidas por las Fuerzas Armadas.
#
# Encargadas:                 Coordinación de datos de Intersecta
# 
# Fecha de creación:          08 de diciembre de 2022
# Última actualización:       23 de enero     de 2023
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(srvyr, tidyverse, dplyr, lubridate, scales, beepr, readxl)

# Limpiar espacio de trabajo 
rm(list=ls())

# Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# Establecer directorios
paste_data       <- function(x){paste0("03_datos_limpios/"             , x)}
paste_figs_FFAA  <- function(x){paste0("04_figuras/01_características/", x)}

# 1. Cargar datos --------------------------------------------------------------

## 1.1. ENPOL ------------------------------------------------------------------

load(paste_data("df_ENPOL_2021.RData"))

## 1.2. Censo 2020 -------------------------------------------------------------
# Fuente: https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=Poblacion_Poblacion_01_e60cd8cf-927f-4b94-823e-972457a12d4b&idrt=123&opc=t

df_censo <- read_xlsx(paste_data("Poblacion_01.xlsx"),
                      range = "A5:C22")

# 2. Funciones -----------------------------------------------------------------

## 2.01. Sí o no ---------------------------------------------------------------

# ---- Dos opciones 
codificar_siono2 <- function(var = x){
    
    v_respuestas <- c("Sí", "No")
    
    case_when(
        var == 1 ~ v_respuestas[1],
        var == 2 ~ v_respuestas[2],
        var == 0 ~ v_respuestas[2]
    )
}

# ---- Cuatro opciones 
codificar_siono4 <- function(var = x){
    
    v_respuestas <- c("Sí", "No", "No sabe", "No responde")
    
    case_when(
        var == 1 ~ v_respuestas[1],
        var == 2 ~ v_respuestas[2],
        var == 8 ~ v_respuestas[3],
        var == 9 ~ v_respuestas[4]
    )
}

## 2.02. Autoridades -----------------------------------------------------------

codificar_autoridad <- function(var = x){
    
    v_autoridad <- c("Autoridades civiles", "Fuerzas Armadas")
    
    case_when(
        var == 1 ~ v_autoridad[1], # Policía Municipal
        var == 2 ~ v_autoridad[1], # Policía Estatal 
        var == 3 ~ v_autoridad[1], # Policía Federal
        var == 4 ~ v_autoridad[1], # Policía Estatal Ministerial o Judicial
        var == 5 ~ v_autoridad[1], # Policía Federal Ministerial (PGR o FGR)
        var == 6 ~ v_autoridad[1], # Guardia Nacional
        var == 7 ~ v_autoridad[2], # Ejército
        var == 8 ~ v_autoridad[2]  # Marina
    )
}

## 2.03. Nivel educativo -------------------------------------------------------

codificar_escolaridad <- function(var = x){
    
    v_escolaridad <- c("Ninguno", "Preescolar", "Primaria", "Secundaria", 
                       "Carrera técnica con secundaria",
                       "Normal básica", "Preparatoria o bachillerato", 
                       "Carrera técnica con preparatoria",
                       "Licenciatura", "Maestría o doctorado", "Otros")
    
    case_when( 
        var == 0 ~ v_escolaridad[1],
        var == 1 ~ v_escolaridad[2], 
        var == 2 ~ v_escolaridad[3], 
        var == 3 ~ v_escolaridad[4], 
        var == 4 ~ v_escolaridad[5], 
        var == 5 ~ v_escolaridad[6], 
        var == 6 ~ v_escolaridad[7], 
        var == 7 ~ v_escolaridad[8], 
        var == 8 ~ v_escolaridad[9], 
        var == 9 ~ v_escolaridad[10], 
        var == 98 ~ v_escolaridad[11],
        var == 99 ~ v_escolaridad[11],
        TRUE ~ v_escolaridad[11])
}

## 2.04. Delito ----------------------------------------------------------------
# Revisar categorías 

codificar_delito <- function(var = x){
    
    v_delitos <- c("Robo", 
                   "Posesión o comercio ilegal de drogas",
                   "Secuestro o privación de la libertad",
                   "Delitos sexuales",
                   "Lesiones",
                   "Homicidio culposo",
                   "Homicidio doloso",
                   "Portación ilegal de armas",
                   "Violencia familiar",
                   "Delincuencia organizada ",
                   "Otros",
                   "No sabe",
                   "No responde")
    
    case_when(
        (var == "P5_31_01" | var == "P5_11_01") ~ v_delitos[1],
        (var == "P5_31_02" | var == "P5_11_02") ~ v_delitos[1],
        (var == "P5_31_03" | var == "P5_11_03") ~ v_delitos[1],
        (var == "P5_31_04" | var == "P5_11_04") ~ v_delitos[1],
        (var == "P5_31_05" | var == "P5_11_05") ~ v_delitos[1],
        (var == "P5_31_06" | var == "P5_11_06") ~ v_delitos[1],
        (var == "P5_31_07" | var == "P5_11_07") ~ v_delitos[1],
        (var == "P5_31_08" | var == "P5_11_08") ~ v_delitos[2],
        (var == "P5_31_09" | var == "P5_11_09") ~ v_delitos[2],
        (var == "P5_31_10" | var == "P5_11_10") ~ v_delitos[5],
        (var == "P5_31_11" | var == "P5_11_11") ~ v_delitos[6],
        (var == "P5_31_12" | var == "P5_11_12") ~ v_delitos[7],
        (var == "P5_31_13" | var == "P5_11_13") ~ v_delitos[8],
        (var == "P5_31_14" | var == "P5_11_14") ~ v_delitos[11],
        (var == "P5_31_15" | var == "P5_11_15") ~ v_delitos[9],
        (var == "P5_31_16" | var == "P5_11_16") ~ v_delitos[11],
        (var == "P5_31_17" | var == "P5_11_17") ~ v_delitos[3],
        (var == "P5_31_18" | var == "P5_11_18") ~ v_delitos[4],
        (var == "P5_31_19" | var == "P5_11_19") ~ v_delitos[11],
        (var == "P5_31_20" | var == "P5_11_20") ~ v_delitos[10],
        (var == "P5_31_21" | var == "P5_11_21") ~ v_delitos[4],
        (var == "P5_31_22" | var == "P5_11_22") ~ v_delitos[11],
        (var == "P5_31_23" | var == "P5_11_23") ~ v_delitos[3],
        (var == "P5_31_24" | var == "P5_11_24") ~ v_delitos[11],
        (var == "P5_31_25" | var == "P5_11_25") ~ v_delitos[11],
        (var == "P5_31_26" | var == "P5_11_26") ~ v_delitos[11],
        (var == "P5_31_98" | var == "P5_11_98") ~ v_delitos[12],
        (var == "P5_31_99" | var == "P5_11_99") ~ v_delitos[13]
    )
}

## 2.05. Grupos de edad --------------------------------------------------------

codificar_edad <- function(var = x){
    
    v_edad   <- c("18-29", "30-39", "40-49", "50-59", "Mayores de 60", "Otros")
    
    case_when(
        var >= 18 & var <= 29 ~ v_edad[1],
        var >= 30 & var <= 39 ~ v_edad[2],
        var >= 40 & var <= 49 ~ v_edad[3], 
        var >= 50 & var <= 59 ~ v_edad[4],
        var >= 60 & var <= 97 ~ v_edad[5],
        (var == 98 | var == 99) ~ v_edad[6],
        TRUE ~ v_edad[6])
}

## 2.06. Sexo ------------------------------------------------------------------

codificar_sexo <- function(var = x){
    
    v_sexo <- c("Hombres", "Mujeres")
    
    case_when(
        var == 1 ~ v_sexo[1],
        var == 2 ~ v_sexo[2]
    )
}

## 2.07. Ocupación -------------------------------------------------------------

codificar_ocupacion <- function(var = x){
    
    v_ocupacion <- c(
        "Funcionarios, directores y jefes",
        "Profesionistas y técnicos",
        "Trabajadores en actividades administrativas",
        "Comerciantes, empleados en ventas",
        "Servicios personales y de vigilancia",
        "Actividades agrícolas, ganaderas, forestales, caza y pesca",
        "Trabajadores artesanales, de construcción y otros oficios",
        "Operadores de maquinaria industrial, ensambladores y choferes",
        "Actividades elementales y de apoyo",
        "Actividades delictivas",
        "No trabajaba",
        "Otros"
    )
    
    case_when(
        var == 1 ~ v_ocupacion[1],
        var == 2 ~ v_ocupacion[2],
        var == 3 ~ v_ocupacion[3],
        var == 4 ~ v_ocupacion[4],
        var == 5 ~ v_ocupacion[5],
        var == 6 ~ v_ocupacion[6],
        var == 7 ~ v_ocupacion[7],
        var == 8 ~ v_ocupacion[8],
        var == 9 ~ v_ocupacion[9],
        var == 10 ~ v_ocupacion[10],
        var == 97 ~ v_ocupacion[11],
        TRUE ~ v_ocupacion[12]
    )
}

## 2.08. Entidad federativa ----------------------------------------------------

codificar_entidad <- function(var = x){
    
    v_entidades <- c(
        "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
        "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", 
        "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", 
        "Estado de México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", 
        "Oaxaca", "Puebla", "Querétaro",  "Quintana Roo", "San Luis Potosí", 
        "Sinaloa", "Sonora",  "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
        "Yucatán", "Zacatecas", "Fuera de México", "No especificado")
    
    case_when(
        var == 1 ~ v_entidades[1], 
        var == 2 ~ v_entidades[2], 
        var == 3 ~ v_entidades[3], 
        var == 4 ~ v_entidades[4], 
        var == 5 ~ v_entidades[5], 
        var == 6 ~ v_entidades[6], 
        var == 7 ~ v_entidades[7], 
        var == 8 ~ v_entidades[8], 
        var == 9 ~ v_entidades[9], 
        var == 10 ~ v_entidades[10],   
        var == 11 ~ v_entidades[11], 
        var == 12 ~ v_entidades[12], 
        var == 13 ~ v_entidades[13], 
        var == 14 ~ v_entidades[14], 
        var == 15 ~ v_entidades[15], 
        var == 16 ~ v_entidades[16], 
        var == 17 ~ v_entidades[17], 
        var == 18 ~ v_entidades[18], 
        var == 19 ~ v_entidades[19], 
        var == 20 ~ v_entidades[10], 
        var == 21 ~ v_entidades[21],
        var == 22 ~ v_entidades[22],
        var == 23 ~ v_entidades[23],
        var == 24 ~ v_entidades[24],
        var == 25 ~ v_entidades[25],
        var == 26 ~ v_entidades[26],
        var == 27 ~ v_entidades[27],
        var == 28 ~ v_entidades[28],
        var == 29 ~ v_entidades[29],
        var == 30 ~ v_entidades[30],
        var == 31 ~ v_entidades[31],
        var == 32 ~ v_entidades[32],
        var == 98 ~ v_entidades[33],
        TRUE ~ v_entidades[34])
}

## 2.09. Ingresos el mes antes de la dentención --------------------------------

codificar_ingresos <- function(var = x){
    
    v_ingresos  <- c("Menos de $3,000",
                     "De $3,000 a $5,500",
                     "De $5,501 a $7,500",
                     "De $7,501 a $9,000",
                     "De $9,001 a $11,000",
                     "Más de $11,000",
                     "No recibía ingresos",
                     "Otros")
    
    case_when(
        var == 1 ~ v_ingresos[1],
        var == 2 ~ v_ingresos[2],
        var == 3 ~ v_ingresos[3],
        var == 4 ~ v_ingresos[4],
        var == 5 ~ v_ingresos[5],
        var == 6 ~ v_ingresos[6],
        var == 7 ~ v_ingresos[7],
        var == 8 ~ v_ingresos[8],
        var == 9 ~ v_ingresos[8],
        TRUE ~ v_ingresos[8])
}

## 2.10. Identidad de género ----------------------------------------------------

codificar_identidad <- function(var = x){
    
    v_identidad   <- c("Hombre cis", "Mujer cis", "Mujer trans", "Hombre trans", "Otra", "Otros")
    
    case_when(
        var == 1 ~ v_identidad[1], 
        var == 2 ~ v_identidad[2], 
        var == 3 ~ v_identidad[3], 
        var == 4 ~ v_identidad[4], 
        var == 5 ~ v_identidad[5],
        TRUE ~ v_identidad[6])
}

## 2.11. Orientación sexual ----------------------------------------------------

codificar_orientacion <- function(var = x){
    
    v_orientacion       <- c("Bisexual", "Homosexual", "Heterosexual", "Otra", "Otros")
    
    case_when(
        var == 1 ~ v_orientacion[1], 
        var == 2 ~ v_orientacion[2], 
        var == 3 ~ v_orientacion[3], 
        var == 4 ~ v_orientacion[4],
        TRUE ~ v_orientacion[5])
}

## 2.12. Color de piel ---------------------------------------------------------
# Escala construida en Campos-Vázquez (2018)
# https://doi.org/10.1007/s13524-018-0734-z

codificar_piel <- function(var = x){
    
    v_color_piel_5      <- c("Café oscuro", "Café", "Café intermedio", 
                             "Café claro", "Blanco", "Otros")
    
    case_when(
        var == "A" ~ v_color_piel_5[1], 
        var == "B" ~ v_color_piel_5[1], 
        var == "C" ~ v_color_piel_5[1], 
        var == "D" ~ v_color_piel_5[1], 
        var == "E" ~ v_color_piel_5[1],
        var == "F" ~ v_color_piel_5[2], 
        var == "G" ~ v_color_piel_5[3], 
        var == "H" ~ v_color_piel_5[4], 
        var == "I" ~ v_color_piel_5[5], 
        var == "J" ~ v_color_piel_5[5],
        var == "K" ~ v_color_piel_5[5],
        TRUE ~ v_color_piel_5[6])
}

# 3. Procesar datos ------------------------------------------------------------

df_codificada <- df_ENPOL_2021                          %>% 
    # Convertir en numéricos 
    mutate(
        # Ocupación 
        R2_10_X = as.numeric(as.character(R2_10_X)),
        # Sexo 
        SEXO = as.numeric(as.character(SEXO)),
        # Edad
        P1_3 = as.numeric(as.character(P1_3)),
        # Autoridad
        P3_2 = as.numeric(as.character(P3_2)),
        # Nivel educativo
        P1_18_N = as.numeric(as.character(P1_18_N)),
        # Año de detención
        P3_5_A = as.numeric(as.character(P3_5_A)),
        # Leguna indígena
        P1_12 = as.numeric(as.character(P1_12)),
        # Indígena y/o afrodescendiente 
        P1_15 = as.numeric(as.character(P1_15)),
        # Entidad federativa 
        P3_3 = as.numeric(as.character(P3_3)),
        # Ingresos 
        P2_15 = as.numeric(as.character(P2_15)),
        # Identidad
        P1_22 = as.numeric(as.character(P1_22)),
        # Orientación 
        P1_23 = as.numeric(as.character(P1_23)),
        # Delitos 
        across(starts_with("P5_31_"), ~as.numeric(as.character(.))))  %>%  
    # Filtrar personas detenidas por GN antes de 2019   
    mutate(filtro = ifelse(
        (P3_5_A <= 2018 & P3_2 == 6), 1, 0))            %>% 
    filter(filtro == 0)                                 %>% 
    # Filtrar autoridades
    filter(P3_2 %in% c(1:8))                            %>% 
    # Usar funciones para codificar
    mutate(
        autoridad = codificar_autoridad(P3_2),
        sexo      = codificar_sexo(SEXO),
        g_edad    = codificar_edad(P1_3),
        n_edu     = codificar_escolaridad(P1_18_N),
        ocupacion = codificar_ocupacion(R2_10_X),
        ingresos  = codificar_ingresos(P2_15),
        entidad   = codificar_entidad(P3_3),
        identidad = codificar_identidad(P1_22),
        orient    = codificar_orientacion(P1_23),
        c_piel    = codificar_piel(as.character(P10_10))
    ) %>% 
    mutate(
        #LGBT+
        lgbt = case_when(
            (P1_22 == 3 | P1_22 == 4 | P1_22 == 5 | P1_23 == 1 | P1_23 == 2 | P1_23 == 4)~ "LGBT+",
            (P1_22 == 6 & P1_23 == 8 |
                 P1_22 == 6 & P1_23 == 8 |
                 P1_22 == 6 & P1_23 == 9 |
                 P1_22 == 8 & P1_23 == 8 |
                 P1_22 == 8 & P1_23 == 9) ~ "Otros",
            TRUE ~ "No LGBT+"
        ),
        # Persona trans 
        trans = case_when(
            (P1_22 == 3 | P1_22 == 4) ~ "Persona trans",
            (P1_22 == 1 | P1_22 == 2) ~ "Persona cis",
            TRUE ~ "Otros",
        ),
        # Heterosexuales
        hetero = case_when(
            (P1_23 == 1 | P1_23 == 2 | P1_23 == 4) ~ "Otras orientaciones",
            (P1_23 == 3) ~ "Heterosexuales",
            TRUE ~ "Otros"
        )
    )

# 4. Encuesta ------------------------------------------------------------------

df_encuesta <- df_codificada                    %>%
    # Convertir a formato numérico para implementar diseño de encuesta
    # Nota: Como se trata de variables tipo factor, primero se pasa a caracter.
    mutate_at(
        .vars = c("ID_PER", "EST_DIS", "FPC", "FAC_PER"), 
        ~as.numeric(as.character(.)))           %>% 
    # Diseño de encuesta
    as_survey_design(
        ids = ID_PER, strata = EST_DIS, weights = FAC_PER, fpc = FPC) 

# 5. Figuras -------------------------------------------------------------------

 ## 5.0. Configuración ----------------------------------------------------------

# ---- Tema 
tema <-  theme_linedraw() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot", 
        plot.title            = element_text(family = "Roboto Slab", face = "bold", color = "#16213E", size = 10,  margin = margin(5,5,5,5)),
        plot.subtitle         = element_text(family = "Fira Sans", color = "#0F3460", size = 10, face = "italic", margin = margin(2, 5, 0, 5)),
        plot.caption          = element_text(family = "Fira Sans", face = "italic", color = "#0F3460", size = 8,  hjust = 0, margin = margin(5, 5, 5, 5)),
        panel.grid.minor.y    = element_line(linetype = 3, color = "#B3C2C9", size = 0.4),
        panel.grid.major.y    = element_line(linetype = 3, color = "#B3C2C9", size = 0.4),
        panel.grid.minor.x    = element_line(linetype = 3, color = "#B3C2C9", size = 0.4),
        panel.grid.major.x    = element_line(linetype = 3, color = "#B3C2C9", size = 0.4),
        panel.spacing         = unit(0.5, "cm"),
        plot.margin           = margin(0, .5, 0, .5, "cm"),
        legend.position       = "top",
        panel.border          = element_blank(),
        legend.title          = element_text(size = 9, family = "Fira Sans", face   = "bold", color = "#0F3460", margin = margin(0,0,0,0)),
        legend.text           = element_text(size = 8, family = "Fira Sans", color = "#0F3460", margin = margin(0,0,0,0)),
        axis.title            = element_text(size = 8, family = "Fira Sans", hjust = .5, color = "#0F3460"),
        axis.text.y           = element_text(size = 8, family = "Fira Sans", hjust = 1, vjust = 0.5, color = "#0F3460"),
        axis.text.x           = element_text(size = 8, family = "Fira Sans", hjust=.5, vjust = 0.5, color = "#0F3460"),
        strip.text.x          = element_text(size = 9, family = "Fira Sans", face = "bold", color = "#0F3460", margin = margin(0,0,15,0)),
        strip.text.y          = element_text(size = 9, family = "Fira Sans", face = "bold", color = "#0F3460", margin = margin(0,13,0,13)), 
        strip.background      = element_rect(fill = "white", color = NA),
        axis.ticks            = element_blank(),
        axis.line.y.left      = element_line(color="#0F3460", size = .3),
        axis.line.x.bottom    = element_blank())

# ---- Colores 
v_letra  <- "#16213E"
v_ejes   <- "#0F3460"
v_color  <- "#E94560"
v_lineas <- "#B3C2C9"
v_colores <- c("#16213E", "#E94560")
v_colores_piel  <- c("Café oscuro" = "#513b2e", 
                     "Café" = "#9e7852", 
                     "Café intermedio" = "#bea07e", 
                     "Café claro" = "#e5c8a0",
                     "Blanco" = "#f4d8d7",
                     "Otros" = "gray")


# ---- Vectores de texto 
v_caption <- "Fuente: Encuesta Nacional de Población Privada de la Libertad 2021 (ENPOL).\nDatos procesados por Intersecta.org"
v_empty   <- ""

## 5.1. Sexo -------------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta        %>% 
    group_by(autoridad, sexo) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Cuál es el sexo de las personas privadas de su libertad detenidas por las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = sexo, y = porcentaje, fill = sexo)) +
    geom_col() +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              angle = 0,  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none", fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1),
                       expand = c(0,0)) 

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("01_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.2. Edad -------------------------------------------------------------------

### 5.2.1. General --------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta          %>% 
    group_by(autoridad, g_edad) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Cuál es la edad de las personas privadas de su libertad detenidas por las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(g_edad != "Otros", autoridad == "Fuerzas Armadas"), 
       aes(x = g_edad, y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= 0.5, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= -0.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .45),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(10))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("02_edad_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.2.2. Por sexo -------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta                %>% 
    group_by(autoridad, sexo, g_edad) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Cuál es la edad de las personas privadas de su libertad detenidas por las Fuerzas Armadas?"
v_subtitle <- "Por sexo de la persona privada de su libertad\n"

# Figura 
ggplot(df_data %>% filter(g_edad != "Otros", autoridad == "Fuerzas Armadas"),
       aes(x = g_edad, y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2.5, hjust=.5, vjust=-0.6, angle = 0,  
              family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .45),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    theme(legend.box.margin = margin(0,35,0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("02_edad_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm") 

### 5.2.3. Comparación con población -------------------------------------------

# ---- Población privada de su libertad

df_data_enpol <- df_encuesta           %>% 
    group_by(autoridad, g_edad)        %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())    %>% 
    filter(g_edad != "Otros")          %>% 
    select(-porcentaje_se, -total_se)  %>%
    mutate(pop = "Población privada de su libertad")

# ---- Población nacional

df_data <- df_censo                         %>% 
    # Homologar grupos de edad
    mutate(g_edad = case_when(
        `Grupo quinquenal de edad` %in% c("20 a 24 años", "25 a 29 años") ~ "18-29",
        `Grupo quinquenal de edad` %in% c("30 a 34 años", "35 a 39 años") ~ "30-39",
        `Grupo quinquenal de edad` %in% c("40 a 44 años", "45 a 49 años") ~ "40-49",
        `Grupo quinquenal de edad` %in% c("50 a 54 años", "55 a 59 años") ~ "50-59",
        TRUE ~ "Mayores de 60"
    ))                                      %>%  
    group_by(g_edad)                        %>% 
    summarise(total = sum(Total))           %>% 
    ungroup()                               %>% 
    mutate(porcentaje = total/sum(total))   %>%
    slice(rep(1:n(), each = 2))             %>%
    mutate(pop = "Población nacional")


df_data_censo <- cbind(data.frame(autoridad = c("Autoridades civiles",
                                                "Fuerzas Armadas")),
                       df_data)

df_data <- df_data_enpol %>% full_join(df_data_censo)

# ---- Figura 
# Labels 
v_title <- "¿Cuál es la edad de las personas privadas de su libertad detenidas por las Fuerzas Armadas?\n"
v_caption_e <- "Fuente: Encuesta Nacional de Población Privada de la Libertad 2021 (ENPOL) y Censo de Población\ny Vivienda 2020.
Datos procesados por Intersecta.org
*El primer grupo de edad de la población nacional es de 20 a 29 años."

# Figura 
ggplot(df_data %>% filter(g_edad != "Otros", autoridad == "Fuerzas Armadas"),
       aes(x = g_edad, y = porcentaje, fill = pop)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = pop,
                  color = pop, fontface = "bold"),
              position = position_dodge(1), size=2.5, hjust=.5, vjust=-0.6, angle = 0,  
              family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption_e,
        x = v_empty,
        y = v_empty,
        fill = "Población"
    ) +
    guides(color = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .45),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    theme(legend.box.margin = margin(0,35,0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("02_edad_FFAA_pop.png"), 
    width = 16.5, height = 11, units = "cm") 

# ---- Figura bala
ggplot() +
    geom_col(data = df_data_enpol %>% filter(autoridad == "Fuerzas Armadas"),
             aes(x = g_edad, y = porcentaje, fill = "Población privada de su libertad")) +
    geom_crossbar(data = df_data_censo %>% filter(autoridad == "Fuerzas Armadas"),
                  aes(x = g_edad, y = porcentaje, 
                      ymin = porcentaje, ymax = porcentaje,
                      fill = "Población nacional"),
                  width=0.6, linewidth = .2, 
                  alpha = 0, show.legend = F) +
    geom_point(data = df_data_censo,
               aes(x = g_edad, y = porcentaje,
                   fill = "Población nacional"),
               show.legend = F, size = 0.65) +
    geom_text(data = df_data_enpol %>% filter(autoridad == "Fuerzas Armadas"),
              aes(x = g_edad, y = porcentaje,
                  label=paste0(round(porcentaje,3)*100, "%"), group = pop,
                  fontface = "bold"),
              position = position_dodge(1), size=2.5, hjust=.5, vjust=-0.6, angle = 0,  
              family = "Fira Sans", color = "#E94560") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption_e,
        x = "Edad (años)\n",
        y = v_empty
    ) +
    # guides(color = "none",
    #        shape = "none") +
    scale_fill_manual(name = "", values = c("Población privada de su libertad" = "#E94560",
                                            "Población nacional" = "#0F3460")) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .44),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    theme(legend.box.margin = margin(0,35,0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("02_edad_FFAA_pop_v2.png"), 
    width = 16.5, height = 11, units = "cm") 

## 5.3. Nivel educativo --------------------------------------------------------

### 5.3.1. General -------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta          %>% 
    group_by(autoridad, n_edu)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Cuál es el último grado escolar alcanzado por las personas privadas de su libertad\ndetenidas por las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(n_edu != "Otros", autoridad == "Fuerzas Armadas"), 
       aes(y = reorder(n_edu, porcentaje), x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, vjust=.5, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              hjust= -0.2) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .1,
    #                           df_data$porcentaje+.035, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .52),
                       expand = c(0,0),
                       breaks = seq(0, .4, .1)) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("03_escolaridad_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.3.2. Por sexo ------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta                %>% 
    group_by(autoridad, sexo, n_edu)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Cuál es el último grado escolar alcanzado por las personas privadas de su libertad\ndetenidas por las Fuerzas Armadas?\n"
v_subtitle <- "Por sexo de la persona privada de su libertad\n"

# Figura 
ggplot(df_data %>% filter(n_edu != "Otros", autoridad == "Fuerzas Armadas"), 
       aes(x = reorder(n_edu, porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .55),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(20)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("03_escolaridad_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.2.3. Comparación con población -------------------------------------------

# ---- Población privada de su libertad

df_data_enpol <- df_encuesta                              %>% 
    group_by(autoridad, n_edu)                            %>% 
    summarise(
        total = survey_total(),
        porcentaje = round(survey_prop(), digits = 3))    %>%
    ungroup()                                             %>% 
    filter(n_edu != "Otros",
           autoridad == "Fuerzas Armadas")                %>%       
    mutate(pop = "Población privada de su libertad")      %>% 
    select(-porcentaje_se, -total_se, -total, -autoridad) %>% 
    mutate(n_edu = factor(n_edu,
                          levels = c("Maestría o doctorado",
                                     "Licenciatura",
                                     "Carrera técnica con preparatoria",
                                     "Carrera técnica con secundaria",
                                     "Preparatoria o bachillerato",
                                     "Secundaria",
                                     "Primaria",
                                     "Preescolar",
                                     "Ninguno")))

# ---- Población nacional

df_data_censo <- data.frame(n_edu = c("Carrera técnica con secundaria",
                                      "Carrera técnica con preparatoria",
                                      "Licenciatura",
                                      "Maestría o doctorado",
                                      "Ninguno",
                                      "Normal básica",
                                      "Preescolar",
                                      "Preparatoria o bachillerato",
                                      "Primaria",
                                      "Secundaria",
                                      "Otros"),
                            porcentaje = c(0.015, 
                                           0.013,
                                           0.187,
                                           0.019,
                                           0.053,
                                           0.002,
                                           0.002,
                                           0.216,
                                           0.224,
                                           0.260,
                                           0.010),
                            pop = "Población Nacional") %>% 
    filter(!n_edu %in% c("Normal básica", "Otros"))     %>% 
    mutate(n_edu = factor(n_edu,
                          levels = c("Maestría o doctorado",
                                     "Licenciatura",
                                     "Carrera técnica con preparatoria",
                                     "Carrera técnica con secundaria",
                                     "Preparatoria o bachillerato",
                                     "Secundaria",
                                     "Primaria",
                                     "Preescolar",
                                     "Ninguno")))

# ---- Unir bases 
df_data <- df_data_enpol %>% full_join(df_data_censo) 

# ---- Figura 
# Labels 
v_title <- "¿Cuál es el último grado escolar alcanzado por las personas privadas de su libertad\ndetenidas por las Fuerzas Armadas?\n"
v_caption_e <- "Fuente: Encuesta Nacional de Población Privada de la Libertad 2021 (ENPOL) y Muestra del Cuestionario Ampliado\nCenso 2020.
Datos procesados por Intersecta.org."

# ---- Figura bala
ggplot() +
    geom_col(data = df_data_enpol,
             aes(x = n_edu, y = porcentaje, fill = "Población privada de su libertad")) +
    geom_crossbar(data = df_data_censo,
                  aes(x = n_edu, y = porcentaje, 
                      ymin = porcentaje, ymax = porcentaje,
                      fill = "Población nacional"),
                  width=0.6, linewidth = .18, 
                  alpha = 0, show.legend = F) +
    geom_point(data = df_data_censo,
               aes(x = n_edu, y = porcentaje,
                   fill = "Población nacional"),
               show.legend = F, size = 0.6) +
    geom_text(data = df_data_enpol,
              aes(x = n_edu, y = porcentaje,
                  label=paste0(round(porcentaje,3)*100, "%"), group = pop,
                  fontface = "bold"),
              position = position_dodge(1), size=2.4, 
              vjust=.5, angle = 0,  
              # vjust= ifelse((df_data_enpol$porcentaje == 0 | df_data_enpol$porcentaje == 0.204 | df_data_enpol$porcentaje == 0.024), -0.3, -1.1),
              hjust= ifelse((df_data_enpol$porcentaje == 0.011 | df_data_enpol$porcentaje == 0.204 | df_data_enpol$porcentaje == 0.007), -0.87, -.2),
              family = "Fira Sans", color = "#E94560") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption_e,
        x = v_empty,
        y = v_empty
    ) +
    # guides(color = "none",
    #        shape = "none") +
    scale_fill_manual(name = "", values = c("Población privada de su libertad" = "#E94560",
                                            "Población nacional" = "#0F3460")) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .51),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(20)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("03_escolaridad_FFAA_pop.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.4. Ocupación --------------------------------------------------------------

### 5.4.1. General -------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta              %>% 
    group_by(autoridad, ocupacion)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿En qué trabajaban las personas privadas de su libertad detenidas por las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"), 
       aes(x = reorder(ocupacion, porcentaje), y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, hjust=-.2, 
              angle = 0,  family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) 
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .18),
                       expand = c(0,0),
                       breaks = seq(0, .4, .1)) +
    scale_x_discrete(labels = scales::wrap_format(30)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("04_ocupación_FFAA.png"), 
    width = 16.5, height = 14, units = "cm")

### 5.4.2. Por sexo ------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta                    %>% 
    group_by(autoridad, sexo, ocupacion)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿En qué trabajaban las personas privadas de su libertad detenidas por las Fuerzas Armadas?"
v_subtitle <- "Por sexo de la persona privada de su libertad\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = reorder(ocupacion, porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2.2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .26),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(30)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,112,0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("04_ocupación_sexo_FFAA.png"), 
    width = 16.5, height = 14, units = "cm")

## 5.5. Delito -----------------------------------------------------------------

### 5.5.1. Totales por sexo y delito -------------------------------------------

# Vector de variables de delitos 
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P5_11_"), starts_with("P5_31_"))))

# Para las personas sentenciadas es P5_11_
# Para las personas procesadas es P5_31_

# ---- Obtener totales para sentenciadxs

# Primer columna
df_data_s <- df_encuesta                                                %>%
    filter(P5_3 == 3)                                                   %>%
    rename(respuesta = v_codes[1])                                      %>%
    select(respuesta, sexo)                                             %>%
    mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
    srvyr::group_by(respuesta, sexo)                                    %>%
    srvyr::summarise(
        total = srvyr::survey_total())                                  %>%
    mutate(delito = v_codes[1],
           situacion = "Sentienciadx")

# Resto de columnas 
for (i in 2:28) {
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(P5_3 == 3)                                                   %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta, sexo)                                             %>%
        mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
        srvyr::group_by(respuesta, sexo)                                    %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                  %>%
        mutate(delito = v_codes[i],
               situacion = "Sentienciadx")
    
    df_data_s <- df_data_s %>% bind_rows(df_data_loop)
}

# ---- Obtener totales para procesadxs

# Primer columna
df_data_p <- df_encuesta                                                %>%
    filter(P5_3 %in% c(1, 2))                                           %>%
    rename(respuesta = v_codes[29])                                     %>%
    select(respuesta, sexo)                                             %>%
    mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
    srvyr::group_by(respuesta, sexo)                                    %>%
    srvyr::summarise(
        total = srvyr::survey_total())                                  %>%
    mutate(delito = v_codes[29],
           situacion = "Procesadx")

# Resto de columnas 
for (i in 30:56) {
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(P5_3 %in% c(1, 2))                                           %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta, sexo)                                             %>%
        mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
        srvyr::group_by(respuesta, sexo)                                    %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                  %>%
        mutate(delito = v_codes[i],
               situacion = "Procesadx")
    
    df_data_p <- df_data_p %>% bind_rows(df_data_loop)
}

# ---- Obtener proporciones por delito del total de delitos

# Juntar bases de datos 
df_data <- df_data_s %>% full_join(df_data_p)

# Proporciones 
df_prop <- df_data                        %>% 
    group_by(sexo, delito, respuesta)     %>%
    summarise(total_delito = sum(total))  %>% 
    ungroup()                             %>% 
    group_by(sexo, delito)                %>% 
    mutate(porcentaje = total_delito/sum(total_delito))

### 5.5.2. Totales por sexo y TIPO de delito -----------------------------------

# ---- Crear variables de nueva categoría de delitos sentenciadxs y procesadxs

df_data_raw <- df_encuesta   %>% 
    mutate(
        # Robos 
        del_robo = case_when(
            # Variables para sentenciadxs 
            (P5_11_01 == 1 | P5_11_02 == 1 | P5_11_03 == 1 | P5_11_04 == 1 |
                 P5_11_05 == 1 | P5_11_06 == 1 | P5_11_07 == 1 |
                 # Variables para procesadxs 
                 P5_31_01 == 1 | P5_31_02 == 1 | P5_31_03 == 1 | P5_31_04 == 1 |
                 P5_31_05 == 1 | P5_31_06 == 1 | P5_31_07 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Drogas 
        del_drogas = case_when(
            # Variables para sentenciadxs 
            (P5_11_08 == 1 | P5_11_09 == 1 | 
                 # Variables para procesadxs
                 P5_31_08 == 1 | P5_31_09 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Secuestro 
        del_secuestro = case_when(
            # Variables para sentenciadxs
            (P5_11_17 == 1 | P5_11_23 == 1 | 
                 # Variables para procesadxs
                 P5_31_17 == 1 | P5_31_23 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Delitos sexuales
        del_sexuales = case_when(
            # Variables para sentenciadxs 
            (P5_11_18 == 1 | P5_11_21 == 1 | 
                 # Variables para procesadxs 
                 P5_31_18 == 1 | P5_31_21 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Lesiones 
        del_lesiones = case_when(
            # Variables para sentenciadxs
            (P5_11_10 == 1 | 
                 # Variables para procesadxs
                 P5_31_10 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Homicidio culposo 
        del_h_culposo = case_when(
            # Variables para sentenciadxs
            (P5_11_11 == 1 | 
                 # Variables para procesadxs
                 P5_31_11 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Homicidio doloso
        del_h_doloso = case_when(
            # Variables para sentenciadxs
            (P5_11_12 == 1 | 
                 # Variables para procesadxs
                 P5_31_12 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Portación ilegal de armas
        del_armas = case_when(
            # Variables para sentenciadxs
            (P5_11_13 == 1 | 
                 # Variables para procesadxs
                 P5_31_13 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Violencia familiar
        del_v_familiar = case_when(
            # Variables para sentenciadxs
            (P5_11_15 == 1 | 
                 # Variables para procesadxs
                 P5_31_15 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Delincuencia organizada
        del_organizada = case_when(
            # Variables para sentenciadxs
            (P5_11_20 == 1 |
                 # Variables para procesadxs
                 P5_31_20 == 1) ~ "Sí",
            TRUE ~ "No"),
        # Otros delitos
        del_otros = case_when(
            # Variables para sentenciadxs
            (P5_11_14 == 1 | P5_11_16 == 1 | P5_11_19 == 1 | P5_11_22 == 1 |
                 P5_11_24 == 1 | P5_11_25 == 1 | P5_11_26 == 1 |
                 # Variables para procesadxs
                 P5_31_14 == 1 | P5_31_16 == 1 | P5_31_19 == 1 | P5_31_22 == 1 |
                 P5_31_24 == 1 | P5_31_25 == 1 | P5_31_26 == 1) ~ "Sí",
            TRUE ~ "No")
    )

# ---- Obtener totales 

# Vector de variables 
v_codes <- c(names(df_data_raw$variables %>% select(starts_with("del_"))))

# Primer columna
df_data <- df_data_raw                               %>%
    rename(respuesta = v_codes[1])                   %>%
    select(respuesta, sexo, autoridad)               %>%
    srvyr::group_by(respuesta, sexo, autoridad)      %>%
    srvyr::summarise(
        total = srvyr::survey_total())               %>%
    mutate(delito = v_codes[1])

# Resto de columnas 
for (i in 2:length(v_codes)) {
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_data_raw                         %>%
        rename(respuesta = v_codes[i])                  %>%
        select(respuesta, sexo, autoridad)              %>%
        srvyr::group_by(respuesta, sexo, autoridad)     %>%
        srvyr::summarise(
            total = srvyr::survey_total())              %>%
        mutate(delito = v_codes[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar delitos 
df_data_totales <- df_data %>% 
    mutate(`t_delito` = recode(delito,
                               "del_robo"       = "Robo",
                               "del_drogas"     = "Posesión o comercio ilegal de drogas",
                               "del_secuestro"  = "Secuestro o privación de la libertad",
                               "del_sexuales"   = "Delitos sexuales",
                               "del_lesiones"   = "Lesiones",
                               "del_h_culposo"  = "Homicidio culposo",
                               "del_h_doloso"   = "Homicidio doloso",
                               "del_armas"      = "Portación ilegal de armas",
                               "del_v_familiar" = "Violencia familiar",
                               "del_organizada" = "Delincuencia organizada",
                               "del_otros"      = "Otros"))

### 5.5.3. Figura general ------------------------------------------------------
# Correr sección 5.4.2. para obtener totales

# ---- Obtener proporciones 
df_prop <- df_data_totales                               %>%
    group_by(autoridad, t_delito, respuesta)             %>% 
    summarise(total_delito = sum(total))                 %>% 
    ungroup()                                            %>% 
    group_by(autoridad, t_delito)                        %>% 
    mutate(porcentaje = total_delito/sum(total_delito))  %>% 
    arrange(porcentaje)

# ---- Figura 
# Labels 
v_title <- "¿Por cuáles delitos fueron acusadas las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"

# Figura 
ggplot(df_prop %>% filter(respuesta == "Sí", autoridad == "Fuerzas Armadas"), 
       aes(
           # x = factor(t_delito, 
           #            levels = c("Otros",
           #                       "Violencia familiar",
           #                       "Lesiones",
           #                       "Delincuencia organizada",
           #                       "Homicidio culposo",
           #                       "Posesión o comercio ilegal de drogas",
           #                       "Portación ilegal de armas",
           #                       "Delitos sexuales",
           #                       "Secuestro o privación de la libertad",
           #                       "Homicidio doloso",
           #                       "Robo")), 
           x = reorder(t_delito, porcentaje),
           y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, hjust=-.2, 
              angle = 0,  family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .48),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(23)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("05_delito_FFAA_v2.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.5.4. Figura por sexo -----------------------------------------------------
# Correr sección 5.4.2. para obtener totales

# ---- Obtener proporciones 
df_prop <- df_data_totales                 %>% 
    group_by(autoridad, sexo, t_delito)    %>% 
    mutate(porcentaje = total/sum(total))  %>% 
    arrange(t_delito, autoridad, sexo)

# ---- Figura 
# Labels 
v_title <- "¿Por cuáles delitos fueron acusadas las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"
v_subtitle <- "Por sexo de la persona privada de su libertad\n"

# Figura 
ggplot(df_prop %>% filter(respuesta == "Sí", autoridad == "Fuerzas Armadas"), 
       aes(
           # x = factor(t_delito,
           #            levels = c("Otros",
           #                       "Violencia familiar",
           #                       "Lesiones",
           #                       "Delincuencia organizada",
           #                       "Homicidio culposo",
           #                       "Posesión o comercio ilegal de drogas",
           #                       "Portación ilegal de armas",
           #                       "Delitos sexuales",
           #                       "Secuestro o privación de la libertad",
           #                       "Homicidio doloso",
           #                       "Robo")),
           x = reorder(t_delito, porcentaje),
           y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2.2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .47),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(23)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,112,0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("05_delito_sexo_FFAA_v2.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.6. Lengua indígena --------------------------------------------------------

### 5.6.1. General -------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                       %>% 
    mutate(lengua = codificar_siono4(P1_12)) %>% 
    group_by(autoridad, lengua)              %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura

# Labels 
v_title <- "¿Las personas privadas de su libertad detenidas por las Fuerzas Armadas hablan\nalguna lengua nacional distinta al español?\n"

# Figura 
ggplot(df_data %>% filter(lengua %in% c("Sí", "No"), autoridad == "Fuerzas Armadas"), 
       aes(
           x = lengua,
           y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, hjust=.5, 
              angle = 0,  family = "Fira Sans",
              color = "#16213E",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.05),
                       expand = c(0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("06_lengua_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.6.2. Por sexo ------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                       %>% 
    mutate(lengua = codificar_siono4(P1_12)) %>% 
    group_by(autoridad, sexo, lengua)        %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura

# Labels 
v_title <- "¿Qué porcentaje de personas privadas de su libertad detenidas por las Fuerzas Armadas\nhablan alguna lengua nacional\ndistinta al español?"
v_subtitle <- "Por sexo de la persona detenida\n"

# Figura 
ggplot(df_data %>% filter(lengua == "Sí"), 
       aes(
           x = sexo,
           y = porcentaje,
           fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold", color = sexo),
              position = position_dodge(0.9),
              size=2.5, hjust=.5, 
              angle = 0,  family = "Fira Sans",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty
    ) +
    guides(color = "none",
           fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .065),
                       expand = c(0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("06_lengua_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")


## 5.7. Autoadscripción indígena -----------------------------------------------

### 5.7.1. General -------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                  %>%  
    mutate(
        a_indigena = case_when(
            P1_15 == 2 ~ "Sí",
            TRUE ~ "No"
        )
    )                                   %>% 
    group_by(autoridad, a_indigena)     %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura

# Labels 
v_title <- "¿Las personas privadas de su libertad detenidas por las Fuerzas Armadas se\nconsideran indígenas?\n"

# Figura 
ggplot(df_data %>% filter(a_indigena %in% c("Sí", "No"), autoridad == "Fuerzas Armadas"), 
       aes(
           x = a_indigena,
           y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, hjust=.5, 
              angle = 0,  family = "Fira Sans",
              color = "#16213E",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .92),
                       expand = c(0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("07_indigena_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.7.2. Por sexo ------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                     %>% 
    mutate(
        a_indigena = case_when(
            P1_15 == 2 ~ "Sí",
            TRUE ~ "No"
        )
    )                                      %>% 
    group_by(autoridad, sexo, a_indigena)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura

# Labels 
v_title <- "¿Qué porcentaje de las personas privadas de su libertad detenidas por las Fuerzas Armadas\nse consideran indígenas?"
v_subtitle <- "Por sexo de la persona detenida\n"

# Figura 
ggplot(df_data %>% filter(a_indigena == "Sí"), 
       aes(
           x = sexo,
           y = porcentaje,
           fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold", color = sexo),
              position = position_dodge(0.9),
              size=2.5, hjust=.5, 
              angle = 0,  family = "Fira Sans",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none",
           fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .22),
                       expand = c(0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("07_indígena_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.8. Autoadscripción afrodescendiente ---------------------------------------

### 5.8.1. General -------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                       %>%  
    mutate(
        a_afrodescendiente = case_when(
            P1_15 == 1 ~ "Sí",
            TRUE ~ "No"
        )
    )                                        %>% 
    group_by(autoridad, a_afrodescendiente)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura

# Labels 
v_title <- "¿Las personas privadas de su libertad detenidas por las Fuerzas Armadas se\nconsideran afrodescendientes?\n"

# Figura 
ggplot(df_data %>% filter(a_afrodescendiente %in% c("Sí", "No"), autoridad == "Fuerzas Armadas"), 
       aes(
           x = a_afrodescendiente,
           y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, hjust=.5, 
              angle = 0,  family = "Fira Sans",
              color = "#16213E",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1),
                       expand = c(0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("08_afrodescendiente_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.8.2. Por sexo ------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                             %>% 
    mutate(
        a_afrodescendiente = case_when(
            P1_15 == 1 ~ "Sí",
            TRUE ~ "No"
        )
    )                                              %>% 
    group_by(autoridad, sexo, a_afrodescendiente)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura

# Labels 
v_title <- "¿Qué porcentaje de las personas privadas de su libertad detenidas por las Fuerzas Armadas\nse consideran afrodescendientes?"
v_subtitle <- "Por sexo de la persona detenida\n"

# Figura 
ggplot(df_data %>% filter(a_afrodescendiente == "Sí", autoridad == "Fuerzas Armadas"), 
       aes(
           x = sexo,
           y = porcentaje,
           fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold", color = sexo),
              position = position_dodge(0.9),
              size=2.5, hjust=.5, 
              angle = 0,  family = "Fira Sans",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .02,
    #                           df_data$porcentaje+.01, df_data$porcentaje)),
    #            size=2.2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none",
           fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .021),
                       expand = c(0,0))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("08_afrodescendiente_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.9. Entidad federativa -----------------------------------------------------

### 5.9.1. General -------------------------------------------------------------

# ---- Proporciones 

df_data <- df_encuesta            %>% 
    group_by(autoridad, entidad)  %>%
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 

# Labels 
v_title <- "¿En qué entidad federativa fueron las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"), 
       aes(x = reorder(entidad, -porcentaje), y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              size=2.5, hjust=-.1, 
              angle = 90,  family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .23),
                       expand = c(0,0)) +
    theme(axis.text.x = element_text(angle = 90, size = 7))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("09_entidad_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")


### 5.9.2. Por sexo ------------------------------------------------------------

# ---- Proporciones 

df_data <- df_encuesta                  %>% 
    group_by(autoridad, sexo, entidad)  %>%
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 

# Labels 
v_title <- "¿En qué entidad federativa fueron las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"
v_subtitle <- "Por sexo de la persona detenida\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"), 
       aes(x = reorder(entidad, -porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold", color = sexo),
              size=1.9, hjust=-.1, 
              angle = 90,  family = "Fira Sans",
              position = position_dodge(.9),
              vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none") +
    tema +
    scale_color_manual(values = v_colores) +
    scale_fill_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .27),
                       expand = c(0,0)) +
    theme(axis.text.x = element_text(angle = 90, size = 7))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("09_entidad_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.10. Ingresos --------------------------------------------------------------

### 5.10.1. General ------------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta              %>% 
    group_by(autoridad, ingresos)   %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()) %>% 
    mutate(ingresos = factor(ingresos, 
                             levels = c("Otros",
                                        "Más de $11,000",
                                        "De $9,001 a $11,000",
                                        "De $7,501 a $9,000",
                                        "De $5,501 a $7,500",
                                        "De $3,000 a $5,500",
                                        "Menos de $3,000",
                                        "No recibía ingresos")))


# ---- Figura 
# Labels 
v_title    <- "¿Cuánto ganaron en el mes previo a su detención las personas en privadas de su libertad\ndetenidas por las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(ingresos != "Otros", autoridad == "Fuerzas Armadas"),
       aes(y = ingresos, x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= -0.1, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= 0.5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .27),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(30)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("10_ingresos_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.10.2. Por sexo -----------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta                  %>% 
    group_by(autoridad, sexo, ingresos) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())     %>% 
    mutate(ingresos = factor(ingresos, 
                             levels = c("Otros",
                                        "Más de $11,000",
                                        "De $9,001 a $11,000",
                                        "De $7,501 a $9,000",
                                        "De $5,501 a $7,500",
                                        "De $3,000 a $5,500",
                                        "Menos de $3,000",
                                        "No recibía ingresos")))

# ---- Figura 

# Labels 
v_title    <- "¿Cuánto ganaron en el mes previo a su detención las personas en privadas de su libertad\ndetenidas por las Fuerzas Armadas?\n"
v_subtitle <- "Por sexo de la persona privada de su libertad\n"

# Figura 
ggplot(df_data %>% filter(ingresos != "Otros", autoridad == "Fuerzas Armadas"),
       aes(y = ingresos, x = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2.5, hjust=-.2, vjust=.5, angle = 0,  
              family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .39),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(30)) +
    theme(legend.box.margin = margin(0,75,0,0)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("10_ingresos_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm") 

## 5.11. Identidad de género ---------------------------------------------------

### 5.11.1. Desagregado --------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta             %>% 
    group_by(autoridad, identidad) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Con qué identidad de género se identifican las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = reorder(identidad, -porcentaje), y = porcentaje)) +
    geom_col(fill = v_colores[1]) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none", fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .99),
                       expand = c(0,0)) 

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("11_identidad_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.11.2. Agregado -----------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta         %>% 
    group_by(autoridad, trans) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Qué porcentaje de las personas privadas de su libertad detenidas por las Fuerzas Armadas\nse identificaron como trans?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = reorder(trans, -porcentaje), y = porcentaje)) +
    geom_col(fill = v_colores[1]) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none", fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1),
                       expand = c(0,0)) 

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("11_identidad_agregado_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.12. Orientación sexual ----------------------------------------------------

### 5.12.1. Desagregado --------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta          %>% 
    group_by(autoridad, orient) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Con qué orientación sexual se identifican las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = reorder(orient, -porcentaje), y = porcentaje)) +
    geom_col(fill = v_colores[1]) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none", fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.05),
                       expand = c(0,0)) 

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("12_orientación_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.12.2. Agregado -----------------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta          %>% 
    group_by(autoridad, hetero) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Cuál es la orientación sexual de las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = reorder(hetero, -porcentaje), y = porcentaje)) +
    geom_col(fill = v_colores[1]) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none", fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1),
                       expand = c(0,0)) 

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("12_orientación_agregado_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.13. LGBT+ -----------------------------------------------------------------

### 5.13.1. LGBT+ agregado -----------------------------------------------------

# ---- Proporciones
df_data <- df_encuesta        %>% 
    group_by(autoridad, lgbt) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# ---- Figura 
# Labels 
v_title <- "¿Qué porcentaje de las personas privadas de su libertad detenidas por las Fuerzas Armadas\npertenecen a la comunidad LGBT+?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       aes(x = reorder(lgbt, -porcentaje), y = porcentaje)) +
    geom_col(fill = v_colores[1]) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_empty,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    guides(color = "none", fill = "none") +
    tema +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.05),
                       expand = c(0,0)) 

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("13_lgbt_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.14. Color de piel ---------------------------------------------------------

### 5.14.1. General ------------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta              %>%
    group_by(autoridad, c_piel)     %>%
    # Estimar el porcentaje de personas con intervalos de confianza
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())

# ---- Figura

# Labels 
v_title <- "¿Cón qué color de piel se identifican las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       # Coordenadas y geoms
       aes(x = factor(c_piel,
                      levels = c("Café oscuro", "Café", "Café intermedio", 
                                 "Café claro", "Blanco", "Otros")),
           y = porcentaje, fill = c_piel)) +
    geom_col() +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_empty, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_fill_manual(values = v_colores_piel) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .48),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(15))

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("14_piel_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

### 5.14.2. Por sexo -----------------------------------------------------------

# ---- Proporciones 
df_data <- df_encuesta                    %>%
    group_by(autoridad, sexo, c_piel)     %>%
    # Estimar el porcentaje de personas con intervalos de confianza
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())

# ---- Figura

# Labels 
v_title <- "¿Cón qué color de piel se identifican las personas privadas de su libertad detenidas\npor las Fuerzas Armadas?"
v_subtitle <- "Por sexo de la persona detenida\n"

# Figura 
ggplot(df_data %>% filter(autoridad == "Fuerzas Armadas"),
       # Coordenadas y geoms
       aes(x = factor(c_piel,
                      levels = c("Café oscuro", "Café", "Café intermedio", 
                                 "Café claro", "Blanco", "Otros")),
           y = porcentaje, fill = c_piel)) +
    geom_col() +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
    #                color = sexo, fontface = "bold"),
    #            position = position_stack(1), size=2.2, hjust=.5, vjust=.5, angle = 0, fill = "white",  family = "Fira Sans") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"),
                  fontface = "bold"),
              position = position_stack(1), size=2.5, hjust=.5, vjust=-0.6,
              color = v_colores[1], angle = 0,  family = "Fira Sans") +
    facet_wrap(~sexo) +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_fill_manual(values = v_colores_piel) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .64),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    theme(
        axis.text.x = element_text(size = 6)
    )

# ---- Guardar
ggsave(
    file = paste_figs_FFAA("14_piel_sexo_FFAA.png"), 
    width = 16.5, height = 11, units = "cm")

# FIN. -------------------------------------------------------------------------
