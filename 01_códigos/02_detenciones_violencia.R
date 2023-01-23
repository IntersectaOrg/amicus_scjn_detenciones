#------------------------------------------------------------------------------#
# Proyecto:                   AMICUS para la SCJN sobre LNRD
# Objetivo:                   ¿Cómo se realizan las detenciones?
#
# Encargadas:                 Coordinación de datos de Intersecta
# 
# Fecha de creación:          16 de enero de 2023
# Última actualización:       23 de enero de 2023
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
paste_data  <- function(x){paste0("03_datos_limpios/"         , x)}
paste_figs  <- function(x){paste0("04_figuras/02_detenciones/", x)}

# 1. Cargar datos --------------------------------------------------------------

## 1.1. ENPOL ------------------------------------------------------------------

load(paste_data("df_ENPOL_2021.RData"))

# 2. Funciones -----------------------------------------------------------------

## 2.1. Sí o no ----------------------------------------------------------------

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

## 2.2. Autoridades ------------------------------------------------------------

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

## 2.3. Lugar después del arresto ----------------------------------------------

codificar_lugar <- function(var = x){

    v_lugar <- c("Agencia del Ministerio Público",
                 "Juez de lo penal",
                 "Instalación de la policía",
                 "Centro de arraigo",
                 "Centro penitenciario",
                 "Oficina de gobierno",
                 "Casa particular",
                 "Establecimiento comercial",
                 "Le mantuvieron en un vehículo",
                 "Terreno baldío",
                 "Zona militar",
                 "Centro de detención para migrantes",
                 "Hospital, clínica o centro del salud",
                 "Otro",
                 "Otros")
    
    case_when(
        var == 1 ~ v_lugar[1],
        var == 2 ~ v_lugar[2],
        var == 3 ~ v_lugar[3],
        var == 4 ~ v_lugar[4],
        var == 5 ~ v_lugar[5],
        var == 6 ~ v_lugar[6],
        var == 7 ~ v_lugar[7],
        var == 8 ~ v_lugar[8],
        var == 9 ~ v_lugar[9],
        var == 10 ~ v_lugar[10],
        var == 11 ~ v_lugar[11],
        var == 12 ~ v_lugar[12],
        var == 13 ~ v_lugar[13],
        var == 14 ~ v_lugar[14],
        TRUE ~ v_lugar[15]
    )
}

## 2.4. Tiempo entre detención y disposición? ----------------------------------

codificar_tiempo <- function(var = x){
    
    v_tiempo <- c("Hasta 30 minutos",
                  "Entre 30 minutos y 1 hora",
                  "Más de 1 hora hasta 2 horas",
                  "Más de 2 horas hasta 4 horas",
                  "Más de 4 horas hasta 6 horas",
                  "Más de 6 horas hasta 24 horas",
                  "Más de 24 horas hasta 48 horas",
                  "Más de 48 horas hasta 72 horas",
                  "Más de 72 horas",
                  "Otros")
    
    case_when(
        var == 1 ~ v_tiempo[1],
        var == 2 ~ v_tiempo[2],
        var == 3 ~ v_tiempo[3],
        var == 4 ~ v_tiempo[4],
        var == 5 ~ v_tiempo[5],
        var == 6 ~ v_tiempo[6],
        var == 7 ~ v_tiempo[7],
        var == 8 ~ v_tiempo[8],
        var == 9 ~ v_tiempo[9],
        TRUE ~ v_tiempo[10]
    )
}

## 2.5. Sexo -------------------------------------------------------------------

codificar_sexo <- function(var = x){
    
    v_sexo <- c("Hombres", "Mujeres")
    
    case_when(
        var == 1 ~ v_sexo[1],
        var == 2 ~ v_sexo[2]
    )
}

## 2.6. Situación en la que se llevó a cabo la detención ----------------------

codificar_situacion <- function(var = x){
    
    v_situacion <- c("Realizando la conducta o acto por el que se les acusa",
                     "Inmediatamente después de la conducta o el acto por el que se les acusa",
                     "Con una orden de detención",
                     "Después de una inspección o revisión",
                     "Otros")
    
    case_when(
        var == 1 ~ v_situacion[1],
        var == 2 ~ v_situacion[2],
        var == 3 ~ v_situacion[3],
        var == 4 ~ v_situacion[4],
        var == 5 ~ v_situacion[5],
        var == 9 ~ v_situacion[5],
        TRUE ~ v_situacion[5])
}

## 2.7. Motivo -----------------------------------------------------------------

codificar_motivo <- function(var = x){
    
    # v_motivos <- c("Porque cometió un delito",
    #                "Porque ayudó en la realización de un delito",
    #                "Porque no ha podido comprobar su inocencia",
    #                "Lo(a) acusaron falsamente de cometer un delito",
    #                "Otros")
    
    v_motivos <- c("Porque cometió un delito",
                   "Porque ayudó en la realización de un delito",
                   "Porque dicen que cometió un delito",
                   "Otros")
    
    case_when(
        var == 1 ~ v_motivos[1],
        var == 2 ~ v_motivos[2],
        var == 3 ~ v_motivos[3],
        var == 4 ~ v_motivos[3],
        var == 5 ~ v_motivos[4],
        var == 8 ~ v_motivos[4],
        var == 9 ~ v_motivos[4])
}

## 2.8. Tipo de violencia ------------------------------------------------------
codificar_tipo <- function(var = x){
    
    v_eventos <- c(
        # Primera pregunta
        "Le amenazaron con levantarle cargos falsos", "Le amenazaron con matarlo(a)",
        "Le amenazaron con hacerle daño", "Le amenazaron con dañar a su familia",
        "Le hicieron otras amenazas", "Le presionaron para denunciar a alguien",
        "Le incomunicaron o aislaron", "Le pasearon en un automóvil",
        "Le hicieron daño a su familia", "Le desvistieron", "Le vendaron los ojos",
        # Segunda pregunta
        "Ataron su cuerpo", "Asfixia", "Tehuacanazo", "Patadas y golpes", 
        "Golpes con objetos", "Quemaduras", "Descargas eléctricas", "Aplastamiento",
        "Herida con arma blanca", "Le encajaron agujas", "Lesiones por arma de fuego", 
        "Acoso sexual, manoseo, exhibicionismo o intento de violación", "Lesiones en órganos sexuales", 
        "Violación sexual", "Otra agresión física")
    
    case_when(
        (var == v_eventos[1] | var == v_eventos[2] | var == v_eventos[3] |
             var == v_eventos[4] | var == v_eventos[5] | var == v_eventos[6] |
             var == v_eventos[7] | var == v_eventos[8] | var == v_eventos[9] |
             var == v_eventos[11]) ~ "Psicológica",
        (var == v_eventos[10] | var == v_eventos[23] | var == v_eventos[24] | 
             var == v_eventos[25]) ~ "Sexual",
        TRUE ~ "Física"
    )
}

# 3. Procesar datos ------------------------------------------------------------

df_codificada <- df_ENPOL_2021 %>% 
    # Convertir a valores numéricos
    mutate(
        # Autoridad
        P3_2 = as.numeric(as.character(P3_2)),
        # Año de detención
        P3_5_A = as.numeric(as.character(P3_5_A)),
        # Lugar después de la detención 
        P3_19 = as.numeric(as.character(P3_19)),
        # Tiempo entre detención y disposición
        P3_20 = as.numeric(as.character(P3_20)),
        # Contexto de la detención
        P3_10 = as.numeric(as.character(P3_10)),
        # Contexto de la detención 2 
        P3_11 = as.numeric(as.character(P3_11)),
        # Motivo 
        P3_1 = as.numeric(as.character(P3_1)),
        # Inspección 
        across(starts_with("P3_12_"), ~as.numeric(as.character(.))),
        # Variables de uso de la fuerza
        across(starts_with("P3_13_"), ~as.numeric(as.character(.))),
        # Variables de violencia 
        across(starts_with("P3_17_"), ~as.numeric(as.character(.))),
        across(starts_with("P3_18_"), ~as.numeric(as.character(.)))
    ) %>% 
    # Filtrar personas detenidas por GN antes de 2019    
    mutate(filtro = ifelse(
        (P3_5_A <= 2018 & P3_2 == 6), 1, 0))            %>% 
    filter(filtro == 0)                                 %>% 
    # Filtrar autoridades
    filter(P3_2 %in% c(1:8))                            %>% 
    # Usar funciones para recodificar 
    mutate(
        # Sexo
        sexo = codificar_sexo(SEXO),
        # Lugar 
        lugar = codificar_lugar(P3_19),
        # Tiempo 
        tiempo = codificar_tiempo(P3_20),
        # Autoridad 
        autoridad = codificar_autoridad(P3_2),
        # Motivo
        motivo    = codificar_motivo(P3_1),
        # Contexto de la detención
        situacion = codificar_situacion(P3_10)
    ) %>% 
    # Crear variable por tipo de violencia
    mutate(
        # Violencia psicológica 
        violencia_psic = case_when(
            # Casos donde no hubo violencia psicológica
            !(P3_17_01 == 1 | P3_17_02 == 1 | P3_17_03 == 1 | P3_17_04 == 1 | 
                  P3_17_05 == 1 | P3_17_06 == 1 | P3_17_07 == 1 | P3_17_08 == 1 | 
                  P3_17_09 == 1 | P3_17_11 == 1) ~ 0,
            # Casos donde sí hubo violencia psicológica
            (P3_17_01 == 1 | P3_17_02 == 1 | P3_17_03 == 1 | P3_17_04 == 1 | 
                 P3_17_05 == 1 | P3_17_06 == 1 | P3_17_07 == 1 | P3_17_08 == 1 | 
                 P3_17_09 == 1 | P3_17_11 == 1) ~ 1,
            # Casos donde no sabe o no respondió 
            (P3_17_01 %in% c(8, 9) & P3_17_02 %in% c(8, 9) & 
                 P3_17_03 %in% c(8, 9) & P3_17_04 %in% c(8, 9) & 
                 P3_17_05 %in% c(8, 9) & P3_17_06 %in% c(8, 9) & 
                 P3_17_07 %in% c(8, 9) & P3_17_08 %in% c(8, 9) & 
                 P3_17_09 %in% c(8, 9) & P3_17_11 %in% c(8, 9)) ~ NA_real_), 
        #  Violencia física
        violencia_fisica = case_when(
            # Casos donde no hubo violencia física
            !(P3_18_01 == 1 | P3_18_02 == 1 | P3_18_03 == 1 | P3_18_04 == 1 | 
                  P3_18_05 == 1 | P3_18_06 == 1 | P3_18_07 == 1 | P3_18_08 == 1 | 
                  P3_18_09 == 1 | P3_18_10 == 1 | P3_18_11 == 1 | P3_18_15 == 1) ~ 0,
            # Casos donde sí hubo violencia física
            (P3_18_01 == 1 | P3_18_02 == 1 | P3_18_03 == 1 | P3_18_04 == 1 | 
                 P3_18_05 == 1 | P3_18_06 == 1 | P3_18_07 == 1 | P3_18_08 == 1 | 
                 P3_18_09 == 1 | P3_18_10 == 1 | P3_18_11 == 1 | P3_18_15 == 1) ~ 1, 
            # Casos donde no sabe o no respondió 
            (P3_18_01 %in% c(8, 9) & P3_18_02 %in% c(8, 9) &
                 P3_18_03 %in% c(8, 9) & P3_18_04 %in% c(8, 9) & 
                 P3_18_05 %in% c(8, 9) & P3_18_06 %in% c(8, 9) & 
                 P3_18_07 %in% c(8, 9) & P3_18_08 %in% c(8, 9) & 
                 P3_18_09 %in% c(8, 9) & P3_18_10 %in% c(8, 9) & 
                 P3_18_11 %in% c(8, 9) & P3_18_15 %in% c(8, 9) ~ NA_real_)),
        # Violencia sexual 
        violencia_sexual = case_when(
            !(P3_17_10 == 1 |  P3_18_12 == 1 |  P3_18_13 == 1 | P3_18_14 == 1) ~ 0,  
            (P3_17_10 == 1 |  P3_18_12 == 1 |  P3_18_13 == 1 | P3_18_14 == 1) ~ 1, 
            (P3_17_10 %in% c(8, 9) & P3_18_12 %in% c(8, 9) &
                 P3_18_13 %in% c(8, 9) & P3_18_14 %in% c(8, 9)) ~ NA_real_)
    ) %>% 
    mutate(
        # Juntar las dos variables de tortura
        t_violencia = ifelse(
            violencia_psic == 1 | violencia_fisica == 1 | violencia_sexual == 1,
            1, 0)
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
                
# ---- Vectores de texto 
v_caption <- "Fuente: Encuesta Nacional de Población Privada de la Libertad 2021 (ENPOL).\nDatos procesados por Intersecta.org"
v_empty   <- ""

## 5.1. Lugar después de la detención ------------------------------------------

# ---- General 

# Proporciones 
df_data <- df_encuesta %>% 
    group_by(autoridad, lugar) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())

# Labels
v_title <- "¿A dónde se llevaron a las personas privadas de su libertad después de su detención?"
v_subtitle <- "Por autoridad que realizó la detención\n"

# Figura 
ggplot(df_data %>% filter(lugar != "Otros"), 
       aes(y = reorder(lugar, porcentaje), x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= -.1, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    facet_wrap(~autoridad) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .68),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(35)) +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# Guardar
ggsave(
    file = paste_figs("01_lugar_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo 

# Proporciones 
df_data <- df_encuesta               %>% 
    group_by(autoridad, sexo, lugar) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())

# Labels 
v_title <- "¿A dónde se llevaron a las personas privadas de su libertad después de su detención?"
v_subtitle <- "Por autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura
ggplot(df_data %>% filter(lugar != "Otros"), aes(x = reorder(lugar, porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .7),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(20)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# Guardar
ggsave(
    file = paste_figs("01_lugar_sexo_autoridad.png"), 
    width = 16.5, height = 13, units = "cm")

## 5.2. Tiempo entre detención y disposición -----------------------------------

# ---- General 

# Proporciones 
df_data <- df_encuesta              %>% 
    group_by(autoridad, tiempo)     %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()) %>% 
    mutate(tiempo = factor(tiempo, 
                           levels = c("Otros",
                                      "Más de 72 horas",
                                      "Más de 48 horas hasta 72 horas",
                                      "Más de 24 horas hasta 48 horas",
                                      "Más de 6 horas hasta 24 horas",
                                      "Más de 4 horas hasta 6 horas",
                                      "Más de 2 horas hasta 4 horas",
                                      "Más de 1 hora hasta 2 horas",
                                      "Entre 30 minutos y 1 hora",
                                      "Hasta 30 minutos")))

# Labels
v_title <- "¿Cuánto tiempo pasan retenidas las personas arrestadas antes de ser presentadas ante\nuna Agencia de Ministerio Público o Juez de lo penal?"
v_subtitle <- "Por autoridad que realizó la detención\n"

# Figura 
ggplot(df_data %>% filter(tiempo != "Otros"), 
       aes(y = tiempo, x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= -.1, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    facet_wrap(~autoridad) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .37),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(22)) +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# Guardar
ggsave(
    file = paste_figs("02_tiempo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo 

# Proporciones 
df_data <- df_encuesta                %>% 
    group_by(autoridad, sexo, tiempo) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop())   %>% 
    mutate(tiempo = factor(tiempo, 
                           levels = c("Otros",
                                      "Más de 72 horas",
                                      "Más de 48 horas hasta 72 horas",
                                      "Más de 24 horas hasta 48 horas",
                                      "Más de 6 horas hasta 24 horas",
                                      "Más de 4 horas hasta 6 horas",
                                      "Más de 2 horas hasta 4 horas",
                                      "Más de 1 hora hasta 2 horas",
                                      "Entre 30 minutos y 1 hora",
                                      "Hasta 30 minutos")))

# Labels 
v_title <- "¿Cuánto tiempo pasó entre la detención y la puesta a disposición de las personas\nprivadas de su libertad?"
v_subtitle <- "Por autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura
ggplot(df_data %>% filter(tiempo != "Otros"), 
       aes(x = tiempo, y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .36),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(20)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# Guardar
ggsave(
    file = paste_figs("02_tiempo_sexo_autoridad.png"), 
    width = 16.5, height = 13, units = "cm")

## 5.3. Uso de la fuerza -------------------------------------------------------

# ---- General 

# Códigos 
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P3_13_"))))
v_fuerza <- c("Le indicó que se detuviera",
              "Aplicó la fuerza para someterle",
              "Le esposó",
              "Utilizó un arma contudente para someterle",
              "Utilizó un arma no mortal para someterle",
              "Utilizó alguna sustancia química para someterle",
              "Le amenazó con un arma de fuego para someterle",
              "Le causó alguna lesión menor",
              "Le causó alguna lesión grave",
              "Le causó alguna lesión que pusiera en riesgo su vida",
              "Le disparó con arma de fuego",
              "Le hirió con arma de fuego")

# Proporciones para primer variable 
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>%
    group_by(autoridad, respuesta)                                          %>%
    summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_fuerza[1])

# Proporciones para el resto de las variables
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        rename(respuesta = v_codes[i])                                      %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>%
        group_by(autoridad, respuesta)                                      %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_fuerza[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Labels 
v_title <- "¿En qué porcentaje de las detenciones de las personas privadas de la libertad\nla autoridad hizo uso de la fuerza?"
v_subtitle <- "Por autoridad que realizó la detención\n"

# Figura
ggplot(df_data %>% filter(respuesta == "Sí"), 
       aes(y = reorder(tipo, porcentaje), x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= -.1, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    facet_wrap(~autoridad) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .9),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(35)) +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# Guardar
ggsave(
    file = paste_figs("03_fuerza_autoridad.png"), 
    width = 16.5, height = 13, units = "cm")

# ---- Por sexo

# Códigos 
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P3_13_"))))
v_fuerza <- c("Le indicó que se detuviera",
              "Aplicó la fuerza para someterle",
              "Le esposó",
              "Utilizó un arma contudente para someterle",
              "Utilizó un arma no mortal para someterle",
              "Utilizó alguna sustancia química para someterle",
              "Le amenazó con un arma de fuego para someterle",
              "Le causó alguna lesión menor",
              "Le causó alguna lesión grave",
              "Le causó alguna lesión que pusiera en riesgo su vida",
              "Le disparó con arma de fuego",
              "Le hirió con arma de fuego")

# Proporciones para primer variable 
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>%
    group_by(autoridad, sexo, respuesta)                                    %>%
    summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_fuerza[1])

# Proporciones para el resto de las variables
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        rename(respuesta = v_codes[i])                                      %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>%
        group_by(autoridad, sexo, respuesta)                                %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_fuerza[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Labels 
v_title <- "¿En qué porcentaje de las detenciones de las personas privadas de la libertad\nla autoridad hizo uso de la fuerza?"
v_subtitle <- "Por autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura
ggplot(df_data %>% filter(respuesta == "Sí"), 
       aes(x = reorder(tipo, porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .9),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(30)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# Guardar
ggsave(
    file = paste_figs("03_fuerza_sexo_autoridad.png"), 
    width = 16.5, height = 13, units = "cm")

## 5.4. Tipo de violencia ------------------------------------------------------

# ---- General 

v_codes <- c(names(df_encuesta$variables %>% select(starts_with("violencia_"))))
v_tipos <- c("Psicológica",
             "Física",
             "Sexual")

# Proporciones para primer variable 
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>%
    mutate(respuesta = codificar_siono2(as.character(respuesta)))           %>%
    group_by(autoridad, respuesta)                                          %>%
    summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

# Proporciones para el resto de las variables
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        rename(respuesta = v_codes[i])                                      %>%
        mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
        group_by(autoridad, respuesta)                                      %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Labels 
v_title <- "¿En qué porcentaje de las detenciones de las personas privadas de la libertad\nla autoridad ejerció violencia?"
v_subtitle <- "Por autoridad que realizó la detención\n"

# Figura
ggplot(df_data %>% filter(respuesta == "Sí"), 
       aes(x = autoridad, y = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= .5, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= -.6) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    facet_wrap(~tipo) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, .82),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(15))

# Guardar
ggsave(
    file = paste_figs("04_violencia_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo

v_codes <- c(names(df_encuesta$variables %>% select(starts_with("violencia_"))))
v_tipos <- c("Psicológica",
             "Física",
             "Sexual")

# Proporciones para primer variable 
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>%
    mutate(respuesta = codificar_siono2(as.character(respuesta)))           %>%
    group_by(autoridad, sexo, respuesta)                                    %>%
    summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

# Proporciones para el resto de las variables
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        rename(respuesta = v_codes[i])                                      %>%
        mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
        group_by(autoridad, sexo, respuesta)                                %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Labels 
v_title <- "¿Qué porcentaje de las personas privadas de su libertad vivieron violencia\ndurante su detención?"
v_subtitle <- "Por autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura
ggplot(df_data %>% filter(respuesta == "Sí"), 
       aes(x = reorder(tipo, -porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2.5, hjust=.5, vjust=-.6, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .86),
                       expand = c(0,0)) 

# Guardar
ggsave(
    file = paste_figs("04_violencia_sexo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.5. Situación de violencia específica --------------------------------------

# ---- General 

v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P3_17_"), starts_with("P3_18_"))))
v_eventos <- c(
    # Primera pregunta
    "Le amenazaron con levantarle cargos falsos", "Le amenazaron con matarlo(a)",
    "Le amenazaron con hacerle daño", "Le amenazaron con dañar a su familia",
    "Le hicieron otras amenazas", "Le presionaron para denunciar a alguien",
    "Le incomunicaron o aislaron", "Le pasearon en un automóvil",
    "Le hicieron daño a su familia", "Le desvistieron", "Le vendaron los ojos",
    # Segunda pregunta
    "Ataron su cuerpo", "Asfixia", "Tehuacanazo", "Patadas y golpes", 
    "Golpes con objetos", "Quemaduras", "Descargas eléctricas", "Aplastamiento",
    "Herida con arma blanca", "Le encajaron agujas", "Lesiones por arma de fuego", 
    "Acoso sexual, manoseo, exhibicionismo o intento de violación", "Lesiones en órganos sexuales", 
    "Violación sexual", "Otra agresión física")

# Proporciones
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>% 
    select(respuesta, autoridad)                                            %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>% 
    srvyr::group_by(autoridad, respuesta)                                   %>%
    srvyr::summarise(   
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%
    mutate(evento = v_eventos[1],
           tipo = codificar_tipo(evento))

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                                 %>% 
        rename(respuesta = v_codes[i])                                          %>%
        select(respuesta, autoridad)                                            %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>% 
        srvyr::group_by(autoridad, respuesta)                                   %>%
        srvyr::summarise(
            porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%
        mutate(evento = v_eventos[i],
               tipo = codificar_tipo(evento))
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar base
df_violencias <- df_data

# Labels
v_title <- "¿Qué porcentaje de las personas privadas de su libertad vivieron violencia\ndurante su detención?"
v_subtitle <- "Entre el momento de su detención y su puesta a disposición del Ministerio Público\nPor autoridad que realizó la detención\n"

# Figura 
ggplot(df_violencias %>% filter(respuesta == "Sí"), 
       aes(x = porcentaje, y = reorder(evento, porcentaje))) +
    facet_wrap(~autoridad,
               scales = "free_x") +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = autoridad,
                  fontface = "bold"),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              family = "Fira Sans", color = "#16213E") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .7), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = scales::wrap_format(38)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_autoridad.png"), 
    width = 16.5, height = 20, units = "cm")

# ---- Por sexo 

v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P3_17_"), starts_with("P3_18_"))))
v_eventos <- c(
    # Primera pregunta
    "Le amenazaron con levantarle cargos falsos", "Le amenazaron con matarlo(a)",
    "Le amenazaron con hacerle daño", "Le amenazaron con dañar a su familia",
    "Le hicieron otras amenazas", "Le presionaron para denunciar a alguien",
    "Le incomunicaron o aislaron", "Le pasearon en un automóvil",
    "Le hicieron daño a su familia", "Le desvistieron", "Le vendaron los ojos",
    # Segunda pregunta
    "Ataron su cuerpo", "Asfixia", "Tehuacanazo", "Patadas y golpes", 
    "Golpes con objetos", "Quemaduras", "Descargas eléctricas", "Aplastamiento",
    "Herida con arma blanca", "Le encajaron agujas", "Lesiones por arma de fuego", 
    "Acoso sexual, manoseo, exhibicionismo o intento de violación", "Lesiones en órganos sexuales", 
    "Violación sexual", "Otra agresión física")

# Proporciones
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>% 
    select(respuesta, autoridad, sexo)                                      %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>% 
    srvyr::group_by(autoridad, sexo, respuesta)                             %>%
    srvyr::summarise(   
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%
    mutate(evento = v_eventos[1],
           tipo = codificar_tipo(evento))

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                                 %>% 
        rename(respuesta = v_codes[i])                                          %>%
        select(respuesta, sexo, autoridad)                                      %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>% 
        srvyr::group_by(autoridad, sexo, respuesta)                             %>%
        srvyr::summarise(
            porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%
        mutate(evento = v_eventos[i],
               tipo = codificar_tipo(evento))
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar base
df_violencias_sexo <- df_data

# Labels
v_title <- "¿Qué porcentaje de las personas privadas de su libertad vivieron violencia\ndurante su detención?"
v_subtitle <- "Entre el momento de su detención y su puesta a disposición del Ministerio Público\nPor autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura 
ggplot(df_violencias_sexo %>% filter(respuesta == "Sí"), 
       aes(x = porcentaje, y = reorder(evento, porcentaje), fill = sexo)) +
    facet_wrap(~autoridad,
               scales = "free_x") +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  fontface = "bold", color = sexo),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              position = position_dodge(.9),
              family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = "Sexo de la persona privada de su libertad",
        caption  = v_caption) +
    # Tema 
    tema +
    guides(color = "none") +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .7), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = scales::wrap_format(38)) +
    theme(legend.box.margin = margin(0,150,0,0)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_sexo_autoridad.png"), 
    width = 16.5, height = 20, units = "cm")

## 5.6. Violencia física -------------------------------------------------------
# Correr sección 5.5 para obtener proporciones

# ---- General 

# Labels
v_title <- "¿Con qué frecuencia se agredió físicamente a las personas detenidas?"
v_subtitle <- "Por tipo de agresión y por tipo de autoridad\n"

# Figura 
ggplot(df_violencias %>% filter(respuesta == "Sí",
                                tipo == "Física"), 
       aes(x = porcentaje, y = reorder(evento, porcentaje))) +
    facet_wrap(~autoridad,
               scales = "free_x") +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = autoridad,
                  fontface = "bold"),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              family = "Fira Sans", color = "#16213E") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .64), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = scales::wrap_format(38)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_físicas_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo 

# Labels
v_title <- "¿Con qué frecuencia se agredió físicamente a las personas detenidas?"
v_subtitle <- "Por tipo de autoridad y sexo de la persona detenida\n"

# Figura 
ggplot(df_violencias_sexo %>% filter(respuesta == "Sí", 
                                     tipo == "Física"), 
       aes(x = porcentaje, y = reorder(evento, porcentaje), fill = sexo)) +
    facet_wrap(~autoridad,
               scales = "free_x") +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  fontface = "bold", color = sexo),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              position = position_dodge(.9),
              family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = "Sexo de la persona privada de su libertad",
        caption  = v_caption) +
    # Tema 
    tema +
    guides(color = "none") +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .65), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = scales::wrap_format(38)) +
    theme(legend.box.margin = margin(0,150,0,0)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_físicas_sexo_autoridad.png"), 
    width = 16.5, height = 12, units = "cm")

## 5.7. Violencia psicológica --------------------------------------------------
# Correr sección 5.5 para obtener proporciones

# ---- General 

# Labels
v_title <- "¿Con qué frecuencia se agredió psicológicamente a las personas detenidas?"
v_subtitle <- "Por tipo de agresión y por tipo de autoridad\n"

# Figura 
ggplot(df_violencias %>% filter(respuesta == "Sí",
                                tipo == "Psicológica"), 
       aes(x = porcentaje, y = reorder(evento, porcentaje))) +
    facet_wrap(~autoridad,
               scales = "free_x") +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = autoridad,
                  fontface = "bold"),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              family = "Fira Sans", color = "#16213E") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .66), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = scales::wrap_format(28)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_psicológicas_autoridad.png"), 
    width = 16.5, height = 12, units = "cm")

# ---- Por sexo 

# Labels
v_title <- "¿Con qué frecuencia se agredió psicológicamente a las personas detenidas?"
v_subtitle <- "Por tipo de autoridad y sexo de la persona detenida\n"

# Figura 
ggplot(df_violencias_sexo %>% filter(respuesta == "Sí", 
                                     tipo == "Psicológica"), 
       aes(x = porcentaje, y = reorder(evento, porcentaje), fill = sexo)) +
    facet_wrap(~autoridad,
               scales = "free_x") +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  fontface = "bold", color = sexo),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              position = position_dodge(.9),
              family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = "Sexo de la persona privada de su libertad",
        caption  = v_caption) +
    # Tema 
    tema +
    guides(color = "none") +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .7), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = scales::wrap_format(28)) +
    theme(legend.box.margin = margin(0,150,0,0)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_psicológicas_sexo_autoridad.png"), 
    width = 16.5, height = 12, units = "cm")

## 5.8. Violencia sexual -------------------------------------------------------
# Correr sección 5.5 para obtener proporciones

# ---- General 

# Labels
v_title <- "¿Con qué frecuencia se agredió sexualmente a las personas detenidas?"
v_subtitle <- "Por tipo de agresión y por tipo de autoridad\n"

# Figura 
ggplot(df_violencias %>% filter(respuesta == "Sí",
                                tipo == "Sexual"), 
       aes(x = porcentaje, y = reorder(autoridad, porcentaje))) +
    facet_wrap(~evento,
               scales = "free_x",
               labeller = label_wrap_gen(35)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = autoridad,
                  fontface = "bold"),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              family = "Fira Sans", color = "#16213E") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, NA), expand = expansion(mult = c(0, .15))) +
    scale_y_discrete(labels = scales::wrap_format(38)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_sexuales_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo 

# Labels
v_title <- "¿Con qué frecuencia se agredió sexualmente a las personas detenidas?"
v_subtitle <- "Por tipo de autoridad y sexo de la persona detenida\n"

# Figura 
ggplot(df_violencias_sexo %>% filter(respuesta == "Sí", 
                                     tipo == "Sexual"), 
       aes(x = porcentaje, y = reorder(autoridad, porcentaje), fill = sexo)) +
    facet_wrap(~evento,
               scales = "free_x",
               labeller = label_wrap_gen()) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  fontface = "bold", color = sexo),
              size=2.5, hjust=-.1, vjust=.5, angle = 0,  
              position = position_dodge(.9),
              family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = "Sexo de la persona privada de su libertad",
        caption  = v_caption) +
    # Tema 
    tema +
    guides(color = "none") +
    scale_fill_manual(values = v_colores) +
    scale_color_manual(values = v_colores) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, NA), expand = expansion(mult = c(0, .15))) +
    scale_y_discrete(labels = scales::wrap_format(38)) +
    theme(legend.box.margin = margin(0,75,0,0)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          axis.text.x = element_text(size = 7))

# Guardar 
ggsave(
    file = paste_figs("05_violencias_sexuales_sexo_autoridad.png"), 
    width = 16.5, height = 12, units = "cm")

# ---- Violación 

# Labels
v_title <- "¿Con qué frecuencia las personas detenidas fueron violadas por las autoridades?"
v_subtitle <- "Por tipo de autoridad y por sexo de la persona detenida\n"

# Figura 
ggplot(df_violencias_sexo %>% filter(respuesta == "Sí",
                                evento == "Violación sexual"), 
       aes(y = porcentaje, x = reorder(sexo, -porcentaje))) +
    facet_wrap(~autoridad,
               scales = "free_x",
               labeller = label_wrap_gen(35)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = autoridad,
                  fontface = "bold"),
              size=2.5, hjust=.5, vjust=-.7, angle = 0,  
              family = "Fira Sans", color = "#16213E") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, NA), expand = expansion(mult = c(0, .13))) +
    scale_x_discrete(labels = scales::wrap_format(38))

# Guardar 
ggsave(
    file = paste_figs("05_violación_sexo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.9. Contexto de la detención -----------------------------------------------

# ---- General 

# Proporciones 
df_data <- df_encuesta     %>% 
    filter(!is.na(P3_10))  %>% 
    mutate(contexto = case_when(
        P3_10 == 1 ~ "Realizando la conducta",
        P3_10 == 2 ~ "Inmediantamente después de realizar la conducta",
        P3_10 == 3 ~ "Con una orden de detención",
        (P3_10 == 4 & P3_11 == 1) ~ "Después de una inspección",
        (P3_10 == 4 & P3_11 == 2) ~ "Después de una inspección",
        (P3_10 == 5 & P3_11 == 1) ~ "Sacándola del lugar en donde estaba",
        (P3_10 == 5 & P3_11 == 2) ~ "Mientras iba pasando por la calle",
        TRUE ~ "Otros"
    ))                         %>% 
    group_by(autoridad, contexto)   %>% 
    summarise(
        porcentaje = survey_prop()
    )

# Labels
v_title <- "¿Cómo fueron detenidas las personas privadas de su libertad?"
v_subtitle <- "Por autoridad que realizó la detención\n"

# Figura 
ggplot(df_data %>% filter(contexto != "Otros"), 
       aes(y = factor(contexto, levels = c(
           "Otros",
           "Mientras iba pasando por la calle",
           "Sacándola del lugar en donde estaba",
           "Después de una inspección",
           "Inmediantamente después de realizar la conducta",
           "Realizando la conducta",
           "Con una orden de detención"
       )), x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= -.1, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    facet_wrap(~autoridad) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .3),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# Guardar
ggsave(
    file = paste_figs("06_contexto_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo 

# Proporciones 
df_data <- df_encuesta                    %>% 
    filter(!is.na(P3_10))                 %>% 
    mutate(contexto = case_when(
        P3_10 == 1 ~ "Realizando la conducta",
        P3_10 == 2 ~ "Inmediantamente después de realizar la conducta",
        P3_10 == 3 ~ "Con una orden de detención",
        (P3_10 == 4 & P3_11 == 1) ~ "Después de una inspección",
        (P3_10 == 4 & P3_11 == 2) ~ "Después de una inspección",
        (P3_10 == 5 & P3_11 == 1) ~ "Sacándolas del lugar en donde estaba",
        (P3_10 == 5 & P3_11 == 2) ~ "Mientras iba pasando por la calle",
        TRUE ~ "Otros"
    ))                                    %>% 
    group_by(autoridad, sexo, contexto)   %>% 
    summarise(
        porcentaje = survey_prop()
    )

# Labels 
v_title <- "¿Cómo fueron detenidas las personas privadas de su libertad?"
v_subtitle <- "Por autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura
ggplot(df_data %>% filter(contexto != "Otros"), 
       aes(x = factor(contexto, levels = c(
           "Otros",
           "Mientras iba pasando por la calle",
           "Sacándolas del lugar en donde estaba",
           "Después de una inspección",
           "Inmediantamente después de realizar la conducta",
           "Realizando la conducta",
           "Con una orden de detención"
       )), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2.5, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .31),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(25)) +
    coord_flip() +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# Guardar
ggsave(
    file = paste_figs("06_contexto_sexo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.10. Motivo -----------------------------------------------------------------

# ---- General

# Proporciones
df_data <- df_encuesta          %>% 
    group_by(autoridad, motivo) %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# Labels 
v_title <- "¿Por qué motivo las personas afirman estar privadas de su libertad?"
v_subtitle <- "Por autoridad que realizó su detención\n"

# Figura 
ggplot(df_data %>% filter(motivo != "Otros"), aes(y = reorder(motivo, porcentaje), x = porcentaje)) +
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
    facet_wrap(~autoridad) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .71),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# ---- Guardar
ggsave(
    file = paste_figs("07_motivo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo

# Proporciones
df_data <- df_encuesta                 %>% 
    group_by(autoridad, sexo, motivo)  %>% 
    summarise(
        total = survey_total(),
        porcentaje = survey_prop()
    )

# Labels 
v_title <- "¿Por qué motivo las personas afirman estar privadas de su libertad?"
v_subtitle <- "Por autoridad que realizó su detención y sexo de la persona privada de su libertad\n"

# Figura 
ggplot(df_data %>% filter(motivo != "Otros"), aes(x = reorder(motivo, porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .77),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(20)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 7),
          axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# ---- Guardar
ggsave(
    file = paste_figs("07_motivo_sexo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

## 5.11. Inspección -------------------------------------------------------------

# ---- General 

# Variables 
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P3_12_"))))
v_tipos <- c("Le desvistió",
             "Le dijo qué objeto buscaba",
             "Encontró el objeto que buscaba o algún otro objeto ilegal",
             "Le sembró algún objeto",
             "Videograbo la inspección")

# Proporciones para primer variable 
df_data <- df_encuesta                                                      %>%
    filter(P3_10 == 4)                                                      %>% 
    rename(respuesta = v_codes[1])                                          %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>%
    group_by(autoridad, respuesta)                                          %>%
    summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

# Proporciones para el resto de las variables
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(P3_10 == 4)                                                  %>% 
        rename(respuesta = v_codes[i])                                      %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>%
        group_by(autoridad, respuesta)                                      %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Labels 
v_title <- "¿Cómo fue realizada la inspección a las personas privadas de su libertad durante su detención?"
v_subtitle <- "Por autoridad que realizó la detención\n"

# Figura
ggplot(df_data %>% filter(respuesta == "Sí"), 
       aes(y = reorder(tipo, porcentaje), x = porcentaje)) +
    geom_col(fill = "#16213E") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), 
                  fontface = "bold"),
              size= 2.5, hjust= -.1, 
              angle = 0, family = "Fira Sans",
              color = "#16213E",
              vjust= .5) +
    # geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"),
    #                y = ifelse(df_data$porcentaje <= .01,
    #                           df_data$porcentaje+.015, df_data$porcentaje)),
    #            size=2, hjust=.5, 
    #            angle = 0, fill = "white",  family = "Fira Sans",
    #            color = "#16213E",
    #            vjust= .5) +
    facet_wrap(~autoridad) +
    labs(
        title = v_title,
        subtitle = v_subtitle,
        caption = v_caption,
        x = v_empty,
        y = v_empty,
        fill = "Sexo de la persona privada de su libertad"
    ) +
    tema +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, .48),
                       expand = c(0,0)) +
    scale_y_discrete(labels = scales::wrap_format(30)) +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank())

# Guardar
ggsave(
    file = paste_figs("08_inspección_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# ---- Por sexo

# Variables 
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("P3_12_"))))
v_tipos <- c("Le desvistió",
             "Le dijo qué objeto buscaba",
             "Encontró el objeto que buscaba o algún otro objeto ilegal",
             "Le sembró algún objeto",
             "Videograbo la inspección")

# Proporciones para primer variable 
df_data <- df_encuesta                                                      %>%
    filter(P3_10 == 4)                                                      %>% 
    rename(respuesta = v_codes[1])                                          %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))           %>%
    group_by(autoridad, sexo, respuesta)                                    %>%
    summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

# Proporciones para el resto de las variables
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(P3_10 == 4)                                                  %>% 
        rename(respuesta = v_codes[i])                                      %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>%
        group_by(autoridad, sexo, respuesta)                                %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Labels 
v_title <- "¿Cómo fue realizada la inspección a las personas privadas de su libertad durante su detención?"
v_subtitle <- "Por autoridad que realizó la detención y sexo de la persona detenida\n"

# Figura
ggplot(df_data %>% filter(respuesta == "Sí"), 
       aes(x = reorder(tipo, porcentaje), y = porcentaje, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo,
                  color = sexo, fontface = "bold"),
              position = position_dodge(1), size=2, hjust=-.15, vjust=0.5, angle = 0,  
              family = "Fira Sans") +
    facet_wrap(~autoridad) +
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
                       limits = c(0, .59),
                       expand = c(0,0)) +
    scale_x_discrete(labels = scales::wrap_format(30)) +
    coord_flip() +
    theme(axis.line.x.bottom = element_line(color="#0F3460", size = .3),
          axis.line.y.left = element_blank(),
          legend.box.margin = margin(0,75,0,0))

# Guardar
ggsave(
    file = paste_figs("08_inspección_sexo_autoridad.png"), 
    width = 16.5, height = 11, units = "cm")

# FIN. -------------------------------------------------------------------------
