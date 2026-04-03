####### Codificación Casillas - Municipios
library(readxl)
library(dplyr)
library(stringr)

casillas_bcs <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/PJ25_BCS_Listado-Ubicacion-IntegracionCasillas.xlsx')
casillas_hgo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/PJ25_HGO_Listado-Ubicacion-IntegracionCasillas-1.xlsx')
casillas_qroo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/PJ25_QROO_Listado-Ubicacion-IntegracionCasillas.xlsx')
casillas_gro <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/PJ25_GRO_Listado-Ubicacion-IntegracionCasillas.xlsx')

tabla_equivalencias <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/tablaEquivalenciasSeccionalesDesde1994.xlsx', sheet = 2)

#Limpiar observaciones
casillas_bcs <- casillas_bcs %>%
  filter(Entidad != "Entidad")

casillas_hgo <- casillas_hgo %>%
  filter(Entidad == "HIDALGO")

casillas_qroo <- casillas_qroo %>%
  filter(Entidad == "QUINTANA ROO")

casillas_gro <- casillas_gro %>%
  filter(Entidad == "GUERRERO")

#Creamos tabla de equivalencias para cada estado

tabla_equivalencia_bcs <- tabla_equivalencias %>%
  filter(edo == "bcs")

tabla_equivalencia_hgo <- tabla_equivalencias %>%
  filter(edo == "hgo")

tabla_equivalencia_gro <- tabla_equivalencias %>%
  filter(edo == "gue")

tabla_equivalencia_qroo <- tabla_equivalencias %>%
  filter(edo == "qui")

glimpse(casillas_bcs)
glimpse(tabla_equivalencia_bcs)

#Limpiamos las bases de casillas

casillas_bcs <- casillas_bcs %>%
  mutate(
    Municipio = str_remove(Municipio, "^\\d+\\) "),
    Localidad = str_remove(Localidad, "^\\d+\\) "),
    Seccion = as.numeric(Sección)
  )

casillas_hgo <- casillas_hgo %>%
  mutate(
    Municipio = str_remove(Municipio, "^\\d+\\) "),
    Localidad = str_remove(Localidad, "^\\d+\\) "),
    Seccion = as.numeric(Sección)
  )

casillas_gro <- casillas_gro %>%
  mutate(
    Municipio = str_remove(Municipio, "^\\d+\\) "),
    Localidad = str_remove(Localidad, "^\\d+\\) "),
    Seccion = as.numeric(Sección)
  )

casillas_qroo <- casillas_qroo %>%
  mutate(
    Municipio = str_remove(Municipio, "^\\d+\\) "),
    Localidad = str_remove(Localidad, "^\\d+\\) "),
    Seccion = as.numeric(Sección)
  )

#Cambiar unidad de análisis de casillas a secciones
secciones_bcs <- casillas_bcs %>%
  distinct(Entidad, Municipio, Localidad, Seccion)

secciones_gro <- casillas_gro %>%
  distinct(Entidad, Municipio, Localidad, Seccion)

secciones_hgo <- casillas_hgo %>%
  distinct(Entidad, Municipio, Localidad, Seccion)

secciones_qroo <- casillas_qroo %>%
  distinct(Entidad, Municipio, Localidad, Seccion)

#Pequeño error en La Paz
secciones_bcs <- secciones_bcs %>%
  mutate(
    Municipio = if_else(Municipio == "LA", "LA PAZ", Municipio)
  )

##Función: 
#Supuesto esencial: dentro de cada municipio, la localidad que concentre más secciones, se clasifica como la cabecera municipal.
#Si no hay alguna localidad con muchas secciones, es 0 automaticamente en el municipio.

normalizar_texto <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9 ]", " ")
  x <- str_squish(x)
  x
}

clasificar_cabecera <- function(secciones_df) {
  
  # 1. Contar secciones por localidad dentro de cada municipio
  ranking_localidades <- secciones_df %>%
    group_by(Municipio, Localidad) %>%
    summarise(
      n_secciones = n_distinct(Seccion),
      .groups = "drop"
    )
  
  # 2. Identificar máximo por municipio
  cabeceras <- ranking_localidades %>%
    group_by(Municipio) %>%
    mutate(
      max_secciones = max(n_secciones, na.rm = TRUE),
      
      cabecera = case_when(
        max_secciones == 1 ~ NA_integer_,                 # no se puede identificar
        n_secciones == max_secciones ~ 1L,                # localidad con más secciones
        TRUE ~ 0L
      )
    ) %>%
    ungroup()
  
  # 3. Pegar resultado a la base original
  salida <- secciones_df %>%
    left_join(
      cabeceras %>%
        select(Municipio, Localidad, n_secciones, max_secciones, cabecera),
      by = c("Municipio", "Localidad")
    ) %>%
    arrange(Municipio, Seccion)
  
  return(salida)
}

# Aplicar
bcs_cod  <- clasificar_cabecera(secciones_bcs)
hgo_cod  <- clasificar_cabecera(secciones_hgo)
gro_cod  <- clasificar_cabecera(secciones_gro)
qroo_cod <- clasificar_cabecera(secciones_qroo)

# Revisión
#Localidad que fue marcada como cabecera
bcs_cod %>%
  filter(cabecera == 1) %>%
  distinct(Municipio, Localidad, n_secciones)%>%
  arrange(desc(n_secciones))
  
gro_cod %>%
  filter(cabecera == 1) %>%
  distinct(Municipio, Localidad, n_secciones) %>%
  arrange(desc(n_secciones))

qroo_cod %>%
  filter(cabecera == 1) %>%
  distinct(Municipio, Localidad, n_secciones) %>%
  arrange(desc(n_secciones))

hgo_cod %>%
  filter(cabecera == 1) %>%
  distinct(Municipio, Localidad, n_secciones) %>%
  arrange(desc(n_secciones))


############ Parte II: con documentos oficiales
#Secciones INEGI: INE -> Cartografía Electoral -> Productos Cartográficos -> CATÁLOGO DE LOCALIDADES (CLOC)
library(dplyr)
library(stringr)
library(tibble)
library(writexl)
library(stringdist)
library(readxl)

secciones_bcs <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/BCS_Catálogo de Localidades con Sección.xlsx')
secciones_hgo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/HGO_Catálogo de Localidades con Sección.xlsx')
secciones_qroo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/QROO_Catálogo de Localidades con Sección.xlsx')
secciones_gro <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/GRO_Catálogo de Localidades con Sección.xlsx')
secciones_ags <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/AGS_Catálogo de Localidades con Sección.xlsx')
secciones_bc <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/BC_Catálogo de Localidades con Sección.xlsx')
secciones_cam <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CAM_Catálogo de Localidades con Sección.xlsx')
secciones_cdmx <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CDMX_Catálogo de Localidades con Sección.xlsx')
secciones_chi <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CHI_Catálogo de Localidades con Sección.xlsx')
secciones_chih <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CHIH_Catálogo de Localidades con Sección.xlsx')
secciones_coa <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/COA_Catálogo de Localidades con Sección.xlsx')
secciones_col <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/COL_Catálogo de Localidades con Sección.xlsx')
secciones_dgo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/DGO_Catálogo de Localidades con Sección.xlsx')
secciones_edo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/EDO_Catálogo de Localidades con Sección.xlsx')
secciones_gto <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/GTO_Catálogo de Localidades con Sección.xlsx')
secciones_jal <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/JAL_Catálogo de Localidades con Sección.xlsx')
secciones_mich <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/MICH_Catálogo de Localidades con Sección.xlsx')
secciones_mor <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/MOR_Catálogo de Localidades con Sección.xlsx')
secciones_nay <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/NAY_Catálogo de Localidades con Sección.xlsx')
secciones_nl <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/NL_Catálogo de Localidades con Sección.xlsx')
secciones_oax <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/OAX_Catálogo de Localidades con Sección.xlsx')
secciones_pue <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/PUE_Catálogo de Localidades con Sección.xlsx')
secciones_qro <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/QRO_Catálogo de Localidades con Sección.xlsx')
secciones_sin <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/SIN_Catálogo de Localidades con Sección.xlsx')
secciones_slp <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/SLP_Catálogo de Localidades con Sección.xlsx')
secciones_son <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/SON_Catálogo de Localidades con Sección.xlsx')
secciones_tab <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/TAB_Catálogo de Localidades con Sección.xlsx')
secciones_tam <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/TAM_Catálogo de Localidades con Sección.xlsx')
secciones_tla <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/TLA_Catálogo de Localidades con Sección.xlsx')
secciones_ver <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/VER_Catálogo de Localidades con Sección.xlsx')
secciones_yuc <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/YUC_Catálogo de Localidades con Sección.xlsx')
secciones_zac <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/ZAC_Catálogo de Localidades con Sección.xlsx')

tabla_equivalencias <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/tablaEquivalenciasSeccionalesDesde1994.xlsx', sheet = 2)


#Datos sobre municipios: Catálogo Único de Claves de Áreas Geoestadísticas Estatales, Municipales y Localidades Nivel AGEEM
ageem_bcs <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/BCS_AGEEML_20263181142838.xlsx')
ageem_hgo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/HGO_AGEEML_20263181143880.xlsx')
ageem_qroo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/QRO_AGEEML_20263181143134.xlsx')
ageem_gro <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/GRO_AGEEML_20263181142218.xlsx')
ageem_ags <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/AGS_AGEEML_20263291550830.xlsx', skip = 3)
ageem_bc <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/BC_AGEEML_20263291551325.xlsx', skip = 3)
ageem_cam <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CAM_AGEEML_20263291552233.xlsx', skip = 3)
ageem_cdmx <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CDMX_AGEEML_20263291553683.xlsx', skip = 3)
ageem_chi <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CHI_AGEEML_20263291553688.xlsx', skip = 3)
ageem_chih <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/CHIH_AGEEML_20263291553645.xlsx', skip = 3)
ageem_coa <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/COA_AGEEML_20263291552673.xlsx', skip = 3)
ageem_col <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/COL_AGEEML_20263291552319.xlsx', skip = 3)
ageem_dgo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/DGO_AGEEML_20263291554414.xlsx', skip = 3)
ageem_edo <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/EDO_AGEEML_202632915552.xlsx', skip = 3)
ageem_gto <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/GTO_AGEEML_20263291555773.xlsx', skip = 3)
ageem_jal <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/JAL_AGEEML_20263291555366.xlsx', skip = 3)
ageem_mich <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/MICH_AGEEML_20263291556718.xlsx', skip = 3)
ageem_mor <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/MOR_AGEEML_20263291556898.xlsx', skip = 3)
ageem_nay <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/NAY_AGEEML_20263291556922.xlsx', skip = 3)
ageem_nl <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/NL_AGEEML_202632915567.xlsx', skip = 3)
ageem_oax <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/OAX_AGEEML_20263291557279.xlsx', skip = 3)
ageem_pue <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/PUE_AGEEML_2026329155737.xlsx', skip = 3)
ageem_pue <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/QRO_AGEEML_20263291557788.xlsx', skip = 3)
ageem_qro <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/QRO_AGEEML_20263291557788.xlsx', skip = 3)
ageem_sin <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/SIN_AGEEML_20263291558999.xlsx', skip = 3)
ageem_slp <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/SLP_AGEEML_20263291558365.xlsx', skip = 3)
ageem_son <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/SON_AGEEML_2026329155812.xlsx', skip = 3)
ageem_tab <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/TAB_AGEEML_2026329155910.xlsx', skip = 3)
ageem_tam <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/TAM_AGEEML_20263291559925.xlsx', skip = 3)
ageem_tla <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/TLA_AGEEML_20263291559326.xlsx', skip = 3)
ageem_ver <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/VER_AGEEML_2026329160130.xlsx', skip = 3)
ageem_yuc <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/YUC_AGEEML_2026329160677.xlsx', skip = 3)
ageem_zac <- read_xlsx('/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Catalogos oficiales/ZAC_AGEEML_2026329160258.xlsx', skip = 3)

str(ageem_gro)
#Detección de cabecera municipal: El manual del catálogo contiene la siguiente descripción:
#Número compuesto por cuatro dígitos, asignada a la cabecera municipal, generalmente le corresponde la clave 0001 salvo algunas excepciones. 
# Por lo que podemos detectar la localidad de la cabecera municipal a través de estos archivos.
#La estrategia sería: hacer merge con las secciones electorales, luego detectar el codigo, si es 0001, entonces es cabecera.

#Ejemplo de visualización de estructura de datos de AGEEM y secciones
str(ageem_bcs)
str(secciones_bcs)

# ==========================================================
# 1) FUNCIONES BASE
# ==========================================================
normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- str_replace_all(x, '"', "")
  x <- str_replace_all(x, "\\r\\n|\\n|\\r", " ")
  x <- str_replace_all(x, "[^A-Z0-9 ]", " ")
  x <- str_squish(x)
  x
}

simplificar_nombre <- function(x) {
  x <- normalizar_texto(x)
  x <- str_replace(x, "^CIUDAD ", "")
  x <- str_replace(x, "^VILLA DE ", "")
  x <- str_replace_all(x, "\\b(DE|DEL|LA|LAS|LOS|EL)\\b", " ")
  x <- str_squish(x)
  x
}

# ==========================================================
# 2) LIMPIEZA DE CLOC
# ==========================================================
limpiar_cloc <- function(df, alias_municipios = NULL) {
  
  out <- df %>%
    mutate(
      ENTIDAD            = as.character(ENTIDAD),
      MUNICIPIO          = as.character(MUNICIPIO),
      LOCALIDAD          = as.character(LOCALIDAD),
      SECCION            = as.character(SECCION),
      
      `NOMBRE ENTIDAD`   = normalizar_texto(`NOMBRE ENTIDAD`),
      `NOMBRE MUNICIPIO` = normalizar_texto(`NOMBRE MUNICIPIO`),
      `NOMBRE LOCALIDAD` = normalizar_texto(`NOMBRE LOCALIDAD`),
      TIPO               = normalizar_texto(TIPO),
      
      CVE_ENT      = str_pad(ENTIDAD, 2, pad = "0"),
      CVE_MUN_CLOC = str_pad(MUNICIPIO, 3, pad = "0"),
      CVE_LOC_CLOC = str_pad(LOCALIDAD, 4, pad = "0"),
      SECCION_NUM  = suppressWarnings(as.numeric(SECCION))
    ) %>%
    filter(!is.na(SECCION_NUM))
  
  # aplicar alias de municipios si existen
  if (!is.null(alias_municipios)) {
    out <- out %>%
      left_join(alias_municipios, by = c("CVE_ENT", "NOMBRE MUNICIPIO")) %>%
      mutate(
        NOMBRE_MUNICIPIO_AJUSTADO = coalesce(municipio_ageem, `NOMBRE MUNICIPIO`)
      ) %>%
      select(-municipio_ageem)
  } else {
    out <- out %>%
      mutate(
        NOMBRE_MUNICIPIO_AJUSTADO = `NOMBRE MUNICIPIO`
      )
  }
  
  out %>%
    mutate(
      mun_norm  = normalizar_texto(NOMBRE_MUNICIPIO_AJUSTADO),
      mun_simpl = simplificar_nombre(NOMBRE_MUNICIPIO_AJUSTADO),
      loc_norm  = normalizar_texto(`NOMBRE LOCALIDAD`),
      loc_simpl = simplificar_nombre(`NOMBRE LOCALIDAD`)
    )
}

# ==========================================================
# 3) LIMPIEZA DE AGEEM
# ==========================================================
limpiar_ageem <- function(df) {
  df %>%
    mutate(
      CVE_ENT = str_pad(as.character(CVE_ENT), 2, pad = "0"),
      CVE_MUN = str_pad(as.character(CVE_MUN), 3, pad = "0"),
      CVE_LOC = str_pad(as.character(CVE_LOC), 4, pad = "0"),
      
      NOM_ENT = normalizar_texto(NOM_ENT),
      NOM_MUN = normalizar_texto(NOM_MUN),
      NOM_LOC = normalizar_texto(NOM_LOC),
      
      mun_norm  = normalizar_texto(NOM_MUN),
      mun_simpl = simplificar_nombre(NOM_MUN),
      cab_norm  = normalizar_texto(NOM_LOC),
      cab_simpl = simplificar_nombre(NOM_LOC)
    ) %>%
    distinct()
}

# ==========================================================
# 4) MATCH AUTOMATICO DE CABECERA DENTRO DE CADA MUNICIPIO
# ==========================================================
detectar_cabecera_localidad <- function(base0, max_dist = 3, alias_localidades = NULL) {
  
  candidatos <- base0 %>%
    distinct(
      CVE_ENT, mun_norm, mun_simpl,
      `NOMBRE MUNICIPIO`,
      `NOMBRE LOCALIDAD`,
      loc_norm, loc_simpl,
      CABECERA_OFICIAL, cab_norm, cab_simpl
    ) %>%
    mutate(
      match_exacto = !is.na(cab_norm)  & loc_norm  == cab_norm,
      match_simpl  = !is.na(cab_simpl) & loc_simpl == cab_simpl,
      match_cont_1 = !is.na(cab_simpl) & str_detect(loc_simpl, fixed(cab_simpl)),
      match_cont_2 = !is.na(cab_simpl) & str_detect(cab_simpl, fixed(loc_simpl)),
      dist_lv      = if_else(
        !is.na(cab_simpl),
        stringdist::stringdist(loc_simpl, cab_simpl, method = "lv"),
        NA_real_
      )
    ) %>%
    mutate(
      score = case_when(
        match_exacto ~ 1,
        match_simpl ~ 2,
        match_cont_1 | match_cont_2 ~ 3,
        !is.na(dist_lv) & dist_lv <= max_dist ~ 4,
        TRUE ~ 99
      )
    )
  
  elegidos <- candidatos %>%
    group_by(CVE_ENT, mun_norm, mun_simpl) %>%
    mutate(
      mejor_score = min(score, na.rm = TRUE),
      es_mejor = score == mejor_score,
      mejor_dist = if_else(
        any(es_mejor & !is.na(dist_lv)),
        min(dist_lv[es_mejor], na.rm = TRUE),
        NA_real_
      ),
      es_mejor_final = case_when(
        mejor_score < 4 ~ es_mejor,
        mejor_score == 4 ~ es_mejor & dist_lv == mejor_dist,
        TRUE ~ FALSE
      ),
      n_mejores_final = sum(es_mejor_final, na.rm = TRUE),
      
      localidad_cabecera_auto = case_when(
        mejor_score == 99 ~ NA_character_,
        n_mejores_final == 1 ~ `NOMBRE LOCALIDAD`[es_mejor_final][1],
        TRUE ~ NA_character_
      ),
      
      loc_cab_auto_norm = case_when(
        mejor_score == 99 ~ NA_character_,
        n_mejores_final == 1 ~ loc_norm[es_mejor_final][1],
        TRUE ~ NA_character_
      ),
      
      metodo_auto = case_when(
        mejor_score == 1 & n_mejores_final == 1 ~ "exacto",
        mejor_score == 2 & n_mejores_final == 1 ~ "simplificado",
        mejor_score == 3 & n_mejores_final == 1 ~ "contencion",
        mejor_score == 4 & n_mejores_final == 1 ~ "distancia",
        mejor_score == 99 ~ "sin_match",
        TRUE ~ "ambiguo"
      )
    ) %>%
    ungroup() %>%
    distinct(
      CVE_ENT, mun_norm, mun_simpl,
      localidad_cabecera_auto,
      loc_cab_auto_norm,
      metodo_auto
    )
  
  if (is.null(alias_localidades)) {
    return(
      elegidos %>%
        rename(
          localidad_cabecera_final = localidad_cabecera_auto,
          loc_cab_final_norm = loc_cab_auto_norm,
          metodo_final = metodo_auto
        )
    )
  }
  
  elegidos %>%
    left_join(alias_localidades, by = c("CVE_ENT", "mun_norm")) %>%
    mutate(
      localidad_cabecera_final = case_when(
        !is.na(localidad_manual) ~ localidad_manual,
        TRUE ~ localidad_cabecera_auto
      ),
      loc_cab_final_norm = case_when(
        !is.na(localidad_manual) ~ normalizar_texto(localidad_manual),
        TRUE ~ loc_cab_auto_norm
      ),
      metodo_final = case_when(
        !is.na(localidad_manual) ~ "manual",
        TRUE ~ metodo_auto
      )
    ) %>%
    distinct(
      CVE_ENT, mun_norm, mun_simpl,
      localidad_cabecera_final,
      loc_cab_final_norm,
      metodo_final
    )
}

# ==========================================================
# 5) FUNCION PRINCIPAL
# ==========================================================
construir_base_cabeceras_auto <- function(secciones_df,
                                          ageem_df,
                                          max_dist = 3,
                                          alias_municipios = NULL,
                                          alias_localidades = NULL) {
  
  cloc  <- limpiar_cloc(secciones_df, alias_municipios = alias_municipios)
  ageem <- limpiar_ageem(ageem_df)
  
  cabeceras_oficiales <- ageem %>%
    filter(CVE_LOC == "0001") %>%
    distinct(CVE_ENT, mun_norm, .keep_all = TRUE) %>%
    transmute(
      CVE_ENT,
      mun_norm,
      mun_simpl,
      MUNICIPIO_AGEEM  = NOM_MUN,
      CABECERA_OFICIAL = NOM_LOC,
      cab_norm,
      cab_simpl
    )
  
  base0 <- cloc %>%
    left_join(
      cabeceras_oficiales,
      by = c("CVE_ENT", "mun_norm", "mun_simpl")
    )
  
  matching_final <- detectar_cabecera_localidad(
    base0,
    max_dist = max_dist,
    alias_localidades = alias_localidades
  )
  
  base <- base0 %>%
    left_join(
      matching_final,
      by = c("CVE_ENT", "mun_norm", "mun_simpl")
    ) %>%
    mutate(
      cabecera = case_when(
        is.na(CABECERA_OFICIAL) ~ NA_integer_,
        metodo_final %in% c("sin_match", "ambiguo") ~ NA_integer_,
        loc_norm == loc_cab_final_norm ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    arrange(`NOMBRE MUNICIPIO`, SECCION_NUM, `NOMBRE LOCALIDAD`)
  
  municipios_revision <- base %>%
    group_by(CVE_ENT, mun_norm, `NOMBRE MUNICIPIO`) %>%
    summarise(
      CABECERA_OFICIAL = first(CABECERA_OFICIAL),
      metodo_final = first(metodo_final),
      tiene_cabecera = any(cabecera == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!tiene_cabecera | metodo_final %in% c("sin_match", "ambiguo"))
  
  diagnostico <- tibble(
    obs_totales = nrow(base),
    secciones_unicas = n_distinct(paste(base$CVE_ENT, base$CVE_MUN_CLOC, base$SECCION_NUM)),
    municipios_unicos = n_distinct(base$mun_norm),
    municipios_con_cabecera_oficial = n_distinct(base$mun_norm[!is.na(base$CABECERA_OFICIAL)]),
    municipios_match_exacto = n_distinct(base$mun_norm[base$metodo_final == "exacto"]),
    municipios_match_simpl = n_distinct(base$mun_norm[base$metodo_final == "simplificado"]),
    municipios_match_cont = n_distinct(base$mun_norm[base$metodo_final == "contencion"]),
    municipios_match_dist = n_distinct(base$mun_norm[base$metodo_final == "distancia"]),
    municipios_sin_match_o_ambiguos = n_distinct(base$mun_norm[base$metodo_final %in% c("sin_match", "ambiguo")]),
    cabecera_1 = sum(base$cabecera == 1, na.rm = TRUE),
    cabecera_0 = sum(base$cabecera == 0, na.rm = TRUE)
  )
  
  list(
    base = base,
    diagnostico = diagnostico,
    cabeceras_oficiales = cabeceras_oficiales,
    matching_final = matching_final,
    municipios_revision = municipios_revision,
    cloc = cloc,
    ageem = ageem
  )
}

# ==========================================================
# 6) COLAPSAR A SECCION PURA
# ==========================================================
colapsar_a_seccion <- function(df) {
  df %>%
    group_by(CVE_ENT, CVE_MUN_CLOC, SECCION_NUM) %>%
    summarise(
      `NOMBRE ENTIDAD`   = first(`NOMBRE ENTIDAD`),
      `NOMBRE MUNICIPIO` = first(`NOMBRE MUNICIPIO`),
      cabecera = case_when(
        all(is.na(cabecera)) ~ NA_integer_,
        any(cabecera == 1, na.rm = TRUE) ~ 1L,
        TRUE ~ 0L
      ),
      n_localidades_en_seccion = n_distinct(CVE_LOC_CLOC),
      n_localidades_cabecera_en_seccion = n_distinct(CVE_LOC_CLOC[cabecera == 1]),
      tipo_seccion = case_when(
        any(TIPO == "URBANO A", na.rm = TRUE) ~ "URBANO A",
        any(TIPO == "RURAL", na.rm = TRUE) ~ "RURAL",
        TRUE ~ first(TIPO)
      ),
      .groups = "drop"
    ) %>%
    arrange(CVE_ENT, CVE_MUN_CLOC, SECCION_NUM)
}

#Imputaciones manuales
alias_municipios_qroo <- tibble::tribble(
  ~CVE_ENT, ~`NOMBRE MUNICIPIO`, ~municipio_ageem,
  "23",     "PLAYA DEL CARMEN",  "SOLIDARIDAD"
)

alias_localidades_hgo <- tibble::tribble(
  ~CVE_ENT, ~mun_norm,                    ~localidad_manual,
  "13",     "SAN AGUSTIN METZQUITITLAN",  "SAN AGUSTIN METZQUITITLAN",
  "13",     "TULANCINGO DE BRAVO",        "TULANCINGO DE BRAVO"
)

alias_localidades_gro <- tibble::tribble(
  ~CVE_ENT, ~mun_norm,             ~localidad_manual,
  "12",     "COYUCA DE CATALAN",   "COYUCA"
)

#Correr por estado
res_bcs <- construir_base_cabeceras_auto(
  secciones_bcs,
  ageem_bcs,
  max_dist = 3
)

res_hgo <- construir_base_cabeceras_auto(
  secciones_hgo,
  ageem_hgo,
  max_dist = 3,
  alias_localidades = alias_localidades_hgo
)

res_qroo <- construir_base_cabeceras_auto(
  secciones_qroo,
  ageem_qroo,
  max_dist = 3,
  alias_municipios = alias_municipios_qroo
)

res_gro <- construir_base_cabeceras_auto(
  secciones_gro,
  ageem_gro,
  max_dist = 3,
  alias_localidades = alias_localidades_gro
)

res_ags <- construir_base_cabeceras_auto(
  secciones_ags,
  ageem_ags,
  max_dist = 3
)

res_bc <- construir_base_cabeceras_auto(
  secciones_bc,
  ageem_bc,
  max_dist = 3
)

res_cam <- construir_base_cabeceras_auto(
  secciones_cam,
  ageem_cam,
  max_dist = 3
)

res_cdmx <- construir_base_cabeceras_auto(
  secciones_cdmx,
  ageem_cdmx,
  max_dist = 3
)

res_chi <- construir_base_cabeceras_auto(
  secciones_chi,
  ageem_chi,
  max_dist = 3
)

res_chih <- construir_base_cabeceras_auto(
  secciones_chih,
  ageem_chih,
  max_dist = 3
)

res_coa <- construir_base_cabeceras_auto(
  secciones_coa,
  ageem_coa,
  max_dist = 3
)

res_col <- construir_base_cabeceras_auto(
  secciones_col,
  ageem_col,
  max_dist = 3
)

res_dgo <- construir_base_cabeceras_auto(
  secciones_dgo,
  ageem_dgo,
  max_dist = 3
)

res_edo <- construir_base_cabeceras_auto(
  secciones_edo,
  ageem_edo,
  max_dist = 3
)

res_gto <- construir_base_cabeceras_auto(
  secciones_gto,
  ageem_gto,
  max_dist = 3
)

res_jal <- construir_base_cabeceras_auto(
  secciones_jal,
  ageem_jal,
  max_dist = 3
)

res_mich <- construir_base_cabeceras_auto(
  secciones_mich,
  ageem_mich,
  max_dist = 3
)

res_mor <- construir_base_cabeceras_auto(
  secciones_mor,
  ageem_mor,
  max_dist = 3
)

res_nay <- construir_base_cabeceras_auto(
  secciones_nay,
  ageem_nay,
  max_dist = 3
)

res_nl <- construir_base_cabeceras_auto(
  secciones_nl,
  ageem_nl,
  max_dist = 3
)

res_oax <- construir_base_cabeceras_auto(
  secciones_oax,
  ageem_oax,
  max_dist = 3
)

res_pue <- construir_base_cabeceras_auto(
  secciones_pue,
  ageem_pue,
  max_dist = 3
)

res_qro <- construir_base_cabeceras_auto(
  secciones_qro,
  ageem_qro,
  max_dist = 3
)

res_sin <- construir_base_cabeceras_auto(
  secciones_sin,
  ageem_sin,
  max_dist = 3
)

res_slp <- construir_base_cabeceras_auto(
  secciones_slp,
  ageem_slp,
  max_dist = 3
)

res_son <- construir_base_cabeceras_auto(
  secciones_son,
  ageem_son,
  max_dist = 3
)

res_tab <- construir_base_cabeceras_auto(
  secciones_tab,
  ageem_tab,
  max_dist = 3
)

res_tam <- construir_base_cabeceras_auto(
  secciones_tam,
  ageem_tam,
  max_dist = 3
)

res_tla <- construir_base_cabeceras_auto(
  secciones_tla,
  ageem_tla,
  max_dist = 3
)

res_ver <- construir_base_cabeceras_auto(
  secciones_ver,
  ageem_ver,
  max_dist = 3
)

res_yuc <- construir_base_cabeceras_auto(
  secciones_yuc,
  ageem_yuc,
  max_dist = 3
)

res_zac <- construir_base_cabeceras_auto(
  secciones_zac,
  ageem_zac,
  max_dist = 3
)

# Bases intermedias a nivel seccion-localidad
bcs_tmp  <- res_bcs$base
hgo_tmp  <- res_hgo$base
qroo_tmp <- res_qroo$base
gro_tmp  <- res_gro$base
ags_tmp <- res_ags$base
bc_tmp <- res_bc$base
cam_tmp <- res_cam$base
cdmx_tmp <- res_cdmx$base
chi_tmp <- res_chi$base
chih_tmp <- res_chih$base
coa_tmp <- res_coa$base
col_tmp <- res_col$base
dgo_tmp <- res_dgo$base
edo_tmp <- res_edo$base
gto_tmp <- res_gto$base
jal_tmp <- res_jal$base
mich_tmp <- res_mich$base
mor_tmp <- res_mor$base
nay_tmp <- res_nay$base
nl_tmp <- res_nl$base
oax_tmp <- res_oax$base
pue_tmp <- res_pue$base
qro_tmp <- res_qro$base
sin_tmp <- res_sin$base
slp_tmp <- res_slp$base
son_tmp <- res_son$base
tab_tmb <- res_tab$base
tam_tmb <- res_tam$base
tla_tmb <- res_tla$base
ver_tmb <- res_ver$base
yuc_tmb <- res_yuc$base
zac_tmb <- res_zac$base

# Bases finales a nivel seccion pura
bcs_final  <- colapsar_a_seccion(bcs_tmp)
hgo_final  <- colapsar_a_seccion(hgo_tmp)
qroo_final <- colapsar_a_seccion(qroo_tmp)
gro_final  <- colapsar_a_seccion(gro_tmp)
ags_final <- colapsar_a_seccion(ags_tmp)
bc_final <- colapsar_a_seccion(bc_tmp)
cam_final <- colapsar_a_seccion(cam_tmp)
cdmx_final <- colapsar_a_seccion(cdmx_tmp)
chi_final <- colapsar_a_seccion(chi_tmp)
chih_final <- colapsar_a_seccion(chih_tmp)
coa_final <- colapsar_a_seccion(coa_tmp)
col_final <- colapsar_a_seccion(col_tmp)
dgo_final <- colapsar_a_seccion(dgo_tmp)
edo_final <- colapsar_a_seccion(edo_tmp)
gto_final <- colapsar_a_seccion(gto_tmp)
jal_final <- colapsar_a_seccion(jal_tmp)
mich_final <- colapsar_a_seccion(mich_tmp)
mor_final <- colapsar_a_seccion(mor_tmp)
nay_final <- colapsar_a_seccion(nay_tmp)
nl_final <- colapsar_a_seccion(nl_tmp)
oax_final <- colapsar_a_seccion(oax_tmp)
pue_final <- colapsar_a_seccion(pue_tmp)
qro_final <- colapsar_a_seccion(qro_tmp)
slp_final <- colapsar_a_seccion(slp_tmp)
sin_final <- colapsar_a_seccion(sin_tmp)
son_final <- colapsar_a_seccion(son_tmp)
tab_final <- colapsar_a_seccion(tab_tmb)
tam_final <- colapsar_a_seccion(tam_tmb)
tla_final <- colapsar_a_seccion(tla_tmb)
ver_final <- colapsar_a_seccion(ver_tmb)
yuc_final <- colapsar_a_seccion(yuc_tmb)
zac_final <- colapsar_a_seccion(zac_tmb)

# ==========================================================
# 8) VALIDACION
# ==========================================================
res_bcs$diagnostico
res_hgo$diagnostico
res_qroo$diagnostico
res_gro$diagnostico
#Aproximadamente coincide

#Municipios que necesitan ser revisados
res_bcs$municipios_revision
res_hgo$municipios_revision
res_qroo$municipios_revision
res_gro$municipios_revision
res_ags$municipios_revision
res_bc$municipios_revision
res_cam$municipios_revision
res_cdmx$municipios_revision
res_cam$municipios_revision
res_chi$municipios_revision
res_chih$municipios_revision
res_coa$municipios_revision
res_col$municipios_revision
res_dgo$municipios_revision
res_edo$municipios_revision
res_gto$municipios_revision
res_jal$municipios_revision
res_mich$municipios_revision
res_mor$municipios_revision
res_nay$municipios_revision
res_nl$municipios_revision
res_oax$municipios_revision
res_pue$municipios_revision
res_qro$municipios_revision
res_slp$municipios_revision
res_sin$municipios_revision
res_son$municipios_revision
res_tab$municipios_revision
res_tam$municipios_revision
res_tla$municipios_revision
res_ver$municipios_revision
res_yuc$municipios_revision
res_zac$municipios_revision

# Verificar unicidad de seccion: solo debería de haber una sección
bcs_final %>% count(CVE_ENT, CVE_MUN_CLOC, SECCION_NUM) %>% filter(n > 1)
hgo_final %>% count(CVE_ENT, CVE_MUN_CLOC, SECCION_NUM) %>% filter(n > 1)
qroo_final %>% count(CVE_ENT, CVE_MUN_CLOC, SECCION_NUM) %>% filter(n > 1)
gro_final %>% count(CVE_ENT, CVE_MUN_CLOC, SECCION_NUM) %>% filter(n > 1)

# Municipios sin cabecera
bcs_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

hgo_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

qroo_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

gro_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

ags_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

bc_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

cam_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

chi_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

chih_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

coa_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

cdmx_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

dgo_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

gto_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

jal_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

mich_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

mor_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

nay_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

nl_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

oax_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

pue_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

qro_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

slp_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

sin_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

son_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

tab_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

tam_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

tla_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

ver_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

yuc_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

zac_final %>%
  group_by(`NOMBRE MUNICIPIO`) %>%
  summarise(tiene_cabecera = any(cabecera == 1, na.rm = TRUE)) %>%
  filter(!tiene_cabecera)

# ==========================================================
# 9) BASE FINAL DE ENTREGA
# ==========================================================
armar_entrega <- function(df) {
  df %>%
    rename(CVE_MUN = CVE_MUN_CLOC) %>%
    select(CVE_ENT, CVE_MUN, SECCION_NUM,
           `NOMBRE ENTIDAD`, `NOMBRE MUNICIPIO`,
           tipo_seccion, n_localidades_en_seccion,
           n_localidades_cabecera_en_seccion, cabecera) %>%
    distinct()
}

ags_entrega  <- armar_entrega(ags_final)
bc_entrega   <- armar_entrega(bc_final)
bcs_entrega  <- armar_entrega(bcs_final)
cam_entrega  <- armar_entrega(cam_final)
cdmx_entrega <- armar_entrega(cdmx_final)
chi_entrega  <- armar_entrega(chi_final)
chih_entrega <- armar_entrega(chih_final)
coa_entrega  <- armar_entrega(coa_final)
col_entrega <- armar_entrega(col_final)
dgo_entrega  <- armar_entrega(dgo_final)
edo_entrega <- armar_entrega(edo_final)
gro_entrega  <- armar_entrega(gro_final)
gto_entrega  <- armar_entrega(gto_final)
hgo_entrega  <- armar_entrega(hgo_final)
jal_entrega  <- armar_entrega(jal_final)
mich_entrega <- armar_entrega(mich_final)
mor_entrega  <- armar_entrega(mor_final)
nay_entrega  <- armar_entrega(nay_final)
nl_entrega   <- armar_entrega(nl_final)
oax_entrega  <- armar_entrega(oax_final)
pue_entrega  <- armar_entrega(pue_final)
qro_entrega  <- armar_entrega(qro_final)
qroo_entrega <- armar_entrega(qroo_final)
slp_entrega  <- armar_entrega(slp_final)
sin_entrega  <- armar_entrega(sin_final)
son_entrega  <- armar_entrega(son_final)
tab_entrega  <- armar_entrega(tab_final)
tam_entrega  <- armar_entrega(tam_final)
tla_entrega  <- armar_entrega(tla_final)
ver_entrega  <- armar_entrega(ver_final)
yuc_entrega  <- armar_entrega(yuc_final)
zac_entrega  <- armar_entrega(zac_final)

#El tamaño del dataset debe de estar del tamaño del numero de secciones de un estado
str(bcs_entrega)
str(hgo_entrega)
str(qroo_entrega)
str(gro_entrega)

############### Parte III: Tablas de equivalencias de secciones 
tabla_equivalencias_ags <- tabla_equivalencias %>%
  filter(edon == 1)
tabla_equivalencias_bc <- tabla_equivalencias %>%
  filter(edon == 2)
tabla_equivalencias_bcs <- tabla_equivalencias %>%
  filter(edon == 3)
tabla_equivalencias_cam <- tabla_equivalencias %>%
  filter(edon == 4)
tabla_equivalencias_coa <- tabla_equivalencias %>%
  filter(edon == 5)
tabla_equivalencias_col <- tabla_equivalencias %>%
  filter(edon == 6)
tabla_equivalencias_chi <- tabla_equivalencias %>%
  filter(edon == 7)
tabla_equivalencias_chih <- tabla_equivalencias %>%
  filter(edon == 8)
tabla_equivalencias_cdmx <- tabla_equivalencias %>%
  filter(edon == 9)
tabla_equivalencias_dgo <- tabla_equivalencias %>%
  filter(edon == 10)
tabla_equivalencias_gto <- tabla_equivalencias %>%
  filter(edon == 11)
tabla_equivalencias_gro <- tabla_equivalencias %>%
  filter(edon == 12)
tabla_equivalencias_hgo <- tabla_equivalencias %>%
  filter(edon == 13)
tabla_equivalencias_jal <- tabla_equivalencias %>%
  filter(edon == 14)
tabla_equivalencias_edo <- tabla_equivalencias %>%
  filter(edon == 15)
tabla_equivalencias_mich <- tabla_equivalencias %>%
  filter(edon == 16)
tabla_equivalencias_mor <- tabla_equivalencias %>%
  filter(edon == 17)
tabla_equivalencias_nay <- tabla_equivalencias %>%
  filter(edon == 18)
tabla_equivalencias_nl <- tabla_equivalencias %>%
  filter(edon == 19)
tabla_equivalencias_oax <- tabla_equivalencias %>%
  filter(edon == 20)
tabla_equivalencias_pue <- tabla_equivalencias %>%
  filter(edon == 21)
tabla_equivalencias_qro <- tabla_equivalencias %>%
  filter(edon == 22)
tabla_equivalencias_qroo <- tabla_equivalencias %>%
  filter(edon == 23)
tabla_equivalencias_slp <- tabla_equivalencias %>%
  filter(edon == 24)
tabla_equivalencias_sin <- tabla_equivalencias %>%
  filter(edon == 25)
tabla_equivalencias_son <- tabla_equivalencias %>%
  filter(edon == 26)
tabla_equivalencias_tab <- tabla_equivalencias %>%
  filter(edon == 27)
tabla_equivalencias_tam <- tabla_equivalencias %>%
  filter(edon == 28)
tabla_equivalencias_tla <- tabla_equivalencias %>%
  filter(edon == 29)
tabla_equivalencias_ver <- tabla_equivalencias %>%
  filter(edon == 30)
tabla_equivalencias_yuc <- tabla_equivalencias %>%
  filter(edon == 31)
tabla_equivalencias_zac <- tabla_equivalencias %>%
  filter(edon == 32)

#revision
str(ags_entrega)
str(tabla_equivalencias_ags)

#Anti-joins
comparar_secciones <- function(base_entrega, tabla_equivalencias_estado) {
  
  tabla_eq_join <- tabla_equivalencias_estado %>%
    transmute(
      CVE_ENT = str_pad(as.character(edon), 2, pad = "0"),
      SECCION_NUM = as.numeric(seccion)
    ) %>%
    distinct()
  
  base_entrega %>%
    anti_join(
      tabla_eq_join,
      by = c("CVE_ENT", "SECCION_NUM")
    )
}

# ==========================================================
# REVISION DE SECCIONES VS TABLA DE EQUIVALENCIAS
# ==========================================================

rev_ags  <- comparar_secciones(ags_entrega,  tabla_equivalencias_ags)
rev_bc   <- comparar_secciones(bc_entrega,   tabla_equivalencias_bc)
rev_bcs  <- comparar_secciones(bcs_entrega,  tabla_equivalencias_bcs)
rev_cam  <- comparar_secciones(cam_entrega,  tabla_equivalencias_cam)
rev_cdmx <- comparar_secciones(cdmx_entrega, tabla_equivalencias_cdmx)
rev_chi  <- comparar_secciones(chi_entrega,  tabla_equivalencias_chi)
rev_chih <- comparar_secciones(chih_entrega, tabla_equivalencias_chih)
rev_coa  <- comparar_secciones(coa_entrega,  tabla_equivalencias_coa)
rev_col <- comparar_secciones(col_entrega, tabla_equivalencias_col)
rev_dgo  <- comparar_secciones(dgo_entrega,  tabla_equivalencias_dgo)
rev_edo <- comparar_secciones(edo_entrega, tabla_equivalencias_edo)
rev_gro  <- comparar_secciones(gro_entrega,  tabla_equivalencias_gro)
rev_gto  <- comparar_secciones(gto_entrega,  tabla_equivalencias_gto)
rev_hgo  <- comparar_secciones(hgo_entrega,  tabla_equivalencias_hgo)
rev_jal  <- comparar_secciones(jal_entrega,  tabla_equivalencias_jal)
rev_mich <- comparar_secciones(mich_entrega, tabla_equivalencias_mich)
rev_mor  <- comparar_secciones(mor_entrega,  tabla_equivalencias_mor)
rev_nay  <- comparar_secciones(nay_entrega,  tabla_equivalencias_nay)
rev_nl   <- comparar_secciones(nl_entrega,   tabla_equivalencias_nl)
rev_oax  <- comparar_secciones(oax_entrega,  tabla_equivalencias_oax)
rev_pue  <- comparar_secciones(pue_entrega,  tabla_equivalencias_pue)
rev_qro  <- comparar_secciones(qro_entrega,  tabla_equivalencias_qro)
rev_qroo <- comparar_secciones(qroo_entrega, tabla_equivalencias_qroo)
rev_slp  <- comparar_secciones(slp_entrega,  tabla_equivalencias_slp)
rev_sin  <- comparar_secciones(sin_entrega,  tabla_equivalencias_sin)
rev_son  <- comparar_secciones(son_entrega,  tabla_equivalencias_son)
rev_tab  <- comparar_secciones(tab_entrega,  tabla_equivalencias_tab)
rev_tam  <- comparar_secciones(tam_entrega,  tabla_equivalencias_tam)
rev_tla  <- comparar_secciones(tla_entrega,  tabla_equivalencias_tla)
rev_ver  <- comparar_secciones(ver_entrega,  tabla_equivalencias_ver)
rev_yuc  <- comparar_secciones(yuc_entrega,  tabla_equivalencias_yuc)
rev_zac  <- comparar_secciones(zac_entrega,  tabla_equivalencias_zac)

#Exportación
# ==========================================================
# EXPORTACION DE SECCIONES NO EN TABLA DE EQUIVALENCIAS
# ==========================================================

write.csv(rev_ags,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/ags_antijoin.csv',  row.names = FALSE)
write.csv(rev_bc,   '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/bc_antijoin.csv',   row.names = FALSE)
write.csv(rev_bcs,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/bcs_antijoin.csv',  row.names = FALSE)
write.csv(rev_cam,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/cam_antijoin.csv',  row.names = FALSE)
write.csv(rev_cdmx, '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/cdmx_antijoin.csv', row.names = FALSE)
write.csv(rev_chi,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/chi_antijoin.csv',  row.names = FALSE)
write.csv(rev_chih, '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/chih_antijoin.csv', row.names = FALSE)
write.csv(rev_coa,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/coa_antijoin.csv',  row.names = FALSE)
write.csv(rev_col,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/col_antijoin.csv',  row.names = FALSE)
write.csv(rev_dgo,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/dgo_antijoin.csv',  row.names = FALSE)
write.csv(rev_edo,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/edo_antijoin.csv',  row.names = FALSE)
write.csv(rev_gro,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/gro_antijoin.csv',  row.names = FALSE)
write.csv(rev_gto,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/gto_antijoin.csv',  row.names = FALSE)
write.csv(rev_hgo,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/hgo_antijoin.csv',  row.names = FALSE)
write.csv(rev_jal,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/jal_antijoin.csv',  row.names = FALSE)
write.csv(rev_mich, '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/mich_antijoin.csv', row.names = FALSE)
write.csv(rev_mor,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/mor_antijoin.csv',  row.names = FALSE)
write.csv(rev_nay,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/nay_antijoin.csv',  row.names = FALSE)
write.csv(rev_nl,   '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/nl_antijoin.csv',   row.names = FALSE)
write.csv(rev_oax,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/oax_antijoin.csv',  row.names = FALSE)
write.csv(rev_pue,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/pue_antijoin.csv',  row.names = FALSE)
write.csv(rev_qro,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/qro_antijoin.csv',  row.names = FALSE)
write.csv(rev_qroo, '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/qroo_antijoin.csv', row.names = FALSE)
write.csv(rev_slp,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/slp_antijoin.csv',  row.names = FALSE)
write.csv(rev_sin,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/sin_antijoin.csv',  row.names = FALSE)
write.csv(rev_son,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/son_antijoin.csv',  row.names = FALSE)
write.csv(rev_tab,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/tab_antijoin.csv',  row.names = FALSE)
write.csv(rev_tam,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/tam_antijoin.csv',  row.names = FALSE)
write.csv(rev_tla,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/tla_antijoin.csv',  row.names = FALSE)
write.csv(rev_ver,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/ver_antijoin.csv',  row.names = FALSE)
write.csv(rev_yuc,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/yuc_antijoin.csv',  row.names = FALSE)
write.csv(rev_zac,  '/Users/gabinomartinez/Documents/ITAM/Semestre 12/Asistente de Investigación/Antijoins/zac_antijoin.csv',  row.names = FALSE)




