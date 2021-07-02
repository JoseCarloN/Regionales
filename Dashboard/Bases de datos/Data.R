library(stringr)
library(reshape2)
library(dplyr)

# De este df salen las metas de cada indicador por region #
df = read.csv("Indicadores Metas.csv", 
              encoding = "UTF-8", 
              na.strings = c("ND", "N.D", "N/D", "#N/A", "N/A", "N.A", "")
              )
colnames(df) = c("Eje.Estatal", colnames(df[,-1]))

# De este df salen los valores de cada indicador por region #
df2 = read.csv("Indicadores Valores.csv",
               encoding = "UTF-8",
               na.strings = c("NA", "")
               )
colnames(df2) = c("Nombre.del.Indicador.Regional", colnames(df2[,-1]))

# Transformando de formato wide a long #
indicadores_valores = melt(df2, 
                           id.vars = "Nombre.del.Indicador.Regional") %>%
  mutate(
    value = ifelse(value == "Bajo",
                   100,
                   ifelse(value == "Intermedio",
                          90,
                          ifelse(value == "Alto",
                                 50,
                                 value
                                 )
                          )
                   )
  )

# Quitando todas las observaciones que no son un numero #
indicadores_valores = indicadores_valores[!grepl('^[^0-9]+$', indicadores_valores$value),]

colnames(indicadores_valores) = c("Nombre.del.Indicador.Regional", "Region", "Valor")
indicadores_valores$Valor = as.numeric(indicadores_valores$Valor)

# Remplaza el "." en el vector por un espacio #
indicadores_valores$Region = str_replace(
  # Extrae la string desde start que es desde donde comientza la palabra "Región" #
  str_sub(
    indicadores_valores$Region, 
    start = str_locate(indicadores_valores$Region, "Región")[, 1]), 
  pattern = "\\.", 
  replacement = " "
  )

# Extrae las metas de cada uno de los indicadores por region #
indicadores_metas = df %>%
  select(
    Nombre.del.Indicador.Regional,
    tendencia,
    M.2021.Región.1,
    M.2021.Región.2,
    M.2021.Región.3,
    M.2021.Región.4,
    M.2021.Región.5,
    M.2021.Región.6,
    M.2021.Región.7
    )

# Las convierte de formato wide a long #
indicadores_metas = melt(indicadores_metas,
                         id.vars = c("Nombre.del.Indicador.Regional", "tendencia"))

indicadores_metas[which(indicadores_metas$Nombre.del.Indicador.Regional == "Porcentaje de municipios con nivel de endeudamiento bajo"), "value"] = 1

indicadores_metas = indicadores_metas%>%
  mutate(
    value = as.numeric(value),
    value = ifelse(str_starts(Nombre.del.Indicador.Regional, "Porcentaje"), value / 100, value)
  )

# Remueve todas aquellas que no son un numero #
indicadores_metas = indicadores_metas[!grepl('^[^0-9]+$', indicadores_metas$value),]

colnames(indicadores_metas) = c("Nombre.del.Indicador.Regional", "Tendencia", "Region", "Meta")
indicadores_metas$Meta = as.numeric(indicadores_metas$Meta)

# Remplaza el "." en el vector por un espacio #
indicadores_metas$Region = str_replace(
  # Extrae la string desde start que es desde donde comientza la palabra "Región" #
    str_sub(
    indicadores_metas$Region, 
    start = str_locate(indicadores_metas$Region, "Región")[, 1]), 
  pattern = "\\.", 
  replacement = " "
  )

# Hace un merge por nombre de indicador y region de las metas y las lineas base #
indicadores = merge(
  x = indicadores_valores, 
  y = indicadores_metas, 
  by = c("Nombre.del.Indicador.Regional", "Region")
  ) %>%
  mutate(
    Avance = case_when(
      Tendencia == "Descendente" ~ Meta/Valor,
      Tendencia %in% c("Constante", "Ascendente") ~ case_when(
        Meta <= 0 & Valor <= 0 ~ Meta/Valor,
        Meta >= 0 & Valor >= 0 ~ Valor/Meta,
        Meta >= 0 & Valor <= 0 ~ 0,
        Meta <= 0 & Valor >= 0 ~ 1
        )
    ),
    # Si el avance sobre pasa la meta se le asigna 100% #
    Avance = ifelse(
      round(Avance * 100, 0) > 100, 
      100, 
      ifelse(
        round(Avance * 100, 0) < 0, 
        0, 
        round(Avance * 100, 0)
        )
      )
    ) %>%
  select(
    Nombre.del.Indicador.Regional,
    Region,
    Avance
  )

colnames(indicadores) = c("Indicador", colnames(indicadores[,-1]))
write.csv(indicadores, "Indicadores.csv", row.names = FALSE)
