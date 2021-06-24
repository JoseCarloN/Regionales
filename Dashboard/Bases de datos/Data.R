library(stringr)
library(reshape2)
library(dplyr)

# df_metas = read.csv("Metas.csv", encoding = "UTF-8")
# df_lb = read.csv("LineasBase.csv", encoding = "UTF-8")
# colnames(df_metas) =  c("Indicador", colnames(df_metas[,-1]))
# colnames(df_lb) =  c("Indicador", colnames(df_lb[,-1]))
# 
# df_metas.melt = melt(df_metas, id.vars = "Indicador")
# df_lb.melt = melt(df_metas, id.vars = "Indicador")
# 
# str_detect(df.melt$variable, "")

df = read.csv("Indicadores.csv", encoding = "UTF-8")
colnames(df) = c("Eje.Estatal", colnames(df[,-1]))

indicadores_valores = df %>%
  select(Nombre.del.Indicador.Regional, LB.valor.Región.1,
         LB.valor.Región.2,
         LB.valor.Región.3,
         LB.valor.Región.4,
         LB.valor.Región.5,
         LB.valor.Región.6,
         LB.valor.Región.7)
indicadores_valores = melt(indicadores_valores, id.vars = "Nombre.del.Indicador.Regional")
indicadores_valores = indicadores_valores[!grepl('^[^0-9]+$', indicadores_valores$value),]
colnames(indicadores_valores) = c("Nombre.del.Indicador.Regional", "Region", "LB")
indicadores_valores$LB = as.numeric(indicadores_valores$LB)
indicadores_valores$Region = str_replace(str_sub(indicadores_valores$Region, start = str_locate(indicadores_valores$Region, "Región")[, 1]), pattern = "\\.", replacement = " ")

indicadores_metas = df %>%
  select(Nombre.del.Indicador.Regional, M.2021.Región.1,
         M.2021.Región.2,
         M.2021.Región.3,
         M.2021.Región.4,
         M.2021.Región.5,
         M.2021.Región.6,
         M.2021.Región.7)
indicadores_metas = melt(indicadores_metas, id.vars = "Nombre.del.Indicador.Regional")
indicadores_metas = indicadores_metas[!grepl('^[^0-9]+$', indicadores_metas$value),]
colnames(indicadores_metas) = c("Nombre.del.Indicador.Regional", "Region", "Meta")
indicadores_metas$Meta = as.numeric(indicadores_metas$Meta)
indicadores_metas$Region = str_replace(str_sub(indicadores_metas$Region, start = str_locate(indicadores_metas$Region, "Región")[, 1]), pattern = "\\.", replacement = " ")

indicadores = merge(x = indicadores_valores, y = indicadores_metas, by = c("Nombre.del.Indicador.Regional", "Region")) %>%
  mutate(Avance = ifelse(round(LB/Meta * 100, 0) > 100, 100, 
                         ifelse(round(LB/Meta * 100, 0) < 0, 0, round(LB/Meta * 100, 0))))

write.csv(indicadores, "Test.csv")
