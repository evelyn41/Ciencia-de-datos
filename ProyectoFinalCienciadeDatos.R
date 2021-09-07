{ 
  library(readxl)
  library(dplyr)
  library(openxlsx)
  library(ggplot2)
  library(skimr)
  library(vcd)
  library(corrplot)
  library(janitor)
  library(survival)
  library(rms)
  library(MASS)
  library(descr)
  library(survminer)
  library(xtable)
}


#### Lectura de los datos ####
db <- read_excel("~/Propension de Fuga/Fuga Autos.xlsx", 
                              sheet = "Fuga") %>% 
  dplyr::mutate(MODELO = as.character(MODELO),
         SALIDA = as.numeric(SALIDA),
         PRIMA_MENSUAL = as.numeric(PRIMA_MENSUAL),
         VALOR_ASEGURADO = as.numeric(VALOR_ASEGURADO)) %>% 
  dplyr::select(-Ciudad) %>% 
  filter(EDAD > 16) %>%
  filter(EDAD < 100) %>% 
  filter(GENERO != "NO REGISTRA") %>% 
  mutate(GENERO = as.factor(GENERO))

## Tipo de objeto de cada variable
glimpse(db)

#### Matriz de correlaciones ####

db %>% select_if(is.numeric) %>%
  dplyr::select(-SALIDA) %>% 
  cor() %>%
  corrplot()

#### Remover variable BENEFICIARIOS de la base ####

db <- db %>% 
  dplyr::select(-BENEFICIARIOS)

#### Tabla de frecuencias de la variable salida

churn = tabyl(db$SALIDA)
churn

mean(db$ANTIGÜEDAD)

#### Conteo de variable ANTIGÜEDAD de acuerdo a SALIDA ####

plotPermanencia <- as.data.frame(db) %>% 
  mutate(Exited = SALIDA %>% factor(labels = c("No", "Yes"))) %>% 
  ggplot() +
  geom_histogram(aes(x = ANTIGÜEDAD,
                     fill = factor(SALIDA)), binwidth = 1) +
  geom_vline(aes(xintercept=mean(ANTIGÜEDAD))) +
  facet_grid( ~ SALIDA) +
  theme(legend.position = "none")
plotPermanencia # Promedio de permanencia para retirados y no retirados 


## Comportamiento variable edad ##
ggplot(db, aes(x=EDAD)) +
  geom_histogram(fill="#69b3a2", position="dodge")
                    
# Añadir categorización de la variable EDAD
db <- db %>% mutate(EDAD_CAT = ifelse(EDAD >=60, "EXPERTOS", "APASIONADOS"),
                    EDAD_CAT = ifelse(EDAD <=40, "CRECIMIENTO", EDAD_CAT))
db$EDAD_CAT <- factor(db$EDAD_CAT)

# Añadir categorización de la variable MODELO
db <- db %>% mutate(MODELO_CAT = ifelse(MODELO >= "2015", "MODERNOS", "ANTIGUOS"),
                    MODELO_CAT = ifelse(MODELO <= "2005", "INTERMEDIO", MODELO_CAT))
db$MODELO_CAT <- factor(db$MODELO_CAT)

## Añadir categorización de gamas de vehículos
db <- db %>% mutate(GAMAS = ifelse(VALOR_ASEGURADO >= 80000000, "ALTA", "MEDIA"),
                    GAMAS = ifelse(VALOR_ASEGURADO <= 50000000, "BAJA", GAMAS))
db$GAMAS <- factor(db$GAMAS)


#### Partición de datos ####
set.seed(101) #semilla para replicación
tamano.total <- nrow(db)
tamano.entreno <- round(tamano.total*0.7)
datos.indices <- sample(1:tamano.total , size=tamano.entreno)
datos.entreno <- db[datos.indices,]
datos.test <- db[-datos.indices,]
xtable(as.data.frame(table(datos.entreno$SALIDA))) #Exportacion para formato latex
xtable(as.data.frame(table(datos.test$SALIDA))) #Exportacion para formato latex


#### Creación de objeto para modelo de sobrevivencia ####
## Se trabaja con datos de entrenamiento ##

cbind(datos.entreno %>% dplyr::select(ANTIGÜEDAD, SALIDA),
      surv = Surv(datos.entreno$ANTIGÜEDAD, datos.entreno$SALIDA)) %>% head(10)

survObj <- Surv(datos.entreno$ANTIGÜEDAD, datos.entreno$SALIDA)

#### Curvas de supervivencia ####
## Curva simple ##
fit <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ 1, data = datos.entreno)
print(fit) #Estimación puntual de la mediana
ggsurvplot(fit, #Gráfica
           surv.median.line = "hv",
           xlab = "Permanencia", ylab = "Prob. de sobrevivencia", main = "Función de sobreviviencia")
res.sum <- surv_summary(fit) #Resumen estimación KM
print(xtable(res.sum), include.rownames = FALSE) #Para latex
## Curva por género ##
fitGenero <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO, data = datos.entreno)
ggsurvplot(fitGenero, pval = T,
           surv.median.line = "hv",
           xlab = "Permanencia", ylab = "Prob. de sobrevivencia")
res.sumG <- surv_summary(fitGenero, datos.test) #Estimación
print(xtable(res.sum), include.rownames = FALSE) #Para latex
## Comparación entre grupos de género
## Prueba de rango logarítmico 
## Hipótesis nula es que no hay diferencia en la supervivencia entre los grupos
surv_diff <- survdiff(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO, data = datos.entreno);surv_diff 
#Muestra lo mismo que el p valor de la gráfica
## Curva para edad ##
fitEdad <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ EDAD_CAT, data = datos.entreno)
ggsurvplot(fitEdad, xlab = "Permanencia", ylab = "Prob. de sobrevivencia",
           surv.median.line = "hv", pval = T)
res.sumE <- surv_summary(fitEdad)
xtable(res.sumE)
surv_diff <- survdiff(Surv(ANTIGÜEDAD, SALIDA) ~ EDAD_CAT, data = datos.entreno);surv_diff
## Curva para salario ##
fitSalario <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ SALARIO, data = datos.entreno)
ggsurvplot(fitSalario, pval = TRUE,
           surv.median.line = "hv",
           xlab = "Permanencia", ylab = "Prob. de sobrevivencia")
res.sumS <- surv_summary(fitSalario) %>% head(20)
xtable(res.sumS)
surv_diff <- survdiff(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO, data = datos.entreno);surv_diff
## Curva para gama de auto ##
fitGama <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ GAMAS, data = datos.entreno)
ggsurvplot(fitGama, pval = T,
           surv.median.line = "hv",
           xlab = "Permanencia", ylab = "Prob. de sobrevivencia")
res.sumS <- surv_summary(fitGama) %>% head(20)
xtable(res.sumS)
surv_diff <- survdiff(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO, data = datos.entreno);surv_diff
## Curva para modelo de auto ##
fitModelo <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ MODELO_CAT, data = datos.entreno)
ggsurvplot(fitModelo, pval = T,
           surv.median.line = "hv")
res.sumS <- surv_summary(fitModelo) %>% head(20)
xtable(res.sumS)
surv_diff <- survdiff(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO, data = datos.entreno);surv_diff

#### KM Con varios factores ####
## Genero-salario ##
fit2 <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO+
                  SALARIO, data = datos.entreno)

ggsurv <- ggsurvplot(fit2, conf.int = TRUE,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(SALARIO ~ GENERO)
## Genero-estado civil
fit3 <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO+
                  ESTADO_CIVIL, data = datos.entreno)

ggsurv <- ggsurvplot(fit3, conf.int = TRUE,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(ESTADO_CIVIL ~ GENERO)
## Salario-estado civil
fit4 <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ SALARIO+
                  ESTADO_CIVIL, data = datos.entreno)

ggsurv <- ggsurvplot(fit4, conf.int = TRUE,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(ESTADO_CIVIL ~ SALARIO)
## Salario-edad ##
fit5 <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ SALARIO+
                  EDAD_CAT, data = datos.entreno)

ggsurv <- ggsurvplot(fit5, conf.int = TRUE,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(EDAD_CAT ~ SALARIO)
## Genero-edad ##
fit6 <- survfit(Surv(ANTIGÜEDAD, SALIDA) ~ GENERO+
                  EDAD_CAT, data = datos.entreno)

ggsurv <- ggsurvplot(fit6, conf.int = TRUE,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(EDAD_CAT ~ GENERO)

#### Modelo se sobrevivencia Cox Proportional Hazards ####
## Primer modelo general ##
fitcoxph1 <- coxph(survObj ~ ESTADO_CIVIL+
                     EDAD_CAT + GENERO + VALOR_ASEGURADO+PRIMA_MENSUAL+
                    SALARIO + NUMERO_CONVENIOS, 
                   data = datos.entreno)
AIC(fitcoxph1)
summary(fitcoxph1)
ggforest(fitcoxph1, data = datos.test)
print(xtable(broom::tidy(fitcoxph1, exp = TRUE)), include.rownames = FALSE)
## Modelo sin valor y prima pero incluyendo variable gama ##
fitcoxph2 <- coxph(survObj ~ ESTADO_CIVIL+
                     EDAD_CAT + GENERO+ SALARIO + 
                     NUMERO_CONVENIOS + GAMAS + MODELO_CAT, 
                   data = datos.entreno)
AIC(fitcoxph2)
summary(fitcoxph2)
ggforest(fitcoxph2, data = datos.test)
print(xtable(broom::tidy(fitcoxph2, exp = TRUE)), include.rownames = FALSE)
## Modelo sin gama ##
fitcoxph3 <- coxph(survObj ~ ESTADO_CIVIL+ GENERO +
                     EDAD_CAT + SALARIO + 
                     NUMERO_CONVENIOS  + MODELO_CAT, 
                   data = datos.entreno)
AIC(fitcoxph3)
summary(fitcoxph3)
ggforest(fitcoxph3, data = datos.test)
print(xtable(broom::tidy(fitcoxph3, exp = TRUE)), include.rownames = FALSE)

# Survival curve general con datos de test
ggsurvplot(survfit(fitcoxph3), palette = "#2E9FDF",
           surv.median.line = "hv", 
           xlab = "Permanencia", ylab = "Prob. de sobrevivencia",
                  data = datos.test)

print(xtable(surv_summary(survfit(fitcoxph3))), include.rownames = FALSE)
print(survfit(fitcoxph3))
