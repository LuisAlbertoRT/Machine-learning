#####FUENTE DE CONSULTA: https://www.cienciadedatos.net/documentos/41_machine_learning_con_r_y_caret#División_de_los_datos_en_entrenamiento_y_test




# Instalación de los paquetes que unifica caret. Esta instalación puede tardar.
# Solo es necesario ejecutarla si no se dispone de los paquetes.
install.packages("caret", dependencies = c("Depends", "Suggests"))


library(caret)

library(tidyverse)
library(titanic)
datos <- titanic_train



# Resumen del set de datos
glimpse(datos)

#Cambio de variable numerica a factor si o no para evitar problemas futuros
datos$Survived <- if_else(datos$Survived == 1, "Si", "No")
#cambiar el tipo de formato a factor
datos$Survived <- as.factor(datos$Survived)


#Cambiamos el tipo de clase a factor para ya que la codificacion es mejor
datos$Pclass <- as.factor(datos$Pclass)

#cambiamos como factor a los hermanos y los padres
datos$SibSp <- as.factor(datos$SibSp)
datos$Parch <- as.factor(datos$Parch)
#El sexo y el embarcadero tambien lo guardams como factor
datos$Sex      <- as.factor(datos$Sex)
datos$Embarked <- as.factor(datos$Embarked)


#Cambiamos a estructura: observación, variable, valor

datos_long <- datos %>% gather(key = "variable", value = "valor", -PassengerId)
head(datos_long)


#Numero de observaciones
nrow(datos)




# Detección si hay alguna fila incompleta
any(!complete.cases(datos))


# Número de datos ausentes por variable
map_dbl(datos, .f = function(x){sum(is.na(x))}) #No identifica cabin por que toma espacio como caracter

#Prueba que datos son caracteres
datos %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})

# La variable Cabin está almacenada como character se sustituye por NA
datos$Cabin[datos$Cabin == ""] <- NA

#Analisis de la variable enbarcadero para encontrar NA
levels(datos$Embarked)

#cambiamos la variable a caracter
datos$Embarked <- as.character(datos$Embarked)
#sustituimos espacio por NA
datos$Embarked[datos$Embarked == ""] <- NA
#VOLVEMOS A TRANSFORMARLA A FACTOR
datos$Embarked <- as.factor(datos$Embarked)
#VOLVEMOS A HACER LA PRUEBA
levels(datos$Embarked)

# Este cambio también se aplica al dataframe datos_long
datos_long$valor[datos_long$valor == ""] <- NA



# Número de datos ausentes por variable
map_dbl(datos, .f = function(x){sum(is.na(x))})



# Representación gráfica de los datos ausentes
datos_long <- datos_long %>%  mutate(ausente = is.na(valor))
ggplot(data = datos_long, aes(x = variable, y = PassengerId, fill = ausente)) +
  geom_raster() +
  scale_fill_manual(values = c("gray60", "orangered2")) +
  theme_bw() +
  labs(title = "Valores ausentes por variable") +
  theme(legend.position = "bottom")


#Grafico de porcentaje de datos ausentes
datos_long %>%
  group_by(variable) %>% 
  summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) %>%
  ggplot(aes(x = reorder(variable, desc(porcentaje_NA)), y = porcentaje_NA)) +
  geom_col() +
  labs(title = "Porcentaje valores ausentes por variable",
       x = "Variable", y = "Porcentaje NAs") +
  theme_bw()



#Analisis de la varible a predecir en este caso la supervivencia
ggplot(data = datos, aes(x = Survived, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Supervivencia") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias 
table(datos$Survived)

#Porcentaje
prop.table(table(datos$Survived)) %>% round(digits = 2)


# Porcentaje de aciertos si se predice para todas las observaciones que no sobrevivieron.
n_observaciones <- nrow(datos)
predicciones <- rep(x = "No",  n_observaciones)
mean(predicciones == datos$Survived) * 100




#Analisis de que variables estan mas relacionadas a la supervivencia
library(ggpubr)
p1 <- ggplot(data = datos, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = datos, aes(x = Survived, y = Age, color = Survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Age", size = 15))
final_plot


# Estadísticos de la edad de los supervivientes y fallecidos
datos %>% filter(!is.na(Age)) %>% group_by(Survived) %>%
  summarise(media = mean(Age),
            mediana = median(Age),
            min = min(Age),
            max = max(Age))


#Creamos una nueva variables que divida como niños, adultos y ancianos
datos <- datos %>%
  mutate(Age_grupo = case_when(Age <= 10  ~ "niño",
                               Age > 10 & Age <= 60  ~ "adulto",
                               Age > 60 ~ "anciano"))
datos$Age_grupo <- as.factor(datos$Age_grupo)


#Analisis del precio del boleto
p1 <- ggplot(data = datos, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = datos, aes(x = Survived, y = Fare, color = Survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Fare", size = 15))
final_plot



# Estadísticos del precio del billete de los supervivientes y fallecidos
datos %>% filter(!is.na(Fare)) %>% group_by(Survived) %>%
  summarise(media = mean(Fare),
            mediana = median(Fare),
            min = min(Fare),
            max = max(Fare))



#TANSFORMACION LOGARITMICA
p1 <- ggplot(data = datos, aes(x = log(Fare), fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = datos, aes(x = Survived, y = log(Fare), color = Survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Log(Fare)", size =15))
final_plot


#############DISTRIBUCION PARA VARIABLES CUALITATIVAS FACTOR###########

#Analisis de la clase de los tripulantes
ggplot(data = datos, aes(x = Pclass, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Pclass") +
  theme_bw() +
  theme(legend.position = "bottom")


# Tabla de frecuencias relativas de supervivientes por clase
#Porcentajes por clase
prop.table(table(datos$Pclass, datos$Survived), margin = 1) %>% round(digits = 2)





#####Analisis por sexo
ggplot(data = datos, aes(x = Sex, y = ..count.., fill = Survived)) +
  geom_bar() +
  labs(title = "Sex") +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw() +
  theme(legend.position = "bottom")


# Tabla de frecuencias relativas de supervivientes por sexo
prop.table(table(datos$Sex, datos$Survived), margin =1 ) %>% round(digits = 2)



##ANALISIS DE LAS PERSONAS QUE TENIAN HERMANOS
ggplot(data = datos, aes(x = SibSp, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "SibSp") +
  theme_bw() +
  theme(legend.position = "bottom")


# Tabla de frecuencias relativas de supervivientes por número de familiares
#Porcentajes
prop.table(table(datos$SibSp, datos$Survived), margin = 1) %>% round(digits = 2)




#ANALISIS NUMERO DE PADRES E HIJOS EN EL BARCO
ggplot(data = datos, aes(x = Parch, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Parch") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por Parch
prop.table(table(datos$Parch, datos$Survived), margin = 1) %>% round(digits = 2)



#TRANSFORMACION DE HERMANOS Y LA VARIABLE PADRES A HIJOS A NUMERICA
# Para pasar de factor a numeric primero se convierte a character
datos$SibSp <- as.character(datos$SibSp)
datos$SibSp <- as.numeric(datos$SibSp)
datos$Parch <- as.character(datos$Parch)
datos$Parch <- as.numeric(datos$Parch)

#Grafico de tipo de embarcadero
ggplot(data = datos, aes(x = Embarked, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Embarked") +
  theme_bw() +
  theme(legend.position = "bottom")


# Tabla de frecuencias relativas de supervivientes por puerto de embarque
prop.table(table(datos$Embarked, datos$Survived), margin = 1) %>% round(digits = 2)


#Analisis por el rango de edad creado anteriormente
ggplot(data = datos, aes(x = Age_grupo, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Age_grupo") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por grupo de edad
prop.table(table(datos$Age_grupo, datos$Survived), margin = 1) %>% round(digits = 2)




#Correlacion de variables continuas por pearson edad y precio del boleto

cor.test(x = datos$Age, y = datos$Fare, method = "pearson")


ggplot(data = datos, aes(x = Age, y = log(Fare))) +
  geom_point(color = "gray30") +
  geom_smooth(color = "firebrick") +
  theme_bw()



#No existe correlacion con p-value <0.05






#test de contraste de proporciones.


#TRanformacion a variables dummys a las variable cualitativas
# Se excluyen las variables continuas y las cualitativas que no agrupan a los
# pasajeros. También la variable Cabin por su alto % de valores NA.
datos_cualitativos <- datos %>%
  select(-Age, -Fare, -Name, -Ticket, -Cabin, -PassengerId)

datos_cualitativos_tidy <- datos_cualitativos %>%
  gather(key = "variable", value = "grupo",-Survived)

# Se eliminan los valores NA para que no se interpreten como un grupo
datos_cualitativos_tidy <- datos_cualitativos_tidy %>% filter(!is.na(grupo))

# Se añade un identificador formado por el nombre de la variable y el grupo 
datos_cualitativos_tidy <- datos_cualitativos_tidy %>%
  mutate(variable_grupo = paste(variable, grupo, sep = "_"))


# Función que calcula el test de proporciones para la columna "Survived" de un df
test_proporcion <- function(df){
  n_supervivientes <- sum(df$Survived == "Si") 
  n_fallecidos     <- sum(df$Survived == "No")
  n_total <- n_supervivientes + n_fallecidos
  test <- prop.test(x = n_supervivientes, n = n_total, p = 0.3838)
  prop_supervivientes <- n_supervivientes / n_total
  return(data.frame(p_value = test$p.value, prop_supervivientes))
}


#Probabilidades relativas pr grupo
# Se agrupan los datos por "variable_grupo" y se aplica a cada grupo la función
# test_proporcion()
analisis_prop <- datos_cualitativos_tidy %>%
  group_by(variable_grupo) %>%
  nest() %>%
  arrange(variable_grupo) %>%
  mutate(prop_test = map(.x = data, .f = test_proporcion)) %>%
  unnest(prop_test) %>%
  arrange(p_value) %>% 
  select(variable_grupo,p_value, prop_supervivientes)
view(analisis_prop)




# Representación gráfica de la distribución de los 6 grupos con menor p-value
top6_grupos <- analisis_prop %>% pull(variable_grupo) %>% head(6)

view(top6_grupos)

# Se crea una función que, dados un dataframe y el nombre de un grupo, genere la
# representación gráfica de supervivientes y no supervivientes.
plot_grupo <- function(grupo, df, threshold_line = 0.3838){
  
  p <- ggplot(data = df, aes(x = 1, y = ..count.., fill = Survived)) +
    geom_bar() +
    scale_fill_manual(values = c("gray50", "orangered2")) +
    # Se añade una línea horizontal en el nivel basal
    geom_hline(yintercept = nrow(df) * threshold_line,
               linetype = "dashed") +
    labs(title = grupo) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
  return(p)
}

datos_graficos <- datos_cualitativos_tidy %>%
  filter(variable_grupo %in% top6_grupos) %>%
  group_by(variable_grupo) %>%
  nest() %>%
  arrange(variable_grupo)

plots <- map2(datos_graficos$variable_grupo, .y = datos_graficos$data,
              .f = plot_grupo)

ggarrange(plotlist = plots, common.legend = TRUE)



#Importancia de las variables a traves de random forest

library(randomForest)
datos_rf <- datos %>%
  select(-PassengerId, -Name, -Ticket, -Cabin, -Age, -Age_grupo) %>%
  na.omit()
datos_rf <- map_if(.x = datos_rf, .p = is.character, .f = as.factor) %>%
  as.data.frame()
modelo_randforest <- randomForest(formula = Survived ~ . ,
                                  data = na.omit(datos_rf),
                                  mtry = 5,
                                  importance = TRUE, 
                                  ntree = 1000) 
importancia <- as.data.frame(modelo_randforest$importance)
importancia <- rownames_to_column(importancia,var = "variable")

p1 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                     y = MeanDecreaseAccuracy,
                                     fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducción de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseGini),
                                     y = MeanDecreaseGini,
                                     fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Reducción de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)





#####División de los datos en entrenamiento y test



library("caret")
set.seed(123)
# Se crean los índices de las observaciones de entrenamiento
train <- createDataPartition(y = datos$Survived, p = 0.8, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]


#verificamos que tenemos una relacion muy similar en las dos muestras
prop.table(table(datos_train$Survived))
prop.table(table(datos_test$Survived))




#DATOS QUE TENEMOS
nrow(datos)
#DATOS DESPUES DE QUITAR TODOS LO NA
nrow(na.omit(datos))

#IMPUTACION PARA EL PUERTO  CAMBIANDO NA POR C
datos_train <- datos_train %>%
  mutate(Embarked = replace(Embarked, is.na(Embarked), "C"))
datos_test <-  datos_test %>%
  mutate(Embarked = replace(Embarked, is.na(Embarked), "C"))


#PARA LA VARIABLE AGE GRUPO
library(recipes)
# Se crea un objeto recipe() con la variable respuesta y los predictores. 
# Las variables *PassengerId*, *Name*, *Ticket* no parecen aportar información
# relevante sobre la supervivencia de los pasajeros. Excluyendo todas estas
# variables, se propone como modelo inicial el formado por los predictores:
#  Pclass + Sex + SibSp + Parch + Fare + Embarked + Age_grupo.

objeto_recipe <- recipe(formula = Survived ~ Pclass + Sex + SibSp + Parch +
                          Fare + Embarked + Age_grupo,
                        data =  datos_train)
objeto_recipe

#step_bagimpute(): imputación vía Bagged Trees.
objeto_recipe <- objeto_recipe %>% step_bagimpute(Age_grupo)
objeto_recipe


#TEST DE VARINZA 0 PARA ELIMINAR VARIABLES

datos %>% select(Pclass, Sex, SibSp, Parch, Fare, Embarked, Age_grupo) %>%
  nearZeroVar(saveMetrics = TRUE)

#NINGUNA VARIABLE TIENE VARIANZA CERO O CERCANA

objeto_recipe <- objeto_recipe %>% step_nzv(all_predictors())


#ESTANDARIZAWR TODAS LAS VARIABLES NUMERICAS
objeto_recipe <- objeto_recipe %>% step_center(all_numeric())
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())


#BINARIZACION DE LAS VARIABLES FACTRES O CARACTERES
objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())

# Una vez que se ha creado el objeto recipe con todas las transformaciones de preprocesado, se aprenden con los datos de entrenamiento y se aplican a los dos conjuntos.
# Se entrena el objeto recipe
trained_recipe <- prep(objeto_recipe, training = datos_train)
trained_recipe

#APLICAMOS LAS TRANSFORMACIONES AL CONJUNTO DE ENTRENAMIENTO Y AL DE PRUEBA
datos_train_prep <- bake(trained_recipe, new_data = datos_train)
datos_test_prep  <- bake(trained_recipe, new_data = datos_test)


glimpse(datos_train_prep)




install.packages("doMC", repos="http://R-Forge.R-project.org")




# ELIMINACIÓN RECURSIVA MEDIANTE RANDOM FOREST Y BOOTSTRAPPING
# =============================================================================

# Se paraleliza el proceso para que sea más rápido. El número de cores debe 
# seleccionarse en función del ordenador que se está empleando.
library(doMC)
registerDoMC(cores = 4)
# Tamaño de los conjuntos de predictores analizados
subsets <- c(3:11)

# Número de resamples para el proceso de bootstrapping
repeticiones <- 30

# Se crea una semilla para cada repetición de validación. Esto solo es necesario si
# se quiere asegurar la reproducibilidad de los resultados, ya que la validación
# cruzada y el bootstrapping implican selección aleatoria.

# El número de semillas necesarias depende del número total de repeticiones: 
# Se necesitan B+1 elementos donde B es el número total de particiones (CV) o
# resampling (bootstrapping). Los primeros B elementos deben ser vectores formados
# por M números enteros, donde M es el número de modelos ajustados, que en este caso
# se corresponde con el número de tamaños. El último elemento solo necesita un único
# número para ajustar el modelo final.
set.seed(123)
seeds <- vector(mode = "list", length = repeticiones + 1)
for (i in 1:repeticiones) {
  seeds[[i]] <- sample.int(1000, length(subsets))
} 
seeds[[repeticiones + 1]] <- sample.int(1000, 1)

# Se crea un control de entrenamiento donde se define el tipo de modelo empleado
# para la selección de variables, en este caso random forest, la estrategia de
# resampling, en este caso bootstrapping con 30 repeticiones, y las semillas para
# cada repetición. Con el argumento returnResamp = "all" se especifica que se
# almacene la información de todos los modelos generados en todas las repeticiones.
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "boot", number = repeticiones,
                       returnResamp = "all", allowParallel = TRUE, verbose = FALSE,
                       seeds = seeds)

# Se ejecuta la eliminación recursiva de predictores
set.seed(342)
rf_rfe <- rfe(Survived ~ ., data = datos_train_prep,
              sizes = subsets,
              metric = "Accuracy",
              # El accuracy es la proporción de clasificaciones correctas
              rfeControl = ctrl_rfe,
              ntree = 500)
# Dentro de rfe() se pueden especificar argumentos para el modelo empleado, por
# ejemplo, el hiperparámetro ntree=500.

rf_rfe
















# Paralelización
# =============================================================================
library(doMC)
registerDoMC(cores = 4)

# Control de entrenamiento
# =============================================================================
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "cv",
                       number = 5,
                       allowParallel = TRUE,
                       genParallel = TRUE, 
                       verbose = FALSE)

# Selección de predictores
# =============================================================================
set.seed(10)
rf_ga <- gafs(x = datos_train_prep %>% select(-Survived),
              y = datos_train_prep$Survived,
              iters = 10, 
              popSize = 10,
              gafsControl = ga_ctrl,
              ntree = 100)


rf_ga











# FILTRADO DE PREDICTORES MEDIANTE ANOVA, RANDOM FOREST Y CV-REPETIDA
# =============================================================================
#n el código anterior, CV 10 veces significa dividir su conjunto de datos de entrenamiento al azar en 10 partes y luego usar cada una de las 10 partes como conjunto de datos de prueba para el modelo entrenado en otras 9. Tomamos el promedio de los 10 términos de error así obtenidos.


# Se paraleliza para que sea más rápido
library(doMC)
registerDoMC(cores = 4)

# Se crea una semilla para cada partición y cada repetición: el vector debe
# tener B+1 semillas donde B = particiones * repeticiones.
particiones = 10
repeticiones = 5
set.seed(123)
seeds <- sample.int(1000, particiones * repeticiones + 1)

# Control del filtrado
ctrl_filtrado <- sbfControl(functions = rfSBF, method = "repeatedcv",
                            number = particiones, repeats = repeticiones,
                            seeds = seeds, verbose = FALSE, 
                            saveDetails = TRUE, allowParallel = TRUE)
set.seed(234)
rf_sbf <- sbf(Survived ~ ., data = datos_train_prep,
              sbfControl = ctrl_filtrado,
              # argumentos para el modelo de evaluación
              ntree = 500)



rf_sbf



#####PRIMER MODELO MAQUINA VECTOR SOPORTE LINEAL#####
# Predictores: Survived, SibSp, Parch, Fare, Pclass_X2, Pclass_X3, Sex_male, Embarked_Q,
#              Embarked_S, Age_grupo_anciano, Age_grupo_niño
modelo_svmlineal <- train(Survived ~ ., method = "svmLinear", data = datos_train_prep)


modelo_svmlineal$finalModel

#error de entrenamiento (training error). Este error se corresponde con el error que comete el modelo al predecir las mismas observaciones con las que se ha entrenado, un 20% en este caso.



###como la evalucion se hice con la misma data de entrenamiento el error test puede 
#estar influidapor esta por lo que hacemos otro tipo de analis antes de probar
#con la base de prueba




#Se ajusta de nuevo una máquina vector soporte lineal, esta vez con validación cruzada repetida para estimar su error.






# PARALELIZACIÓN DE PROCESO
#===============================================================================
# Se paraleliza para que sea más rápido. El número de cores activados depende del
# las características del ordenador donde se ejecute el código.
library(doMC)
registerDoMC(cores = 4)

# NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
# Es este caso se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

# Las semillas solo son necesarias si se quiere asegurar la reproducibilidad de
# los resultados, ya que la validación cruzada y el bootstrapping implican selección
# aleatoria. Las semillas se almacenan en una lista con B+1 elementos donde B depende
# del método de validación empleado:
# 
#   "cv": B es el número total de particiones
#   "repeatedcv": B es el número de particiones por el número de repeticiones.
#   "boot": B es el número de resamples.
#   "LOGOCV":   B es el número de repeticiones.
#   
# Los primeros B elementos deben ser vectores formados por M números enteros,
# donde M es el número de modelos ajustados en cada partición o repetición,
# es decir, el total de hiperparámetros comparados. El último elemento (B+1) solo
# necesita un único número para ajustar el modelo final.

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  # Cada elemento de la lista, excepto el último tiene que tener tantas semillas
  # como hiperparámetros analizados. En este caso, se emplea el valor por 
  # defecto C = 1, por lo que cada elemento de seeds está formada por un
  # único valor.
  seeds[[i]] <- sample.int(1000, 1) 
}
# La última semilla se emplea para ajustar el modelo final con todas las observaciones.
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmlineal <- train(Survived ~ ., data = datos_train_prep,
                          method = "svmLinear",
                          metric = "Accuracy",
                          trControl = control_train)
modelo_svmlineal




# Valores de validación (Accuracy y Kappa) obtenidos en cada partición y repetición.
modelo_svmlineal$resample %>% head(50)


summary(modelo_svmlineal$resample$Accuracy)




#representacion grafica

p1 <- ggplot(data = modelo_svmlineal$resample, aes(x = Accuracy)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modelo_svmlineal$resample$Accuracy),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modelo_svmlineal$resample, aes(x = 1, y = Accuracy)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Accuracy obtenido en la validación", size = 15))
final_plot



#El accuracy promedio estimado mediante validación cruzada repetida es de 0.79, el modelo predice correctamente la supervivencia de los pasajeros un 79% de las veces. 


####OPTIMIZACION DE HIPERPARAMETROS###


# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

hiperparametros <- data.frame(C = c(0.001, 0.01, 0.1, 0.5, 1, 10))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmlineal <- train(Survived ~ ., data = datos_train_prep,
                          method = "svmLinear",
                          tuneGrid = hiperparametros,
                          metric = "Accuracy",
                          trControl = control_train)
modelo_svmlineal





--------------------------------------------------------------------------
  
  
  
  
  
  # PARALELIZACIÓN DE PROCESO
  #===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros aleatorios
set.seed(123)
hiperparametros <- data.frame(C = runif(n = 5, min = 0.001, max = 20))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmlineal_random <- train(Survived ~ ., data = datos_train_prep,
                                 method = "svmLinear",
                                 tuneGrid = hiperparametros,
                                 metric = "Accuracy",
                                 trControl = control_train)



modelo_svmlineal_random










#HIPERPARAMETROS
----------------------------------------------------------
  
  # PARALELIZACIÓN DE PROCESO
  #===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Número de hiperparámetros aleatorios
n_hiperparametros <- 10

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, n_hiperparametros)
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "adaptive_cv", number = particiones,
                              repeats = repeticiones,
                              adaptive = list(min = 5, alpha = 0.05, 
                                              method = "gls", complete = TRUE),
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE, search = "random")

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmlineal_adaptative <- train(Survived ~ ., data = datos_train_prep,
                                     method = "svmLinear", trControl = control_train,
                                     tuneLength = 5)



modelo_svmlineal_adaptative











#PREDICCIONES

----------------------------------------------------------
  
  
  predicciones_raw <- predict(modelo_svmlineal, newdata = datos_test_prep,
                              type = "raw")
predicciones_raw





# El algoritmo svmLinear no calcula de forma nativa probabilidades, para obtenerlas
# se reajusta el modelo indicando `classProbs = TRUE`.
particiones  <- 10
repeticiones <- 5
hiperparametros <- expand.grid(C = c(1))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              classProbs = TRUE, allowParallel = TRUE)

set.seed(342)
modelo_svmlineal <- train(Survived ~ ., data = datos_train_prep,
                          method = "svmLinear",
                          tuneGrid = hiperparametros,
                          metric = "Accuracy",
                          trControl = control_train)

predicciones_prob <- predict(modelo_svmlineal, newdata = datos_test_prep,
                             type = "prob")
predicciones_prob %>% head()



#TABLA DE RESULTADOS
---------------------------------------------
  predicciones <- extractPrediction(
    models = list(svm = modelo_svmlineal),
    testX = datos_test_prep %>% select(-Survived),
    testY = datos_test_prep$Survived
  )
predicciones %>% head()





#EFECTIVIDAD DEL MODELO
confusionMatrix(data = predicciones_raw, reference = datos_test_prep$Survived,
                positive = "Si")


# Error de test
error_test <- mean(predicciones_raw != datos_test_prep$Survived)
paste("El error de test del modelo:", round(error_test*100, 2), "%")









##############MODELO K-Nearest Neighbor (kNN)

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30, 50))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_knn <- train(Survived ~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)
modelo_knn




# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
  theme_bw()


##############Con un modelo kNN con k=2 se consigue un accuracy de validación promedio del 79%.




#############MODELO Naive Bayes
# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(usekernel = FALSE, fL = 0 , adjust = 0)

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_nb <- train(Survived ~ ., data = datos_train_prep,
                   method = "nb",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train)
modelo_nb


#Empleando un modelo Naive Bayes con usekernel = FALSE, fL = 0, adjust = 0, se consigue un accuracy promedio de validación del 79%.

#######MODELO Regresión logística

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_logistic <- train(Survived ~ ., data = datos_train_prep,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = "Accuracy",
                         trControl = control_train,
                         family = "binomial")
modelo_logistic



summary(modelo_logistic$finalModel)









### Distribución de Pérdidas Agregadas LDA
#Linear Discriminant Analysis 


# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_lda <- train(Survived ~ ., data = datos_train_prep,
                    method = "lda",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)
modelo_lda



modelo_lda$finalModel


#Empleando como modelo un LDA, se consigue un accuracy promedio de validación del 80.0%.


#Árbol de clasificación simple

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)
library(C50)
# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_C50Tree <- train(Survived ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros,
                        metric = "Accuracy",
                        trControl = control_train)
modelo_C50Tree



summary(modelo_C50Tree$finalModel)

#Empleando como modelo un árbol simple C5.0, se consigue un accuracy promedio de validación del 81%.








######MODELO BOSQUES ALEATOREOS

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- expand.grid(mtry = c(3, 4, 5, 7),
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
                               splitrule = "gini")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_rf <- train(Survived ~ ., data = datos_train_prep,
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train,
                   # Número de árboles ajustados
                   num.trees = 500)
modelo_rf


modelo_rf$finalModel


# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_rf, highlight = TRUE) +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Evolución del accuracy del modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()



#Empleando un modelo random forest con mtry = 7, min.node.size = 15 y splitrule = "gini", se consigue un accuracy promedio de validación del 84%.










###### MODELO Gradient boosting o Potenciación del gradiente,


# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- expand.grid(interaction.depth = c(1, 2),
                               n.trees = c(500, 1000, 2000),
                               shrinkage = c(0.001, 0.01, 0.1),
                               n.minobsinnode = c(2, 5, 15))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_boost <- train(Survived ~ ., data = datos_train_prep,
                      method = "gbm",
                      tuneGrid = hiperparametros,
                      metric = "Accuracy",
                      trControl = control_train,
                      # Número de árboles ajustados
                      distribution = "adaboost",
                      verbose = FALSE)
modelo_boost



# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_boost, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() +
  theme(legend.position = "bottom")


#Empleando un modelo boosting con n.trees = 500, minobsinnode = 2, interaction.depth = 2 y shrinkage = 0.1 se consigue un accuracy promedio de validación del 83%.






##### MODELO  MAQUINA DE VECTORES DE SOPORTE


# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- expand.grid(sigma = c(0.001, 0.01, 0.1, 0.5, 1),
                               C = c(1 , 20, 50, 100, 200, 500, 700))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmrad <- train(Survived ~ ., data = datos_train_prep,
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = "Accuracy",
                       trControl = control_train)
modelo_svmrad



modelo_svmrad$finalModel


# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_svmrad, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVM Radial") +
  theme_bw()



#Empleando un modelo SVM Radial con sigma = 0.001 y C = 500, se consigue un accuracy promedio de validación del 83%.












###MODELO DE REDES NEURONALES

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 4)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- expand.grid(size = c(10, 20, 50, 80, 100, 120),
                               decay = c(0.0001, 0.1, 0.5))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_nnet <- train(Survived ~ ., data = datos_train_prep,
                     method = "nnet",
                     tuneGrid = hiperparametros,
                     metric = "Accuracy",
                     trControl = control_train,
                     # Rango de inicialización de los pesos
                     rang = c(-0.7, 0.7),
                     # Número máximo de pesos
                     # se aumenta para poder incluir más meuronas
                     MaxNWts = 2000,
                     # Para que no se muestre cada iteración por pantalla
                     trace = FALSE)
modelo_nnet




# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_nnet, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo NNET") +
  theme_bw()



#Empleando un modelo NNET con size = 50 y decay = 0.5, se consigue un accuracy promedio de validación del 82%.






####COMPARACION DE MODELOS
#Métricas de validacion



modelos <- list(KNN = modelo_knn, NB = modelo_nb, logistic = modelo_logistic,
                LDA = modelo_lda, arbol = modelo_C50Tree, rf = modelo_rf,
                boosting = modelo_boost, SVMradial = modelo_svmrad,
                NNET = modelo_nnet)

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)



# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()





#Accuracy y Kappa promedio de cada modelo

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))



#GRAFICO

metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.62, linetype = "dashed") +
  annotate(geom = "text", y = 0.72, x = 8.5, label = "Accuracy basal") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()


#GRAFICA ACURACITY BASAL
metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.62, linetype = "dashed") +
  annotate(geom = "text", y = 0.65, x = 8.5, label = "Accuracy basal") +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")




#Test de Friedman para comparar el accuracy de los modelos

matriz_metricas <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = modelo, value = valor) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metricas)



#Para un nivel de significancia (α = 0.05), el test de Friedman sí encuentra evidencias para rechazar la hipótesis nula de que los 9 clasificadores consiguen la misma precisión,




#PRUEBA DE SUMA DE RANGOS DE WILKONXON

# Comparaciones múltiples con un test suma de rangos de Wilcoxon
# ==============================================================================

metricas_accuracy <- metricas_resamples %>% filter(metrica == "Accuracy")
comparaciones  <- pairwise.wilcox.test(x = metricas_accuracy$valor, 
                                       g = metricas_accuracy$modelo,
                                       paired = TRUE,
                                       p.adjust.method = "holm")

# Se almacenan los p_values en forma de dataframe
comparaciones <- comparaciones$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modeloA") %>%
  gather(key = "modeloB", value = "p_value", -modeloA) %>%
  na.omit() %>%
  arrange(modeloA) 

comparaciones



#Acorde a las comparaciones por pares, no existen evidencias suficientes para considerar que la capacidad predictiva de los modelos Random Forest, SVM radial y Boosting es distinta.




#########USO DE LOS DATOS DE PRUEBA


predicciones <- extractPrediction(
  models = modelos,
  testX = datos_test_prep %>% select(-Survived),
  testY = datos_test_prep$Survived
)
predicciones %>% head()




###RESULTADOS DE TODOS LOS MODELOS

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))

metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))



#GRAFICO %DE EFECTIVIDAD DEL MODELO EN DATA TRINIG Y TEST

ggplot(data = metricas_predicciones,
       aes(x = reorder(object, accuracy), y = accuracy,
           color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.62, linetype = "dashed") +
  annotate(geom = "text", y = 0.66, x = 8.5, label = "Accuracy basal") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")



#COMPARATIVA ENTRE PARES DE MODELO
comparaciones %>% filter((modeloA == "rf") | (modeloA == "SVMradial" & modeloB == "rf"))









# ################COMBINACION DE MODELOS Model ensembling (stacking)

