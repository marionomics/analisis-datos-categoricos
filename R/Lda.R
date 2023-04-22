# Linear Discriminant Analysis

# Cargar las librerías
library(MASS)
library(ggplot2)

# Instalar la base de datos

attach(iris)
str(iris)

# Escalamos los datos para que la media sea 0 y la desv. est. sea 1
iris[1:4] <- scale(iris[1:4])

# La media
apply(iris[1:4], 2, mean)
apply(iris[1:4], 2, sd)

##########################################
set.seed(42)

muestra <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob = c(0.7, 0.3))

train <- iris[muestra, ]
test <- iris[!muestra, ]

################################
# Ajustar un modelo LDA

modelo <- lda(Species ~ ., data =  train)
modelo

# Predecir en los datos de prueba
prediccion <- predict(modelo, test)

prediccion


#### Checar la precisión de nuestro modelo
mean(prediccion$class==test$Species)

## Visualización de los resultados

plot_lda <- cbind(train, predict(modelo)$x)

png('plots/lda.png')
ggplot(plot_lda, aes(LD1, LD2))+
    geom_point(aes(color = Species))
dev.off()
