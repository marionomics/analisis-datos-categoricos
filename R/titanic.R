
titanic = read.csv("./datos/train-2.csv")

head(titanic)

prop.table(table(titanic$Survived, titanic$Pclass), 2)

mean(titanic$Fare[titanic$Pclass == 3])


titanic_logit <- glm(Survived ~ Fare, 
                    family = binomial(),
                        data = titanic)
summary(titanic_logit)

calcular_prob <- function(coeficiente){
    odds <- exp(coeficiente)
    prob <- odds/(1+odds)
    return(prob)
}

calcular_prob(titanic_logit$coefficients)



##  Clase

head(titanic)

titanic_logit <- glm(Survived ~ factor(Pclass),
                    family = binomial(),
                    data = titanic)

summary(titanic_logit)

calcular_prob <- function(coeficientes, segunda = 0, tercera = 0){
    odds <- exp(coeficientes[1] + coeficientes[2]* segunda + coeficientes[3]*tercera )
    probabilidad = odds/(1+odds)
    return(probabilidad)
}

calcular_prob(titanic_logit$coefficients, 0, 1)


head(titanic)

titanic %>%
    ggplot(aes(x = Age))+
    geom_density()

titanic_logit <- glm(Survived ~ factor(Pclass) + Sex + Age + SibSp,
                    family = binomial(),
                    data = titanic)
summary(titanic_logit)


calcular_prob <- function(coeficientes, segunda = 0, tercera = 0, hombre = 0, edad, sibsp = 0){
    odds <- exp(coeficientes[1] + coeficientes[2]* segunda + coeficientes[3]*tercera )
    probabilidad = odds/(1+odds)
    return(probabilidad)
}