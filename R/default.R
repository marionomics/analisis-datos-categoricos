library(tidyverse)

# install.packages('ISLR')
library(ISLR)

head(Default)


default_logit <- glm(default ~ balance,
                    family = binomial(),
                    data =  Default)

summary(default_logit)

calcular_prob <- function(x, coeficientes){
    odds <- exp(coeficientes[1] + coeficientes[2]* x)
    probabilidad = odds/(1+odds)
    return(probabilidad)
}

calcular_prob(2000, default_logit$coefficients)


### Student

default_logit <- glm(default ~ student,
                    family = binomial(),
                    data =  Default)

summary(default_logit)

calcular_prob(0, default_logit$coefficients)


### Gráfico
png('plots/default_logit.png')
plot(default_logit,2)
dev.off()


### Regresión Múltiple

default_logit <- glm(default ~ balance + student + income,
                    family = binomial(),
                    data =  Default)
summary(default_logit)

mean(Default$income)

calcular_prob <- function(coeficientes, balance = 1000, dummy_estudiante = 0, ingreso = 30000){
    odds <- exp(coeficientes[1] + coeficientes[2]* balance + coeficientes[3] * dummy_estudiante + coeficientes[4] * ingreso)
    probabilidad = odds/(1+odds)
    return(probabilidad)
}

calcular_prob(default_logit$coefficients,
             dummy_estudiante = 1,
             balance = 2000,
             ingreso = 40000)


# Genera predicciones

Default$prob_default <- calcular_prob(default_logit$coefficients, Default$balance, ifelse(Default$student == "Yes", 1, 0), Default$income)

Default %>%
    ggplot(aes(x = prob_default))+
    geom_density()
mean(Default$prob_default)

Default$pred_default <- ifelse(Default$prob_default > 0.0333, "Yes", "No")

table(Default$default, Default$pred_default)






