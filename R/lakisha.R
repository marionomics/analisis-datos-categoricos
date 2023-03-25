# Paso 0: Carga las librerías
library(tidyverse)

# Paso 1: Carga los datos de github
df_lakisha <- foreign::read.dta("https://github.com/marionomics/modelos-lineales/blob/main/data/lakisha/lakisha_aer.dta?raw=true")


# Paso 2: explora los datos
head(df_lakisha)
str(df_lakisha)

# Paso 3: Realiza una tabla de contingencia
table(df_lakisha$race, df_lakisha$call) %>%
    prop.table(1)

prop.table(table(df_lakisha$race, df_lakisha$call),1)

# Los nombres de personas afroamericanas (b) y blancos (w)
df_lakisha$firstname[df_lakisha$race == "b"] %>% unique()


table(df_lakisha$race, df_lakisha$call, df_lakisha$sex) %>%
    prop.table(1)

mean(df_lakisha$fracblack, na.rm = TRUE)

modelo_logit_lakisha <- glm(call ~ fracblack, 
    family = binomial,
    data = df_lakisha)
summary(modelo_logit_lakisha)

modelo_logit_lakisha$coefficients

calcular_prob <- function(coeficiente){
    odds <- exp(coeficiente)
    prob <- odds/(1+odds)
    return(prob)
}

calcular_prob(modelo_logit_lakisha$coefficients[1])
calcular_prob(modelo_logit_lakisha$coefficients[2])

