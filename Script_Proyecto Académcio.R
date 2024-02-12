### Proyecto Académico-Predicción de problemas cardiovasculares
## Emanuele De Stefano
# 08/01/2024

#### Bibliografía ####

"El dataset utilizado en el siguiente trabajo se obtuvo a través del portal Kaggel.
Este es de libre acceso y contiene datos utilizados para la predicción de 
riesgo a sufrir muerte por fallo cardíaco."

"BMC Medical Informatics and Decision Making 20, 16 (2020). 
(https://www.kaggle.com/datasets/andrewmvd/heart-failure-clinical-data?resource=download)"

#### 1.- Datos y variables ####-------------------------------------------------

## 1.1- Importar datos- Crear dataset a partir de archivo csv:
  Datos <- read.csv("Data/heart_failure_clinical_records_dataset.csv")

## 1.2- Crear variables a partir de las columnas del dataset solo para variables relevantes:

  # Variables categóricas binarias (Borales):
  Hipertension <- as.factor(Datos$high_blood_pressure) # 0=No & 1=Si
  Diabetes <- as.factor(Datos$diabetes) # 0=No & 1=Si
  Anemia <- as.factor(Datos$anaemia) # 0=No & 1=Si
  Fumador <- as.factor(Datos$smoking) # 0=No & 1=Si
  Sexo <- as.factor(Datos$sex) # 0=Mujer & 1=Hombre
  Muerte_cardiovascular <- as.factor(Datos$DEATH_EVENT) # 0=No & 1=Si

  # Variables numéricas:
  Edad <- Datos$age
  Fraccion_Eyeccion <- Datos$ejection_fraction 
  # Porccentaje de sangre que sale del corazón en cada contracción
  Plaquetas <- Datos$platelets
    # Número de plaquetas por microlitro
    #plot(density(Plaquetas))
  Sodio <- Datos$serum_sodium
    # Sodio en suero en unidades de miliequivalente por litro
    #plot(density(Sodio))
    #lines(seq(100, 160, 0.1), dnorm(seq(100, 160, 0.1), mean = mean(Sodio), sd = sd(Sodio)), col="red")
  Creatinina <- Datos$serum_creatinine
    # Niveles de creatinina en suero en unidades de mg/dL.



#### 2.- Análisis de distribución de variables (T-test)####---------------------
library(Hmisc) #contiene función "describe"
describe(Datos)

"Lo que buscamos es entender cuáles variables están relacionadas con la muerte 
por fallo cardiovascular. Dado que el objetivo es poder pronosticar, la variable
Muerte_Cardiovascular no nos es de utilidad, buscaremos entonces variables 
cuantificables (numéricas continuas) que se vean representadas de manera distinta
en personas sanas y aquellas que posteriormente presentaron muerte por fallo 
cardio vascular. EN RESUMEN- Haremos Una serie de T-test para identificar cuál
variable numérica presenta una diferencia más significativa entre indivduos sanos
y con muerte por fallo cardiovascular."

# 2.1- Separar variables numéricas de acuerdo a variable categórica
  # Fracción de eyección:
  Frac.Eyeccion_Sano <- Fraccion_Eyeccion[Muerte_cardiovascular==0]
  Frac.Eyeccion_Muerte <- Fraccion_Eyeccion[Muerte_cardiovascular==1]

  # Plaquetas:
  Plaquetas_Sano <- Plaquetas[Muerte_cardiovascular==0]
  Plaquetas_Muerte <-Plaquetas[Muerte_cardiovascular==1]
  
  # Sodio:
  Sodio_Sano <- Sodio[Muerte_cardiovascular==0]
  Sodio_Muerte <-Sodio[Muerte_cardiovascular==1]
  
  # Creatinina:
  Creatinina_Sano <- Creatinina[Muerte_cardiovascular==0]
  Creatinina_Muerte <-Creatinina[Muerte_cardiovascular==1]
  
# 2.2- T-test
  library(ggplot2) #Paquete para hacer boxplots que ayuden a vizualizar 
  
  # Fracción de eyección:
  t.test(Frac.Eyeccion_Sano,Frac.Eyeccion_Muerte)
  ggplot() +
    geom_boxplot(data = data.frame(Frac_Eyeccion = Frac.Eyeccion_Sano), aes(x = "Sano", y = Frac_Eyeccion, fill = "Sano"), position = position_dodge(width = 0.75), alpha = 0.5) +
    geom_boxplot(data = data.frame(Frac_Eyeccion = Frac.Eyeccion_Muerte), aes(x = "Muerte Cardio Vascular", y = Frac_Eyeccion, fill = "MMuerte Cardio Vascular"), position = position_dodge(width = 0.75), alpha = 0.5) +
    labs(title = "Comparación de Fracción de Eyección",
         x = "Categoría",
         y = "Fracción de Eyección") +
    theme_minimal()
  
  # Plaquetas:
  t.test(Plaquetas_Sano,Plaquetas_Muerte)
  ggplot() +
    geom_boxplot(data = data.frame(Plaquetas = Plaquetas_Sano), aes(x = "Sano", y = Plaquetas, fill = "Sano"), position = position_dodge(width = 0.75), alpha = 0.5) +
    geom_boxplot(data = data.frame(Plaquetas = Plaquetas_Muerte), aes(x = "Muerte Cardio Vascular", y = Plaquetas, fill = "Muerte Cardio Vascular"), position = position_dodge(width = 0.75), alpha = 0.5) +
    labs(title = "Comparación de Plaquetas",
         x = "Categoría",
         y = "Plaquetas") +
    theme_minimal()
  
  # Sodio:
  t.test(Sodio_Sano,Sodio_Muerte)
  ggplot() +
    geom_boxplot(data = data.frame(Sodio = Sodio_Sano), aes(x = "Sano", y = Sodio, fill = "Sano"), position = position_dodge(width = 0.75), alpha = 0.5) +
    geom_boxplot(data = data.frame(Sodio = Sodio_Muerte), aes(x = "Muerte Cardio Vascular", y = Sodio, fill = "Muerte Cardio Vascular"), position = position_dodge(width = 0.75), alpha = 0.5) +
    labs(title = "Comparación de Sodio",
         x = "Categoría",
         y = "Sodio") +
    theme_minimal()
  
  # Creatinina:
  t.test(Creatinina_Sano,Creatinina_Muerte)
  ggplot() +
    geom_boxplot(data = data.frame(Creatinina = Creatinina_Sano), aes(x = "Sano", y = Creatinina, fill = "Sano"), position = position_dodge(width = 0.75), alpha = 0.5) +
    geom_boxplot(data = data.frame(Creatinina = Creatinina_Muerte), aes(x = "Muerte Cardio Vascular", y = Creatinina, fill = "Muerte Cardio Vascular"), position = position_dodge(width = 0.75), alpha = 0.5) +
    labs(title = "Comparación de Creatinina",
         x = "Categoría",
         y = "Creatinina") +
    theme_minimal()
"A partir de los resultados anteriores se observa que las dos variables que 
  presentan una diferencia más significativa entre las medias de sus poblaciones
  sanas y con muerte cardiovascular son: Fracción de eyección, con un p-value = 
  9.647e-06 y Creatinina con un p-value = 6.399e-05. La varible Sodio tambien 
  presenta una diferencia significativa con un p-value = 0.0018. Por último, el 
  T-test de la variable Plaquetas arrojó un p-value mayor a 0.05 por lo que con 
  nuestros datos no encontramos información suficiente para rechazar la hipótesis
  nula, entendiendo que entonces la media de sus valores no es significativamente 
  distinta entre individuos sanos y con muerte por fallo cardiovascular, lamentable
  ya que era la única variable que seguía claramente un comportamiento normal, 
  facilitando su estudio."  
  
# 2.3- Análisis de homocedastisidad y normalidad 
"Para corroborar la fiabilidad de los resultados obtenidos en el T-test se 
  analizará que se cumplan los principios de homocedastisidad y normalidad de las
  variables numéricas categorizadas de acuerdo a la variable Muerte_cardiovascular"
  
  # Homocedastisidad (igualdad de varianza)
    
    # Fracción de eyección:
    var.test(Frac.Eyeccion_Sano,Frac.Eyeccion_Muerte)
    # Creatinina:
    var.test(Creatinina_Sano,Creatinina_Muerte)
    # Sodio:
    var.test(Sodio_Sano,Sodio_Muerte)
    
  "El test de varianza indicó que no se respeta el principio de homocedastisidad
    entre las categorías de las variables Creatinina y Sodio, obteniendo p-values
    de 2.2 E-16 y 0.007,respectivamente, mucho menor al limite de 0.05, por lo 
    que con nuestros datos existe información suficiente para rechazar la hipótesis 
    nula de que el ratio de las varianzas entre categorías es igual a 1. El test
    indica que el principio de homosedastisidad solo se cumple para las categorías
    de la variable Fracción de eyección, obteniendo un p-value = 0.09" 
    
  # Normalidad
    
    # Fracción de eyección:
    hist(Frac.Eyeccion_Sano)
    plot(density(Frac.Eyeccion_Sano))
    lines(seq(0, 100, 0.1), dnorm(seq(0, 100, 0.1), mean = mean(Frac.Eyeccion_Sano), sd = sd(Frac.Eyeccion_Sano)), col="red")
    qqnorm(Frac.Eyeccion_Sano)
    qqline(Frac.Eyeccion_Sano)
    library(psych)
    skew(Frac.Eyeccion_Sano)/sqrt(6/203)  #Asimetria estandarizada. skew(X)/sqrt(6/nº de muestras)
    kurtosi(Frac.Eyeccion_Sano)/sqrt(24/203) #Kurtosis estandarizada. KURTOSI(X)/sqrt(24/nº de muestras)
    shapiro.test(Frac.Eyeccion_Sano)
    
    hist(Frac.Eyeccion_Muerte)
    plot(density(Frac.Eyeccion_Muerte))
    lines(seq(0, 100, 0.1), dnorm(seq(0, 100, 0.1), mean = mean(Frac.Eyeccion_Muerte), sd = sd(Frac.Eyeccion_Muerte)), col="red")
    qqnorm(Frac.Eyeccion_Muerte)
    qqline(Frac.Eyeccion_Muerte)
    skew(Frac.Eyeccion_Muerte)/sqrt(6/96)  #Asimetria estandarizada. skew(X)/sqrt(6/nº de muestras)
    kurtosi(Frac.Eyeccion_Muerte)/sqrt(24/96) #Kurtosis estandarizada. KURTOSI(X)/sqrt(24/nº de muestras)
    shapiro.test(Frac.Eyeccion_Muerte)
    "Se analizó unicamente la normalidad de las categorías de la 
    variable Fracción de eyección ya que es la única que respetaba el principio
    de homocedastisidad. Los histogramas y gráficos de densidad arrojaban indicios
    de un comportamiento no normal, por lo que se graficaron los residuos en un
    QQplot, estos no se ajustaban del todo a la línea recta aunque seguián un patrón
    simétrico por lo que para definid de manera más precisa si estas variables
    siguen o no un comportamiento normal se analizaron los coeficientes de 
    asimetría y de curtosis estandarizados, en ambos casos seo obtuvieron valores
    fuera del rango de -2 a 2. Por último se realizaron tests de normalidad de 
    Shapiro -Wilk, obteniendo p-values de 4.7E-9 y 4.3E-5 respectivamente, valores
    mucho menores al valor crítico de 0.05, sugiriendo así que existe información
    suficiente para rechazar la hipotesis nula que indica un comportamiento normal."
    
    
    "Todo esto en conjunto indica poca fiabilidad en los resultados obtenidos en
    los T-test, no obstante, el T-test es un método robusto que en R por default
    aplica la corrección de Welch para no tener que asumir homosedastisidad, y
    el gran número de observaciones, 299>>30, hace que la distribución no normal
    no suponga un grave problema."
  
#### 3.-Análisis de factores relacionados a fallo cardiovacular (ANOVA) ###-----
"Habiendo decidido que nos interesa seguir estudiando la variable Fracción de
eyección debido a su diferencia entre sujetos sanos y con muerte por fallo 
cardiovascular, como indica el T-test, analizaremos ahora qué otras variables 
presentan una relación con esta mediante un análisis de tipo ANOVA"
    
# 3.1.- Transformar variables numéricas a categóricas:
  # Variables categóricas con más de un nivel:
  "Para analizar el efecto de variables numéricas sobre otra variable numérica
  LO IDEAL SERÍA HACER UNA REGRESIÓN LINEAL. No obstante ya que haremos un 
  análisis de tipo ANOVA hemos de transformar las variables numéricas continuas
  en variables categóricas con varias niveles. Para esto las dividiremos en 
  cuartiles."
  # Plaquetas:
  Plaquetas_Cuartiles <- cut(Plaquetas, breaks = quantile(Plaquetas, probs = seq
    (0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
    
  # Sodio:
  Sodio_Cuartiles <- cut(Sodio, breaks = quantile(Sodio, probs = seq(0, 1, by = 0.25))
    , labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
    
  # Creatinina:
  Creatinina_Cuartiles <- cut(Creatinina, breaks = quantile(Creatinina, probs = 
    seq(0, 1, by = 0.25)), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
  
# 3.2- Análisis ANOVA
  Modelo_anova <- aov(Fraccion_Eyeccion ~ Anemia + Diabetes + Hipertension + 
    Fumador + Plaquetas_Cuartiles + Sodio_Cuartiles + Creatinina_Cuartiles)
  summary(Modelo_anova)
 " De acuerdo a los p-values obtenidos en el análisis ANOVA no existe información
  suficiente para sugerir que alguna de las variables binarias o numéricas 
  transformadas en categóricas tengan una relación significativa o expliquen la
  variación de la variable Fracción de eyección. No obstante la variable Sodio
  obtuvo un p-value cercano al crítico (= 0.0582), lo que sugiere que existe algunos
  inidicios, aunque insuficientes o no extrapolables de que los  niveles de 
  sodio estan relacionados con los niveles de fracción de eyección y por ende con
  el riesgo a padecer muerte por fallo cardiovascular. Los resultados obtenidos
  no aportan suficiente sustento para esta conclusión ni explican si la relación
  entre niveles de sodio y fracción de eyección tienen una correlación positiva 
  o negativa ni si es de naturaleza lineal o no. Para esto sería preciso 
  realizar un análisis de regresión lineal." 
  
 