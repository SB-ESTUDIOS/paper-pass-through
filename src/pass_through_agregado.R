# Interest Rate Pass-Through Analysis
# Autor: Italo López
# Descripción: Este script realiza las estimaciones y gráficos para el análisis del pass-through. Asimismo, hace extracciones de la base de datos
# Fecha: Marzo 2025

# Cargar bibliotecas necesarias
library(dplyr)         # Manipulación de datos
library(ggplot2)       # Gráficos (no utilizado directamente en este script)
library(zoo)           # Manejo de series temporales
library(plotly)        # Gráficos interactivos
library(tibble)        # Manejo de tibbles
library(tseries)       # Pruebas estadísticas
library(ROracle)       # Conexión a Oracle
library(keyring)       # Gestión de contraseñas de base de datos
library(urca)          # Pruebas de raíz unitaria (ADF, KPSS)
library(lubridate)     # Manipulación de fechas
library(gap)           # Funciones económicas
library(tidyr)         # Manipulación de datos
library(timetk)        # Análisis de series temporales
library(lmtest)        # Paquete para hacer tests robustos
library(sandwich)
library(car)
library(moments)



# Cargar funciones auxiliares desde un script externo
source("/utils.R")

#Este script realiza un análisis del pass-through de los tipos de interés en República Dominicana




#Cargando data tasas de mercado pasivas
tasas_pasivas <- read.csv("data_bcrd_tasas_pasivas.csv")
tasas_pasivas <-tasas_pasivas%>%ts(start = c(2008,1),frequency = 12)
tasas_pasivas <-tasas_pasivas%>%as.zoo()

#Estimando relacion de largo plazo con OLS
model_tipm_360 <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA,data = tasas_pasivas)

summary(model_tipm_360)

#Ploteando desequilibrio de largo plazo
fig <- plot_ly(y=residuals(model_tipm_360)%>%as.numeric(),x=index(tasas_pasivas), type = 'scatter', mode = 'lines')%>%
  layout(
    title = "Desequilibrio de largo plazo: Muestra completa (tasas pasivas)",
    xaxis = list(title = "Fecha", titlefont = list(size = 18), tickfont = list(size = 18)),
    yaxis = list(title = "Puntos porcentuales", titlefont = list(size = 18), tickfont = list(size = 18))
  )
fig

long_run_disequilibrium <- residuals(model_tipm_360)

#Tests de estacionariedad del desequilibrio
adf_res <- ur.df(long_run_disequilibrium,type="none",selectlags = "AIC")
summary(adf_res)

###############En esta parte testeamos si el coeficiente de pass-through es igual a uno
coeftest(model_tipm_360, vcov = NeweyWest(model_tipm_360))  # HAC standard errors

# Wald test: test if beta_x = 1
linearHypothesis(model_tipm_360, "TIPM_INTERBANCARIA = 1", vcov = NeweyWest)

######################################





###########################En esta parte vemos el equilibrio de largo plazo por submuestras pre y post COVID


#Tomamos la data pre-COVID
pre_covid_data <- tasas_pasivas[index(tasas_pasivas)<as.yearmon(as.Date("2020-03-01")),]

#Estimamos el equilibrion de largo plazo para la submuestra pre-COVID
model_tipm_360_pre_covid <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA,data = pre_covid_data)

#Output del modelo
summary(model_tipm_360_pre_covid)

#Plot del desequilibrio
fig <- plot_ly(y=residuals(model_tipm_360_pre_covid)%>%as.numeric(),x=index(pre_covid_data), type = 'scatter', mode = 'lines')%>%
  layout(title="Long-run disequilibrium: Pre COVID" )
fig

long_run_disequilibrium_pre_covid <- residuals(model_tipm_360_pre_covid)

#Testeamos si el desequilibrio es estacionario, como es necesario en un ECM
adf_res <- ur.df(long_run_disequilibrium_pre_covid,type="none",selectlags = "AIC")
summary(adf_res)


###############En esta parte testeamos si el coeficiente de pass-through es igual a uno
coeftest(model_tipm_360_pre_covid, vcov = NeweyWest(model_tipm_360_pre_covid))  # HAC standard errors

# Wald test: test if beta_x = 1
linearHypothesis(model_tipm_360_pre_covid, "TIPM_INTERBANCARIA = 1", vcov = NeweyWest)

######################################


#Tomamos la data post-COVID
post_covid_data <- tasas_pasivas[index(tasas_pasivas)>=as.yearmon(as.Date("2020-03-01")),]

#Estimamos el equilibrion de largo plazo para la submuestra post-COVID
model_tipm_360_post_covid <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA,data = post_covid_data)

#Output del modelo
summary(model_tipm_360_post_covid)

#Plot del desequilibrio
fig <- plot_ly(y=residuals(model_tipm_360_post_covid)%>%as.numeric(),x=index(post_covid_data), type = 'scatter', mode = 'lines')%>%
  layout(title="Long-run disequilibrium: Post COVID" )
fig

long_run_disequilibrium_post_covid <- residuals(model_tipm_360_post_covid)

#Testeamos si el desequilibrio es estacionario, como es necesario en un ECM
adf_res <- ur.df(long_run_disequilibrium_post_covid,type="none",selectlags = "AIC")
summary(adf_res)


###############En esta parte testeamos si el coeficiente de pass-through es igual a uno
coeftest(model_tipm_360_post_covid, vcov = NeweyWest(model_tipm_360_post_covid))  # HAC standard errors

# Wald test: test if beta_x = 1
linearHypothesis(model_tipm_360_post_covid, "TIPM_INTERBANCARIA = 1", vcov = NeweyWest)

######################################





############Aqui hacemos un plot de las tasas para poder apreciar como ha cambiado 
############la relacion entre politica monetaria y tasas del mercado pasivas


# Convert zoo to data frame
df <- data.frame(
  fecha = index(tasas_pasivas),
  coredata(tasas_pasivas)
)

# Create the plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~TIPM_INTERBANCARIA, name = "TIPM_INTERBANCARIA", line = list(color = 'blue')) %>%
  add_lines(y = ~TIPM_PLAZO_360, name = "TIPM_PLAZO_360", line = list(color = 'red')) %>%
  layout(title = "Desequilibrio de las tasas del mercado",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Valor (puntos porcentuales)"))

fig



#Cargando data tasas de mercado activas
tasas_activas <- read.csv("data_bcrd_tasas_activas.csv")
tasas_activas <-tasas_activas%>%ts(start = c(2008,1),frequency = 12)
tasas_activas <-tasas_activas%>%as.zoo()


#Uniendo las tasas activas con las pasivas 
tasas <- full_join(tasas_pasivas%>%as.data.frame(), tasas_activas%>%as.data.frame(), by = c("ANO","MES"))

#Formateando el data frame como una serie de tiempo
tasas <-tasas%>%ts(start = c(2008,1),frequency = 12)
tasas <-tasas%>%as.zoo()


#Quitando lsa filas con nulls en la tasa activa de 360 dias
tasas <- tasas%>%head(-1)

#Estimando relacion de largo plazo con OLS
model_tiam_360 <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA,data = tasas)

summary(model_tiam_360)




#Ploteando desequilibrio
fig <- plot_ly(y=residuals(model_tiam_360)%>%as.numeric(),x=index(tasas), type = 'scatter', mode = 'lines')%>%
  layout(title="Desequilibrio de largo plazo: Muestra completa (Tasas activas)" ,
         xaxis = list(title = "Fecha", titlefont = list(size = 18), tickfont = list(size = 18)),
         yaxis = list(title = "Puntos porcentuales", titlefont = list(size = 18), tickfont = list(size = 18)))
fig

long_run_disequilibrium_activa <- residuals(model_tiam_360)

#Tests de estacionariedad del desequilibrio
adf_res <- ur.df(long_run_disequilibrium_activa,type="none",selectlags = "AIC")
summary(adf_res)


###############En esta parte testeamos si el coeficiente de pass-through es igual a uno
coeftest(model_tiam_360, vcov = NeweyWest(model_tiam_360))  # HAC standard errors

# Wald test: test if beta_x = 1
linearHypothesis(model_tiam_360, "TIPM_INTERBANCARIA = 1", vcov = NeweyWest)

######################################



############En esta parte estimamos el modelo por submuestras

#Tomamos data pre-COVID
pre_covid_data_activa <- tasas[index(tasas)<as.yearmon(as.Date("2020-03-01")),]

#EStimamos equilibrio de largo plazo
model_tiam_360_pre_covid <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA,data = pre_covid_data_activa)

summary(model_tiam_360_pre_covid)

#Ploteamos desequilibrio de corto plazo
fig <- plot_ly(y=residuals(model_tiam_360_pre_covid)%>%as.numeric(),x=index(pre_covid_data_activa), type = 'scatter', mode = 'lines')%>%
  layout(title="Long-run disequilibrium (tasa activa): Pre COVID" )
fig

long_run_disequilibrium_pre_covid_activa <- residuals(model_tiam_360_pre_covid)

#Tests de estacionariedad del desequilibrio
adf_res <- ur.df(long_run_disequilibrium_pre_covid_activa,type="none",selectlags = "AIC")
summary(adf_res)



###############En esta parte testeamos si el coeficiente de pass-through es igual a uno
coeftest(model_tiam_360_pre_covid, vcov = NeweyWest(model_tiam_360_pre_covid))  # HAC standard errors

# Wald test: test if beta_x = 1
linearHypothesis(model_tiam_360_pre_covid, "TIPM_INTERBANCARIA = 1", vcov = NeweyWest)

######################################




post_covid_data_activa <- tasas[index(tasas)>=as.yearmon(as.Date("2020-03-01")),]

model_tipm_360_post_covid_activa <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA,data = post_covid_data_activa)

summary(model_tipm_360_post_covid_activa)

fig <- plot_ly(y=residuals(model_tipm_360_post_covid_activa)%>%as.numeric(),x=index(post_covid_data_activa), type = 'scatter', mode = 'lines')%>%
  layout(title="Long-run disequilibrium (Tasa activa): Post COVID" )
fig

long_run_disequilibrium_post_covid_activa <- residuals(model_tipm_360_post_covid_activa)

#Tests de estacionariedad del desequilibrio
adf_res <- ur.df(long_run_disequilibrium_post_covid_activa,type="none",selectlags = "AIC")
summary(adf_res)


###############En esta parte testeamos si el coeficiente de pass-through es igual a uno
coeftest(model_tipm_360_post_covid_activa, vcov = NeweyWest(model_tiam_360_pre_covid))  # HAC standard errors

# Wald test: test if beta_x = 1
linearHypothesis(model_tipm_360_post_covid_activa, "TIPM_INTERBANCARIA = 1", vcov = NeweyWest)

######################################




#En esta parte ploteamos la tasa proxy de política monetaria con la tasa activa de 360 dias

#Formateamos la data para el plot
df <- data.frame(
  fecha = index(tasas),
  coredata(tasas)
)

# Creamos plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~TIPM_INTERBANCARIA, name = "TIPM_INTERBANCARIA", line = list(color = 'blue')) %>%
  add_lines(y = ~TIAM_PLAZO_360, name = "TIAM_PLAZO_360", line = list(color = 'red')) %>%
  layout(title = "Desequilibrio de las tasas del mercado",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Valor (puntos porcentuales)"))

fig


#En esta parte ploteamos la proxy de política monetaria, la tasa pasiva y la tasa activa
df <- data.frame(
  fecha = index(tasas),
  coredata(tasas)
)

#Creamos plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~TIPM_INTERBANCARIA, name = "Tasa interbancaria", line = list(color = 'blue')) %>%
  add_lines(y = ~TIAM_PLAZO_360, name = "Tasa activa de largo plazo (360 días)", line = list(color = 'red')) %>%
  add_lines(y = ~TIPM_PLAZO_360, name = "Tasa pasiva de largo plazo (360 días)", line = list(color = 'green')) %>%
  layout(
    title = list(
      text = "Desequilibrio de las tasas del mercado",
      font = list(size = 24),
      y = 0.95
    ),
    xaxis = list(
      title = list(text = "Fecha", font = list(size = 22)),
      tickfont = list(size = 18),
      showgrid = FALSE
    ),
    yaxis = list(
      title = list(text = "Valor (puntos porcentuales)", font = list(size = 22)),
      tickfont = list(size = 18),
      showgrid = FALSE
    ),
    legend = list(
      font = list(size = 20),
      y = 0.95,
      x = 0.75
    )
  )

fig



###############
####Testeando para ver orden de integracion de las tasas

summary(ur.df(tasas_pasivas$TIPM_PLAZO_360,type="drift",selectlags = "AIC"))
adf.test(tasas_pasivas$TIPM_PLAZO_360)
pp.test(tasas_pasivas$TIPM_PLAZO_360)
kpss.test(tasas_pasivas$TIPM_PLAZO_360)

summary(ur.df(tasas_pasivas$TIPM_PLAZO_360%>%diff(),type="drift",selectlags = "AIC"))
adf.test(tasas_pasivas$TIPM_PLAZO_360%>%diff())
pp.test(tasas_pasivas$TIPM_PLAZO_360%>%diff())
kpss.test(tasas_pasivas$TIPM_PLAZO_360%>%diff())

summary(ur.df(tasas_pasivas$TIPM_INTERBANCARIA,type="drift",selectlags = "AIC"))
adf.test(tasas_pasivas$TIPM_INTERBANCARIA)
pp.test(tasas_pasivas$TIPM_INTERBANCARIA)
kpss.test(tasas_pasivas$TIPM_INTERBANCARIA)

summary(ur.df(tasas_pasivas$TIPM_INTERBANCARIA%>%diff(),type="drift",selectlags = "AIC"))
adf.test(tasas_pasivas$TIPM_INTERBANCARIA%>%diff())
pp.test(tasas_pasivas$TIPM_INTERBANCARIA%>%diff())
kpss.test(tasas_pasivas$TIPM_INTERBANCARIA%>%diff())


summary(ur.df(tasas$TIAM_PLAZO_360,type="drift",selectlags = "AIC"))
adf.test(tasas$TIAM_PLAZO_360)
pp.test(tasas$TIAM_PLAZO_360)
kpss.test(tasas$TIAM_PLAZO_360)

summary(ur.df(tasas$TIAM_PLAZO_360%>%diff(),type="drift",selectlags = "AIC"))
adf.test(tasas$TIAM_PLAZO_360%>%diff())
pp.test(tasas$TIAM_PLAZO_360%>%diff())
kpss.test(tasas$TIAM_PLAZO_360%>%diff())



###################
# ECM model para la tasa pasiva


ecm_term <- long_run_results_function(model_tipm_360,tasas_pasivas,"Muestra completa (tasa pasiva)")


# Create First Differences
dY <- diff(tasas_pasivas$TIPM_PLAZO_360)
dX1 <- diff(tasas_pasivas$TIPM_INTERBANCARIA)
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_total_pasiva <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_total_pasiva)


# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_total_pasiva), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_total_pasiva))

###################
#ECM Model para la tasa pasiva pre covid
ecm_term <- long_run_results_function(model_tipm_360_pre_covid,pre_covid_data,"Muestra pre-COVID (tasa pasiva)")


# Create First Differences
dY <- diff(pre_covid_data$TIPM_PLAZO_360)
dX1 <- diff(pre_covid_data$TIPM_INTERBANCARIA)
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_pre_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_pre_covid)


# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_pre_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_pre_covid))

#####################
# ECM model para la tasa pasiva post covid

ecm_term_post_covid <- long_run_results_function(model_tipm_360_post_covid,post_covid_data,"Muestra post-COVID (tasa pasiva)")


dY <- diff(post_covid_data$TIPM_PLAZO_360)
dX1 <- diff(post_covid_data$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term_post_covid, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_post_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_post_covid)

# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_post_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_post_covid))

##################
#ECM para la tasa activa
ecm_term <- long_run_results_function(model_tiam_360,tasas,"Muestra total (tasa activa)")


dY <- diff(tasas$TIAM_PLAZO_360)
dX1 <- diff(tasas$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_activa <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_activa )


# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_activa), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_activa))

#####################
####ECM tasa activa pre-COVID
ecm_term <- long_run_results_function(model_tiam_360_pre_covid,pre_covid_data_activa,"Muestra pre-COVID (tasa activa)")


dY <- diff(pre_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(pre_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_activa_pre_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_activa_pre_covid)


# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_activa_pre_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_activa_pre_covid))


###################
# ECM model para la tasa activa post covid
ecm_term <- long_run_results_function(model_tipm_360_post_covid_activa,post_covid_data_activa,"Muestra post-COVID (tasa activa)")


dY <- diff(post_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(post_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_activa_post_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_activa_post_covid)



# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_activa_post_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_activa_post_covid))






#################################
#####En esta parte hacemos la rolling expanding regression




##########################En esta parte haremos Chow test
chow_test_r_pasiva <-chow.test(
  pre_covid_data$TIPM_PLAZO_360
  , pre_covid_data$TIPM_INTERBANCARIA
  , post_covid_data$TIPM_PLAZO_360
  , post_covid_data$TIPM_INTERBANCARIA
)

chow_test_r_pasiva




chow_test_r_activa <-chow.test(
  pre_covid_data_activa$TIAM_PLAZO_360
  , pre_covid_data_activa$TIPM_INTERBANCARIA
  , post_covid_data_activa$TIAM_PLAZO_360
  , post_covid_data_activa$TIPM_INTERBANCARIA
)

chow_test_r_activa


##############En esta parte haremos una versio robusta del test de Chow consistente en un test de Wald



#####Primero la tasa pasiva
# Suppose your data frame is 'data' and there's a time variable 'date'
break_point <- as.Date("2020-03-01")%>%as.yearmon()
tasas_pasivas$group <- ifelse(tasas_pasivas%>%index() >= break_point, 1, 0)  # 1 = after break

# Define the condition
condition <- tasas_pasivas$group == 1

# Create the subsamples
tasas_pasivas_pre <- subset(tasas_pasivas, group == 0)  # Before break
tasas_pasivas_post <- subset(tasas_pasivas, group == 1)  # After break

# Pooled model with interactions to test for structural break
model_pooled <- lm(TIPM_PLAZO_360 ~ TIPM_INTERBANCARIA + group:TIPM_INTERBANCARIA, data = tasas_pasivas)


# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("TIPM_INTERBANCARIA:group")

# Robust (HAC) covariance matrix
robust_cov <- vcovHAC(model_pooled)

# Wald test with robust standard errors
linearHypothesis(model_pooled, coefs_to_test, vcov = robust_cov)






####Ahora con la tasa activa
break_point <- as.Date("2020-03-01")%>%as.yearmon()
tasas$group <- ifelse(tasas%>%index() >= break_point, 1, 0)  # 1 = after break

# Define the condition
condition <- tasas$group == 1

# Create the subsamples
tasas_pre <- subset(tasas, group == 0)  # Before break
tasas_post <- subset(tasas, group == 1)  # After break


model_pooled <- lm(TIAM_PLAZO_360 ~ TIPM_INTERBANCARIA + group:TIPM_INTERBANCARIA, data = tasas)


# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("TIPM_INTERBANCARIA:group")

# Robust (HAC) covariance matrix
robust_cov <- vcovHAC(model_pooled)

# Wald test with robust standard errors
linearHypothesis(model_pooled, coefs_to_test, vcov = robust_cov)


###############################En esta parte hacemos una expanding regression
#Primero para la tasa pasiva
# Number of observations
n <- nrow(tasas)

# Store coefficients
coefs <- matrix(NA, nrow = n, ncol = 2)
colnames(coefs) <- c("(Intercept)", "Tasa_Interbancaria")

# Loop over expanding windows
for (i in 60:n) {  # start at 10 obs, adjust as needed
  fit <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA, data = tasas[1:i, ])
  coefs[i, ] <- coef(fit)
}

# Convert to zoo object with the same time index
coef_zoo <- zoo(coefs, order.by = index(tasas))


coef_df <- fortify.zoo(coef_zoo)


plot_ly(coef_df , x = ~Index, y = ~Tasa_Interbancaria, type = 'scatter', mode = 'lines') %>%
  layout(title = list(
    text = "Coeficiente de Traspaso de Regresión Expansiva (tasas pasivas)",
    font = list(size = 24),
    y = 0.97# Main title font size
  ),
         xaxis = list(
           title = list(text = "Fecha", font = list(size = 24)),   # Axis title font
           tickfont = list(size = 20)                             # Tick label font
         ),
         yaxis = list(
           title = list(text = "Coeficiente", font = list(size = 24)),
           tickfont = list(size = 20)
         ))


#Ahora para la tasa activa
n <- nrow(tasas)

# Store coefficients
coefs <- matrix(NA, nrow = n, ncol = 2)
colnames(coefs) <- c("(Intercept)", "Tasa_Interbancaria")

# Loop over expanding windows
for (i in 60:n) {  # start at 10 obs, adjust as needed
  fit <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA, data = tasas[1:i, ])
  coefs[i, ] <- coef(fit)
}

# Convert to zoo object with the same time index
coef_zoo <- zoo(coefs, order.by = index(tasas))


coef_df <- fortify.zoo(coef_zoo)


plot_ly(coef_df , x = ~Index, y = ~Tasa_Interbancaria, type = 'scatter', mode = 'lines') %>%
  layout(title = list(
    text = "Coeficiente de Traspaso de Regresión Expansiva (tasas activas)",
    font = list(size = 24),
    y = 0.97# Main title font size
  ),
  xaxis = list(
    title = list(text = "Fecha", font = list(size = 24)),   # Axis title font
    tickfont = list(size = 20)                             # Tick label font
  ),
  yaxis = list(
    title = list(text = "Coeficiente", font = list(size = 24)),
    tickfont = list(size = 20)
  ))



##############Graficando las dos juntas

n <- nrow(tasas)

# Coef for tasas pasivas
coefs_pasiva <- matrix(NA, nrow = n, ncol = 2)
colnames(coefs_pasiva) <- c("(Intercept)", "Tasa_Interbancaria")
for (i in 60:n) {
  fit <- lm(TIPM_PLAZO_360 ~ TIPM_INTERBANCARIA, data = tasas[1:i, ])
  coefs_pasiva[i, ] <- coef(fit)
}
coef_zoo_pasiva <- zoo(coefs_pasiva[, "Tasa_Interbancaria"], order.by = index(tasas))

# Coef for tasas activas
coefs_activa <- matrix(NA, nrow = n, ncol = 2)
colnames(coefs_activa) <- c("(Intercept)", "Tasa_Interbancaria")
for (i in 60:n) {
  fit <- lm(TIAM_PLAZO_360 ~ TIPM_INTERBANCARIA, data = tasas[1:i, ])
  coefs_activa[i, ] <- coef(fit)
}
coef_zoo_activa <- zoo(coefs_activa[, "Tasa_Interbancaria"], order.by = index(tasas))

# Combine both into one data frame
coef_df <- data.frame(
  Fecha = index(tasas),
  Coef_Pasiva = coredata(coef_zoo_pasiva),
  Coef_Activa = coredata(coef_zoo_activa)
)

# Plot with one y-axis
plot_ly(coef_df) %>%
  add_lines(x = ~Fecha, y = ~Coef_Pasiva, name = "Tasa Pasiva", line = list(color = 'blue')) %>%
  add_lines(x = ~Fecha, y = ~Coef_Activa, name = "Tasa Activa", line = list(color = 'red')) %>%
  layout(
    title = list(
      text = "",#Coeficiente de Traspaso de Regresión Expansiva (Tasas Activa y Pasiva)
      font = list(size = 24),
      y = 0.97
    )
    ,
    xaxis = list(
      title = list(text = "Fecha", font = list(size = 24)),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = list(text = "Coeficiente", font = list(size = 24)),
      tickfont = list(size = 20)
    ),
    legend = list(
      font = list(size = 20),
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top"
    )
  )

###########################Aquí plot de tasas con desequilibrio
ecm_term_pasiva <- long_run_results_function(model_tipm_360,tasas_pasivas,"Muestra completa (tasa pasiva)")

tasa_pasiva_y_desequilibrio <- merge(tasas_pasivas$TIPM_PLAZO_360,tasas_pasivas$TIPM_INTERBANCARIA,ecm_term_pasiva)



df <- data.frame(
  fecha = index(tasa_pasiva_y_desequilibrio),
  coredata(tasa_pasiva_y_desequilibrio)
)

# Create the plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~tasas_pasivas.TIPM_INTERBANCARIA, name = "Tasa interbancaria", line = list(color = 'blue')) %>%
  add_lines(y = ~ecm_term_pasiva, name = "Desequilibrio de largo plazo", line = list(color = 'red')) %>%
  add_lines(y = ~tasas_pasivas.TIPM_PLAZO_360, name = "Tasa pasiva de largo plazo (360 días)", line = list(color = 'green')) %>%
  layout(title = "Desequilibrio de las tasas del mercado",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Valor (puntos porcentuales)"))

fig


ecm_term_activa <- long_run_results_function(model_tiam_360,tasas,"Muestra total (tasa activa)")


ecm_term_pasiva <- ecm_term_pasiva%>%head(-1)

ecm_term_pasiva <-ecm_term_pasiva%>%ts(start = c(2008,1),frequency = 12)
ecm_term_pasiva <-ecm_term_pasiva%>%as.zoo()

ecm_term_activa <-ecm_term_activa%>%ts(start = c(2008,1),frequency = 12)
ecm_term_activa <-ecm_term_activa%>%as.zoo()

desequilibrio_activa_y_pasiva <- merge(ecm_term_pasiva,ecm_term_activa)



df <- data.frame(
  fecha = index(desequilibrio_activa_y_pasiva),
  coredata(desequilibrio_activa_y_pasiva)
)

# Create the plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~ecm_term_activa, name = "Desequilibrio de largo plazo (activa)", line = list(color = 'blue')) %>%
  add_lines(y = ~ecm_term_pasiva, name = "Desequilibrio de largo plazo (pasiva)", line = list(color = 'red')) %>%
  layout(
    title = list(
      text = "Desequilibrio de las tasas del mercado",
      font = list(size = 24),
      y = 0.95  # Optional: lowers the title slightly
    ),
    xaxis = list(
      title = list(text = "Fecha", font = list(size = 22)),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = list(text = "Valor (puntos porcentuales)", font = list(size = 22)),
      tickfont = list(size = 20)
    ),
    legend = list(
      font = list(size = 20),
      y = 0.95,
      x = 0.75# Optional: lowers the title slightly
    )
  )

fig




# Assume zdata is a zoo object with multiple columns
zscore_desequilibrio_activa_y_pasiva <- scale(desequilibrio_activa_y_pasiva)



df <- data.frame(
  fecha = index(zscore_desequilibrio_activa_y_pasiva),
  coredata(zscore_desequilibrio_activa_y_pasiva)
)

# Create the plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~ecm_term_activa, name = "Z-score Desequilibrio de largo plazo (activa)", line = list(color = 'blue')) %>%
  add_lines(y = ~ecm_term_pasiva, name = "Z-score Desequilibrio de largo plazo (pasiva)", line = list(color = 'red')) %>%
  layout(
    title = list(
      text = "Desequilibrio de las tasas del mercado",
      font = list(size = 24),
      y = 0.95  # Move the title slightly downward
    ),
    xaxis = list(
      title = list(text = "Fecha", font = list(size = 22)),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = list(text = "Z-score", font = list(size = 22)),
      tickfont = list(size = 20)
    ),
    legend = list(
      font = list(size = 20),
      y = 0.95,
      x = 0.70# Move the title slightly downward
    )
  )

fig



##############Ahora hacemos el test de cointegracion asimetrica


#Primero tasa pasiva
#Muestra pre-COVID
#Extraemos desequilibrio de largo plazo en el periodo pre-COVID
resid_pre_pasiva <- residuals(model_tipm_360_pre_covid)

tar_model <- tar_model_estimation(resid_pre_pasiva,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")



#Muestra post_COVID
resid_post_pasiva <- residuals(model_tipm_360_post_covid)

tar_model <- tar_model_estimation(resid_post_pasiva,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")


#Ahora tasa activa
#Muestra pre-COVID
#Extraemos desequilibrio de largo plazo en el periodo pre-COVID
resid_pre_activa <- residuals(model_tiam_360_pre_covid)

tar_model <- tar_model_estimation(resid_pre_activa,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")



#Muestra post_COVID
resid_post_activa <- residuals(model_tipm_360_post_covid_activa)

tar_model <- tar_model_estimation(resid_post_activa,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")


####Ahora calcularemos el numero de periodos para restablecer el equilibrio de largo plazo y su intervalo de confianza

#Primero la tasa pasiva
#Muestra pre-COVID

summary(optimal_model_pre_covid)

summary_model <- summary(optimal_model_pre_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_pasiva_pre_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_pre <- restoration_time_pasiva_pre_covid$distribution_periods



# Step 2: Compute histogram manually
hist_data_pre <- hist(data_pre, breaks = "FD", plot = FALSE)

rel_freqs <- hist_data_pre$counts / sum(hist_data_pre$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_pre$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre to include only values in valid bins
filtered_data_pre <- data_pre[sapply(data_pre, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre
dens_filtered_pre <- density(filtered_data_pre)

x_range <- range(dens_filtered_pre$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_pre <- restoration_time_pasiva_pre_covid$point_estimate 

# Step 5: Clip original data_pre to match density range for the histogram
hist_data_pre_clipped <- data_pre[data_pre >= x_range[1] & data_pre <= x_range[2]]

hist_time_pre_pasiva <- plot_ly() %>%
  add_trace(
    x = ~hist_data_pre_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_pre$x,
    y = ~dens_filtered_pre$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_pre, vline_x_pre),
    y = c(0, max(dens_filtered_pre$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período pre-COVID (tasa pasiva)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Valor", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_pre$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18),
      range = c(0, 25)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20)) 
  )

hist_time_pre_pasiva


#Estadistica descriptiva de la simulacion para muestra pre-COVID y tasa pasiva
stats <- list(
  mean   = mean(hist_data_pre_clipped, na.rm = TRUE),
  sd     = sd(hist_data_pre_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_pre_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_pre_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_pre_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_pre_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_pre_clipped, na.rm = TRUE)
)

stats

#Muestra post-COVID

summary(optimal_model_post_covid)

summary_model <- summary(optimal_model_post_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_pasiva_post_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_post <- restoration_time_pasiva_post_covid$distribution_periods
# Step 2: Compute histogram manually
hist_data_post <- hist(data_post, breaks = "FD", plot = FALSE)
rel_freqs <- hist_data_post$counts / sum(hist_data_post$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_post$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_post to include only values in valid bins
filtered_data_post <- data_post[sapply(data_post, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_post
dens_filtered_post <- density(filtered_data_post)

x_range <- range(dens_filtered_post$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_post <- restoration_time_pasiva_post_covid$point_estimate 

# Step 5: Clip original data_post to match density range for the histogram
hist_data_post_clipped <- data_post[data_post >= x_range[1] & data_post <= x_range[2]]

hist_time_post_pasiva  <- plot_ly() %>%
  add_trace(
    x = ~hist_data_post_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_post$x,
    y = ~dens_filtered_post$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_post, vline_x_post),
    y = c(0, max(dens_filtered_post$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período post-COVID (tasa pasiva)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Valor", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_post$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18),
      range = c(0, 25)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20)) 
  )

hist_time_post_pasiva

# Combine vertically with shared x-axis
fig <- subplot(hist_time_pre_pasiva, hist_time_post_pasiva, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "",#Tiempo de Restablecimiento del Equilibrio (tasas pasivas)
         annotations = list(
           list(
             x = 0.5,
             y = 0.95,
             text = "Período Pre-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           ),
           list(
             x = 0.5,
             y = 0.45,
             text = "Período Post-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           )
         ))

fig


#Estadistica descriptiva de la simulacion para muestra post-COVID y tasa pasiva
stats <- list(
  mean   = mean(hist_data_post_clipped, na.rm = TRUE),
  sd     = sd(hist_data_post_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_post_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_post_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_post_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_post_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_post_clipped, na.rm = TRUE)
)


stats

#Tasa activa

#Primero período pre-COVID
summary(optimal_model_activa_pre_covid)


summary_model <- summary(optimal_model_activa_pre_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_activa_pre_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)



data_pre_activa <- restoration_time_activa_pre_covid$distribution_periods
# Step 2: Compute histogram manually
hist_data_pre_activa <- hist(data_pre_activa, breaks = "FD", plot = FALSE)
rel_freqs <- hist_data_pre_activa$counts / sum(hist_data_pre_activa$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_pre_activa$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre_activa to include only values in valid bins
filtered_data_pre_activa <- data_pre_activa[sapply(data_pre_activa, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre_activa
dens_filtered_pre_activa <- density(filtered_data_pre_activa)

x_range <- range(dens_filtered_pre_activa$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_pre_activa <- restoration_time_activa_pre_covid$point_estimate 

# Step 5: Clip original data_pre_activa to match density range for the histogram
hist_data_pre_activa_clipped <- data_pre_activa[data_pre_activa >= x_range[1] & data_pre_activa <= x_range[2]]

hist_time_pre_activa <- plot_ly() %>%
  add_trace(
    x = ~hist_data_pre_activa_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_pre_activa$x,
    y = ~dens_filtered_pre_activa$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_pre_activa, vline_x_pre_activa),
    y = c(0, max(dens_filtered_pre_activa$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),  # now dashed
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período pre-COVID (tasa activa)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Valor", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_pre_activa$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20))  # 👈 makes "Estimador puntual" larger
  )

hist_time_pre_activa


#Estadistica descriptiva de la simulacion para muestra pre-COVID y tasa activa
stats <- list(
  mean   = mean(hist_data_pre_activa_clipped, na.rm = TRUE),
  sd     = sd(hist_data_pre_activa_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_pre_activa_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_pre_activa_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_pre_activa_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_pre_activa_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_pre_activa_clipped, na.rm = TRUE)
)


stats

#Período post-COVID
summary(optimal_model_activa_post_covid)


summary_model <- summary(optimal_model_activa_post_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_activa_post_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_post_activa <- restoration_time_activa_post_covid$distribution_periods
# Step 2: Compute histogram manually
hist_data_post_activa <- hist(data_post_activa, breaks = "FD", plot = FALSE)
rel_freqs <- hist_data_post_activa$counts / sum(hist_data_post_activa$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_post_activa$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_post_activa to include only values in valid bins
filtered_data_post_activa <- data_post_activa[sapply(data_post_activa, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_post_activa
dens_filtered_post_activa <- density(filtered_data_post_activa)

x_range <- range(dens_filtered_post_activa$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_post_activa <- restoration_time_activa_post_covid$point_estimate 

# Step 5: Clip original data_post_activa to match density range for the histogram
hist_data_post_activa_clipped <- data_post_activa[data_post_activa >= x_range[1] & data_post_activa <= x_range[2]]

hist_time_post_activa <- plot_ly() %>%
  add_trace(
    x = ~hist_data_post_activa_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_post_activa$x,
    y = ~dens_filtered_post_activa$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_post_activa, vline_x_post_activa),
    y = c(0, max(dens_filtered_post_activa$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),  # now dashed
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período post-COVID (tasa activa)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Valor", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_post_activa$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20))  # Emphasizes "Estimador puntual"
  )

hist_time_post_activa



# Combine vertically with shared x-axis
fig <- subplot(hist_time_pre_activa, hist_time_post_activa, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "",#Tiempo de Restablecimiento del Equilibrio (tasas activas)
         annotations = list(
           list(
             x = 0.6,
             y = 0.95,
             text = "Período Pre-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           ),
           list(
             x = 0.5,
             y = 0.45,
             text = "Período Post-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           )
         ))

fig


#Estadistica descriptiva de la simulacion para muestra post-COVID y tasa activa
stats <- list(
  mean   = mean(hist_data_post_activa_clipped, na.rm = TRUE),
  sd     = sd(hist_data_post_activa_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_post_activa_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_post_activa_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_post_activa_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_post_activa_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_post_activa_clipped, na.rm = TRUE)
)


stats



#####Estimaciones de dinámicas de corto plazo asimétricas

#Tasa pasiva pre-COVID

#TAR-ECM Model para la tasa pasiva pre covid
ecm_term <- long_run_results_function(model_tipm_360_pre_covid,pre_covid_data,"Muestra pre-COVID (tasa pasiva)")


# Create First Differences
dY <- diff(pre_covid_data$TIPM_PLAZO_360)
dX1 <- diff(pre_covid_data$TIPM_INTERBANCARIA)
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_pre_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_pre_covid)


#Tasa pasiva post-COVID

#TAR-ECM Model para la tasa pasiva post covid

ecm_term_post_covid <- long_run_results_function(model_tipm_360_post_covid,post_covid_data,"Muestra post-COVID (tasa pasiva)")


dY <- diff(post_covid_data$TIPM_PLAZO_360)
dX1 <- diff(post_covid_data$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term_post_covid, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_post_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_post_covid)



####ECM tasa activa pre-COVID
ecm_term <- long_run_results_function(model_tiam_360_pre_covid,pre_covid_data_activa,"Muestra pre-COVID (tasa activa)")


dY <- diff(pre_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(pre_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_activa_post_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_activa_post_covid)



####Ahora simularemos la distribución del tiempo de restablecimiento del equilibrio

#Período pre-COVID: bajadas y subidas de tasas pasivas
##Bajadas
summary(tar_optimal_model_pre_covid)

summary_model <- summary(tar_optimal_model_pre_covid)

alpha_hat <- summary_model$coefficients["ECT_pos","Estimate"]

se_alpha_hat <- summary_model$coefficients["ECT_pos","Std. Error"]

restoration_time_pasiva_pre_covid_positive <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_pre_positive <- restoration_time_pasiva_pre_covid_positive$distribution_periods



# Step 2: Compute histogram manually
hist_data_pre_positive <- hist(data_pre_positive, breaks = "FD", plot = FALSE)

rel_freqs <- hist_data_pre_positive$counts / sum(hist_data_pre_positive$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_pre_positive$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre to include only values in valid bins
filtered_data_pre_positive <- data_pre_positive[sapply(data_pre_positive, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre
dens_filtered_pre_positive <- density(filtered_data_pre_positive)

x_range <- range(dens_filtered_pre_positive$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_pre_positive <- restoration_time_pasiva_pre_covid_positive$point_estimate 

# Step 5: Clip original data_pre to match density range for the histogram
hist_data_pre_clipped_positive <- data_pre_positive[data_pre_positive >= x_range[1] & data_pre_positive <= x_range[2]]

hist_time_pre_pasiva_positive <- plot_ly() %>%
  add_trace(
    x = ~hist_data_pre_clipped_positive,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_pre_positive$x,
    y = ~dens_filtered_pre_positive$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_pre_positive, vline_x_pre_positive),
    y = c(0, max(dens_filtered_pre_positive$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "",#Histograma del Tiempo de Restablecimiento del Equilibrio período pre-COVID (tasa pasiva: bajadas)
      font = list(size = 22),
      y = 0.95
    ),
    xaxis = list(
      title = list(text = "Número de Períodos", font = list(size = 20)),
      tickfont = list(size = 20),
      range = dens_filtered_pre_positive$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 20)),
      tickfont = list(size = 20),
      range = c(0, 32)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20))
  )

hist_time_pre_pasiva_positive


stats <- list(
  mean   = mean(hist_data_pre_clipped_positive, na.rm = TRUE),
  sd     = sd(hist_data_pre_clipped_positive, na.rm = TRUE),
  q25    = quantile(hist_data_pre_clipped_positive, 0.25, na.rm = TRUE),
  median = median(hist_data_pre_clipped_positive, na.rm = TRUE),
  q75    = quantile(hist_data_pre_clipped_positive, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_pre_clipped_positive, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_pre_clipped_positive, na.rm = TRUE)
)


stats


##Subidas

ecm_term <- long_run_results_function(model_tipm_360_pre_covid,pre_covid_data,"Muestra pre-COVID (tasa pasiva)")


# Create First Differences
dY <- diff(pre_covid_data$TIPM_PLAZO_360)
dX1 <- diff(pre_covid_data$TIPM_INTERBANCARIA)
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_pre_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_pre_covid)


#TAR-ECM Model para la tasa activa post-COVID
ecm_term <- long_run_results_function(model_tipm_360_post_covid_activa,post_covid_data_activa,"Muestra post-COVID (tasa activa)")


dY <- diff(post_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(post_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_activa_post_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_activa_post_covid)



####Ahora simularemos la distribución del tiempo de restablecimiento del equilibrio

#Período pre-COVID: bajadas y subidas
##Subidas
summary(tar_optimal_model_pre_covid)

summary_model <- summary(tar_optimal_model_pre_covid)

alpha_hat <- summary_model$coefficients["ECT_neg","Estimate"]

se_alpha_hat <- summary_model$coefficients["ECT_neg","Std. Error"]

restoration_time_pasiva_pre_covid_negative <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_pre_negative <- restoration_time_pasiva_pre_covid_negative$distribution_periods



# Step 2: Compute histogram manually
hist_data_pre_negative <- hist(data_pre_negative, breaks = "FD", plot = FALSE)

rel_freqs <- hist_data_pre_negative$counts / sum(hist_data_pre_negative$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_pre_negative$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre to include only values in valid bins
filtered_data_pre_negative <- data_pre_negative[sapply(data_pre_negative, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre
dens_filtered_pre_negative <- density(filtered_data_pre_negative)

x_range <- range(dens_filtered_pre_negative$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_pre_negative <- restoration_time_pasiva_pre_covid_negative$point_estimate 

# Step 5: Clip original data_pre to match density range for the histogram
hist_data_pre_clipped_negative <- data_pre_negative[data_pre_negative >= x_range[1] & data_pre_negative <= x_range[2]]

hist_time_pre_pasiva_negative <- plot_ly() %>%
  add_trace(
    x = ~hist_data_pre_clipped_negative,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_pre_negative$x,
    y = ~dens_filtered_pre_negative$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_pre_negative, vline_x_pre_negative),
    y = c(0, max(dens_filtered_pre_negative$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "",#Histograma del Tiempo de Restablecimiento del Equilibrio período pre-COVID (tasa pasiva: subidas)
      font = list(size = 22),
      y = 0.95
    ),
    xaxis = list(
      title = list(text = "Número de Períodos", font = list(size = 20)),
      tickfont = list(size = 20),
      range = dens_filtered_pre_negative$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 20)),
      tickfont = list(size = 20),
      range = c(0, 32)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20))
  )

hist_time_pre_pasiva_negative


stats <- list(
  mean   = mean(hist_data_pre_clipped_negative, na.rm = TRUE),
  sd     = sd(hist_data_pre_clipped_negative, na.rm = TRUE),
  q25    = quantile(hist_data_pre_clipped_negative, 0.25, na.rm = TRUE),
  median = median(hist_data_pre_clipped_negative, na.rm = TRUE),
  q75    = quantile(hist_data_pre_clipped_negative, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_pre_clipped_negative, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_pre_clipped_negative, na.rm = TRUE)
)


stats



# Combine vertically with shared x-axis
fig <- subplot( hist_time_pre_pasiva_negative,hist_time_pre_pasiva_positive ,  nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "",#Tiempo de Restablecimiento del Equilibrio pre-COVID (tasas pasivas)
         annotations = list(
           list(
             x = 0.6,
             y = 0.95,
             text = "Subidas",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           ),
           list(
             x = 0.5,
             y = 0.45,
             text = "Bajadas",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           )
         ))

fig


#Período post-COVID: bajadas y subidas de tasas activass
##Bajadas
summary(tar_optimal_model_activa_post_covid)

summary_model <- summary(tar_optimal_model_activa_post_covid)

alpha_hat <- summary_model$coefficients["ECT_pos","Estimate"]

se_alpha_hat <- summary_model$coefficients["ECT_pos","Std. Error"]

restoration_time_activa_post_covid_positive <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)


data_post_positive <- restoration_time_activa_post_covid_positive$distribution_periods



# Step 2: Compute histogram manually
hist_data_post_positive <- hist(data_post_positive, breaks = "FD", plot = FALSE)

rel_freqs <- hist_data_post_positive$counts / sum(hist_data_post_positive$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.0025)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_post_positive$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre to include only values in valid bins
filtered_data_post_positive <- data_post_positive[sapply(data_post_positive, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre
dens_filtered_post_positive <- density(filtered_data_post_positive)

x_range <- range(dens_filtered_post_positive$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_post_positive <- restoration_time_activa_post_covid_positive$point_estimate 


# Step 5: Clip original data_pre to match density range for the histogram
hist_data_post_clipped_positive <- data_post_positive[data_post_positive >= x_range[1] & data_post_positive <= x_range[2]]

hist_time_post_activa_positive <- plot_ly() %>%
  add_trace(
    x = ~hist_data_post_clipped_positive,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_post_positive$x,
    y = ~dens_filtered_post_positive$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_post_positive, vline_x_post_positive),
    y = c(0, max(dens_filtered_post_positive$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "",#Histograma del Tiempo de Restablecimiento del Equilibrio período post-COVID (tasa activa: bajadas)
      font = list(size = 22),
      y = 0.95
    ),
    xaxis = list(
      title = list(text = "Número de Períodos", font = list(size = 20)),
      tickfont = list(size = 20),
      range = dens_filtered_post_positive$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 20)),
      tickfont = list(size = 20),
      range = c(0, 38)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20))
  )

hist_time_post_activa_positive


stats <- list(
  mean   = mean(hist_data_post_clipped_positive, na.rm = TRUE),
  sd     = sd(hist_data_post_clipped_positive, na.rm = TRUE),
  q25    = quantile(hist_data_post_clipped_positive, 0.25, na.rm = TRUE),
  median = median(hist_data_post_clipped_positive, na.rm = TRUE),
  q75    = quantile(hist_data_post_clipped_positive, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_post_clipped_positive, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_post_clipped_positive, na.rm = TRUE)
)


stats


##Subidas
summary(tar_optimal_model_activa_post_covid)

summary_model <- summary(tar_optimal_model_activa_post_covid)

alpha_hat <- summary_model$coefficients["ECT_neg","Estimate"]

se_alpha_hat <- summary_model$coefficients["ECT_neg","Std. Error"]

restoration_time_activa_post_covid_negative <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)


data_post_negative <- restoration_time_activa_post_covid_negative$distribution_periods



# Step 2: Compute histogram manually
hist_data_post_negative <- hist(data_post_negative, breaks = "FD", plot = FALSE)

rel_freqs <- hist_data_post_negative$counts / sum(hist_data_post_negative$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.0025)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_post_negative$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre to include only values in valid bins
filtered_data_post_negative <- data_post_negative[sapply(data_post_negative, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre
dens_filtered_post_negative <- density(filtered_data_post_negative)

x_range <- range(dens_filtered_post_negative$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_post_negative <- restoration_time_activa_post_covid_negative$point_estimate 


# Step 5: Clip original data_pre to match density range for the histogram
hist_data_post_clipped_negative <- data_post_negative[data_post_negative >= x_range[1] & data_post_negative <= x_range[2]]

hist_time_post_activa_negative <- plot_ly() %>%
  add_trace(
    x = ~hist_data_post_clipped_negative,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_post_negative$x,
    y = ~dens_filtered_post_negative$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_post_negative, vline_x_post_negative),
    y = c(0, max(dens_filtered_post_negative$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "",#Histograma del Tiempo de Restablecimiento del Equilibrio período post-COVID (tasa activa: subidas)
      font = list(size = 22),
      y = 0.95
    ),
    xaxis = list(
      title = list(text = "Número de Períodos", font = list(size = 20)),
      tickfont = list(size = 20),
      range = dens_filtered_post_negative$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 20)),
      tickfont = list(size = 20),
      range = c(0, 38)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20))
  )

hist_time_post_activa_negative


stats <- list(
  mean   = mean(hist_data_post_clipped_negative, na.rm = TRUE),
  sd     = sd(hist_data_post_clipped_negative, na.rm = TRUE),
  q25    = quantile(hist_data_post_clipped_negative, 0.25, na.rm = TRUE),
  median = median(hist_data_post_clipped_negative, na.rm = TRUE),
  q75    = quantile(hist_data_post_clipped_negative, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_post_clipped_negative, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_post_clipped_negative, na.rm = TRUE)
)


stats



# Combine vertically with shared x-axis
fig <- subplot(hist_time_post_activa_negative, hist_time_post_activa_positive ,  nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "",#Tiempo de Restablecimiento del Equilibrio post-COVID (tasas activas)
         annotations = list(
           list(
             x = 0.6,
             y = 0.95,
             text = "Subidas",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           ),
           list(
             x = 0.5,
             y = 0.45,
             text = "Bajadas",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           )
         ))

fig
