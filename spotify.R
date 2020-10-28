# Análise dos Dados do Spotify #################################################
# https://www.kaggle.com/edumucelli/spotifys-worldwide-daily-song-ranking
# https://spotifycharts.com/regional/br/daily/latest

# Pacotes ######################################################################
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(lubridate)
library(dygraphs)
library(xts)
library(forecast)
library(zoo)
library(imputeTS)
library(scales)
library(lmtest)
library(tseries)
library(fpp2)
library(prophet)
library(opera)
library(forecastHybrid)

load("spotifyatual.RData")

# Leitura dos Dados ############################################################

# df <-
#   read_csv("spotifys-worldwide-daily-song-ranking/data.csv")

df <- read_csv("spotify-br-2018-2020.csv")

# Tratamento dos Dados - 1 #####################################################

# Como é cada variável?
head(df)
str(df)
summary(df[, c(1, 4, 6)])

# Quantidade de dias da base
max(df$Date) - min(df$Date) + 1

# Temos quantidades iguais de músicas por posição
table(table(df$Position)) # 1017 dias, ok

# A frequência dos dados é diária, então precisamos possuir
# 1017 dias
length(table(df$Date))

# Não existe nenhuma posição repetida por data
df %>%
  group_by(Position, Date) %>%
  count() %>% filter(n != 1)

# Arrumando os nomes do dataframe
names(df) <-
  c("Position",
    "Track",
    "Artist",
    "Streams",
    "URL",
    "Date")

# save.image("spotifyatual.RData")

# Análise Descritiva ###########################################################

# Coeficiente de Variação
df %>% group_by(year(Date)) %>% summarise(desvio = sd(Streams)/mean(Streams))

#Como se comportam os streams para as 5 primeiras posições
#(sem levar em consideração a data)?
ggplot(df %>% filter(Position < 6),
       aes(y = Streams, col = factor(Position))) +
  geom_boxplot() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Como se comportam os streams para as 5 primeiras posições?
ggplot(df %>% filter(Position < 6),
       aes(
         x = Date,
         y = Streams
       )) +
  geom_line()+
facet_wrap(~Position)

# Quais os artistas com maior número de streams?
# Durante todo o período estudado.
ggplot(
  df %>%
    group_by(Artist) %>%
    summarise(Streams = sum(Streams)) %>%
    arrange(-Streams) %>%
    head(10),
  aes(y = (Streams) / 1000000, x = reorder(Artist, -Streams))
) +
  geom_bar(stat = 'identity') +
  scale_y_continuous("Streams, em milhões") +
  scale_x_discrete("Artista")+
  theme(axis.text.x = element_text(angle = 50))

# Quem são os artistas que mais aparecem
# nos rankings do Brasil, por data?
artistas <- df %>%
  group_by(Date, Artist) %>%
  summarise(c = n()) %>%
  filter(row_number(desc(c)) == 1)

plot_ly(df %>%
          group_by(Date) %>%
          summarise(c = sum(Streams)) %>% 
          left_join(artistas, by=c("Date"="Date")), 
        x = ~Date, y = ~c.x, type = 'scatter', mode = 'markers',
        hoverinfo = 'text',
        text = ~paste('</br> Artista: ', Artist,
                      '</br> # Músicas: ', c.y))

# Quantos streams o Brasil teve por dia?
ggplot(df %>%
         group_by(Date) %>%
         summarise(c = sum(Streams)),
       aes(x = Date, y = c)) +
  geom_line() +
  ggtitle("Quantos streams por dia?") +
  scale_y_continuous("Quantidade de Streams") +
  scale_x_date("Data")

# Qual a quantidade de streams por dia da semana?
# Primeiro precisamos saber que dia da semana cada dia cai
# Pelo pacote lubridate, a função wday() dá o dia exato,
# dado que a semana começa no Domingo (1).
df$WeekDay <- wday(df$Date)

weeks <- ggplot(df %>%
                  group_by(Date, WeekDay) %>%
                  summarise(c = sum(Streams)),
                aes(
                  x = Date,
                  y = c,
                  color = factor(WeekDay)
                )) +
  geom_line() +
  ggtitle("Quantos streams por dia da semana?") +
  scale_y_continuous("Quantidade de Streams") +
  scale_x_date("Data") +
  scale_color_discrete("Dia da Semana")

ggplotly(weeks)

weeks2 <- ggplot(df %>%
                   mutate(ANO=year(Date)) %>% 
                   group_by(ANO, WeekDay) %>%
                   summarise(c = median(Streams)),
                 aes(
                   x = WeekDay,
                   y = c,
                   color = factor(ANO)
                 )) +
  geom_line() +
  ggtitle("Média de streams por dia da semana") +
  scale_y_continuous("streams") +
  scale_color_discrete("Ano")

ggplotly(weeks2)

# E entre os meses?
df$DATA <- format(df$Date,"%m-%d")

months2 <- ggplot(df %>%
                    mutate(ANO=year(Date), MES = month(Date)) %>% 
                    group_by(ANO, MES) %>%
                    summarise(c = mean(Streams)),
                  aes(
                    x = MES,
                    y = c,
                    color = factor(ANO)
                  )) +
  geom_line() +
  ggtitle("Média de streams por mês") +
  scale_y_continuous("Streams") +
  scale_color_discrete("Ano")

ggplotly(months2)

# Modelagem ####################################################################

# Após feita a análise descritiva dos dados,
# temos uma base boa para começarmos a pensar
# em algum tipo de modelagem...

# Primeiro, precisamos pensar no que queremos responder.
# Quais nossos objetivos com esses dados?
# Vamos prever algo? Vamos apenas explicar/ajustar?

# Como análise inicial, queremos prever quantos streams teremos
# no próximo mês na playlist top 200 no Brasil.
# Para isso, utilizaremos modelos de séries temporais.

# Já sabemos que os streams são maiores de sexta e sábado do que
# nos outros dias da semana. Isso nos dá indícios de sazonalidade.
# Vimos também uma tendência crescente nos dados.
# Será que há outros tipos de sazonalidade ou ciclo que não pegamos?
# A variabilidade também aumenta conforme passam os anos...

streams <- df %>%
  group_by(Date) %>%
  summarise(c = sum(Streams))

serie <- xts(streams$c, order.by = streams$Date)

dygraph(serie) %>%
  dyRangeSelector()

# A frequência mais apropriada para nossos dados é 7
findfrequency(serie)

# Vamos trabalhar com a série de streams
y <- ts(streams$c, frequency = 7)

# Características empíricas da nossa série
# Possui tendência crescente, sazonalidade semanal e outras ondulações.
par(mfrow=c(2,1))
plot(y)
plot(log(y))

dev.off()

y <- log(y)

# Separando os dados entre treino e teste
treino <- window(y, start = 1, end = 140)
teste <- window(y, start = 140.1)

plot(y)
lines(treino, col = 'red')
lines(teste, col = 'blue')

# Modelo 1 - SARIMA ############################################################

# É estacionária? E a diferenciada?
plot(diff(treino))

# Diferenciar sazonalmente
plot(diff(treino, lag = 7))

# E a estrutura de correlações da nossa série?
# Não é estacionária e possui sazonalidade, como já sabemos.
par(mfrow = c(2, 1))
acf(as.numeric(treino), lag.max = 200)
pacf(as.numeric(treino), lag.max = 200)

acf(as.numeric(diff(treino)), lag.max = 200)
pacf(as.numeric(diff(treino)), lag.max = 200)

acf(as.numeric(diff(treino, lag = 7)), lag.max = 200)
pacf(as.numeric(diff(treino, lag = 7)), lag.max = 200)

# Ajustes

fit <- Arima(treino, order = c(1, 0, 0), seasonal = c(0, 1, 0))
coeftest(fit)
AIC(fit)

acf(as.numeric(fit$residuals), 100)
pacf(as.numeric(fit$residuals), 100)

fit2 <- Arima(treino, order = c(1, 0, 0), seasonal = c(0, 1, 2))
coeftest(fit2)
AIC(fit2)

acf(as.numeric(fit2$residuals), 100)
pacf(as.numeric(fit2$residuals), 100)

fit3 <- Arima(treino, order = c(1, 0, 1), 
              seasonal = c(0, 1, 2))
coeftest(fit3)
AIC(fit3)

acf(fit3$residuals, 100)
pacf(fit3$residuals, 100)

fit4 <- Arima(treino, order = c(12, 0, 12), 
              seasonal = c(0, 1, 2),
              fixed = c(NA, rep(0,10), rep(NA,2), rep(0,10), rep(NA,3)))
coeftest(fit4)
AIC(fit4)

acf(as.numeric(fit4$residuals), 100)
pacf(as.numeric(fit4$residuals), 100)

fit_ <- auto.arima(treino)
coeftest(fit_)
acf(fit_$residuals, 100)
pacf(fit_$residuals, 100)

# Qual será o melhor modelo?
# Em relação ao ajuste

dev.off()
plot(treino)
lines(ts(fit$fitted, start = 1, frequency = 7),
      col = 'red')
lines(ts(fit2$fitted, start = 1, frequency = 7),
      col = 'blue')
lines(ts(fit3$fitted, start = 1, frequency = 7),
      col = 'green')
lines(ts(fit4$fitted, start = 1, frequency = 7),
      col = 'yellow')
lines(ts(fit_$fitted, start = 1, frequency = 7),
      col = 'purple')

# Em relação aos resíduos??
fit %>% checkresiduals()
fit2 %>% checkresiduals()
fit3 %>% checkresiduals()
fit4 %>% checkresiduals()
fit_ %>% checkresiduals()

# Em relação às previsões
forecast_SARIMA <- forecast::forecast(fit, h = length(teste))$mean
forecast_SARIMA2 <- forecast::forecast(fit2, h = length(teste))$mean
forecast_SARIMA3 <- forecast::forecast(fit3, h = length(teste))$mean
forecast_SARIMA4 <- forecast::forecast(fit4, h = length(teste))$mean
forecast_SARIMA_ <- forecast::forecast(fit_, h = length(teste))$mean

# Fizemos uma transformação nos dados!!
# Log(y) -> Exp(y)
plot(exp(teste))
lines(ts(exp(forecast_SARIMA), start=140.1, frequency = 7), col='red')
lines(ts(exp(forecast_SARIMA2), start=140.1, frequency = 7), col='blue')
lines(ts(exp(forecast_SARIMA3), start=140.1, frequency = 7), col='green')
lines(ts(exp(forecast_SARIMA4), start=140.1, frequency = 7), col='yellow')
lines(ts(exp(forecast_SARIMA_), start=140.1, frequency = 7), col='purple')

# MAPE - erro em porcentagem
MAPE <- mean(abs((exp(teste)-exp(forecast_SARIMA))/exp(teste))) * 100
MAPE2 <- mean(abs((exp(teste)-exp(forecast_SARIMA2))/exp(teste))) * 100
MAPE3 <- mean(abs((exp(teste)-exp(forecast_SARIMA3))/exp(teste))) * 100
MAPE4 <- mean(abs((exp(teste)-exp(forecast_SARIMA4))/exp(teste))) * 100
MAPE_ <- mean(abs((exp(teste)-exp(forecast_SARIMA_))/exp(teste))) * 100

# Modelo 2 - Prophet ###########################################################

# Definindo a série input do modelo
n <- length(y)
treino_df <-
  data.frame(ds = streams$Date[1:length(treino)], y = treino)
teste_df <-
  data.frame(ds = streams$Date[(length(treino) + 1):n], y = teste)

# Criando a estrutura do modelo
prop <- prophet(
  treino_df,
  weekly.seasonality = T,
  yearly.seasonality = T,
  daily.seasonality = T
)

# Criando a estrutura dos forecasts
future <-
  make_future_dataframe(prop, periods = length(teste), freq = 'day')
tail(future)

# Previsões do modelo
forecast_PROPHET <- predict(prop, future)
tail(forecast_PROPHET[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(prop, forecast_PROPHET)

MAPE_prophet <- mean(abs((exp(teste)-exp(forecast_PROPHET$yhat[(length(treino) + 1):n]))/exp(teste))) * 100

# Ensemble #####################################################################
# https://robjhyndman.com/hyndsight/forecast-combinations/

forecasts <- cbind(SARIMA = forecast_SARIMA,
                   PROPHET = forecast_PROPHET$yhat[(length(treino) + 1):n])
y2 <- cbind(exp(teste), exp(forecasts))
autoplot(y2)

MLpol0 <- mixture(model = "MLpol", loss.type = "square")
weights <- predict(MLpol0, exp(forecasts), exp(teste), type = 'weights')
head(weights)

# Fazendo o ensemble  
z <-
  ts(
    predict(MLpol0, exp(forecasts), exp(teste), type = 'response'),
    start = 140.1,
    freq = 7
  )
ensemble <- cbind(exp(teste), z)
colnames(ensemble) <- c("Data", "Ensemble")
autoplot(ensemble)

plot(exp(y))
lines(exp(forecasts[, 1]), col = 'red')
lines(exp(forecasts[, 2]), col = 'green')
lines(z, col = 'blue')

MAPE_ensemble <- mean(abs((exp(teste)-as.numeric(z))/exp(teste))) * 100

# Usando o melhor modelo,
# Quantos streams terá essa playlist amanhã?
# E depois?

today() - max(df$Date)

# Pegando as previsões, com IC de 95%
tail(exp(forecast(fit, h = length(teste)+3)$lower[,2]),1)
tail(exp(forecast(fit, h = length(teste)+3)$mean),1)
tail(exp(forecast(fit, h = length(teste)+3)$upper[,2]),1)
