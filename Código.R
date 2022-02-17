#pacotes necessários
library(COVID19)
library(dplyr)
library(ggplot2)
library(zoo)
library(scales)

#coletando os dados de Brasil do pacote COVID19
bz <- covid19(country = "Brazil", level = 1)
bz$date <- as.Date(bz$date, format="%d/%m/%Y")

#adicionando colunas de novos casos e novas mortes
bz <- bz %>% 
  mutate(novos_casos = confirmed - lag(confirmed, default = first(confirmed)), .after = confirmed)
bz <- bz %>% 
  mutate(novas_mortes = deaths - lag(deaths, default = first(deaths)), .after = deaths)


#adicionando colunas de mm7d de novos casos e novas mortes
bz <- bz %>%
  mutate(novos_casos_mm7d = rollmean(novos_casos, k = 7, fill = NA), .after = novos_casos)
bz <- bz %>%
  mutate(novas_mortes_mm7d = rollmean(novas_mortes, k = 7, fill = NA), .after = novas_mortes)

#adicionando colunas de parcela da população vacinada e não vacinada
bz <- bz %>% 
  mutate(pop_pct_vacinada = people_fully_vaccinated/population, .after = people_fully_vaccinated)
bz <- bz %>% 
  mutate(pop_pct_naovacinada = 1 - pop_pct_vacinada, .after = pop_pct_vacinada)

#gráfico novos casos (mm7d) e novas mortes (mm7d) Brasil
  
  #definindo data do início da vacinação
  #row_index <- which(!is.na(bz$people_vaccinated), arr.ind = TRUE)[1]
  #data_vacinacao <- bz$date[row_index]
  
  bz %>% 
    ggplot() +
    geom_line(aes(x = date, y = novos_casos_mm7d), color="red") + 
    #geom_vline(xintercept = data_vacinacao, linetype = "longdash", show.legend = T) +
    labs(title="Brasil - Novos casos de COVID-19", subtitle= "Em média móvel de 7 dias",x="Data", y = "Novos casos (MM7D)", caption = "Fonte: COVID-19 Data Hub") +
    theme(plot.title = element_text(face = "bold")) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_date(labels = "%m/%Y", date_labels = "%m/%Y") 
  
  bz %>% 
    ggplot() +
    geom_line(aes(x = date, y = novas_mortes_mm7d), color="red") +
    #geom_vline(xintercept = data_vacinacao, linetype = "longdash") +
    labs(title="Brasil - Novas mortes de COVID-19", subtitle= "Em média móvel de 7 dias",x="Data", y = "Novas mortes (MM7D)", caption = "Fonte: COVID-19 Data Hub") +
    theme(plot.title = element_text(face = "bold")) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_date(labels = "%m/%Y", date_labels = "%m/%Y")
  
#gráfico de vacinados x não vacinados 
row_index <- which(bz$pop_pct_vacinada == max(bz$pop_pct_vacinada, na.rm = T), arr.ind = TRUE)[1]
  
df <- data.frame(
  group = c("Vacinados", "Não vacinados"),
  value = c(bz$pop_pct_vacinada[row_index],bz$pop_pct_naovacinada[row_index])
)
  
df %>% 
  ggplot(aes(x="", y=value, fill=group)) +
  geom_bar(width=1, stat="identity", color = "white") +
  coord_polar("y", start=0) +
  labs(title="Brasil - Vacinados x Não vacinados", subtitle= "Em porcentagem da população", caption = "Fonte: COVID-19 Data Hub") +
  scale_fill_brewer("Legenda") +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = percent(value/100), fontface = 2), size=5) +
  theme_void()
  

  