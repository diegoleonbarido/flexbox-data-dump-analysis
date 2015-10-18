##################### 
##################### Wordcloud
##################### 

library(tm)
library(wordcloud)
library(gridExtra)

setwd('/Users/Diego/Desktop/Data/Nicaragua/implementation_nicaragua/all_data')
all_data_survey <- read.csv('all_survey_data_complete.csv')

############## Wrting the text files 

## Seasons
all_data_survey$porque_consume_epocas <- as.character(all_data_survey$porque_consume_epocas)
write(as.character(all_data_survey$porque_consume_epocas),file="consumo_epocas_file/all_consumo_epocas.txt")

## Most important expenditures
all_data_survey$principal_gastos_ME <- as.character(all_data_survey$principal_gastos_ME)
all_data_survey$especifique_principal_gastos <- as.character(all_data_survey$especifique_principal_gastos)
all_data_survey$segundo_gastos_ME <- as.character(all_data_survey$segundo_gastos_ME)
all_data_survey$especifique_segundo_gastos <- as.character(all_data_survey$especifique_segundo_gastos)
all_data_survey$tercero_gastos_ME <- as.character(all_data_survey$tercero_gastos_ME)
all_data_survey$especifique_tercer_gastos <- as.character(all_data_survey$especifique_tercer_gastos)

write(as.character(all_data_survey$principal_gastos_ME),file="costos_file/principal_gastos_ME")
write(as.character(all_data_survey$especifique_principal_gastos),file="costos_file/especifique_principal_gastos")
write(as.character(all_data_survey$segundo_gastos_ME),file="costos_file/segundo_gastos_ME")
write(as.character(all_data_survey$especifique_segundo_gastos),file="costos_file/especifique_segundo_gastos")
write(as.character(all_data_survey$tercero_gastos_ME),file="costos_file/tercero_gastos_ME")
write(as.character(all_data_survey$especifique_tercer_gastos),file="costos_file/especifique_tercer_gastos")

# Preference of Payment 
all_data_survey$porque_facil_pago <- as.character(all_data_survey$porque_facil_pago)
all_data_survey$porque_relafacil_pago <- as.character(all_data_survey$porque_relafacil_pago)
all_data_survey$porque_complicado_pago <- as.character(all_data_survey$porque_complicado_pago)
all_data_survey$porque_muycomplicado_pago <- as.character(all_data_survey$porque_muycomplicado_pago)

write(as.character(all_data_survey$porque_facil_pago),file="preference_payment/porque_facil_pago")
write(as.character(all_data_survey$porque_relafacil_pago),file="preference_payment/porque_relafacil_pago")
write(as.character(all_data_survey$porque_complicado_pago),file="preference_payment/porque_complicado_pago")
write(as.character(all_data_survey$porque_muycomplicado_pago),file="preference_payment/porque_muycomplicado_pago")

# Data usefulness
all_data_survey$para_que_util_nada <- as.character(all_data_survey$para_que_util_nada)
all_data_survey$para_que_util_masmenos <- as.character(all_data_survey$para_que_util_masmenos)
all_data_survey$para_que_util_uil <- as.character(all_data_survey$para_que_util_uil)
all_data_survey$para_que_util_muyutil <- as.character(all_data_survey$para_que_util_muyutil)

write(as.character(all_data_survey$para_que_util_nada),file="data_usefulness/para_que_util_nada")
write(as.character(all_data_survey$para_que_util_masmenos),file="data_usefulness/para_que_util_masmenos")
write(as.character(all_data_survey$para_que_util_uil),file="data_usefulness/para_que_util_uil")
write(as.character(all_data_survey$para_que_util_muyutil),file="data_usefulness/para_que_util_muyutil")


# Preference of Payment 
all_data_survey$para_que_util_nada <- as.character(all_data_survey$para_que_util_nada)
all_data_survey$para_que_util_masmenos <- as.character(all_data_survey$para_que_util_masmenos)
all_data_survey$para_que_util_uil <- as.character(all_data_survey$para_que_util_uil)
all_data_survey$para_que_util_muyutil <- as.character(all_data_survey$para_que_util_muyutil)

write(as.character(all_data_survey$para_que_util_nada),file="data_usefulness/para_que_util_nada")
write(as.character(all_data_survey$para_que_util_masmenos),file="data_usefulness/para_que_util_masmenos")
write(as.character(all_data_survey$para_que_util_uil),file="data_usefulness/para_que_util_uil")
write(as.character(all_data_survey$para_que_util_muyutil),file="data_usefulness/para_que_util_muyutil")


# Climate Change 
all_data_survey$para_que_util_nada <- as.character(all_data_survey$cambio_climatico_causa)
write(as.character(all_data_survey$cambio_climatico_causa),file="climate_change/cambio_climatico_causa")


# What would you like more of in life?
all_data_survey$percepcion_eleccion <- as.character(all_data_survey$percepcion_eleccion)
all_data_survey$percepcion_eleccion_otro <- as.character(all_data_survey$percepcion_eleccion_otro)

write(as.character(all_data_survey$percepcion_eleccion),file="preference_life/percepcion_eleccion")
write(as.character(all_data_survey$percepcion_eleccion_otro),file="preference_life/percepcion_eleccion_otro")


# What concerns you most about hot days and rainy days?
all_data_survey$preocupacion_dias_calientes <- as.character(all_data_survey$preocupacion_dias_calientes)
all_data_survey$preocupacion_lluvias <- as.character(all_data_survey$preocupacion_lluvias)

write(as.character(all_data_survey$preocupacion_dias_calientes),file="preocupacion_cambio/preocupacion_dias_calientes")
write(as.character(all_data_survey$preocupacion_lluvias),file="preocupacion_cambio/preocupacion_lluvias")


# How do renewables benefit you?
all_data_survey$specifiy_impacto_fuentes <- as.character(all_data_survey$specifiy_impacto_fuentes)
all_data_survey$especifique_beneficiosI <- as.character(all_data_survey$especifique_beneficiosI)
all_data_survey$especifique_beneficiosII <- as.character(all_data_survey$especifique_beneficiosII)

write(as.character(all_data_survey$specifiy_impacto_fuentes),file="impacto_fuentes/specifiy_impacto_fuentes")
write(as.character(all_data_survey$especifique_beneficiosI),file="impacto_fuentes/especifique_beneficiosI")
write(as.character(all_data_survey$especifique_beneficiosII),file="impacto_fuentes/especifique_beneficiosII")


# Why do you think contributing to a renewable energy transition is important?
all_data_survey$porque_contr_importante <- as.character(all_data_survey$porque_contr_importante)
write(as.character(all_data_survey$porque_contr_importante),file="contribuir/porque_contr_importante")



############## Wordclouds


# Why do you think you experience seasonal variation in electricity consumption?
words <- Corpus(DirSource("consumo_epocas_file"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# Which costs concern you the most?
words <- Corpus(DirSource("costos_file"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# What are your main considerations when deciding on a form of payment?
words <- Corpus(DirSource("preference_payment"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# What is data useful for?
words <- Corpus(DirSource("data_usefulness"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# What is the cause of climate change?
words <- Corpus(DirSource("climate_change"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# If you could have more of something - what would it be?
words <- Corpus(DirSource("preference_life"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# What worries you about really hot days and really wet days?
words <- Corpus(DirSource("preocupacion_cambio"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# How does renewable energy benefit you?
words <- Corpus(DirSource("impacto_fuentes"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)


# Why do you think it's important to contribute to a renewable energy transition?
words <- Corpus(DirSource("impacto_fuentes"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)

# Why is it importance to contribute to helping transition to a renewable energy based grid?
words <- Corpus(DirSource("contribuir"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1)

#######################
#######################
# Plotting all words
words <- Corpus(DirSource("alltxts"))
words <- tm_map(words, removeWords,'n/a')
wordcloud(words,colors=brewer.pal(8, "Spectral"),max.words=100,random.order=TRUE,min.freq=1,main='Osito')


