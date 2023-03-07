#LABORATORIO BIO-DEM
#Ancarani, Cagnani, Ferraro, Giribone

#Librerie
library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats)
library(gapminder)
library(ggridges)
library(viridis)
library(haven)
library(knitr)
library(corrplot)
library(GGally)
library(devtools)
library(eurostat)
library(rvest)
library(sf)
library(XML)
library(tmap)
library(tmaptools)

#Puliamo environment
rm(list=ls())

#Working directory
setwd("C:/Users/bianc/OneDrive/Desktop/UNIVERSITA'/3° anno/Laboratorio bio-demografico/Lavoro di gruppo")

#Caricamento dati del dataset base
skill <- read.csv("skills.csv") %>% select(LOCATION, Country, Skills, Value)

mismatch <- read.csv("mismatch.csv") %>% select(LOCATION, Mismatch, Value)

gdp <- read.csv("gdp_worldbank.csv") %>% 
  rename(GDP=X2019..YR2019., LOCATION=Country.Code) %>% 
  select(LOCATION, GDP)

#' I dati vengono dall'OECD Skills for Jobs database
#' I dati relativi alle Skills si riferiscono al 2019 con l'eccezione di Svizzera (CHE), Francia, Irlanda, Italia, Polonia, per i quali i dati sono relativi al 2018
#' Per la Germania (DEU) e il Regno Unito (GBR) al 2017, per la Turchia al 2015, per Islanda e Slovenia al 2012
#' I dati relativi al mismatch sono tutti relativi al 2019 tranne che per la Turchia per la quale si riferiscono al 2015
#' Per semplicit? consideriamo i valori del GDP per capita (PPP constant 2017 international $) relativi al 2019, considerando non significative le differenze con gli anni precedenti (dati World Bank)
#' Interpretazione per Skill needs by Country: Positive values indicate skill shortage while negative values point to skill surplus. The larger the absolute value, the larger the imbalance. The value of 1 represents the largest shortage and the value of -1 the largest surplus
#' Interpretazione per Mismatch by Country: Average percentage of workers that have a qualification or field-of-study that does not match their job's requirements.

#Reshape
skill_wide<- skill %>%
  pivot_wider(
    names_from = "Skills",
    values_from = "Value"
  )
View(skill_wide)

mismatch_wide<- mismatch %>%
  pivot_wider(
    names_from = "Mismatch",
    values_from = "Value"
  )
View(mismatch_wide)

#Uniamo
dati_intermedio <- inner_join(mismatch_wide, skill_wide, by="LOCATION")
dati_tot <- left_join(dati_intermedio, gdp, by="LOCATION")
view(dati_tot)
#Sistemiamo
dati_tot <- dati_tot[-c(41, 42),] %>% 
  relocate(Country, .after = LOCATION) %>% 
  relocate(`Field-of-study mismatch`:Underqualification, .before = GDP)

#Aggiungiamo indicazione sul continente per tenere solo le Nazioni Europee
gapminder <- gapminder

continenti <- gapminder %>% filter(year == 2007) %>% select(country, continent) %>% rename(Country = country)

dati_cont <- left_join(dati_tot, continenti, by="Country") %>% relocate (continent, .after = Country)

dati_cont$continent[dati_cont$Country=="Korea"]<-"Asia"
dati_cont$continent[is.na(dati_cont$continent)==T]<-"Europe"

dati <- dati_cont %>% filter(continent == "Europe")
view(dati)

#Caricamento dati serie temporale
temp <- read.csv("serie_temp.csv") %>% select(LOCATION, Country, Year, Value)

#Da long a wide
temp_wide<- temp %>%
  pivot_wider(
    names_from = "Year",
    values_from = "Value"
  )

serie_temp <- left_join(temp_wide, continenti, by="Country") %>% relocate (continent, .after = Country)
serie_temp$continent[is.na(serie_temp$continent)==T]<-"Europe"

#Noi utilizzaremo il dataset "dati" per confronti tra variabili e il dataset 
#"serie_temp" per lavorare sulle serie temporali dei 30 paesi europei considerati

summary(dati)
dati$GDP <- as.numeric(dati$GDP)

#Grafici semplici ####

#Pil per nazioni

gdp_plot <- ggplot(data=dati, aes(x=fct_reorder(Country,GDP), y=GDP)) +
  geom_bar(stat="identity", color="blue", fill="azure") + 
  theme_minimal()+ coord_flip() +
  labs(y="PIl pro capite",
       x="Nazioni",
       title="PIL pro capite per nazioni",
       caption="Fonte: OECD") 
gdp_plot

art_plot <- ggplot(data=dati, aes(x=fct_reorder(Country,dati$`arts and humanities knowledge`), y=dati$`arts and humanities knowledge`)) +
  geom_bar(stat="identity", color="purple", fill="violet") + 
  theme_minimal()+ coord_flip() +
  labs(y="Valori",
       x="Nazioni",
       title="Arts and humanities knowledge",
       caption="Fonte: OECD") + ylim(-0.5, 0.5)
art_plot

#Grafici di correlazione col il PIL
#Lo faccio per law and public safety knowledge e per Underqualification, dato che sono gli unici risultati correlati

PIL_law <- ggplot(data=dati, aes(y=`law and public safety knowledge`, x=GDP)) +
  geom_point(alpha=0.3) +
  theme_minimal() +
  labs(y="Conoscenze di legge e pubblica sicurezza",
       x="PIL pro capite",
       title="Conoscenze di legge e pubblica sicurezza & PIL",
       subtitle="Ogni punto rappresenta un paese europeo",
       caption="Fonte: OECD")+ 
  scale_x_log10(labels=scales::dollar)+
  geom_smooth(method="lm", color="red", se=F)
PIL_law

PIL_under <- ggplot(data=dati, aes(y=Underqualification, x=GDP)) +
  geom_point(alpha=0.3) +
  theme_minimal() +
  labs(y="Sottoqualificazione",
       x="PIL pro capite",
       title="Sottoqualificazione & PIL",
       subtitle="Ogni punto rappresenta un paese europeo",
       caption="Fonte: OECD")+ 
  scale_x_log10(labels=scales::dollar)+
  geom_smooth(method="lm", color="red", se=F)
PIL_under

#digital skill e production and technology
digi_techno <- ggplot(data=dati, aes(y=`digital skills`, x=`production and technology knowledge`)) +
  geom_point(alpha=0.3) +
  theme_minimal() +
  labs(y="Digital skills",
       x="Production and technology knowledge",
       title="Abilità digitali e conoscenze tecniche e produttive",
       subtitle="Ogni punto rappresenta un paese europeo",
       caption="Fonte: OECD")+
  geom_smooth(method="lm", color="red", se=F)
digi_techno

# Serie temporale ####

#Nella serie temporale si considera la variabile "Proportion of workers who are well matched"
#L'interpretazione è la seguente: costoro sono i lavoratori il cui livello e tipologia
#di istruzione è ben associato a quello richiesto dal loro lavoro
#Questo valore viene misurato come il livello modale dell'istruzione per tutti i lavoratori nello stesso campo

#per Paese

temporale<-temp  %>% 
  ggplot(aes(x=Year, y=Value))+
  theme_minimal()+
  geom_line(col="darkred") +
  facet_wrap(~Country, ncol=7)+
  labs( y="Proportion of workers who are well matched", x="Anni",
        title="Andamento annuale (2003-2013) della proporzione di lavoratori correttamente impiegati",
        subtitle="Con lavoratore correttamente impiegato si intened lavoratore impiegato nel campo per cui si è formato", 
        caption="Fonte dati: OECD.")

temporale

#Tutti insieme (è un casino)
temp0<-  temp %>% ggplot( aes(x=Year))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="Proporzione di lavoratori correttamente impiegati",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")
temp0

temp1<-temp0 +
  geom_point(aes(y=Value, col=Country))+
  geom_line(aes(y=Value, col=Country)) + xlim(2002, 2014)
temp1

# Analisi territoriale ####
geodata <-get_eurostat_geospatial(nuts_level=0) 
view(geodata)

geodata_link<- geodata %>% 
  mutate(LOCATION=recode(NUTS_ID,
                         "BG"= "BGR" ,
                         "CH"= "CHE" ,
                         "CY"= "CYP" ,
                         "CZ"= "CZE" ,
                         "BE"= "BEL" ,
                         "AT"= "AUT" ,
                         "DE"="DEU",
                         "DK"="DNK",
                         "EE"="EST",
                         "EL"="GRC",
                         "ES"="ESP",
                         "FI"="FIN",
                         "FR"="FRA",
                         "HU"="HUN",
                         "IE"="IRL",
                         "IS"="ISL",
                         "IT"="ITA",
                         "LT"="LTU",
                         "LU"="LUX",
                         "LV"="LVA",
                         "NL"="NLD",
                         "NO"="NOR",
                         "PL"="POL",
                         "PT"="PRT",
                         "RO"="ROU",
                         "SE"="SWE",
                         "SI"="SVN",
                         "SK"="SVK",
                         "TR"="TUR",
                         "UK"="GBR"))
view(geodata_link)

map0 <- tm_shape(geodata_link) +
  tm_fill("lightgrey") +
  tm_grid() #Creates a tmap-element that draws coordinate grid lines
map0

bb(geodata)

bb_cont<-bb(geodata,xlim=c(-24,45), ylim=c(35,71) ) # redefine the bounding box

map1 <- tm_shape(geodata_link, bbox=bb_cont) + # bb = bounding box
  tm_polygons() +
  tm_fill("lightgrey")
map1

mapdata <- geodata_link %>%  left_join(dati, by="LOCATION") %>% 
  mutate(categorie=cut(`Qualification mismatch`,breaks=c(15,20,25,30,35,40,45),labels =c("15-20%","20-25%","25-30%","30-35%","35-40%","40-45%")))


map_mismatch <- tm_shape(mapdata, bbox=bb_cont) +
  tm_polygons("categorie", title = "Qualification mismatch",
              palette = "Blues", border.col = "black")  +
  tm_layout(title = "Qualification mismatch",
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("right","center"), 
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map_mismatch

