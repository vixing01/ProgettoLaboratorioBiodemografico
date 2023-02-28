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
#' Per semplicità consideriamo i valori del GDP per capita (PPP constant 2017 international $) relativi al 2019, considerando non significative le differenze con gli anni precedenti (dati World Bank)
#' Interpretazione per Skill needs by Country: Positive values indicate skill shortage while negative values point to skill surplus. The larger the absolute value, the larger the imbalance. The value of 1 represents the largest shortage and the value of -1 the largest surplus
#' Interpretazione per Mismatch by Country: Average percentage of workers that have a qualification or field-of-study that does not match their job's requirements.

#Reshape
skill_wide<- skill %>%
  pivot_wider(
    names_from = "Skills",
    values_from = "Value"
  )


mismatch_wide<- mismatch %>%
  pivot_wider(
    names_from = "Mismatch",
    values_from = "Value"
  )


#Uniamo
dati_intermedio <- inner_join(mismatch_wide, skill_wide, by="LOCATION")
dati_tot <- left_join(dati_intermedio, gdp, by="LOCATION")

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


# Analisi territoriale ####
geodata <-get_eurostat_geospatial(nuts_level=0) 


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

taglio<-function(variabile){
  cut(variabile, breaks = c(-1,-0.6, -0.2, 0.2, 0.6, 1), labels=c("forte surplus","surplus moderato","equilibrio","carenza moderata","forte carenza"))}

mapdata <- geodata_link %>%  left_join(dati, by="LOCATION") %>% 
  mutate(mismatch_c=cut(`Qualification mismatch`,breaks=c(15,20,25,30,35,40,45),labels =c("15-20%","20-25%","25-30%","30-35%","35-40%","40-45%")),
         over_c=cut(Overqualification,breaks=c(5,10,15,20,25,30,35),labels =c("5-10%","10-15%","15-20%","20-25%","25-30%","35-40%")),
         under_c=cut(Underqualification,breaks=c(5,10,15,20,25,30,35),labels =c("5-10%","10-15%","15-20%","20-25%","25-30%","35-40%")),
         cognitive_c=taglio(`cognitive skills`), communication_c=taglio(`communication skills`),digital_c=taglio(`digital skills`), physical_c=taglio(`physical skills`),ciao=taglio(dati[,4]))
view(mapdata)
summary(dati$`Qualification mismatch`)
summary(dati$Overqualification)
summary(dati$Underqualification)

cartina<-function(variabile, colore, titolo){tm_shape(mapdata, bbox=bb_cont) +
    tm_polygons(variabile,title=titolo, palette = colore, border.col = "black")  +
    tm_layout(legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("right","center"), 
              legend.bg.color = "#ffffff00",
              legend.bg.alpha = 1,
              legend.only = )}

map_mismatch<-cartina("mismatch_c", "Blues", "Qualification mismatch")
map_mismatch
map_over<-cartina("over_c", "Greens", "Overqualification")
map_under<-cartina("under_c", "Reds", "Underqualification")
tmap_arrange(map_over,map_under)

library(RColorBrewer)
map_cognitive<-cartina("cognitive_c", "-RdYlGn", "Cognitive skills")
map_communication<-cartina("communication_c", "-RdYlGn", "Communication skills")
map_digital<-cartina("digital_c", "-RdYlGn", "Digital skills")
map_physical<-cartina("physical_c", "-RdYlGn", "Physical skills")
tmap_arrange(map_cognitive, map_communication, nrow=2)
?tmap_arrange


#Tabelle: guarda lezione 4
#Correlazione
dati$GDP<-as.numeric(dati$GDP)
matrice_corr<-dati %>% select(!(LOCATION:continent)) %>%   cor(use="complete.obs")
round(matrice_corr,2)

corrplot(matrice_corr, method="color")
corrplot(corMAT, type="upper")
corrplot(matrice_corr, type="upper", order="hclust") # for hierarchical clustering order


?as


