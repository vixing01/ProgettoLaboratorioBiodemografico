#LABORATORIO BIO-DEM
#Ancarani, Cagnani, Ferraro, Giribone

#Librerie
pkg <- c("shiny", "tidyverse", "ggpubr","ggplot2","ggthemes","forcats","gapminder",
  "haven",  "tidymodels", "broom", "devtools","ggridges","viridis",
  "ggiraphExtra", "dotwhisker", "texreg", "margins","haven","knitr","corrplot",
  "ggeffects","GGally","devtools","eurostat","rvest","sf","XML","tmap","tmaptools",
  "dplyr","RColorBrewer")
sapply(pkg, require, character.only = TRUE)
rm(list=ls())

setwd("C:/Users/Virginia/OneDrive/Documents/MATERIALE UNIVERSITA'/LABORATORIO/tesina")

#PULIZIA DEI DATI ####
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
    values_from = "Value")


mismatch_wide<- mismatch %>%
  pivot_wider(
    names_from = "Mismatch",
    values_from = "Value")


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
dati$GDP<-as.numeric(dati$GDP)
dati<-dati[,-c(5,13,16,17)]


#Caricamento dati serie temporale
temp <- read.csv("serie_temp.csv") %>% select(LOCATION, Country, Year, Value)

#Da long a wide
temp_wide<- temp %>%
  pivot_wider(
    names_from = "Year",
    values_from = "Value")


#Noi utilizzaremo il dataset "dati" per confronti tra variabili e il dataset 
#"temp" per lavorare sulle serie temporali dei 30 paesi europei considerati

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

grafico_temporale<-temp  %>%
  ggplot(aes(x=Year, y=Value))+
  geom_line(col="darkred") +
  theme_minimal()+
  facet_wrap(~Country, ncol=6)+
  labs( y="Proportion of workers who are well matched", x="Anni",
        title="Andamento annuale (2003-2013) della proporzione di lavoratori correttamente impiegati",
        subtitle="Con lavoratore correttamente impiegato si intened lavoratore impiegato nel campo per cui si è formato", 
        caption="Fonte dati: OECD.")+
  scale_x_continuous(breaks=NULL) #altrimenti si pu? mettere come breaks 2003 e 2013 ma rimangono sempre un pochino sovrapposte le scritte

grafico_temporale #ha davvero senso tenere Malta?

#In base a cosa raggruppare questi paesi?
dati_temporale<-temp_wide %>% select("LOCATION", "2013")%>%
  inner_join(dati,by="LOCATION")
dati_temporale<-dati_temporale %>% select(!c(Country, LOCATION,continent))
round(cor(dati_temporale),2)[,1]
#La correlazione con il GDP non ? abbastanza forte da giustificare un raggruppamento dei paesi sulla base di esso
#Per questo motivo verranno raggruppati sulla base della loro posizione geografica

#Assegniamo ogni paese alla sua macro-regione
temp <- temp %>% mutate(
  GEO = case_when(
    Country == "Denmark" | Country == "Finland" |
      Country == "Iceland" | Country == "Ireland" | Country == "Norway" |
      Country == "Sweden" | Country == "United Kingdom" | Country == "Estonia" | 
      Country == "Latvia" | Country == "Lithuania" ~ 1,
    Country == "Greece" | Country == "Italy" | Country == "Portugal" |
      Country == "Spain" | Country == "Türkiye" | Country == "Cyprus" |
      Country == "Slovenia" | Country == "Malta" ~ 2,
    Country == "Czech Republic" | Country == "Hungary" | Country == "Poland" | 
      Country == "Slovak Republic" | Country == "Bulgaria" | Country == "Romania" ~ 3,
    Country == "Austria" | Country == "Belgium" | Country == "France" |
      Country == "Germany" | Country == "Luxembourg" | Country == "Netherlands" |
      Country == "Switzerland" ~ 4
  ), 
  GEO = factor(GEO, 1:4, c("North", "South", "East", "West"))
)

#Calcoliamo media e deviazione standard per ogni macro-regione
temp_geo <- temp %>% select(Value, Year, GEO) %>% group_by(GEO, Year) %>%
  summarize_all(.funs=list(val_medio=~mean(Value, na.rm=T),
                           STD=~sd(Value, na.rm=T),
                           N = ~sum(!is.na(Value))))


temp_plot<-  temp_geo %>% ggplot(aes(x=Year))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="Proporzione di lavoratori correttamente impiegati",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_point(aes(y=val_medio, col=GEO))+
  geom_line(aes(y=val_medio, col=GEO)) + xlim(2002, 2014)
temp_plot


#grafico alternativo

temporaneo <- temp_geo %>% mutate(Country = GEO)

temp_tot<-  temp %>% 
  ggplot(aes(x=Year, y=Value, group=Country))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="Proporzione di lavoratori correttamente impiegati",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_line(col="grey", alpha=0.5) + xlim(2002, 2014)+
  geom_line(data=temporaneo, 
            aes(x=Year, y=val_medio, col=Country),
            linewidth=1.8)
temp_tot

#suddiviso per aree geografiche

temp_North<-  temp %>% filter(GEO=="North") %>%
  ggplot(aes(x=Year, y=Value, group=Country))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="North",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_line(col="grey", alpha=0.5) + xlim(2002, 2014)+
  geom_line(data=temporaneo %>% filter(Country == "North"), 
            aes(x=Year, y=val_medio),
            linewidth=1.8)
temp_North

temp_South<-  temp %>% filter(GEO=="South") %>%
  ggplot(aes(x=Year, y=Value, group=Country))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="South",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_line(col="grey", alpha=0.5) + xlim(2002, 2014)+
  geom_line(data=temporaneo %>% filter(Country == "South"), 
            aes(x=Year, y=val_medio),
            linewidth=1.8)
temp_South

temp_East<-  temp %>% filter(GEO=="East") %>%
  ggplot(aes(x=Year, y=Value, group=Country))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="East",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_line(col="grey", alpha=0.5) + xlim(2002, 2014)+
  geom_line(data=temporaneo %>% filter(Country == "East"), 
            aes(x=Year, y=val_medio),
            linewidth=1.8)
temp_East

temp_West<-  temp %>% filter(GEO=="West") %>%
  ggplot(aes(x=Year, y=Value, group=Country))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="West",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_line(col="grey", alpha=0.5) + xlim(2002, 2014)+
  geom_line(data=temporaneo %>% filter(Country == "West"), 
            aes(x=Year, y=val_medio),
            linewidth=1.8)
temp_West

ggarrange(temp_North, temp_South, temp_East, temp_West)

#Provo PIL per Nazioni raggruppate in macroaree
dati_geo <- dati %>% mutate(
  GEO = case_when(
    Country == "Denmark" | Country == "Finland" |
      Country == "Iceland" | Country == "Ireland" | Country == "Norway" |
      Country == "Sweden" | Country == "United Kingdom" | Country == "Estonia" | 
      Country == "Latvia" | Country == "Lithuania" ~ 1,
    Country == "Greece" | Country == "Italy" | Country == "Portugal" |
      Country == "Spain" | Country == "Türkiye" | Country == "Cyprus" |
      Country == "Slovenia" | Country == "Malta" ~ 2,
    Country == "Czech Republic" | Country == "Hungary" | Country == "Poland" | 
      Country == "Slovak Republic" | Country == "Bulgaria" | Country == "Romania" ~ 3,
    Country == "Austria" | Country == "Belgium" | Country == "France" |
      Country == "Germany" | Country == "Luxembourg" | Country == "Netherlands" |
      Country == "Switzerland" ~ 4
  ), 
  GEO = factor(GEO, 1:4, c("North", "South", "East", "West"))
) %>% group_by(GEO) %>% summarize_all(.funs=list(val_medio=~mean(GDP, na.rm=T)))



gdp_plot_geo <- ggplot(data=dati_geo, aes(x=fct_reorder(GEO,GDP_val_medio), y=GDP_val_medio)) +
  geom_bar(stat="identity", color="blue", fill="azure") + 
  theme_minimal()+ coord_flip() +
  labs(y="PIl pro capite",
       x="Macroaree geografiche",
       title="PIL pro capite per macroareegeografiche",
       caption="Fonte: OECD") 
gdp_plot_geo

#L'europa dell'est è la più povera, potrebbe voler dire qualcosa?

#PROVA: DIVIDERE I PAESI IN BASE ALL'AREA GEOGRAFICA in 2

temp <- read.csv("serie_temp.csv") %>% select(LOCATION, Country, Year, Value)
temp <- temp %>% mutate(
  GEO = case_when(
    Country == "Denmark" | Country == "Finland" |
      Country == "Iceland" | Country == "Ireland" | Country == "Norway" |
      Country == "Sweden" | Country == "United Kingdom" | Country == "Estonia" | 
      Country == "Latvia" | Country == "Lithuania" ~ 1,
    Country == "Greece" | Country == "Italy" | Country == "Portugal" |
      Country == "Spain" | Country == "Türkiye" | Country == "Cyprus" |
      Country == "Slovenia" | Country == "Malta" ~ 1,
    Country == "Czech Republic" | Country == "Hungary" | Country == "Poland" | 
      Country == "Slovak Republic" | Country == "Bulgaria" | Country == "Romania" ~ 2,
    Country == "Austria" | Country == "Belgium" | Country == "France" |
      Country == "Germany" | Country == "Luxembourg" | Country == "Netherlands" |
      Country == "Switzerland" ~ 1), 
  GEO = factor(GEO, 1:2, c("Ovest", "Est"))
)

#Calcoliamo media e deviazione standard per ogni macro-regione
temp_geo <- temp %>% select(Value, Year, GEO) %>% group_by(GEO, Year) %>%
  summarize_all(.funs=list(val_medio=~mean(Value, na.rm=T),
                           STD=~sd(Value, na.rm=T),
                           N = ~sum(!is.na(Value))))

temp_plot<-  temp_geo %>% ggplot(aes(x=Year))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore (%)",
    title="Proporzione di lavoratori correttamente impiegati",
    subtitle="Anni 2003-2013",
    caption= "Source: OECD")+
  geom_point(aes(y=val_medio, col=GEO),size=1.4)+
  geom_line(aes(y=val_medio, col=GEO),lwd=1.2) + 
  xlim(2002, 2014)+
  geom_ribbon(aes(ymin=val_medio-1.96*STD/sqrt(N),
                  ymax=val_medio+1.96*STD/sqrt(N),fill=GEO),
              linetype=2, outline.type = "both")+
  scale_color_manual(values = c("#4EB09BFF","#F78425FF"))+
  scale_fill_manual(values = c("#4EB09B44","#F7842544"))
  
temp_plot

#PROVA: aggiungere il 2019 per vedere se il trend continua cos?

temp_wide_2019<-temp_wide %>% left_join(dati, by="LOCATION")
temp_wide_2019<-temp_wide_2019 %>%
  select("LOCATION","Country.x","Qualification mismatch", starts_with("20"))%>%
  mutate("2019"=(100-`Qualification mismatch`)) %>% 
  rename("Country"="Country.x") %>% 
  relocate("2019", .after = "2013")

temp_long_2019<-temp_wide_2019 %>% 
  pivot_longer(cols=starts_with("20"), names_to = "Year", values_to ="Value")

temp_long_2019 <- temp_long_2019 %>% mutate(
  GEO = case_when(
    Country == "Denmark" | Country == "Finland" |
      Country == "Iceland" | Country == "Ireland" | Country == "Norway" |
      Country == "Sweden" | Country == "United Kingdom" | Country == "Estonia" | 
      Country == "Latvia" | Country == "Lithuania" ~ 1,
    Country == "Greece" | Country == "Italy" | Country == "Portugal" |
      Country == "Spain" | Country == "Türkiye" | Country == "Cyprus" |
      Country == "Slovenia" | Country == "Malta" ~ 1,
    Country == "Czech Republic" | Country == "Hungary" | Country == "Poland" | 
      Country == "Slovak Republic" | Country == "Bulgaria" | Country == "Romania" ~ 2,
    Country == "Austria" | Country == "Belgium" | Country == "France" |
      Country == "Germany" | Country == "Luxembourg" | Country == "Netherlands" |
      Country == "Switzerland" ~ 1), 
  GEO = factor(GEO, 1:2, c("Ovest", "Est"))
)

temp_geo_2019 <- temp_long_2019 %>% select(Value, Year, GEO) %>% group_by(GEO, Year) %>%
  summarize_all(.funs=list(val_medio=~mean(Value, na.rm=T),
                           STD=~sd(Value, na.rm=T),
                           N = ~sum(!is.na(Value))))
temp_geo_2019$Year<-as.numeric(temp_geo_2019$Year)

temp_plot<-  temp_geo_2019 %>% ggplot(aes(x=Year))+
  theme_bw()+
  labs(
    x="Anno",
    y= "Valore",
    title="Proporzione di lavoratori correttamente impiegati",
    subtitle="Anni 2003-2019",
    caption= "Source: OECD")+
  geom_point(aes(y=val_medio, col=GEO),size=1.4)+
  geom_line(aes(y=val_medio, col=GEO),lwd=1.2) +
  geom_ribbon(aes(ymin=val_medio-1.96*STD/sqrt(N),
                  ymax=val_medio+1.96*STD/sqrt(N),fill=GEO),
              linetype=2, outline.type = "both")+
  scale_color_manual(values = c("#4EB09BFF","#F78425FF"))+
  scale_fill_manual(values = c("#4EB09B44","#F7842544"))

temp_plot

#Non penso abbia senso tenerlo per il fatto che in realt? i dati che usiamo non sono tutti del 2019
#Per? ci d? un po' un'idea del fatto che le differenze tra i due gruppi negli anni sono diminuite e tra il 2013 e il 2019 non hanno ricominciato ad aumentare


#TABELLE CON FREQUENZE RELATIVE ####

#Funzione per suddividere in classi skills e knowledge
taglio<-function(variabile){
  cut(variabile, breaks = c(-1,-0.6, -0.2, 0.2, 0.6, 1), labels=c("forte surplus","surplus moderato","equilibrio","carenza moderata","forte carenza"))}

library(dplyr)
#Dataset con variabili in classi
dati_c<- dati %>% 
  select(4:16) %>% 
  mutate_all(.funs = taglio) %>% 
  rename_with(~ paste0(.,"_c"))
dati_c<-cbind(dati,dati_c)
dati_c<- dati_c %>%
  mutate(mismatch_c=cut(`Qualification mismatch`,breaks=c(15,20,25,30,35,40,45),labels =c("15-20%","20-25%","25-30%","30-35%","35-40%","40-45%")),
         over_c=cut(Overqualification,breaks=c(5,10,15,20,25,30,35),labels =c("5-10%","10-15%","15-20%","20-25%","25-30%","30-35%")),
         under_c=cut(Underqualification,breaks=c(5,10,15,20,25,30,35),labels =c("5-10%","10-15%","15-20%","20-25%","25-30%","30-35%")))

library(knitr)

tab_mismatch <-round(prop.table(table(dati_c$mismatch_c))*100,1)
tab_over <-round(prop.table(table(dati_c$over_c))*100,1)
tab_under <-round(prop.table(table(dati_c$under_c))*100,1)

tab_over_under<-cbind(tab_over, tab_under)
tab_over_under<-kable(tab_over_under,"simple",col.names = c("Overqualification","Underqualification"))
tab_over_under
cumsum(tab_over)
cumsum(tab_under)
summary(dati_c$`Qualification mismatch`)
summary(dati_c$Overqualification)
summary(dati_c$Underqualification)
#La maggior parte dei paesi ha una overqualification del 10-15% e una underqualification del 10-20%
#L'overqualification risulta essere pi? spostata verso il basso: guardando le frequenze cumulate tra lo 0 e il 15% troviamo il 56.7% dei paesi per quanto riguarda l'over mentre solo il 36.7% per l'under
#Entrambe le variabili, analizzate come continue, hanno il minimo intorno a 8 e il massimo intorno a 30; la media per over ? 15.87 mentre per under ? 17.32
tab_mismatch<-kable(tab_mismatch, "simple", col.names = c("","Qualification mismatch"))
tab_mismatch
#Analizzando il mismatch, essendo una somma di over e under, si vede che la categoria pi? frequente ? pi? elevata e corrisponde al 35-40%

tab_arts <-round(prop.table(table(dati_c$`arts and humanities knowledge_c`))*100,1)
tab_law <-round(prop.table(table(dati_c$`law and public safety knowledge_c`))*100,1)
tab_medicine <-round(prop.table(table(dati_c$`medicine knowledge_c`))*100,1)
tab_technology <-round(prop.table(table(dati_c$`production and technology knowledge_c`))*100,1)
tab_scientific <-round(prop.table(table(dati_c$`scientific knowledge_c`))*100,1)

tab_knowledge<-cbind(tab_arts,tab_law, tab_medicine, tab_technology, tab_scientific)
tab_knowledge<-kable(tab_knowledge, "simple", col.names = c("Art","Law","Medicine","Technology","Scientific"))
tab_knowledge
#La categoria pi? frequente ? sempre l'equilibrio; in nessun paese si rileva un forte surplus mentre nel 10% (Belgio, Danimarca e Spagna) si rileva una forte carenza di conoscenze mediche
#Non sono presenti paesi con carenze riguardanti le conoscenze giuridiche e tecnologiche
#Non sono presenti paesi con surplus di conoscenze scientifiche
dati_c %>%  filter(`medicine knowledge_c`=="forte carenza") %>% select(Country)


tab_communication <-round(prop.table(table(dati_c$`communication skills_c`))*100,1)
tab_cognitive<-round(prop.table(table(dati_c$`cognitive skills_c`))*100,1)
tab_digital <-round(prop.table(table(dati_c$`digital skills_c`))*100,1)
tab_physical <-round(prop.table(table(dati_c$`physical skills_c`))*100,1)
tab_social <-round(prop.table(table(dati_c$`social skills_c`))*100,1)

tab_skills<-cbind(tab_communication, tab_cognitive,tab_digital,tab_physical,tab_social)
tab_skills<-kable(tab_skills,"simple",col.names = c("Communication","Cognitive","Digital","Physical","Social"))
tab_skills
#La categoria pi? frequente ? sempre l'equilibrio e nessuno dei paesi considerati presenta una forte carenza.
#Un forte surplus si ha nel campo delle abilit? fisiche nel 6.7% dei paesi (Belgio ed Estonia)
#Non sono presenti paesi con carenze nel campo delle abilit? comunicative
dati_c %>%  filter(`physical skills_c`=="forte surplus") %>% select(Country)

summary(dati$GDP)
#Abbiamo tutti paesi ad alto reddito avendo considerato solo paesi europei, per cui non ha senso suddividere il GDP in classi
dati_c %>% select(GDP) %>% summarize_all(.funs= list(media  = ~mean(x=., na.rm=T), 
                                                     dev.std = ~sd(x=., na.rm=T)))
#La media ? 47230 mentre la deviazione standard 18948


#ANALISI TERRITORIALE ####
#Fonte dati spaziali: Eurostat
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
  tm_grid()

bb(geodata)

bb_cont<-bb(geodata,xlim=c(-24,51), ylim=c(35,71) ) # redefine the bounding box

#Mappa dell'Europa
map1 <- tm_shape(geodata_link, bbox=bb_cont) + # bb = bounding box
  tm_polygons() +
  tm_fill("lightgrey")
map1

mapdata <- geodata_link %>%  left_join(dati_c, by="LOCATION")

#Funzione per creare le mappe
cartina<-function(variabile, colore, titolo_leg, valore,titolo){tm_shape(mapdata, bbox=bb_cont) +
    tm_polygons(variabile,title=titolo_leg, palette = colore, border.col = "black")  +
    tm_layout(title = titolo,
              title.size = 1.2,
              title.position = c("left","top"),
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("right","center"), 
              legend.bg.color = "#ffffff00",
              legend.bg.alpha = 1,
              legend.show = valore)}

legenda_skills<-tm_shape(mapdata, bbox=bb_cont) +
  tm_polygons("cognitive skills_c",title="Legenda", palette = "-RdYlGn", border.col = "black")  +
  tm_layout(legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("center","center"), 
            legend.bg.color = "#ffffff00",
            legend.bg.alpha = 1,
            legend.only = TRUE)

map_mismatch<-cartina("mismatch_c", "Blues", "Qualification mismatch", TRUE," ")
map_mismatch

mapdata %>% filter(mismatch_c=="40-45%") %>% select(Country)
#I paesi che presentano il maggior livello di mismatch sono Grecia, Spagna, Irlanda, Portogallo, Turchia e Regno Unito
mapdata %>% filter(mismatch_c=="15-20%") %>% select(Country)
#Il paese con il minore livello di mismatch ? la Repubblica Ceca

map_over<-cartina("over_c", "Greens", "Overqualification",TRUE,"")
map_under<-cartina("under_c", "Reds", "Underqualification",TRUE,"")
tmap_arrange(map_over,map_under, ncol=2)

mapdata %>% filter(over_c=="5-10%") %>% select(Country)
#I paesi con minor overqualification sono Bulgaria, Repubblica Ceca, Finlandia, Ungheria e Polonia
mapdata %>% filter(over_c=="25-30%") %>% select(Country)
#I paesi con maggior overqualification (25-30%) sono Grecia, Portogallo e Turchia
mapdata %>% filter(under_c=="5-10%") %>% select(Country)
#I paesi con minor underqualification sono Repubblica Ceca e Slovacchia
mapdata %>% filter(under_c=="30-35%") %>% select(Country)
#Il paese con maggior underqualification ? l'Irlanda

colnames(mapdata)[33:45]
map_arts<-cartina("arts and humanities knowledge_c", "-RdYlGn", "Art knowledge",FALSE, "Art knowledge")
map_law<-cartina("law and public safety knowledge_c", "-RdYlGn", "Law knowledge",FALSE, "Law knowledge")
map_medicine<-cartina("medicine knowledge_c", "-RdYlGn", "Medicine knowledge",FALSE, "Medicine knowledge")
map_technology<-cartina("production and technology knowledge_c", "-RdYlGn", "Technology knowledge",FALSE, "Technology knowledge")
map_scientific<-cartina("scientific knowledge_c", "-RdYlGn", "Scientific knowledge",FALSE, "Scientific knowledge")

map_communication<-cartina("communication skills_c", "-RdYlGn", "Communication skills",FALSE,"Communication skills")
map_cognitive<-cartina("cognitive skills_c", "-RdYlGn", "Cognitive skills",FALSE, "Cognitive skills")
map_digital<-cartina("digital skills_c", "-RdYlGn", "Digital skills",FALSE,"Digital skills")
map_physical<-cartina("physical skills_c", "-RdYlGn", "Physical skills",FALSE,"Physical skills")
map_social<-cartina("social skills_c", "-RdYlGn", "Social skills",FALSE, "Social skills")

tmap_arrange(map_communication, map_cognitive, map_digital, map_physical, map_social, legenda_skills, ncol=3)
tmap_arrange(map_arts,map_law, map_medicine,map_technology,map_scientific, legenda_skills)


#CORRELAZIONE ####
dati_senza<-dati %>% select(!c(LOCATION, Country, continent))
#In questo dataset non sono presenti valori mancanti e abbiamo tutte variabili quantitative

matrice_corr<-dati_senza %>% cor()
round(matrice_corr,2)[,14] #Siamo interessate alla correlazione delle nostre variabili con il GDP
#La correlazione pi? elevata si ha con Underqualification (0.38)

#Correlazione delle variabili knowledge
round(matrice_corr,2)[c(1,5,6,8,9),14]
#Si va da una correlazione di 0.32 con le conoscenze giuridiche a una pressoch? indipendenza con le conoscenze tecnologiche

#Correlazione con le variabili skills
round(matrice_corr,2)[c(2,3,4,7,10),14]
#Digital, cognitive e physical skills sono pressoch? indipendenti mentre la correlazione pi? rilevante (0.21) si ha per le skill comunicative

#Correlazione con over, under e mismatch
round(matrice_corr,2)[11:13,14]
#La correlazione ? positiva per l'underqualification mentre ? debolmente negativa per l'overqualification
#Possibile interpretazione: sprecare persone che hanno studiato mettendole a fare lavori al di sotto della loro preparazione non ? positivo per il PIL
#Mettere persone non preparate a fare lavori per cui non sono qualificate ha una parziale correlazione positiva con il PIL, ma forse per il semplice fatto che nei paesi con elevata underqualification l'istruzione superiore ? meno diffusa
#Per valutare questa interpretazione, per?, bisognerebbe avere dei dati: CONTROLLA SE POTREBBE ESSERE UN CONFONDENTE

corrplot(matrice_corr, method="color",type = "upper")


#Correlazione con l'istruzione?####
browseURL("https://ec.europa.eu/eurostat/databrowser/view/EDAT_LFSE_03__custom_5173078/default/table?lang=en")
dati_prova<-dati[order(-dati$Underqualification),]
dati_prova<- dati_prova %>% select(LOCATION, Country,Underqualification,Overqualification)

istruzione<-read.csv("istruzione.csv") #upper-secondary, post-secondary non tertiary and tertiary education
istruzione<-istruzione %>% select(geo, OBS_VALUE) %>% 
  mutate(LOCATION=recode(geo,
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
                         "UK"="GBR")) %>% 
  right_join(dati_prova, by="LOCATION") %>%
  rename("%istruzione"=OBS_VALUE)

cor_ist<-istruzione %>% select(`%istruzione`,Overqualification,Underqualification) %>% cor()
round(cor_ist,2)
#La percentuale di istruzione risulta pressoch? indipendente dall'underqualification mentre negativamente correlata con over
#Nei paesi in cui pi? persone vengono messe a fare lavori pi? "semplici" la percentuale di persone con istruzione almeno secondaria ? pi? bassa
#Strano


tertiary<-read.csv("tertiary.csv") #educazione terziaria
tertiary<-tertiary %>% select(geo, OBS_VALUE) %>% 
  mutate(LOCATION=recode(geo,
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
                         "UK"="GBR")) %>% 
  right_join(dati_prova, by="LOCATION") %>% 
  rename("%tertiary"=OBS_VALUE)
cor_ter<-tertiary %>% select(`%tertiary`,Overqualification,Underqualification) %>% cor()

round(cor_ter,2)
#I paesi con pi? persone con educazione terziaria hanno anche pi? underqualification
#I paesi con pi? persone con educazione terziaria hanno un'overqualification pi? bassa
#Molto strano

#Se le cose stanno cos? forse non ha senso tirare in ballo la diffusione dell'istruzione

#MODELLO MULTIVARIATO: prova####
colnames(dati_senza)

all<-lm(GDP~., data = dati_senza)
summary(all)

mod0<-lm(GDP~1, data = dati_senza)
summary(mod0)

stats::step(all, scope=list(lower=mod0, upper=all), direction="backward")

summary(lm(formula = GDP ~ `law and public safety knowledge` + `physical skills` + 
             Overqualification + `Qualification mismatch`, data = dati_senza))

mult_mod_skills<-lm(GDP~ `cognitive skills`+`communication skills`+`digital skills`+`physical skills`+`social skills`, data=dati_senza)
summary(mult_mod_skills)
stats::step(mult_mod_skills, scope=list(lower=mod0, upper=mult_mod_skills), direction="backward")
#L'associazione tra il GDP e le skills non ? abbastanza forte: nel modello multivariato nessuno dei coefficienti associato ad esse risulta essere significativo
#Utilizzando la selezione backward il modello migliore risulta essere quello nullo

mult_mod_knowledge<-lm(GDP~`arts and humanities knowledge`+`law and public safety knowledge`+`medicine knowledge`+`production and technology knowledge`+`scientific knowledge`, data=dati_senza)
summary(mult_mod_knowledge)
stats::step(mult_mod_knowledge, scope=list(lower=mod0, upper=mult_mod_knowledge), direction="backward")
#In questo caso utilizzando la selezione backward viene mantenuta solo la variabile "law and public safety knowledge"

mult_mod_mismatch<-lm(GDP~Overqualification+Underqualification, data = dati_senza)
summary(mult_mod_mismatch)
#In questo caso l'unico coefficiente significativo risulta essere quello relativo all'Underqualification, che come avevamo gi? visto ? la variabile con la maggiore correlazione con il GDP
#Interpretazione: boh

#MODELLO UNIVARIATO CON UNDERQUALIFICATION

#Verifica se GDP ha una distribuzione normale
x <- dati$GDP
h<-hist(x, breaks=9, col="sienna1")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
#La distribuzione sembra essere approssimabile a una normale quindi possiamo usare la regressione lineare

mod_under<-lm(GDP~Underqualification, data=dati_senza)
tidy(mod_under)
summary(mod_under)$r.squared #il modello comunque non si adatta per niente bene ai dati: R^2=0.14

dati_senza%>%
  ggplot(aes(x=Underqualification, y=GDP)) +
  geom_point() +
  theme_bw()+
  labs(
    title = "GDP e Underqualification",
    x= "Underqualification",
    y= "GDP per capita",
    caption = "Dati 2019")+
  geom_smooth(method=lm, se=FALSE, col="orangered2", fullrange = TRUE) +
  scale_x_continuous(limits=c(0, 50))+
  scale_y_continuous(limits=c(0 ,150000))+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept= 23873) +
  geom_vline(xintercept = 0)


#'Commento per me:
#'Overqualification: metti persone che hanno studiato a fare lavori manuali
#'Underqualification: metti persone che non hanno studiato a fare lavori complessi
#'Domanda di ricerca: Il PIL di un paese ? influenzato da una giusta assegnazione dei lavoratori al loro ambito e in base alla loro qualifica?

#STRUTTURA ANALISI ####

#Bozza

#'Descrittive: cartine per tutte le skills e knowledge
#'Distribuzione del PIl per far vedere che sono tutti paesi ricchi
#'Questi grafici possono essere accompagnati dalle tabelle con le frequenze relative per dare dei valori
#'Nel caso in cui volessimo fare una suddivisione di questi paesi? Divisione geografica
#'Dalla divisione geografica si passa all'andamento temporale facendo vedere che i valori non hanno grandi variazioni nel tempo (in media circa 5 punti percentuali)
#'Possiamo anche dire che andando avanti con il tempo Nord-Ovest e Sud-Est sono sempre pi? simili
#'Analisi delle correlazioni
#'Grafici con le correlazioni pi? forti o per far vedere l'indipendenza tra coppie di variabili
#'Eventuale costruzione del modello di regressione
#'Conclusioni






