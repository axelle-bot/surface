library("dplyr")
library("tidyr")
library("readxl")
library(stringr)
library(writexl)

# Liste des 96 départements métropolitains (01 à 95 + 2A/2B)
codes_metro <- c(sprintf("%02d", 1:19), "2A", "2B", as.character(21:95))

#naissance2 <- read_xlsx("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/NAIS_DOM_ANNUELLES.xlsx", sheet =  "COM")
#Il semblerait qu'on utilise pas cette base de données 

crechedep2223 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep.csv", sep = ";")
crechedep1722 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep_hist.csv", sep = ";")
crechedep1722 <- crechedep1722 %>%
  bind_rows(crechedep1722) %>%
  filter(Numéro.Département %in% codes_metro)
crechedep2223 <- crechedep2223 %>%
  bind_rows(crechedep2223) %>%
  filter(Numéro.Département %in% codes_metro)

#Taux de couverture par les crèches de 2017 à 2023 

crechedep23 <- crechedep2223 %>%
  filter(Date.référence == "2023")


#SI ON VEUT VIRER DES VARIABLES C'EST ICI QU'IL FAUT LE FAIRE 
crechedep23 <- crechedep23[ , c("Date.référence", "Numéro.Département", "Nom.Département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU", "Taux.de.couv.EAJE...ensemble", "Taux.de.couv.préscolarisation", "Taux.de.couv.assistante.maternelle", "Taux.de.couv.salariée.à.domicile", "Taux.de.couv.accueil.individuel...ensemble", "Taux.de.couv.global")]                 
names(crechedep23) <- c("année", "Code.de.département", "département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU", "Taux.de.couv.EAJE...ensemble", "Taux.de.couv.préscolarisation", "Taux.de.couv.assistante.maternelle", "Taux.de.couv.salariée.à.domicile", "Taux.de.couv.accueil.individuel...ensemble", "Taux.de.couv.global")


crechedep17 <- crechedep1722 %>%
  filter(Date.référence == "2017")

crechedep17 <- crechedep17[ , c("Date.référence", "Numéro.Département", "Nom.Département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU", "Taux.de.couv.EAJE...ensemble", "Taux.de.couv.préscolarisation", "Taux.de.couv.assistante.maternelle", "Taux.de.couv.salariée.à.domicile", "Taux.de.couv.accueil.individuel...ensemble", "Taux.de.couv.global")]                 
names(crechedep17) <- c("année", "Code.de.département", "département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU", "Taux.de.couv.EAJE...ensemble", "Taux.de.couv.préscolarisation", "Taux.de.couv.assistante.maternelle", "Taux.de.couv.salariée.à.domicile", "Taux.de.couv.accueil.individuel...ensemble", "Taux.de.couv.global")



#Création d'une base de données commune pour 2017 et 2023 : 
crechedep1723 <- crechedep17 %>%
  bind_rows(crechedep23)




#Variable de contrôle emploi par département
emploi <- read_xlsx("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/t201.xlsx", sheet = "ENS - T")


emploi <- emploi %>%
  slice(-(1:3))

names(emploi) <- c("département", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007",
                   "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                   "2018", "2019", "2020", "2021", "2022", "2023")

emploi$Code.de.département <- sub("-.*", "", emploi$département)

keep <- grepl("^(0[1-9]|[1-9][0-9]|2A|2B|97[1-6]|975)$", emploi$Code.de.département)
emploi <- emploi[keep, ]

emploi_reduit <- emploi[, c("département",
                            "Code.de.département",
                            "2017",
                            "2023")]
emploi <- emploi %>%
  filter(!département %in% c(
    "971-Guadeloupe",
    "972-Martinique",
    "973-Guyane",
    "974-La Réunion"
  ))

#Tableau naissance 

tauxdenatalité17 <- read_xls("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/N1D.xls")

tauxdenatalité23 <- read_xlsx("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/irsocsd2023_dd9_xlsx/n1d.xlsx")

tauxdenatalité17 <- tauxdenatalité17 %>%
  slice(-(1:3)) %>% 
  select(1:2)

names(tauxdenatalité17) <- c("département", "naissances")
codes_dep <- data.frame(
  Code.de.département = c(
    "01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19",
    "21","22","23","24","25","26","27","28","29","2A","2B","30","31","32","33","34","35","36","37",
    "38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56",
    "57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75",
    "76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94",
    "95","971","972","973","974","976"),
  département = c(
    "Ain","Aisne","Allier","Alpes-de-Haute-Provence","Hautes-Alpes","Alpes-Maritimes","Ardèche","Ardennes","Ariège","Aube",
    "Aude","Aveyron","Bouches-du-Rhône","Calvados","Cantal","Charente","Charente-Maritime","Cher","Corrèze",
    "Côte-d'Or","Côtes-d'Armor","Creuse","Dordogne","Doubs","Drôme","Eure","Eure-et-Loir","Finistère",
    "Corse-du-Sud","Haute-Corse","Gard","Haute-Garonne","Gers","Gironde","Hérault","Ille-et-Vilaine","Indre",
    "Indre-et-Loire","Isère","Jura","Landes","Loir-et-Cher","Loire","Haute-Loire","Loire-Atlantique","Loiret",
    "Lot","Lot-et-Garonne","Lozère","Maine-et-Loire","Manche","Marne","Haute-Marne","Mayenne","Meurthe-et-Moselle",
    "Meuse","Morbihan","Moselle","Nièvre","Nord","Oise","Orne","Pas-de-Calais","Puy-de-Dôme","Pyrénées-Atlantiques",
    "Hautes-Pyrénées","Pyrénées-Orientales","Bas-Rhin","Haut-Rhin","Rhône","Haute-Saône","Saône-et-Loire","Sarthe",
    "Savoie","Haute-Savoie","Paris","Seine-Maritime","Seine-et-Marne","Yvelines","Deux-Sèvres","Somme","Tarn",
    "Tarn-et-Garonne","Var","Vaucluse","Vendée","Vienne","Haute-Vienne","Vosges","Yonne","Territoire de Belfort",
    "Essonne","Hauts-de-Seine","Seine-Saint-Denis","Val-de-Marne","Val-d'Oise",
    "Guadeloupe","Martinique","Guyane","La Réunion","Mayotte"))

tauxdenatalité17 <- merge(tauxdenatalité17,
                        codes_dep,
                        by.x = "département",
                        by.y = "département",
                        all.x = TRUE)

tauxdenatalité23 <- tauxdenatalité23 %>%
  slice(-(1:3)) %>% 
  select(1:2)
names(tauxdenatalité23) <- c("département", "naissances")
tauxdenatalité23 <- merge(tauxdenatalité23,
                          codes_dep,
                          by.x = "département",
                          by.y = "département",
                          all.x = TRUE)

tauxdenatalité17 <- tauxdenatalité17[!is.na(tauxdenatalité17$Code.de.département), ]
tauxdenatalité23 <- tauxdenatalité23[!is.na(tauxdenatalité23$Code.de.département), ]

tauxdenatalité17 <- tauxdenatalité17 %>%
  filter(Code.de.département %in% codes_metro)

tauxdenatalité23 <- tauxdenatalité23 %>%
  filter(Code.de.département %in% codes_metro)

#Tableau population et taux de natalité final par département

population17 <- read_xls("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/ensemble.xls", sheet = "Départements")
population23 <- read_xlsx("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/ensemble.xlsx", sheet = "Départements")

names(population17) <- c("a", "b", "Code.de.département", "département", "d", "e", "f", "g", "population")
names(population23) <- c("a", "b", "Code.de.département", "département", "d", "e", "f", "g", "population")

population17<- population17 %>%
  select(Code.de.département, département, population) %>%
  slice(-(1:7))

population23<- population23 %>%
  select(Code.de.département, département, population) %>%
  slice(-(1:7))

population17 <- population17 %>%
  filter(Code.de.département %in% codes_metro)
population23 <- population23 %>%
  filter(Code.de.département %in% codes_metro)

taux17 <- merge(tauxdenatalité17, population17, by = "Code.de.département")
taux23 <- merge(tauxdenatalité23, population17, by = "Code.de.département")
taux17$naissances <- as.numeric(gsub(" ", "", taux17$naissances))
taux23$naissances <- as.numeric(gsub(" ", "", taux23$naissances))
taux17$population <- as.numeric(gsub(" ", "", taux17$population))
taux23$population <- as.numeric(gsub(" ", "", taux23$population))

taux17$département.y <- NULL
taux23$département.y <- NULL


names(taux17) <- c("Code.de.département", "département", "naissances", "population")
names(taux23) <- c("Code.de.département", "département", "naissances", "population")


taux17$département.y <- NULL
taux23$département.y <- NULL

taux17$naissances <- as.numeric(gsub("[ ,]", "", taux17$naissances))
taux17$population <- as.numeric(gsub("[ ,]", "", taux17$population))
taux17 <- taux17 %>%
  mutate(taux_natalite_2017 = (naissances / population) * 1000)

taux23$naissances <- as.numeric(gsub("[ ,]", "", taux23$naissances))
taux23$population <- as.numeric(gsub("[ ,]", "", taux23$population))
taux23 <- taux23 %>%
  mutate(taux_natalite_2023 = (naissances / population) * 1000)

#Emploi par département


emploi17 <- emploi_reduit %>%
  select(-`2023`)

emploi23 <- emploi_reduit %>%
  select(-`2017`)

names(emploi17) <- c("Département", "Code.de.département", "emplois")
names(emploi23) <- c("Département", "Code.de.département", "emplois")

base17 <- emploi17 %>%
  left_join(population17, by = "Code.de.département")

base17$emplois <- as.numeric(gsub(" ", "", base17$emplois))
base17$population <- as.numeric(gsub(" ", "", base17$population))

taux_france_2017 <- sum(base17$emplois, na.rm = TRUE)/
  sum(base17$population, na.rm = TRUE)


base17 <- base17 %>%
  mutate(indice_emploi = (emplois / population) / taux_france_2017 * 100)

base23 <- emploi23 %>%
  left_join(population23, by = "Code.de.département")

base23$emplois <- as.numeric(gsub("[ ,]", "", base23$emplois))
base23$population <- as.numeric(gsub("[ ,]", "", base23$population))

taux_france_2023 <- sum(base23$emplois, na.rm = TRUE)/
  sum(base23$population, na.rm = TRUE)

base23 <- base23 %>%
  mutate(indice_emploi = (emplois / population) / taux_france_2023 * 100)

#Merge du tableau


tauxdenatalité17 <- tauxdenatalité17 %>%
  mutate(Code.de.département = as.character(Code.de.département))

tauxdenatalité23 <- tauxdenatalité23 %>%
  mutate(Code.de.département = as.character(Code.de.département))

crechedep17 <- crechedep17 %>%
  mutate(Code.de.département = as.character(Code.de.département))

crechedep23<- crechedep23 %>%
  mutate(Code.de.département = as.character(Code.de.département))

base17 <- base17 %>%
  mutate(Code.de.département = as.character(Code.de.département))

base23 <- base23 %>%
  mutate(Code.de.département = as.character(Code.de.département))

taux17 <- taux17 %>%
  mutate(Code.de.département = as.character(Code.de.département))

taux23 <- taux23%>%
  mutate(Code.de.département = as.character(Code.de.département))

final17 <- merge(crechedep17, taux17, by ="Code.de.département", all=TRUE)
final17 <- merge(final17, base17, by="Code.de.département", all=TRUE)
final23 <- merge(crechedep23, taux23, by="Code.de.département", all=TRUE)
final23 <- merge(final23, base23, by="Code.de.département", all=TRUE)

names(final17)
final17 <- final17 %>%
  select(-c("département.x", "département.x", "département.y", "population.x", "année", "département"))

names(final23)
final23 <- final23 %>%
  select(-c("département.y", "département.x", "département.x", "population.x", "année", "département"))








final17 <- final17 %>%
  mutate(across(starts_with("Taux"), ~ as.numeric(gsub(",", ".", .)))) %>%
  mutate(
    Taux.de.couv.collectif = 
      Taux.de.couv.EAJE.PSU +
      Taux.de.couv.hors.EAJE.PSU +
      Taux.de.couv.préscolarisation,
    
    Taux.de.couv.individuel = 
      Taux.de.couv.assistante.maternelle +
      Taux.de.couv.salariée.à.domicile
  ) 



final23 <- final23 %>%
  mutate(across(starts_with("Taux"), ~ as.numeric(gsub(",", ".", .)))) %>%
  mutate(
    Taux.de.couv.collectif = 
      Taux.de.couv.EAJE.PSU +
      Taux.de.couv.hors.EAJE.PSU +
      Taux.de.couv.préscolarisation,
    
    Taux.de.couv.individuel = 
      Taux.de.couv.assistante.maternelle +
      Taux.de.couv.salariée.à.domicile
  ) 


Codedep= c(
  "01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19",
  "21","22","23","24","25","26","27","28","29","2A","2B","30","31","32","33","34","35","36","37",
  "38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56",
  "57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75",
  "76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94",
  "95"
)







getwd()

names(final17)

names(final17) <- c("Code", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU", "Taux.de.couv.EAJE...ensemble", "Taux.de.couv.préscolarisation", "Taux.de.couv.assistante.maternelle", "Taux.de.couv.salariée.à.domicile", "Taux.de.couv.accueil.individuel...ensemble", "Taux.de.couv.global", "Naissances", "Taux.de.natalité", "Département", "Emplois", "Population", "Indice.emploi" , "Taux.de.couv.collectif", "Taux.de.couv.individuel")
names(final23) <- c("Code", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU", "Taux.de.couv.EAJE...ensemble", "Taux.de.couv.préscolarisation", "Taux.de.couv.assistante.maternelle", "Taux.de.couv.salariée.à.domicile", "Taux.de.couv.accueil.individuel...ensemble", "Taux.de.couv.global", "Naissances", "Taux.de.natalité", "Département", "Emplois", "Population", "Indice.emploi" , "Taux.de.couv.collectif", "Taux.de.couv.individuel")

final17 <- final17 %>%
  filter(!is.na(Taux.de.couv.EAJE.PSU))

final23 <- final23 %>%
  filter(!is.na(Taux.de.couv.EAJE.PSU))
  

final17 <- final17 %>%
  distinct(Code, .keep_all = TRUE)


final23 <- final23 %>%
  distinct(Code, .keep_all = TRUE)

final17 <- final17[!is.na(final17$Taux.de.couv.EAJE.PSU), ]
final23 <- final23[!is.na(final23$Taux.de.couv.EAJE.PSU), ]

final17_1 <- final17
final23_1 <- final23

library(writexl)
write_xlsx(final17, "mon_tableau_final17.xlsx")
write_xlsx(final23, "mon_tableau_final23.xlsx")




