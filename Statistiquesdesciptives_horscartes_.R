library(psych)

##Statistiques descriptives 

final17$`Taux.de.couv.global` <- as.numeric(gsub(",", ".", final17$`Taux.de.couv.global`))
final23$`Taux.de.couv.global` <- as.numeric(gsub(",", ".", final23$`Taux.de.couv.global`))

final17$taux_natalite_2017 <- as.numeric(final17$taux_natalite_2017)
final23$taux_natalite_2023 <- as.numeric(final23$taux_natalite_2023)

final17$indice_emploi <- as.numeric(final17$indice_emploi)
final23$indice_emploi <- as.numeric(final23$indice_emploi)

final17 <- na.omit(final17)
final23 <- na.omit(final23)

describe(final17[,c("taux_natalite_2017",
                    "Taux.de.couv.global",
                    "indice_emploi")])

## Statistiques descriptives

library(psych)
library(readxl)
library(ggplot2)
library(dplyr)

# 1. Chargement
naissance <- read_xlsx("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/demo-naiss-nbre-taux.xlsx")

# 2. Nettoyage des lignes (on garde la logique de votre script)
# On définit la ligne 2 comme source des noms temporaires
colnames(naissance) <- as.character(naissance[2, ])
naissance <- naissance[-c(1, 2), ]
naissance <- naissance[1:(nrow(naissance) - 4), ]
rownames(naissance) <- NULL

# Suppression de la ligne spécifique (ex: France sans Mayotte)
naissance <- naissance[-13, ]
naissance[12, 1] <- "2014"

# 3. LE POINT CRUCIAL : Renommer par index pour éviter les erreurs de texte
# On force des noms simples pour manipuler les données facilement
colnames(naissance)[1] <- "Annee"
colnames(naissance)[2] <- "Nb_Naissances"
colnames(naissance)[3] <- "Taux_Natalite"

# 4. Conversion propre (suppression d'éventuels caractères invisibles)
naissance$Annee <- as.numeric(naissance$Annee)

# Pour les chiffres, on retire les espaces ou virgules traîtresses avant conversion
naissance$Taux_Natalite <- as.numeric(gsub(",", ".", naissance$Taux_Natalite))
naissance$Nb_Naissances <- as.numeric(gsub("[[:space:]]", "", naissance$Nb_Naissances))

# 5. Vérification rapide
str(naissance)

# 6. Graphique
ggplot(naissance, aes(x = Annee, y = Taux_Natalite)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  theme_minimal() +
  labs(
    title = "Évolution du taux de natalité en France",
    x = "Année",
    y = "Taux de natalité (pour 1 000 habitants)"
  )





#Quelques données élémentaires sur les données utilisées -> si on veut en enlever dedans il suffira de les enlever dans la bse de données tableau final
Tableaustatsdebase <- describe(final17, skew = FALSE)
Tableaustatsdebase <- Tableaustatsdebase %>%
  select("mean", "sd", "median", "min", "max") %>%
  file.remove("Département*", "emplois*", "population.y*")

names(Tableaustatsdebase)

crechedep2223 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep.csv", sep = ";")
crechedep1722 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep_hist.csv", sep = ";")


pop_enfant <- read_xlsx("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/age-insee-2020.xlsx", sheet =  "DEP")

pop_enfant <- pop_enfant[, c("INSEE", "NOM", "F0-2", "H0-2")]

pop_enfant <- pop_enfant %>%
  mutate(`0-2` = `F0-2` + `H0-2`) %>%
  select(INSEE, NOM, `0-2`)

names(popenfant)
# A TERMINER






install.packages("shiny")


library(shiny)
library(dplyr)



#crechedep1722 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep_hist.csv", sep = ";")

##ATTENTION IL FAUDRA ENLEVER LES OUTRE MERS

# Nettoyage des données


#crechedep2223 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep.csv", sep = ";")
#crechedep1722 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep_hist.csv", sep = ";")

#crechedep17 <- filter(crechedep1722, "année" = 2017)


departementsmetrop <- c(
  "SAONE ET LOIRE", "SEINE ET MARNE", "MANCHE", "FINISTERE", "MARNE",
  "HAUTE LOIRE", "HAUTE SAONE", "TARN", "DROME", "HAUTES ALPES",
  "CORREZE", "ARDENNES", "AIN", "HAUT RHIN", "MAYENNE",
  "HAUTE CORSE", "INDRE", "CANTAL", "CHARENTE", "COTES D ARMOR",
  "CALVADOS", "NORD", "OISE", "TARN ET GARONNE", "COTE D OR",
  "VAR", "AUDE", "GARD", "VAL D OISE", "ESSONNE",
  "LOIR ET CHER", "LOZERE", "MOSELLE", "AISNE", "GIRONDE",
  "ALLIER", "SOMME", "HAUTE GARONNE", "ISERE", "SARTHE",
  "LOIRE", "AVEYRON", "PYRENEES ATLANTIQUES", "INDRE ET LOIRE",
  "HAUTE MARNE", "ARDECHE", "DORDOGNE", "MORBIHAN", "VAL DE MARNE",
  "MAINE ET LOIRE", "DEUX SEVRES", "TERRITOIRE DE BELFORT", "CHER",
  "PAS DE CALAIS", "VENDEE", "HAUTE VIENNE", "EURE",
  "ALPES MARITIMES", "CHARENTE MARITIME", "LOIRE ATLANTIQUE", "JURA",
  "MEURTHE ET MOSELLE", "ARIEGE", "ILLE ET VILAINE", "YONNE",
  "BAS RHIN", "HAUTS DE SEINE", "PUY DE DOME", "VOSGES",
  "ALPES DE HAUTE PROVENCE", "HERAULT", "LOT ET GARONNE",
  "CORSE DU SUD", "DOUBS", "EURE ET LOIR", "VIENNE",
  "GERS", "NIEVRE", "SEINE SAINT DENIS", "YVELINES",
  "LANDES", "CREUSE", "SAVOIE", "MEUSE", "ORNE",
  "RHONE", "LOT", "PARIS", "SEINE MARITIME", "AUBE",
  "HAUTE SAVOIE", "PYRENEES ORIENTALES", "LOIRET",
  "VAUCLUSE", "BOUCHES DU RHONE", "HAUTES PYRENEES"
)
crechedep1723_1 <- crechedep1723 %>%
  filter(département %in% departementsmetrop)
  

  
crechedep1723_1 <- crechedep1723_1 %>%
  select(
    année,
    département,
    Taux.de.couv.EAJE.PSU,
    Taux.de.couv.hors.EAJE.PSU,
    Taux.de.couv.préscolarisation,
    Taux.de.couv.assistante.maternelle,
    Taux.de.couv.salariée.à.domicile
  ) %>%
  mutate(across(starts_with("Taux"), ~ as.numeric(gsub(",", ".", .)))) %>%
  mutate(
    Taux.de.couv.collectif = 
      Taux.de.couv.EAJE.PSU +
      Taux.de.couv.hors.EAJE.PSU +
      Taux.de.couv.préscolarisation,
    
    Taux.de.couv.individuel = 
      Taux.de.couv.assistante.maternelle +
      Taux.de.couv.salariée.à.domicile
  ) %>%
  group_by(département, année) %>%
  summarise(
    Taux.de.couv.collectif = mean(Taux.de.couv.collectif, na.rm = TRUE),
    Taux.de.couv.individuel = mean(Taux.de.couv.individuel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = année,
    values_from = c(Taux.de.couv.collectif, Taux.de.couv.individuel)
  ) %>%
  mutate(
    diff_collectif = Taux.de.couv.collectif_2023 - Taux.de.couv.collectif_2017,
    diff_individuel = Taux.de.couv.individuel_2023 - Taux.de.couv.individuel_2017
  ) %>%
  select(
    département,
    diff_collectif,
    diff_individuel
  )

crechedep2223_1 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep.csv", sep = ";")
crechedep1722_1 <- read.csv("C:/Users/axell/OneDrive - Université Paris Sciences et Lettres/Bureau/txcouv_pe_dep_hist.csv", sep = ";")

crechedep23_1 <- crechedep2223_1 %>%
  filter(Date.référence == "2023")

crechedep17_1 <- crechedep1722_1 %>%
  filter(Date.référence == "2017")

crechedep17_1 <- crechedep17_1[ , c("Date.référence", "Numéro.Département", "Nom.Département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU","Taux.de.couv.préscolarisation","Taux.de.couv.assistante.maternelle","Taux.de.couv.salariée.à.domicile")]                      


crechedep23_1 <- crechedep23_1[ , c("Date.référence", "Numéro.Département", "Nom.Département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU","Taux.de.couv.préscolarisation","Taux.de.couv.assistante.maternelle","Taux.de.couv.salariée.à.domicile")]                      


crechedep1723_1 <- crechedep17_1 %>%
  bind_rows(crechedep23_1)

names(crechedep1723_1)

names(crechedep1723_1) <- c("annee", "Codedep", "Nom.Département", "Taux.de.couv.EAJE.PSU", "Taux.de.couv.hors.EAJE.PSU","Taux.de.couv.préscolarisation","Taux.de.couv.assistante.maternelle","Taux.de.couv.salariée.à.domicile")


library(dplyr)
library(tidyr)
library(ggplot2)

# Étape 1
df_test <- crechedep1723_1 %>%
  mutate(
    collectif = `Taux.de.couv.EAJE.PSU` + `Taux.de.couv.préscolarisation` + `Taux.de.couv.hors.EAJE.PSU`,
    individuel = `Taux.de.couv.assistante.maternelle` + `Taux.de.couv.salariée.à.domicile`
  )

names(df_test)   # ← vérifie

# Étape 2
df_test2 <- df_test %>%
  select(Nom.Département, annee, collectif, individuel)

names(df_test2)

# Étape 3
df_test3 <- df_test2 %>%
  pivot_longer(
    cols = c("collectif", "individuel"),
    names_to = "type_accueil",
    values_to = "taux"
  )

names(df_test3) 
head(df_test3)

library(dplyr)
library(tidyr)
library(ggplot2)

df_evol_dep <- df_test3 %>%
  filter(annee %in% c(2017, 2023)) %>%
  pivot_wider(
    names_from = annee,
    values_from = taux
  ) %>%
  mutate(evolution = `2023` - `2017`)

ggplot(df_evol_dep, aes(x = reorder(Nom.Département, evolution), y = evolution, fill = type_accueil)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Évolution des modes d'accueil par département (2017 → 2023)",
    x = "Département",
    y = "Évolution du taux"
  ) +
  scale_fill_manual(
    values = c("collectif" = "#1f77b4", "individuel" = "#ff7f0e"),
    labels = c("collectif" = "Accueil collectif", "individuel" = "Accueil individuel")
  ) +
  theme_minimal()


library(shiny)

library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Analyse du Taux de Couverture par Département"),
  
  fluidRow(
    column(
      width = 3,
      selectInput(
        "annee",
        "Année",
        choices = c(2017, 2023),
        selected = 2023,
        width = "100%"
      )
    )
  ),
  
  fluidRow(
    # Important : on définit une grande hauteur (1000px) pour que les 100 lignes respirent
    plotOutput("barplot", height = "1000px")
  )
)

server <- function(input, output) {
  
  data_filtre <- reactive({
    # 1. On garde uniquement les colonnes que tu as demandées
    res <- crechedep1723 %>%
      filter(année == input$annee) %>%
      select(
        département,
        Taux.de.couv.EAJE.PSU,
        Taux.de.couv.hors.EAJE.PSU,
        Taux.de.couv.préscolarisation,
        Taux.de.couv.assistante.maternelle,
        Taux.de.couv.salariée.à.domicile
      )
    
    # 2. On calcule le total pour pouvoir trier
    res$total <- rowSums(res[, -1], na.rm = TRUE)
    
    # 3. On trie du plus petit au plus grand pour que le plus haut soit en haut du graph
    res %>% arrange(total)
  })
  
  output$barplot <- renderPlot({
    df <- data_filtre()
    if(nrow(df) == 0) return(NULL)
    
    # On transforme en matrice les colonnes de taux uniquement (on exclut Nom et Total)
    mat <- as.matrix(df[, 2:(ncol(df)-1)])
    
    # Réglage des marges : plus d'espace à gauche pour les noms de départements
    par(mar = c(5, 15, 4, 2))
    
    # Barplot horizontal
    bp <- barplot(
      t(mat),
      beside = FALSE,        # Empilé
      horiz = TRUE,          # HORIZONTAL
      col = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00"),
      names.arg = df$département,
      las = 1,               # Texte des départements à l'horizontale
      cex.names = 0.7,       # Taille police départements
      xlim = c(0, 150),      # On pousse à 150 pour laisser de la place aux labels
      main = paste("Répartition du taux de couverture en", input$annee),
      xlab = "Taux de couverture cumulé (%)",
      border = "white",
      space = 0.5            # Espace entre les barres
    )
    
    # Ajout des chiffres (%) au bout de chaque barre
    text(
      x = df$total, 
      y = bp, 
      labels = paste0(round(df$total, 1), "%"), 
      pos = 4, 
      cex = 0.6, 
      font = 2
    )
    
    # Légende positionnée proprement
    legend(
      "topright",
      legend = c("EAJE PSU", "Hors PSU", "Préscolarisation", "Ass. Maternelle", "Domicile"),
      fill = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00"),
      cex = 0.8,
      bty = "n",
      bg = "white"
    )
  })
}

shinyApp(ui, server)


summary(final17)



