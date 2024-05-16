#EXO1 
# Données des mesures ajustées
mesures <- c(850, 740, 900, 1070, 930, 850, 950, 980, 980, 880, 1000, 980, 930, 650, 760,
             810, 1000, 1000, 960, 960, 960, 940, 960, 940, 880, 800, 850, 880, 900, 840,
             830, 790, 810, 880, 880, 830, 800, 790, 760, 800, 880, 880, 880, 860, 720,
             720, 620, 860, 970, 950, 880, 910, 850, 870, 840, 840, 850, 840, 840, 840,
             890, 810, 810, 820, 800, 770, 760, 740, 750, 760, 910, 920, 890, 860, 880,
             720, 840, 850, 850, 780, 890, 840, 780, 810, 760, 810, 790, 810, 820, 850,
             870, 870, 810, 740, 810, 940, 950, 800, 810, 870)

# Valeur attendue de la vitesse de la lumière ajustée pour la base de 299000 km/s
valeur_attendue <- 734.5

# Calcul de la moyenne et de l'écart-type de l'échantillon
moyenne_echantillon <- mean(mesures)
ecart_type_echantillon <- sd(mesures)
taille_echantillon <- length(mesures)

# Calcul de l'intervalle de confiance à 95%
erreur_standard <- ecart_type_echantillon / sqrt(taille_echantillon)
intervalle_confiance <- moyenne_echantillon + c(-1, 1) * qt(0.975, taille_echantillon - 1) * erreur_standard

# Test t pour un échantillon
t_test <- t.test(mesures, mu = valeur_attendue, conf.level = 0.95)

# Affichage des résultats
intervalle_confiance
t_test

#EXO2
# Données de l'échantillon
donnees <- c(67.8, 30.5, 52.5, 40.4, 29.6, 58.3, 62.6, 53.6, 64.6, 54.4, 53.8, 49.8, 57.4, 63.1, 53.4, 59.4, 48.6, 40.7, 51.9)

# Paramètres connus
mu_0 <- 50
sigma <- 10
n <- length(donnees)
x_bar <- mean(donnees)

# Calcul de la statistique de test z
z <- (x_bar - mu_0) / (sigma / sqrt(n))

# Calcul de la valeur p pour un test à une queue
p_value <- 1 - pnorm(z)

# Affichage de la statistique de test et de la valeur p
list(z = z, p_value = p_value)

#EXO3
# Mesures avant et après traitement
avant <- c(15,18,17,20,21,18,17,15,19,16,19,17,19,15,14,16,21,20,21,18,17,17,17,15,17,18,16,10,17,18,14,15,15,17,17,20,17)
apres <- c(12,16,17,18,17,15,18,14,16,18,20,16,15,17,18,16,15,14,11,13,13,15,14,15,19,14,16,14,14,15,19,19,16,19,15,17,16)

# Calcul des différences
differences <- avant - apres

# Test t pour échantillons appariés
t_test <- t.test(differences, alternative = "two.sided", conf.level = 0.95)

# Affichage des résultats du test t
t_test


#EXO4 
# Données
jus_orange <- c(
  8.2, 9.4, 9.6, 9.7, 10.0, 14.5, 15.2, 16.1, 17.6, 21.5, 14.0, 13.8,
  12.8, 15.0, 9.5, 10.9, 12.4, 14.7, 10.7, 11.1, 13.8, 13.1, 8.6, 13.9, 15.2, 13.6, 13.4,
  12.3, 15.2, 11.2, 19.6, 7.8, 14.1, 12.5, 14.1, 17.6, 13.5, 12.4, 12.6, 14.6, 15.5, 11.6,
  11.8, 12.9, 8.1, 11.8, 18.7, 12.6, 16.0, 15.8, 17.2, 16.4, 11.2, 10.2, 13.6, 13.2, 15.9,
  9.8, 8.8, 12.0
)
acide_ascorbique <- c(
  4.2, 5.2, 5.8, 6.4, 7.0, 7.3, 10.1, 11.2, 11.3, 11.5, 7.1, 9.8,
  5.3, 4.8, 11.9, 10.1, 12.5, 14.6, 4.9, 9.7, 7.0, 3.8, 5.0, 9.3, 8.7, 8.7, 8.7, 9.5, 2.5,
  6.6, 13.6, 6.6, 9.4, 12.1, 13.1, 4.1, 12.1, 8.8, 7.0, 7.5
)

# Test de Levene pour évaluer l'égalité des variances
library(car)
leveneTest(jus_orange, acide_ascorbique)

# Test t pour échantillons indépendants
t_test <- t.test(jus_orange, acide_ascorbique, alternative = "greater", var.equal = FALSE)

# Affichage des résultats du test t
t_test







