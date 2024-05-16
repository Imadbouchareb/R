#EXO 4 
# Installation du package BioStatR si ce n'est pas déjà fait
if (!require(BioStatR)) install.packages("BioStatR", dependencies = TRUE)

# Chargement du package BioStatR
library(BioStatR)

# Chargement des données
data(Mesures)

# Vérifiez les premières lignes du jeu de données pour comprendre sa structure
head(Mesures)

# Extraire les données des glycines blanches
glycines_blanches <- subset(Mesures, espece == "glycine blanche")

# Donner un nom à votre jeu de données extrait
# Par exemple, si vous voulez nommer votre sous-ensemble "donnees_glycines"
donnees_glycines <- glycines_blanches

# Vérifier le résultat de l'extraction
print(head(donnees_glycines))
dim(donnees_glycines)  # Pour s'assurer que vous avez 54 entrées, comme attendu

#2
# Calcul de la moyenne et de l'écart-type de la taille des glycines blanches
moyenne <- mean(donnees_glycines$taille)
ecart_type <- sd(donnees_glycines$taille)

# Taille de l'échantillon
n <- length(donnees_glycines$taille)

# Calcul de l'intervalle de confiance à 95%
z <- 1.96  # Valeur critique pour 95% de confiance
erreur_standard <- ecart_type / sqrt(n)
marge_erreur <- z * erreur_standard

intervalle_inf <- moyenne - marge_erreur
intervalle_sup <- moyenne + marge_erreur

# Affichage de l'intervalle de confiance
cat("L'intervalle de confiance à 95% pour l'espérance de la taille est [",
    intervalle_inf, ", ", intervalle_sup, "]", sep="")


#-----------------------------------------------------------------------------
#EXO2
# Charger le dataset Mesures
data(Mesures)

# Extraire les données pour les glycines blanches
donnees_glycines <- subset(Mesures, espece == "glycine blanche")

# Calcul de la variance de l'échantillon
n <- nrow(donnees_glycines)  # Taille de l'échantillon
s2 <- var(donnees_glycines$taille)  # Variance de l'échantillon

# Valeurs critiques Chi-carré pour un intervalle de confiance de 95%
chi2_lower <- qchisq(0.025, n-1, lower.tail = TRUE)
chi2_upper <- qchisq(0.975, n-1, lower.tail = TRUE)

# Calcul de l'intervalle de confiance pour la variance
IC_lower <- ((n - 1) * s2) / chi2_upper
IC_upper <- ((n - 1) * s2) / chi2_lower

# Affichage de l'intervalle de confiance
cat("L'intervalle de confiance à 95% pour la variance est [",
    IC_lower, ", ", IC_upper, "]", sep="")

#EXO3
#1 Intervalle de confiance à 95% avec l'écart-type de l'échantillon
poids <- c(19.6, 20, 20.2, 20.1, 20, 19.9, 20, 20.3, 20.1, 19.8)
n <- length(poids)
moyenne <- mean(poids)
ecart_type <- sd(poids)
erreur_standard <- ecart_type / sqrt(n)

# Degrés de liberté
df <- n - 1

# Quantile de la distribution t pour 95% de confiance
t_critique <- qt(0.975, df)

# Intervalle de confiance
intervalle_confiance <- c(moyenne - t_critique * erreur_standard, 
                          moyenne + t_critique * erreur_standard)
intervalle_confiance

#2. Intervalle de confiance à 95% avec l'écart-type de la population
sigma <- 0.2

# Quantile de la distribution normale pour 95% de confiance
z_critique <- qnorm(0.975)

# Intervalle de confiance
intervalle_confiance_population <- c(moyenne - z_critique * (sigma / sqrt(n)), 
                                     moyenne + z_critique * (sigma / sqrt(n)))
intervalle_confiance_population


#EXO4
# Générer les données
set.seed(123)  # Pour la reproductibilité
n <- 20
mu <- 12
sigma <- 2
donnees <- rnorm(n, mean = mu, sd = sigma)

# Calculs pour l'intervalle de confiance de la variance
somme_carres_ecarts <- sum((donnees - mu)^2)

# Quantiles de la distribution du Chi-carré pour 99% de confiance
chi2_bas <- qchisq(0.005, df = n)  # Quantile inférieur
chi2_haut <- qchisq(0.995, df = n) # Quantile supérieur

# Intervalle de confiance pour la variance
intervalle_confiance_variance <- c(somme_carres_ecarts / chi2_haut, 
                                   somme_carres_ecarts / chi2_bas)
intervalle_confiance_variance
























