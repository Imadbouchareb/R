#EXO 1 

# Chargement des bibliothèques nécessaires
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

# Paramètres de la distribution normale
mu <- 10
sigma <- 2

# Création d'une séquence de valeurs x de 5 à 15
x <- seq(5, 15, length.out = 300)

# Calcul de la densité de probabilité de la normale pour chaque x
y <- dnorm(x, mean = mu, sd = sigma)

# Création du graphique
ggplot(data.frame(x, y), aes(x, y)) +
  geom_line() + # Ajout de la ligne de la courbe de densité
  labs(title = "Densité", x = "Valeur de X", y = "Densité de probabilité") +
  theme_minimal() # Utilisation d'un thème minimaliste pour le graphique


# Calcul de P(X ≤ 12)
p_x_le_12 <- pnorm(12, mean = mu, sd = sigma)

# Calcul de P(X > 12)
p_x_gt_12 <- 1 - p_x_le_12

# Calcul de P(7 < X ≤ 15)
p_7_lt_x_le_15 <- pnorm(15, mean = mu, sd = sigma) - pnorm(7, mean = mu, sd = sigma)

# Affichage des résultats
cat("P(X ≤ 12) = ", p_x_le_12, "\n")
cat("P(X > 12) = ", p_x_gt_12, "\n")
cat("P(7 < X ≤ 15) = ", p_7_lt_x_le_15, "\n")

# Calcul du quantile q0.05
q_0_05 <- qnorm(0.05, mean = mu, sd = sigma)

# Affichage du résultat
cat("Le quantile q0.05, tel que P(X ≤ q) ≥ 0.05, est :", q_0_05, "\n")



# Création d'une séquence de valeurs x de -5 à 5
x <- seq(-5, 5, length.out = 300)

# Calcul de la fonction de répartition pour chaque x
y <- pnorm(x, mean = mu, sd = sigma)

# Création du graphique
p <- ggplot(data.frame(x, y), aes(x, y)) +
  geom_line() + # Ajout de la ligne de la courbe de répartition
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") + # Ligne horizontale à y = 0.05
  geom_vline(xintercept = q_0_05, linetype = "dashed", color = "red") + # Ligne verticale à x = q0.05
  labs(title = "Fonction de répartition", x = "x", y = "y") +
  theme_minimal() # Utilisation d'un thème minimaliste pour le graphique

# Affichage du graphique
print(p)


# Génération de 20 nombres aléatoires suivant la loi normale spécifiée
random_numbers <- rnorm(20, mean = mu, sd = sigma)

# Affichage des nombres générés
print(random_numbers)


# Définition de la fonction g(x)
g <- function(x) {
  (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# Création d'une séquence de valeurs x de -6 à 6
x <- seq(-6, 6, length.out = 300)

# Calcul de g(x) pour chaque x
y <- g(x)

# Création du graphique
p <- ggplot(data.frame(x, y), aes(x, y)) +
  geom_line() + # Ajout de la ligne de la courbe de densité
  labs(title = "Densité de la loi normale centrée et réduite", x = "x", y = "y") +
  theme_minimal() # Utilisation d'un thème minimaliste pour le graphique

# Affichage du graphique
print(p)

# Calcul de l'intégrale de g(x) de -1.96 à 1.96
prob_between <- pnorm(1.96) - pnorm(-1.96)

# Calcul de l'intégrale de g(x) de -∞ à -1.96
prob_below <- pnorm(-1.96)

# Calcul de l'intégrale de g(x) de 1.96 à ∞
prob_above <- 1 - pnorm(1.96)

# Affichage des résultats
cat("P(-1.96 < X < 1.96) =", prob_between, "\n")
cat("P(X < -1.96) =", prob_below, "\n")
cat("P(X > 1.96) =", prob_above, "\n")

# Fonction pour la densité de la distribution normale avec µ = 10 et σ = 2
density_function <- function(x) {
  dnorm(x, mean = 10, sd = 2)
}

# Fonction pour la densité de la loi normale centrée et réduite (µ = 0, σ = 1)
standard_density <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# Plage de valeurs x pour le graphique
x_values <- seq(2, 12, length.out = 300)

# Création d'un dataframe pour les données
data1 <- data.frame(x = x_values, y = sapply(x_values, density_function))
data2 <- data.frame(x = x_values, y = sapply(x_values, standard_density))

# Création du graphique initial
p <- ggplot() +
  geom_line(data = data1, aes(x, y), colour = "red", linetype = "solid") +
  geom_line(data = data2, aes(x, y), colour = "green", linetype = "dashed") +
  labs(title = "Plusieurs graphiques", x = "x", y = "y") +
  xlim(2, 12) + ylim(0, NA) + # Définir les limites de l'axe x et y
  theme_minimal()

# Affichage du graphique
print(p)

#--------------------------------------------------------------------
#EXO 2
# Paramètres de la distribution binomiale
n <- 8
p <- 0.2

# Calcul des probabilités pour chaque valeur de k de 0 à n
probabilities <- dbinom(0:n, size = n, prob = p)

# Création de l'histogramme
barplot(probabilities, names.arg = 0:n, col = "lightblue", 
        main = "Histogramme de la loi binomiale (n=8, p=0.2)", 
        xlab = "Nombre de succès", ylab = "Probabilité")


#2
# Calcul de P(X = 2)
p_x_equals_2 <- dbinom(2, size = n, prob = p)

# Calcul de P(X ≤ 2)
p_x_less_than_or_equal_2 <- pbinom(2, size = n, prob = p)

# Calcul de P(X ≥ 4)
p_x_greater_than_or_equal_4 <- 1 - pbinom(3, size = n, prob = p)  # 1 - P(X ≤ 3)

# Affichage des résultats
cat("P(X = 2) =", p_x_equals_2, "\n")
cat("P(X ≤ 2) =", p_x_less_than_or_equal_2, "\n")
cat("P(X ≥ 4) =", p_x_greater_than_or_equal_4, "\n")


#3
# Calcul du quantile q0.3
q_0_3 <- qbinom(0.3, size = n, prob = p)

# Affichage du résultat
cat("Le quantile q0.3, tel que P(X ≤ q) ≥ 0.3, est :", q_0_3, "\n")


#4
# Calcul des probabilités cumulatives pour chaque k de 0 à n
k_values <- 0:n
probabilities <- pbinom(k_values, size = n, prob = p)

# Création d'un dataframe pour ggplot
data <- data.frame(k = k_values, Prob = probabilities)

# Création du graphique
p <- ggplot(data, aes(x = k, y = Prob)) +
  geom_step() +  # Utiliser geom_step pour le graphique en escalier
  geom_point() +  # Ajouter les points sur chaque k
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "blue") +  # Ligne horizontale à y = 0.3
  geom_vline(xintercept = q_0_3, linetype = "dashed", color = "red") +  # Ligne verticale à x = q0.3
  labs(title = "Fonction de répartition binomiale", x = "x", y = "y") +
  scale_x_continuous(breaks = -2:10) +  # Échelle de l'axe x de -2 à 10
  theme_minimal()

# Affichage du graphique
print(p)

#5  
# Paramètres de la distribution binomiale
n <- 8
p <- 0.2

# Génération de 20 nombres aléatoires suivant la loi binomiale spécifiée
random_numbers <- rbinom(20, size = n, prob = p)

# Affichage des nombres générés
print(random_numbers)


#6
# Calcul des probabilités pour chaque valeur possible de k de 0 à n
probabilities <- dbinom(0:n, size = n, prob = p)

# Trouver la valeur de k pour laquelle la probabilité est maximale
k_0 <- which.max(probabilities) - 1  # which.max renvoie l'index, donc soustrayez 1 pour obtenir k

# Affichage du résultat
cat("La valeur de k0 telle que P(X = k0) est maximale est :", k_0, "\n")

#7
# Calcul de 8!
factorial_8 <- factorial(8)

# Calcul de C8,2
choose_8_2 <- choose(8, 2)

# Affichage des résultats
cat("8! =", factorial_8, "\n")
cat("C8,2 =", choose_8_2, "\n")


