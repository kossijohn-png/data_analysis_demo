# =============================================================================
# SCRIPT R – RÉGRESSION LINÉAIRE MULTIPLE
# Variable à prédire  : TERME (terme de la grossesse en semaines d'aménorrhée)
# Variable explicative: type_grossesse (Unique / Gémellaire)
# Covariables         : Age, instruction, admission, MAP, RPM,
#                       DLP, Saignement, gestite, parite, CPN
# Auteur              : [DEGBEVI John]
# Date                : 2026
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 0. INSTALLATION ET CHARGEMENT DES PACKAGES
# ─────────────────────────────────────────────────────────────────────────────

# Installer les packages si nécessaire (décommenter si première utilisation)
# install.packages(c("tidyverse","car","lmtest","nortest","broom",
#                    "ggplot2","readxl","flextable","officer","corrplot"))

library(tidyverse)   # Manipulation et visualisation des données
library(car)         # VIF – détection de la multicolinéarité
library(lmtest)      # Tests sur les résidus (Breusch-Pagan, Durbin-Watson)
library(nortest)     # Tests de normalité (Anderson-Darling)
library(broom)       # Mise en forme des résultats de régression
library(ggplot2)     # Graphiques avancés
library(corrplot)    # Matrice de corrélation visuelle
library(readr)    # Matrice de corrélation visuelle
# library(readxl)    # Décommenter si import depuis Excel


# ─────────────────────────────────────────────────────────────────────────────
# 1. IMPORTATION DES DONNÉES
# ─────────────────────────────────────────────────────────────────────────────

# --- Option A : Fichier CSV (séparateur tabulation) ---
data <- read.csv("Base Fleurie.csv", sep = ";", header = TRUE)
data

# Aperçu initial
cat("=== STRUCTURE DES DONNÉES ===\n")
str(data)
cat("\n=== DIMENSIONS ===\n")
cat("Lignes :", nrow(data), "| Colonnes :", ncol(data), "\n")


# ─────────────────────────────────────────────────────────────────────────────
# 2. PRÉPARATION ET RECODAGE DES VARIABLES
# ─────────────────────────────────────────────────────────────────────────────

# Suppression de l'identifiant (non analytique)
data <- data %>% select(-num_id)

# --- Variables catégorielles → facteurs ---
data$type_grossesse <- factor(data$type_grossesse,
                              levels = c("Unique", "Gemellaire"))
# "Unique" = modalité de référence
data$instruction <- as.factor(data$instruction)
data$admission   <- as.factor(data$admission)

# --- Variables binaires → facteurs ---
data$MAP        <- factor(data$MAP,        levels = c(0,1),
                          labels = c("Non","Oui"))
data$RPM        <- factor(data$RPM,        levels = c(0,1),
                          labels = c("Non","Oui"))
data$DLP        <- factor(data$DLP,        levels = c(0,1),
                          labels = c("Non","Oui"))
data$Saignement <- factor(data$Saignement, levels = c(0,1),
                          labels = c("Non","Oui"))

# --- Variables numériques continues ---
data$Age     <- as.numeric(data$Age)
data$gestite <- as.numeric(data$gestite)
data$parite  <- as.numeric(data$parite)
data$CPN     <- as.numeric(data$CPN)
data$TERME   <- as.numeric(data$TERME)

cat("\n=== STRUCTURE APRÈS RECODAGE ===\n")
str(data)
cat("\n=== NIVEAUX DES FACTEURS ===\n")
cat("type_grossesse :", levels(data$type_grossesse), "\n")
cat("CLAS_AGE :", levels(data$CLAS_AGE), "\n")
cat("instruction :", levels(data$instruction), "\n")
cat("admission :", levels(data$admission), "\n")

data
# ─────────────────────────────────────────────────────────────────────────────
# 3. STATISTIQUES DESCRIPTIVES
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")
summary(data)

# Résumé du TERME par type de grossesse
cat("\n=== TERME SELON TYPE DE GROSSESSE ===\n")
data %>%
  group_by(type_grossesse) %>%
  summarise(
    N        = n(),
    Moyenne  = round(mean(TERME, na.rm = TRUE), 2),
    Mediane  = median(TERME, na.rm = TRUE),
    ET       = round(sd(TERME, na.rm = TRUE), 2),
    Min      = min(TERME, na.rm = TRUE),
    Max      = max(TERME, na.rm = TRUE)
  ) %>% print()

# Valeurs manquantes
cat("\n=== VALEURS MANQUANTES PAR VARIABLE ===\n")
colSums(is.na(data))


# ─────────────────────────────────────────────────────────────────────────────
# 4. VISUALISATIONS EXPLORATOIRES
# ─────────────────────────────────────────────────────────────────────────────

# 4.1 Distribution de TERME
ggplot(data, aes(x = TERME)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(count)), color = "darkred", linewidth = 1) +
  labs(title = "Distribution du terme de la grossesse",
       x = "Terme (semaines d'aménorrhée)", y = "Fréquence") +
  theme_minimal()

# 4.2 TERME selon type_grossesse (boxplot)
ggplot(data, aes(x = type_grossesse, y = TERME, fill = type_grossesse)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(title = "Terme selon le type de grossesse",
       x = "Type de grossesse", y = "Terme (SA)") +
  theme_minimal() +
  theme(legend.position = "none")

# 4.3 Matrice de corrélation (variables numériques)
data_num <- data %>% select(Age, gestite, parite, CPN, TERME)
cor_matrix <- cor(data_num, use = "complete.obs")
cat("\n=== MATRICE DE CORRÉLATION ===\n")
print(round(cor_matrix, 3))

corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         title = "Corrélations entre variables numériques",
         mar = c(0,0,2,0))


# ─────────────────────────────────────────────────────────────────────────────
# 5. VÉRIFICATION DE LA NORMALITÉ DE TERME (avant régression)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== TEST DE NORMALITÉ DE TERME ===\n")
shapiro_test <- shapiro.test(data$TERME)
print(shapiro_test)
if (shapiro_test$p.value > 0.05) {
  cat("→ Normalité de TERME : ACCEPTÉE (p =", round(shapiro_test$p.value, 4), ")\n")
} else {
  cat("→ Normalité de TERME : REJETÉE (p =", round(shapiro_test$p.value, 4),
      ") – Interpréter les résultats avec prudence\n")
}

# QQ-plot de TERME
qqnorm(data$TERME, main = "QQ-plot – Distribution de TERME")
qqline(data$TERME, col = "red", lwd = 2)


# ─────────────────────────────────────────────────────────────────────────────
# 6. MODÉLISATION – RÉGRESSION LINÉAIRE MULTIPLE
# ─────────────────────────────────────────────────────────────────────────────

# 6.1 Modèle complet (toutes les variables)
cat("\n=== MODÈLE COMPLET ===\n")
modele_complet <- lm(TERME ~ type_grossesse + Age + instruction +
                       admission + MAP + RPM + DLP + Saignement +
                       gestite + parite + CPN,
                     data = data)
summary(modele_complet)

# 6.2 Sélection des variables – méthode stepwise (critère AIC)
cat("\n=== SÉLECTION STEPWISE (direction = both) ===\n")
modele_nul <- lm(TERME ~ 1, data = data)

modele_stepwise <- step(modele_complet,
                        scope = list(lower = modele_nul,
                                     upper = modele_complet),
                        direction = "both",
                        trace = TRUE)

cat("\n=== MODÈLE APRÈS SÉLECTION STEPWISE ===\n")
summary(modele_stepwise)

# 6.3 Sélection backward (alternative)
cat("\n=== SÉLECTION BACKWARD ===\n")
modele_backward <- step(modele_complet, direction = "backward", trace = FALSE)
summary(modele_backward)

# Choix du modèle final (stepwise par défaut)
modele_final <- modele_stepwise

cat("\n=== COMPARAISON DES AIC ===\n")
cat("AIC modèle complet  :", AIC(modele_complet), "\n")
cat("AIC modèle stepwise :", AIC(modele_stepwise), "\n")
cat("AIC modèle backward :", AIC(modele_backward), "\n")
cat("→ Retenir le modèle avec l'AIC le plus faible\n")


# ─────────────────────────────────────────────────────────────────────────────
# 7. VÉRIFICATION DES HYPOTHÈSES DU MODÈLE FINAL
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== VÉRIFICATION DES HYPOTHÈSES ===\n")

# 7.1 Graphiques de diagnostic (4 en un)
par(mfrow = c(2, 2))
plot(modele_final,
     main = c("Résidus vs Ajustés","QQ-plot Résidus",
              "Scale-Location","Distance de Cook"))
par(mfrow = c(1, 1))

# 7.2 Normalité des résidus
cat("\n--- Test de normalité des résidus (Shapiro-Wilk) ---\n")
sw_resid <- shapiro.test(residuals(modele_final))
print(sw_resid)
if (sw_resid$p.value > 0.05) {
  cat("→ Résidus normalement distribués : VALIDÉ\n")
} else {
  cat("→ Normalité des résidus : NON VALIDÉE – Prudence requise\n")
}

# 7.3 Homoscédasticité (Test de Breusch-Pagan)
cat("\n--- Test d'homoscédasticité (Breusch-Pagan) ---\n")
bp_test <- bptest(modele_final)
print(bp_test)
if (bp_test$p.value > 0.05) {
  cat("→ Homoscédasticité : VALIDÉE\n")
} else {
  cat("→ Hétéroscédasticité détectée – Envisager une transformation de TERME\n")
}

# 7.4 Indépendance des résidus (Test de Durbin-Watson)
cat("\n--- Test d'autocorrélation des résidus (Durbin-Watson) ---\n")
dw_test <- dwtest(modele_final)
print(dw_test)
if (dw_test$statistic > 1.5 & dw_test$statistic < 2.5) {
  cat("→ Indépendance des résidus : VALIDÉE (DW =",
      round(dw_test$statistic, 3), ")\n")
} else {
  cat("→ Autocorrélation possible des résidus (DW =",
      round(dw_test$statistic, 3), ")\n")
}

# 7.5 Multicolinéarité (VIF)
cat("\n--- Facteurs d'Inflation de la Variance (VIF) ---\n")
tryCatch({
  vif_vals <- vif(modele_final)
  print(round(vif_vals, 3))
  if (any(vif_vals > 5)) {
    cat("→ Attention : VIF > 5 détecté pour :",
        names(vif_vals[vif_vals > 5]), "– Multicolinéarité possible\n")
  } else {
    cat("→ Multicolinéarité : ACCEPTABLE (tous VIF < 5)\n")
  }
}, error = function(e) {
  cat("→ VIF non calculable sur ce modèle (trop peu de variables ou données)\n")
})


# ─────────────────────────────────────────────────────────────────────────────
# 8. INTERPRÉTATION DU MODÈLE FINAL
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== RÉSULTATS FINAUX DU MODÈLE ===\n")
resultats_complets <- summary(modele_final)
print(resultats_complets)

cat("\n--- Coefficients avec IC 95% ---\n")
ic <- confint(modele_final, level = 0.95)
coeff_ic <- cbind(coef(modele_final), ic)
colnames(coeff_ic) <- c("Coefficient", "IC 2.5%", "IC 97.5%")
print(round(coeff_ic, 4))

cat("\n--- Qualité d'ajustement ---\n")
cat("R²          :", round(resultats_complets$r.squared, 4), "\n")
cat("R² ajusté   :", round(resultats_complets$adj.r.squared, 4), "\n")
cat("F-statistic :", round(resultats_complets$fstatistic[1], 4), "\n")
cat("p-value (F) :", pf(resultats_complets$fstatistic[1],
                        resultats_complets$fstatistic[2],
                        resultats_complets$fstatistic[3],
                        lower.tail = FALSE), "\n")
cat("AIC         :", AIC(modele_final), "\n")
cat("BIC         :", BIC(modele_final), "\n")


# ─────────────────────────────────────────────────────────────────────────────
# 9. VALEURS PRÉDITES ET RÉSIDUS
# ─────────────────────────────────────────────────────────────────────────────

data$TERME_predit <- fitted(modele_final)
data$residus      <- residuals(modele_final)

cat("\n=== APERÇU : VALEURS OBSERVÉES vs PRÉDITES ===\n")
print(data %>% select(type_grossesse, TERME, TERME_predit, residus) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))

# Graphique : valeurs observées vs prédites
ggplot(data, aes(x = TERME_predit, y = TERME, color = type_grossesse)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray50", linewidth = 1) +
  scale_color_manual(values = c("steelblue","tomato")) +
  labs(title = "Valeurs observées vs valeurs prédites",
       x = "TERME prédit (SA)", y = "TERME observé (SA)",
       color = "Type de grossesse") +
  theme_minimal()


# ─────────────────────────────────────────────────────────────────────────────
# 10. EXPORT DES RÉSULTATS
# ─────────────────────────────────────────────────────────────────────────────

# Tableau des coefficients (format broom)
tableau_resultats <- tidy(modele_final, conf.int = TRUE, conf.level = 0.95) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n=== TABLEAU FINAL DES COEFFICIENTS ===\n")
print(tableau_resultats)

# Export CSV
write.csv(tableau_resultats, "resultats_regression_TERME.csv", row.names = FALSE)
cat("\n→ Résultats exportés : resultats_regression_TERME.csv\n")

# Export données avec prédictions
write.csv(data, "donnees_avec_predictions.csv", row.names = FALSE)
cat("→ Données avec prédictions exportées : donnees_avec_predictions.csv\n")

cat("\n=== FIN DU SCRIPT ===\n")
