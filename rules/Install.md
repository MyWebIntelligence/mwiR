# Guide d’Installation – mwiR

Ce document explique comment installer et configurer le package **mwiR** ainsi que toutes ses dépendances, pour un usage en développement ou en production.

---

## 1. Prérequis système

- **R** (version ≥ 4.0 recommandée)
- **RStudio** (optionnel, mais recommandé pour le développement)
- **Git** (pour cloner le dépôt et gérer le versionnement)
- **Python 3** (pour l’extraction de contenu web avec Trafilatura)
- **SQLite** (généralement inclus avec R, sinon installer via votre gestionnaire de paquets)

### Installation des dépendances système sous Ubuntu/Debian

```bash
sudo apt update
sudo apt install r-base r-base-dev git python3 python3-pip sqlite3
```

### Installation sous macOS (avec Homebrew)

```bash
brew install --cask r rstudio
brew install git python3 sqlite3
```

---

## 2. Installation des dépendances R

Ouvrez R ou RStudio, puis exécutez :

```r
install.packages(c(
  "devtools", "testthat", "roxygen2", "DBI", "RSQLite", "httr", "jsonlite", "dplyr", "stringr"
))
```

**Remarque :**  
Certaines dépendances peuvent évoluer. Vérifiez le fichier `DESCRIPTION` pour la liste à jour.

---

## 3. Installation de Trafilatura (Python)

Trafilatura est utilisé pour l’extraction de texte web. Installez-le via pip :

```bash
pip3 install trafilatura
```

**Vérification :**

```bash
python3 -c "import trafilatura; print(trafilatura.__version__)"
```

---

## 4. Installation du package mwiR

### Depuis le dépôt Git (développement)

```bash
git clone <url_du_depot>
cd <dossier_du_projet>
devtools::load_all(".")
```

### Depuis une archive (utilisateur)

Téléchargez et décompressez l’archive, puis :

```r
install.packages("chemin/vers/mwiR.tar.gz", repos = NULL, type = "source")
```

---

## 5. Configuration initiale

- Lancez RStudio et ouvrez le projet `mwiR.Rproj`.
- Exécutez la fonction d’initialisation :

```r
library(mwiR)
initmwi()
```

- Vérifiez la création de la base `mwi.db` dans le dossier `extdata/` ou à la racine.

---

## 6. (Optionnel) Configuration de SerpAPI

Pour utiliser la recherche d’URLs via SerpAPI, créez un compte sur [serpapi.com](https://serpapi.com/), récupérez votre clé API et configurez-la dans votre environnement R :

```r
Sys.setenv(SERPAPI_KEY = "votre_cle_api")
```

---

## 7. Vérification de l’installation

- Lancez les tests unitaires :

```r
devtools::test()
```

- Vérifiez que toutes les fonctions principales s’exécutent sans erreur :

```r
library(mwiR)
listlands()
```

---

## 8. Problèmes courants et solutions

- **Erreur “package ‘xxx’ is not available”** : Vérifiez la version de R et la connexion internet.
- **Problème avec Trafilatura** : Vérifiez l’installation Python et le chemin d’accès à Python dans R (`reticulate::py_config()`).
- **Base SQLite non trouvée** : Relancez `initmwi()` ou vérifiez les permissions d’écriture.
- **Tests qui échouent** : Vérifiez que toutes les dépendances sont installées et à jour.

---

## 9. Désinstallation

Pour supprimer le package et ses dépendances :

```r
remove.packages("mwiR")
# Supprimez manuellement le dossier du projet et la base mwi.db si besoin
```

---

## 10. Ressources complémentaires

- [Documentation R Packages](https://r-pkgs.org/)
- [Trafilatura](https://trafilatura.readthedocs.io/en/latest/)
- [SerpAPI](https://serpapi.com/)
- [SQLite](https://www.sqlite.org/docs.html)

---

**En cas de difficulté, consultez le README.md ou contactez le mainteneur du projet.**
