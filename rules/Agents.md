# Guide Complet du Mainteneur – Architecture et Rôles des Fichiers dans mwiR

Bienvenue dans le projet **mwiR**. Ce document s’adresse à tout développeur ou mainteneur qui doit comprendre, corriger, faire évoluer ou documenter ce package R. L’objectif est de vous donner une vision claire de l’architecture, des conventions, des dépendances, des workflows et des points de vigilance pour garantir la robustesse et la pérennité du logiciel.

---

## 1. Vue d’ensemble du projet

**mwiR** est un package R destiné à l’analyse d’écosystèmes numériques, principalement pour la recherche en sciences sociales. Il s’appuie sur une architecture modulaire, une base de données SQLite, et des outils R standards pour la documentation, les tests et la distribution.

---

## 2. Arborescence et rôle des répertoires

### Racine du projet

- `README.md` : Introduction, objectifs, installation, premiers pas. À lire en priorité pour comprendre le contexte.
- `DESCRIPTION` : Métadonnées du package (titre, version, dépendances, auteurs, etc.). Toute nouvelle dépendance doit y être déclarée.
- `NAMESPACE` : Généré automatiquement, liste les fonctions exportées/importées. Ne pas éditer à la main.
- `codemeta.json` : Métadonnées au format standard pour l’interopérabilité (utilisé par des outils externes).
- `LICENSE` et `LICENSE.md` : Conditions d’utilisation et de distribution.
- `mwiR.Rproj` : Fichier de projet RStudio (ouvre le projet avec la bonne configuration).
- `mwi.db` : Base de données SQLite principale (créée/écrasée lors de l’initialisation).
- `.gitignore`, `.Rbuildignore` : Fichiers à exclure du contrôle de version ou du build CRAN.

### Répertoires principaux

#### R/
**Contient tout le code source du package.**  
Chaque fichier correspond à un module fonctionnel :
- `crawl.R` : Fonctions de crawling web, gestion des requêtes, parsing, gestion des erreurs réseau.
- `export.R` : Fonctions d’export (CSV, GEXF, etc.), gestion des formats, vérification des données.
- `initmwi.R` : Initialisation du projet, création de la base, configuration des chemins.
- `land.R` : Gestion des “lands” (projets de recherche), création, suppression, listing.
- `listurl.R` : Génération de listes d’URLs à partir de moteurs de recherche, parsing des résultats.
- `recode.R` : Fonctions de recodage, annotation, nettoyage, transformation des données.

**À savoir pour la maintenance :**
- Les fonctions sont documentées en tête de fichier avec des commentaires roxygen2.
- Toute modification doit être suivie d’une mise à jour de la documentation (`devtools::document()`).
- Vérifiez l’impact sur les autres modules (ex : une modification dans `crawl.R` peut affecter `export.R`).

#### man/
**Documentation technique au format Rd.**
- Un fichier `.Rd` par fonction exportée.
- Généré automatiquement via roxygen2.
- Inclut des figures (ex : `mwibanner.png`) pour illustrer la documentation.

**Conseil :**  
Après modification d’une fonction, mettez à jour les commentaires roxygen2 et regénérez la doc. Ne modifiez jamais les `.Rd` à la main.

#### tests/
**Tests unitaires avec testthat.**
- Un fichier de test par module/fonction principale.
- Exemples : `test-crawl.R`, `test-initmwi.R`, etc.
- Les tests sont essentiels pour garantir la stabilité lors des évolutions.

**Bonnes pratiques :**
- Ajoutez un test pour chaque nouvelle fonctionnalité ou bugfix.
- Exécutez `devtools::test()` avant chaque commit.
- Utilisez `covr::package_coverage()` pour vérifier la couverture (>80% recommandé).

#### extdata/
**Données externes et jeux de tests.**
- `mwi.db` : Base SQLite par défaut (peut être régénérée).
- Fichiers CSV de test pour valider les exports/imports.

**Attention :**
- Ne modifiez pas ces fichiers sans mettre à jour les tests associés.
- Limitez la taille des fichiers pour éviter les problèmes de build ou de partage.

#### rules/
**Contraintes, règles et guides internes.**
- `AGENT.md` : Comportement attendu des agents (à lire pour comprendre les attentes en matière d’automatisation et d’IA).
- `Install.md` : Procédure d’installation détaillée, dépendances système, astuces pour résoudre les problèmes courants.

---

## 3. Fonctionnement général et workflow de développement

### Initialisation

- Lancer `initmwi()` pour créer la base et initialiser le projet.
- Vérifier la présence de `mwi.db` dans `extdata/` ou à la racine.

### Création et gestion de projets (“lands”)

- Utiliser `create_land()` pour démarrer un nouveau projet de collecte.
- Les lands sont listés et gérés via `listlands()` et `deleteland()`.

### Collecte de données

- Les fonctions de crawling (`crawlurls`, `crawlDomain`) récupèrent les pages web et les stockent dans la base.
- Les listes d’URLs sont générées automatiquement ou ajoutées manuellement (`addurl`).

### Analyse, recodage et export

- Les données collectées peuvent être annotées (`GPT_Recode`), nettoyées (`clean_string`, `clean_url`), tokenisées, etc.
- L’export se fait via `export_land()` ou d’autres fonctions selon le format souhaité (CSV, GEXF pour Gephi, etc.).

### Tests et validation

- Chaque modification doit être couverte par un test.
- Les tests sont dans `tests/testthat/`.
- Utilisez RStudio ou la ligne de commande pour lancer tous les tests.

### Documentation

- Les fonctions sont documentées en roxygen2.
- Après modification, exécutez `devtools::document()` pour mettre à jour la doc et le NAMESPACE.

---

## 4. Dépendances et intégrations externes

- **Trafilatura (Python)** : Extraction de contenu web. Vérifiez que l’environnement Python est bien configuré.
- **SQLite** : Stockage des données. La base peut être supprimée et régénérée à tout moment.
- **SerpAPI (optionnel)** : Pour la recherche d’URLs sur les moteurs (clé API à configurer si besoin).

---

## 5. Conseils et points de vigilance pour la maintenance

- **Modularité** : Chaque module doit rester indépendant autant que possible. Évitez les dépendances circulaires.
- **Tests** : Ne jamais merger une modification sans test associé.
- **Documentation** : Gardez la documentation à jour, c’est la première source d’information pour les nouveaux arrivants.
- **Base de données** : Sauvegardez la base avant toute opération risquée. Prévoyez des scripts de migration si le schéma évolue.
- **Dépendances** : Toute nouvelle dépendance doit être ajoutée dans `DESCRIPTION` et documentée dans `Install.md`.
- **Conventions de nommage** : Respectez les conventions R (snake_case pour les fonctions, UpperCamelCase pour les classes S3/S4 si utilisées).
- **Gestion des erreurs** : Privilégiez les messages d’erreur explicites et les exceptions gérées.

---

## 6. Cas d’usage typiques

- Analyse de controverses numériques (collecte, annotation, visualisation de réseaux d’acteurs).
- Cartographie d’écosystèmes d’acteurs sur le web.
- Étude de la circulation des discours en ligne.
- Recherche exploratoire en sciences sociales numériques.

---

## 7. Ressources utiles pour aller plus loin

- [Guide R Packages – Hadley Wickham](https://r-pkgs.org/)
- [Documentation testthat](https://testthat.r-lib.org/)
- [roxygen2](https://roxygen2.r-lib.org/)
- [Trafilatura](https://trafilatura.readthedocs.io/en/latest/)
- [SQLite](https://www.sqlite.org/docs.html)

---

## 8. Prise en main rapide pour un nouveau mainteneur

1. Lire ce fichier et le README.md.
2. Installer les dépendances système et R (voir Install.md).
3. Lancer les tests (`devtools::test()`).
4. Explorer le code source dans `R/` et la documentation dans `man/`.
5. Modifier une fonction, regénérer la doc, relancer les tests.
6. Vérifier l’impact sur la base de données et les exports.
7. Documenter toute modification significative dans le README.md ou un changelog.

---

**En cas de doute, privilégiez la clarté, la robustesse et la documentation. Ce projet vise la reproductibilité et la transparence pour la recherche.**
