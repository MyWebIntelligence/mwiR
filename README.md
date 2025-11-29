# mwiR 0.9.0 (Beta) The R Package of My Web Intelligence Project
<!-- badges: start -->
![MyWebIntelligence Banner](man/figures/mwibanner.png)
<!-- badges: end -->

## Purpose of My Web Intelligence

**Context and Objectives**

My Web Intelligence (MWI) is a project designed to meet the growing need
for tools and methodologies in the field of digital methods in social
sciences and information and communication sciences (ICS). The main
objective is to map the digital ecosystem to identify key actors, assess
their influence, and analyze their discourses and interactions. This
project addresses the increasing centrality of digital information and
interactions in various fields, including health, politics, culture, and
beyond.

### About the Author

**Amar LAKEL**

Amar Lakel is a researcher in information and communication sciences,
specializing in digital methods applied to social studies. He is
currently a member of the MICA laboratory (Mediation, Information,
Communication, Arts) at the University of Bordeaux Montaigne. His work
focuses on the analysis of online discourse, mapping digital ecosystems,
and the impact of digital technologies on social and cultural practices.

#### Online Profiles

-   **MICA Labo**: [MICA Labo
    Profile](https://mica.u-bordeaux-montaigne.fr/amar-lakel/)
-   **Google Scholar**: [Google Scholar
    Profile](https://scholar.google.com/citations?user=hqquhfwAAAAJ)
-   **ORCID**: [ORCID Profile](https://orcid.org/0000-0002-1234-5678)
-   **ResearchGate**: [ResearchGate
    Profile](https://www.researchgate.net/profile/Amar_Lakel)
-   **Academia**: [Academia
    Profile](https://univ-bordeaux.academia.edu/AmarLakel)
-   **Twitter**: [Twitter MyWebIntel
    Profile](https://twitter.com/mywebintel)
-   **LinkedIn**: [LinkedIn
    Profile](https://www.linkedin.com/in/amar-lakel-123456789/)

## Methodology

**Research Protocol**

The research protocol of MWI relies on a combination of quantitative and
qualitative methods:

1.  **Data Extraction and Archiving**: Using crawl technologies to
    collect data from the web.
2.  **Data Qualification and Annotation**: Applying algorithms to
    analyze, classify, and annotate the data.
3.  **Data Visualization**: Developing dashboards and relational maps to
    interpret the results.

**Methodological Challenges**

The MWI project utilizes techniques from the sociology of controversies,
social network analysis, and text mining methods to:

-   Analyze the strategic positions of speakers in a heterogeneous and
    complex digital corpus.
-   Identify and understand the dynamics of online discourses.
-   Map the relationships between different actors and their respective
    influences.

## Case Studies

**Diverse Cases**

1.  **Health Information**

-   **Asthma and Diabetes in Children**: Studies of online discourses
    related to these diseases to identify influential actors, understand
    their positions, and evaluate their impact on patients’ perceptions
    and behaviors. [Source](https://journals.openedition.org/rfsic/8376)

2.  **Online Political Controversy**

-   **Juan Branco Project**: Analysis of discourses and influence
    surrounding the public figure Juan Branco, exploring the dynamics of
    positioning and controversy.
    [Source](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4584133)

3.  **Research Sociology**

-   **Digital Humanities**: Studies on the impact of digital
    technologies on humanities and social sciences, including how
    researchers use the web to disseminate and discuss their work.
    [Source](https://hal.science/hal-02485370)

**Results and Impact**

The results of these studies show that online discourses play a crucial
role in shaping opinions and behaviors in various fields. They also
highlight the importance for researchers and professionals to actively
engage in these discussions to promote reliable and scientifically
validated information.

## Repositories and Documentation

**NAKALA Repositories** The data and results of the MWI project are
deposited on the NAKALA platform, providing open access for other
researchers and practitioners. Here are some important repositories:

1.  [The
    collection](https://nakala.fr/collection/10.34847/nkl.b4aarv3j):
    Contains a detailed description of the project, methodology, and
    results.
2.  [Positions and Influences on the Web: The Case of Health
    Information](https://nakala.fr/prise-de-position): Detailed analysis
    of discourses on childhood asthma.
3.  [French Digital Humanities
    communities](https://nakala.fr/10.34847/nkl.f43by03n): A study case
    on French digital humanities development on the web.

# Development of the R Package

The R package developed within the framework of My Web Intelligence is
designed to: - Facilitate the replication of analyses conducted in the
project. - Enable the extension of developed methods and tools for other
research. - Provide researchers and professionals with a powerful tool
to understand and manage the dynamics of online information.

**Main Features**

-   **Project Management**: Tools to initiate and manage web exploration
    projects.
-   **Data Extraction**: Functions to crawl the web and extract data
    corpora.
-   **Analysis and Annotation**: Algorithms to analyze and annotate
    extracted data.
-   **Visualization**: Dashboards and maps to visualize relationships
    between actors and discourses.

## Conclusion

My Web Intelligence is an integrative project aimed at transforming how
we understand and analyze digital information across various fields in
social sciences and ICS. By combining innovative methodologies and
advanced technological tools, MWI offers new perspectives on digital
dynamics and proposes solutions to better understand online interactions
and discourses. The R package developed from this project is an
essential tool for researchers and practitioners, enabling them to fully
exploit web data for in-depth and relevant analyses.

# Using mwiR : a study case

## Installation

You can install the development version of mwiR from
[GitHub](https://github.com/) with:

   

## Project (‘land’) Setup

This is a basic example which shows you how to solve a common problem:

    install.packages("remotes")  # si nécessaire
    remotes::install_git("https://github.com/MyWebIntelligence/mwiR.git")

    library(mwiR)


Trafilatura is a python package necessary library for using this package.
If it is not already installed, you can install it using the following code in your terminal console:

pip install trafilatura

For 'Enter your SERP API key or press Enter:'
The serp_api function is a future feature. For now, you can just press Enter.
In future versions, this feature will allow the use of an API key to interact with search engine results pages (SERP).

## Step 1: Creating the Research Project

In this step-by-step guide, we will walk through the initial setup and
execution of a research project using the My Web Intelligence (MWI)
method. This method allows researchers to analyze the impact of various
factors, such as AI on work, by collecting and organizing web data. Here
is a breakdown of the R script provided:

### 1. Load the Required Packages

    initmwi()



The `initmwi()` function initializes the My Web Intelligence environment
by loading all necessary packages and setting up the environment for
further operations. This function ensures that all dependencies and
configurations are correctly initialized.

### 2. Set Up the Database

    db_setup()


The `db_setup()` function sets up the database needed for storing and
managing the data collected during the research project. It initializes
the necessary database schema and ensures that the database is ready for
data insertion and retrieval.

-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

### 3. Create a Research Project (Land)

    create_land(name = "AIWork", desc = "Impact of AI on work", lang="en")


The `create_land()` function creates a new research project, referred to
as a “land” in MWI terminology. This land will serve as the container
for all data and analyses related to the project.

-   `name`: A string specifying the name of the land.
-   `desc`: A string providing a description of the land.
-   `lang`: A string specifying the language of the land. Default is
    `"en"`.
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

### 4. Add Search Terms

    addterm("AIWork", "AI, artificial intelligence, work, employment, job, profession, labor market")

The `addterm()` function adds search terms to the project. These terms
will be used to crawl and collect relevant web data.

-   `land_name`: A string specifying the name of the land.
-   `terms`: A comma-separated string of terms to add.

### 5. Verify the Project Creation

    listlands("AIWork")

The `listlands()` function lists all lands or projects that have been
created. By specifying the project name “AIWork”, it verifies that the
project has been successfully created.

-   `land_name`: A string specifying the name of the land to list. If
    `NULL`, all lands are listed. Default is `NULL`.
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

### 6. Add URLs Manually or Using a File

    addurl("AIWork", urls = "https://www.fr.adp.com/rhinfo/articles/2022/11/la-disparition-de-certains-metiers-est-elle-a-craindre.aspx")


The `addurl()` function adds URLs to the project. These URLs point to
web pages that contain relevant information for the research.

-   `land_name`: A string specifying the name of the land.
-   `urls`: A comma-separated string of URLs to add. Default is `NULL`.
-   `path`: A string specifying the path to a file containing URLs.
    Default is `NULL`.
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

Alternatively, URLs can be added using a text file:

    # If using a text file
    
    addurl("AIWork", path = "_ai_or_artificial_intelligence___work_or_employment_or_job_or_profession_or_labor_market01.txt")

-   `path`: The path to a text file containing the URLs to be added.

### 7. List the Projects or a Specific Project

    listlands("AIWork")

This function is used again to list the projects or a specific project,
ensuring that the URLs have been added correctly to “AIWork”.

### 8. Optionally Delete a Project

    deleteland(land_name = "AIWork")


The `deleteland()` function deletes a specified project. This can be
useful for cleaning up after the research is completed or if a project
needs to be restarted.

-   `land_name`: A string specifying the name of the land to delete.
-   `maxrel`: An integer specifying the maximum relevance for
    expressions to delete. Default is `NULL`.
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

This script demonstrates the basic setup and execution of a research
project using My Web Intelligence, including project creation, term
addition, URL management, and project verification.

## Step 2: Crawling

In this section, we will walk through the process of crawling URLs and
extracting content for analysis using the My Web Intelligence (MWI)
method. The following R code snippets demonstrate how to perform these
tasks.

### Crawl URLs for a Specific Land

    crawlurls("AIWork", limit = 10)


The `crawlurls()` function crawls URLs for a specific land, updates the
database, and calculates relevance scores.

-   `land_name`: A character string representing the name of the land.
-   `urlmax`: An integer specifying the maximum number of URLs to be
    processed (default is 50).
-   `limit`: An optional integer specifying the limit on the number of
    URLs to crawl.
-   `http_status`: An optional character string specifying the HTTP
    status to filter URLs.
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

**Example:**

This example demonstrates crawling up to 10 URLs for the land named
“IATravail”.

    crawlurls("AIWork", limit = 10)



### Crawl Domains

    crawlDomain(1000)



The `crawlDomain()` function crawls domains and updates the Domain table
with the fetched data.

-   `nburl`: An integer specifying the number of URLs to be crawled
    (default is 100).
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

**Example:**

This example demonstrates crawling 1000 URLs and updating the Domain
table.

    crawlDomain(1000)

    

## Step 3: Export Files and Corpora

In this section, we will walk through the process of exporting data and
corpora from a research project using the My Web Intelligence (MWI)
method. The following R code snippets demonstrate how to perform these
tasks.

### Export Land Data

    #type = ['pagecsv', 'pagegexf', 'fullpagecsv', 'nodecsv', 'nodegexf', 'mediacsv', 'corpus']

    # Exemple d'utilisation "le projet", "type d'export", "relevance", "file"
    
    export_land("giletsjaunes", "pagegexf", 3)

The `export_land()` function manages the exportation of land data based
on the specified export type.

-   `land_name`: A character string specifying the name of the land.
-   `export_type`: A character string specifying the type of export.
    Options include `"pagecsv"`, `"fullpagecsv"`, `"nodecsv"`,
    `"mediacsv"`, `"pagegexf"`, `"nodegexf"`, or `"corpus"`.
-   `minimum_relevance`: A numeric value specifying the minimum
    relevance score for inclusion in the export. Default is `1`.
-   `labase`: A character string specifying the name of the database
    file. Default is `"mwi.db"`.

**Example:**

This example demonstrates exporting data for the project “giletsjaunes”
with a minimum relevance score of 3 into a GEXF file.

    export_land("giletsjaunes", "pagegexf", 3)

## Step 4: Enrich Your Corpus with SerpAPI Helpers

Once the foundational land is in place, the next objective is to broaden your web perimeter. The package provides dedicated helpers around [SerpAPI](https://serpapi.com/) so you can script keyword expansion and SERP harvesting before every crawl.

### 1. Discover Related Queries

```r
related_query("intelligence artificielle", lang = "fr", country = "France")
```

`related_query()` returns the "People also search for" block as a tidy data frame. Typical workflow: collect the suggestions, inspect them quickly in R, fold the most relevant ones back into `addterm()`, and archive the CSV for methodological transparency.

### 2. Capture Google, DuckDuckGo, and Bing Result Lists

```r
urlist_Google(
  query = "ai OR artificial intelligence",
  datestart = "2024-01-01",
  dateend   = "2024-03-31",
  timestep  = "month",
  sleep_seconds = 2,
  lang = "en"
)
```

`urlist_Google()`, `urlist_Duck()`, and `urlist_Bing()` paginate SERP responses and write raw URL dumps on disk (one file per query). You can then read those files back with `importFile()` and feed them to `addurl()`. Remember to space requests (`sleep_seconds`) to stay inside rate limits.

### 3. Monitor SEO Signals

```r
mwir_seorank(
  filename = "aiwatch_seo",
  urls     = c("example.com", "opencorpus.org"),
  api_key  = Sys.getenv("SEORANK_API_KEY")
)
```

`mwir_seorank()` queries the SEO Rank API for MOZ/PageSpeed style indicators. Because the function appends rows as soon as a response arrives, you can launch it overnight on dozens of domains and obtain a ready-to-share CSV.

## Step 5: Transform and Diagnose Numeric Variables

When the time comes to model or discretise quantitative indicators (e.g., in-degree, frequency, sentiment scores), the package offers statistical helpers inspired by social-science workflows.

### 1. Explore Transformations Visually

```r
plotlog(
  df         = analytics,
  variables  = c("in_degree", "reach"),
  trans_type = c(in_degree = "log1p", reach = "zscore"),
  save       = TRUE
)
```
`plotlog()` overlays the original and transformed histograms so you can compare scales immediately. Main arguments and expected inputs:

- `df` — data frame passed to the function. If you leave `variables = NULL`, every **numeric** column in `df` is analysed.
- `variables` — character vector that specifies the columns to plot. You can supply a named vector or list so each variable receives its own transformation rule.
- `trans_type` — transformation applied to each series. Recognised keywords: `"none"`, `"log"`, `"log1p"`, `"sqrt"`, `"rank"`, `"zscore"`. Provide a single value to reuse it everywhere, a named vector/list to mix them, or a custom function returning a numeric vector.
- `bins` — histogram resolution. Accept an integer (e.g. `30`) or one of the standard rules: `"sturges"` (default), `"fd"`/`"freedman-diaconis"`, `"scott"`, `"sqrt"`, `"rice"`, `"doane"`, `"auto"` (maximum of Sturges and F-D).
- `colors`, `alpha` — choose the two fill colours (original vs transformed) and set the transparency level between 0 and 1.
- `theme` — any `ggplot2` theme object (`theme_minimal()` by default).
- `density`, `show_rug` — booleans that toggle a kernel density overlay and a rug showing individual points.
- `na_rm`, `min_non_missing` — control filtering. `na_rm = TRUE` drops non-finite values before plotting; `min_non_missing` (default 5) is the minimum number of finite values required for a variable to be plotted.
- `shift_constant` — positive offset automatically added before log/sqrt transformations when the data contains zero or negative values (default 1).
- `display` — `TRUE` prints the combined panel to the current graphics device; set to `FALSE` to return the object silently.
- `save` — set to `TRUE` to export the plots. Use with `save_dir` (folder), `save_format` (`"png"`, `"pdf"`, or `"jpg"`), `save_dpi`, `device_width`, and `device_height` to control the files that are written.
- `verbose` — produces progress messages when `TRUE` (defaults to `interactive()`).

### 2. Apply Robust Transformations Programmatically

```r
scaled <- transform_variable(
  x         = analytics$reach,
  method    = "yeojohnson",
  winsorize = 0.01
)
```

`transform_variable()` stores both the transformed values and the inverse mapping. This makes it easy to export model-ready columns while keeping de-standardisation metadata.

- `x` — numeric vector to transform (NA/Inf allowed; non-finite entries propagate).
- `method` — transformation choice: `"auto"` (bestNormalize search), `"none"`, `"center"`, `"zscore"`, `"robust_z"`, `"log"`, `"log1p"`, `"sqrt"`, `"boxcox"`, `"yeojohnson"`, `"ranknorm"`, or a user-supplied function.
- `winsorize` — optional share of tails to trim before transforming (0 ≤ value < 0.5). Use `NULL` to skip.
- `shift_constant` — positive constant automatically added before log/sqrt transforms when `x` contains non-positive values (default 1).
- `handle_na` — choose `"keep"` (default) to leave NA in place or `"omit"` to drop them before fitting the transform.
- `...` — forwarded to `bestNormalize` helpers (e.g. Box-Cox tweaks) when the selected method requires it.

### 3. Segment Indicators into Meaningful Classes

```r
clusters <- find_clusters(
  analytics$reach,
  max_G         = 5,
  transform     = "auto",
  winsorize     = 0.01,
  return_breaks = TRUE
)

classes <- discretize_variable(
  analytics$reach,
  method = "manual",
  breaks = clusters$breaks,
  labels = c("Faible", "Moyen", "Élevé")
)
```

- `find_clusters()` ajuste des mélanges gaussiens 1D pour révéler des typologies. Paramètres essentiels : `max_G` (nombre de composantes), `criterion` (`"bic"` ou `"icl"`), `transform` (`"none"`, `"log1p"`, `"yeojohnson"`, `"zscore"`, `"auto"`) et `winsorize` (0–0.5). Avec `return_breaks = TRUE`, la fonction fournit des bornes prêtes à l’emploi et expose la `classification`, les probabilités `posterior`, `n_clusters` et plusieurs diagnostics.
- `discretize_variable()` transforme ensuite la mesure continue en classes interprétables. Les méthodes disponibles (`"equal_freq"`, `"equal_width"`, `"quantile"`, `"jenks"`, `"kmeans"`, `"gmm"`, `"manual"`) couvrent la plupart des scénarios. En mode `"manual"`, fournissez vos `breaks` (ceux du clustering, par exemple) et des `labels` parlants. Le facteur retourné reste ordonné et conserve un attribut `discretize_meta` (bornes, effectifs, avertissements).

### 4. Examine Heavy-Tailed Behaviours

```r
powerlaw <- analyse_powerlaw(
  analytics$reach,
  type             = "discrete",
  candidate_models = c("powerlaw", "lognormal", "exponential"),
  bootstrap_sims   = 200,
  winsorize        = 0.01,
  threads          = 4
)
```

- `analyse_powerlaw()` confronte plusieurs lois de queue pour tester la présence d’une véritable loi de puissance.
- En mode `"discrete"`, les données sont arrondies et les valeurs < 1 exclues ; vérifie qu’il reste assez d’observations positives (`min_n` = 50 par défaut).
- Ajuste `type`, `candidate_models`, `winsorize`, `xmin` et `threads` (nombre de cœurs utilisés pendant le bootstrap) selon tes besoins de robustesse et de temps de calcul.
- `candidate_models` accepte `"powerlaw"`, `"lognormal"`, `"exponential"` (et `"weibull"` en continu). Tu peux fournir un sous-ensemble ciblé ou changer l’ordre selon les lois pertinentes pour ton terrain.
- `bootstrap_sims` contrôle le nombre de simulations KS, `bootstrap_models` restreint la liste des modèles simulés. Diminue `bootstrap_sims` pour un résultat rapide, augmente-le pour plus de précision.
- La sortie regroupe `best_model`, les paramètres (`best_fit`), les comparaisons de vraisemblance (`comparisons`), les diagnostics bootstrap (`bootstrap`) et un `data_summary` directement mobilisable dans les rapports.
- Bonnes pratiques : essayer plusieurs `winsorize`, surveiller `best_fit$n_tail`, examiner les p-values bootstrap et justifier le `xmin` retenu.

## Step 8: Leverage AI Assistance Responsibly

Large Language Models can speed up qualitative coding, but they demand guardrails. The unified `LLM_Recode()` function supports multiple providers (OpenAI, OpenRouter, Anthropic, Ollama) with a simple, consistent interface.

### Configuration (once per session)

```r
# Configure your preferred provider
LLM_Config(provider = "openai", lang = "fr")

# Or set API keys directly
Sys.setenv(OPENAI_API_KEY = "sk-...")
Sys.setenv(OPENROUTER_API_KEY = "orpk-...")
Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")
```

### Basic Usage with Vectors

```r
# Simple translation of a vector
translations <- LLM_Recode(
  data        = c("Hello world", "Automation and labour markets"),
  prompt      = "Traduire en français: {value}",
  temperature = 0.4
)
```

### Advanced Usage with Data Frames

```r
# Process multiple columns using glue templates
results <- LLM_Recode(
  data   = my_dataframe,
  prompt = "Résumer en 20 mots: {title} - {content}",
  provider        = "openrouter",
  model           = "openrouter/auto",
  temperature     = 0.2,
  max_tokens      = 80,
  return_metadata = TRUE,
  parallel        = TRUE,       # Enable parallel processing
  workers         = 4
)

# Check failures
failed <- results[results$status != "ok", ]
```

**Entrées de `LLM_Recode()`**
- `data` : vecteur ou data.frame à traiter (obligatoire).
- `prompt` : template glue avec placeholders `{variable}` (obligatoire). Pour les vecteurs, utiliser `{value}`.
- `provider` : `"openai"`, `"openrouter"`, `"anthropic"`, ou `"ollama"` (auto-détecté si omis).
- `model` : identifiant du modèle (défaut selon provider).
- `temperature` (0–2) : aléa de génération.
- `max_tokens` : plafond de tokens en sortie.
- `max_retries`, `retry_delay`, `backoff_multiplier` : stratégie de retry avec backoff exponentiel.
- `rate_limit_delay` : délai entre requêtes pour respecter les quotas.
- `parallel`, `workers` : traitement parallèle via `future`/`furrr`.
- `return_metadata` : retourne un data.frame avec statut, tentatives, tokens utilisés.
- `on_error` : `"continue"` (défaut) ou `"stop"` sur la première erreur.

**Sortie**
- Par défaut, un vecteur de chaînes recodées (NA en cas d'échec).
- Avec `return_metadata = TRUE`, un data.frame avec colonnes `value`, `status`, `attempts`, `tokens_used`, `model_used`, `error_message`.

**`LLM_Config()` - Configuration de session**
```r
LLM_Config(
  provider = "openai",    # Provider par défaut
  model    = "gpt-4o",    # Modèle par défaut
  lang     = "fr",        # Langue des messages (fr/en)
  verbose  = TRUE         # Afficher les messages de progression
)
```

**Bonnes pratiques**
- Documenter `prompt`, `sysprompt`, modèle et version dans votre carnet de labo.
- Baisser `temperature` pour des traductions fidèles, l'augmenter pour des reformulations créatives.
- Limiter `max_tokens` pour garder des réponses concises.
- Utiliser `return_metadata = TRUE` pour auditer les résultats.
- Activer `parallel = TRUE` avec modération (respecter les rate limits des APIs).

## Step 9: Maintain the Database Throughout the Project Lifecycle

The database layer underpins every land. The following helpers keep it healthy and synchronised with external edits.

### 1. Connect Programmatically and Reuse IDs

```r
con      <- connect_db()
land_id  <- get_land_id(con, "AIWork")
domaines <- list_domain(con, land_name = "AIWork")
```

- `connect_db()` returns a ready-to-use `DBI` connection.
- `get_land_id()` converts human-readable land names into numeric IDs when you automate workflows.
- `list_domain()` produces a domain summary (counts, keywords) to monitor coverage.

### 2. Import Additional Material

```r
urls <- importFile()
addurl("AIWork", urls = urls$url)
```

Use `importFile()` whenever you enrich your corpus from spreadsheets or open postings. The helper returns a data frame; pass the relevant column to `addurl()`.

### 3. Reinstate Externally Annotated Data

```r
annotatedData(
  dataplus = curated_notes,
  table    = "Expression",
  champ    = "description",
  by       = "id"
)
```

`annotatedData()` wraps transactional updates so a batch edit either fully succeeds or rolls back. Always back up `mwi.db` before bulk reinsertion.

### 4. Export Precisely What You Need

Beyond `export_land()`, the family of dedicated exporters gives you fine-grained control:

- `export_pagecsv()` and `export_fullpagecsv()` to share tabular corpora.
- `export_nodecsv()` / `export_nodegexf()` for network analysis.
- `export_mediacsv()` to audit associated media.
- `export_pagegexf()` for expression-level graphs.
- `export_corpus()` to assemble text files plus metadata headers (ideal for CAQDAS tools).

Each exporter accepts `minimum_relevance`, so you can balance breadth and focus depending on the audience.
