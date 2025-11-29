# Plan: Fonction unifiée LLM_Recode pour mwiR

## Objectif

Fusionner `GPT_Recode` et `OpenRouter_Recode` en une fonction unique `LLM_Recode` utilisant des templates glue pour les prompts.

## Public cible : Chercheurs en sciences sociales

La fonction doit être **simple**, **interactive** et **résiliente** :

- Peu de paramètres obligatoires (seulement `data` et `prompt`)
- Si la clé API manque → question interactive à l'utilisateur
- Si le provider n'est pas choisi → suggestion basée sur les clés disponibles
- Messages d'erreur clairs et en français (option)
- Mémoire de session (clés API, provider préféré)

## Décisions de conception

- **glue**: Dépendance obligatoire (Imports)
- **Anciennes fonctions**: Supprimées entièrement
- **Variable par défaut**: `{value}` pour les vecteurs simples
- **Extensibilité**: Prévoir Claude/Anthropic, Ollama, Azure
- **Parallélisme**: Oui, avec `future` + `furrr`
- **UX**: Mode interactif pour configuration manquante

---

## 1. Signature de la fonction

```r
LLM_Recode <- function(
  # Paramètres principaux
  data,                          # Vecteur ou data.frame
  prompt,                        # Template glue: "Traduire {value} en français"
  provider = c("openai", "openrouter", "anthropic", "ollama"),
  model = NULL,                  # NULL = défaut du provider

  # Paramètres du modèle
  temperature = 0.7,
  max_tokens = 1000,
  sysprompt = "You are a helpful assistant that recodes dataframe values. Return only the transformed value.",

  # Configuration API
  api_key = NULL,                # NULL = auto-détection depuis env
  api_base = NULL,               # URL personnalisée (modèles locaux)
  timeout = 60,

  # Retry et rate limiting
  max_retries = 3,
  retry_delay = 1,
  backoff_multiplier = 2,
  rate_limit_delay = 0,          # Délai entre requêtes

  # Contrôle de sortie
  validate = TRUE,
  return_metadata = FALSE,       # Liste avec valeur + métadonnées
  on_error = c("continue", "stop"),

  # Parallélisme
  parallel = FALSE,              # Activer le traitement parallèle
  workers = NULL,                # Nombre de workers (NULL = auto)

  # Affichage
  verbose = TRUE,
  progress = TRUE,

  # Spécifique OpenRouter
  referer = getOption("mwiR.llm.referer", "https://github.com/MyWebIntelligence"),
  title = getOption("mwiR.llm.title", "mwiR"),
  extra_headers = NULL
)
```

---

## 2. Architecture interne

### 2.1 Registre des providers

```r
.llm_providers <- list(
  openai = list(
    env_key = "OPENAI_API_KEY",
    default_model = "gpt-4o",
    default_base = "https://api.openai.com/v1/chat/completions",
    call_fn = ".call_openai"
  ),
  openrouter = list(
    env_key = "OPENROUTER_API_KEY",
    default_model = "openrouter/auto",
    default_base = "https://openrouter.ai/api/v1/chat/completions",
    call_fn = ".call_openrouter"
  ),
  anthropic = list(
    env_key = "ANTHROPIC_API_KEY",
    default_model = "claude-sonnet-4-20250514",
    default_base = "https://api.anthropic.com/v1/messages",
    call_fn = ".call_anthropic"
  ),
  ollama = list(
    env_key = NULL,  # Pas de clé API pour local
    default_model = "llama3",
    default_base = "http://localhost:11434/api/chat",
    call_fn = ".call_ollama"
  )
)
```

### 2.2 Fonctions internes

- `.get_provider_config(provider)` - Configuration du provider
- `.resolve_api_key(provider, api_key)` - Auto-détection ou validation
- `.render_prompt(template, data_row)` - Substitution glue
- `.prepare_data(data, prompt)` - Conversion en data.frame standard
- `.call_openai(messages, config)` - Appel HTTP OpenAI
- `.call_openrouter(messages, config)` - Appel HTTP OpenRouter
- `.call_anthropic(messages, config)` - Appel HTTP Anthropic
- `.call_ollama(messages, config)` - Appel HTTP Ollama local
- `.call_llm(messages, provider, config)` - Dispatcher
- `.call_with_retry(prompt, call_fn, config)` - Retry avec backoff exponentiel
- `.process_single_row(data_row, ...)` - Traitement d'une ligne
- `.process_batch(data, ...)` - Traitement par lot avec progress bar
- `.format_results(results, return_metadata)` - Formatage sortie

---

## 3. Traitement des templates glue

### Syntaxe supportée

```r
prompt = "Traduire en français: {value}"
prompt = "Résumer en {n_mots} mots: {titre} - {description}"
```

### Implémentation

```r
.render_prompt <- function(template, data_row) {
  glue::glue_data(.x = as.list(data_row), template, .null = "NA")
}

.prepare_data <- function(data, prompt) {
  # Extraire les variables du template
  vars <- regmatches(prompt, gregexpr("\\{([^}]+)\\}", prompt))[[1]]
  vars <- gsub("[{}]", "", vars)

  if (is.vector(data) && !is.data.frame(data)) {
    # Vecteur simple -> data.frame avec colonne "value"
    data <- data.frame(value = data, stringsAsFactors = FALSE)
  }

  # Valider que toutes les variables existent
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    stop("Variables manquantes dans data: ", paste(missing, collapse = ", "))
  }
  data
}
```

---

## 4. Format de retour

### Par défaut (return_metadata = FALSE)

Vecteur caractère de même longueur que l'entrée.

### Avec métadonnées (return_metadata = TRUE)

```r
data.frame(
  row_index = 1:n,
  value = c("résultat1", "résultat2", NA),
  status = c("ok", "ok", "error"),
  attempts = c(1, 1, 3),
  tokens_used = c(45, 38, NA),
  model_used = c("gpt-4o", "gpt-4o", NA),
  error_message = c(NA, NA, "Rate limit exceeded")
)
```

---

## 5. Traitement parallèle (future + furrr)

### Configuration

```r
.process_batch <- function(data, prompt_template, call_fn, config,
                           parallel, workers, progress, on_error) {
  n <- nrow(data)

  if (parallel) {
    # Configuration future
    if (is.null(workers)) workers <- future::availableCores() - 1
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)

    # Traitement parallèle avec furrr
    if (progress) {
      progressr::handlers(global = TRUE)
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = n)
        furrr::future_map(seq_len(n), function(i) {
          p()
          .process_single_row(data[i, , drop = FALSE], prompt_template, call_fn, config, on_error)
        }, .options = furrr::furrr_options(seed = TRUE))
      })
    } else {
      results <- furrr::future_map(seq_len(n), function(i) {
        .process_single_row(data[i, , drop = FALSE], prompt_template, call_fn, config, on_error)
      }, .options = furrr::furrr_options(seed = TRUE))
    }
  } else {
    # Traitement séquentiel avec progress bar
    results <- vector("list", n)
    if (progress && interactive()) {
      pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
      on.exit(close(pb), add = TRUE)
    }
    for (i in seq_len(n)) {
      if (config$rate_limit_delay > 0 && i > 1) Sys.sleep(config$rate_limit_delay)
      results[[i]] <- .process_single_row(data[i, , drop = FALSE], prompt_template, call_fn, config, on_error)
      if (progress && interactive()) utils::setTxtProgressBar(pb, i)
    }
  }
  results
}
```

### Attention rate limits

- En mode parallèle, les rate limits API peuvent être atteints rapidement
- Recommander `rate_limit_delay > 0` ou limiter `workers` pour APIs avec quotas stricts

---

## 6. Suppression anciennes fonctions

Supprimer complètement `GPT_Recode` et `OpenRouter_Recode` du fichier `R/recode.R` (lignes ~1984-2307).

---

## 7. Séquence d'implémentation

### Phase 1: Dépendances

1. Ajouter à DESCRIPTION Imports: `glue`, `future`, `furrr`, `progressr`
2. Bump version (0.2.1 -> 0.3.0)

### Phase 2: Infrastructure (fonctions internes)

3. Créer `.llm_providers` registre avec 4 providers
4. Implémenter `.render_prompt()` avec glue
5. Implémenter `.prepare_data()` avec variable par défaut `{value}`
6. Implémenter `.resolve_api_key()`

### Phase 3: Providers

7. Implémenter `.call_openai()` (refactorer depuis GPT_Recode)
8. Implémenter `.call_openrouter()` (refactorer depuis OpenRouter_Recode)
9. Implémenter `.call_anthropic()` (nouveau)
10. Implémenter `.call_ollama()` (nouveau)
11. Implémenter `.call_llm()` dispatcher

### Phase 4: Traitement

12. Implémenter `.call_with_retry()` avec backoff exponentiel
13. Implémenter `.process_single_row()`
14. Implémenter `.process_batch()` (séquentiel + parallèle)
15. Implémenter `.format_results()`

### Phase 5: Fonction principale

16. Implémenter `LLM_Recode()` avec validation complète
17. Documentation roxygen2 complète

### Phase 6: Nettoyage

18. Supprimer `GPT_Recode` (lignes ~1984-2095)
19. Supprimer `OpenRouter_Recode` (lignes ~2098-2305)
20. Supprimer helper `%||%` si plus utilisé ailleurs

### Phase 7: UX Chercheurs

21. Implémenter `.auto_detect_provider()`
22. Implémenter `.ask_for_api_key()` (interactif)
23. Implémenter `.msg()` (messages bilingues)
24. Implémenter `LLM_Config()` (helper de configuration)
25. Implémenter `.print_summary()`

### Phase 8: Tests et finalisation

26. Tests unitaires (validation, templates, mocks API, parallélisme)
27. `devtools::document()`
28. `devtools::check()`

---

## 8. Fichiers à modifier

| Fichier | Action |
|---------|--------|
| `R/recode.R` | Supprimer anciennes fonctions, ajouter LLM_Recode + helpers |
| `DESCRIPTION` | Ajouter `glue`, `future`, `furrr`, `progressr` à Imports, bump version |
| `tests/testthat/test-recode.R` | Supprimer tests GPT_Recode/OpenRouter_Recode, ajouter tests LLM_Recode |
| `NAMESPACE` | Auto-généré par devtools::document() |
| `man/LLM_Recode.Rd` | Auto-généré par devtools::document() |
| `man/GPT_Recode.Rd` | À supprimer (auto via devtools::document()) |
| `man/OpenRouter_Recode.Rd` | À supprimer (auto via devtools::document()) |

---

## 9. UX pour chercheurs : Mode interactif et mémoire de session

### 9.1 Détection automatique du provider

```r
.auto_detect_provider <- function() {
  # Vérifier quelles clés API sont disponibles
  available <- c()
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) available <- c(available, "openai")
  if (nzchar(Sys.getenv("OPENROUTER_API_KEY"))) available <- c(available, "openrouter")
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) available <- c(available, "anthropic")

  # Ollama toujours disponible si installé localement
  if (.ollama_is_running()) available <- c(available, "ollama")

  if (length(available) == 0) return(NULL)
  if (length(available) == 1) return(available)

  # Retourner le provider préféré de la session ou le premier disponible
  pref <- getOption("mwiR.llm.preferred_provider")
  if (!is.null(pref) && pref %in% available) return(pref)
  available[1]
}
```

### 9.2 Questions interactives si config manquante

```r
.ask_for_api_key <- function(provider) {
  if (!interactive()) {
    stop("Clé API ", provider, " manquante. Définissez-la avec:\n",
         "  Sys.setenv(", .llm_providers[[provider]]$env_key, " = 'votre-clé')")
  }

  message("\n=== Configuration ", toupper(provider), " ===")
  message("Aucune clé API trouvée pour ", provider, ".")
  message("Vous pouvez obtenir une clé sur:")
  message("  - OpenAI: https://platform.openai.com/api-keys")
  message("  - OpenRouter: https://openrouter.ai/keys")
  message("  - Anthropic: https://console.anthropic.com/")

  key <- readline("Entrez votre clé API (ou appuyez sur Entrée pour annuler): ")
  if (!nzchar(key)) stop("Opération annulée par l'utilisateur.")

  # Stocker en session
  do.call(Sys.setenv, setNames(list(key), .llm_providers[[provider]]$env_key))
  message("Clé API enregistrée pour cette session R.")
  message("Pour la rendre permanente, ajoutez à votre .Renviron:\n",
          "  ", .llm_providers[[provider]]$env_key, "=", key)

  key
}
```

### 9.3 Mémoire de session via options()

```r
# Stocker les préférences utilisateur
options(
  mwiR.llm.preferred_provider = "openai",  # Provider par défaut
  mwiR.llm.default_model = NULL,           # Modèle par défaut (NULL = auto)
  mwiR.llm.verbose = TRUE,                 # Afficher les messages
  mwiR.llm.lang = "fr"                     # Langue des messages (fr/en)
)

# Fonction helper pour configurer
LLM_Config <- function(
  provider = NULL,
  model = NULL,
  api_key = NULL,
  verbose = NULL,
  lang = NULL
) {
  if (!is.null(provider)) options(mwiR.llm.preferred_provider = provider)
  if (!is.null(model)) options(mwiR.llm.default_model = model)
  if (!is.null(verbose)) options(mwiR.llm.verbose = verbose)
  if (!is.null(lang)) options(mwiR.llm.lang = lang)

  if (!is.null(api_key) && !is.null(provider)) {
    env_key <- .llm_providers[[provider]]$env_key
    if (!is.null(env_key)) {
      do.call(Sys.setenv, setNames(list(api_key), env_key))
    }
  }

  # Afficher la config actuelle
  message("Configuration LLM actuelle:")
  message("  Provider: ", getOption("mwiR.llm.preferred_provider", "auto"))
  message("  Modèle: ", getOption("mwiR.llm.default_model", "auto"))
  message("  Verbose: ", getOption("mwiR.llm.verbose", TRUE))
  message("  Langue: ", getOption("mwiR.llm.lang", "fr"))

  invisible(NULL)
}
```

### 9.4 Messages d'erreur bilingues

```r
.msg <- function(key, ...) {
  lang <- getOption("mwiR.llm.lang", "fr")
  messages <- list(
    fr = list(
      no_api_key = "Aucune clé API trouvée pour {provider}.",
      rate_limit = "Limite de requêtes atteinte. Pause de {delay} secondes...",
      success = "Traitement terminé: {n} éléments, {errors} erreurs.",
      missing_vars = "Variables manquantes dans les données: {vars}"
    ),
    en = list(
      no_api_key = "No API key found for {provider}.",
      rate_limit = "Rate limit reached. Waiting {delay} seconds...",
      success = "Processing complete: {n} items, {errors} errors.",
      missing_vars = "Missing variables in data: {vars}"
    )
  )
  glue::glue(messages[[lang]][[key]], ...)
}
```

### 9.5 Exemple d'utilisation simplifiée

```r
# Cas le plus simple - tout est auto-détecté
result <- LLM_Recode(
  data = mes_textes,
  prompt = "Traduire en français: {value}"
)

# Si la clé manque, la fonction demande interactivement
# Si le provider n'est pas spécifié, elle utilise celui configuré ou le premier disponible

# Configuration une fois pour toute la session
LLM_Config(provider = "openai", lang = "fr")
```

---

## 10. Gestion des erreurs résiliente

### 10.1 Stratégie de retry intelligent

```r
.call_with_retry <- function(prompt, call_fn, config, row_index) {
  delay <- config$retry_delay
  last_error <- NULL

  for (attempt in seq_len(config$max_retries)) {
    result <- tryCatch({
      call_fn(prompt, config)
    }, error = function(e) {
      list(success = FALSE, error = e, status_code = NA)
    })

    if (isTRUE(result$success)) {
      return(result)
    }

    last_error <- result$error

    # Message informatif (si verbose)
    if (getOption("mwiR.llm.verbose", TRUE)) {
      message(.msg("retry", attempt = attempt, max = config$max_retries,
                   error = conditionMessage(result$error)))
    }

    # Backoff exponentiel sur rate limit
    if (!is.na(result$status_code) && result$status_code == 429) {
      delay <- min(delay * config$backoff_multiplier, 60)  # Max 60s
      if (getOption("mwiR.llm.verbose", TRUE)) {
        message(.msg("rate_limit", delay = delay))
      }
    }

    if (attempt < config$max_retries) {
      Sys.sleep(delay)
      delay <- delay * config$backoff_multiplier
    }
  }

  # Échec après tous les essais
  list(
    value = NA_character_,
    status = "failed",
    attempts = config$max_retries,
    error_message = if (!is.null(last_error)) conditionMessage(last_error) else "Unknown error"
  )
}
```

### 10.2 Résumé final pour l'utilisateur

```r
.print_summary <- function(results, verbose) {
  if (!verbose) return(invisible(NULL))

  n_total <- length(results)
  n_success <- sum(vapply(results, function(r) r$status == "ok", logical(1)))
  n_failed <- n_total - n_success

  message("\n", .msg("success", n = n_total, errors = n_failed))

  if (n_failed > 0) {
    message("Lignes en erreur: ",
            paste(which(vapply(results, function(r) r$status != "ok", logical(1))),
                  collapse = ", "))
  }
}
