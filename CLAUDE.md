# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

mwiR (My Web Intelligence R package) is a digital methods research tool for social sciences. It provides functionality for web crawling, data extraction, network analysis, and AI-assisted text processing to map digital ecosystems and analyze online influence.

## Development Commands

### Build & Check

```bash
# Build the package
R -e "devtools::build()"

# Run R CMD check
R -e "devtools::check()"

# Document with roxygen2 (ALWAYS run after modifying functions)
R -e "devtools::document()"

# Load package for development
R -e "devtools::load_all('.')"
```

### Testing

```bash
# Run all tests
R -e "devtools::test()"

# Run a specific test file
R -e "testthat::test_file('tests/testthat/test-crawl.R')"

# Check test coverage (aim for >80%)
R -e "covr::package_coverage()"
```

### Installation

```bash
# Install from GitHub
R -e "remotes::install_git('https://github.com/MyWebIntelligence/mwiR.git')"

# Install in development mode
R -e "devtools::install()"

# Install from source archive
R -e "install.packages('path/to/mwiR.tar.gz', repos = NULL, type = 'source')"
```

## System Dependencies

### Required Software

- **R** (≥ 4.0 recommended)
- **Python 3** with trafilatura: `pip3 install trafilatura`
- **SQLite** (usually included with R)
- **Git** for version control

### R Dependencies

Core packages listed in DESCRIPTION:
- DBI, RSQLite (database)
- httr, rvest, urltools (web)
- stringr, SnowballC, cld3 (text)
- ggplot2, gridExtra (viz)
- openai, reticulate (AI/Python)
- testthat, mockery (testing)

## Architecture

### Core Components

1. **Project Management (land.R)**: "Lands" are research project containers
   - `createLand()`: Initialize new project
   - `listLands()`: List all projects
   - `deleteLand()`: Remove project

2. **Web Crawling (crawl.R)**: URL crawling with relevance scoring
   - `crawlUrls()`: Main crawling function
   - `crawlDomain()`: Domain-specific crawling
   - Error handling for network issues

3. **Search Integration (listurl.R)**: Search engine interfaces
   - `addUrl()`: Manual URL addition
   - SerpAPI integration for Google/Bing/DuckDuckGo
   - Requires SERPAPI_KEY environment variable

4. **AI Processing (recode.R)**: Text analysis and recoding
   - `GPT_Recode()`: OpenAI integration
   - OpenRouter support for multiple models
   - Annotation and cleaning functions

5. **Data Export (export.R)**: Multiple export formats
   - `export_land()`: Main export function
   - CSV, GEXF (Gephi), corpus formats
   - Data validation before export

### Database Schema

SQLite database (mwi.db) structure:
- `lands`: Project metadata and configuration
- `data`: Crawled content, metadata, annotations
- `urls`: URL tracking, scoring, crawl status
- Database can be regenerated with `init.mwi()`

### Critical Files

- `R/initmwi.R`: Project initialization, database creation
- `.Renviron`: API keys storage (create if needed)
- `extdata/mwi.db`: Default database template

## Maintenance Guidelines

### Before Making Changes

1. Read existing code to understand conventions
2. Check impact on other modules
3. Write tests BEFORE implementing features

### Code Conventions

- **Functions**: snake_case (e.g., `create_land()`)
- **Documentation**: Complete roxygen2 headers
- **Error Messages**: Clear, actionable messages with `stop()`
- **Database**: Always clean up connections
- **Dependencies**: Declare in DESCRIPTION

### Testing Requirements

- Every new feature needs a test
- Run `devtools::test()` before commits
- Use mockery for external services
- Test edge cases and error conditions

### Documentation Updates

After code changes:
1. Update roxygen2 comments
2. Run `devtools::document()`
3. Never edit .Rd files manually
4. Update README.md if needed

## Workflow Examples

### Initialize New Project

```r
library(mwiR)
init.mwi()  # Create/reset database
createLand("my_project", "Research on digital controversies")
```

### Typical Research Workflow

```r
# 1. Setup project
land_id <- createLand("controversy_analysis", "Climate debate mapping")

# 2. Collect URLs (requires SERPAPI_KEY)
Sys.setenv(SERPAPI_KEY = "your_key")
# ... URL collection ...

# 3. Crawl content
crawlUrls(land_id, depth = 2)

# 4. Analyze/Recode
GPT_Recode(land_id, prompt = "Identify main arguments")

# 5. Export results
export_land(land_id, format = "csv")
```

## API Configuration

### Required Environment Variables

```r
# In .Renviron or session:
SERPAPI_KEY="your_serpapi_key"  # For search engines
OPENAI_API_KEY="your_openai_key"  # For GPT features
OPENROUTER_API_KEY="your_key"  # Alternative AI provider
```

## Common Issues & Solutions

1. **"package not found"**: Check R version, run `install.packages()`
2. **Trafilatura errors**: Verify Python setup with `reticulate::py_config()`
3. **Database locked**: Close connections, restart R session
4. **API errors**: Check keys in environment variables
5. **Test failures**: Update all dependencies, check mock functions

## External Integrations

- **Trafilatura**: Python library for content extraction
- **SerpAPI**: Search engine results (subscription required)
- **OpenAI/OpenRouter**: AI text processing
- **Gephi**: Network visualization via GEXF export
- **Shiny**: Interactive web interface in `inst/shiny-app/`

## Development Best Practices

1. **Modularity**: Keep modules independent
2. **Error Handling**: Fail gracefully with informative messages
3. **Performance**: Use vectorized operations, limit API calls
4. **Security**: Never commit API keys, sanitize user inputs
5. **Reproducibility**: Document all analysis steps

## Quick Start for New Maintainers

1. Clone repository and open `mwiR.Rproj`
2. Install dependencies: `devtools::install_deps()`
3. Run tests: `devtools::test()`
4. Make changes, document, test
5. Check package: `devtools::check()`
6. Submit PR with tests and documentation

## Resources

- [R Packages Book](https://r-pkgs.org/)
- [testthat Documentation](https://testthat.r-lib.org/)
- [roxygen2 Guide](https://roxygen2.r-lib.org/)
- [Trafilatura Docs](https://trafilatura.readthedocs.io/)
- Internal guides in `.claude/` directory