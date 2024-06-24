# mwiR 0.8.0 (Beta) The R Package of My Web Intelligence Project
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

    devtools::install_github("MyWebIntelligence/mwiR")
    library(mwiR)

    ## Warning: replacing previous import 'readr::guess_encoding' by
    ## 'rvest::guess_encoding' when loading 'mwiR'

    ## Warning: replacing previous import 'tools::toHTML' by 'XML::toHTML' when
    ## loading 'mwiR'

    ## basic example code

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

    ## Trafilatura is installed and available.

    ## Enter your SERP API key or press Enter:

The `initmwi()` function initializes the My Web Intelligence environment
by loading all necessary packages and setting up the environment for
further operations. This function ensures that all dependencies and
configurations are correctly initialized.

### 2. Set Up the Database

    db_setup()

    ## Database setup completed.

The `db_setup()` function sets up the database needed for storing and
managing the data collected during the research project. It initializes
the necessary database schema and ensures that the database is ready for
data insertion and retrieval.

-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

### 3. Create a Research Project (Land)

    create_land(name = "AIWork", desc = "Impact of AI on work", lang="en")

    ## Land 'AIWork' created

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

    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"

    ## [1] 1

The `addterm()` function adds search terms to the project. These terms
will be used to crawl and collect relevant web data.

-   `land_name`: A string specifying the name of the land.
-   `terms`: A comma-separated string of terms to add.

### 5. Verify the Project Creation

    listlands("AIWork")

    ## Land name: AIWork
    ## Creation date: 1719057037.79256
    ## Description: Impact of AI on work
    ## Terms in the dictionary:  ai, artificial intelligence, work, employment, job, profession, labor market 
    ## Total number of expressions: 0
    ## Number of expressions remaining to be fetched: 0
    ## HTTP status codes: NA: NA

The `listlands()` function lists all lands or projects that have been
created. By specifying the project name “AIWork”, it verifies that the
project has been successfully created.

-   `land_name`: A string specifying the name of the land to list. If
    `NULL`, all lands are listed. Default is `NULL`.
-   `db_name`: A string specifying the name of the SQLite database file.
    Default is `"mwi.db"`.

### 6. Add URLs Manually or Using a File

    addurl("AIWork", urls = "https://www.fr.adp.com/rhinfo/articles/2022/11/la-disparition-de-certains-metiers-est-elle-a-craindre.aspx")

    ## [1] "URL added: https://www.fr.adp.com/rhinfo/articles/2022/11/la-disparition-de-certains-metiers-est-elle-a-craindre.aspx"
    ## [1] "Total number of new URLs added: 1"
    ## [1] "URLs added to land AIWork"

    ## [1] 1

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

    ## Land name: AIWork
    ## Creation date: 1719057037.79256
    ## Description: Impact of AI on work
    ## Terms in the dictionary:  ai, artificial intelligence, work, employment, job, profession, labor market 
    ## Total number of expressions: 1
    ## Number of expressions remaining to be fetched: 1
    ## HTTP status codes: NA: 1

This function is used again to list the projects or a specific project,
ensuring that the URLs have been added correctly to “AIWork”.

### 8. Optionally Delete a Project

    deleteland(land_name = "AIWork")

    ## [1] "Land AIWork and all its associated records have been deleted"

    ## [1] 1

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

    crawlurls("IATravail", limit = 10)


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

    crawlurls("IATravail", limit = 10)



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

## Extra: Build a URLs List from Query

The `listurl.R` file provides functions to build a list of URLs from various search engines using the SerpAPI service. This can be useful for web scraping, data collection, and research purposes.

### Functions Overview

The main functions included in `listurl.R` are:

- `urlist_Google`: Retrieves URLs from Google search results.
- `urlist_Duck`: Retrieves URLs from DuckDuckGo search results.
- `urlist_Bing`: Retrieves URLs from Bing search results.

### Usage Examples

#### Retrieving URLs from Google Search

The `urlist_Google` function allows you to retrieve URLs from Google search results based on a query, date range, and other parameters.

```r
library(mwiR)

# Retrieve URLs from Google search
urlist_Google(
  query = "data science",
  datestart = "2023-01-01",
  dateend = "2023-03-01",
  timestep = "month",
  sleep_seconds = 2,
  lang = "en"
)
```

Parameters:
- `query`: A character string representing the search query.
- `datestart`: A character string representing the start date for the search in "yyyy-mm-dd" format.
- `dateend`: A character string representing the end date for the search in "yyyy-mm-dd" format.
- `timestep`: A character string representing the time step for the date sequence (default is "week").
- `sleep_seconds`: A numeric value representing the sleep time between API requests to avoid rate-limiting (default is 1).
- `lang`: A character string representing the language for the search (default is "fr").

#### Retrieving URLs from DuckDuckGo Search

The `urlist_Duck` function allows you to retrieve URLs from DuckDuckGo search results.

```r
library(mwiR)

# Retrieve URLs from DuckDuckGo search
urlist_Duck(
  query = "data science",
  filename = "data_science_results.txt",
  sleep_seconds = 2,
  lang = "en"
)
```

Parameters:
- `query`: A character string representing the search query.
- `filename`: A character string representing the name of the file where the results will be saved. If NULL, a filename will be generated based on the query (default is NULL).
- `sleep_seconds`: A numeric value representing the sleep time between API requests to avoid rate-limiting (default is 1).
- `lang`: A character string representing the language for the search (default is "fr").

#### Retrieving URLs from Bing Search

The `urlist_Bing` function allows you to retrieve URLs from Bing search results.

```r
library(mwiR)

# Retrieve URLs from Bing search
urlist_Bing(
  query = "data science",
  filename = "data_science_results.txt",
  sleep_seconds = 2,
  lang = "en"
)
```

Parameters:
- `query`: A character string representing the search query.
- `filename`: A character string representing the name of the file where the results will be saved. If NULL, a filename will be generated based on the query (default is NULL).
- `sleep_seconds`: A numeric value representing the sleep time between API requests to avoid rate-limiting (default is 1).
- `lang`: A character string representing the language for the search (default is "fr").

### Note

To use these functions, you need to set up an API key for the SerpAPI service. You will be prompted to enter your API key if it is not already set.

These functions write the search results to a text file, making it easy to analyze and utilize the retrieved URLs for your research or data collection projects.
