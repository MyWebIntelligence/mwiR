# WARNING dont use this package is in dev mwiR The R Package of My Web Intelligence Project

<!-- badges: start -->
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

1.  **Online Political Controversy**

-   **Juan Branco Project**: Analysis of discourses and influence
    surrounding the public figure Juan Branco, exploring the dynamics of
    positioning and controversy.
    [Source](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4584133)

1.  **Research Sociology**

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

    # install.packages("devtools")
    devtools::install_github("MyWebIntelligence/mwiR")

    ## Using GitHub PAT from the git credential store.

    ## Downloading GitHub repo MyWebIntelligence/mwiR@HEAD

    ## reticulate (1.37.0 -> 1.38.0) [CRAN]

    ## Installing 1 packages: reticulate

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/k3/vj6gzhmn43g3j13116l042jr0000gn/T//RtmpoUq2Ix/downloaded_packages
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##      checking for file ‘/private/var/folders/k3/vj6gzhmn43g3j13116l042jr0000gn/T/RtmpoUq2Ix/remotesb4ea180109b9/MyWebIntelligence-mwiR-4ec3e11/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/k3/vj6gzhmn43g3j13116l042jr0000gn/T/RtmpoUq2Ix/remotesb4ea180109b9/MyWebIntelligence-mwiR-4ec3e11/DESCRIPTION’
    ##   ─  preparing ‘mwiR’:
    ##    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##      Omitted ‘LazyData’ from DESCRIPTION
    ##   ─  building ‘mwiR_0.1.0.tar.gz’
    ##      
    ## 

## Project (‘land’) Setup

This is a basic example which shows you how to solve a common problem:

    library(mwiR)
    ## basic example code

## Step 1: Creating the research project

    # Load the packages
    initmwi()

    ## @postlight/parser is already installed

    ## Trafilatura is installed and available.

    ## Enter your SERP API key or press Enter:

    # Database setup

    db_setup()

    ## Database setup completed.

    # Create a land

    create_land(name = "AIWork", desc = "Impact of AI on work", lang="en")

    ## Land 'AIWork' created

    # Add terms

    addterm("AIWork", "AI, artificial intelligence, work, employment, job, profession, labor market")

    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"
    ## [1] "Term added to land AIWork"

    ## [1] 1

    # Verify that the project is created

    listlands("AIWork")

    ## Land name: AIWork
    ## Creation date: 1718975512.2099
    ## Description: Impact of AI on work
    ## Terms in the dictionary:  ai, artificial intelligence, work, employment, job, profession, labor market 
    ## Total number of expressions: 0
    ## Number of expressions remaining to be fetched: 0
    ## HTTP status codes: NA: NA

    ## Manual method or using a file



    addurl("AIWork", urls = "https://www.fr.adp.com/rhinfo/articles/2022/11/la-disparition-de-certains-metiers-est-elle-a-craindre.aspx") # froma a dataframe 'paste(AIWorkdeepcrawl$link, collapse = ",")'

    ## [1] "URL added: https://www.fr.adp.com/rhinfo/articles/2022/11/la-disparition-de-certains-metiers-est-elle-a-craindre.aspx"
    ## [1] "Total number of new URLs added: 1"
    ## [1] "URLs added to land AIWork"

    ## [1] 1

    # If using a text file
    #addurl("AIWork", path = "_ai_or_artificial_intelligence___work_or_employment_or_job_or_profession_or_labor_market01.txt")

    # List the lands or a specific land
    listlands("AIWork")

    ## Land name: AIWork
    ## Creation date: 1718975512.2099
    ## Description: Impact of AI on work
    ## Terms in the dictionary:  ai, artificial intelligence, work, employment, job, profession, labor market 
    ## Total number of expressions: 1
    ## Number of expressions remaining to be fetched: 1
    ## HTTP status codes: NA: 1

    # Optionally delete a land
    deleteland(land_name = "AIWork") # OR maxrel= 2 option

    ## [1] "Land AIWork and all its associated records have been deleted"

    ## [1] 1
