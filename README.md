# InflationNarratives
Online Repository for the Paper "Going Viral: Inflation Narratives about the Macroeconomy."

The repository contains the following folders:

## Code 
`./code`: Contains the relevant R code. "project.Rproj" is the project file for RStudio; please open this file first, as it defines the working directory. Run the files in the following order:

- **00_motivation**: Creates the motivation plot.
- **01_corpus**: Creates the corpus file from the raw Dow Jones newswire by prefiltering it by subject codes and keywords.
- **02_spacy_script**: Python code to run lemmatization with spaCy.
- **03_dfm**: Creates a document-term matrix (dfm) from the lemmatized corpus using quanteda and applies trimming to reduce size.
- **04_keyATM**: Selects keywords (supported by word embeddings, etc.) and converts the dfm into a keyATM document; estimates the model with 5,000 iterations.
- **05_analyse_topics**: Conducts descriptive analysis of topic models and robustness checks between the DJN and WSJ corpora.
- **06_LSS**: Estimates the polarity score using latent semantic scaling (LSS).
- **07_tone_timeseries**: Multiplies the polarity score and topic proportions at the document level to create tone-adjusted time series.
- **08_test**: Runs diagnostic tests (e.g., unit root).
- **09_granger_causality**: Estimates multivariate Granger causality tests.
- **10_local_projections**: Infers the diffusion of narratives using local projections.

`functions`: Contains basic functions to create plots or read in files.

## Data

- `./corpus`: Contains the created corpora (not available due to legal reasons).
- `./datatable`: Contains the raw Dow Jones newswire (not available due to legal reasons).
- `./dfm`: Contains the created and filtered dfms (not available due to legal reasons).
- `./localprojections`: Contains macroeconomic data as well as estimated data from LSS and keyATM.
- `./models`: Contains estimated keyATM models.

## Text

- `./figures`: Stores all created figures.
- `./output`: Includes all estimation results from Granger causality tests and local projections.
- `./tables`: Contains stored tables (top words from keyATM as well as unit root tests).
- `./bibtex`: Stores the bibliography used.

## Software

The following software was used:

- **R** version 4.4.1
- **RStudio** 2024.09.0 Build 375

## Contact

- max.weinig@uni-hamburg.de
- ulrich.fritsche@uni-hamburg.de
