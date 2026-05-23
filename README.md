# <img alt = "LikertMakeR logo" src="www/LikertMakeR_4.png" width = 10%>  LikertMakeR Online

An interactive Shiny application for generating synthetic Likert scale data 
with specified correlations, means, and standard deviations.

## Overview

LikertMakeR Online provides a user-friendly interface for the 
[LikertMakeR](https://CRAN.R-project.org/package=LikertMakeR) R package, 
allowing researchers and analysts to create synthetic survey data for testing, 
teaching, and simulation purposes.

## Features

  - **Interactive Scale Definition**: Define rating scales with custom parameters
  - **Correlation Matrix Builder**: Specify correlations among scales with visual feedback
  - **Data Generation**: Create synthetic datasets with your exact specifications
  - **Data Validation**: Built-in checks for positive-definite correlation matrices, scale parameters, etc.
  - **Visual Summary**: Automatic pairs plots showing distributions and correlations
  - **Statistics Dashboard**: View means, standard deviations, Cronbach's alpha, and eigenvalues of generated data
- **Export Options**: Download generated data and codebooks as CSV files

## Live Demo

Try the app online: [LikertMakeR Online](https://winzarh-likertmaker-online.share.connect.posit.cloud/)

## Local Installation

### Prerequisites

  - R (>= 4.0.0)
  - RStudio (recommended)

### Required Packages

```r
install.packages(c(
  "shiny",
  "bslib",
  "bsicons",
  "shinycssloaders",
  "matrixcalc",
  "ragg",
  "markdown",
  "DT",
  "GGally",
  "ggplot2",
  "Matrix",
  "LikertMakeR"
))
```

### Running Locally on your own computer

1. Clone this repository:

```bash
git clone https://github.com/WinzarH/likertMakeR_online.git
cd likertMakeR_online
```

2. Open `app.R` in RStudio and click "Run App", or run from the console:

```r
shiny::runApp()
```

## File Structure

```
likertMakeR_online/
├── app.R           # Main application entry point
├── global.R        # Shared libraries and helper functions
├── ui.R            # User interface definition
├── server.R        # Server logic
└── www/
    └── custom.css  # Application styling
    └── information .md files
```

## Usage

  1. **Set Correlations**: Build your correlation matrix using the interactive grid
  2. **Define Scales**: Set the number of scales and specify parameters (mean, SD, range) for each
  3. **Generate Data**: Click "Generate my data" to create your synthetic dataset
  4. **Validate**: Review the Data Validation tab for summary statistics and visual confirmation
  5. **Download**: Export your data and codebook as CSV files

## Deployment

This app is designed to run on:

  - Posit Connect Cloud (formerly shinyapps.io)
  - Posit Connect Server
  - Any standard Shiny Server

## About LikertMakeR

LikertMakeR is an R package for synthesizing and correlating rating-scale 
data with predefined first & second moments. 

For more information:

  - [CRAN Package](https://CRAN.R-project.org/package=LikertMakeR)
  - [Development Version](https://winzarh.github.io/LikertMakeR/)

## License

 MIT

## Citation

If you use this app in your research or teaching, 
please cite the LikertMakeR package:

##### LikertMakeR_online:

Winzar, H. (2026). LikertMakeR_online. _Posit Connect Cloud_, 
<https://winzarh-likertmaker-online.share.connect.posit.cloud>


##### LikertMakeR package documentation

Winzar, H. (2026). LikertMakeR: Synthesise and correlate rating-scale data, 
GitHub Pages, 
<https://winzarh.github.io/LikertMakeR/index.html>


##### LikertMakeR R package

Winzar, H. (2026). LikertMakeR: Synthesise and correlate rating-scale 
data with predefined first & second moments, 
The Comprehensive R Archive Network (CRAN),
<https://doi.org/10.32614/CRAN.package.LikertMakeR>


## Contact

Hume Winzar <winzar@gmail.com>

## Acknowledgments

Built with [Shiny](https://shiny.rstudio.com/) and [bslib](https://rstudio.github.io/bslib/).
