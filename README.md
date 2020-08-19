# HICbioclean
An R package specifically designed for automating the process of cleaning, calibrating and validating continuous biological water quality data from the Flemish HIC (Hydrological Information Centre) database.

It provides both R functions for integration into R scrpits and easy to use R Shiny graphical interfaces for intuative data cleaning, validation and calibration without the need for coding.

## Installation

Download the latest version of R from https://cran.r-project.org/ if not already installed.

Open the program R.

To download the HICbioclean package from github, you will first have to install the package devtools if you don't already have it.
Copy the following code into the R consol and press enter: <br>
***install.packages("devtools")***

Install the HICbioclean package into R.
Copy the following code into the R consola nd press enter: <br>
***devtools::install_github("pgelsomini/HICbioclean", build_vignettes = TRUE)***

Open the package library for HICbioclean.
Copy the following code into the R consola nd press enter: <br>
***library(HICbioclean)***