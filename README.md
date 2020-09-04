# HICbioclean

Written and developed by Pali Felice Gelsomini with support from Tom Maris of the Ecosystem Management Research Group (ECOBE) University of Antwerp. Funded by De Vlaamse Waterweg (Flemish Waterways) for the Hydrological Information Center (HIC) of Flanders Hydraulics Research.

An R package specifically designed for automating the process of cleaning, calibrating and validating continuous biological water quality data from the Flemish HIC (Hydrological Information Centre) database.

It provides both R functions for integration into R scripts and easy to use R Shiny graphical apps for intuitive data cleaning, validation and calibration without the need for coding.

Note: In this version of the package, the only dummy codes are provided for import into the HIC database system.

## Installation

Download the latest version of R from https://cran.r-project.org/ if not already installed.

Open the program R.

To download the HICbioclean package from github, you will first have to install the package devtools if you don't already have it.
Copy the following code into the R console and press enter: <br>
***install.packages("devtools")***

Install the HICbioclean package into R.
Copy the following code into the R console and press enter: <br>
***devtools::install_github("pgelsomini/HICbioclean", build_vignettes = TRUE)***

Open the package library for HICbioclean.
Copy the following code into the R console and press enter: <br>
***library(HICbioclean)***

See MANUAL-TOTORIAL-HICbioclean-Rpackage.pdf for instructions on its use.
