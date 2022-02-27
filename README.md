
Project structure:

- Open AgeSR.Rproj to run everything through the project

- Input: contains raw dataset files we extracted data from, p-curve output and focal test .rtf document needed for the p-checker analysis. (note: only datasets that we are allowed to share publicly are available in this directory, but the code for our transformations is still available in the scripts)

- Analyses:
  - ***coding_comparison_Lippens.R***: this file contains data extractions used to compare to coding conducted in the new Lippens et al. (2021) meta-analysis and has coding different than the one used for our meta-analyses
  
  -  ***extraction_source.R***: contains actual wrangling of extracted data to get the form needed to calculate effect sizes, this file is sourced in the data_extraction.Rmd and Results.Rmd
    
  - ***data_extraction.Rmd***: contains information on how we extracted data from each article (either from article summary statistics or raw datasets when available) and focal tests we extracted from each article
  
  - ***Results.Rmd***: markdown file containing the code for all effect size calculations, conducting meta-analyses and reporting results and creating forest plots (bs_plot.pdf, ws_plot.pdf)
  

- The project scripts ran under R version 4.1.2 and packages: tidyverse 1.3.1, haven 2.4.3, metafor 3.1-45, here 1.0.1, papaja 0.1.0.9997, knitr 1.37
