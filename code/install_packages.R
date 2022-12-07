# install packages required for training

# tyler jackson
# 11/28/2022

## Rmarkdown / bookdown training
packages <- c("tidyverse", "knitr", "xtable", "kable", "kableExtra", "rmarkdown", "bookdown", "patchwork", "cowplot", "bbmle", "tinytex")
install.packages(setdiff(packages, rownames(installed.packages())))  
# install latex distribution
tinytex::install_tinytex()