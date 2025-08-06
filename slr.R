# tutorial: https://www.r-bloggers.com/2023/03/automated-systematic-literature-search-using-r-litsearchr-and-google-scholar-web-scraping/
packages <- c("easyPubMed",
  "litsearchr", "stopwords", "igraph",
  "ggplot2", "ggraph", "ggrepel"
)
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Load packages
lapply(packages, library, character.only = TRUE)


## Need to prepare the preliminary search and we continue with litsearchr
