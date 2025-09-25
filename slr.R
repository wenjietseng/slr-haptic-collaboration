## packages
packages <- c("easyPubMed", "dplyr",
              "litsearchr", "stopwords", "igraph",
              "ggplot2", "ggraph", "ggrepel",
              "rvest", "polite", "remotes", "httr2"
)
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Load packages
lapply(packages, library, character.only = TRUE)

## Testing litsearchr
# https://rdrr.io/github/elizagrames/litsearchr/f/vignettes/litsearchr_vignette.rmd
# https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html#Writing_a_new_search
# Naive search term is:
# ("collaborative virtual environment" OR
## "shared virtual environment" OR "remote collaboration") AND "haptic"
search_directory <- "~/Documents/slr-haptic-collaboration/naive_search_0923/"
naive_import <- litsearchr::import_results(search_directory, verbose = TRUE)
naive_results <- litsearchr::remove_duplicates(naive_import, field = "title", method = "string_osa")
table(naive_import$filename)
table(naive_results$filename)

## let's keep conference and journals
ggplot(naive_results, aes(x=source_type)) +
  geom_bar()

dta <- filter(naive_results, source_type == "JOUR" | source_type == "CONF")

dim(naive_import)
dim(naive_results)
dim(dta)

is.na(dta$year)
dta$cleaned_year <- ifelse(is.na(dta$year), substr(dta$date_generated, start = 1, stop = 4), dta$year)

ggplot(dta, aes(x=cleaned_year)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = -60))

## Check: if we cover the final list
final_list_import <- litsearchr::import_results("./final_list/", verbose = TRUE)
sum((final_list_import$title) %in% (dta$title))
length(final_list_import$title) # with the naive search, we only cover 1/3 of the final list.
final_list_import$title[(final_list_import$title) %in% (dta$title)] 

## extract keywords from titles and abstracts using the fakerake algorithm.
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(dta$title, dta$abstract),
    method = "fakerake",
    min_freq = 5, # if min_freq equals (3,4,5), the algorithm extracts (686,355,226) keywords.
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )
rakedkeywords

## unused code
# lapply(final_list$issn, litsearchr::clean_keywords) |>
#   lapply(function(x) {strsplit(x, "[,;]")}) |>
#   unlist() |>
#   tolower() |> table()

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = dta$keywords,
    method = "tagged",
    min_freq = 4, # if min_freq equals (2,3,4), the algorithm extracts (212,85,54) keywords.
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )
taggedkeywords

all_keywords <- unique(append(taggedkeywords, rakedkeywords))
all_keywords # 251

### co‐occurrence  network
# an example to show how this work
dfm <- create_dfm(
  elements = c(
    "Cross-scale occupancy dynamics of a postfire specialist 
    in response to variation across a fire regime", # data 1
    "Variation in home-range size of Black-backed Woodpeckers", # data 2
    "Black-backed woodpecker occupancy in burned and beetle-killed forests" # data 3
  ),
  features = c("occupancy", "variation", "black-backed woodpecker", "burn")
)
as.matrix(dfm)
# which keywords co-occur in the same title or abstract, create a matrix[text, keywords]

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(dta$title, dta$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 3,
    min_occ = 3
  )

str(naivegraph)
plot(naivegraph)

plot(sort(igraph::strength(naivegraph)),
     ylab = "Node strength",
     xlab = "Rank",
     main = "Ranked node strengths",
     type = "l",
     lwd = "3",
     col = "steelblue"
     )


cutoff_medium <-
  find_cutoff(graph=naivegraph, method = "cumulative",
              percent = 0.8,
              imp_method = "strength"
  ) 
cutoff_medium

cutoff_core <-
  find_cutoff(graph=naivegraph, method = "changepoint",
              knot_num = 3,
              imp_method = "strength"
  )
cutoff_core

plot(sort(igraph::strength(naivegraph)),
     ylab = "Node strength",
     xlab = "Rank",
     main = "Ranked node strengths",
     type = "l",
     lwd = "3",
     col = "steelblue"
)
abline(h = cutoff_core, col="red")
abline(h = cutoff_medium, col="red", lty = 3, lwd = 2)

reduced_graph_core <- reduce_graph(naivegraph, cutoff_strength = cutoff_core[1])
plot(reduced_graph_core)
search_terms_core <- get_keywords(reduced_graph_core)
search_terms_core
## compare to the naive search we used ("collaborative virtual environment" OR
## "shared virtual environment" OR "remote collaboration") AND "haptic" 
## the core keywords include VR, AR, MR, and XR.

## now we check the medium strength keywords.
reduced_graph_medium <- reduce_graph(naivegraph, cutoff_strength = cutoff_medium)
plot(reduced_graph_medium)
search_terms_medium <- get_keywords(reduced_graph_medium)
search_terms_medium

idx <- sapply(search_terms_core, function(x) {
  which(search_terms_medium == x)  
})
search_terms_medium <- search_terms_medium[-idx]

keywords <- c(search_terms_core, search_terms_medium)
keyword_type <- c(rep("core", length(search_terms_core)), rep("medium", length(search_terms_medium)))

write.table(data.frame(keywords, keyword_type), "./search_terms/2409.csv",
            row.names = FALSE, col.names = TRUE,
            sep = ";")
