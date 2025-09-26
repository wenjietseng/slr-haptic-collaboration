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
search_directory <- "~/Documents/slr-haptic-collaboration/polished_search/"
naive_import <- litsearchr::import_results(search_directory, verbose = TRUE)
naive_results <- litsearchr::remove_duplicates(naive_import, field = "title", method = "string_osa")
table(naive_import$filename)
table(naive_results$filename)

## let's keep conference and journals
ggplot(naive_results, aes(x=source_type)) +
  geom_bar()

dta <- filter(naive_results, source_type == "JOUR" | source_type == "CONF")

dim(naive_import) # 396 
dim(naive_results) # remove duplicate papers 388
dim(dta) # remove book and chapters 380

is.na(dta$year)
dta$cleaned_year <- ifelse(is.na(dta$year), substr(dta$date_generated, start = 1, stop = 4), dta$year)

ggplot(dta, aes(x=cleaned_year)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = -60))

## Check: if we cover the final list
golden_standard_import <- litsearchr::import_results("./golden_standard/", verbose = TRUE)
sum(tolower(golden_standard_import$title) %in% tolower(dta$title))
length(golden_standard_import$title) # 16/19
golden_standard_import$title[!(tolower(golden_standard_import$title) %in% tolower(dta$title))] 

# in 10 years
dta_10yrs <- dta[(as.numeric(dta$cleaned_year) > 2014),]
dim(dta_10yrs)

# title
sum(is.na(dta$title))

# abstract
dta[which(is.na(dta$abstract)),]
sum(is.na(dta$abstract))

# year
sum(is.na(dta$cleaned_year))

# authors
sum(is.na(dta_10yrs$author))

# published where?
sum(is.na(dta$source))
dta$source[which(is.na(dta$source))] <- dta$proceedings_title[which(is.na(dta$source))]
dta$source[which(is.na(dta$source))] <- dta$journal[which(is.na(dta$source))]

# link: if url is missing, replace with doi
sum(is.na(dta$doi))
dta$link <- ifelse(str_starts(dta$doi, "https://doi.org/"), dta$doi, paste0("https://doi.org/", dta$doi))

library(tidyverse) # we user stringr
dta |> select(title, abstract, cleaned_year, author, source, link) |>
  write.table("./screening/2609.csv",
              row.names = FALSE, col.names = TRUE,
              sep = ";")



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
    min_freq = 3, # if min_freq equals (2,3,4), the algorithm extracts (212,85,54) keywords.
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )
taggedkeywords

all_keywords <- unique(append(taggedkeywords, rakedkeywords))
all_keywords # 297

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

ggraph(naivegraph, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha="none")


# pruning
strengths <- strength(naivegraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE) +
  theme_bw()

cutoff_fig

# cumulative method
cutoff_cum <-
  find_cutoff(graph=naivegraph, method = "cumulative",
              percent = 0.8) 
cutoff_cum
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

get_keywords(reduce_graph(naivegraph, cutoff_cum)) # 136 keywords

# changepoint method
cutoff_change <-
  find_cutoff(graph=naivegraph, method = "changepoint",
              knot_num = 3)
cutoff_change
cutoff_fig +
  geom_hline(yintercept = cutoff_change, linetype = "dashed")

get_keywords(reduce_graph(naivegraph, cutoff_change[1])) # 31 keywords

# plot(sort(igraph::strength(naivegraph)),
#      ylab = "Node strength",
#      xlab = "Rank",
#      main = "Ranked node strengths",
#      type = "l",
#      lwd = "3",
#      col = "steelblue"
#      )
# abline(h = cutoff_core, col="red")
# abline(h = cutoff_medium, col="red", lty = 3, lwd = 2)


reduced_graph_core <- reduce_graph(naivegraph, cutoff_change[1])

ggraph(reduced_graph_core, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-4.5, 3.5)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha="none")

search_terms_core <- get_keywords(reduced_graph_core)
search_terms_core
## compare to the naive search we used ("collaborative virtual environment" OR
## "shared virtual environment" OR "remote collaboration") AND "haptic" 
## the core keywords include VR, AR, MR, and XR.

## now we check the medium strength keywords.
reduced_graph_medium <- reduce_graph(naivegraph, cutoff_cum)
ggraph(reduced_graph_medium, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha="none")
search_terms_medium <- get_keywords(reduced_graph_medium)
search_terms_medium

idx <- sapply(search_terms_core, function(x) {
  which(search_terms_medium == x)  
})
search_terms_medium <- search_terms_medium[-idx]

keywords <- c(search_terms_core, search_terms_medium)
keyword_type <- c(rep("core", length(search_terms_core)), rep("medium", length(search_terms_medium)))

write.table(data.frame(keywords, keyword_type), "./search_terms/2509.csv",
            row.names = FALSE, col.names = TRUE,
            sep = ";")
