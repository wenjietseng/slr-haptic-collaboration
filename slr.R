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



#' Testing Scraping from google scholars
#' to do:
#' 1. a set of proxies
#' 2. a set of user agents
#' 3. random delays (45-120 secs)
#' 4. backoff on CAPTCHAs (pause for an hour)

### the function
# useragent <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") # spoof user agent
# proxy <- httr::use_proxy(url = "proxy.com", port = 8080, username = "dave", password = "pass", auth = "basic")

## 1) Read an html page with urls: input strings and polite
## 2) Scrap useful information from the html data. In our case, title, authors, year, citation, venue?
## 3) Use regular expressions to parse the scraped html data.
## 4) Store the data, consder tibble? json?


gs_url_base <- "https://scholar.google.com/scholar"
gs_test_url <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%22collaboration%22+%22haptic%22+%22virtual+reality%22&oq="
result_list <- list()


wbpage <- rvest::read_html(gs_test_url)
rvest::html_elements(wbpage, ".gs_ab_mdw")
rvest::html_elements(wbpage, ".gs_rt")
rvest::html_elements(wbpage, ".gs_a")
rvest::html_elements(wbpage, ".gs_rs")
bottom_row_nodes <- rvest::html_elements(wbpage, ".gs_fl")
bottom_row_nodes <- bottom_row_nodes[!grepl("gs_ggs gs_fl", as.character(bottom_row_nodes), fixed = TRUE)] # exclude the ones with this tag, they are download links 
bottom_row <- rvest::html_text(bottom_row_nodes)


session <- bow("https://www.cheese.com/by_type", force = TRUE)
result <- scrape(session, query=list(t="semi-soft", per_page=100)) %>%
  html_node("#main-body") %>% 
  html_nodes("h3") %>% 
  html_text()
head(result)

### testing polite, not super useful for scraping on GS
session <- bow(gs_test_url,
               user_agent = "WJTS")

session |> scrape()

## <polite session> https://en.wikipedia.org/wiki/AFC_Asian_Cup_records_and_statistics
##     User-agent: Ryo's R Webscraping Tutorial
##     robots.txt: 454 rules are defined for 33 bots
##    Crawl delay: 5 sec
##   The path is scrapable for this user-agent

### Testing litsearchr
# https://rdrr.io/github/elizagrames/litsearchr/f/vignettes/litsearchr_vignette.rmd


### create my own naive search terms first
# one ris is based on the final list
# another is based on the naive search on ACM DL,
# collaborative virtual environment AND haptic AND virtual reality
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

c(search)
write.table(as.data.frame(sort(search_terms)), "./search_terms/2409.csv",
            row.names = FALSE, col.names = FALSE,
            sep = ",")
