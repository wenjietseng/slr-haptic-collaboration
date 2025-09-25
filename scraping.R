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
