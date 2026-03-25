library(ggplot2)

dta <- read.table(file = "report-years-and-venues.csv", sep = ",", h = T)
hist(dta$cleaned_year)

ggplot(dta, aes(x=cleaned_year)) +
  geom_histogram(fill = "lightblue", color="gray50", 
                 position='identity',
                 binwidth=2, alpha=0.8) +
  labs(x="Year", y="Paper count") +
  theme_bw()

sort(table(dta$source))

# https://github.com/davidsjoberg/ggsankey
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

 