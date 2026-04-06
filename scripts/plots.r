library(ggplot2)
setwd(dir = "/Users/tseng/Documents/slr-haptic-collaboration/results/")
dta <- read.table(file = "results.csv", sep = ",", h = T)
hist(dta$cleaned_year)

papers.by.year <- ggplot(dta, aes(x=cleaned_year)) +
  geom_histogram(fill = "lightblue", color="gray50", 
                 position='identity',
                 binwidth=1, alpha=0.8) +
  labs(x="Year", y="Paper count") +
  scale_x_continuous(breaks=seq(2000, 2025, by = 5)) +
  theme_bw()
  # + theme(axis.text.x = element_text(face="bold", color="#993333", 
  #                                    size=14, angle=45),
  #         axis.text.y = element_text(face="bold", color="#993333", 
  #                                    size=14, angle=45))

ggsave("papers-by-year.pdf", papers.by.year,
       width = 24.89, height = 15, units = "cm", dpi = 300)

level_order <- names(sort(table(dta$venue_type)))
dta$venue_type <- factor(dta$venue_type, levels = level_order)

dta %>% group_by(venue_type) %>% filter(n() >= 2) %>%
  ggplot(aes(x=venue_type)) +
    geom_bar(fill="steelblue") +
    theme_bw() +
    coord_flip()

sort(table(dta$source))


library(data.table)
dta_code <- fread("results-temp.csv", h=T)
str(dta_code)

lapply(dta_code, function(x) sort(table(x)))

# https://github.com/davidsjoberg/ggsankey
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(metan)

colnames(dta_code)[16] <- "User_Rep"

d <- mtcars %>%
  make_long(cyl, vs)

d <- dta_code %>%
  make_long(Time, Space, Scale, Symmetry_role, Focus, Scenario, Interaction, User_Rep)

# ggplot(d, aes(x = x, 
#                next_x = next_x, 
#                node = node, 
#                next_node = next_node,
#                fill = factor(node))) +
#   geom_sankey() +
#   scale_fill_discrete(drop=FALSE)

collaboration.codes <- ggplot(d, aes(x = x, next_x = next_x, node = node, next_node = next_node,
               fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Collaboration")

ggsave("collaboration-codes.pdf", collaboration.codes,
       width = 24.89, height = 15, units = "cm", dpi = 300)
