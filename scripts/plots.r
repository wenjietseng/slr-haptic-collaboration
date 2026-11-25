##########################################
# Setup
##########################################
library(ggplot2)
library(gridExtra)
library(data.table)
library(forcats)
# https://github.com/davidsjoberg/ggsankey
# devtools::install_github("davidsjoberg/ggsankey")
library(metan)
library(ggsankey)

# dir
setwd(dir = "/Users/tseng/Documents/slr-haptic-collaboration/results/")
##########################################
# Paper Overview
##########################################
dta <- read.table(file = "venues.csv", sep = ",", h = T)

# years
hist(dta$cleaned_year)
papers.by.year <- ggplot(dta, aes(x=cleaned_year)) +
  geom_histogram(fill = "gray80", color="gray50", 
                 position='identity',
                 binwidth=1, alpha=0.8) +
  labs(x="Year", y="Count") +
  scale_x_continuous(breaks=seq(2000, 2025, by = 5)) +
  theme_bw()

# venue
plot.venue <- dta$venue_type[dta$venue_type %in% 
                 names(which(table(dta$venue_type) >=2))]
level_order <- names(sort(table(plot.venue)))
plot.venue <- factor(plot.venue, levels = level_order)
sort(table(plot.venue))

papers.by.venue <- plot.venue |> data.frame() |> ggplot(aes(x=plot.venue)) +
  geom_bar(fill="gray80") +
  labs(x="Venue", y="Count") +
  theme_bw() +
  coord_flip()

my_theme <- theme(
  axis.text=element_text(size=12),
  axis.title=element_text(size=12),
  plot.margin = unit(c(.2, .5, .2, .5), "cm"))

combined_plots <-
  grid.arrange(arrangeGrob(
    papers.by.venue + my_theme,
    papers.by.year + my_theme, nrow=1, widths=c(3, 2)))

# + theme(axis.text.x = element_text(face="bold", color="#993333", 
#                                    size=14, angle=45),
#         axis.text.y = element_text(face="bold", color="#993333", 
#                                    size=14, angle=45))

ggsave("papers-overview.pdf", combined_plots,
       width = 24, height = 10, units = "cm", dpi = 300)

##########################################
# Code Book
##########################################
# dta_code <- fread("results-temp.csv", h=T)
dta_code <- fread("Codebook (haptic collaboration) - extracted data.csv", h=T)

str(dta_code)
lapply(dta_code[,c(16, 18:31)], function(x) sort(table(x)))

# table(dta_code$C1.1_Time)
# table(unlist(strsplit(dta_code$C4.3_Limitation_code, ", ")))

# d <- mtcars %>%
#   make_long(cyl, vs)
# ggplot(d, aes(x = x,
#                next_x = next_x,
#                node = node,
#                next_node = next_node,
#                fill = factor(node))) +
#   geom_sankey() +
#   scale_fill_discrete(drop=FALSE)


###
colnames(dta_code)[6:10] <- c("C1.1 Time", "C1.2 Space", "C1.3 Scale",
                              "C1.7 Symmetry (role)", "C1.5 Focus")
R1_collaboration <- dta_code %>%
  make_long(`C1.1 Time`, `C1.2 Space`, `C1.3 Scale`, `C1.7 Symmetry (role)`, `C1.5 Focus`)

collaboration.codes <- ggplot(R1_collaboration, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                         fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 14) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) #+  ggtitle("Collaboration Properties")
collaboration.codes

ggsave("collaboration-codes.pdf", collaboration.codes,
       width = 24.89, height = 15, units = "cm", dpi = 300)


###
colnames(dta_code)
colnames(dta_code)[c(12, 13, 15, 18)] <- c("C1.4 Display",
                                           "C1.7 Task goal",
                                           "C1.8 Interaction",
                                           "C1.9 User representation")
R1_interaction <- dta_code %>%
  make_long(`C1.4 Display`,
            `C1.7 Task goal`,
            `C1.8 Interaction`,
            `C1.9 User representation`)

interaction.codes <- ggplot(R1_interaction,
                            aes(x = x, next_x = next_x, node = node,
                                next_node = next_node,
                                fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 14) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) #+  ggtitle("Collaboration Properties")
interaction.codes

ggsave("interaction-codes.pdf", interaction.codes,
       width = 24.89, height = 15, units = "cm", dpi = 300)



###
colnames(dta_code)[c(20, 21, 22, 23, 27, 28, 29, 30)] <- c(
  "C2.1 Device", "C2.2 Installation", "C2.3 Location",
  "C2.4 Feedback", "C2.5 Purpose", "C2.5 Symmetry (feedback)",
  "C2.6 Direction", "C2.6 From what")

haptic_setup <- dta_code %>% 
  make_long(`C2.1 Device`, `C2.2 Installation`, `C2.3 Location`,
            `C2.4 Feedback`, `C2.5 Purpose`, `C2.6 From what`)

haptic.setup.codes <- ggplot(haptic_setup,
                            aes(x = x, next_x = next_x, node = node,
                                next_node = next_node,
                                fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 12) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) #+  ggtitle("Collaboration Properties")
haptic.setup.codes
ggsave("haptic-setup-codes.pdf", haptic.setup.codes,
       width = 24.89, height = 15, units = "cm", dpi = 300)


symmetry <- dta_code |>
  make_long(`C1.7 Symmetry (role)`, `C2.5 Symmetry (feedback)`, `C2.6 Direction`)

symmetry.codes <- ggplot(symmetry,
                             aes(x = x, next_x = next_x, node = node,
                                 next_node = next_node,
                                 fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 5, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 16) +
  labs(x =  NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) #+  ggtitle("Collaboration Properties")
symmetry.codes
ggsave("symmetry.pdf", symmetry.codes,
       width = 24.89, height = 15, units = "cm", dpi = 300)


dta_code$`C1.1 Time` <- dta_code$`C1.1 Time` |>
  fct_infreq()
dta_code$`C1.1 Time`

test <- dta_code |> 
  make_long(`C1.1 Time`, `C1.2 Space`, `C1.3 Scale`, `C1.4 Display`)


factor(dta_code$`C1.1 Time`, levels = )
names(sort(table(dta_code$`C1.1 Time`)))


# test <- dta_code %>% 
#   make_long(`C2.1 Device`, `C2.4 Feedback`, `C1.8 Interaction`, C3.2_Motivation)


new4.2 <- ggplot(test,
       aes(x = x, next_x = next_x, node = node,
           next_node = next_node,
           fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 5, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 16) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

new4.2
ggsave("for4-2.pdf", new4.2,
       width = 24.89, height = 15, units = "cm", dpi = 300)


###
evaluation <- dta_code |>
  make_long(C3.1_Study_motivation, C3.2.2_Is_Haptic_feedback_an_IV_or_constant)


### IVs
IVs <- sort(table(unlist(strsplit(dta_code$C3.2_IV, ", "))))
level_order_IVs <- names(IVs)
IVs.plot <- factor(unlist(strsplit(dta_code$C3.2_IV, ", ")), levels = level_order)

ggplot(as.data.frame(IVs.plot), aes(x=IVs.plot)) +
  geom_bar(fill="gray70") +
  labs(x="IV_Code", y="Count") +
  theme_bw() + coord_flip()


### DVs performance
DVs_perf <- sort(table(
  unlist(strsplit(unlist(strsplit(unlist(strsplit(dta_code$`C3.3.1_DVs: Code Performance etc`, "\n")), ": ")),", "))
  ))
level_order_DVs_perf <- names(DVs_perf)
DVs.perf.plot <- factor(unlist(strsplit(
  unlist(strsplit(
    unlist(strsplit(dta_code$`C3.3.1_DVs: Code Performance etc`, "\n")), ": ")),", ")),
  levels = level_order_DVs_perf)

ggplot(as.data.frame(DVs.perf.plot), aes(x=DVs.perf.plot)) +
  geom_bar(fill="gray70") +
  labs(x="DVs performance code", y="Count") +
  theme_bw() + coord_flip()


### DVs experience
DVs_exp <- sort(table(
  unlist(strsplit(dta_code$`C3.3.2_DVs: Code self-reports`, ", "))
  ))
level_order_DVs_exp <- names(DVs_exp)
DVs.exp.plot <- factor(unlist(strsplit(dta_code$`C3.3.2_DVs: Code self-reports`, ", ")),
                       levels = level_order_DVs_exp)

ggplot(as.data.frame(DVs.exp.plot), aes(x=DVs.exp.plot)) +
  geom_bar(fill="gray70") +
  labs(x="DVs experience code", y="Count") +
  theme_bw() + coord_flip()

## sample size
sort(table(dta_code$C3.6_Sample_size))
dta_code$C3.6_Sample_size[which(dta_code$C3.6_Sample_size == "20 (in both)")] <- 10
dta_code$C3.6_Sample_size[which(dta_code$C3.6_Sample_size == "24 for each study")] <- 24
dta_code$C3.6_Sample_size[which(dta_code$C3.6_Sample_size == "not precised")] <- 1
dta_code$C3.6_Sample_size[which(dta_code$C3.6_Sample_size == "6+15")] <- 21
sort(table(dta_code$C3.6_Sample_size))
dta_code$C3.6_Sample_size <- as.numeric(dta_code$C3.6_Sample_size)

ggplot(dta_code, aes(x=0, y=C3.6_Sample_size)) +
  geom_boxplot(fill="gray90", width=0.5) +
  labs(y="Sample size", x="") +
  xlim(-1,1) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  
## by study and evaluation
study_motiv <- as.data.frame(t(matrix(unlist(strsplit(dta_code$C3.1_Study_motivation, ": ")), nrow = 2, ncol = 84)))
colnames(study_motiv) <- c("Goal", "Type")
study_motiv$C3.6_Sample_size <- dta_code$C3.6_Sample_size

study_motiv_mean <- as.data.frame(with(study_motiv, tapply(C3.6_Sample_size, Goal, FUN=mean)))
colnames(study_motiv_mean) <- c("Sample_size_m")
study_motiv_mean$Goal <- rownames(study_motiv_mean)
study_motiv_mean

ggplot(study_motiv, aes(y=C3.6_Sample_size, group=Goal)) +
  geom_boxplot(aes(fill=Goal), width=0.5, alpha=.7) +
  geom_hline(data=study_motiv_mean, aes(yintercept = Sample_size_m, color=Goal), linetype="dashed") +
  labs(y="Sample size", x="") +
  xlim(-1,1) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.85, .85))

sort(table(dta_code$C3.6.1_Sample_size_description))
table(study_motiv$Goal)


library(paletteer)

study_motiv$Type <- reorder(study_motiv$Type, study_motiv$C3.6_Sample_size, median)
ggplot(study_motiv, aes(y=C3.6_Sample_size, group=Type)) +
  geom_boxplot(aes(fill=Type), width=.8) +
  labs(y="Sample size", x="") +
  xlim(-.8,.8) +
  theme_bw() +
  scale_fill_paletteer_d ("ggsci::nrc_npg") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.86, .70))
# scale_fill_discrete(paletteer_d("ggsci::light_blue_material") ) + 
### Concluding statements
CS <- sort(table(
  unlist(strsplit(dta_code$J_CS_code, ", "))
  ))

level_order_CS <- names(CS)
CS.plot <- factor(unlist(strsplit(dta_code$J_CS_code, ", ")),
                  levels = level_order_CS)

ggplot(as.data.frame(CS.plot), aes(x=CS.plot)) +
  geom_bar(fill="gray70") +
  labs(x="Concluding statement code", y="Count") +
  theme_bw() + coord_flip()

### Benefits
Benefits <- sort(table(dta_code$C4.2_Benefit_code))
level_order_benefits <- names(Benefits)
Benefits.plot <- factor(dta_code$C4.2_Benefit_code,
                  levels = level_order_benefits)
ggplot(as.data.frame(Benefits.plot), aes(x=Benefits.plot)) +
  geom_bar(fill="gray70") +
  labs(x="Benefits code", y="Count") +
  theme_bw() + coord_flip()

### Limits
Limits <- sort(table(unlist(strsplit(dta_code$C4.3_Limitation_code, ", "))))
level_order_limits <- names(Limits)
Limits.plot <- factor(unlist(strsplit(dta_code$C4.3_Limitation_code, ", ")),
                        levels = level_order_limits)
ggplot(as.data.frame(Limits.plot), aes(x=Limits.plot)) +
  geom_bar(fill="gray70") +
  labs(x="Limitations code", y="Count") +
  theme_bw() + coord_flip()


### Future works
FW <- sort(table(
  unlist(strsplit(dta_code$C4.4_Future_work_code, ", "))
  ))
level_order_FW <- names(FW)
FW.plot <- factor(unlist(strsplit(dta_code$C4.4_Future_work_code, ", ")),
                        levels = level_order_FW)
ggplot(as.data.frame(FW.plot), aes(x=FW.plot)) +
  geom_bar(fill="gray70") +
  labs(x="Future work code", y="Count") +
  theme_bw() + coord_flip()



#################
## try with networkD3
library(networkD3)
# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

link <- data.frame(
  source = c(rep("Off-the-shelf", 4), rep("Customized", 3)),
  target = c("Kinesthetic", "Vibrotactile", "K+V", "Tactile",
             "Kinesthetic", "Vibrotactile","Passive"),
  value = c(49, 3, 3, 2, 10, 9, 7)
)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(link$source), 
         as.character(link$target)) |> unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
link$IDsource <- match(link$source, nodes$name)-1 
link$IDtarget <- match(link$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = link, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p


