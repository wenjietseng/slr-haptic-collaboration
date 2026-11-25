library(ggplot2) 
library(unikn)
library(hrbrthemes)
library(RColorBrewer)


dvs <- data.frame(
  Dimension = c(rep("Performance",4), rep("Behavioral", 4), rep("Self-reports",10)),
  Measure = c( "Time", "Precision/Error", "Score", "Effort/Efficiency",
               "Force applied", "Position/Movements", "Social interaction", "Physiological responses",
               "User experience", "Social presence", "Collaboration", "Preference", 
              "Haptic experience", "Workload", "Virtual presence", "Perceived performance",
              "Perception", "Affective experience"),
  Scale = c(44,33,23,21,
            11,8,3,1,
            31,16,10,9,8,8,7,6,2,2)
)

dvs$Dimension <- factor(dvs$Dimension, levels=c("Performance","Self-reports","Behavioral"))
dvs$Measure <- factor(dvs$Measure, levels=c("Effort/Efficiency" , "Score",
                                            "Precision/Error","Time", "Affective experience", "Perception", "Perceived performance","Virtual presence","Workload"
                                            , "Haptic experience", "Preference", "Collaboration","Social presence",
                                            "User experience",  "Physiological responses", "Social interaction", 
                                            "Position/Movements", "Force applied"))

ggplot(dvs, aes(fill=Measure, y=Scale, x=Measure)) +
  facet_wrap(~Dimension) +
  scale_fill_manual(values = c("Time"="#7FC97F", "Precision/Error"="#BEAED4", "Score"="#FDC086", "Effort/Efficiency"="#FFFF99",
                               "Force applied"="#7FC97F", "Position/Movements"="#BEAED4", "Social interaction"="#FDC086", "Physiological responses"="#FFFF99",
                               "User experience"="#80B1D3", "Social presence"="#FBB4AE", "Collaboration"="#FA9FB5", "Preference"="#F2F2F2",
                               "Haptic experience"="#80CDC1", "Workload"="#E5C494", "Virtual presence"="#7FC97F", "Perceived performance"= "#BEAED4",
                               "Perception"="#FDC086", "Affective experience"="#FFFF99")) +
  geom_bar(stat="identity") +
  theme_ipsum(base_size = 18) +
  xlab("") + ylab("")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())