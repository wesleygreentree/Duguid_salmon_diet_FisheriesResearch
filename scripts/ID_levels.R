# Barplot of broad prey categories at different identification levels to
# look at usefulness of diagnostic hard parts/decision rules

# load packages
library(readxl)
library(tidyverse)
library(reshape2)
library(vegan)

# load data
salmon.data <- read_excel("data/ASDPsalmon_v1.0.xlsx")
stomach.data <- read_excel("data/ASDPstomachs_v1.0.xlsx")

# stomachs to remove before pooled % weight analysis
stomach.rm <- read_excel("data/ASDP_stomachs_to_remove.xlsx") 

## Process salmon data ---
nrow(salmon.data) # 3472
salmon.data <- select(salmon.data, `Fish Code`, Species)

# stomach prey categories ---

# OriginalID (using level of target taxonomic levels)
sort(unique(stomach.data$OriginalID))
stomach.data$OriginalHighest <- rep(NA, nrow(stomach.data))

stomach.data$OriginalHighest[stomach.data$OriginalID %in% c("herring", "anchovy", "northern anchovy", "myctophid",
                                                            "surfperch", "sand lance", "gadid", "pacific cod", "walleye pollock",
                                                            "pacific hake", "rockfish", "threespine stickleback", "eulachon",
                                                            "surf smelt", "chinook salmon", "salmon", "flatfish",
                                                            "barracudina", "prowfish", "lingcod", "sablefish",
                                                            "poacher", "tubesnout")] <- "Identified Fish"
stomach.data$OriginalHighest[stomach.data$OriginalID == "unidentified fish"] <- "Unidentified Fish"

stomach.data$OriginalHighest[stomach.data$OriginalID %in% c("amphipod", "gammarid amphipod", "brachyuran megalopa", 
                                                            "cancrid megalopa", "dungeness crab megalopa", "brachyuran zoea",
                                                            "cancrid zoea", "pandalid", "pandalus sp.", "pandalus sp",
                                                            "pasiphaea pacifica", "pasiphaeid", "mysid", 
                                                            "eusergestes similis", "sergestid", "euphausiid", "isopod",
                                                            "pagurid megalopa", "pinnotherid", "neotrypaea sp.", 
                                                            "insect", "squid", "cephalopod", "unidentified cephalopod",
                                                            "polychaete", "brachyuran zoea and  megalopae")] <- "Identified Invertebrate"

stomach.data$OriginalHighest[stomach.data$OriginalID %in% c("unidentified crustacean", "unidentified brachyuran", 
                                                            "brachyuran larvae","brachyuran", "unidentified brachyuran larvae",
                                                            "euphausiid and cancrid", "unidentified shrimp",  
                                                            "unidentified invertebrate")] <- "Unidentified Invertebrate"

stomach.data$OriginalHighest[stomach.data$OriginalID %in% c("other", "pebble", "unidentified plant",
                                                            "unidentified material")] <- "Unidentified Material"
stomach.data$OriginalHighest[stomach.data$OriginalID == "empty"] <- "Empty"

subset(stomach.data,is.na(OriginalHighest)) # 0 good

## Likely assignment
stomach.data$Likely <- rep(NA, nrow(stomach.data))
sort(unique(stomach.data$FinalID))

stomach.data$Likely[stomach.data$FinalID %in% c("herring", "anchovy", "northern anchovy", "myctophid", "surfperch",
                                                "sand lance", "gadid", "pacific cod", "walleye pollock",
                                                "hake", "pacific hake", "rockfish", "threespine stickleback",
                                                "surf smelt", "eulachon", "deep-sea smelt", "northern smoothtongue",
                                                "salmon", "chinook salmon", "chum salmon", "pink salmon", "prowfish",
                                                "barracudina", "jack mackerel", "pacific saury", "flatfish", "larval flat fish",
                                                "sablefish", "lingcod", "staghorn sculpin", "poacher", "midshipman",
                                                "tubesnout")] <- "Identified Fish"
stomach.data$Likely[stomach.data$FinalID %in% c("unidentified fish", "gasterosteiformes")] <- "Unidentified Fish"

stomach.data$Likely[stomach.data$FinalID %in% c("amphipod", "caprellid", "metacaprella", "gammarid amphipod",
                                                "hyperiid amphipod", "hyperia medusarum", "themisto pacifica",
                                                "primno", "brachyuran megalopa", "cancrid megalopa", "dungeness crab megalopa",
                                                "brachyuran zoea", "cancrid zoea", "caridean shrimp", "crangonidae", "pandalus sp.",
                                                "pasiphaea pacifica", "sergestid", "eusergestes similis", "euphausiid",
                                                "isopod", "mysid", "copepod",  "pagurid megalopa", "neotrypaea sp.",
                                                "brachyuran zoeae and megalopae", "pinnotherid", "insect",
                                                "octopus", "squid", "stubby squid", "unidentified cephalopod",
                                                "polychaete")] <- "Identified Invertebrate" 
stomach.data$Likely[stomach.data$FinalID %in% c("unidentified shrimp", "unidentified brachyuran larvae",
                                                "unidentified crustacean", "brachyuran", "decapod")] <- "Unidentified Invertebrate"

stomach.data$Likely[stomach.data$FinalID %in% c("algae", "other", "gastropod", "unidentified material", 
                                                "zostera spp.")] <- "Unidentified Material" # includes a small number of other
stomach.data$Likely[stomach.data$FinalID == "empty"] <- "Empty"

subset(stomach.data, is.na(Likely)) # 0

view(stomach.data %>% group_by(Likely, FinalID) %>% tally()) # 79

## Confirmed = Yes level 
stomach.data$ConfY <- rep(NA, nrow(stomach.data))
sort(unique(stomach.data$FinalID))
sort(unique(stomach.data$Confidence))

stomach.data$ConfY[stomach.data$Likely == "Identified Fish" &
                   stomach.data$Confidence == TRUE] <- "Identified Fish"
stomach.data$ConfY[stomach.data$Likely == "Identified Fish" & 
                   stomach.data$Confidence == FALSE] <- "Unidentified Fish"

stomach.data$ConfY[stomach.data$Likely == "Identified Invertebrate" &
                     stomach.data$Confidence == TRUE] <- "Identified Invertebrate"
stomach.data$ConfY[stomach.data$Likely == "Identified Invertebrate" &
                     stomach.data$Confidence == FALSE] <- "Unidentified Invertebrate"

stomach.data$ConfY[stomach.data$Likely == "Unidentified Invertebrate"] <- "Unidentified Invertebrate"
stomach.data$ConfY[stomach.data$Likely == "Unidentified Fish"] <- "Unidentified Fish"
stomach.data$ConfY[stomach.data$Likely == "Unidentified Material"] <- "Unidentified Material"

stomach.data$ConfY[stomach.data$FinalID == "empty"] <- "Empty"

subset(stomach.data, is.na(ConfY))

# % pooled weight ----

# remove stomachs not useful for pooled % weight
stomach.rm.pooled <- subset(stomach.rm, `Keep for Fullness` == "no")

nrow(stomach.data) # 7283
stomach.pooled <- subset(stomach.data, !`Fish Code` %in% stomach.rm.pooled$`Fish Code`)
nrow(stomach.pooled) # 7197
length(unique(stomach.pooled$`Fish Code`)) # 3447

subset(stomach.pooled, is.na(Weight)) # 0

# merge in salmon species ID
nrow(stomach.pooled) # 7197
table(salmon.data$Species) # 2997 ch, 475 co

stomach.pooled <- merge(x = salmon.data, y = stomach.pooled, by = "Fish Code", 
                        all.x = FALSE, all.y = FALSE)
nrow(stomach.pooled) # 7197
length(unique(stomach.pooled$`Fish Code`)) # 3447, good

ch.pooled <- subset(stomach.pooled, Species == "ch")
co.pooled <- subset(stomach.pooled, Species == "co")

# calculate pooled mass proportions
all.data <- data.frame(matrix(NA, nrow = 0, ncol = 5))
names(all.data) <- c("PreyCategory", "PooledWeight", "Prop", "Level", "Species")
all.data

species.list <- sort(unique(stomach.pooled$Species))
species.list

for (i in 1:length(species.list)) {
  pooled.sub <- subset(stomach.pooled, Species == species.list[i])
  
  original.aggregate.sub <- aggregate(pooled.sub$Weight, by = list(pooled.sub$OriginalHighest), FUN = sum)
  names(original.aggregate.sub) <- c("PreyCategory", "PooledWeight") 
  original.aggregate.sub$Prop <- original.aggregate.sub$PooledWeight / sum(original.aggregate.sub$PooledWeight)
  original.aggregate.sub$Level <- "1Original"  
  
  likely.aggregate.sub <- aggregate(pooled.sub$Weight, by = list(pooled.sub$Likely), FUN = sum)
  names(likely.aggregate.sub) <- c("PreyCategory", "PooledWeight") 
  likely.aggregate.sub$Prop <- likely.aggregate.sub$PooledWeight / sum(likely.aggregate.sub$PooledWeight)
  likely.aggregate.sub$Level <- "2Likely"
  
  confY.aggregate.sub <- aggregate(pooled.sub$Weight, by = list(pooled.sub$ConfY), FUN = sum)
  names(confY.aggregate.sub) <- c("PreyCategory", "PooledWeight")
  confY.aggregate.sub$Prop <- confY.aggregate.sub$PooledWeight / sum(confY.aggregate.sub$PooledWeight)
  confY.aggregate.sub$Level <- "3ConfY"
  
  sub.all.levels <- rbind(original.aggregate.sub, likely.aggregate.sub, confY.aggregate.sub)
  sub.all.levels$Species <- species.list[i]
  
  all.data <- rbind(all.data, sub.all.levels) 
}

all.data$Prop <- all.data$Prop * 100

all.data <- subset(all.data, PreyCategory != "Empty")

all.data$Species[all.data$Species == "ch"] <- "Chinook"
all.data$Species[all.data$Species == "co"] <- "Coho"

all.data

# % identifiable at each level for Results section:

# % identifiable based on external morphology
52.30960475 + 8.37292150 # 60.7% for Chinook
31.04022854 + 12.29447567 # 43.3% for coho

# % identifiable for likely
86.65570448 + 8.70076389 # 95.4% for Chinook
77.26422439 + 15.70563254 # 93.0% coho

# % identifiable to confirmed
85.88764080 + 8.57395437 # 94.5% for Chinook
75.62116072 + 15.68388272 # 91.3% for coho

# plot
ggplot() +
  geom_col(data = all.data, aes(x = Level, y = Prop, fill = PreyCategory)) +
  xlab("Identification Level") + ylab("% Pooled Weight") +
  scale_x_discrete(labels = c("Original ID", "Likely", "Confirmed"), expand = c(0.27, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
        strip.text = element_text(size = 12, colour = "black"),
        strip.background = element_blank()) +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#56B4E9", "#FA9C1B", "#009E73")) +
  facet_wrap(~ Species) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave("figures/figure3_ID_levels.PNG", height = 16, width = 19,
       units = "cm", dpi = 1600, bg = "white")
