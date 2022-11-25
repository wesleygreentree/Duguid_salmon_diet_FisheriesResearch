# Diet composition tables for main text and supplemental material using multiple metrics
# metrics: individual-level mean % weight, bulk % weight, stomach and intestine % FO

# load packages
library(readxl)
library(tidyverse)
library(reshape2)
library(flextable)

# load data
salmon.data <- read_excel("data/ASDPsalmon_v1.0.xlsx")
stomach.data <- read_excel("data/ASDPstomachs_v1.0.xlsx")
intestine.data <- read_excel("data/ASDPintestines_v1.0.xlsx")

# stomachs to remove before some diet composition metrics
stomach.rm <- read_excel("data/ASDP_stomachs_to_remove.xlsx")

names(salmon.data)
nrow(salmon.data)

salmon.data <- select(salmon.data, `Fish Code`, Species)

# stomach prey categories --- (reporting diet composition in a taxonomically-hierachichal way)
sort(unique(stomach.data$FinalID))

## assign lowest level of taxonomic hierarchy
stomach.data$Lowest <- rep(NA, nrow(stomach.data))
sort(unique(stomach.data$FinalID))

# Pancrustacea
stomach.data$Lowest[stomach.data$FinalID == "unidentified crustacean"] <- "Pancrustacea"

stomach.data$Lowest[stomach.data$FinalID == "amphipod"] <- "Amphipoda"
stomach.data$Lowest[stomach.data$FinalID %in% c("caprellid", "metacaprella")] <- "Caprellidae"
stomach.data$Lowest[stomach.data$FinalID == "gammarid amphipod"] <- "Gammaridae"
stomach.data$Lowest[stomach.data$FinalID %in% c("hyperiid amphipod")] <- "Hyperiidae"
stomach.data$Lowest[stomach.data$FinalID == "hyperia medusarum"] <- "Hyperia medusarum"
stomach.data$Lowest[stomach.data$FinalID == "themisto pacifica"] <- "Themisto pacifica"
stomach.data$Lowest[stomach.data$FinalID == "primno"] <- "Primno spp."

stomach.data$Lowest[stomach.data$FinalID == "copepod"] <- "Copepoda"

stomach.data$Lowest[stomach.data$FinalID %in% c("decapod", "post-larval crab", 
                                                "unidentified shrimp",
                                                "shrimp zoea")] <- "Decapoda"
stomach.data$Lowest[stomach.data$FinalID == "pagurid megalopa"] <- "Paguridae Megalopae"
stomach.data$Lowest[stomach.data$FinalID == "neotrypaea sp."] <- "Neotrypaea spp."

stomach.data$Lowest[stomach.data$FinalID %in% c("brachyuran", "brachyuran zoeae and megalopae",
                                                "unidentified brachyuran larvae")] <- "Brachyura"
stomach.data$Lowest[stomach.data$FinalID == "brachyuran megalopa"] <- "Brachyura Megalopae"
stomach.data$Lowest[stomach.data$FinalID == "brachyuran zoea"] <- "Brachyura Zoeae"
stomach.data$Lowest[stomach.data$FinalID == "cancrid zoea"] <- "Cancridae Zoeae"
stomach.data$Lowest[stomach.data$FinalID %in% c("cancrid megalopa", 
                                                "dungeness crab megalopa")] <- "Cancridae Megalopae"
stomach.data$Lowest[stomach.data$FinalID == "pinnotherid"] <- "Pinnotheridae"

stomach.data$Lowest[stomach.data$FinalID == "caridean shrimp"] <- "Caridea"
stomach.data$Lowest[stomach.data$FinalID == "pandalus sp."] <- "Pandalus spp."
stomach.data$Lowest[stomach.data$FinalID == "pasiphaea pacifica"] <- "Pasiphaea spp."
stomach.data$Lowest[stomach.data$FinalID == "crangonidae"] <- "Crangonidae"

stomach.data$Lowest[stomach.data$FinalID %in% c("sergestid", 
                                                "eusergestes similis")] <- "Sergestidae"
stomach.data$Lowest[stomach.data$FinalID == "euphausiid"] <- "Euphausiidae"

stomach.data$Lowest[stomach.data$FinalID == "isopod"] <- "Isopoda"

stomach.data$Lowest[stomach.data$FinalID == "mysid"] <- "Mysidae"

stomach.data$Lowest[stomach.data$FinalID == "insect"] <- "Insecta"

# Fish
stomach.data$Lowest[stomach.data$FinalID == "unidentified fish"] <- "Osteichthyes"

stomach.data$Lowest[stomach.data$FinalID == "herring"] <- "Pacific Herring"
stomach.data$Lowest[stomach.data$FinalID %in% c("anchovy", 
                                                "northern anchovy")] <- "Northern Anchovy"
stomach.data$Lowest[stomach.data$FinalID == "myctophid"] <- "Myctophidae"
stomach.data$Lowest[stomach.data$FinalID == "surfperch"] <- "Embiotocidae"
stomach.data$Lowest[stomach.data$FinalID == "sand lance"] <- "Pacific Sand Lance"
stomach.data$Lowest[stomach.data$FinalID == "prowfish"] <- "Prowfish"
stomach.data$Lowest[stomach.data$FinalID == "barracudina"] <- "Barracudina"
stomach.data$Lowest[stomach.data$FinalID == "jack mackerel"] <- "Jack Mackerel"
stomach.data$Lowest[stomach.data$FinalID == "pacific saury"] <- "Pacific Saury"
stomach.data$Lowest[stomach.data$FinalID %in% c("flatfish", "larval flat fish")] <- "Pleuronectoidei"
stomach.data$Lowest[stomach.data$FinalID == "gadid"] <- "Gadiformes"
stomach.data$Lowest[stomach.data$FinalID == "pacific cod"] <- "Pacific Cod"
stomach.data$Lowest[stomach.data$FinalID == "walleye pollock"] <- "Walleye Pollock"
stomach.data$Lowest[stomach.data$FinalID %in% c("hake", "pacific hake")] <- "Pacific Hake"
stomach.data$Lowest[stomach.data$FinalID == "rockfish"] <- "Scorpaenidae" 
stomach.data$Lowest[stomach.data$FinalID == "sablefish"] <- "Sablefish"
stomach.data$Lowest[stomach.data$FinalID == "lingcod"] <- "Lingcod"
stomach.data$Lowest[stomach.data$FinalID == "staghorn sculpin"] <- "Staghorn Sculpin"
stomach.data$Lowest[stomach.data$FinalID == "poacher"] <- "Agonidae"
stomach.data$Lowest[stomach.data$FinalID == "midshipman"] <- "Plainfin Midshipman"
stomach.data$Lowest[stomach.data$FinalID == "gasterosteiformes"] <- "Perciformes"
stomach.data$Lowest[stomach.data$FinalID == "threespine stickleback"] <- "Threespine Stickleback"
stomach.data$Lowest[stomach.data$FinalID == "tubesnout"] <- "Tubesnout"
stomach.data$Lowest[stomach.data$FinalID == "eulachon"] <- "Eulachon"
stomach.data$Lowest[stomach.data$FinalID == "surf smelt"] <- "Surf Smelt"
stomach.data$Lowest[stomach.data$FinalID %in% c("deep-sea smelt", 
                                                "northern smoothtongue")] <- "Bathylagidae"
stomach.data$Lowest[stomach.data$FinalID == "salmon"] <- "Oncorhynchus spp."
stomach.data$Lowest[stomach.data$FinalID == "chinook salmon"] <- "Chinook Salmon"
stomach.data$Lowest[stomach.data$FinalID == "chum salmon"] <- "Chum Salmon"
stomach.data$Lowest[stomach.data$FinalID == "pink salmon"] <- "Pink Salmon"

# cephalopods
stomach.data$Lowest[stomach.data$FinalID == "unidentified cephalopod"] <- "Cephalopoda"
stomach.data$Lowest[stomach.data$FinalID == "octopus"] <- "Octopoda"
stomach.data$Lowest[stomach.data$FinalID %in% c("squid", "stubby squid")] <- "Teuthida"

# polychaete
stomach.data$Lowest[stomach.data$FinalID == "polychaete"] <- "Polychaeta"

# other
stomach.data$Lowest[stomach.data$FinalID %in% c("algae", "other", 
                                                "gastropod", "zostera spp.")] <- "Other"
stomach.data$Lowest[stomach.data$FinalID == "unidentified material"] <- "Unidentified Material"

# empty
stomach.data$Lowest[stomach.data$FinalID == "empty"] <- "Empty"


## Stomach frequency of occurrence ----
stomach.fo <- stomach.data

stomach.fo$Weight <- 1
class(stomach.fo$Weight) # numeric
subset(stomach.fo, is.na(Weight)) # 0, good

stomach.fo.aggregate <- aggregate(stomach.fo$Weight,
                                  by = list(stomach.fo$Lowest, stomach.fo$`Fish Code`),
                                  FUN = mean)
names(stomach.fo.aggregate) <- c("Lowest", "Fish Code", "Weight")
unique(stomach.fo.aggregate$Weight) # 1, good
head(stomach.fo.aggregate)

stomach.fo.wide <- dcast(stomach.fo.aggregate, `Fish Code` ~ Lowest)
stomach.fo.wide[is.na(stomach.fo.wide)] <- 0

# higher levels of the hierarchy: 
names(stomach.fo.wide)


# fish 
stomach.fo.wide$Osteichthyes <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Osteichthyes", "Pacific Herring", "Northern Anchovy",
                                                                                        "Myctophidae", "Embiotocidae", "Pacific Sand Lance", "Prowfish",
                                                                                        "Barracudina", "Jack Mackerel", "Pacific Saury", "Pleuronectoidei",
                                                                                        "Gadiformes", "Pacific Cod", "Walleye Pollock", "Pacific Hake",
                                                                                        "Scorpaenidae", "Sablefish", "Lingcod", "Staghorn Sculpin", "Agonidae",
                                                                                        "Plainfin Midshipman", "Perciformes", "Threespine Stickleback",
                                                                                        "Tubesnout", "Eulachon", "Surf Smelt", "Bathylagidae",
                                                                                        "Oncorhynchus spp.", "Chinook Salmon", "Chum Salmon", "Pink Salmon")])

stomach.fo.wide$Clupeiformes <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Pacific Herring", "Northern Anchovy")])
stomach.fo.wide$`Oncorhynchus spp.` <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Oncorhynchus spp.", "Chinook Salmon", "Chum Salmon",
                                                                                               "Pink Salmon")])
stomach.fo.wide$Osmeridae <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Eulachon", "Surf Smelt")])
stomach.fo.wide$Gadiformes <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Gadiformes", "Pacific Cod", "Walleye Pollock", "Pacific Hake")])
stomach.fo.wide$`Gadus spp.` <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Pacific Cod", "Walleye Pollock")])
stomach.fo.wide$Carangiformes <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Pleuronectoidei", "Jack Mackerel")])
stomach.fo.wide$Perciformes <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Perciformes", "Prowfish", "Pacific Sand Lance", "Scorpaenidae",
                                                                                       "Threespine Stickleback", "Tubesnout", "Sablefish", "Lingcod",
                                                                                       "Staghorn Sculpin", "Agonidae")])

# Pancrustaceans
names(stomach.fo.wide)
stomach.fo.wide$Pancrustacea <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Pancrustacea", "Amphipoda", "Caprellidae", "Gammaridae", "Hyperiidae",
                                                                                        "Hyperia medusarum", "Themisto pacifica", "Primno spp.", "Copepoda",
                                                                                        "Decapoda", "Paguridae Megalopae", "Neotrypaea spp.", "Brachyura",
                                                                                        "Brachyura Megalopae", "Brachyura Zoeae", "Pinnotheridae",
                                                                                        "Cancridae Zoeae", "Cancridae Megalopae", "Caridea",
                                                                                        "Pandalus spp.", "Pasiphaea spp.", "Crangonidae", "Sergestidae",
                                                                                        "Euphausiidae", "Isopoda", "Mysidae", "Insecta")])

stomach.fo.wide$Amphipoda <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Amphipoda", "Caprellidae", "Gammaridae", "Hyperiidae", 
                                                                                     "Hyperia medusarum", "Themisto pacifica", "Primno spp.")])
stomach.fo.wide$Hyperiidae <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Hyperiidae", "Hyperia medusarum", "Themisto pacifica")])
stomach.fo.wide$Decapoda <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Decapoda", "Paguridae Megalopae", "Neotrypaea spp.",
                                                                                    "Brachyura", "Brachyura Megalopae", "Brachyura Zoeae",
                                                                                    "Cancridae Zoeae", "Cancridae Megalopae",
                                                                                    "Caridea", "Pandalus spp.", "Pasiphaea spp.", "Crangonidae",
                                                                                    "Sergestidae", "Pinnotheridae")])
stomach.fo.wide$Brachyura <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Brachyura", "Brachyura Megalopae", "Brachyura Zoeae",
                                                                                     "Cancridae Zoeae", "Cancridae Megalopae", "Pinnotheridae")])
stomach.fo.wide$`Brachyura Megalopae` <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Brachyura Megalopae", "Cancridae Megalopae")])
stomach.fo.wide$`Brachyura Zoeae` <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Brachyura Zoeae", "Cancridae Zoeae")])
stomach.fo.wide$Cancridae <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Cancridae Zoeae", "Cancridae Megalopae")])
stomach.fo.wide$Caridea <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Caridea", "Pandalus spp.", "Pasiphaea spp.", "Crangonidae")])

# cepahalopods
stomach.fo.wide$Cephalopoda <- rowSums(stomach.fo.wide[, names(stomach.fo.wide) %in% c("Cephalopoda", "Octopoda", "Teuthida")])

# need to convert all > 0 to 1
names(stomach.fo.wide)
ncol(stomach.fo.wide) # 71
stomach.fo.wide[2:(ncol(stomach.fo.wide))][stomach.fo.wide[2:(ncol(stomach.fo.wide))] > 0] <- 1 


# Intestine frequency of occurrence ----
nrow(intestine.data) # 4409

# % of stomachs without corresponding intestines
nrow(salmon.data)
length(unique(intestine.data$`Fish Code`))
(1 - (3331/3472)) * 100 # 4.1%

# intestine prey categories
intestine.data$Lowest <- rep(NA, nrow(intestine.data))
sort(unique(intestine.data$`Final ID`))

names(intestine.data)
intestine.data$Lowest2 <- intestine.data$Lowest

intestine.data$Lowest[intestine.data$`Final ID` == "unidentified fish"] <- "Osteichthyes"
intestine.data$Lowest[intestine.data$`Final ID` == "herring"] <- "Pacific Herring"
intestine.data$Lowest[intestine.data$`Final ID` == "northern anchovy"] <- "Northern Anchovy"
intestine.data$Lowest[intestine.data$`Final ID` == "myctophid"] <- "Myctophidae"
intestine.data$Lowest[intestine.data$`Final ID` == "gadid"] <- "Gadiformes"
intestine.data$Lowest[intestine.data$`Final ID` %in% c("hake", "pacific hake")] <- "Pacific Hake"
intestine.data$Lowest[intestine.data$`Final ID` == "surfperch"] <- "Embiotocidae"
intestine.data$Lowest[intestine.data$`Final ID` == "sand lance"] <- "Pacific Sand Lance"
intestine.data$Lowest[intestine.data$`Final ID` == "rockfish"] <- "Scorpaenidae"
intestine.data$Lowest[intestine.data$`Final ID` == "salmon"] <- "Oncorhynchus spp."

intestine.data$Lowest[intestine.data$`Final ID` == "unidentified crustacean"] <- "Pancrustacea"
intestine.data$Lowest[intestine.data$`Final ID` == "amphipod"] <- "Amphipoda"
intestine.data$Lowest[intestine.data$`Final ID` == "cancrid megalopa"] <- "Cancridae Megalopae"
intestine.data$Lowest[intestine.data$`Final ID` == "pandalus sp."] <- "Pandalus spp."
intestine.data$Lowest[intestine.data$`Final ID` == "isopod"] <- "Isopoda"

intestine.data$Lowest[intestine.data$`Final ID` == "polychaete"] <- "Polychaeta"
intestine.data$Lowest[intestine.data$`Final ID` == "unidentified cephalopod"] <- "Cephalopoda"
intestine.data$Lowest[intestine.data$`Final ID` == "squid"] <- "Teuthida"

intestine.data$Lowest[intestine.data$`Final ID` == "empty"] <- "Empty"
subset(intestine.data, is.na(Lowest)) # 0

sort(unique(intestine.data$Lowest))

# intestine FO diet matrix
intestine.fo.wide <- dcast(intestine.data, `Fish Code` ~ Lowest, value.var = "DummyValue", mean)
intestine.fo.wide <- intestine.fo.wide %>% mutate_all(~replace(., is.nan(.), 0))

# higher levels of taxonomic hierarchy

names(intestine.fo.wide)
intestine.fo.wide$Osteichthyes <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Embiotocidae", "Gadiformes", "Myctophidae",
                                                                                              "Northern Anchovy", "Oncorhynchus spp.", "Osteichthyes",
                                                                                              "Pacific Hake", "Pacific Herring", "Pacific Sand Lance",
                                                                                              "Scorpaenidae")])
intestine.fo.wide$Clupeiformes <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Northern Anchovy", "Pacific Herring")])
intestine.fo.wide$Perciformes <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Scorpaenidae", "Pacific Sand Lance")])
intestine.fo.wide$Gadiformes <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Gadiformes", "Pacific Hake")])


intestine.fo.wide$Pancrustacea <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Amphipoda", "Cancridae Megalopae", "Isopoda",
                                                                                              "Pancrustacea", "Pandalus spp.")])

# building out levels of hierarchy needed to combine with stomach data
intestine.fo.wide$Cancridae <- intestine.fo.wide$`Cancridae Megalopae`
intestine.fo.wide$Brachyura <- intestine.fo.wide$`Cancridae Megalopae`
intestine.fo.wide$`Brachyura Megalopae` <- intestine.fo.wide$`Cancridae Megalopae`
intestine.fo.wide$Caridea <- intestine.fo.wide$`Pandalus spp.`
intestine.fo.wide$Decapoda <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Cancridae Megalopae", "Pandalus spp.")])

intestine.fo.wide$Cephalopoda <- rowSums(intestine.fo.wide[, names(intestine.fo.wide) %in% c("Cephalopoda", "Teuthida")])

# assign values > 1 to 1
names(intestine.fo.wide)
intestine.fo.wide[2:(ncol(intestine.fo.wide))][intestine.fo.wide[2:(ncol(intestine.fo.wide))] > 0] <- 1


# Calculate stomach % FO ---- 
nrow(stomach.fo.wide) # 3472
nrow(salmon.data) # 3472

stomach.fo.wide <- merge(x = salmon.data, y = stomach.fo.wide, by = "Fish Code", all.x = FALSE, all.y = FALSE)
nrow(stomach.fo.wide) # 3472
table(stomach.fo.wide$Species) # 2997 Chinook, 475 coho

empty.stomachs <- subset(stomach.fo.wide, Empty == 1)
# storing for later calculation of % empty stomachs that had a full intestine

stomach.fo.wide <- subset(stomach.fo.wide, Empty != 1)
stomach.fo.wide <- stomach.fo.wide[, !names(stomach.fo.wide) == "Empty"]
nrow(stomach.fo.wide) # 2757
table(stomach.fo.wide$Species) # 2357 Chinook, 400 coho

# get % empty stomachs for Chinook and coho (with stomach FO dataset) 
(2997-2357)/2997 # 21.4%
(475-400)/475 # 15.8%

ch.stomach.fo.wide <- subset(stomach.fo.wide, Species == "ch")
co.stomach.fo.wide <- subset(stomach.fo.wide, Species == "co")

# Chinook stomach FO
names(ch.stomach.fo.wide)
ch.stomach.fo <- as.data.frame(colMeans(ch.stomach.fo.wide[3:(ncol(ch.stomach.fo.wide))])) 
ch.stomach.fo$PreyCategory <- rownames(ch.stomach.fo)
rownames(ch.stomach.fo) <- c()
ch.stomach.fo <- relocate(ch.stomach.fo, PreyCategory)
names(ch.stomach.fo)[2] <- "ChinookStomachFO"

# coho stomach FO
names(co.stomach.fo.wide)
co.stomach.fo <- as.data.frame(colMeans(co.stomach.fo.wide[3:(ncol(co.stomach.fo.wide))]))
co.stomach.fo$PreyCategory <- rownames(co.stomach.fo)
rownames(co.stomach.fo) <- c()
co.stomach.fo <- relocate(co.stomach.fo, PreyCategory)
names(co.stomach.fo)[2] <- "CohoStomachFO"

nrow(ch.stomach.fo) # 69
nrow(co.stomach.fo) # 69

both.stomach.fo <- merge(x = ch.stomach.fo, y = co.stomach.fo, by = "PreyCategory", all.x = FALSE, all.y = FALSE)
view(both.stomach.fo)

## calculate intestine % FO ----
nrow(intestine.fo.wide) # 3331

intestine.fo.wide <- merge(x = salmon.data, y = intestine.fo.wide, by = "Fish Code", all.x = FALSE, all.y = FALSE)
nrow(intestine.fo.wide) # 3331
table(intestine.fo.wide$Species) # 2875 Chinook, 456 coho

# % of empty stomachs with intestines
nrow(empty.stomachs)
table(empty.stomachs$Species)
empty.stomach.has.intestine <- subset(empty.stomachs, `Fish Code` %in% intestine.fo.wide$`Fish Code`)
table(empty.stomach.has.intestine$Species)
(609/640)*100 # 95.2%
(74/75)*100 # 98.7%

# % empty intestines and remove empty intestines
empty.intestines <- subset(intestine.fo.wide, Empty == 1) # 595
nrow(empty.intestines) # 595

intestine.fo.wide <- subset(intestine.fo.wide, Empty != 1)
intestine.fo.wide <- intestine.fo.wide[, !names(intestine.fo.wide) == "Empty"]
nrow(intestine.fo.wide) # 2736
table(intestine.fo.wide$Species)
(2875-2396)/2875 # 16.7% 
(456-340)/456 # 25.4% 

# % of empty stomachs with non-empty intestines
empty.stomach.full.intestine <- subset(empty.stomachs, `Fish Code` %in% intestine.fo.wide$`Fish Code`)
table(empty.stomach.full.intestine$Species)
(328/609)*100 # 53.9%
(39/74)*100 # 52.7%

# intestine FO by species
ch.intestine.fo.wide <- subset(intestine.fo.wide, Species == "ch")
co.intestine.fo.wide <- subset(intestine.fo.wide, Species == "co")

names(ch.intestine.fo.wide)
ncol(ch.intestine.fo.wide) # 27

# Chinook intestine FO
ch.intestine.fo <- as.data.frame(colMeans(ch.intestine.fo.wide[3:(ncol(ch.intestine.fo.wide))]))
ch.intestine.fo$PreyCategory <- rownames(ch.intestine.fo)
rownames(ch.intestine.fo) <- c()
ch.intestine.fo <- relocate(ch.intestine.fo, PreyCategory)
names(ch.intestine.fo)[2] <- "ChinookIntestineFO"

co.intestine.fo <- as.data.frame(colMeans(co.intestine.fo.wide[3:(ncol(co.intestine.fo.wide))]))
co.intestine.fo$PreyCategory <- rownames(co.intestine.fo)
rownames(co.intestine.fo) <- c()
co.intestine.fo <- relocate(co.intestine.fo, PreyCategory)
names(co.intestine.fo)[2] <- "CohoIntestineFO"

nrow(ch.intestine.fo) # 25
nrow(co.intestine.fo) # 25

both.intestine.fo <- merge(x = ch.intestine.fo, y = co.intestine.fo, by = "PreyCategory", all.x = FALSE, all.y = FALSE)
nrow(both.intestine.fo) # 25
view(both.intestine.fo)

## Individual-level mean % weight ----  

# remove stomachs
stomach.rm.prop <- subset(stomach.rm, `Keep for Proportion` == "no")
nrow(stomach.rm.prop) # 21

stomach.indiv <- subset(stomach.data, !`Fish Code` %in% stomach.rm.prop$`Fish Code`)
nrow(stomach.indiv) # 7211

length(unique(stomach.indiv$`Fish Code`)) # 3455

###
# weight was not measured for a few number of prey items. Replace 0 with 0.0001 g
# 20826, 20827 are dealt with separately - only add 0.0001 g to the Unidentified Material row
keep.for.prop <- subset(stomach.rm, `Keep for Proportion` == "yes" & `Keep for Fullness` == "no" &
                                    !`Fish Code` %in% c(20826, 20827))
keep.for.prop
nrow(subset(keep.for.prop, `Fish Code` %in% stomach.data$`Fish Code`)) # 6
view(subset(stomach.data, `Fish Code` %in% keep.for.prop$`Fish Code`))
# 17115: don't add 0.0001 g to SalmonChinook Salmon since another salmon row with weight
# 18705: don't add 0.0001 g to Unid FishHerring since another herring row with weight

keep.for.prop.no.weight <- subset(keep.for.prop, !`Fish Code` %in% c(17115, 18705))
subset(stomach.data, `Fish Code` %in% keep.for.prop.no.weight$`Fish Code`)
# only 4 of these are in stomach.data since the others are pre-April 2017 or from the UBC trawl stomachs

# assign 0.0001 g
stomach.data$Weight[stomach.data$`Fish Code` %in% keep.for.prop.no.weight$`Fish Code`] <- 0.0001
# double check with anti-join
view(subset(stomach.data, `Fish Code` %in% keep.for.prop.no.weight$`Fish Code`))

# deal with 20826, 20827 separately, assign 0.0001 g to the Unidentified Material row
subset(stomach.data, `Fish Code` %in% c(20826, 20827))
stomach.data$Weight[stomach.data$`Fish Code` %in% c(20826, 20827) &
                      stomach.data$FinalID == "unidentified material"] <- 0.0001
###

subset(stomach.indiv, is.na(Weight)) # check for NA's, none

# aggregate
stomach.indiv.aggregate <- aggregate(stomach.indiv$Weight,
                                     by = list(stomach.indiv$Lowest, stomach.indiv$`Fish Code`), 
                                     FUN = sum)
head(stomach.indiv.aggregate)
names(stomach.indiv.aggregate) <- c("Lowest", "Fish Code", "Weight")

# cast to wide
stomach.indiv.weight <- dcast(stomach.indiv.aggregate, `Fish Code` ~ Lowest) 
nrow(stomach.indiv.weight) # 3455

stomach.indiv.weight[is.na(stomach.indiv.weight)] <- 0
head(stomach.indiv.weight)

names(stomach.indiv.weight)
stomach.indiv.weight$totalWeight <- rowSums(stomach.indiv.weight[2:(ncol(stomach.indiv.weight))])
head(stomach.indiv.weight)

# get individual proportions
stomach.indiv.prop <- stomach.indiv.weight

names(stomach.indiv.prop)
stomach.indiv.prop[2:(ncol(stomach.indiv.prop) - 1)] <- stomach.indiv.prop[2:(ncol(stomach.indiv.prop) - 1)] / stomach.indiv.prop$totalWeight
head(stomach.indiv.prop)

# deal with NaN (not a number), caused by dividing 0/0
stomach.indiv.prop <- stomach.indiv.prop %>% mutate_all(~replace(., is.nan(.), 0))
head(stomach.indiv.prop)

names(stomach.indiv.prop)
stomach.indiv.prop$totalProp <- rowSums(stomach.indiv.prop[2:(ncol(stomach.indiv.prop) - 1)])
unique(stomach.indiv.prop$totalProp) # 0, 1

# merge individual proportions with salmon data
nrow(salmon.data) # 3472
nrow(stomach.indiv.prop) # 3455 # makes sense, 17 fewer
head(salmon.data)

stomach.indiv.prop <- merge(x = salmon.data, y = stomach.indiv.prop, 
                            by = "Fish Code", all.x = FALSE, all.y = FALSE)
nrow(stomach.indiv.prop) # 3455

table(stomach.indiv.prop$Species) # 2989 Chinook, 466 coho

# remove empty stomachs
stomach.indiv.prop <- subset(stomach.indiv.prop, totalProp != 0)
names(stomach.indiv.prop)
stomach.indiv.prop <- stomach.indiv.prop[, !names(stomach.indiv.prop) == "Empty"]
nrow(stomach.indiv.prop) # 2734

table(stomach.indiv.prop$Species) # 2343 Chinook, 391 coho
(2989-2343)/2989 # 21.6% empty
(466-391)/466 # 16.1% empty

# calculate individual-level mean % weight for Chinook and coho, including unidentified material
ch.stomach.indiv.prop <- subset(stomach.indiv.prop, Species == "ch")
co.stomach.indiv.prop <- subset(stomach.indiv.prop, Species == "co")

# Chinook individual-level mean % weight
names(ch.stomach.indiv.prop)
ch.indiv.prop <- as.data.frame(colMeans(ch.stomach.indiv.prop[3:(ncol(ch.stomach.indiv.prop) - 2)]))
names(ch.indiv.prop) <- "ChinookMeanProportion"
ch.indiv.prop$PreyCategory <- rownames(ch.indiv.prop)
ch.indiv.prop <- relocate(ch.indiv.prop, PreyCategory)
rownames(ch.indiv.prop) <- c()
ch.indiv.prop
sum(ch.indiv.prop$ChinookMeanProportion) # 1 

# Coho individual-level mean % weight
names(co.stomach.indiv.prop)
co.indiv.prop <- as.data.frame(colMeans(co.stomach.indiv.prop[3:(ncol(ch.stomach.indiv.prop) - 2)]))
names(co.indiv.prop) <- "CohoMeanProportion"
co.indiv.prop$PreyCategory <- rownames(co.indiv.prop)
co.indiv.prop <- relocate(co.indiv.prop, PreyCategory)
rownames(co.indiv.prop) <- c()
co.indiv.prop
sum(co.indiv.prop$CohoMeanProportion) # 1

# higher levels of hierarchy
nrow(ch.indiv.prop) # 62
nrow(co.indiv.prop) # 62
both.indiv.prop <- merge(x = ch.indiv.prop, y = co.indiv.prop, 
                         by = "PreyCategory", all.x = FALSE, all.y = FALSE)
nrow(both.indiv.prop) # 62
view(both.indiv.prop)

both.indiv.prop.wide <- as.data.frame(t(both.indiv.prop))
colnames(both.indiv.prop.wide) <- both.indiv.prop.wide[1, ]
both.indiv.prop.wide <- both.indiv.prop.wide[2:3, ]
view(both.indiv.prop.wide)
names(both.indiv.prop.wide)
# converted proportions to character

both.indiv.prop.wide <- mutate_all(both.indiv.prop.wide, function(x) as.numeric(x))
str(both.indiv.prop.wide)

# Proportion of prey mean % weight represented by intestine DHPs
view(both.indiv.prop.wide)
names(both.indiv.prop.wide)
rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Pacific Herring", "Northern Anchovy", "Myctophidae",
                                                                  "Embiotocidae", "Pacific Sand Lance", "Gadiformes",
                                                                  "Pacific Hake", "Pacific Cod", "Walleye Pollock",
                                                                  "Scorpaenidae", "Oncorhynchus spp.", "Chinook Salmon",
                                                                  "Chum Salmon", "Pink Salmon", "Amphipoda", "Caprellidae",
                                                                  "Gammaridae", "Hyperia medusarum", "Hyperiidae", 
                                                                  "Primno spp.", "Themisto pacifica", "Isopoda",
                                                                  "Cancridae Megalopae", "Pandalus spp.", "Cephalopoda",
                                                                  "Octopoda", "Teuthida", "Polychaeta")])
# 80.5% for Chinook, 65.0% for coho

# assign higher levels
names(both.indiv.prop.wide)

both.indiv.prop.wide$Osteichthyes <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Osteichthyes", "Pacific Herring", "Northern Anchovy",
                                                                                                       "Myctophidae", "Embiotocidae", "Pacific Sand Lance", 
                                                                                                       "Prowfish", "Barracudina", "Jack Mackerel", "Pacific Saury",
                                                                                                       "Pleuronectoidei", "Gadiformes", "Pacific Cod",
                                                                                                       "Walleye Pollock", "Pacific Hake", "Scorpaenidae",
                                                                                                       "Sablefish", "Lingcod", "Staghorn Sculpin", "Agonidae",
                                                                                                       "Plainfin Midshipman", "Perciformes", "Threespine Stickleback",
                                                                                                       "Tubesnout", "Eulachon", "Surf Smelt", "Bathylagidae",
                                                                                                       "Oncorhynchus spp.", "Chinook Salmon", "Chum Salmon",
                                                                                                       "Pink Salmon")])

both.indiv.prop.wide$Clupeiformes <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Pacific Herring", "Northern Anchovy")])
both.indiv.prop.wide$`Oncorhynchus spp.` <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Oncorhynchus spp.", "Chinook Salmon", "Chum Salmon",
                                                                                                              "Pink Salmon")])
both.indiv.prop.wide$Osmeridae <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Eulachon", "Surf Smelt")])
both.indiv.prop.wide$Gadiformes <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Gadiformes", "Pacific Cod",
                                                                                                     "Walleye Pollock", "Pacific Hake")])
both.indiv.prop.wide$`Gadus spp.` <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Pacific Cod", "Walleye Pollock")])
both.indiv.prop.wide$Carangiformes <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Jack Mackerel", "Pleuronectoidei")])
both.indiv.prop.wide$Perciformes <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Perciformes", "Prowfish", "Pacific Sand Lance",
                                                                                                      "Scorpaenidae", "Threespine Stickleback", "Tubesnout",
                                                                                                      "Sablefish", "Lingcod", "Staghorn Sculpin", "Agonidae")])

names(both.indiv.prop.wide)
both.indiv.prop.wide$Pancrustacea <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Pancrustacea", "Amphipoda", "Caprellidae", "Gammaridae",
                                                                                                       "Hyperiidae", "Hyperia medusarum", "Themisto pacifica", 
                                                                                                       "Primno spp.", "Copepoda", "Decapoda", "Paguridae Megalopae",
                                                                                                       "Neotrypaea spp.", "Brachyura", "Brachyura Megalopae",
                                                                                                       "Brachyura Zoeae", "Pinnotheridae", "Cancridae Zoeae",
                                                                                                       "Cancridae Megalopae", "Pandalus spp.", "Pasiphaea spp.",
                                                                                                       "Sergestidae", "Euphausiidae", "Isopoda", "Mysidae",
                                                                                                       "Insecta")])
both.indiv.prop.wide$Amphipoda <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Amphipoda", "Caprellidae", "Gammaridae", "Hyperiidae", 
                                                                                                    "Hyperia medusarum", "Themisto pacifica", "Primno spp.")])
both.indiv.prop.wide$Hyperiidae <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Hyperiidae", "Hyperia medusarum", "Themisto pacifica")])


both.indiv.prop.wide$Decapoda <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Decapoda", "Paguridae Megalopae", "Neotrypaea spp.",
                                                                                                   "Brachyura", "Brachyura Zoeae", "Brachyura Megalopae",
                                                                                                   "Cancridae Zoeae", "Cancridae Megalopae", "Pandalus spp.",
                                                                                                   "Pasiphaea spp.", "Sergestidae", "Pinnotheridae")])

both.indiv.prop.wide$Brachyura <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Brachyura", "Brachyura Zoeae", "Brachyura Megalopae",
                                                                                                    "Cancridae Zoeae", "Cancridae Megalopae", "Pinnotheridae")])
both.indiv.prop.wide$`Brachyura Megalopae` <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Brachyura Megalopae", "Cancridae Megalopae")])
both.indiv.prop.wide$`Brachyura Zoeae` <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Brachyura Zoeae", "Cancridae Zoeae")])

both.indiv.prop.wide$Cancridae <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Cancridae Zoeae", "Cancridae Megalopae")])

both.indiv.prop.wide$Caridea <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Pandalus spp.", "Pasiphaea spp.")])

both.indiv.prop.wide$Cephalopoda <- rowSums(both.indiv.prop.wide[, names(both.indiv.prop.wide) %in% c("Cephalopoda", "Octopoda", "Teuthida")])

both.indiv.prop <- as.data.frame(t(both.indiv.prop.wide))
both.indiv.prop$PreyCategory <- rownames(both.indiv.prop)
both.indiv.prop <- relocate(both.indiv.prop, PreyCategory)
rownames(both.indiv.prop) <- c()

view(both.indiv.prop)

## Pooled % weight ----
stomach.rm.bulk.prop <- subset(stomach.rm, `Keep for Fullness` == "no")
view(stomach.rm.bulk.prop)

nrow(stomach.data)
length(unique(stomach.data$`Fish Code`)) # 3472
stomach.pooled <- subset(stomach.data, !`Fish Code` %in% stomach.rm.bulk.prop$`Fish Code`)
nrow(stomach.pooled) # 7197
length(unique(stomach.pooled$`Fish Code`)) # 3447 # 25 fewer, makes sense

# make sure there are no NA weights
subset(stomach.pooled, is.na(Weight)) # 0

# merge with salmon data, subset to each species
nrow(salmon.data) # 3472
length(unique(stomach.pooled$`Fish Code`)) # 3447

stomach.pooled <- merge(x = salmon.data, y = stomach.pooled,
                        by = "Fish Code", all.x = FALSE, all.y = FALSE)
length(unique(stomach.pooled$`Fish Code`)) # 3447

# separate by species
ch.pooled <- subset(stomach.pooled, Species == "ch")
length(unique(ch.pooled$`Fish Code`)) # 2981
co.pooled <- subset(stomach.pooled, Species == "co")
length(unique(co.pooled$`Fish Code`)) # 466

view(ch.pooled)
sort(unique(ch.pooled$Lowest))
nrow(subset(ch.pooled, Lowest == "Empty")) # 640/2981 = 21.5% Chinook 
nrow(subset(co.pooled, Lowest == "Empty")) # 75/466 = 16.1% coho 

(640/2981) # 21.5%
(75/466) # 16.1%


# Chinook pooled % weight
ch.pooled.aggregate <- aggregate(ch.pooled$Weight, by = list(ch.pooled$Lowest), FUN = sum)
nrow(ch.pooled.aggregate) # 44 
names(ch.pooled.aggregate) <- c("PreyCategory", "PooledWeight")
sort(unique(ch.pooled.aggregate$PreyCategory))

sum(ch.pooled.aggregate$PooledWeight) # 65409.6
ch.pooled.aggregate$ChinookPooledProportion <- ch.pooled.aggregate$PooledWeight / sum(ch.pooled.aggregate$PooledWeight)
head(ch.pooled.aggregate)

ch.pooled.prop <- select(ch.pooled.aggregate, PreyCategory, ChinookPooledProportion)
view(ch.pooled.prop)

# Coho pooled % weight 
co.pooled.aggregate <- aggregate(co.pooled$Weight, by = list(co.pooled$Lowest), FUN = sum)
nrow(co.pooled.aggregate) # 45
names(co.pooled.aggregate) <- c("PreyCategory", "PooledWeight")

sum(co.pooled.aggregate$PooledWeight)
co.pooled.aggregate$CohoPooledProportion <- co.pooled.aggregate$PooledWeight / sum(co.pooled.aggregate$PooledWeight)
co.pooled.aggregate

co.pooled.prop <- select(co.pooled.aggregate, PreyCategory, CohoPooledProportion)
view(co.pooled.prop)

# higher levels of hierarchy
nrow(ch.pooled.prop) # 44
nrow(co.pooled.prop) # 45
both.pooled.prop <- merge(x = ch.pooled.prop, y = co.pooled.prop, by = "PreyCategory", all.x = TRUE, all.y = TRUE)
nrow(both.pooled.prop) # 63

both.pooled.prop.wide <- as.data.frame(t(both.pooled.prop))
colnames(both.pooled.prop.wide) <- both.pooled.prop.wide[1, ]
view(both.pooled.prop.wide)

both.pooled.prop.wide <- both.pooled.prop.wide[2:3, ]
view(both.pooled.prop.wide)
names(both.pooled.prop.wide)
str(both.pooled.prop.wide)

both.pooled.prop.wide <- mutate_all(both.pooled.prop.wide, function(x) as.numeric(x))
str(both.pooled.prop.wide)

view(both.pooled.prop.wide)

# convert NAs to zero
both.pooled.prop.wide[is.na(both.pooled.prop.wide)] <- 0

# Proportion of prey pooled % weight represented by intestine DHPs
view(both.pooled.prop.wide)
names(both.pooled.prop.wide)
rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Pacific Herring", "Northern Anchovy", "Myctophidae",
                                                                  "Embiotocidae", "Pacific Sand Lance", "Gadiformes",
                                                                  "Pacific Hake", "Pacific Cod", "Walleye Pollock",
                                                                  "Scorpaenidae", "Oncorhynchus spp.", "Chinook Salmon",
                                                                  "Chum Salmon", "Pink Salmon", "Amphipoda", "Caprellidae",
                                                                  "Gammaridae", "Hyperia medusarum", "Hyperiidae", 
                                                                  "Primno spp.", "Themisto pacifica", "Isopoda",
                                                                  "Cancridae Megalopae", "Pandalus spp.", "Cephalopoda",
                                                                  "Octopoda", "Teuthida", "Polychaeta")])
# 94.3% for Chinook, 78.2% for coho

# assign higher levels
view(both.pooled.prop.wide)
names(both.pooled.prop.wide)

both.pooled.prop.wide$Osteichthyes <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Osteichthyes", "Pacific Herring", "Northern Anchovy",
                                                                                                          "Myctophidae", "Embiotocidae", "Pacific Sand Lance", 
                                                                                                          "Prowfish", "Barracudina", "Jack Mackerel",
                                                                                                          "Pacific Saury", "Pleuronectoidei", "Gadiformes",
                                                                                                          "Pacific Cod", "Walleye Pollock", "Pacific Hake",
                                                                                                          "Scorpaenidae", "Sablefish", "Lingcod",
                                                                                                          "Staghorn Sculpin", "Agonidae", "Plainfin Midshipman",
                                                                                                          "Perciformes", "Threespine Stickleback",
                                                                                                          "Tubesnout", "Eulachon", "Surf Smelt", "Bathylagidae",
                                                                                                          "Oncorhynchus spp.", "Chinook Salmon", "Chum Salmon",
                                                                                                          "Pink Salmon")])

both.pooled.prop.wide$Clupeiformes <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Pacific Herring", "Northern Anchovy")])
both.pooled.prop.wide$`Oncorhynchus spp.` <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Oncorhynchus spp.", "Chinook Salmon",
                                                                                                                 "Chum Salmon", "Pink Salmon")])
both.pooled.prop.wide$Osmeridae <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Eulachon", "Surf Smelt")])
both.pooled.prop.wide$Gadiformes <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Gadiformes", "Pacific Cod",
                                                                                                        "Walleye Pollock", "Pacific Hake")])
both.pooled.prop.wide$`Gadus spp.` <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Pacific Cod", "Walleye Pollock")])
both.pooled.prop.wide$Carangiformes <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Jack Mackerel", "Pleuronectoidei")])
both.pooled.prop.wide$Perciformes <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Perciformes", "Pacific Sand Lance", "Prowfish",
                                                                                                         "Scorpaenidae", "Sablefish", "Lingcod",
                                                                                                         "Staghorn Sculpin", "Agonidae", "Threespine Stickleback",
                                                                                                         "Tubesnout")])

names(both.pooled.prop.wide)
both.pooled.prop.wide$Pancrustacea <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Pancrustacea", "Amphipoda", "Caprellidae", "Gammaridae",
                                                                                                          "Hyperiidae", "Hyperia medusarum", "Themisto pacifica",
                                                                                                          "Primno spp.", "Copepoda", "Decapoda", 
                                                                                                          "Paguridae Megalopae", "Neotrypaea spp.", "Brachyura",
                                                                                                          "Brachyura Zoeae", "Brachyura Megalopae", 
                                                                                                          "Cancridae Zoeae", "Cancridae Megalopae", "Pandalus spp.",
                                                                                                          "Pasiphaea spp.", "Sergestidae", "Euphausiidae",
                                                                                                          "Isopoda", "Mysidae", "Insecta", "Pinnotheridae")])
both.pooled.prop.wide$Amphipoda <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Amphipoda", "Caprellidae", "Gammaridae", "Hyperiidae", 
                                                                                                       "Hyperia medusarum", "Themisto pacifica", "Primno spp.")])
both.pooled.prop.wide$Hyperiidae <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Hyperiidae", "Hyperia medusarum", "Themisto pacifica")])

both.pooled.prop.wide$Decapoda <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Decapoda", "Paguridae Megalopae", "Neotrypaea spp.",
                                                                                                      "Brachyura", "Brachyura Zoeae", "Brachyura Megalopae",
                                                                                                      "Cancridae Zoeae", "Cancridae Megalopae", "Pandalus spp.",
                                                                                                      "Pasiphaea spp.", "Sergestidae", "Pinnotheridae")])
both.pooled.prop.wide$Brachyura <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Brachyura", "Brachyura Zoeae", "Brachyura Megalopae",
                                                                                                       "Cancridae Zoeae", "Cancridae Megalopae", "Pinnotheridae")])
both.pooled.prop.wide$`Brachyura Megalopae` <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Brachyura Megalopae", "Cancridae Megalopae")])
both.pooled.prop.wide$`Brachyura Zoeae` <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Brachyura Zoeae", "Cancridae Zoeae")])
both.pooled.prop.wide$Cancridae <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Cancridae Zoeae", "Cancridae Megalopae")])
both.pooled.prop.wide$Caridea <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Pandalus spp.", "Pasiphaea spp.")])

both.pooled.prop.wide$Cephalopoda <- rowSums(both.pooled.prop.wide[, names(both.pooled.prop.wide) %in% c("Cephalopoda", "Octopoda", "Teuthida")])
names(both.pooled.prop.wide)

both.pooled.prop <- as.data.frame(t(both.pooled.prop.wide))
both.pooled.prop$PreyCategory <- rownames(both.pooled.prop)
both.pooled.prop <- relocate(both.pooled.prop, PreyCategory)
rownames(both.pooled.prop) <- c()

both.pooled.prop <- subset(both.pooled.prop, PreyCategory != "Empty")
view(both.pooled.prop)

# Combine tables ----
view(both.indiv.prop) # 68
view(both.pooled.prop) # 68
view(both.stomach.fo) # 69
view(both.intestine.fo) # 25

view(subset(both.indiv.prop, !PreyCategory %in% both.pooled.prop$PreyCategory))
view(subset(both.pooled.prop, !PreyCategory %in% both.indiv.prop$PreyCategory))
view(subset(both.stomach.fo, !PreyCategory %in% both.indiv.prop$PreyCategory)) # Crangonidae
view(both.intestine.fo)
# use this to make sure I have all levels of the hierarchy in every column it should be

# Table with 4 columns

to.merge <- list(both.indiv.prop, both.pooled.prop, both.stomach.fo, both.intestine.fo)

all.table <- Reduce(function(x, y) merge(x, y, all = TRUE), to.merge, accumulate = FALSE)
nrow(all.table) # 69
view(all.table)

names(all.table)

all.table <- relocate(all.table, ChinookMeanProportion, .after = PreyCategory)
all.table <- relocate(all.table, ChinookPooledProportion, .after = ChinookMeanProportion)
all.table <- relocate(all.table, ChinookStomachFO, .after = ChinookPooledProportion)
all.table <- relocate(all.table, ChinookIntestineFO, .after = ChinookStomachFO)
all.table <- relocate(all.table, CohoMeanProportion, .after = ChinookIntestineFO)
all.table <- relocate(all.table, CohoPooledProportion, .after = CohoMeanProportion)
all.table <- relocate(all.table, CohoStomachFO, .after = CohoPooledProportion)
all.table <- relocate(all.table, CohoIntestineFO, .after = CohoStomachFO)

view(all.table)

# Convert NAs to zero, but not for intestine FO
names(all.table)
all.table[2:4][is.na(all.table[2:4])] <- 0
all.table[6:8][is.na(all.table[6:8])] <- 0
view(all.table)

# order rows
all.table$OrderRows <- rep(NA, nrow(all.table))

sort(unique(all.table$PreyCategory))

all.table$OrderRows[all.table$PreyCategory == "Osteichthyes"] <- 1
all.table$OrderRows[all.table$PreyCategory == "Clupeiformes"] <- 2
all.table$OrderRows[all.table$PreyCategory == "Pacific Herring"] <- 3
all.table$OrderRows[all.table$PreyCategory == "Northern Anchovy"] <- 4
all.table$OrderRows[all.table$PreyCategory == "Bathylagidae"] <- 5
all.table$OrderRows[all.table$PreyCategory == "Oncorhynchus spp."] <- 6
all.table$OrderRows[all.table$PreyCategory == "Chinook Salmon"] <- 7
all.table$OrderRows[all.table$PreyCategory == "Chum Salmon"] <- 8
all.table$OrderRows[all.table$PreyCategory == "Pink Salmon"] <- 9
all.table$OrderRows[all.table$PreyCategory == "Osmeridae"] <- 10
all.table$OrderRows[all.table$PreyCategory == "Eulachon"] <- 11
all.table$OrderRows[all.table$PreyCategory == "Surf Smelt"] <- 12
all.table$OrderRows[all.table$PreyCategory == "Barracudina"] <- 13
all.table$OrderRows[all.table$PreyCategory == "Myctophidae"] <- 14
all.table$OrderRows[all.table$PreyCategory == "Gadiformes"] <- 15
all.table$OrderRows[all.table$PreyCategory == "Gadus spp."] <- 16
all.table$OrderRows[all.table$PreyCategory == "Pacific Cod"] <- 17
all.table$OrderRows[all.table$PreyCategory == "Walleye Pollock"] <- 18
all.table$OrderRows[all.table$PreyCategory == "Pacific Hake"] <- 19
all.table$OrderRows[all.table$PreyCategory == "Plainfin Midshipman"] <- 20
all.table$OrderRows[all.table$PreyCategory == "Carangiformes"] <- 21
all.table$OrderRows[all.table$PreyCategory == "Pleuronectoidei"] <- 22
all.table$OrderRows[all.table$PreyCategory == "Jack Mackerel"] <- 23
all.table$OrderRows[all.table$PreyCategory == "Embiotocidae"] <- 24
all.table$OrderRows[all.table$PreyCategory == "Pacific Saury"] <- 25
all.table$OrderRows[all.table$PreyCategory == "Perciformes"] <- 26
all.table$OrderRows[all.table$PreyCategory == "Prowfish"] <- 27
all.table$OrderRows[all.table$PreyCategory == "Pacific Sand Lance"] <- 28
all.table$OrderRows[all.table$PreyCategory == "Scorpaenidae"] <- 29
all.table$OrderRows[all.table$PreyCategory == "Threespine Stickleback"] <- 30
all.table$OrderRows[all.table$PreyCategory == "Tubesnout"] <- 31
all.table$OrderRows[all.table$PreyCategory == "Sablefish"] <- 32
all.table$OrderRows[all.table$PreyCategory == "Lingcod"] <- 33
all.table$OrderRows[all.table$PreyCategory == "Staghorn Sculpin"] <- 34
all.table$OrderRows[all.table$PreyCategory == "Agonidae"] <- 35

all.table$OrderRows[all.table$PreyCategory == "Pancrustacea"] <- 36
all.table$OrderRows[all.table$PreyCategory == "Amphipoda"] <- 37
all.table$OrderRows[all.table$PreyCategory == "Gammaridae"] <- 38
all.table$OrderRows[all.table$PreyCategory == "Hyperiidae"] <- 39
all.table$OrderRows[all.table$PreyCategory == "Hyperia medusarum"] <- 40
all.table$OrderRows[all.table$PreyCategory == "Themisto pacifica"] <- 41
all.table$OrderRows[all.table$PreyCategory == "Primno spp."] <- 42
all.table$OrderRows[all.table$PreyCategory == "Caprellidae"] <- 43
all.table$OrderRows[all.table$PreyCategory == "Copepoda"] <- 44

all.table$OrderRows[all.table$PreyCategory == "Decapoda"] <- 45
all.table$OrderRows[all.table$PreyCategory == "Brachyura"] <- 46
all.table$OrderRows[all.table$PreyCategory == "Brachyura Zoeae"] <- 47
all.table$OrderRows[all.table$PreyCategory == "Brachyura Megalopae"] <- 48
all.table$OrderRows[all.table$PreyCategory == "Cancridae"] <- 49
all.table$OrderRows[all.table$PreyCategory == "Cancridae Zoeae"] <- 50
all.table$OrderRows[all.table$PreyCategory == "Cancridae Megalopae"] <- 51
all.table$OrderRows[all.table$PreyCategory == "Pinnotheridae"] <- 52
all.table$OrderRows[all.table$PreyCategory == "Caridea"] <- 53
all.table$OrderRows[all.table$PreyCategory == "Pandalus spp."] <- 54
all.table$OrderRows[all.table$PreyCategory == "Pasiphaea spp."] <- 55
all.table$OrderRows[all.table$PreyCategory == "Crangonidae"] <- 56
all.table$OrderRows[all.table$PreyCategory == "Sergestidae"] <- 57
all.table$OrderRows[all.table$PreyCategory == "Paguridae Megalopae"] <- 58
all.table$OrderRows[all.table$PreyCategory == "Neotrypaea spp."] <- 59
all.table$OrderRows[all.table$PreyCategory == "Euphausiidae"] <- 60
all.table$OrderRows[all.table$PreyCategory == "Mysidae"] <- 61
all.table$OrderRows[all.table$PreyCategory == "Isopoda"] <- 62
all.table$OrderRows[all.table$PreyCategory == "Insecta"] <- 63

all.table$OrderRows[all.table$PreyCategory == "Cephalopoda"] <- 64
all.table$OrderRows[all.table$PreyCategory == "Teuthida"] <- 65
all.table$OrderRows[all.table$PreyCategory == "Octopoda"] <- 66
all.table$OrderRows[all.table$PreyCategory == "Polychaeta"] <- 67
all.table$OrderRows[all.table$PreyCategory == "Other"] <- 68
all.table$OrderRows[all.table$PreyCategory == "Unidentified Material"] <- 69

all.table <- all.table[order(all.table$OrderRows), ]

# subset(all.table, is.na(OrderRows))
# unique(table(all.table$OrderRows))

# remove row order column
all.table <- all.table[, !names(all.table) == "OrderRows"]

# convert proportions to percentages
names(all.table)
all.table[2:9] <- all.table[2:9] * 100
view(all.table)

# where rounds to 0, but acutally >0%, use <0.1%
names(all.table)
view(all.table)

all.table$ChinookMWRound <- formatC(as.numeric(as.character(round(all.table$ChinookMeanProportion, 1))),
                                digits = 1, format = "f")
all.table$ChinookMWRound[all.table$ChinookMeanProportion > 0 & all.table$ChinookMeanProportion < 0.1] <- "<0.1"

all.table$ChinookPWRound <- formatC(as.numeric(as.character(round(all.table$ChinookPooledProportion, 1))),
                                digits = 1, format = "f")
all.table$ChinookPWRound[all.table$ChinookPooledProportion > 0 & all.table$ChinookPooledProportion < 0.1] <- "<0.1"

all.table$ChinookStomachFORound <- formatC(as.numeric(as.character(round(all.table$ChinookStomachFO, 1))),
                                digits = 1, format = "f")
all.table$ChinookStomachFORound[all.table$ChinookStomachFO > 0 & all.table$ChinookStomachFO < 0.1] <- "<0.1"

all.table$ChinookIntestineFORound <- formatC(as.numeric(as.character(round(all.table$ChinookIntestineFO, 1))),
                                          digits = 1, format = "f")
all.table$ChinookIntestineFORound[all.table$ChinookIntestineFO > 0 & all.table$ChinookIntestineFO < 0.1] <- "<0.1"

all.table$CohoMWRound <- formatC(as.numeric(as.character(round(all.table$CohoMeanProportion, 1))),
                             digits = 1, format = "f")
all.table$CohoMWRound[all.table$CohoMeanProportion > 0 & all.table$CohoMeanProportion < 0.1] <- "<0.1"

all.table$CohoPWRound <- formatC(as.numeric(as.character(round(all.table$CohoPooledProportion, 1))),
                             digits = 1, format = "f")
all.table$CohoPWRound[all.table$CohoPooledProportion > 0 & all.table$CohoPooledProportion < 0.1] <- "<0.1"

all.table$CohoStomachFORound <- formatC(as.numeric(as.character(round(all.table$CohoStomachFO, 1))),
                             digits = 1, format = "f")
all.table$CohoStomachFORound[all.table$CohoStomachFO > 0 & all.table$CohoStomachFO < 0.1] <- "<0.1"

all.table$CohoIntestineFORound <- formatC(as.numeric(as.character(round(all.table$CohoIntestineFO, 1))),
                                       digits = 1, format = "f")
all.table$CohoIntestineFORound[all.table$CohoIntestineFO > 0 & all.table$CohoIntestineFO < 0.1] <- "<0.1"


view(all.table)
names(all.table)

all.table <- relocate(all.table, ChinookMWRound, .after = ChinookMeanProportion)
all.table <- relocate(all.table, ChinookPWRound, .after = ChinookPooledProportion)
all.table <- relocate(all.table, ChinookStomachFORound, .after = ChinookStomachFO)
all.table <- relocate(all.table, ChinookIntestineFORound, .after = ChinookIntestineFO)
all.table <- relocate(all.table, CohoMWRound, .after = CohoMeanProportion)
all.table <- relocate(all.table, CohoPWRound, .after = CohoPooledProportion)
all.table <- relocate(all.table, CohoStomachFORound, .after = CohoStomachFO)
all.table <- relocate(all.table, CohoIntestineFORound, .after = CohoIntestineFO)
view(all.table)
# save as the supplemental table, code for this is below the main text table

# main text table, which only includes prey items >1% for at least one of the metrics in at least one species
names(all.table)
main.table.unrounded <- subset(all.table, ChinookMeanProportion >= 1 | ChinookPooledProportion >= 1 | ChinookStomachFO >= 1 |
                                       ChinookIntestineFO >= 1 | CohoMeanProportion >= 1 | CohoPooledProportion >= 1 |
                                       CohoStomachFO >= 1 | CohoIntestineFO >= 1)
nrow(main.table.unrounded) # 36
view(main.table.unrounded)

main.table.round <- select(main.table.unrounded, PreyCategory, ChinookMWRound, ChinookPWRound, ChinookStomachFORound, 
                         ChinookIntestineFORound, CohoMWRound, CohoPWRound, CohoStomachFORound, CohoIntestineFORound)
view(main.table.round)

main.table.round[main.table.round == "NA"] <- NA
view(main.table.round)

main.table <- flextable(main.table.round, cwidth = 1.7)
main.table <- set_header_labels(main.table, PreyCategory = "", ChinookMWRound = "%MW", ChinookPWRound = "%PW", 
                                            ChinookStomachFORound = "%SFO", ChinookIntestineFORound = "%IFO",
                                            CohoMWRound = "%MW", CohoPWRound = "%PW", CohoStomachFORound = "%SFO",
                                            CohoIntestineFORound = "%IFO")
                                
main.table <- add_header_row(main.table, colwidths = c(1, 4, 4), values = c("", "Chinook Salmon", "Coho Salmon"))
main.table <- add_header_row(main.table, top = FALSE, values = c("Sample Size", "2989", "2981", "2997", "2875",
                                                                 "466", "466", "475", "456"))
main.table <- add_header_row(main.table, top = FALSE, values = c("% Empty", "21.6", "21.5", "21.4", "16.7",
                                                                 "16.1", "16.1", "15.8", "25.4"))

main.table <- bold(main.table, i = c(1:2), bold = TRUE, part = "header")

main.table <- line_spacing(main.table, space = 0.3)
main.table <- line_spacing(main.table, i = 1, part = "body", space = 0.8) # adds more space between header and body

main.table

main.table <- align(main.table, i = 1, align = "center", part = "header") # centre Chinook and coho
main.table <- align(main.table, i = c(2:4), align = "right", part = "header")
main.table <- align(main.table, i = c(3, 4), j = 1, align = "left", part = "header")
main.table <- align(main.table, j = 2:9, align = "right", part = "body")

# add a row number for formatting
rownames(main.table.round) <- seq(1, nrow(main.table.round))
main.table.round$PreyCategory

# bold major groups (fish, crustaceans, cephalopods, polychaeta, other, unid material)
main.table <- bold(main.table, i = c(1, 16, 32, 34, 35, 36), j = 1, part = "body", bold = TRUE)

# italicize Latin names, non-italicize "spp." in Word
main.table <- italic(main.table, i = c(5, 20, 30), j = 1, part = "body", italic = TRUE) 

# looked better when spacing was 16, but used 14 to fit

# indent order or family in some cases
main.table <- padding(main.table, i = c(2, 5, 6, 8, 9, 11, 12, 17, 31, 33, 21), 
                                  j = 1, padding.left = 14)

# indent higher species (for fish) or infraorder/family for Pancrustacea
main.table <- padding(main.table, i = c(3, 4, 7, 10, 13, 14, 15, 22, 29),
                                  j = 1, padding.left = 28)

main.table <- padding(main.table, i = c(18, 19, 25, 28, 30), j = 1, padding.left = 42)

main.table <- padding(main.table, i = c(20, 23, 24, 26, 27), j = 1, padding.left = 56)

main.table <- width(main.table, j = 1, width = 2.35, unit = "in")
main.table <- width(main.table, j = c(2:9), width = 0.6, unit = "in")

main.table <- bg(main.table, bg = "white", part = "all")

main.table
#save_as_docx(main.table, path = "tables/table1_major_diet_composition.docx")

# Supplemental table
view(all.table)

all.table.round <- select(all.table, PreyCategory, ChinookMWRound, ChinookPWRound, 
                                        ChinookStomachFORound, ChinookIntestineFORound,
                                        CohoMWRound, CohoPWRound, CohoStomachFORound, CohoIntestineFORound)
all.table.round[all.table.round == "NA"] <- NA

view(all.table.round)

supplemental.table <- flextable(all.table.round, cwidth = 1.7)
supplemental.table <- set_header_labels(supplemental.table, PreyCategory = "", ChinookMWRound = "%MW", ChinookPWRound = "%PW", 
                                ChinookStomachFORound = "%SFO", ChinookIntestineFORound = "%IFO",
                                CohoMWRound = "%MW", CohoPWRound = "%PW", CohoStomachFORound = "%SFO",
                                CohoIntestineFORound = "%IFO")

supplemental.table <- add_header_row(supplemental.table, colwidths = c(1, 4, 4), 
                                     values = c("", "Chinook Salmon", "Coho Salmon"))
supplemental.table <- add_header_row(supplemental.table, top = FALSE, 
                                     values = c("Sample Size", "2989", "2981", "2997", "2875",
                                                 "466", "466", "475", "456"))
supplemental.table <- add_header_row(supplemental.table, top = FALSE, 
                                     values = c("% Empty", "21.6", "21.5", "21.4", "16.7",
                                                 "16.1", "16.1", "15.8", "25.4"))
# double check sample size/% empty to make sure up to date

supplemental.table <- bold(supplemental.table, i = c(1:2), bold = TRUE, part = "header")

supplemental.table <- line_spacing(supplemental.table, space = 0.3)
supplemental.table <- line_spacing(supplemental.table, i = 1, part = "body", space = 0.8) 
# adds more space between header and body

supplemental.table <- align(supplemental.table, i = 1, align = "center", part = "header") # centre Chinook and coho
supplemental.table <- align(supplemental.table, i = c(2:4), align = "right", part = "header")
supplemental.table <- align(supplemental.table, i = c(3, 4), j = 1, align = "left", part = "header")
supplemental.table <- align(supplemental.table, j = 2:9, align = "right", part = "body")

all.table.round$PreyCategory

# bold major groups (fish, crustaceans, cephalopods, polychaeta, other, unid material)
supplemental.table <- bold(supplemental.table, i = c(1, 36, 64, 67, 68, 69), j = 1, part = "body", bold = TRUE)

# italicize Latin names, non-italicize "spp." in Word
supplemental.table <- italic(supplemental.table, i = c(6, 16, 40, 41, 42, 54, 55, 59),
                             j = 1, part = "body", italic = TRUE) 

# indent order or family in some cases
supplemental.table <- padding(supplemental.table, i = c(2, 5, 6, 10, 13, 14, 15, 20, 21, 24, 25, 26,
                                                        37, 44, 45, 60, 61, 62, 63, 65, 66), 
                      j = 1, padding.left = 14)

# indent higher species (for fish) or infraorder/family for Pancrustacea
supplemental.table <- padding(supplemental.table, i = c(3, 4, 7, 8, 9, 11, 12, 16, 19, 22, 23, 27,
                                                        28, 29, 30, 31, 32, 33, 34, 35, 38, 39, 42, 43,
                                                        46, 53, 57, 58, 59),
                      j = 1, padding.left = 28)

supplemental.table <- padding(supplemental.table, i = c(17, 18, 40, 41, 49, 52,
                                                        54, 55, 56),
                              j = 1, padding.left = 42)

supplemental.table <- padding(supplemental.table, i = c(47, 48, 50, 51), j = 1, padding.left = 56)

supplemental.table <- width(supplemental.table, j = 1, width = 2.35, unit = "in")
supplemental.table <- width(supplemental.table, j = c(2:9), width = 0.6, unit = "in")

supplemental.table <- bg(supplemental.table, bg = "white", part = "all")

supplemental.table
#save_as_docx(supplemental.table, path = "tables/tableS1_diet_composition_all.docx")
