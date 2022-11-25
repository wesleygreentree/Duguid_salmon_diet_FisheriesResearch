## Figure 1: Sampling coverage by region and month

# load packages
library(readxl)
library(tidyverse)
library(PBSmapping)
library(ggpubr)

# load data
salmon.data <- read_excel("data/ASDPsalmon_v1.0.xlsx")

table(salmon.data$Species) # 2997 Chinook, 475 coho

# assign season
salmon.data$Season <- rep(NA, nrow(salmon.data))
salmon.data$Season[salmon.data$Month %in% c(4, 5, 6, 7, 8, 9)] <- "Summer"
salmon.data$Season[salmon.data$Month %in% c(10, 11, 12, 1, 2, 3)] <- "Winter"

# convert lat/long to numeric
salmon.data$Longitude <- as.numeric(salmon.data$Longitude)
salmon.data$Latitude <- as.numeric(salmon.data$Latitude)

## Group by species, region, month ---
species.region.month <- salmon.data %>% group_by(Species, Region, Month) %>% tally()
head(species.region.month)

species.region.month$Species[species.region.month$Species == "ch"] <- "Chinook"
species.region.month$Species[species.region.month$Species == "co"] <- "Coho"

species.region.month
species.region.month.ch <- subset(species.region.month, Species == "Chinook")
species.region.month.co <- subset(species.region.month, Species == "Coho")

## Chinook monthly sample coverage ---
species.region.month.ch$Region <- factor(species.region.month.ch$Region, 
                                         levels = c("Haida Gwaii", "Central Coast",
                                                    "WCVI", "Salish Sea"))

ch.n <- ggplot() + 
  geom_col(data = species.region.month.ch, aes(x = Month, y = n, fill = Region)) +
  ylab("Number of stomachs") +
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1), 
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(lim = c(0, 870), breaks = c(0, 200, 400, 600, 800), 
                     expand = c(0.01, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 7.5, colour = "black"),
        axis.title.x = element_text(vjust = 2),
        axis.text = element_text(size = 6.5, colour = "black"),
        axis.line = element_line(size = 0.3, colour = "black"),
        axis.ticks = element_line(size = 0.3, colour = "black"),
        legend.position = "none",
        plot.margin = margin(b = 0.1)) + 
  scale_fill_manual(values = c("#E26700", "#6a3d9a", "#33a02c", "#1f78b4"))
ch.n

## Coho monthly sample coverage ---
species.region.month.co

# Coho not sampled in all months, add Jan and Feb rows with n = 0 for plotting purposes
co.jan <- c("Coho", "Salish Sea", 1, 0)
co.feb <- c("Coho", "Salish Sea", 2, 0)

co.jan.feb <- data.frame(rbind(co.jan, co.feb))
names(co.jan.feb) <- c("Species", "Region", "Month", "n")
co.jan.feb$Month <- as.numeric(co.jan.feb$Month)
co.jan.feb$n <- as.numeric(co.jan.feb$n)

species.region.month.co <- rbind(species.region.month.co, co.jan.feb)

species.region.month.co$Region <- factor(species.region.month.co$Region, 
                                         levels = c("Haida Gwaii", "Central Coast",
                                                    "WCVI", "Salish Sea"))
co.n <- ggplot() +
  geom_col(data = species.region.month.co, aes(x = Month, y = n, fill = Region)) +
  ylab("Number of stomachs") +
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1), 
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(lim = c(0, 160), breaks = c(0, 50, 100, 150), 
                     expand = c(0.01, 0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 7.5, colour = "black"),
        axis.title.x = element_text(vjust = 2),
        axis.text = element_text(size = 6.5, colour = "black"),
        axis.line = element_line(size = 0.3, colour = "black"),
        axis.ticks = element_line(size = 0.3, colour = "black"),
        legend.position = "none",
        plot.margin = margin(b = 0.1)) +
  scale_fill_manual(values = c("#E26700", "#6a3d9a", "#33a02c", "#1f78b4")) 
co.n


## Sampling coverage map ---

# load basemap from PBSmapping
data(nepacLLhigh)

# get sample sizes by lat/long (including PFMA or subarea only stomachs)
coordinates.tally <- salmon.data %>% group_by(PFMA, Longitude, Latitude) %>% tally()
view(coordinates.tally)

coordinates.tally$Region <- rep(NA, nrow(coordinates.tally))
coordinates.tally$Region[coordinates.tally$PFMA %in% c(1, 101)] <- "Haida Gwaii"
coordinates.tally$Region[coordinates.tally$PFMA %in% c(13, 14, 15, 16, 17, 18, 19,
                                                       20, 28, 29)] <- "Salish Sea"
coordinates.tally$Region[coordinates.tally$PFMA %in% c(21, 23, 24, 25, 26, 27, 121,
                                                       123, 124, 125, 126)] <- "West Coast Vancouver Island"
coordinates.tally$Region[coordinates.tally$PFMA %in% c(6, 9, 11, 12)] <- "Central Coast"

# convert latitude and longitude to UTM zone 10, using PBSmapping::convUL
attr(nepacLLhigh, "zone") <- 10
nepacUTM <- convUL(nepacLLhigh)

head(coordinates.tally)
names(coordinates.tally)[2:3] <- c("X", "Y")

attr(coordinates.tally, "zone") <- 10
attr(coordinates.tally, "projection") <- "LL"

coordinates.tally <- convUL(coordinates.tally)

coordinates.tally$Region <- factor(coordinates.tally$Region, 
                                   levels = c("Haida Gwaii", "Central Coast",
                                  "West Coast Vancouver Island", "Salish Sea"))

map <- ggplot() +
  geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID), 
               fill = "grey85", col = "black", lwd = 0.075) +
  coord_equal(xlim = c(-300, 600), ylim = c(5190, 6150)) + 
  xlab("") + ylab("") + 
  geom_point(data = coordinates.tally, aes(x = X, y = Y, color = Region, size = n),
             alpha = 0.5, shape = 16) +
  scale_size_continuous(range = c(0.8, 9), name = "# of stomachs", 
                        breaks = c(10, 100, 300)) +
  
  # north arrow
  geom_segment(arrow = arrow(length = unit(0.2, "cm")),
               aes(x = 600, xend = 600, y = 6110, yend = 6160), size = 0.5) +
  annotate("text", x = 600, y = 6173, label = "N", fontface = "bold", size = 2.8) +
  
  # scale bar
  annotate("rect", xmin = 275, xmax = 325, ymin = 5168, ymax = 5176,
           col = "black", fill = "black", size = 0.2) +
  annotate("rect", xmin = 325, xmax = 375, ymin = 5168, ymax = 5176,
           col = "black", fill = "white", size = 0.2) +
  annotate("text", x = 325, y = 5157, label = "kilometres", size = 2.3) +
  annotate("text", x = 275, y = 5190, label = "0", size = 2.3) +
  annotate("text", x = 325, y = 5190, label = "50", size = 2.3) +
  annotate("text", x = 375, y = 5190, label = "100", size = 2.3) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 0.4, color = "black", fill = NA),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = c(0.17, 0.17), 
        legend.title = element_text(size = 7.5, 
                                    margin = margin(b = 0.1, t = 0,
                                                    l = 0, r = 0, "cm")),
        legend.text = element_text(size = 6.5),
        legend.key = element_blank(),
        legend.key.height = unit(0.2, "cm"),
        legend.spacing.y = unit(0.05, "cm"),
        legend.background = element_blank(),
        legend.key.width = unit(-0.3, "cm"),
        legend.title.align = 0,
        plot.margin = margin(t = 2)) +
  guides(color = guide_legend(override.aes = list(size = 1.8, shape = 19, 
                                                  alpha = 0.9), byrow = TRUE)) +
  guides(size = guide_legend(order = 1, override.aes = list(color = "grey50"),
                             byrow = TRUE)) +
  scale_color_manual(values = c("#E26700", "#6a3d9a", "#33a02c", "#1f78b4"))
map 

## Arrange plots together ---
ggarrange(map, NULL, ggarrange(ch.n, co.n, nrow = 2, labels = c("B", "C"),
                               hjust = 0, font.label = list(size = 8)),
          ncol = 3, labels = "A", widths = c(2, 0.02, 1.0),
          font.label = list(size = 8))

# save
ggsave("figures/figure1_sampling_coverage.PNG", width = 19, height = 13.4, 
       units = "cm", dpi = 1600, bg = "white")

