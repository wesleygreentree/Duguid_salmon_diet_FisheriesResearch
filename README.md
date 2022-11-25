Data and code accompanying:

**A robust, economical diet sampling program for recreationally harvested salmon in British Columbia**

William D. P. Duguid, Nick Bohlender, Katie G. Innes, Wesley L. Greentree, Chloe Kraemer, Bridget Maher, Jessica Qualley, Micah Quindazzi and Francis Juanes

willduguid\@hotmail.com

Submitted to *Fisheries Research*, November 2022

**Description of files**

Scripts: R code used for analysis and production of figures and tables

-   `sampling_coverage.R` produces Figure 1, which visualizes the distribution of samples geographically and in each month

-   `diet_composition.R` produces Table 1 and Table S1, which display 4 metrics of diet composition for Chinook and Coho Salmon

-   `ID_levels.R` produces Figure 3, which visualizes the application of the prey identification framework

Data: exports from version 1.0 of the Adult Salmon Diet Program Database and associated files

-   ASDPsalmon_v1.0.xlsx: catch data for salmon captured by recreational fishers over April 2017-March 2022

    -   `Fish Code` unique identifier assigned to each digestive tract

    -   `Species` salmon species; ch = Chinook Salmon; co = Coho Salmon

    -   `Month` month of capture

    -   `Year` year of capture

    -   `yearMonth` capture year and capture month pasted together to allow subsetting of dataset by seasons/study period

    -   `Creel Site Name` standardized capture site, using either Fisheries and Oceans Canada fishing location ("creel sites") or Adult Salmon Diet Program-specific fishing locations

    -   `PFMA` Pacific Fishery Management Area (Fisheries and Oceans Canada)

    -   `Region` levels: Salish Sea, West Coast Vancouver Island, Central Coast, Haida Gwaii

    -   `Longitude` fishing site longitude

    -   `Latitude` fishing site latitude

-   ASDPstomachs_v1.0.xlsx: weights for prey categories in salmon stomachs. In this export, prey items are aggregated by `OriginalID`, `FinalID`, `Digestion`, and `Confidence`. Each weight may represent multiple prey items.

    -   `Fish Code` unique identifier assigned to each digestive tract

    -   `OriginalID` initial prey identification based on external morphology

    -   `FinalID` final prey identification using identification framework, including diagnostic hard parts

    -   `Digestion` prey digestion score; levels: 1, 2, 3, 4

    -   `Confidence` designates if FinalID is highly confident (TRUE) or not (FALSE), based on systematic application of diagnostic hard parts

    -   `Weight` weight of prey class (in grams)

-   ASDPintestines_v1.0.xlsx: prey presence in salmon intestines.

    -   `Fish Code` unique identifier assigned to each digestive tract

    -   `Final ID` final prey identification using identification framework, including diagnostic hard parts

    -   `Just 1` - `Just 9` codes for diagnostic hard parts found in intestine and used for identification

    -   `DummyValue` repeated 1, to allow coding of presence (1)/absence (0) matrix

-   ASDP_stomachs_to_remove.xlsx: a small proportion of stomachs are not appropriate for all diet composition metrics used in this study, and must be removed prior to calculation of one or more metrics

    -   `Fish Code` unique identifier assigned to each digestive tract

    -   `Keep for FO` indicates stomachs that must be removed before Frequency of Occurrence (%FO) calculation (indicated by `Keep for FO` = "no")

    -   `Keep for Proportion` indicates stomachs that must be removed before Individual-level Mean % Weight (%MW) calculation (indicated by `Keep for Proportion` = "no")

    -   `Keep for Fullness` indicates stomachs that must be removed before Pooled % Weight (%PW) calculation (indicated by `Keep for Fullness` = "no")
