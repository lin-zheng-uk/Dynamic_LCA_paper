install.packages('bibliometrix', dependencies = TRUE)
library(bibliometrix)
install.packages('tidyverse')
library(tidyverse)
install.packages('ggpubr')
library(ggpubr)
install.packages('ggplot2')
library(ggplot2)
library(dplyr)
library(igraph)

# set the working directory
setwd("/Users/lin/Dev/Dynamic_LCA_paper")
# Converting collection into a bibliographic dataframe
M_BIM <- convert2df(file = './Article_selection_Wos_Scopus/selection_WOS_BIM.bib', dbsource = "isi", format = "bibtex")
M_ML <- convert2df(file = './Article_selection_Wos_Scopus/selection_WOS_ML.bib', dbsource = "isi", format = "bibtex")
M_GIS <- convert2df(file = './Article_selection_Wos_Scopus/selection_WOS_GIS.bib', dbsource = "isi", format = "bibtex")

# merge the three dataframes
M <- merge(merge(M_BIM, M_GIS, all = TRUE), M_ML, all = TRUE)
# save the merged table as csv into the current working directory, M=Table: 489*57
write.csv(M, file = './Generated_Data_table/overall_wos_selection.csv', row.names = TRUE)

#-----validation the WOS results by scopus results-----
# validation the WOS results by scopus, check whether the selected articles are in the scopus database, using DOI as the key
# import csv files from scopus database
M_BIM_scopus <- read.csv("./Article_selection_Wos_Scopus/scopus_BIM.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
M_ML_scopus <- read.csv("./Article_selection_Wos_Scopus/scopus_ML.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
M_GIS_scopus <- read.csv("./Article_selection_Wos_Scopus/scopus_GIS.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# merge the scopus dataframes, M_scopus=Table: 346*36
M_scopus <- merge(merge(M_BIM_scopus, M_ML_scopus, all = TRUE), M_GIS_scopus, all = TRUE)
# import the WOS dataframes
M_wos <- read.csv("./Generated_Data_table/overall_wos_selection.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# rename columns name in the scopus dataframe to match the WOS dataframe
colnames(M_scopus)[colnames(M_scopus) == "Title"] <- "TI"
# capitilize the value in column "TI" in the scopus dataframe
M_scopus$TI <- toupper(M_scopus$TI)
# delete the rows with missing values in the column "TI" and "TI" contain words "review"
M_scopus <- M_scopus[!is.na(M_scopus$TI), ]
M_scopus <- M_scopus[!grepl("REVIEW", M_scopus$TI), ]
# check whether there are articles in the scopus database but not in the WOS database, by key "TI" or "DI", either TI or DI can be used as the key
M_scopus_not_in_wos <- M_scopus[!(M_scopus$DI %in% M_wos$DOI), ]
# print the row number of the M_scopus_not_in_wos, so we can see how many articles in the scopus database are not in the WOS database
print(dim(M_scopus_not_in_wos))
# so we can see M_scopus_not_in_wos contains 0 rows, which means all the articles in the scopus database are in the WOS database.


# continue using the WOS dataframes to conduct the bibliometric analysis

# ------check overall summary
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
S$MainInformationDF

# ------check summary for three methods separately
results_BIM <- biblioAnalysis(M_BIM, sep = ";")
options(width=100)
S_BIM <- summary(object = results_BIM, k = 10, pause = FALSE)
S_BIM$MainInformationDF

results_ML <- biblioAnalysis(M_ML, sep = ";")
options(width=100)
S_ML <- summary(object = results_ML, k = 10, pause = FALSE)
S_ML$MainInformationDF

results_GIS <- biblioAnalysis(M_GIS, sep = ";")
options(width=100)
S_GIS <- summary(object = results_GIS, k = 10, pause = FALSE)
S_GIS$MainInformationDF

# ------check ciations

# Extract years and the yearly average number of times each manuscript has been cited
years <- results$Years
citations <- results$TotalCitation
# Combine years and average_citations_per_paper_per_year
citation_data <- data.frame(Year = years, TotalCitations = citations)
# Summarize total citations for each year
total_citations_per_paper_per_year <- aggregate(TotalCitations ~ Year, data = citation_data, sum)
# Print the result
print(total_citations_per_paper_per_year)

years_BIM <- results_BIM$Years
citations_BIM <- results_BIM$TotalCitation
# Combine years and average_citations_per_paper_per_year
citation_data_BIM <- data.frame(Year = years_BIM, TotalCitations = citations_BIM)
# Summarize total citations for each year
total_citations_per_paper_per_year_BIM <- aggregate(TotalCitations ~ Year, data = citation_data_BIM, sum)
# Print the result
print(total_citations_per_paper_per_year_BIM)

years_ML <- results_ML$Years
citations_ML <- results_ML$TotalCitation
# Combine years and average_citations_per_paper_per_year
citation_data_ML <- data.frame(Year = years_ML, TotalCitations = citations_ML)
# Summarize total citations for each year
total_citations_per_paper_per_year_ML <- aggregate(TotalCitations ~ Year, data = citation_data_ML, sum)
# Print the result
print(total_citations_per_paper_per_year_ML)

years_GIS <- results_GIS$Years
citations_GIS <- results_GIS$TotalCitation
# Combine years and average_citations_per_paper_per_year
citation_data_GIS <- data.frame(Year = years_GIS, TotalCitations = citations_GIS)
# Summarize total citations for each year
total_citations_per_paper_per_year_GIS <- aggregate(TotalCitations ~ Year, data = citation_data_GIS, sum)
# Print the result
print(total_citations_per_paper_per_year_GIS)

# ------check most citations paper/ instiuation and save results as csv
S$MostCitedPapers
write.csv(S$MostCitedPapers, file = "/Users/lin/Dev/Phd-Project/Dynamic_LCA_literature_search/Top_documents.csv", row.names = FALSE)

S$MostProdCountries
write.csv(S$MostProdCountries, file = "/Users/lin/Dev/Phd-Project/Dynamic_LCA_literature_search/Top_country.csv", row.names = FALSE)

S$TCperCountries
write.csv(S$TCperCountries, file = "/Users/lin/Dev/Phd-Project/Dynamic_LCA_literature_search/Top_country_citations.csv", row.names = FALSE)


# ----------h-index?????
# H <- Hindex(M, field = "source", elements = NULL, sep = ";", years = 2023)
authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
write.csv(indices$H, file = "/Users/lin/Dev/Phd-Project/Dynamic_LCA_literature_search/CF_Top_Citations_list.csv", row.names = FALSE)
indices$H

results$Authors[names(results$Authors) == "GARCIA-MARTINEZ A "]

# ----------Co-word analysis
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix,  n = 20, Title = " ",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)

# ----------plot
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
head(topAU$dfAU)

# --------

S_BIM$MostCitedPapers
write.csv(S_BIM$MostCitedPapers, file = "/Users/lin/Dev/Phd-Project/Dynamic_LCA_literature_search/CF_Top_documents_BIM.csv", row.names = FALSE)

S_ML$MostCitedPapers

first_five_rows <- S_GIS$MostCitedPapers[1:5, ]
first_five_rows