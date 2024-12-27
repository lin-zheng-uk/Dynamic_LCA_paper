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
install.packages('gridExtra')
library(gridExtra)

# set the working directory
setwd("/Users/lin/Dev/Dynamic_LCA_paper")

# import raw data that exported from the WOS database
# Converting collection into a bibliographic dataframe, check the number of rows and columns in the dataframe
# check the min and max value in column "PY" (year) in the dataframe to validate the collection

# BIM
M_BIM <- convert2df(file = './Article_selection_Wos_Scopus/selection_WOS_BIM.bib', dbsource = "wos", format = "bibtex")
print(dim(M_BIM))
#print the min and max value in column "PY" in the dataframe M_BIM
print(min(M_BIM$PY))
print(max(M_BIM$PY))
print(sum(M_BIM$PY == 2025)) # we can see that there is 1 row has the value 2025 in column "PY" which need to be revised
# we have to classify the max year 2025 as 2024 in order to get the annual growth rate correctly
M_BIM$PY[M_BIM$PY == 2025] <- 2024
# Now printing the maximum value again to validate the result
print(max(M_BIM$PY))

# ML
M_ML <- convert2df(file = './Article_selection_Wos_Scopus/selection_WOS_ML.bib', dbsource = "wos", format = "bibtex")
print(dim(M_ML))
print(min(M_ML$PY))
print(max(M_ML$PY))

# GIS
M_GIS <- convert2df(file = './Article_selection_Wos_Scopus/selection_WOS_GIS.bib', dbsource = "wos", format = "bibtex")
print(dim(M_GIS))
print(min(M_GIS$PY))
print(max(M_GIS$PY))

# combine the three dataframes, keep the duplicates
# Binding rows and tagging them
M <- bind_rows(M_BIM, M_ML, M_GIS, .id = "source")
print(dim(M))
# Now df_combined includes a 'source' column indicating the origin of each row
M_no_duplicates <- merge(merge(M_BIM, M_GIS, all = TRUE), M_ML, all = TRUE)
print(dim(M_no_duplicates))
number_of_duplicates <- nrow(M) - nrow(M_no_duplicates)
print(paste('Number of duplicates:', number_of_duplicates))

#-----validation the WOS results by scopus results, use "Title" or "DOI" as key-----
# validation the WOS results by scopus, check whether the selected articles are in the scopus database
# import bib files from scopus database and convert them into dataframes
M_BIM_scopus = convert2df(file = './Article_selection_Wos_Scopus/scopus_BIM.bib', dbsource = "scopus", format = "bibtex")
M_ML_scopus = convert2df(file = './Article_selection_Wos_Scopus/scopus_ML.bib', dbsource = "scopus", format = "bibtex")
M_GIS_scopus = convert2df(file = './Article_selection_Wos_Scopus/scopus_GIS.bib', dbsource = "scopus", format = "bibtex")

# validate BIM
M_BIM_wos <- M_BIM
print(dim(M_BIM_wos))
print(dim(M_BIM_scopus))
# Checking entries where either title or DOI does not match
M_scopus_not_in_wos_BIM <- M_BIM_scopus[!((M_BIM_scopus$TI %in% M_BIM_wos$TI) | (M_BIM_scopus$DI %in% M_BIM_wos$DI)), ]
print(dim(M_scopus_not_in_wos_BIM))
# the results show that M_scopus_not_in_wos contains 63 rows, which means 86 articles in the scopus database but not included in the WOS database.
# add these 63 articles into the final BIM dataframe
M_BIM_final <- bind_rows(M_BIM_wos, M_scopus_not_in_wos_BIM,.id = "source")
print(dim(M_BIM_final)) # 424+63=487
M_BIM <- M_BIM_final # update the BIM dataframe

# validate ML
M_ML_wos <- M_ML
print(dim(M_ML_wos))
print(dim(M_ML_scopus))
# Checking entries where either title or DOI does not match
M_scopus_not_in_wos_ML <- M_ML_scopus[!((M_ML_scopus$TI %in% M_ML_wos$TI) | (M_ML_scopus$DI %in% M_ML_wos$DI)), ]
print(dim(M_scopus_not_in_wos_ML))
# the results show that M_scopus_not_in_wos contains 7 rows, which means 7 articles in the scopus database but not included in the WOS database.
# add these 7 articles into the final ML dataframe
M_ML_final <- bind_rows(M_ML_wos, M_scopus_not_in_wos_ML,.id = "source")
print(dim(M_ML_final)) # 30+7=37
M_ML <- M_ML_final # update the ML dataframe

# validate GIS
M_GIS_wos <- M_GIS
print(dim(M_GIS_wos))
print(dim(M_GIS_scopus))
# Checking entries where either title or DOI does not match
M_scopus_not_in_wos_GIS <- M_GIS_scopus[!((M_GIS_scopus$TI %in% M_GIS_wos$TI) | (M_GIS_scopus$DI %in% M_GIS_wos$DI)), ]
print(dim(M_scopus_not_in_wos_GIS))
# the results show that M_scopus_not_in_wos contains 3 rows, which means 3 articles in the scopus database but not included in the WOS database.
# add these 3 articles into the final ML dataframe
M_GIS_final <- bind_rows(M_GIS_wos, M_scopus_not_in_wos_GIS,.id = "source")
print(dim(M_GIS_final)) # 20+3=23
M_GIS <- M_GIS_final # update the ML dataframe

# completed the validation, save the final dataframes as csv into the current working directory
# combine the three dataframes, keep the duplicat, binding rows and tagging them
M_final <- bind_rows(M_BIM, M_ML, M_GIS, .id = "source")
print(dim(M_final))
# save the merged table as csv into the current working directory, M=Table: 474*58
write.csv(M_final, file = './Generated_Data_table/overall_articles_selection.csv', row.names = TRUE)


# ------ Uising final completed dataframes to conduct the bibliometric method to generated the dataset we need ------

# Conducting bibliometric analysis
results <- biblioAnalysis(M_final, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
S$MainInformationDF
# print annual growth rate
S$AnnualGrowth

# ------check summary for three methods separately
results_BIM <- biblioAnalysis(M_BIM, sep = ";")
options(width=100)
S_BIM <- summary(object = results_BIM, k = 10, pause = FALSE)
S_BIM$MainInformationDF
# print annual growth rate
S_BIM$AnnualGrowth

results_ML <- biblioAnalysis(M_ML, sep = ";")
options(width=100)
S_ML <- summary(object = results_ML, k = 10, pause = FALSE)
S_ML$MainInformationDF
# print annual growth rate
S_ML$AnnualGrowth

results_GIS <- biblioAnalysis(M_GIS, sep = ";")
options(width=100)
S_GIS <- summary(object = results_GIS, k = 10, pause = FALSE)
S_GIS$MainInformationDF
# print annual growth rate
S_GIS$AnnualGrowth

# generae a table to show the annual growth rate for three methods
annual_growth_rate <- data.frame(Methods = c("BIM-LC", "ML-LC", "GIS-LC"), Annual_Growth_Rate = c(S_BIM$AnnualGrowth, S_ML$AnnualGrowth, S_GIS$AnnualGrowth))
annual_growth_rate
# save the results as csv
write.csv(annual_growth_rate, file = "./Generated_Data_table/annual_growth_rate.csv", row.names = FALSE)

# ------get the total number of publicans in each year
total_publications_all <- S$AnnualProduction
total_publications_all <- data.frame(Year = total_publications_all$Year, Total_Publications_All = total_publications_all$Articles)

total_publications_BIM <- S_BIM$AnnualProduction
total_publications_BIM <- data.frame(Year = total_publications_BIM$Year, Total_Publications_BIM = total_publications_BIM$Articles)

total_publications_ML <- S_ML$AnnualProduction
total_publications_ML <- data.frame(Year = total_publications_ML$Year, Total_Publications_ML = total_publications_ML$Articles)

total_publications_GIS <- S_GIS$AnnualProduction
total_publications_GIS <- data.frame(Year = total_publications_GIS$Year, Total_Publications_GIS = total_publications_GIS$Articles)

# merge: Combine all dataframes into one
years_df <- data.frame(Year = 2007:2024)
total_publications <- merge(years_df, total_publications_all, by = "Year", all = TRUE)
total_publications <- merge(total_publications, total_publications_BIM, by = "Year", all = TRUE)
total_publications <- merge(total_publications, total_publications_ML, by = "Year", all = TRUE)
total_publications <- merge(total_publications, total_publications_GIS, by = "Year", all = TRUE)
# Replace NA with 0
total_publications[is.na(total_publications)] <- 0
# Convert numeric columns to integers
total_publications$Total_Publications_ML <- as.integer(total_publications$Total_Publications_ML)
total_publications$Total_Publications_GIS <- as.integer(total_publications$Total_Publications_GIS)
colnames(total_publications) <- c("Year", "Total_Publications_All", "Total_Publications_BIM", "Total_Publications_ML", "Total_Publications_GIS")
print(total_publications)
# save results as csv
write.csv(total_publications, file = "./Generated_Data_table/total_number_of_publications_each_year.csv", row.names = FALSE)

# ------check ciations
# Extract years and the yearly average number of times each manuscript has been cited
years <- results$Years
citations <- results$TotalCitation
# Combine years and average_citations_per_paper_per_year
citation_data <- data.frame(Year = years, TotalCitations = citations)
# Summarize total citations for each year
total_citations_per_year_all <- aggregate(TotalCitations ~ Year, data = citation_data, sum)
# Print the result
print(total_citations_per_year_all)

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

# merge: Combine all dataframes into one
years_df <- data.frame(Year = 2007:2024)
total_citations_per_year <- merge(years_df, total_citations_per_year_all, by = "Year", all = TRUE)
colnames(total_citations_per_year) <- c("Year", "TotalCitations_All")
total_citations_per_year <- merge(total_citations_per_year, total_citations_per_paper_per_year_BIM, by = "Year", all = TRUE)
colnames(total_citations_per_year) <- c("Year", "TotalCitations_All", "TotalCitations_BIM")
total_citations_per_year <- merge(total_citations_per_year, total_citations_per_paper_per_year_ML, by = "Year", all = TRUE)
colnames(total_citations_per_year) <- c("Year", "TotalCitations_All", "TotalCitations_BIM", "TotalCitations_ML")
total_citations_per_year <- merge(total_citations_per_year, total_citations_per_paper_per_year_GIS, by = "Year", all = TRUE)
colnames(total_citations_per_year) <- c("Year", "TotalCitations_All", "TotalCitations_BIM", "TotalCitations_ML", "TotalCitations_GIS")
# Replace NA with 0
total_citations_per_year[is.na(total_citations_per_year)] <- 0
print(total_citations_per_year)
# save results as csv
write.csv(total_citations_per_year, file = "./Generated_Data_table/total_number_of_citations_each_year.csv", row.names = FALSE)


# ------check most citations paper and save results as csv
S_BIM$MostCitedPapers
write.csv(S_BIM$MostCitedPapers, file = "./Generated_Data_table/top_cited_publications_BIM.csv", row.names = FALSE)
S_ML$MostCitedPapers
write.csv(S_ML$MostCitedPapers, file = "./Generated_Data_table/top_cited_publications_ML.csv", row.names = FALSE)
S_GIS$MostCitedPapers
write.csv(S_GIS$MostCitedPapers, file = "./Generated_Data_table/top_cited_publications_GIS.csv", row.names = FALSE)

# ------check most productive countries and save results as csv
S_BIM$MostProdCountries
write.csv(S_BIM$MostProdCountries, file = "./Generated_Data_table/country_collaboration_BIM.csv", row.names = FALSE)
S_ML$MostProdCountries
write.csv(S_ML$MostProdCountries, file = "./Generated_Data_table/country_collaboration_ML.csv", row.names = FALSE)
S_GIS$MostProdCountries
write.csv(S_GIS$MostProdCountries, file = "./Generated_Data_table/country_collaboration_GIS.csv", row.names = FALSE)

# ------check citations of countries and save results as csv
S_BIM$TCperCountries
write.csv(S_BIM$TCperCountries, file = "./Generated_Data_table/country_citations_BIM.csv", row.names = FALSE)
S_ML$TCperCountries
write.csv(S_ML$TCperCountries, file = "./Generated_Data_table/country_citations_ML.csv", row.names = FALSE)
S_GIS$TCperCountries
write.csv(S_GIS$TCperCountries, file = "./Generated_Data_table/country_citations_GIS.csv", row.names = FALSE)

# -------- check the publishers and save results as csv
S_BIM$MostRelSources
write.csv(S_BIM$MostRelSources, file = "./Generated_Data_table/top_journals_BIM.csv", row.names = FALSE)
S_ML$MostRelSources
write.csv(S_ML$MostRelSources, file = "./Generated_Data_table/top_journals_ML.csv", row.names = FALSE)
S_GIS$MostRelSources
write.csv(S_GIS$MostRelSources, file = "./Generated_Data_table/top_journals_GIS.csv", row.names = FALSE)