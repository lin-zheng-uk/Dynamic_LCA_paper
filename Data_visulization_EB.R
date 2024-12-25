library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(igraph)
install.packages("ggpattern")
library(ggpattern)
# check r version
R.Version()$version.string

# -------- data visualization -------

# read the data from the csv file
final_dataset <- read.csv("./Generated_Data_table/overall_articles_selection.csv")
print(dim(final_dataset))
# print the column names
print(colnames(final_dataset))  # PY is the year of publication， NR is citation count
# rename source column, value 1 is BIM-LC, 2 is ML-LC, 3 is GIS-LC
final_dataset$source <- factor(final_dataset$source, levels = c(1, 2, 3), labels = c("BIM-LC", "ML-LC", "GIS-LC"))
# rename the column name
colnames(final_dataset)[colnames(final_dataset) == "PY"] <- "Year"
colnames(final_dataset)[colnames(final_dataset) == "TC"] <- "Citation_Count"
colnames(final_dataset)[colnames(final_dataset) == "source"] <- "Methods"
# value count for each source
final_dataset %>%
  group_by(Methods) %>%
  summarise(Count = n())

# -------- data visualization ------- Number of publications over time and by methods
# calculate statistics for the number of publications each year by method


# Improved color palette: colors <- RColorBrewer::brewer.pal(3, "Set2")
# OR: Manually set the colors
colors <- c("BIM-LC" = "#56B4E9",  # Blue
            "ML-LC" = "#FFA500",   # Vivid Orange
            "GIS-LC" = "#808080")  # Standard Grey

# Plot 1: Total number of publications each year by method
# data = final_dataset where column "Citation_Count" is not NA or empty or zero
data <- final_dataset
print(dim(data))

yearly_data <- data %>%
  group_by(Year) %>%
  summarise(
    Num_Publications = n(),  # Assuming each row is a publication
    Total_Citations = sum(Citation_Count, na.rm = TRUE),
    Mean_Citations = mean(Citation_Count, na.rm = TRUE),
    Adjusted_Citations = Total_Citations / Num_Publications,  # Normalizing by number of publications
    SEM = sd(Citation_Count, na.rm = TRUE) / sqrt(n()),
    Citation_Rate = Total_Citations / Num_Publications
  )
yearly_data$Lower_CI <- yearly_data$Mean_Citations - 1.96 * yearly_data$SEM
yearly_data$Upper_CI <- yearly_data$Mean_Citations + 1.96 * yearly_data$SEM

yearly_data

# plot1a the total number of publications in each year by line chart, with loess smoothing line
p_figure1a <- ggplot(yearly_data, aes(x = Year, y = Num_Publications)) +
  geom_point(color = "lightgrey", size = 2.5) +  # Data points
  geom_smooth(method = "loess", se = TRUE, aes(fill = "Trend of Publications (Loess Smooth)"), color = "black", size = 1, span = 1) +  # Loess smoothing line with standard error for trend visualization
  #  #se = TRUE parameter will include a shaded area around the loess curve, which represents the standard error of the smoothed means.
  scale_color_manual("Legend", values = "black") +  # Manual color adjustment for legend
  scale_fill_manual("Legend", values = "grey80") +  # Manual fill adjustment for legend
  labs(title = "Number of Publications in Each Year",
       x = "Year",
       y = "Number of Publications (N)",
       color = "Line",  # Legend title for line
       fill = "Shaded Area") +  # Legend title for shaded area
  xlim(min(yearly_data$Year), 2025) +  # Set x-axis limits
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "top")  # Adjust legend position as needed

# Print the plot
print(p_figure1a)

# plot1b the total number of publications in each year breakdown by three methods
p_figure1b <- ggplot(data, aes(x = Year, fill = Methods)) +
  geom_bar(stat = "count", position = "stack") +
  scale_fill_manual(values = colors) +
  labs(title = "Number of Publications in Each Year Breakdown by Three Methods", x = "Year", y = "Number of Publications (N)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "top")

# Combine the two plots
p_figure1 <- ggarrange(p_figure1a,p_figure1b, ncol = 1, nrow = 2, labels = c("(a)", "(b)"))
p_figure1
ggsave("./Figures/figure1_counts_of_publications.pdf", p_figure1, width = 8, height = 6, units = "in",bg = "white",dpi=300)



# -------- data visualization ------- Average number of citations over time and by methods
data <- final_dataset
annual_stats <- data %>%
  group_by(Methods, Year) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Methods) %>%
  summarise(
    Average_Count = mean(Count),
    SE = sd(Count) / sqrt(n())
  )
# View the statistics
print(annual_stats)
# Plot 2: Average number each method in the studied period with error bars
p_figure2a <- ggplot(annual_stats, aes(x = Methods, y = Average_Count, fill = Methods)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  geom_errorbar(aes(ymin = Average_Count - SE, ymax = Average_Count + SE),
                width = 0.05, position = position_dodge(0.5)) +
  scale_fill_manual(values = colors) +
  labs(x = "Three Methods", y = "Average Number of Publications by Methods (N)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "top")
p_figure2a

# -------------
p_figure2b <-
  ggplot(final_dataset, aes(x = Methods, y = Citation_Count, fill = Methods)) +
  geom_bar(stat = "summary", fun = "mean", position = 'dodge',width = 0.5) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.5), width = 0.05) +
    scale_fill_manual(values = colors) + labs(x = "Methods", y = "Average Number of Citations") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "None")  # Remove legend from second plot if same as first
p_figure2b # average number each methods in the studied period by bar chart with error bars

p_figure2 <- ggarrange(p_figure2a,p_figure2b, ncol = 1, nrow = 2, labels = c("(a)", "(b)"))
p_figure2