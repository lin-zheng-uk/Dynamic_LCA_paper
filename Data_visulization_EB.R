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
p_a <- ggplot(yearly_data, aes(x = Year, y = Num_Publications)) +
  geom_point(color = "grey", size = 2) +  # Data points
  geom_smooth(method = "loess", se = TRUE, aes(fill = "Trend of Total Publications (Loess Smooth)"), color = "black", size = 1, span = 1) +  # Loess smoothing line with standard error for trend visualization
  #  #se = TRUE parameter will include a shaded area around the loess curve, which represents the standard error of the smoothed means.
  scale_color_manual("Legend", values = "black") +  # Manual color adjustment for legend
  scale_fill_manual("Legend", values = "grey80") +  # Manual fill adjustment for legend
  labs(title = "N. of Publications Over Time with LOESS Trend",
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

print(p_a)

# plot1b the total number of publications in each year breakdown by three methods, with growth rate
# growth rates are dervied from the bibliography analysis results
p_b <- ggplot(data, aes(x = Year, fill = Methods)) +
  geom_bar(stat = "count", position = "stack") +
  scale_fill_manual(values = colors) +
  labs(title = "N. of Publications Breakdown by Methods", x = "Year", y = "Number of Publications (N)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "top")

min(data$Year)
print(p_b)

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
p_c <- ggplot(annual_stats, aes(x = Methods, y = Average_Count, fill = Methods)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  geom_errorbar(aes(ymin = Average_Count - SE, ymax = Average_Count + SE),
                width = 0.05, position = position_dodge(0.5)) +
  scale_fill_manual(values = colors) +
  labs(x = "Different Methods", y = "Number of Publications (N)",
       title = "Average N.of Publications for Each Methods") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "None")
print(p_c)

# Calculate total citations and number of publications per method
avg_citations_per_pub <- data %>%
  group_by(Methods) %>%
  summarise(
    Total_Publications = n(),  # Count of publications
    Total_Citations = sum(Citation_Count),  # Sum of citations
    Avg_Citations_Per_Publication = Total_Citations / Total_Publications,  # Average citations per publication
    SEM = sd(Citation_Count, na.rm = TRUE) / sqrt(n())  # Calculate SEM, adding na.rm=TRUE to handle any NA values
  )

# Print the data frame to check results
print(avg_citations_per_pub)

p_d <- ggplot(final_dataset, aes(x = Methods, y = Citation_Count, fill = Methods)) +
  geom_bar(stat = "summary", fun = "mean", position = 'dodge', width = 0.5) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_se",
    position = position_dodge(width = 0.5),
    width = 0.05
  ) +
  scale_fill_manual(values = colors) +
  labs(
    x = "Methods",
    y = "Number of Citations",
    title = "Average Citations per Article Each Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "None"
  )
print(p_d)

# Summing citations per year for all methods
data <- final_dataset
annual_citations <- data %>%
  group_by(Year) %>%
  summarise(Total_Citations = sum(Citation_Count, na.rm = TRUE))

# Calculate cumulative citations over years
annual_citations <- annual_citations %>%
  arrange(Year) %>%
  mutate(Cumulative_Citations = cumsum(Total_Citations))

# Plotting cumulative citations with LOESS smoothing
p_e <- ggplot(annual_citations, aes(x = Year, y = Cumulative_Citations)) +
  #geom_line(color = "blue", size = 1) +  # Draw the original line
  geom_point(color = "grey", size = 2) +  # Highlight data points
  geom_smooth(method = "loess", se = TRUE, aes(fill = "Trend of Cumulative Citations (Loess Smooth)"), color = "black", size = 1, span = 1) +  # Loess smoothing line with standard error for trend visualization
  #  #se = TRUE parameter will include a shaded area around the loess curve, which represents the standard error of the smoothed means.
  scale_color_manual("Legend", values = "black") +  # Manual color adjustment for legend
  scale_fill_manual("Legend", values = "grey80") +  # Manual fill adjustment for legend
  labs(title = "Cumulative Citations Over Time with LOESS Trend",
       x = "Year",
       y = "Cumulative Citations") +
  xlim(min(yearly_data$Year), 2025) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top")

# Print the plot
print(p_e)

# --- average article citations per year with confidence interval ---
data <- final_dataset %>%
  group_by(Year) %>%
  summarise(Avg_Citations = mean(Citation_Count, na.rm = TRUE),
            SEM = sd(Citation_Count, na.rm = TRUE) / sqrt(n()))
data
# Plotting Average Citations per Year with Confidence Interval
p_f <- ggplot(data, aes(x = Year, y = Avg_Citations)) +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = Avg_Citations - 1.96 * SEM, ymax = Avg_Citations + 1.96 * SEM),
                width = 0.1, color = "grey") +
  labs(x = "Year", y = "Average Citations per Article", title = "Average Citations per Article Over Time with 95% CI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top")
print(p_f)

#---- annual growth rate by method, yearly---
data <- final_dataset %>%
  group_by(Year, Methods) %>%
  summarise(Publications = n()) %>%
  ungroup()
data
data <- data %>%
  arrange(Methods, Year) %>%
  group_by(Methods) %>%
  mutate(Last_Year = lag(Publications),
         Growth = (Publications - Last_Year) / Last_Year * 100) %>%
  filter(!is.na(Growth))
data
# Plotting Year-over-Year Growth Percentage Chart
p_g <- ggplot(data, aes(x = Year, y = Growth, fill = Methods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Yearly Growth Rate (%)", title = "Yearly Percentage Growth Rate by Method") +
  theme_minimal() +
  scale_fill_manual(values = colors) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top")
print(p_g)

#---- total annual growth rate by method--- ordered by the growth rate
# daraframe = results from the another R script that contains the annual growth rate for each method, calculated from the bibliography analysis results
df_growth_rates <- read.csv("./Generated_Data_table/annual_growth_rate.csv")
df_growth_rates
# Plotting Total Growth Rate Chart
df_growth_rates$Methods <- factor(df_growth_rates$Methods, levels = c("BIM-LC", "ML-LC", "GIS-LC"))
# Plotting Total Growth Rate Chart
p_h <- ggplot(df_growth_rates, aes(x = Methods, y = Annual_Growth_Rate, fill = Methods)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", Annual_Growth_Rate)), vjust = -0.5, position = position_dodge(width = 0.5), size = 4) +  # Adding text labels on top of the bars
  labs(x = "Three Methods", y = "Annual Growth Rate (%)", title = "Total Annual Growth Rate by Methods") +
  theme_minimal() +
  scale_fill_manual(values = colors) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "None") +
  ylim(0,40)

print(p_h)


p_a #
p_b #
p_f #
p_h #

# supplementary figures
p_d
p_e
p_c
p_g

# combine and save the figures
p_figure_combined_1 <- ggarrange(p_a, p_b,p_h,p_f, ncol = 2, nrow = 2, labels = c("(a)", "(b)","(c)", "(d)") )
print(p_figure_combined_1)
ggsave("./Figures/figure1.pdf", p_figure_combined_1, width = 10, height = 8, units = "in",bg = "white",dpi=300)

# combine and save the supplementary figures
p_figure_combined_2 <- ggarrange(p_c,p_g,p_e, p_d, ncol = 2, nrow = 2, labels = c("(a)", "(b)","(c)", "(d)") )
print(p_figure_combined_2)
ggsave("./Figures/figureS1_SMs.pdf", p_figure_combined_2, width = 10, height = 8, units = "in",bg = "white",dpi=300)
