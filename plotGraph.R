# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create dummy data
cognitive_measures <- c(100, 90, 80, 70, 60, 50, 40)
rois <- c("SPL", "IPL", "AL", "Left-Cerebellum-Cortex", "Right-Cerebellum-Cortex", "Left-Cerebellum-White-Matter", "Right-Cerebellum-White-Matter")
group <- rep(c("Group1", "Group2"), each=7)

# Create a dataframe with these measures and hypothetical groups
data <- data.frame(Group=group, ROI=rep(rois, 2), Score=c(cognitive_measures, rev(cognitive_measures)))

# Heatmap Data Preparation
data_melted <- melt(data, id.vars=c("Group", "ROI"), value.name="Score")

# Heatmap Plot
ggplot(data_melted, aes(x=Group, y=ROI, fill=Score)) +
  geom_tile() +
  scale_fill_gradient(low="blue", high="red") +
  theme_minimal() +
  labs(title="Cognitive Scores Heatmap", x="Group", y="Region of Interest")

# Scatter Plot
ggplot(data, aes(x=factor(ROI, levels=rois), y=Score)) +
  geom_point() +
  theme_minimal() +
  labs(title="Cognitive Scores by ROI", x="Region of Interest", y="Score")

# Bar Chart
ggplot(data, aes(x=factor(ROI, levels=rois), y=Score, fill=ROI)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Cognitive Scores by ROI", x="Region of Interest", y="Score")





########################

# Load necessary libraries
library(ggplot2)
library(grid)  # For the unit function

# Create dummy data
cognitive_measures <- c(100, 90, 80, 70, 60, 50, 40)
rois <- c("SPL", "IPL", "AL", "Left-Cerebellum-Cortex", "Right-Cerebellum-Cortex", 
          "Left-Cerebellum-White-Matter", "Right-Cerebellum-White-Matter")
data <- data.frame(ROI=rois, Score=cognitive_measures)

# Bar Chart with Styling
ggplot(data, aes(x=factor(ROI, levels=rois), y=Score, fill=ROI)) +
  geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0, 120, by = 5), limits = c(0, 120)) +
  labs(title = "Cognitive Scores by ROI",
       subtitle = "A demonstration of ggplot2 styling options",
       x = "Region of Interest",
       y = "Score",
       caption = "Source: Dummy Data") +
  theme(plot.margin = margin(t = 10, r = 15, b = 60, l = 5, unit = "pt"),
        text = element_text(size = 10),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "blue"),
        axis.title.x = element_text(size = 14, face = "italic"),
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill = "lavender"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_blank())  # Remove minor grid lines

# Save the plot to a file
ggsave("styled_plot.png", width = 10, height = 8, units = "in")



#########
# Load necessary libraries
library(ggplot2)

# Create dummy data
cognitive_measures <- c(100, 90, 80, 70, 60, 100, 100)
rois <- c("SPL", "IPL", "AL", "Left-Cerebellum-Cortex", "Right-Cerebellum-Cortex",
          "Left-Cerebellum-White-Matter", "Right-Cerebellum-White-Matter")
data <- data.frame(ROI=rois, Score=cognitive_measures, Group=rep(c("A", "B"), length.out = length(rois)))

# Scatter Plot with Styling
ggplot(data, aes(x=factor(ROI, levels=rois), y=Score, color=Group, shape=Group)) +
  geom_point(size=5, aes(fill=Group), pch=21) +  # Use filled shapes
  scale_fill_manual(values=c("A"="blue", "B"="red")) +
  scale_color_manual(values=c("A"="darkblue", "B"="darkred")) +
  geom_smooth(method="lm", se=TRUE, color="black", linetype="dashed") +  # Add a regression line with standard error
  labs(title = "Cognitive Scores by ROI",
       subtitle = "Scatter plot with styled points and regression line",
       x = "Region of Interest",
       y = "Score",
       caption = "Source: Dummy Data") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "ivory", color = "gray", size = 1),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "lightgray")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap long labels for neatness

# Save the plot to a file
ggsave("styled_scatter_plot.png", width = 10, height = 8, units = "in")


######

# Load necessary libraries
library(ggplot2)
library(reshape2)  # For melting the data frame

# Create dummy data
cognitive_measures <- c(100, 100, 100, 70, 60, 100, 100)
rois <- c("SPL", "IPL", "AL", "Left-Cerebellum-Cortex", "Right-Cerebellum-Cortex",
          "Left-Cerebellum-White-Matter", "Right-Cerebellum-White-Matter")
group <- rep(c("Group1", "Group2"), each=7)
scores <- c(cognitive_measures, rev(cognitive_measures))  # Simulate two groups with reversed scores

# Prepare data
data <- data.frame(Group=group, ROI=rep(rois, 2), Score=scores)
data_melted <- melt(data, id.vars = c("Group", "ROI"), value.name = "Score")

# Heatmap with Styling
ggplot(data_melted, aes(x=Group, y=ROI, fill=Score)) +
  geom_tile(color = "white", size = 0.5) +  # Add white lines between tiles
  scale_fill_gradient(low = "blue", high = "red", name = "Cognitive Score") +
  labs(title = "Cognitive Scores Heatmap",
       subtitle = "Heatmap representation of cognitive scores by group and region",
       x = "Group",
       y = "Region of Interest") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.key.size = unit(1, "cm"),  # Larger legend keys
        plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

# Save the plot to a file
ggsave("styled_heatmap.png", width = 10, height = 6, units = "in")


                   library(ComplexHeatmap)
library(circlize)  # Dependency of ComplexHeatmap

# Set seed for reproducibility
set.seed(42)

# Dummy data setup
rois <- c("SPL", "IPL", "AL", "Left-Cerebellum-Cortex", "Right-Cerebellum-Cortex",
          "Left-Cerebellum-White-Matter", "Right-Cerebellum-White-Matter",
          "Vermis", "Left_CrusII", "Left_I_IV", "Left_IX", "Left_V",
          "Left_VI", "Left_VIIb", "Left_VIIIa", "Left_VIIIb", "Left_X",
          "Right_CrusI", "Right_CrusII", "Right_I_IV", "Right_IX", "Right_V",
          "Right_VI", "Right_VIIb", "Right_VIIIa", "Right_VIIIb", "Right_X",
          "Vermis_IX", "Vermis_VI", "Vermis_VII", "Vermis_VIII", "Vermis_X")

# Random cognitive measures across multiple subjects
measure_data <- matrix(rnorm(length(rois) * 10, mean = 50, sd = 10), ncol = length(rois), nrow = 10)
rownames(measure_data) <- paste("Subject", 1:10)
colnames(measure_data) <- rois

# Simulate p-values for significance of each measure
p_values_matrix <- matrix(runif(length(rois) * 10, 0, 0.1), ncol = length(rois), nrow = 10)
p_values_matrix[p_values_matrix <= 0.05] <- 0.01  # Marking some entries as significant
p_values_matrix[p_values_matrix > 0.05] <- 0.1  # Non-significant entries

# Color scale
color_palette <- colorRampPalette(c("blue", "white", "red"))(100)

# Apply color scale based on normalized data values
normalized_data <- (measure_data - min(measure_data)) / (max(measure_data) - min(measure_data))
color_indices <- ceiling(normalized_data * 99) + 1  # Ensures index is between 1 and 100
measure_colors <- matrix(color_palette[color_indices], nrow = 10, ncol = length(rois))

# Adding border to highlight significance
borders <- ifelse(p_values_matrix <= 0.05, "red", NA)  # Red borders for significant values

# Create the heatmap
Heatmap(measure_data,
        col = color_palette,  # Use the color palette
        name = "Cognitive Score",
        border = borders,  # Apply conditional borders based on significance
        column_names_side = "bottom",
        row_names_gp = gpar(fontsize = 10),
        column_names_gp = gpar(fontsize = 10, angle = 45),
        heatmap_legend_param = list(title = "Score", at = c(min(measure_data), mean(measure_data), max(measure_data)), labels = c("Low", "Medium", "High")))




