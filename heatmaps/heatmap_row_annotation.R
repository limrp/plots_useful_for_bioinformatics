#####*************************************************************************************######
#####* Lila Maciel Rodríguez Pérez
#####* 26 May, 2024
#####*************************************************************************************######


library(ComplexHeatmap)
library(magrittr)
library(dplyr)


data_df <- read.csv(
    header = TRUE,
    row.names = 1,
    quote = "",
    stringsAsFactors = FALSE
    )

# Annotation data frame
# all rows of first column DRUG resistance
annotation_df <- data.frame(Resistencia = data_df[, 1])

# Annotation colors
annotation_colours <- list(
    "Resistencia" = c("1" = "#D62728",
                      "0" = "#1F77B4")
)

# Simple annotation of rows: Annotation label
antibiotic <- colnames(data_df)[1]

# Building the Annotation for the rows
row_ann <- HeatmapAnnotation(
    df = annotation_df,
    which = "row", 
    col = annotation_colours, 
    annotation_width = unit(0.00001, "npc"),
    show_legend = FALSE, 
    annotation_name_side = "top", 
    annotation_name_rot = 90, 
    annotation_label = antibiotic,
    annotation_name_gp = gpar(fontsize = 9), 
    gp = gpar(col = "white", lwd = 1)
)

# Remove the first column (Resistance)
data_df <- data_df[, -1]

# Convert to matrix
data_matrix <- as.matrix(x = data_df)

# Colors for cells of the heatmap
chm_colors <- c("1" = "#66C2A5", 
                "0" = "#555555")

# Heatmap
chm <- Heatmap(data_matrix,
               col = chm_colors,
               show_column_names = TRUE, 
               column_names_side = "top", 
               column_names_rot = 0, 
               column_names_centered = TRUE, 
               column_names_gp = gpar(fontsize = 10),
               row_names_gp = gpar(fontsize = 10),
               row_names_side = "left", 
               cluster_rows = FALSE,
               cluster_columns = FALSE,
               rect_gp = gpar(col = "white", lwd = 1), 
               show_heatmap_legend = FALSE,
               left_annotation = row_ann
)

# Legend for Resistance
lgd1 = Legend(title = "Resistencia", 
              labels = c("Resistente", "Sensible"), 
              legend_gp = gpar(fill = c("#D62728", "#1F77B4"))
)

# Legend for mutations present
lgd2 = Legend(title = "Mutación", 
              labels = c("Presente", "Ausente"), 
              legend_gp = gpar(fill = c("#66C2A5", "#555555"))
)

# List of legends
lgd_list <- list(lgd1, lgd2)

# Show / draw the heatmap
draw(chm, 
     annotation_legend_list = lgd_list
)

# Saving the plot
png(
    filename = "/home/koala/my_projects/plots_for_bioinformatics/heatmaps/plots/drug_x.png",
    # width = 2500, height = 1800,
    width = 1600, height = 1900,
    res = 200
    )

draw(chm, 
     annotation_legend_list = lgd_list
)

dev.off()