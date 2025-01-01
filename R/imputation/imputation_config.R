library(reshape2)
library(plyr)
library(tidyverse)
library(RColorBrewer)

minmax <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(1, length(x)))
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}

minmax2 <- function(x, new_min, new_max) {
  scaled_x <- (x - new_min) / (new_max - new_min)  
  return(scaled_x)
}


# Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn 
# PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
palettes <- list("overall" = "Reds",
                 "structure.overall" = "Purples",
                 "structure.metric" = "Purples",
                 "clustering.overall" = "Blues",
                 "clustering.metric" = "Blues",
                 "classification.overall" = "PuRd",
                 "classification.metric" = "PuRd"
                 )

text.metric <- c("method", "celltype")
image.metric <- c("programmingLaguage", "DL", "Peak")
overall.metric <- c("overall", "batch.overall", "clustering.overall", "classification.overall", "classification_ori.overall")

clustering.metric <- c("ARI", "ASW", "cLISI", "iASW", "iFI", "NMI")
classification.metric <- c("Average Accuracy", "Overall Accuracy", "f1_score", "sensitivity", "specificity")
classification_ori.metric <- c("Average Accuracy_ori", "Overall Accuracy_ori", "f1_score_ori", "sensitivity_ori", "specificity_ori")
batch.metric <- c("ASW_batch", "GC", "iLISI", "kBET", "NMI_batch", "ARI_batch")
imputation.metric <- c("ASW_batch", "GC", "iLISI", "kBET", "NMI_batch", "ARI_batch")

label_match <- data.frame(
  label_new = c("Method", "Cell type", "Language", "DL", "Peak", "Overall score", "Clustering overall", "ARI", "ASW", "cLISI", "iFI", "iASW", "NMI", "Batch overall", "ASW", "GC", "iLISI", "kBET", 'NMI', "ARI", "Classification overall", "AAC", "OAC", "F1", "Sens", "Spec", "Seurat v3", "Seurat v5", "online iNMF"),
  label_old = c("method", "celltype", "programmingLaguage", "DL", "Peak", "overall", "clustering.overall", "ARI", "ASW", "cLISI", "iFI", "iASW", "NMI", "batch.overall", "ASW_batch", "GC", "iLISI", "kBET", "NMI_batch", "ARI_batch", "classification.overall", "Average Accuracy", "Overall Accuracy", "f1_score", "sensitivity", "specificity", "Seurat.v3", "Seurat.v5", "online.iNMF"
))