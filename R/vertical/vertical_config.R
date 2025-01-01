
label_match <- data.frame(
  label_new = c("Method", "Cell type", "Language", "DL", "Peak", "Overall score", "Clustering overall", "cLISI", "ARI", "ASW", "iFI", "iASW", "NMI", "Batch overall", "ASW", "GC", "iLISI", "kBET", 'NMI', "ARI", "Classification overall", "AAC", "OAC", "F1", "Sens", "Spec", "Seurat v3", "Seurat v5", "online iNMF", "Seurat WNN"),
  label_old = c("method", "celltype", "programmingLaguage", "DL", "Peak", "overall", "clustering.overall",  "cLISI","ARI", "ASW", "iFI", "iASW", "NMI", "batch.overall", "ASW_batch", "GC", "iLISI", "kBET", "NMI_batch", "ARI_batch", "classification.overall", "Average Accuracy", "Overall Accuracy", "f1_score", "sensitivity", "specificity", "Seurat.v3", "Seurat.v5", "online.iNMF", "Seurat.WNN"
))

palettes <- list(
  "a" = "Purples",
  "vertical.overall" = "Blues",
  "c" = "Greens",
  "vertical.metric" = "Blues"
)
