load_all_data_cross <- function(base_dir) {
  base_dir2 <- gsub("scib_metric", "classification_metrics", base_dir)
  
  combined_data <- data.frame()
  
  # Get the list of main folders
  main_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  main_folders2 <- list.dirs(base_dir2, full.names = TRUE, recursive = FALSE)
  
  # Loop through each main folder
  for (i in 1:length(main_folders)) {
    main_folder <- main_folders[i]
    dataset <- basename(main_folder)
    main_folder2 <- main_folders2[i]
    subdirs <- list.dirs(main_folder, full.names = TRUE, recursive = FALSE)
    subdirs2 <- list.files(main_folder2, full.names = TRUE, recursive = FALSE, pattern = ".csv")
  
    methods2 <- basename(subdirs2)
    methods2[grep("_v5", methods2)] <- gsub("_v5", ".v5", methods2[grep("_v5", methods2)]) # bridge are v5??
    methods2[grep("online_iNMF", methods2)] <- gsub("online_iNMF", "online.iNMF", methods2[grep("online_iNMF", methods2)])
    methods2[grep("_v3", methods2)] <- gsub("_v3", ".v3", methods2[grep("_v3", methods2)])
    
    methods <- basename(subdirs)
    methods[grep("_v5", methods)] <- gsub("_v5", ".v5", methods[grep("_v5", methods)]) # bridge are v5??
    methods[grep("online_iNMF", methods)] <- gsub("online_iNMF", "online.iNMF", methods[grep("online_iNMF", methods)])
    methods[grep("_v3", methods)] <- gsub("_v3", ".v3", methods[grep("_v3", methods)])
    
    temp_method <- list()
    previous_method = ""
    for (j in 1:length(subdirs2)) {
    
      method <- sapply(strsplit(gsub(".csv", "", methods2[j]), "_"), "[[", 1)
      suffix <- gsub(".csv", "", methods2[j])
      if (previous_method != method){
        if (suffix!=method){
          ids <- which(methods == method)
          subdir <- subdirs[ids]
          subdir2 <- subdirs2[j]
        
          metric_path <- file.path(subdir, "metric.csv")
          metric_path2 <- file.path(subdir, "metric_asw_iasw_if1.csv")
          ari_nmi_path <- file.path(subdir, "metric_ari_nmi_batch.csv")

          classification_path <- subdir2
          classification_path2 <- sub("^(.*)_[^_/]+\\.csv$", "\\1.csv", subdir2)
    
        
          if (file.exists(subdir2)) {
            # Read the CSV file
            metric_data <- read.csv(metric_path)
            metric_data2 <- read.csv(metric_path2)
            metric_data <- metric_data %>%
              dplyr::left_join(metric_data2, by = "X", suffix = c("", ".new")) %>% 
              dplyr::mutate(Value = coalesce(Value.new, Value)) %>% 
              dplyr::select(-Value.new)
            
            ari_nmi_data <- read.csv(ari_nmi_path)
            print("#####")
            print(dim(ari_nmi_data))

            ari_nmi_data$X <- paste0(ari_nmi_data$X, "_batch")
            classification_data <- read.csv(classification_path)
            classification_data$X <- paste0(classification_data$X,"_ori")
            classification_data2 <- read.csv(classification_path2)
            
            classification_data <- cbind(X = classification_data[, 1], 
                                         Value = as.numeric(rowMeans(classification_data[,2:ncol(classification_data)])))
            classification_data2 <- cbind(X = classification_data2[, 1], 
                                         Value = as.numeric(rowMeans(classification_data2[,2:ncol(classification_data2)])))
            classification_data[,2] <- as.numeric(classification_data[,2])
            classification_data2[,2] <- as.numeric(classification_data2[,2])
            
            temp_data <- rbind(metric_data, ari_nmi_data, classification_data, classification_data2)
            temp_data$method <- method
            temp_data$dataset <- dataset
            temp_method[[j]] <- temp_data
            names(temp_data) <- c("metric", "value", "method", "dataset")
            combined_data <- rbind(combined_data, temp_data)
          }
          previous_method = method
        }
        if (suffix==method){
          ids <- which(methods == method)
          subdir <- subdirs[ids]
          subdir2 <- subdirs2[j]
        
          metric_path <- file.path(subdir, "metric.csv")
          ari_nmi_path <- file.path(subdir, "metric_ari_nmi_batch.csv")
          classification_path <- subdir2
        
          if (file.exists(subdir2)) {
            # Read the CSV file
            metric_data <- read.csv(metric_path)
            ari_nmi_data <- read.csv(ari_nmi_path)

            print("#####")
            print(dim(ari_nmi_data))

            ari_nmi_data$X <- paste0(ari_nmi_data$X, "_batch")
            classification_data <- read.csv(classification_path)
            classification_data2 <- read.csv(classification_path2)
            
            classification_data <- cbind(X = classification_data[, 1], 
                                         Value = as.numeric(rowMeans(classification_data[,2:ncol(classification_data)])))
            classification_data[,2] <- as.numeric(classification_data[,2])
            colnames(ari_nmi_data) <- colnames(metric_data)
            colnames(classification_data) <- colnames(metric_data)
            temp_data <- rbind(metric_data, ari_nmi_data, classification_data)
            temp_data$method <- method
            temp_data$dataset <- dataset
            names(temp_data) <- c("metric", "value", "method", "dataset")
            combined_data <- rbind(combined_data, temp_data)
          }
          previous_method = method
        }
      }
    }
    names(combined_data) <- c("metric", "value", "method", "dataset")
  }
    return(combined_data)
}
generate_individual_bubble_cross <- function(ds, select_metric, select_method,  base_dir, task_category, new_file_paths=NA, new_file_names=NA) {
  source("./R/cross/cross_config.R")
  if (task_category=="clustering"){source("./R/cross/scIB_knit_table_clustering.R")}
  if (task_category=="batchcorrection"){source("./R/cross/scIB_knit_table_batch.R")}
  if (task_category=="classification"){source("./R/cross/scIB_knit_table_classification.R")}
  combined_data <- load_all_data_cross(base_dir)
  combined_data <- combined_data %>%
    mutate(
      method = ifelse(grepl(".ori$", metric), paste0(method, ".ori"), method),
      metric = gsub(".ori$", "", metric)
  )
  IsMinmax <- TRUE 
  print(ds)
  data <- combined_data %>% 
    filter(dataset == ds) %>% 
    select(c("metric", "value", "method"))
  data$value <- as.numeric(as.character(data$value))
  data <- reshape2::dcast(data, method ~ metric, value.var = "value", fun.aggregate = mean)
  data <- data[data$method %in% select_method,]

  if (!is.null(new_file_paths) && length(new_file_paths) > 0 && !all(is.na(new_file_paths))) {
    for (i in c(1:length(new_file_paths))){
      new_method <- read.csv(new_file_paths[i])
      colnames(new_method) <- c("X", "Value")
      base_new_method <- gsub(".csv", "", basename(new_file_names[i]))
      new_method_values <- setNames(new_method$Value, new_method$X)
      new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(data)))
      colnames(new_row) <- colnames(data)
      new_row$method <- base_new_method
      common_cols <- intersect(names(new_method_values), colnames(data))  # 找到 a 和 data 共有的列
      new_row[1, common_cols] <- new_method_values[common_cols]
      new_row$method <- base_new_method
      data <- rbind(data, new_row)
    }
  }
  
  select_metric <- c("method", select_metric)  # List the desired metrics
  data <- data %>%select(all_of(select_metric))
  
  if (IsMinmax) {
    data[, 2:(ncol(data))] <- apply(as.matrix(data[, 2:(ncol(data))]), 2, function(x) {
      if (all(is.na(x)) || min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
        return(rep(1, length(x)))
      } else {
        return(minmax(x))
      }
    })
  }
  temp_data <- data
  temp_data[is.na(temp_data)] <- 0
  ranked_data <- apply(temp_data[, 2:(ncol(data))], 2, rank, ties.method = "max")
  rownames(ranked_data) <- rownames(data)
  if (dim(temp_data)[2]==2){colnames(ranked_data) <- colnames(data)[2]}
  nan_positions <- which(is.na(as.matrix(data)), arr.ind = TRUE)
  nan_rows <- rownames(data)[nan_positions[, 1]]
  nan_cols <- colnames(data)[nan_positions[, 2]]
  na_idx <- data.frame(Row = nan_rows, Column = nan_cols)
  if (nrow(na_idx) > 0) {
    for (i in seq_len(nrow(na_idx))) {
      ranked_data[na_idx$Row[i], na_idx$Column[i]] <- NA
    }
  } 
  
  data_rank_average <-apply(ranked_data, 1, function(x) mean(x, na.rm = TRUE))
  data$overall <- minmax(data_rank_average)
  ranked_data <- cbind(rank(data_rank_average, ties.method = "max"), ranked_data)
  ranked_data <- as.data.frame(ranked_data)
  colnames(ranked_data)[1] <- c("overall")
  ranked_data[is.na(ranked_data)] <- 0
  ranked_data <- apply(ranked_data,2,minmax2,0, max(ranked_data))
  if (nrow(na_idx) > 0) {
    for (i in c(1:nrow(na_idx))){
      ranked_data[(na_idx$Row[i]), na_idx$Column[i]] <- NA
    }
  }

  methods <- sapply(strsplit(data$method, "_"), function(x)x[[1]])
  data <- data[, c("method", "overall", colnames(data)[2:(length(colnames(data))-1)])]
  ranked_data <- ranked_data[order(data$overall, decreasing = T),]
  data <- data[order(data$overall, decreasing = T), ]
  
  row_info <- data.frame(id = rownames(data))
  column_info <- data.frame(id = colnames(data),
                            geom = c(rep("text", 1),  "bar", rep("circle", (dim(data)[2]-2))))
  p <- scIB_knit_table(data, ranked_data, row_info = row_info, column_info = column_info, na_idx = na_idx, isImage = FALSE)
  print(p)
}


generate_summary_bubble_cross <- function(ds, select_metric, select_method,  base_dir,task_category) {
  source("./R/cross/cross_config.R")
  if (task_category=="clustering"){source("./R/cross/scIB_knit_table_clustering_summary.R")}
  if (task_category=="batchcorrection"){source("./R/cross/scIB_knit_table_batch_summary.R")}
  if (task_category=="classification"){source("./R/cross/scIB_knit_table_classification_summary.R")}
  combined_data <- load_all_data_cross(base_dir)
  print(dim(combined_data))
  combined_data <- combined_data %>%
    mutate(
      method = ifelse(grepl(".ori$", metric), paste0(method, ".ori"), method),
      metric = gsub(".ori$", "", metric)
  )
  combined_data$value <- as.numeric(combined_data$value)
  IsMinmax <- TRUE 
  print(ds)
  ranked_data <- combined_data %>%
      filter(dataset %in% ds) %>% 
      filter(method %in% select_method) %>% 
      filter(metric %in% select_metric) %>% 
      dplyr::group_by(dataset, metric) %>%
      dplyr::mutate(rank = rank(value, ties.method = "max")) %>%
      dplyr::arrange(dataset, metric, rank) 
  ranked_data$value <- as.numeric(ranked_data$value)
  max_value <- max(ranked_data$rank)
  ranked_data <- ranked_data %>%
    dplyr::group_by(dataset, metric) %>%
    dplyr::mutate(max_rank_in_group = max(rank),
           adjustment_needed = max_value - max_rank_in_group,
           rank = rank + adjustment_needed) %>%
    dplyr::select(-max_rank_in_group, -adjustment_needed) %>%  # 移除辅助列
    dplyr::ungroup()
  average_rank_data <- ranked_data %>%
    dplyr::group_by(method, metric) %>%
    dplyr::summarize(avg_rank = mean(rank), .groups = "drop") %>%
    dplyr::arrange(metric, avg_rank)  # 按 dataset 和 avg_rank 排序
  data <- dcast(average_rank_data, method  ~ metric, value.var = "avg_rank")
  if (IsMinmax) {
    data[, 2:(ncol(data))] <- apply(as.matrix(data[, 2:(ncol(data))]), 2, function(x) {
      if (all(is.na(x)) || min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
        return(rep(1, length(x)))
      } else {
        return(minmax(x))
      }
    })
  }

  temp_data <- data
  temp_data[is.na(temp_data)] <- 0
  ranked_data <- apply(temp_data[, 2:(ncol(data))], 2, rank, ties.method = "max", na.last = "keep")
  if (dim(temp_data)[2]==2){colnames(ranked_data) <- colnames(data)[2]}
  nan_positions <- which(is.na(as.matrix(data)), arr.ind = TRUE)
  nan_rows <- rownames(data)[nan_positions[, 1]]
  nan_cols <- colnames(data)[nan_positions[, 2]]
  na_idx <- data.frame(Row = nan_rows, Column = nan_cols)
  if (nrow(na_idx) > 0) {
    for (i in seq_len(nrow(na_idx))) {
      ranked_data[as.numeric(na_idx$Row[i]), na_idx$Column[i]] <- NA
    }
  } 
  
  data_rank_average <-apply(ranked_data, 1, function(x) mean(x, na.rm = TRUE))
  data$overall <- minmax(data_rank_average)
  ranked_data <- cbind(rank(data_rank_average, ties.method = "max"), ranked_data)
  ranked_data <- as.data.frame(ranked_data)
  colnames(ranked_data)[1] <- c("overall")
  ranked_data[is.na(ranked_data)] <- 0
  ranked_data <- apply(ranked_data,2,minmax2,0, max(ranked_data))
  if (nrow(na_idx) > 0) {
    for (i in c(1:nrow(na_idx))){
      ranked_data[as.numeric(na_idx$Row[i]), na_idx$Column[i]] <- NA
    }
  }

  methods <- sapply(strsplit(data$method, "_"), function(x)x[[1]])
  data <- data[, c("method", "overall", colnames(data)[2:(length(colnames(data))-1)])]
  ranked_data <- ranked_data[order(data$overall, decreasing = T),]
  data <- data[order(data$overall, decreasing = T), ]
  rownames(ranked_data) <- rownames(data)
  colnames(data)[2:dim(data)[2]] <- paste0("Grand ",colnames(data)[2:dim(data)[2]])
  colnames(ranked_data)[1:dim(ranked_data)[2]] <- paste0("Grand ",colnames(ranked_data)[1:dim(ranked_data)[2]])
  row_info <- data.frame(id = rownames(data))
  column_info <- data.frame(id = colnames(data),
                            geom = c(rep("text", 1), rep("bar", dim(data)[2]-1)))

  p <- scIB_knit_table(data, ranked_data, row_info = row_info, column_info = column_info, na_idx = na_idx, isImage = FALSE)
  print(p)
}
