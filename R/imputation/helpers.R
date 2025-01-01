load_all_data_imputation <- function(base_dir) {
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
    subdirs2 <- list.dirs(main_folder2, full.names = TRUE, recursive = FALSE)
  
    methods2 <- basename(subdirs2)
    methods <- basename(subdirs)
    
    temp_method <- list()
    for (j in 1:length(subdirs2)) {
      method <- sapply(strsplit(gsub(".csv", "", methods2[j]), "_"), "[[", 1)
      ids <- which(methods == method)
      subdir <- subdirs[ids]
      subdir2 <- subdirs2[j]
    
      metric_path <- file.path(subdir, "metric.csv")
      hvg_path <- file.path(subdir, "hvg_cor.csv")
      de_marker_path <- file.path(subdir, "de_marker_cor.csv")
      smse_path <- file.path(subdir, "smse.csv")
      
      metric_path2 <- file.path(subdir, "metric_asw_iasw_if1.csv")
      classification_path <- file.path(subdir2, "classification.csv") 

    
      if (file.exists(subdir2)) {
        # Read the CSV file
        metric_data <- read.csv(metric_path)
        colnames(metric_data) <- c("X", "Value")
        metric_data2 <- read.csv(metric_path2)
        colnames(metric_data2) <- c("X", "Value")
        metric_data <- metric_data %>%
          dplyr::left_join(metric_data2, by = "X", suffix = c("", ".new")) %>% 
          dplyr::mutate(Value = coalesce(Value.new, Value)) %>% 
          dplyr::select(-Value.new)
        
        smse_data <- read.csv(smse_path)
        smse_data$x <- 1/smse_data$x
        colnames(smse_data) <- c("X", "Value")
        smse_data$X <- "sMSE"
        pFCS_data <- read.csv(hvg_path)
        colnames(pFCS_data) <- c("X", "Value")
        pFCS_data$X <- "pFCS"
        pDES_data <- read.csv(de_marker_path)
        colnames(pDES_data) <- c("X", "Value")
        pDES_data$X <- "pDES"
  
        classification_data <- read.csv(classification_path)
        colnames(classification_data) <- c("X", "Value")
        
        temp_data <- rbind(metric_data, smse_data, pFCS_data, pDES_data, classification_data)
        temp_data$method <- method
        temp_data$dataset <- dataset
        temp_method[[j]] <- temp_data

        names(temp_data) <- c("metric", "value", "method", "dataset")
        combined_data <- rbind(combined_data, temp_data)
      }
    }
    names(combined_data) <- c("metric", "value", "method", "dataset")
  }
    return(combined_data)
}


generate_individual_bubble_imputation <- function(ds, select_metric, select_method,  base_dir, task_category, new_file_paths=NA, new_file_names=NA) {
  source("./R/imputation/imputation_config.R")
  if (task_category=="clustering"){source("./R/imputation/scIB_knit_table_clustering.R")}
  if (task_category=="structure"){source("./R/imputation/scIB_knit_table_structure.R")}
  if (task_category=="classification"){source("./R/imputation/scIB_knit_table_classification.R")}
  combined_data <- load_all_data_imputation(base_dir)
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
  rownames(ranked_data) <- rownames(data)
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


generate_summary_bubble_imputation <- function(ds, select_metric, select_method,  base_dir,task_category) {
  source("./R/imputation/imputation_config.R")
  if (task_category=="clustering"){source("./R/imputation/scIB_knit_table_clustering_summary.R")}
  if (task_category=="classification"){source("./R/imputation/scIB_knit_table_classification_summary.R")}
  if (task_category=="structure"){source("./R/imputation/scIB_knit_table_structure_summary.R")}
  combined_data <- load_all_data_imputation(base_dir)
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

