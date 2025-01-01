load_all_data_fs <- function(base_dir, base_dir2, base_dir3, base_dir4) {
  source("./R/fs/fs_config.R")
  desired_topN <- c('top5', 'top10', 'top20')
  main_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  main_folders2 <- list.dirs(base_dir2, full.names = TRUE, recursive = FALSE)
  main_folders3 <- list.dirs(base_dir3, full.names = TRUE, recursive = FALSE)
  main_folders4 <- list.dirs(base_dir4, full.names = TRUE, recursive = FALSE)
  
  # Initialize a list to store combined data for each dataset
  combined_data <- data.frame()
  for (i in seq_along(main_folders)) {
    main_folder <- main_folders[i]
    dataset <- basename(main_folder)
    
    main_folder2 <- main_folders2[i]
    subdirs <- list.dirs(main_folder, full.names = TRUE, recursive = FALSE)
    up_subdirs2 <- list.dirs(main_folder2, full.names = TRUE, recursive = FALSE)
    subdirs2 <- list.files(up_subdirs2, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
    subdirs2 <- subdirs2[!grepl("rep|seed", subdirs2)]
    
    main_folder3 <- main_folders3[i]
    up_subdirs3 <- list.dirs(main_folder3, full.names = TRUE, recursive = FALSE)
    subdirs3 <- list.files(up_subdirs3, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
    subdirs3 <- subdirs3[!grepl("rep|seed", subdirs3)]
    
    main_folder4 <- main_folders4[i]  # Dataset folder in base_dir4
    up_subdirs4 <- list.dirs(main_folder4, full.names = TRUE, recursive = FALSE)
    subdirs4 <- list.files(up_subdirs4, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
    subdirs4 <- subdirs4[!grepl("rep|seed", subdirs4)]
    methods <- basename(subdirs)
    
    temp_method <- list()
    
    # Process existing metrics and classification data
    for (j in seq_along(subdirs2)) {
      subdir2 <- subdirs2[j]
      
      # Extract method name from the parent directory of subdir2
      method <- basename(dirname(subdir2))
      
      # Extract filename without extension
      filename <- basename(subdir2)
      filename_no_ext <- gsub(".csv$", "", filename)
      
      # Split the filename to get modality and topN
      tokens <- strsplit(filename_no_ext, "_")[[1]]
      modality <- tokens[1]
      topN <- tokens[2]
      
      # Continue only if the topN matches one of the desired_topN
      if (!(topN %in% desired_topN)) {
        next
      }
      
      # Construct new method name combining method, modality, and topN
      method_modality <- paste0(method, "_", toupper(modality), "_", topN)
      
      # Find the matching subdir in subdirs
      ids <- which(methods == method)
      
      if (length(ids) == 0) {
        next
      }
      
      subdir <- subdirs[ids]
      
      # Construct the metric file paths in subdir
      metric_filename <- paste0("metric_", modality, "_", topN, ".csv")
      metric_filename2 <- paste0("metric_asw_iasw_if1_", modality, "_", topN, ".csv")
      metric_path <- file.path(subdir, metric_filename)
      metric_path2 <- file.path(subdir, metric_filename2)
      classification_path <- subdir2
      
      # Check if the necessary files exist
      if (file.exists(metric_path) && file.exists(classification_path)) {
        # Read the CSV files
        metric_data <- read.csv(metric_path)
        metric_data2 <- read.csv(metric_path2)
        metric_data <- metric_data %>%
          dplyr::left_join(metric_data2, by = "X", suffix = c("", ".new")) %>% 
          dplyr::mutate(Value = coalesce(Value.new, Value)) %>% 
          dplyr::select(-Value.new)
        
        classification_data <- read.csv(classification_path)
        
        # Process classification data
        classification_data <- data.frame(
          X = classification_data[, 1],
          Value = as.numeric(rowMeans(classification_data[, 2:ncol(classification_data)]))
        )
        
        # Combine all data
        temp_data <- rbind(metric_data, classification_data)
        temp_data$method <- method_modality  # Assign the new method name
        temp_data$dataset <- dataset
        temp_data$metric <- temp_data$X
        temp_data$value <- as.numeric(temp_data$Value)
        temp_data <- temp_data[, c("metric", "value", "method", "dataset")]
        
        # Store in temp_method
        temp_method[[paste0(method_modality)]] <- temp_data
      } else {
        warning(paste("Missing files for method:", method_modality))
      }
      names(temp_data) <- c("metric", "value", "method", "dataset")
      combined_data <- rbind(combined_data, temp_data)
    }
    
    for (file in subdirs3) {
      # Get the filename without path
      filename <- basename(file)
      # Remove the '_cor.csv' suffix to get method and modality
      filename_no_ext <- gsub("_cor\\.csv$", "", filename)
      # Split the filename to get method and modality
      tokens <- strsplit(filename_no_ext, "_")[[1]]
      method_name <- basename(dirname(file))
      modality <- toupper(tokens[1])
      topN <- gsub(".csv", "", tokens[2])
      method_modality <- paste0(method_name, "_", toupper(modality), "_", topN)
      
      # Read the CSV file
      metric_matrix <- read.csv(file, header = TRUE)
      # Remove the first column (assuming it's indices)
      metric_values <- as.matrix(metric_matrix[, -1])
      # Compute the average value
      avg_value <- mean(metric_values, na.rm = TRUE)
      
      # Create a data frame with the new metric
      new_metric_data <- data.frame(
        metric = "Correlation",  # Use an appropriate metric name
        value = avg_value,
        method = method_modality,
        dataset = dataset,
        stringsAsFactors = FALSE
      )
      
      temp_data <- new_metric_data
      names(temp_data) <- c("metric", "value", "method", "dataset")
      combined_data <- rbind(combined_data, temp_data)
    }
    
    for (metric_file in subdirs4) {
      # Extract modality from filename
      filename <- basename(metric_file)
      # Remove '.csv' to get modality and topN
      filename_no_ext <- gsub(".csv$", "", filename)
      tokens <- strsplit(filename_no_ext, "_")[[1]]
      modality <- tokens[1]  # Extract modality from tokens
      topN <- tokens[2]
      method_name <- basename(dirname(metric_file))
      method_modality <- paste0(method_name, "_", toupper(modality), "_", topN)
      
      if (!(topN %in% desired_topN)) {
        next
      }
      
      # Read the CSV file
      metric_data <- read.csv(metric_file, header = FALSE)
      # Extract the metric value from row 2, column 2
      metric_value <- as.numeric(metric_data[2, 2])
      
      # Create a data frame with the new metric
      new_metric_data <- data.frame(
        metric = "Intersection",  # Name of the metric
        value = metric_value,
        method = method_modality,
        dataset = dataset,
        stringsAsFactors = FALSE
      )
      
      temp_data <- new_metric_data
      names(temp_data) <- c("metric", "value", "method", "dataset")
      combined_data <- rbind(combined_data, temp_data)
    }
    
    names(combined_data) <- c("metric", "value", "method", "dataset")
  }
  return(combined_data)
}



generate_individual_bubble_fs <- function(ds, select_metric, select_method, topN, modality, base_dir, base_dir2, base_dir3, base_dir4, task_category, new_file_paths=NA, new_file_names=NA) {
  if (task_category=="clustering"){source("./R/fs/scIB_knit_table_clustering.R")}
  if (task_category=="repro"){source("./R/fs/scIB_knit_table_repro.R")}
  if (task_category=="classification"){source("./R/fs/scIB_knit_table_classification.R")}
  combined_data <- load_all_data_fs(base_dir,base_dir2,base_dir3,base_dir4)
  select_method <- expand.grid(method = select_method, 
                            mod = modality, 
                            top = topN, 
                            stringsAsFactors = FALSE)
  select_method <- paste(select_method$method, toupper(select_method$mod), select_method$top, sep = "_")
  IsMinmax <- TRUE 
  print(ds)
  data <- combined_data %>% 
    filter(dataset == ds) %>% 
    select(c("metric", "value", "method"))
  data$value <- as.numeric(as.character(data$value))
  data <- reshape2::dcast(data, method ~ metric, value.var = "value", fun.aggregate = mean)
  data <- data[data$method %in% select_method,]
  colnames(data)[colnames(data)=="Intersection"] = "MO"
  colnames(data)[colnames(data)=="Correlation"] = "MC"
  
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


generate_summary_bubble_fs <- function(ds, select_metric, select_method, topN, modality, base_dir, base_dir2, base_dir3, base_dir4, task_category) {
  if (task_category=="clustering"){source("./R/fs/scIB_knit_table_clustering_summary.R")}
  if (task_category=="repro"){source("./R/fs/scIB_knit_table_repro_summary.R")}
  if (task_category=="classification"){source("./R/fs/scIB_knit_table_classification_summary.R")}
  combined_data <- load_all_data_fs(base_dir,base_dir2,base_dir3,base_dir4)
  select_method <- expand.grid(method = select_method, 
                            mod = modality, 
                            top = topN, 
                            stringsAsFactors = FALSE)
  select_method <- paste(select_method$method, toupper(select_method$mod), select_method$top, sep = "_")
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

