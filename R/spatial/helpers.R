load_all_data_spatial <- function(base_dir) {
  combined_data <- data.frame()
  main_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  main_folders <- main_folders[mixedorder(main_folders)]
  for (main_folder in main_folders) {
      dataset <- basename(main_folder)
      subdirs <- list.dirs(main_folder, full.names = TRUE, recursive = FALSE)
      
      # Loop through each subdirectory and read the 'metric.csv' file
      for (subdir in subdirs) {
        file_path <- file.path(subdir, "metrics.csv")
        file_path2 <- file.path(subdir, "scs.csv")
    
        if (file.exists(file_path)) {
          temp_data <- read.csv(file_path)
          colnames(temp_data) <- c("X", "Value")
          temp_data2 <- read.csv(file_path2)
          colnames(temp_data2) <- c("X", "Value")
          temp_data2[1,1] <- "SCS"
          temp_data[temp_data$X=="SCS",]$Value <- temp_data2[1,2]

          temp_data$method <- basename(subdir)
          temp_data$dataset <- dataset
          combined_data <- rbind(combined_data, temp_data)
        }
      }
    }
  names(combined_data) <- c("metric", "value", "method", "dataset")
  return(combined_data)
}

generate_summary_bubble_spatial <- function(ds, select_metric, select_method,  base_dir) {
    source("./R/spatial/scIB_knit_table_summary.R")
    combined_data <- load_all_data_spatial(base_dir)
    print(dim(combined_data))
    IsMinmax <- TRUE 
    print(ds)
    ranked_data <- combined_data %>%
        filter(dataset %in% ds) %>% 
        filter(method %in% select_method) %>% 
        filter(metric %in% select_metric) %>% 
        dplyr::group_by(dataset, metric) %>%
        dplyr::mutate(rank = rank(value, ties.method = "max")) %>%
        dplyr::arrange(dataset, metric, rank) 
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
    ranked_data <- apply(temp_data[, 2:(ncol(data))], 2, rank, ties.method = "max")
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
    
    if (length(data[data$method=="PASTEcentre",]$method)>0){data[data$method=="PASTEcentre",]$method <- "PASTE_centre"}
    if (length(data[data$method=="PASTEpairwise",]$method)>0){data[data$method=="PASTEpairwise",]$method <- "PASTE_pairwise"}
    
    row_info <- data.frame(id = rownames(data))
    column_info <- data.frame(id = colnames(data),
                              geom = c(rep("text", 1), rep("bar", dim(data)[2]-1)))

    p <- scIB_knit_table_summary_spatial(data, ranked_data, row_info = row_info, column_info = column_info, na_idx = na_idx, isImage = FALSE)
    print(p)
  }

generate_individual_bubble_spatial <- function(ds, select_metric, select_method,  base_dir, new_file_paths=NA, new_file_names=NA) {
  source("./R/spatial/scIB_knit_table.R")
  combined_data <- load_all_data_spatial(base_dir)
  IsMinmax <- TRUE 
  print(ds)
  data <- combined_data %>% 
    filter(dataset == ds) %>% 
    select(c("metric", "value", "method"))
  data <- dcast(data, method ~ metric, value.var = "value", fun.aggregate = mean)
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