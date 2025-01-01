
generate_time_plot_mosaic <- function(select_method, data_type, new_file_paths=NA, new_file_names=NA) {
  source("./R/time_speed/mosaic/mosaic_time_config.R")
  read_values_from_csv <- function(root_dir) {
    folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
    all_values <- c()
    for(folder in folders) {
      file_path <- file.path(root_dir, glue("time_{folder}.csv"))
      values <- read.table(file_path)$V1 
      all_values <- c(all_values, values)
    }
    return(all_values)
  }
  read_values_from_csv2 <- function(root_dir) {
    folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
    all_values <- c()
    for(folder in folders) {
      file_path <- file.path(root_dir, glue("time_{folder}.csv"))
      values <- read.csv(file_path)#$x 
      all_values <- c(all_values, values)
    }
    return(all_values)
  }
  read_values_from_csv3 <- function(root_dir) {
    folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
    all_values <- c()
    for(folder in folders) {
      file_path <- file.path(root_dir, glue("time_{folder}.csv"))
      values <- read.csv(file_path)$Running_Time_Seconds
      all_values <- c(all_values, values)
    }
    return(all_values)
  }
  read_values_from_csv4 <- function(root_dir) {
    folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
    all_values <- c()
    for(folder in folders) {
      file_path <- file.path(root_dir, glue("time_{folder}.csv"))
      values <- read.csv(file_path)$x 
      all_values <- c(all_values, values)
    }
    return(all_values)
  }
  if (data_type=="rna_adt"){
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D39/Multigrate//")
    result_StabMap <-read_values_from_csv4("./result/time_memory/mosaic integration/D39/StabMap/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D39/scMoMaT/")
    data_mean <- rbind(result_Multigrate, result_scMoMaT, result_StabMap)
  }
  if (data_type=="rna_atac"){
    result_Cobolt <-read_values_from_csv("./result/time_memory/mosaic integration/D45/Cobolt/")
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D45/Multigrate//")
    result_StabMap <-read_values_from_csv4("./result/time_memory/mosaic integration/D45/StabMap/")
    result_MultiVI <-read_values_from_csv("./result/time_memory/mosaic integration/D45/MultiVI/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D45/scMoMaT//")
    result_SMILE <-read_values_from_csv("./result/time_memory/mosaic integration/D45/SMILE//")
    data_mean <- rbind(result_Cobolt, result_Multigrate, result_scMoMaT, result_StabMap, result_MultiVI, result_SMILE)
  }
  if (data_type=="fle_w_sha"){
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D49/Multigrate//")
    result_StabMap <-read_values_from_csv4("./result/time_memory/mosaic integration/D49/StabMap/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D49/scMoMaT/")
    data_mean <- rbind(result_Multigrate, result_scMoMaT, result_StabMap)
  }
  if (data_type=="fle_wo_sha"){
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D46/Multigrate//")
    result_StabMap <-read_values_from_csv4("./result/time_memory/mosaic integration/D46/StabMap/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D46/scMoMaT/")
    result_UINMF <-read_values_from_csv4("./result/time_memory/mosaic integration/D46/UINMF/")
    data_mean <- rbind(result_Multigrate, result_scMoMaT, result_StabMap, result_UINMF)
  }
  
  data_mean <- data_mean/60
  data_mean[data_mean==0] <- NA
  
  if (!is.null(new_file_paths) && length(new_file_paths) > 0 && !all(is.na(new_file_paths))) {
    new_methods_name <- c()
    for (i in c(1:length(new_file_paths))){
      new_method <- read.csv(new_file_paths[i])
      colnames(new_method) <- c("X", "Value")
      base_new_method <- gsub(".csv", "", basename(new_file_names[i]))
      new_method_values <- setNames(new_method$Value, new_method$X)
      new_method_values <- data.frame(new_method_values)
      colnames(new_method_values) <- base_new_method
      data_mean <- rbind(data_mean, t(new_method_values))
      new_methods_name <- c(new_methods_name, base_new_method)
    }
    random_colors <- sample(colors(), length(new_file_paths)) # 随机生成颜色
    names(random_colors) <- new_methods_name
    new_method_colors <- c(method_colors, random_colors)
    select_method <- c(select_method, new_methods_name)
   } else{
     new_method_colors <- method_colors
   }
  
  data_temp <- as.matrix(data_mean)
  rownames(data_temp)<- gsub("result_", "", rownames(data_temp))
  data_melt<-melt(data_temp)
  colnames(data_melt)<-c("Method","Var2","Time")
  data_melt <- data_melt %>% filter(Method %in% select_method)
  method_num <- length(unique(data_melt$Method))
  data_melt$num <- c(rep(500,method_num), rep(1000,method_num), rep(2000,method_num), rep(3000,method_num), rep(5000,method_num), rep(10000,method_num), rep(20000,method_num), rep(30000,method_num), rep(50000,method_num), rep(80000,method_num), rep(100000,method_num))
  plot<-ggplot(data = data_melt, mapping = aes(x = num, y = Time, color=Method)) + geom_line( size=0.5)+ geom_point(size=4)+ 
     theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Time") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,128)+ labs(y = "Time (minutes)") 
  print(plot)
}




generate_memory_plot_mosaic <- function(select_method, data_type, new_file_paths=NA, new_file_names=NA) {
    source("./R/time_speed/mosaic/mosaic_time_config.R")
    read_values_from_csv <- function(root_dir) {
      folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
      all_values <- c()
      for(folder in folders) {
        file_path <- file.path(root_dir, glue("memory_{folder}.csv"))
        a <- read.table(file_path, header = FALSE)
        numbers <- strsplit(as.character(a$V1), ",")[[1]]
        num1 <- as.numeric(numbers[1]) # 逗号前的数
        num2 <- as.numeric(numbers[2]) # 逗号后
    
        values <- num1+num2
        all_values <- c(all_values, values)
      }
      return(all_values)
    }
    read_values_from_csv2 <- function(root_dir) {
      folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
      all_values <- c()
      for(folder in folders) {
        file_path <- file.path(root_dir, glue("memory_{folder}.csv"))
        values <- read.csv(file_path)$x
        all_values <- c(all_values, values)
      }
      return(all_values)
    }
  if (data_type=="rna_adt"){
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D39/Multigrate//")
    result_StabMap <-read_values_from_csv2("./result/time_memory/mosaic integration/D39/StabMap/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D39/scMoMaT/")
    data_mean <- rbind(result_Multigrate, result_scMoMaT, result_StabMap)
  }
  if (data_type=="rna_atac"){
    result_Cobolt <-read_values_from_csv("./result/time_memory/mosaic integration/D45/Cobolt/")
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D45/Multigrate//")
    result_StabMap <-read_values_from_csv2("./result/time_memory/mosaic integration/D45/StabMap/")
    result_MultiVI <-read_values_from_csv("./result/time_memory/mosaic integration/D45/MultiVI/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D45/scMoMaT//")
    result_SMILE <-read_values_from_csv("./result/time_memory/mosaic integration/D45/SMILE//")
    data_mean <- rbind(result_Cobolt, result_Multigrate, result_scMoMaT, result_StabMap, result_MultiVI, result_SMILE)
  }
  if (data_type=="fle_w_sha"){
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D49/Multigrate//")
    result_StabMap <-read_values_from_csv2("./result/time_memory/mosaic integration/D49/StabMap/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D49/scMoMaT/")
    data_mean <- rbind(result_Multigrate, result_scMoMaT, result_StabMap)
  }
  if (data_type=="fle_wo_sha"){
    result_Multigrate <-read_values_from_csv("./result/time_memory/mosaic integration/D46/Multigrate//")
    result_StabMap <-read_values_from_csv2("./result/time_memory/mosaic integration/D46/StabMap/")
    result_scMoMaT <-read_values_from_csv("./result/time_memory/mosaic integration/D46/scMoMaT/")
    result_UINMF <-read_values_from_csv2("./result/time_memory/mosaic integration/D46/UINMF/")
    data_mean <- rbind(result_Multigrate, result_scMoMaT, result_StabMap, result_UINMF)
  }
  
  data_mean <- data_mean/1024
  data_mean[data_mean==0] <- NA
  
  if (!is.null(new_file_paths) && length(new_file_paths) > 0 && !all(is.na(new_file_paths))) {
    new_methods_name <- c()
    for (i in c(1:length(new_file_paths))){
      new_method <- read.csv(new_file_paths[i])
      colnames(new_method) <- c("X", "Value")
      base_new_method <- gsub(".csv", "", basename(new_file_names[i]))
      new_method_values <- setNames(new_method$Value, new_method$X)
      new_method_values <- data.frame(new_method_values)
      colnames(new_method_values) <- base_new_method
      data_mean <- rbind(data_mean, t(new_method_values))
      new_methods_name <- c(new_methods_name, base_new_method)
    }
    random_colors <- sample(colors(), length(new_file_paths)) # 随机生成颜色
    names(random_colors) <- new_methods_name
    new_method_colors <- c(method_colors, random_colors)
    select_method <- c(select_method, new_methods_name)
   } else{
     new_method_colors <- method_colors
   }
  
  data_temp <- as.matrix(data_mean)
  rownames(data_temp)<- gsub("result_", "", rownames(data_temp))
  data_melt<-melt(data_temp)
  colnames(data_melt)<-c("Method","Var2","Time")
  data_melt <- data_melt %>% filter(Method %in% select_method)
  method_num <- length(unique(data_melt$Method))
  data_melt$num <- c(rep(500,method_num), rep(1000,method_num), rep(2000,method_num), rep(3000,method_num), rep(5000,method_num), rep(10000,method_num), rep(20000,method_num), rep(30000,method_num), rep(50000,method_num), rep(80000,method_num), rep(100000,method_num))
  plot<-ggplot(data = data_melt, mapping = aes(x = num, y = Time, color=Method)) + geom_line( size=0.5)+ geom_point(size=4)+ 
     theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Memory") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,128)+ labs(y = "Memory (Gbit)") 
  print(plot)
}







