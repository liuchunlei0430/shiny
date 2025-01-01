



generate_time_plot_vertical <- function(select_method, data_type, new_file_paths=NA, new_file_names=NA) {
    source("./R/time_speed/vertical/vertical_time_config.R")
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
    if (data_type=="rna_adt"){
      result_Concerto <-read_values_from_csv("./result/time_memory/vertical integration/D7/Concerto/")
      result_Matilda <-read_values_from_csv("./result/time_memory/vertical integration/D7/Matilda/")
      result_moETM <-read_values_from_csv("./result/time_memory/vertical integration/D7/moETM/")
      result_MOFA2 <-read_values_from_csv("./result/time_memory/vertical integration/D7/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/vertical integration/D7/Multigrate/")
      result_sciPENN <-read_values_from_csv("./result/time_memory/vertical integration/D7/sciPENN/")
      result_scMDC <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMoMaT/")
      result_scMSI <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMSI/")
      result_Seurat_WNN <-read_values_from_csv("./result/time_memory/vertical integration/D7/Seurat_v4/")
      result_totalVI <-read_values_from_csv("./result/time_memory/vertical integration/D7/totalVI/")
      result_UINMF <-read_values_from_csv("./result/time_memory/vertical integration/D7/UINMF/")
      result_vimcca <-read_values_from_csv("./result/time_memory/vertical integration/D7/VIMCCA/")
      data_mean <- rbind(result_Matilda, result_Concerto,result_moETM, result_MOFA2,result_Multigrate, result_sciPENN, result_scMDC,  result_scMM, result_scMoMaT, result_scMSI, result_Seurat_WNN,result_totalVI, result_UINMF,result_vimcca)
    }
    if (data_type=="rna_atac"){
      result_iPOLNG <-read_values_from_csv("./result/time_memory/vertical integration/D15/iPOLNG/")
      result_Matilda <-read_values_from_csv("./result/time_memory/vertical integration/D15/Matilda/")
      result_MIRA <-read_values_from_csv("./result/time_memory/vertical integration/D15/MIRA/")
      result_moETM <-read_values_from_csv("./result/time_memory/vertical integration/D15/moETM/")
      result_MOFA2 <-read_values_from_csv("./result/time_memory/vertical integration/D15/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/vertical integration/D15/Multigrate/")
      result_scMDC <-read_values_from_csv("./result/time_memory/vertical integration/D15/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/vertical integration/D15/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/vertical integration/D15/scMoMaT/")
      result_Seurat_WNN <-read_values_from_csv("./result/time_memory/vertical integration/D15/Seurat_v4/")
      result_UINMF <-read_values_from_csv("./result/time_memory/vertical integration/D15/UINMF/")
      result_UnitedNet <-read_values_from_csv("./result/time_memory/vertical integration/D15/UnitedNet/")
      result_vimcca <-read_values_from_csv("./result/time_memory/vertical integration/D15/VIMCCA/")
      data_mean <- rbind(result_iPOLNG, result_Matilda, result_MIRA,result_moETM, result_MOFA2,result_Multigrate, result_scMDC,  result_scMM, result_scMoMaT, result_Seurat_WNN, result_UINMF,result_UnitedNet,result_vimcca)
    }
    if (data_type=="rna_adt_atac"){
      result_Matilda <-read_values_from_csv("./result/time_memory/vertical integration/D22/Matilda/")
      result_MOFA2 <-read_values_from_csv("./result/time_memory/vertical integration/D22/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/vertical integration/D22/Multigrate/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/vertical integration/D22/scMoMaT/")
      result_UINMF <-read_values_from_csv("./result/time_memory/vertical integration/D22/UINMF/")
      data_mean <- rbind(result_Matilda, result_MOFA2,result_Multigrate, result_scMoMaT, result_UINMF)
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
     theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Time") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,120) + labs(y = "Time (minutes)") 
    print(plot)
}






generate_memory_plot_vertical <- function(select_method, data_type, new_file_paths=NA, new_file_names=NA) {
      source("./R/time_speed/vertical/vertical_time_config.R")
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
          a <- read.table(file_path)
          numbers <- strsplit(as.character(a[1,1]), ",")[[1]]
          num1 <- as.numeric(numbers) # 逗号前的数
          num2 <- as.numeric(a[1,2]) # 逗号后
      
          values <- num1+num2
          all_values <- c(all_values, values)
        }
        return(all_values)
      }
    read_values_from_csv3 <- function(root_dir) {
      folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
      all_values <- c()
      for(folder in folders) {
        file_path <- file.path(root_dir, glue("memory_{folder}.csv"))
        values <- read.table(file_path)$V1
        all_values <- c(all_values, values)
      }
      return(all_values)
    }
    read_values_from_csv4 <- function(root_dir) {
      folders <- c("500", "1000", "2000", "3000", "5000", "10000", "20000", "30000", "50000", "80000", "100000")
      all_values <- c()
      for(folder in folders) {
        file_path <- file.path(root_dir, glue("memory_{folder}.csv"))
        a <- read.table(file_path)
        numbers <- strsplit(as.character(a[1,1]), ",")[[1]]
        num1 <- as.numeric(numbers[1]) # 逗号前的数
        num2 <- as.numeric(numbers[2]) # 逗号后
        values <- num1+num2
        all_values <- c(all_values, values)
      }
      return(all_values)
    }
    if (data_type=="rna_adt"){
      result_Concerto <-read_values_from_csv("./result/time_memory/vertical integration/D7/Concerto/")
      result_Matilda <-read_values_from_csv("./result/time_memory/vertical integration/D7/Matilda/")
      result_moETM <-read_values_from_csv("./result/time_memory/vertical integration/D7/moETM/")
      result_MOFA2 <-read_values_from_csv2("./result/time_memory/vertical integration/D7/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/vertical integration/D7/Multigrate/")
      result_sciPENN <-read_values_from_csv("./result/time_memory/vertical integration/D7/sciPENN//")
      result_scMDC <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMoMaT/")
      result_scMSI <-read_values_from_csv("./result/time_memory/vertical integration/D7/scMSI/")
      result_Seurat_WNN <-read_values_from_csv3("./result/time_memory/vertical integration/D7/Seurat_v4/")
      result_totalVI <-read_values_from_csv("./result/time_memory/vertical integration/D7/totalVI/")
      result_UINMF <-read_values_from_csv3("./result/time_memory/vertical integration/D7/UINMF/")
      result_vimcca <-read_values_from_csv("./result/time_memory/vertical integration/D7/VIMCCA/")
      data_mean <- rbind(result_Matilda, result_Concerto,result_moETM, result_MOFA2,result_Multigrate, result_sciPENN, result_scMDC,  result_scMM, result_scMoMaT, result_scMSI, result_Seurat_WNN,result_totalVI, result_UINMF,result_vimcca)
    }
    if (data_type=="rna_atac"){
      result_iPOLNG <-read_values_from_csv("./result/time_memory/vertical integration/D15/iPOLNG/")
      result_Matilda <-read_values_from_csv("./result/time_memory/vertical integration/D15/Matilda/")
      result_MIRA <-read_values_from_csv("./result/time_memory/vertical integration/D15/MIRA/")
      result_moETM <-read_values_from_csv("./result/time_memory/vertical integration/D15/moETM/")
      result_MOFA2 <-read_values_from_csv4("./result/time_memory/vertical integration/D15/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/vertical integration/D15/Multigrate/")
      result_scMDC <-read_values_from_csv("./result/time_memory/vertical integration/D15/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/vertical integration/D15/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/vertical integration/D15/scMoMaT/")
      result_Seurat_WNN <-read_values_from_csv3("./result/time_memory/vertical integration/D15/Seurat_v4/")
      result_UINMF <-read_values_from_csv3("./result/time_memory/vertical integration/D15/UINMF/")
      result_UnitedNet <-read_values_from_csv("./result/time_memory/vertical integration/D15/UnitedNet/")
      result_vimcca <-read_values_from_csv("./result/time_memory/vertical integration/D15/VIMCCA/")
      data_mean <- rbind(result_iPOLNG, result_Matilda, result_MIRA,result_moETM, result_MOFA2,result_Multigrate, result_scMDC,  result_scMM, result_scMoMaT, result_Seurat_WNN, result_UINMF,result_UnitedNet,result_vimcca)
    }
    if (data_type=="rna_adt_atac"){
      result_Matilda <-read_values_from_csv("./result/time_memory/vertical integration/D22/Matilda/")
      result_MOFA2 <-read_values_from_csv4("./result/time_memory/vertical integration/D22/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/vertical integration/D22/Multigrate/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/vertical integration/D22/scMoMaT/")
      result_UINMF <-read_values_from_csv3("./result/time_memory/vertical integration/D22/UINMF/")
      data_mean <- rbind(result_Matilda, result_MOFA2,result_Multigrate, result_scMoMaT, result_UINMF)
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
     theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Memory") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,120)+ labs(y = "Memory (Gbit)") 
    print(plot)
}





