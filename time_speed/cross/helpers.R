



generate_time_plot_cross <- function(select_method, data_type,new_file_paths=NA,new_file_names=NA) {
    source("./R/time_speed/cross/cross_time_config.R")
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
      result_Concerto <-read_values_from_csv("./result/time_memory/cross integration/D53/Concerto/")
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D53/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D53/Multigrate/")
      result_sciPENN <-read_values_from_csv("./result/time_memory/cross integration/D53/sciPENN//")
      result_scMDC <-read_values_from_csv("./result/time_memory/cross integration/D53/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/cross integration/D53/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D53/scMoMaT/")
      result_StabMap <-read_values_from_csv4("./result/time_memory/cross integration/D53/StabMap/")
      result_totalVI <-read_values_from_csv("./result/time_memory/cross integration/D53/totalVI/")
      result_UINMF <-read_values_from_csv4("./result/time_memory/cross integration/D53/UINMF/")
      data_mean <- rbind(result_Concerto, result_MOFA2,result_Multigrate, result_sciPENN, result_scMDC, result_scMM, result_scMoMaT, result_StabMap, result_totalVI, result_UINMF)
    }
    if (data_type=="rna_atac"){
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D56///MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D56/Multigrate/")
      result_scMDC <-read_values_from_csv("./result/time_memory/cross integration/D56/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/cross integration/D56/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D56/scMoMaT/")
      result_StabMap <-read_values_from_csv4("./result/time_memory/cross integration/D56/StabMap/")
      result_UINMF <-read_values_from_csv4("./result/time_memory/cross integration/D56/UINMF/")
      result_UnitedNet <-read_values_from_csv("./result/time_memory/cross integration/D56/UnitedNet/")
      data_mean <- rbind( result_MOFA2,result_Multigrate, result_scMDC,  result_scMM, result_scMoMaT, result_StabMap, result_UINMF,result_UnitedNet)
    }
    if (data_type=="adt_atac"){
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D58///MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D58/Multigrate/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D58/scMoMaT/")
      result_StabMap <-read_values_from_csv4("./result/time_memory/cross integration/D58/StabMap/")
      result_UINMF <-read_values_from_csv4("./result/time_memory/cross integration/D58/UINMF/")
      data_mean <- rbind( result_MOFA2,result_Multigrate, result_scMoMaT, result_StabMap, result_UINMF)
    }
    if (data_type=="rna_adt_atac"){
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D59///MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D59/Multigrate/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D59/scMoMaT/")
      result_StabMap <-read_values_from_csv4("./result/time_memory/cross integration/D59/StabMap/")
      result_UINMF <-read_values_from_csv4("./result/time_memory/cross integration/D59/UINMF/")
      data_mean <- rbind( result_MOFA2,result_Multigrate, result_scMoMaT, result_StabMap, result_UINMF)
    }
    if (data_type =="spatial"){
      result_GPSA <-read_values_from_csv("./result/time_memory/registration/D61/GPSA//")
      result_PASTE_center <-read_values_from_csv("./result/time_memory/registration/D61/PASTE_center//")
      result_PASTE_pairwise <-read_values_from_csv("./result/time_memory/registration/D61/PASTE_pairwise//")
      result_PASTE2 <-read_values_from_csv("./result/time_memory/registration/D61/PASTE2//")
      result_SPIRAL <-read_values_from_csv("./result/time_memory/registration/D61/SPIRAL//")
      data_mean <- rbind(result_GPSA, result_PASTE_center,result_PASTE_pairwise, result_PASTE2,result_SPIRAL)
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
    if (data_type!="spatial"){
      plot<-ggplot(data = data_melt, mapping = aes(x = num, y = Time, color=Method)) + geom_line( size=0.5)+ geom_point(size=4)+ 
       theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Time") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,120)+ labs(y = "Time (minutes)") 
    }
    if (data_type=="spatial"){
      plot<-ggplot(data = data_melt, mapping = aes(x = num, y = Time, color=Method)) + geom_line( size=0.5)+ geom_point(size=4)+ 
       theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Time") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,2800)+ labs(y = "Time (minutes)") 
    }
    print(plot)
}

generate_memory_plot_cross <- function(select_method, data_type,new_file_paths=NA,new_file_names=NA) {
    source("./R/time_speed/cross/cross_time_config.R")
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
    read_values_from_csv3 <- function(root_dir) {
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
      result_Concerto <-read_values_from_csv("./result/time_memory/cross integration/D53/Concerto/")
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D53/MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D53/Multigrate/")
      result_sciPENN <-read_values_from_csv("./result/time_memory/cross integration/D53/sciPENN//")
      result_scMDC <-read_values_from_csv("./result/time_memory/cross integration/D53/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/cross integration/D53/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D53/scMoMaT/")
      result_StabMap <-read_values_from_csv2("./result/time_memory/cross integration/D53/StabMap/")
      result_totalVI <-read_values_from_csv("./result/time_memory/cross integration/D53/totalVI/")
      result_UINMF <-read_values_from_csv2("./result/time_memory/cross integration/D53/UINMF/")
      data_mean <- rbind(result_Concerto, result_MOFA2,result_Multigrate, result_sciPENN, result_scMDC, result_scMM, result_scMoMaT, result_StabMap, result_totalVI, result_UINMF)
    }
    if (data_type=="rna_atac"){
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D56///MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D56/Multigrate/")
      result_scMDC <-read_values_from_csv("./result/time_memory/cross integration/D56/scMDC/")
      result_scMM <-read_values_from_csv("./result/time_memory/cross integration/D56/scMM/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D56/scMoMaT/")
      result_StabMap <-read_values_from_csv2("./result/time_memory/cross integration/D56/StabMap/")
      result_UINMF <-read_values_from_csv2("./result/time_memory/cross integration/D56/UINMF/")
      result_UnitedNet <-read_values_from_csv("./result/time_memory/cross integration/D56/UnitedNet/")
      data_mean <- rbind( result_MOFA2,result_Multigrate, result_scMDC,  result_scMM, result_scMoMaT, result_StabMap, result_UINMF,result_UnitedNet)
    }
    if (data_type=="adt_atac"){
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D58///MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D58/Multigrate/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D58/scMoMaT/")
      result_StabMap <-read_values_from_csv2("./result/time_memory/cross integration/D58/StabMap/")
      result_UINMF <-read_values_from_csv2("./result/time_memory/cross integration/D58/UINMF/")
      data_mean <- rbind( result_MOFA2,result_Multigrate, result_scMoMaT, result_StabMap, result_UINMF)
    }
    if (data_type=="rna_adt_atac"){
      result_MOFA2 <-read_values_from_csv3("./result/time_memory/cross integration/D59///MOFA2/")
      result_Multigrate <-read_values_from_csv("./result/time_memory/cross integration/D59/Multigrate/")
      result_scMoMaT <-read_values_from_csv("./result/time_memory/cross integration/D59/scMoMaT/")
      result_StabMap <-read_values_from_csv2("./result/time_memory/cross integration/D59/StabMap/")
      result_UINMF <-read_values_from_csv2("./result/time_memory/cross integration/D59/UINMF/")
      data_mean <- rbind( result_MOFA2,result_Multigrate, result_scMoMaT, result_StabMap, result_UINMF)
    }
    if (data_type == "spatial"){
      result_GPSA <-read_values_from_csv("./result/time_memory/registration/D61/GPSA//")
      result_PASTE_center <-read_values_from_csv("./result/time_memory/registration/D61/PASTE_center//")
      result_PASTE_pairwise <-read_values_from_csv("./result/time_memory/registration/D61/PASTE_pairwise//")
      result_PASTE2 <-read_values_from_csv("./result/time_memory/registration/D61/PASTE2//")
      result_SPIRAL <-read_values_from_csv("./result/time_memory/registration/D61/SPIRAL//")
      data_mean <- rbind(result_GPSA, result_PASTE_center,result_PASTE_pairwise, result_PASTE2,result_SPIRAL)
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
    if (data_type!="spatial"){
      plot<-ggplot(data = data_melt, mapping = aes(x = num, y = Time, color=Method)) + geom_line( size=0.5)+ geom_point(size=4)+ 
       theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Memory") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,128)+ labs(y = "Memory (Gbit)") 
    }
    if (data_type=="spatial"){
      plot<-ggplot(data = data_melt, mapping = aes(x = num, y = Time, color=Method)) + geom_line( size=0.5)+ geom_point(size=4)+ 
       theme(legend.position="none")+  theme_classic()+ ggtitle(glue("Memory") ) + theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = new_method_colors) +ylim(0,128)+ labs(y = "Memory (Gbit)") 
    }
    print(plot)
}