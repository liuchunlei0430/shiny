library(reshape2)
library(plyr)
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(ggforce)
library(scales)
library(cowplot)
library(png)
library(grid)


server <- function(input, output, session) {
  source("R/helpers.R")
  source("R/spatial/helpers.R")
  source("R/vertical/helpers.R")
  source("R/diagonal/helpers.R")
  source("R/mosaic/helpers.R")
  source("R/cross/helpers.R")
  source("R/imputation/helpers.R")
  source("R/fs/helpers.R")
  source("R/time_speed/vertical/helpers.R")
  source("R/time_speed/diagonal/helpers.R")
  source("R/time_speed/mosaic/helpers.R")
  source("R/time_speed/cross/helpers.R")
  method_data <- read.csv("./files/method.csv")
  data_source <- read.csv("./files/data_source.csv")
  metric_descriptions <- list(
    # Dimension reduction
    "cLISI" = "Cell-type Label Integration Score Index",
    
    # Batch correction
    "iLISI" = "Integration Local Inverse Simpson's Index",
    "kBET" = "k-nearest neighbor batch effect test",
    "GC" = "Graph Connectivity",
    "bARI" = "Batch Adjusted Rand Index",
    "bNMI" = "Batch Normalized Mutual Information",
    "bASW" = "Batch Average Silhouette Width",
    "PCR" = "Principal Component Regression batch",
    
    # Clustering
    "iFI" = "Isolated label F1 score",
    "iASW" = "Isolated label Average Silhouette Width",
    "ARI" = "Cell type Adjusted Rand Index",
    "NMI" = "Cell type Normalized Mutual Information",
    "ASW" = "Cell type Average Silhouette Width",
    
    # Classification
    "OCA" = "Overall Classification Accuracy",
    "ACA" = "Average Classification Accuracy",
    "Spec" = "Specificity",
    "Sens" = "Sensitivity",
    "F1" = "F1 score",
    
    # Feature selection
    "MO" = "Marker Overlap among Different Cell Types",
    "MC" = "Marker Correlation among Downsampled Data",
    
    # Imputation
    "MSE" = "Mean Square Error",
    "PGC" = "Preservation of Gene correlation Structure",
    "PDE" = "Preservation of DE Statistics",
    
    # Spatial registration
    "LTA" = "Label Transfer ARI",
    "PAA" = "Pairwise Alignment Accuracy",
    "SCS" = "Spatial Coherent Score"
  )

  scib_data <- reactive({
    withProgress(message = 'Loading data...', {
      load_all_data("./result/scib_metric/vertical integration")  
    })
  })



  observe({
    modalities <- unique(data_source$modality)
    updateSelectInput(session, "modality_filter",
                     choices = c("All", modalities))
  })
  
 ############## Data Source ################
  filtered_data <- reactive({
    data <- data_source
    
    if (input$modality_filter != "All") {
      data <- data[data$modality == input$modality_filter, ]
    }
    
    if (!is.null(input$search_text) && input$search_text != "") {
      search_pattern <- tolower(input$search_text)
      data <- data[grepl(search_pattern, tolower(paste(
        data[,1],          
        data$modality,
        data$protocol,
        data$description,
        sep = " "
      ))), ]
    }
    
    data[,1] <- ifelse(
      !is.na(data[,8]) & data[,8] != "",
      paste0('<a href="', data[,8],
             '" target="_blank" style="text-decoration: underline; color:blue;">',
             data[,1], '</a>'),
      data[,1]
    )
    
    data <- data[,-8]
    
    return(data)
  })
  
  output$total_datasource <- renderValueBox({
    total <- nrow(filtered_data())
    valueBox(
      value = total,
      subtitle = "Total Data Sources",
      color = "blue"
    )
  })

  output$protocol_plot <- renderPlotly({
    protocol_counts <- table(data_source$protocol)
    df <- data.frame(
      protocol = names(protocol_counts),
      count = as.numeric(protocol_counts)
    )
    
    plot_ly(
      df,
      labels = ~protocol,
      values = ~count,
      type = 'pie',
      textposition = 'inside',
      hoverinfo = 'text',
      text = ~paste(protocol, ':', count),
      textinfo = 'text',
      marker = list(line = list(color = '#FFFFFF', width = 1))
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 30, b = 30),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.2,
          font = list(size = 8)
        )
      )
  })

  observeEvent(input$zoom_plot, {
    showModal(
      modalDialog(
        title = "Full Size Protocol Distribution",
        plotlyOutput("modal_plot", height = "600px"),
        size = "l",
        easyClose = TRUE
      )
    )
  })

  output$modal_plot <- renderPlotly({
    protocol_counts <- table(data_source$protocol)
    df <- data.frame(
      protocol = names(protocol_counts),
      count = as.numeric(protocol_counts)
    )
    plot_ly(
      df,
      labels = ~protocol,
      values = ~count,
      type = 'pie',
      textposition = 'inside',
      hoverinfo = 'text',
      text = ~paste(protocol, ':', count),
      textinfo = 'text',
      marker = list(line = list(color = '#FFFFFF', width = 1))
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 30, b = 30),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.2,
          font = list(size = 8)
        )
      )
  })

  output$data_source <- DT::renderDataTable({
    df <- filtered_data()
    
    df[,2] <- ifelse(
      grepl("^https?://", df[,2], ignore.case = TRUE),
      paste0("<a href='", df[,2], 
            "' target='_blank' style='text-decoration: underline; color:blue;'>Data Link</a>"),
      df[,2]
    )
    
    custom_col_names <- c(
      "data.source.name",                        
      "Accession Number/Data Link",        
      "protocol", "modality", "species", "tissue", "batch_num"
    )
    
    DT::datatable(
      df,
      colnames = custom_col_names,
      escape = FALSE,   
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        responsive = FALSE,
        autoWidth = TRUE

      )
    ) %>%
      formatStyle(
        columns = 7,
        `text-align` = "center"
      ) %>%
      formatStyle(
        columns = 2,
        `text-align` = "center"
      ) 
  })

######## Data Source End ################


####### Dataset ################
  update_filter_choices <- function(data, column_name) {
    choices <- unique(data[[column_name]])
    choices <- choices[!is.na(choices) & choices != ""]
    choices <- sort(choices)  
    
    choices <- c("All" = "", choices)
    return(choices)
  }

  # 读取数据集
  dataset_data <- reactive({
    data <- read.csv("./files/dataset.csv", stringsAsFactors = FALSE)
    
    # 转换列名以匹配数据文件的实际列名
    colnames(data) <- make.names(colnames(data))
    
    # 根据Integration列分组
    split(data, data$Integration)
  })

  # 为每种整合类型创建过滤和显示功能
  integration_types <- c("Vertical integration", "Diagonal integration", 
                        "Mosaic integration", "Cross integration", 
                        "Spatial Registration")

  # 动态创建观察器和输出
  observe({
    data <- dataset_data()
    
    for (type in integration_types) {
      local({
        local_type <- type
        prefix <- tolower(strsplit(local_type, " ")[[1]][1])
        
        # 仅当有数据时更新过滤器
        if (!is.null(data[[local_type]])) {
          type_data <- data[[local_type]]
          
          # 更新所有过滤器，确保包含"All"选项并默认选中
          updateSelectInput(session, 
                          paste0(prefix, "_data_source"),
                          choices = update_filter_choices(type_data, "data.source"),
                          selected = "")  # 空字符串对应"All"选项
          
          updateSelectInput(session, 
                          paste0(prefix, "_protocol"),
                          choices = update_filter_choices(type_data, "protocol"),
                          selected = "")
          
          updateSelectInput(session, 
                          paste0(prefix, "_modality"),
                          choices = update_filter_choices(type_data, "modality"),
                          selected = "")
          
          updateSelectInput(session, 
                          paste0(prefix, "_species"),
                          choices = update_filter_choices(type_data, "species"),
                          selected = "")
          
          updateSelectInput(session, 
                          paste0(prefix, "_tissue"),
                          choices = update_filter_choices(type_data, "tissue"),
                          selected = "")
        }
      })
    }
  })

  # 为每种整合类型创建数据表输出
  lapply(integration_types, function(type) {
    local({
      local_type <- type
      prefix <- tolower(strsplit(local_type, " ")[[1]][1])
      
      output[[paste0(prefix, "_dataset_table")]] <- renderDT({
        # 获取该整合类型的数据
        data <- dataset_data()[[local_type]]
        
        # 获取当前过滤器值
        filters <- list(
          data.source = input[[paste0(prefix, "_data_source")]],
          protocol = input[[paste0(prefix, "_protocol")]],
          modality = input[[paste0(prefix, "_modality")]],
          species = input[[paste0(prefix, "_species")]],
          tissue = input[[paste0(prefix, "_tissue")]]
        )
        
        # 应用过滤器
        if (!is.null(data)) {
          # 对每个非空的过滤器进行过滤
          for (col in names(filters)) {
            if (!is.null(filters[[col]]) && filters[[col]] != "") {
              data <- data[data[[col]] == filters[[col]], ]
            }
          }
          
          display_cols <- c("dataset.name", "data.source", "protocol", "modality", 
                          "species", "tissue", "original_feature_num", 
                          "original_cell_num", "final_feature_num", "final_cell_num")
          
          data <- data[, display_cols]
          
          col_names <- c("Dataset", "Data Source", "Protocol", "Modality", 
                        "Species", "Tissue", "Original Features", 
                        "Original Cells", "Final Features", "Final Cells")
          
          colnames(data) <- col_names
          
    
          # 创建数据表
          DT::datatable(
            data,
            options = list(
              pageLength = 15,  # 增加每页显示的行数
              scrollX = TRUE,
              dom = 'lrtip',
              ordering = TRUE,  # 允许排序
              searchHighlight = TRUE  # 高亮搜索结果
            ),
            rownames = FALSE,
            class = 'cell-border stripe hover'  # 添加样式类
          ) %>%
          formatStyle(
            columns = col_names,
            backgroundColor = 'white',
            borderBottom = '1px solid #ddd'
          )
        }
      })
    })
  })

 ################ Dataset End ################


 ######### Method ################
  method_data_processed <- reactive({
    data <- as.data.frame(method_data, stringsAsFactors = FALSE)
    data[] <- lapply(data, as.character)
    
    safe_split <- function(x, split_char = "\n") {
      if (is.na(x) || !is.character(x)) return(character(0))
      unique(trimws(unlist(strsplit(x, split_char))))
    }
    
    data_structures_raw <- unique(unlist(lapply(data$Data.Stucture., function(x) {
      if (is.na(x)) return(character(0))
      safe_split(gsub("\\[|\\]", "", x))
    })))
    data_structures_raw <- data_structures_raw[data_structures_raw != ""]
    data_structures <- sort(data_structures_raw)

    peak_gene_opts  <- c("All", sort(unique(data$Peak.Gene.Activity[!is.na(data$Peak.Gene.Activity) & data$Peak.Gene.Activity != ""])))
    output_types    <- c("All", sort(unique(data$Output[!is.na(data$Output) & data$Output != ""])))
    
    prog_langs <- unique(unlist(lapply(data$Programming.Language, function(x) {
      safe_split(x, "/")
    })))
    prog_langs <- prog_langs[prog_langs != ""]
    prog_langs <- sort(prog_langs)
    prog_langs <- c("All", prog_langs)
    
    # 提取深度学习选项
    deep_learning_opts <- unique(data$Deep.Learning)
    deep_learning_opts <- deep_learning_opts[!is.na(deep_learning_opts)]
    deep_learning_opts <- c("All", sort(deep_learning_opts))

    # 提取celltype要求
    celltype_opts <- unique(data$CellType.Information.Required)
    celltype_opts <- celltype_opts[!is.na(celltype_opts)]
    celltype_opts <- c("All", sort(celltype_opts))

    # 提取集成类别
    integration_cats <- unique(unlist(lapply(data$Integration.Categories, safe_split)))
    integration_cats <- integration_cats[integration_cats != "" & integration_cats != "N/A"]

    # 提取任务类别
    task_cats <- unique(unlist(lapply(data$Task.Categories., safe_split)))
    task_cats <- task_cats[task_cats != "" & task_cats != "N/A"]

    # 更新选择框选项
    updateSelectizeInput(session, "data_structure", 
                        choices = sort(data_structures),
                        selected =  "")
    updateSelectizeInput(session, "peak_gene", 
                        choices = sort(peak_gene_opts),
                        selected = "All")
    updateSelectizeInput(session, "output_type", 
                        choices = sort(output_types),
                        selected = "All")
    updateSelectizeInput(session, "prog_lang", 
                        choices = sort(prog_langs),
                        selected = "All")
    updateSelectizeInput(session, "deep_learning", 
                        choices = sort(deep_learning_opts),
                        selected = "All")
    updateSelectizeInput(session, "cell_type", 
                        choices = sort(celltype_opts),
                        selected = "All")
    updateSelectizeInput(session, "integration_cat", 
                        choices = sort(integration_cats),
                        selected = "")
    updateSelectizeInput(session, "task_cat", 
                        choices = sort(task_cats),
                        selected = "")
    
    return(data)
  })
  
  # 过滤方法数据
  filtered_method_data <- reactive({
    data <- method_data_processed()
    
    # 打印处理前的列名（调试用）
    print("Columns before filtering:")
    print(names(data))
    
    # 安全的字符串分割和检查函数
    safe_check <- function(x, target_values, split_char = "\n") {
        if (is.na(x) || !is.character(x)) return(FALSE)
        current_values <- trimws(unlist(strsplit(gsub("\\[|\\]", "", x), split_char)))
        # 改为"且"的关系（需要包含所有选中的值）
        all(target_values %in% current_values)
    }
    
    # 多选筛选条件（使用交集）
    if (!is.null(input$data_structure) && length(input$data_structure) > 0) {
        data <- data[sapply(data$Data.Stucture., function(x) safe_check(x, input$data_structure)), ]
    }
    
    if (!is.null(input$integration_cat) && length(input$integration_cat) > 0) {
        data <- data[sapply(data$Integration.Categories, 
                        function(x) safe_check(x, input$integration_cat)), ]
    }
    
    if (!is.null(input$task_cat) && length(input$task_cat) > 0) {
        data <- data[sapply(data$Task.Categories., 
                        function(x) safe_check(x, input$task_cat)), ]
    }
    
    if (!is.null(input$peak_gene) && input$peak_gene != "All") {
        data <- data[data$Peak.Gene.Activity == input$peak_gene, ]
    }
    
    if (!is.null(input$output_type) && input$output_type != "All") {
        data <- data[data$Output == input$output_type, ]
    }
    
    if (!is.null(input$prog_lang) && input$prog_lang != "All") {
        data <- data[data$Programming.Language == input$prog_lang, ]
    }
    
    if (!is.null(input$deep_learning) && input$deep_learning != "All") {
        data <- data[data$Deep.Learning == input$deep_learning, ]
    }
    
    if (!is.null(input$cell_type) && input$cell_type != "All") {
        data <- data[data$CellType.Information.Required == input$cell_type, ]
    }
    
    if ("Code.Link" %in% names(data)) {
        print("Processing Code Link...")
        data$Methods <- ifelse(
        !is.na(data$Code.Link) & data$Code.Link != "",
        paste0('<a href="', data$Code.Link, 
                '" target="_blank" style="text-decoration: underline;">', 
                data$Methods, '</a>'),
        data$Methods
        )
        data <- data[, !(names(data) %in% "Code.Link"), drop = FALSE]
    }
    
    print("Columns after all processing:")
    print(names(data))
    
    return(data)
    })
  


  output$method_table <- DT::renderDataTable({
    data <- filtered_method_data()

    safe_split <- function(x, split_char = "\n") {
      if (is.na(x) || !is.character(x)) return(character(0))
      unique(trimws(unlist(strsplit(x, split_char))))
    }
    
    data$Data.Stucture. <- sapply(data$Data.Stucture., function(x) {
      items <- safe_split(x, "\n")          # 或者根据你实际的分割逻辑
      items <- items[items != ""]           # 过滤空白
      paste0(items, collapse = "<br/>")     # 用 <br/> 拼成多行
    })

    data$Integration.Categories <- sapply(data$Integration.Categories, function(x) {
      items <- safe_split(x, "\n")
      items <- items[items != ""]
      paste0(items, collapse = "<br/>")
    })

    data$Task.Categories. <- sapply(data$Task.Categories., function(x) {
      items <- safe_split(x, "\n")
      items <- items[items != ""]
      paste0(items, collapse = "<br/>")
    })    

    DT::datatable(
        data,
        escape = FALSE,
        filter = "none",  
        options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'lrtip',
        scrollX = TRUE,  
        responsive = TRUE,
        search = list(
            regex = TRUE, 
            caseInsensitive = TRUE,
            smart = TRUE
        )
        )
    ) %>%
        formatStyle(
        'Methods',
        color = 'blue',
        textDecoration = 'underline',
        cursor = 'pointer'
        )
    })

 ################ Method End ################


  output$background_image <- renderImage({
    # 指定图片的完整路径
    filename <- normalizePath(file.path('www/plots/cover_image.png'))
    
    # 返回图片信息
    list(src = filename,
        width = "100%",
        height = "100%",
        style = "object-fit: contain;")
  }, deleteFile = FALSE)

  
  observe({
    # 打印工作目录
    print(paste("Working directory:", getwd()))
    # 检查www目录是否存在
    print(paste("www directory exists:", dir.exists("www")))
    # 检查plots目录是否存在
    print(paste("plots directory exists:", dir.exists("www/plots")))
    # 列出www/plots目录下的文件
    print(paste("Files in plots directory:", paste(list.files("www/plots"), collapse=", ")))
  })


############# Introduction Evaluation ################
  metric_data <- reactive({
    print("Reading data file...")
    df <- read.csv("./files/metric_full.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # 处理链接
    df$Metric <- ifelse(
      !is.na(df$link) & df$link != "",
      sprintf('<a href="%s" target="_blank">%s</a>', df$link, df$Metric),
      df$Metric
    )
    
    # 简化的细节显示按钮
    df$Details <- sprintf(
      '<button class="btn btn-info btn-sm details-control">+</button>'
    )
    
    # 创建可折叠的详情部分
    df$FullDetails <- sprintf(
      '<div class="details-container">
        <div class="detail-section">
          <div class="detail-header" onclick="toggleSection(this)">
            <span class="toggle-icon">▶</span>
            <span class="header-text">Description</span>
          </div>
          <div class="detail-content" style="display: none;">
            <p>%s</p>
          </div>
        </div>
        <div class="detail-section">
          <div class="detail-header" onclick="toggleSection(this)">
            <span class="toggle-icon">▶</span>
            <span class="header-text">Advantages</span>
          </div>
          <div class="detail-content" style="display: none;">
            <p>%s</p>
          </div>
        </div>
        <div class="detail-section">
          <div class="detail-header" onclick="toggleSection(this)">
            <span class="toggle-icon">▶</span>
            <span class="header-text">Disadvantages</span>
          </div>
          <div class="detail-content" style="display: none;">
            <p>%s</p>
          </div>
        </div>
      </div>',
      df$Description,
      df$Advantage,
      df$Disadvantage
    )
    
    return(df)
  })

  filtered_metric_data <- reactive({
    req(input$metric_tabs)
    data <- metric_data()
    
    filtered <- data[data$Task == input$metric_tabs, ]
    
    if(nrow(filtered) > 0) {
      result <- data.frame(
        Metric = filtered$Metric,
        "Label Required" = filtered$Label_required,
        Graph = filtered$Graph,
        Embedding = filtered$Embedding,
        Details = filtered$Details,
        FullDetails = filtered$FullDetails,  # 添加完整详情列，但在显示时会隐藏
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      return(result)
    } else {
      return(data.frame(
        Metric = character(0),
        "Label Required" = character(0),
        Graph = character(0),
        Embedding = character(0),
        Details = character(0),
        FullDetails = character(0),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
  })

  output$metric_table <- DT::renderDataTable({
    data <- filtered_metric_data()
    
    DT::datatable(
      data,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip',
        columnDefs = list(
          list(visible = FALSE, targets = 5),  # 隐藏 FullDetails 列
          list(className = 'dt-center', targets = "_all")
        )
      ),
      callback = JS("
        table.on('click', 'button.details-control', function() {
          var tr = $(this).closest('tr');
          var row = table.row(tr);
          if (row.child.isShown()) {
            row.child.hide();
            $(this).html('+');
          } else {
            row.child(row.data()[5]).show();  // 显示 FullDetails 列的内容
            $(this).html('-');
          }
        });
      "),
      selection = 'none'
    )
  })


############# Introduction Evaluation End ################

##################### Vertical Summary #####################

  vertical_datasets_rna_adt <- c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","SD1","SD2")
  vertical_methods_rna_adt <- c("Concerto", "Matilda", "moETM", "MOFA2", "Multigrate", 
                       "sciPENN", "scMDC", "scMM", "scMoMaT", "scMSI", 
                       "Seurat.WNN", "totalVI", "UINMF", "VIMCCA")
  
  vertical_datasets_rna_atac <- c("D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","SD3","SD4")
  vertical_methods_rna_atac <- c("iPOLNG", "Matilda", "MIRA", "moETM", "MOFA2", "Multigrate", 
                        "scMDC", "scMM", "scMoMaT", "Seurat.WNN", "UINMF", 
                        "UnitedNet", "VIMCCA")
  
  vertical_datasets_rna_adt_atac <- c("D22","D23","SD5","SD6")
  vertical_methods_rna_adt_atac <- c("Matilda", "MOFA2", "Multigrate", "scMoMaT", "UINMF")
  
  # 当 data type 改变时更新datasets和methods选项
  observeEvent(input$vertical_dr_data_type, {
    if (input$vertical_dr_data_type == "rna_adt") {
      updateSelectizeInput(session, "vertical_dr_dataset_choice", 
        choices = vertical_datasets_rna_adt, selected = NULL)
      updateSelectizeInput(session, "vertical_dr_method_choice",
        choices = vertical_methods_rna_adt, selected = NULL)
    } else if (input$vertical_dr_data_type == "rna_atac") {
      updateSelectizeInput(session, "vertical_dr_dataset_choice", 
        choices = vertical_datasets_rna_atac, selected = NULL)
      updateSelectizeInput(session, "vertical_dr_method_choice",
        choices = vertical_methods_rna_atac, selected = NULL)
    } else if (input$vertical_dr_data_type == "rna_adt_atac") {
      updateSelectizeInput(session, "vertical_dr_dataset_choice", 
        choices = vertical_datasets_rna_adt_atac, selected = NULL)
      updateSelectizeInput(session, "vertical_dr_method_choice",
        choices = vertical_methods_rna_adt_atac, selected = NULL)
    }
  })


  output$vertical_metric_info <- renderUI({
    selected_metrics <- input$vertical_metric_choice
    if (length(selected_metrics) > 0) {
      metric_info <- lapply(selected_metrics, function(m) {
        if (!is.null(metric_descriptions[[m]])) {
          tags$div(
            tags$span(style = "font-weight: bold;", m), ": ",
            metric_descriptions[[m]]
          )
        }
      })
      do.call(tags$div, c(list(style="padding: 10px;"), metric_info))
    }
  })  

  output$vertical_dr_summary_plot <- renderPlot({
    req(length(input$vertical_dr_dataset_choice) > 0)
    req(length(input$vertical_dr_method_choice) >= 2)
    req(length(input$vertical_dr_metric_choice) > 0)
    
    dataset <- input$vertical_dr_dataset_choice
    metric <- input$vertical_dr_metric_choice
    method <- input$vertical_dr_method_choice
    
    p <- generate_summary_bubble_vertical(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/vertical integration"  # 根据实际路径修改
    )
    
    p <- p + theme(
      plot.margin = unit(c(0, 0, 0, 0), "pt")
    )
    p
  }, height = 540)

  output$download_vertical_dr_summary <- downloadHandler(
    filename = function() {
      paste("vertical_summary_", paste(input$vertical_dr_dataset_choice, collapse = "_"), "_",
            paste(input$vertical_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_vertical(
        ds = input$vertical_dr_dataset_choice,
        select_metric = input$vertical_dr_metric_choice,
        select_method = input$vertical_dr_method_choice,
        base_dir = "./result/scib_metric/vertical integration"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
##################### Vertical Summary End ##########################


  ################## Vertical Dataset-specific #######################
  # 数据类型与数据集、方法映射
  vertical_ds_data_map <- list(
    rna_adt = list(
      datasets = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","SD1","SD2"),
      methods = c("Concerto", "Matilda","moETM", "MOFA2","Multigrate","sciPENN","scMDC","scMM","scMoMaT","scMSI","Seurat.WNN","totalVI","UINMF","VIMCCA")
    ),
    rna_atac = list(
      datasets = c("D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","SD3","SD4"),
      methods = c("iPOLNG", "Matilda", "MIRA", "moETM", "MOFA2", "Multigrate", 
                  "scMDC", "scMM", "scMoMaT", "Seurat.WNN", "UINMF", 
                  "UnitedNet", "VIMCCA")
    ),
    rna_adt_atac = list(
      datasets = c("D22","D23","SD5","SD6"),
      methods = c("Matilda", "MOFA2", "Multigrate", "scMoMaT", "UINMF")
    )
  )
  observeEvent(input$vertical_ds_data_type, {
    dt <- input$vertical_ds_data_type
    available_datasets <- vertical_ds_data_map[[dt]]$datasets
    available_methods <- vertical_ds_data_map[[dt]]$methods
    updateSelectizeInput(session, "vertical_ds_dataset_choice",
                        choices = NULL, server = TRUE, selected = character(0))    
    updateSelectizeInput(session, "vertical_ds_dataset_choice",
                        choices = available_datasets, server = TRUE, selected = character(0))
    updateSelectizeInput(session, "vertical_ds_method_choice",
                        choices = NULL, server = TRUE, selected = character(0))
    updateSelectizeInput(session, "vertical_ds_method_choice",
                        choices = available_methods, server = TRUE, selected = character(0))
    updateSelectizeInput(session, "vertical_ds_metric_choice",
                        choices = c("cLISI","iFI","iASW","ASW","ARI","NMI"),
                        selected = character(0), server = TRUE)
  })

  fileInputCounter <- reactiveVal(0)

  observeEvent(input$vertical_ds_dataset_choice, {
    fileInputCounter(fileInputCounter() + 1)
    new_input_id <- paste0("vertical_new_file_", fileInputCounter())
    
    output$vertical_ds_file_ui <- renderUI({
      fileInput(new_input_id, "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  output$vertical_ds_file_ui <- renderUI({
    fileInput("vertical_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$vertical_ds_plot <- renderPlot({
    req(input$vertical_ds_dataset_choice)
    req(length(input$vertical_ds_method_choice) >= 2)
    req(length(input$vertical_ds_metric_choice) > 0)
    
    dataset <- input$vertical_ds_dataset_choice
    metric <- input$vertical_ds_metric_choice
    method <- input$vertical_ds_method_choice

    # 获取当前的 inputId
    current_input_id <- paste0("vertical_new_file_", fileInputCounter())
    
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- vertical_ds_generate_individual_bubble(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/vertical integration", 
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  },height = 540)

  output$download_vertical_demo_csv <- downloadHandler(
    filename = function() {
      "vertical_demo.csv"  # 用户下载时看到的文件名
    },
    content = function(file) {
      file.copy("./result/demo_data/new_method2.csv", file)
    }
  )
  output$download_vertical_ds_plot <- downloadHandler(
    filename = function() {
      paste("vertical_", input$vertical_ds_dataset_choice, "_",
            paste(input$vertical_ds_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      dataset <- input$vertical_ds_dataset_choice
      metric <- input$vertical_ds_metric_choice
      method <- input$vertical_ds_method_choice
      
      # 与renderPlot中一致的获取方式
      current_input_id <- paste0("vertical_new_file_", fileInputCounter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- vertical_ds_generate_individual_bubble(
        ds = dataset,
        select_metric = metric,
        select_method = method,
        base_dir = "./result/scib_metric/vertical integration",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## Vertical Dataset-specific End #######################


  ############### Feature Selection Summary ############################
  fs_data_map <- list(
    rna_adt = list(
      datasets = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","SD1","SD2"),
      modality = c("RNA","ADT")
    ),
    rna_atac = list(
      datasets = c("D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","SD3","SD4"),
      modality = c("RNA","ATAC")
    ),
    rna_adt_atac = list(
      datasets = c("D22","D23","SD5","SD6"),
      modality = c("RNA","ADT","ATAC")
    )
  )
  
  observeEvent(input$fs_clustering_data_type, {
    dt <- input$fs_clustering_data_type
    available_datasets <- fs_data_map[[dt]]$datasets
    available_modality <- fs_data_map[[dt]]$modality

    updateSelectizeInput(session, "fs_clustering_dataset_choice",
                        choices = NULL, selected = NULL,server = TRUE)
    updateSelectizeInput(session, "fs_clustering_dataset_choice",
                        choices = available_datasets, selected = NULL,server = TRUE)
    updateSelectizeInput(session, "fs_clustering_modality_choice",
                        choices = NULL, selected = NULL,server = TRUE)
    updateSelectizeInput(session, "fs_clustering_modality_choice",
                        choices = available_modality, selected = NULL,server = TRUE)
  })

  # Classification summary
  observeEvent(input$fs_classification_data_type, {
    dt <- input$fs_classification_data_type
    available_datasets <- fs_data_map[[dt]]$datasets
    available_modality <- fs_data_map[[dt]]$modality
    updateSelectizeInput(session, "fs_classification_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)    
    updateSelectizeInput(session, "fs_classification_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "fs_classification_modality_choice",
                        choices = NULL, selected = character(0),server = TRUE)                        
    updateSelectizeInput(session, "fs_classification_modality_choice",
                        choices = available_modality, selected = character(0),server = TRUE)
  })

  # Repro summary
  observeEvent(input$fs_repro_data_type, {
    dt <- input$fs_repro_data_type
    available_datasets <- fs_data_map[[dt]]$datasets
    available_modality <- fs_data_map[[dt]]$modality
    updateSelectizeInput(session, "fs_repro_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)    
    updateSelectizeInput(session, "fs_repro_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "fs_repro_modality_choice",
                        choices = NULL, selected = character(0),server = TRUE)                        
    updateSelectizeInput(session, "fs_repro_modality_choice",
                        choices = available_modality, selected = character(0),server = TRUE)
  })



  # Clustering summary
  output$fs_clustering_summary_plot <- renderPlot({
    req(length(input$fs_clustering_dataset_choice) > 0)
    req(length(input$fs_clustering_method_choice) >= 2)
    req(length(input$fs_clustering_metric_choice) > 0)
    req(length(input$fs_clustering_topN_choice) > 0)
    req(length(input$fs_clustering_modality_choice) > 0)

    p <- generate_summary_bubble_fs(
      ds = input$fs_clustering_dataset_choice,
      select_metric = input$fs_clustering_metric_choice,
      select_method = input$fs_clustering_method_choice,
      topN = input$fs_clustering_topN_choice,
      modality = input$fs_clustering_modality_choice,
      base_dir = "./result/scib_metric/fs/",
      base_dir2 = "./result/classification_metrics/fs/",
      base_dir3 = "./result/fs_cor/",
      base_dir4 = "./result/fs_intersection/",
      task_category = "clustering"
    )
    p <- p + theme(plot.margin = unit(c(0,0,0,0),"pt"))
    p
  }, height = 540)
  
  output$download_fs_clustering_summary <- downloadHandler(
    filename = function() {
      paste("fs_clustering_summary_",
            paste(input$fs_clustering_dataset_choice, collapse="_"), "_",
            paste(input$fs_clustering_metric_choice, collapse="_"), ".pdf", sep="")
    },
    content = function(file) {
      p <- generate_summary_bubble_fs(
        ds = input$fs_clustering_dataset_choice,
        select_metric = input$fs_clustering_metric_choice,
        select_method = input$fs_clustering_method_choice,
        topN = input$fs_clustering_topN_choice,
        modality = input$fs_clustering_modality_choice,
        base_dir = "./result/scib_metric/fs/",
        base_dir2 = "./result/classification_metrics/fs/",
        base_dir3 = "./result/fs_cor/",
        base_dir4 = "./result/fs_intersection/",
        task_category = "clustering"
      )
      ggsave(file, plot = p, width=12, height=16)
    }
  )

  # Classification summary
  output$fs_classification_summary_plot <- renderPlot({
    req(length(input$fs_classification_dataset_choice) > 0)
    req(length(input$fs_classification_method_choice) >= 2)
    req(length(input$fs_classification_metric_choice) > 0)
    req(length(input$fs_classification_topN_choice) > 0)
    req(length(input$fs_classification_modality_choice) > 0)

    p <- generate_summary_bubble_fs(
      ds = input$fs_classification_dataset_choice,
      select_metric = input$fs_classification_metric_choice,
      select_method = input$fs_classification_method_choice,
      topN = input$fs_classification_topN_choice,
      modality = input$fs_classification_modality_choice,
      base_dir = "./result/scib_metric/fs/",
      base_dir2 = "./result/classification_metrics/fs/",
      base_dir3 = "../result/fs_cor/",
      base_dir4 = "./result/fs_intersection/",
      task_category = "classification"
    )
    p <- p + theme(plot.margin = unit(c(0,0,0,0),"pt"))
    p
  }, height = 540)

  output$download_fs_classification_summary <- downloadHandler(
    filename = function() {
      paste("fs_classification_summary_",
            paste(input$fs_classification_dataset_choice, collapse="_"), "_",
            paste(input$fs_classification_metric_choice, collapse="_"), ".pdf", sep="")
    },
    content = function(file) {
      p <- generate_summary_bubble_fs(
        ds = input$fs_classification_dataset_choice,
        select_metric = input$fs_classification_metric_choice,
        select_method = input$fs_classification_method_choice,
        topN = input$fs_classification_topN_choice,
        modality = input$fs_classification_modality_choice,
        base_dir = "./result/scib_metric/fs/",
        base_dir2 = "./result/classification_metrics/fs/",
        base_dir3 = "./result/fs_cor/",
        base_dir4 = "./result/fs_intersection/",
        task_category = "classification"
      )
      ggsave(file, plot = p, width=12, height=16)
    }
  )

  # Repro summary
  output$fs_repro_summary_plot <- renderPlot({
    req(length(input$fs_repro_dataset_choice) > 0)
    req(length(input$fs_repro_method_choice) >= 2)
    req(length(input$fs_repro_metric_choice) > 0)
    req(length(input$fs_repro_topN_choice) > 0)
    req(length(input$fs_repro_modality_choice) > 0)

    p <- generate_summary_bubble_fs(
      ds = input$fs_repro_dataset_choice,
      select_metric = input$fs_repro_metric_choice,
      select_method = input$fs_repro_method_choice,
      topN = input$fs_repro_topN_choice,
      modality = input$fs_repro_modality_choice,
      base_dir = "./result/scib_metric/fs/",
      base_dir2 = "./result/classification_metrics/fs/",
      base_dir3 = "./result/fs_cor/",
      base_dir4 = "./result/fs_intersection/",
      task_category = "repro"
    )
    p <- p + theme(plot.margin = unit(c(0,0,0,0),"pt"))
    p
  }, height=540)

  output$download_fs_repro_summary <- downloadHandler(
    filename = function() {
      paste("fs_repro_summary_",
            paste(input$fs_repro_dataset_choice, collapse="_"), "_",
            paste(input$fs_repro_metric_choice, collapse="_"), ".pdf", sep="")
    },
    content = function(file) {
      p <- generate_summary_bubble_fs(
        ds = input$fs_repro_dataset_choice,
        select_metric = input$fs_repro_metric_choice,
        select_method = input$fs_repro_method_choice,
        topN = input$fs_repro_topN_choice,
        modality = input$fs_repro_modality_choice,
        base_dir = "./result/scib_metric/fs/",
        base_dir2 = "./result/classification_metrics/fs/",
        base_dir3 = "./result/fs_cor/",
        base_dir4 = "./result/fs_intersection/",
        task_category = "repro"
      )
      ggsave(file, plot = p, width=12, height=16)
    }
  )
 ##################### Feature Selection Summary End #####################


  ################### Feature Selection Dataset-Specific ##################
  observeEvent(input$fs_ds_clustering_data_type, {
    dt <- input$fs_ds_clustering_data_type
    available_datasets <- fs_data_map[[dt]]$datasets
    available_modality <- fs_data_map[[dt]]$modality

    updateSelectizeInput(session, "fs_ds_clustering_dataset_choice",
                        choices = available_datasets, server = TRUE, selected = character(0))
    updateSelectizeInput(session, "fs_ds_clustering_modality_choice",
                        choices = available_modality, server = TRUE, selected = character(0))

    fs_clustering_file_counter(fs_clustering_file_counter() + 1)
    output$fs_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("fs_ds_clustering_new_file_", fs_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  })

  observeEvent(input$fs_ds_classification_data_type, {
    dt <- input$fs_ds_classification_data_type
    available_datasets <- fs_data_map[[dt]]$datasets
    available_modality <- fs_data_map[[dt]]$modality
    updateSelectizeInput(session, "fs_ds_classification_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)    
    updateSelectizeInput(session, "fs_ds_classification_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "fs_ds_classification_modality_choice",
                        choices = NULL, selected = character(0),server = TRUE)                        
    updateSelectizeInput(session, "fs_ds_classification_modality_choice",
                        choices = available_modality, selected = character(0),server = TRUE)
    fs_classification_file_counter(fs_classification_file_counter() + 1)
    output$fs_ds_classification_file_ui <- renderUI({
      fileInput(paste0("fs_ds_classification_new_file_", fs_classification_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })

  })

  observeEvent(input$fs_ds_repro_data_type, {
    dt <- input$fs_ds_repro_data_type
    available_datasets <- fs_data_map[[dt]]$datasets
    available_modality <- fs_data_map[[dt]]$modality

    updateSelectizeInput(session, "fs_ds_repro_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)    
    updateSelectizeInput(session, "fs_ds_repro_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "fs_ds_repro_modality_choice",
                        choices = NULL, selected = character(0),server = TRUE)                        
    updateSelectizeInput(session, "fs_ds_repro_modality_choice",
                        choices = available_modality, selected = character(0),server = TRUE)
    
    fs_repro_file_counter(fs_repro_file_counter() + 1)
    output$fs_ds_repro_file_ui <- renderUI({
      fileInput(paste0("fs_ds_repro_new_file_", fs_repro_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  })  

  fs_clustering_file_counter <- reactiveVal(0)




  observeEvent(input$fs_ds_clustering_dataset_choice, {
    fs_clustering_file_counter(fs_clustering_file_counter() + 1)
    output$fs_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("fs_ds_clustering_new_file_", fs_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  })

  # 初次加载UI的fileInput
  output$fs_ds_clustering_file_ui <- renderUI({
    fileInput("fs_ds_clustering_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  # Demo CSV下载
  output$download_fs_clustering_demo_csv <- downloadHandler(
    filename = function() { "fs_demo.csv" },
    content = function(file) {
      file.copy("./result/demo_data/fs_demo.csv", file)
    }
  )

  # Clustering ds
  output$fs_ds_clustering_plot <- renderPlot({
    req(length(input$fs_ds_clustering_dataset_choice) > 0)
    req(length(input$fs_ds_clustering_method_choice) >= 2)
    req(length(input$fs_ds_clustering_metric_choice) > 0)
    req(length(input$fs_ds_clustering_topN_choice) > 0)
    req(length(input$fs_ds_clustering_modality_choice) > 0)

    current_input_id <- paste0("fs_ds_clustering_new_file_", fs_clustering_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_fs(
      ds = input$fs_ds_clustering_dataset_choice,
      select_metric = input$fs_ds_clustering_metric_choice,
      select_method = input$fs_ds_clustering_method_choice,
      topN = input$fs_ds_clustering_topN_choice,
      modality = input$fs_ds_clustering_modality_choice,
      base_dir = "./result/scib_metric/fs/",
      base_dir2 = "./result/classification_metrics/fs/",
      base_dir3 = "./result/fs_cor/",
      base_dir4 = "./result/fs_intersection/",
      task_category = "clustering",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height=540)

  output$download_fs_ds_clustering_plot <- downloadHandler(
    filename = function() {
      paste("fs_ds_clustering_",
            paste(input$fs_ds_clustering_dataset_choice, collapse="_"), "_",
            paste(input$fs_ds_clustering_metric_choice, collapse="_"), ".pdf", sep="")
    },
    content = function(file) {

      dataset <- input$fs_ds_clustering_dataset_choice
      metric <- input$fs_ds_clustering_metric_choice
      method <- input$fs_ds_clustering_method_choice
      topN <- input$fs_ds_clustering_topN_choice
      modality <- input$fs_ds_clustering_modality_choice

      current_input_id <- paste0("fs_ds_clustering_new_file_", fs_clustering_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_fs(
        ds = input$fs_ds_clustering_dataset_choice,
        select_metric = input$fs_ds_clustering_metric_choice,
        select_method = input$fs_ds_clustering_method_choice,
        topN = input$fs_ds_clustering_topN_choice,
        modality = input$fs_ds_clustering_modality_choice,
        base_dir = "./result/scib_metric/fs/",
        base_dir2 = "./result/classification_metrics/fs/",
        base_dir3 = "./result/fs_cor/",
        base_dir4 = "./result/fs_intersection/",
        task_category = "clustering",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width=12, height=16)
    }
  )

  # Classification ds
  fs_classification_file_counter <- reactiveVal(0)

 

  output$fs_ds_classification_file_ui <- renderUI({
    fileInput("fs_ds_classification_new_file_", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  observeEvent(input$fs_ds_classification_dataset_choice, {
    fs_classification_file_counter(fs_classification_file_counter() + 1)
    output$fs_ds_classification_file_ui <- renderUI({
      fileInput(paste0("fs_ds_classification_new_file_", fs_classification_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  }) 

  output$download_fs_classification_demo_csv <- downloadHandler(
    filename = function() { "fs_demo.csv" },
    content = function(file) {
      file.copy("./result/demo_data/fs_demo.csv", file)
    }
  )

  output$fs_ds_classification_plot <- renderPlot({
    req(length(input$fs_ds_classification_dataset_choice) > 0)
    req(length(input$fs_ds_classification_method_choice) >= 2)
    req(length(input$fs_ds_classification_metric_choice) > 0)
    req(length(input$fs_ds_classification_topN_choice) > 0)
    req(length(input$fs_ds_classification_modality_choice) > 0)

    current_input_id <- paste0("fs_ds_classification_new_file_", fs_classification_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_fs(
      ds = input$fs_ds_classification_dataset_choice,
      select_metric = input$fs_ds_classification_metric_choice,
      select_method = input$fs_ds_classification_method_choice,
      topN = input$fs_ds_classification_topN_choice,
      modality = input$fs_ds_classification_modality_choice,
      base_dir = "./result/scib_metric/fs/",
      base_dir2 = "./result/classification_metrics/fs/",
      base_dir3 = "./result/fs_cor/",
      base_dir4 = "./result/fs_intersection/",
      task_category = "classification",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names

    )
    p
  }, height=540)

  output$download_fs_ds_classification_plot <- downloadHandler(
    filename = function() {
      paste("fs_ds_classification_",
            paste(input$fs_ds_classification_dataset_choice, collapse="_"), "_",
            paste(input$fs_ds_classification_metric_choice, collapse="_"), ".pdf", sep="")
    },
    content = function(file) {
      p <- generate_individual_bubble_fs(
        ds = input$fs_ds_classification_dataset_choice,
        select_metric = input$fs_ds_classification_metric_choice,
        select_method = input$fs_ds_classification_method_choice,
        topN = input$fs_ds_classification_topN_choice,
        modality = input$fs_ds_classification_modality_choice,
        base_dir = "./result/scib_metric/fs/",
        base_dir2 = "./result/classification_metrics/fs/",
        base_dir3 = "./result/fs_cor/",
        base_dir4 = "./result/fs_intersection/",
        task_category = "classification",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width=12, height=16)
    }
  )

  # Repro ds


  fs_repro_file_counter <- reactiveVal(0)

 

  output$fs_ds_repro_file_ui <- renderUI({
    fileInput("fs_ds_repro_new_file_", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  observeEvent(input$fs_ds_repro_dataset_choice, {
    fs_repro_file_counter(fs_repro_file_counter() + 1)
    output$fs_ds_repro_file_ui <- renderUI({
      fileInput(paste0("fs_ds_repro_new_file_", fs_repro_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  }) 

  output$download_fs_repro_demo_csv <- downloadHandler(
    filename = function() { "fs_demo.csv" },
    content = function(file) {
      file.copy("./result/demo_data/fs_demo.csv", file)
    }
  )
  output$fs_ds_repro_plot <- renderPlot({
    req(length(input$fs_ds_repro_dataset_choice) > 0)
    req(length(input$fs_ds_repro_method_choice) >= 2)
    req(length(input$fs_ds_repro_metric_choice) > 0)
    req(length(input$fs_ds_repro_topN_choice) > 0)
    req(length(input$fs_ds_repro_modality_choice) > 0)
    current_input_id <- paste0("fs_ds_repro_new_file_", fs_repro_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }


    p <- generate_individual_bubble_fs(
      ds = input$fs_ds_repro_dataset_choice,
      select_metric = input$fs_ds_repro_metric_choice,
      select_method = input$fs_ds_repro_method_choice,
      topN = input$fs_ds_repro_topN_choice,
      modality = input$fs_ds_repro_modality_choice,
      base_dir = "./result/scib_metric/fs/",
      base_dir2 = "./result/classification_metrics/fs/",
      base_dir3 = "./result/fs_cor/",
      base_dir4 = "./result/fs_intersection/",
      task_category = "repro",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names

    )
    p
  }, height=540)

  output$download_fs_ds_repro_plot <- downloadHandler(
    filename = function() {
      paste("fs_ds_repro_",
            paste(input$fs_ds_repro_dataset_choice, collapse="_"), "_",
            paste(input$fs_ds_repro_metric_choice, collapse="_"), ".pdf", sep="")
    },
    content = function(file) {
      p <- generate_individual_bubble_fs(
        ds = input$fs_ds_repro_dataset_choice,
        select_metric = input$fs_ds_repro_metric_choice,
        select_method = input$fs_ds_repro_method_choice,
        topN = input$fs_ds_repro_topN_choice,
        modality = input$fs_ds_repro_modality_choice,
        base_dir = "./result/scib_metric/fs/",
        base_dir2 = "./result/classification_metrics/fs/",
        base_dir3 = "./result/fs_cor/",
        base_dir4 = "./result/fs_intersection/",
        task_category = "repro",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width=12, height=16)
    }
  )
################# Feature Selection Dataset-Specific End ####################




  ################## spatial registration summary ###################
  
  
  output$spatial_metric_info <- renderUI({
    selected_metrics <- input$spatial_metric_choice
    if (length(selected_metrics) > 0) {
      metric_info <- lapply(selected_metrics, function(m) {
        if (!is.null(metric_descriptions[[m]])) {
          tags$div(
            tags$span(style = "font-weight: bold;", m), ": ",
            metric_descriptions[[m]]
          )
        }
      })
      do.call(tags$div, c(list(style="padding: 10px;"), metric_info))
    }
  })  

  output$spatial_summary_plot <- renderPlot({
    req(length(input$spatial_dataset_choice) > 0)
    req(length(input$spatial_method_choice) >= 2)
    req(length(input$spatial_metric_choice) > 0)
    
    dataset <- input$spatial_dataset_choice
    metric <- input$spatial_metric_choice
    method <- input$spatial_method_choice
    
    p <- generate_summary_bubble_spatial(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/registration_clean/"  # 根据实际路径修改
    )
    
    p <- p + theme(
      plot.margin = unit(c(0, 0, 0, 0), "pt")
    )
    p
  }, height = 540)

  output$download_spatial_summary <- downloadHandler(
    filename = function() {
      paste("spatial_summary_", paste(input$spatial_dataset_choice, collapse = "_"), "_",
            paste(input$spatial_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_spatial(
        ds = input$spatial_dataset_choice,
        select_metric = input$spatial_metric_choice,
        select_method = input$spatial_method_choice,
        base_dir = "./result/registration_clean/"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## spatial registration summary End ###################


  ################## spatial registration dataset specific ###################
  

  output$spatial_file_ui <- renderUI({
    fileInput("spatial_new_file_", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })
  
  spatial_file_counter <- reactiveVal(0)

  output$download_spatial_demo_csv <- downloadHandler(
  filename = function() { "spatial_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/spatial_demo.csv", file)
  }
  )
  
  observeEvent(input$cross_ds_spatial_dataset_choice, {
    spatial_file_counter(spatial_file_counter() + 1)
    output$spatial_file_ui <- renderUI({
      fileInput(paste0("spatial_new_file_", spatial_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })


  output$spatial_metric_info <- renderUI({
    selected_metrics <- input$cross_ds_spatial_metric_choice
    if (length(selected_metrics) > 0) {
      metric_info <- lapply(selected_metrics, function(m) {
        if (!is.null(metric_descriptions[[m]])) {
          tags$div(
            tags$span(style = "font-weight: bold;", m), ": ",
            metric_descriptions[[m]]
          )
        }
      })
      do.call(tags$div, c(list(style="padding: 10px;"), metric_info))
    }
  })  

  output$cross_ds_spatial_plot <- renderPlot({
    req(length(input$cross_ds_spatial_dataset_choice) > 0)
    req(length(input$cross_ds_spatial_method_choice) >= 2)
    req(length(input$cross_ds_spatial_metric_choice) > 0)
    
    dataset <- input$cross_ds_spatial_dataset_choice
    metric <- input$cross_ds_spatial_metric_choice
    method <- input$cross_ds_spatial_method_choice
    current_input_id <- paste0("spatial_new_file_", spatial_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_spatial(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/registration_clean/",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names  
    )
    
    p <- p + theme(
      plot.margin = unit(c(0, 0, 0, 0), "pt")
    )
    p
  }, height = 540)

  output$download_cross_ds_spatial_plot <- downloadHandler(
    filename = function() {
      paste("spatial_summary_", paste(input$cross_ds_spatial_dataset_choice, collapse = "_"), "_",
            paste(input$cross_ds_spatial_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("spatial_new_file_", spatial_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_spatial(
        ds = input$cross_ds_spatial_dataset_choice,
        select_metric = input$cross_ds_spatial_metric_choice,
        select_method = input$cross_ds_spatial_method_choice,
        base_dir = "./result/registration_clean/",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## spatial registration dataset specific End ###################


  ################## diagonal integration summary ###################
  diagonal_data_map <- list(
    rna_atac = list(
      datasets = c("D24","D25","D26","D27","D28","D29","D30","D31","D32","D33","SD7","SD8"),
      methods = c("Conos", "GLUE", "iNMF", "MultiMAP", "online.iNMF", "Portal", "SCALEX", "scBridge", "sciCAN", "scJoint", "Seurat.v3", "Seurat.v5", "uniPort", "VIPCCA")
    ),
    multi_multi = list(
      datasets = c("D34","D35","D36","D37","SD9","SD10"),
      methods = c("Conos","GLUE","iNMF","online.iNMF","scJoint") 
    )
  )

  observeEvent(input$diagonal_dr_data_type, ignoreInit = FALSE, {
      dt <- input$diagonal_dr_data_type
      available_datasets <- diagonal_data_map[[dt]]$datasets
      available_methods <- diagonal_data_map[[dt]]$methods
      updateSelectizeInput(session, "diagonal_dr_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "diagonal_dr_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_dr_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_dr_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$diagonal_batch_data_type,ignoreInit = FALSE, {
      dt <- input$diagonal_batch_data_type
      available_datasets <- diagonal_data_map[[dt]]$datasets
      available_methods <- diagonal_data_map[[dt]]$methods
      updateSelectizeInput(session, "diagonal_batch_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "diagonal_batch_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_batch_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)                          
      updateSelectizeInput(session, "diagonal_batch_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$diagonal_class_data_type,ignoreInit = FALSE, {
      dt <- input$diagonal_class_data_type
      available_datasets <- diagonal_data_map[[dt]]$datasets
      available_methods <- diagonal_data_map[[dt]]$methods
      updateSelectizeInput(session, "diagonal_class_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "diagonal_class_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_class_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)                          
      updateSelectizeInput(session, "diagonal_class_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })



  output$diagonal_dr_summary_plot <- renderPlot({
    req(length(input$diagonal_dr_dataset_choice) > 0)
    req(length(input$diagonal_dr_method_choice) >= 2)
    req(length(input$diagonal_dr_metric_choice) > 0)

    dataset <- input$diagonal_dr_dataset_choice
    metric <- input$diagonal_dr_metric_choice
    method <- input$diagonal_dr_method_choice

    # 使用task_category = "clustering"
    p <- generate_summary_bubble_diagonal(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/diagonal integration",
      task_category = "clustering"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_diagonal_dr_summary <- downloadHandler(
    filename = function() {
      paste("diagonal_dr_summary_", paste(input$diagonal_dr_dataset_choice, collapse = "_"), "_",
            paste(input$diagonal_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_diagonal(
        ds = input$diagonal_dr_dataset_choice,
        select_metric = input$diagonal_dr_metric_choice,
        select_method = input$diagonal_dr_method_choice,
        base_dir = "./result/scib_metric/diagonal integration",
        task_category = "clustering"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )  


  output$diagonal_batch_summary_plot <- renderPlot({
    req(length(input$diagonal_batch_dataset_choice) > 0)
    req(length(input$diagonal_batch_method_choice) >= 2)
    req(length(input$diagonal_batch_metric_choice) > 0)

    dataset <- input$diagonal_batch_dataset_choice
    metric <- input$diagonal_batch_metric_choice
    method <- input$diagonal_batch_method_choice

    # 使用task_category = "batchcorrection"
    p <- generate_summary_bubble_diagonal(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/diagonal integration",
      task_category = "batchcorrection"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_diagonal_batch_summary <- downloadHandler(
    filename = function() {
      paste("diagonal_batch_summary_", paste(input$diagonal_batch_dataset_choice, collapse = "_"), "_",
            paste(input$diagonal_batch_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_diagonal(
        ds = input$diagonal_batch_dataset_choice,
        select_metric = input$diagonal_batch_metric_choice,
        select_method = input$diagonal_batch_method_choice,
        base_dir = "./result/scib_metric/diagonal integration",
        task_category = "batchcorrection"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )


  output$diagonal_class_summary_plot <- renderPlot({
    req(length(input$diagonal_class_dataset_choice) > 0)
    req(length(input$diagonal_class_method_choice) >= 2)
    req(length(input$diagonal_class_metric_choice) > 0)

    dataset <- input$diagonal_class_dataset_choice
    metric <- input$diagonal_class_metric_choice
    method <- input$diagonal_class_method_choice

    # 使用task_category = "classification"
    p <- generate_summary_bubble_diagonal(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/diagonal integration",
      task_category = "classification"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_diagonal_class_summary <- downloadHandler(
    filename = function() {
      paste("diagonal_class_summary_", paste(input$diagonal_class_dataset_choice, collapse = "_"), "_",
            paste(input$diagonal_class_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_diagonal(
        ds = input$diagonal_class_dataset_choice,
        select_metric = input$diagonal_class_metric_choice,
        select_method = input$diagonal_class_method_choice,
        base_dir = "./result/scib_metric/diagonal integration",
        task_category = "classification"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## diagonal integration summary End ###################

  #################################
  ### Diagonal Dataset-Specific ###
  #################################
  
  output$diagonal_ds_clustering_file_ui <- renderUI({
    fileInput("diagonal_ds_clustering_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$diagonal_ds_batch_file_ui <- renderUI({
    fileInput("diagonal_ds_batch_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$diagonal_ds_class_file_ui <- renderUI({
    fileInput("diagonal_ds_class_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  diagonal_clustering_file_counter <- reactiveVal(0)
  diagonal_batch_file_counter <- reactiveVal(0)
  diagonal_class_file_counter <- reactiveVal(0)

  output$download_diagonal_cluster_demo_csv <- downloadHandler(
  filename = function() { "diagonal_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/diagonal_demo.csv", file)
  }
  )
  


  output$download_diagonal_batch_demo_csv <- downloadHandler(
  filename = function() { "diagonal_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/diagonal_demo.csv", file)
  }
  )


  output$download_diagonal_class_demo_csv <- downloadHandler(
  filename = function() { "diagonal_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/diagonal_demo.csv", file)
  }
  )
  observeEvent(input$diagonal_ds_dr_data_type,ignoreInit = FALSE, {
      dt <- input$diagonal_ds_dr_data_type
      available_datasets <- diagonal_data_map[[dt]]$datasets
      available_methods <- diagonal_data_map[[dt]]$methods
      updateSelectizeInput(session, "diagonal_ds_dr_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "diagonal_ds_dr_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_ds_dr_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)                          
      updateSelectizeInput(session, "diagonal_ds_dr_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)

      diagonal_clustering_file_counter(diagonal_clustering_file_counter() + 1)
      output$diagonal_ds_clustering_file_ui <- renderUI({
        fileInput(paste0("diagonal_ds_clustering_new_file_", diagonal_clustering_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
                          
  })

  observeEvent(input$diagonal_ds_batch_data_type,ignoreInit = FALSE, {
      dt <- input$diagonal_ds_batch_data_type
      available_datasets <- diagonal_data_map[[dt]]$datasets
      available_methods <- diagonal_data_map[[dt]]$methods
      updateSelectizeInput(session, "diagonal_ds_batch_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "diagonal_ds_batch_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_ds_batch_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)                          
      updateSelectizeInput(session, "diagonal_ds_batch_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
      diagonal_batch_file_counter(diagonal_batch_file_counter() + 1)
      output$diagonal_ds_batch_file_ui <- renderUI({
        fileInput(paste0("diagonal_ds_batch_new_file_", diagonal_batch_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$diagonal_ds_class_data_type, {
      dt <- input$diagonal_ds_class_data_type
      available_datasets <- diagonal_data_map[[dt]]$datasets
      available_methods <- diagonal_data_map[[dt]]$methods
      updateSelectizeInput(session, "diagonal_ds_class_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "diagonal_ds_class_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_ds_class_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "diagonal_ds_class_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
      diagonal_class_file_counter(diagonal_class_file_counter() + 1)
      output$diagonal_ds_class_file_ui <- renderUI({
        fileInput(paste0("diagonal_ds_class_new_file_", diagonal_class_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })    
  
  observeEvent(input$diagonal_ds_dr_dataset_choice, {
    diagonal_clustering_file_counter(diagonal_clustering_file_counter() + 1)
    output$diagonal_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("diagonal_ds_clustering_new_file_", diagonal_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$diagonal_ds_batch_dataset_choice, {
    diagonal_batch_file_counter(diagonal_batch_file_counter() + 1)
    output$diagonal_ds_batch_file_ui <- renderUI({
      fileInput(paste0("diagonal_ds_batch_new_file_", diagonal_batch_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$diagonal_ds_class_dataset_choice, {
    diagonal_class_file_counter(diagonal_class_file_counter() + 1)
    output$diagonal_ds_class_file_ui <- renderUI({
      fileInput(paste0("diagonal_ds_class_new_file_", diagonal_class_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })



  #### DR and Clustering ####
  output$diagonal_ds_dr_plot <- renderPlot({
    req(length(input$diagonal_ds_dr_dataset_choice) > 0)
    req(length(input$diagonal_ds_dr_method_choice) >= 2)
    req(length(input$diagonal_ds_dr_metric_choice) > 0)

    current_input_id <- paste0("diagonal_ds_clustering_new_file_", diagonal_clustering_file_counter())

    dataset <- input$diagonal_ds_dr_dataset_choice
    metric <- input$diagonal_ds_dr_metric_choice
    method <- input$diagonal_ds_dr_method_choice
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_diagonal(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/diagonal integration", 
      task_category = "clustering" ,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_diagonal_ds_dr_plot <- downloadHandler(
    filename = function() {
      paste("diagonal_ds_dr_", paste(input$diagonal_ds_dr_dataset_choice, collapse = "_"), "_",
            paste(input$diagonal_ds_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("diagonal_ds_clustering_new_file_", diagonal_clustering_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- generate_individual_bubble_diagonal(
        ds = input$diagonal_ds_dr_dataset_choice,
        select_metric = input$diagonal_ds_dr_metric_choice,
        select_method = input$diagonal_ds_dr_method_choice,
        base_dir = "./result/scib_metric/diagonal integration",
        task_category = "clustering",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  #### Batch Correction ####
  output$diagonal_ds_batch_plot <- renderPlot({
    req(length(input$diagonal_ds_batch_dataset_choice) > 0)
    req(length(input$diagonal_ds_batch_method_choice) >= 2)
    req(length(input$diagonal_ds_batch_metric_choice) > 0)

    dataset <- input$diagonal_ds_batch_dataset_choice
    metric <- input$diagonal_ds_batch_metric_choice
    method <- input$diagonal_ds_batch_method_choice
    current_input_id <- paste0("diagonal_ds_batch_new_file_", diagonal_batch_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }
    p <- generate_individual_bubble_diagonal(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/diagonal integration",
      task_category = "batchcorrection",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_diagonal_ds_batch_plot <- downloadHandler(
    filename = function() {
      paste("diagonal_ds_batch_", paste(input$diagonal_ds_batch_dataset_choice, collapse = "_"), "_",
            paste(input$diagonal_ds_batch_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {

      current_input_id <- paste0("diagonal_ds_batch_new_file_", diagonal_batch_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_diagonal(
        ds = input$diagonal_ds_batch_dataset_choice,
        select_metric = input$diagonal_ds_batch_metric_choice,
        select_method = input$diagonal_ds_batch_method_choice,
        base_dir = "./result/scib_metric/diagonal integration",
        task_category = "batchcorrection",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  #### Classification ####
  output$diagonal_ds_class_plot <- renderPlot({
    req(length(input$diagonal_ds_class_dataset_choice) > 0)
    req(length(input$diagonal_ds_class_method_choice) >= 2)
    req(length(input$diagonal_ds_class_metric_choice) > 0)

    dataset <- input$diagonal_ds_class_dataset_choice
    metric <- input$diagonal_ds_class_metric_choice
    method <- input$diagonal_ds_class_method_choice

    current_input_id <- paste0("diagonal_ds_class_new_file_", diagonal_class_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_diagonal(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/diagonal integration",
      task_category = "classification",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_diagonal_ds_class_plot <- downloadHandler(
    filename = function() {
      paste("diagonal_ds_class_", paste(input$diagonal_ds_class_dataset_choice, collapse = "_"), "_",
            paste(input$diagonal_ds_class_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("diagonal_ds_class_new_file_", diagonal_class_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_diagonal(
        ds = input$diagonal_ds_class_dataset_choice,
        select_metric = input$diagonal_ds_class_metric_choice,
        select_method = input$diagonal_ds_class_method_choice,
        base_dir = "./result/scib_metric/diagonal integration",
        task_category = "classification",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## diagonal specific End ###################

  ################## mosaic integration summary ###################


  mosaic_data_map <- list(
  rna_rnaadt_adt = list(
      datasets = c("D38","D39", "D40", "D41", "SD11", "SD12"),
      methods = c("StabMap", "scMoMaT", "Multigrate")
    ),
  rna_rnaatac_atac = list(
      datasets = c("D42","D43","D44","D45","SD13","SD14"),
      methods = c("MultiVI","Cobolt","scMoMaT","Multigrate","StabMap", "SMILE") 
    ),

  mixed_wi_share = list(
      datasets = c("D46","D47"),
      methods = c("StabMap", "UINMF", "Multigrate", "scMoMaT") 
    ),

  mixed_wo_share = list(
      datasets = c("D48","D49", "D50"),
      methods = c("StabMap", "Multigrate", "scMoMaT") 
    )  
  )
  
  observeEvent(input$mosaic_dr_data_type, ignoreInit = FALSE, {
      dt <- input$mosaic_dr_data_type
      available_datasets <- mosaic_data_map[[dt]]$datasets
      available_methods <- mosaic_data_map[[dt]]$methods
      updateSelectizeInput(session, "mosaic_dr_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "mosaic_dr_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_dr_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_dr_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$mosaic_batch_data_type,ignoreInit = FALSE, {
      dt <- input$mosaic_batch_data_type
      available_datasets <- mosaic_data_map[[dt]]$datasets
      available_methods <- mosaic_data_map[[dt]]$methods
      updateSelectizeInput(session, "mosaic_batch_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "mosaic_batch_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_batch_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_batch_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$mosaic_class_data_type,ignoreInit = FALSE, {
      dt <- input$mosaic_class_data_type
      available_datasets <- mosaic_data_map[[dt]]$datasets
      available_methods <- mosaic_data_map[[dt]]$methods
      updateSelectizeInput(session, "mosaic_class_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "mosaic_class_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_class_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)                          
      updateSelectizeInput(session, "mosaic_class_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })



  
  output$mosaic_dr_summary_plot <- renderPlot({
    req(length(input$mosaic_dr_dataset_choice) > 0)
    req(length(input$mosaic_dr_method_choice) >= 2)
    req(length(input$mosaic_dr_metric_choice) > 0)

    dataset <- input$mosaic_dr_dataset_choice
    metric <- input$mosaic_dr_metric_choice
    method <- input$mosaic_dr_method_choice

    # 使用task_category = "clustering"
    p <- generate_summary_bubble_mosaic(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/mosaic integration",
      task_category = "clustering"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_mosaic_dr_summary <- downloadHandler(
    filename = function() {
      paste("mosaic_dr_summary_", paste(input$mosaic_dr_dataset_choice, collapse = "_"), "_",
            paste(input$mosaic_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_mosaic(
        ds = input$mosaic_dr_dataset_choice,
        select_metric = input$mosaic_dr_metric_choice,
        select_method = input$mosaic_dr_method_choice,
        base_dir = "./result/scib_metric/mosaic integration",
        task_category = "clustering"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )  


  output$mosaic_batch_summary_plot <- renderPlot({
    req(length(input$mosaic_batch_dataset_choice) > 0)
    req(length(input$mosaic_batch_method_choice) >= 2)
    req(length(input$mosaic_batch_metric_choice) > 0)

    dataset <- input$mosaic_batch_dataset_choice
    metric <- input$mosaic_batch_metric_choice
    method <- input$mosaic_batch_method_choice

    # 使用task_category = "batchcorrection"
    p <- generate_summary_bubble_mosaic(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/mosaic integration",
      task_category = "batchcorrection"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_mosaic_batch_summary <- downloadHandler(
    filename = function() {
      paste("mosaic_batch_summary_", paste(input$mosaic_batch_dataset_choice, collapse = "_"), "_",
            paste(input$mosaic_batch_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_mosaic(
        ds = input$mosaic_batch_dataset_choice,
        select_metric = input$mosaic_batch_metric_choice,
        select_method = input$mosaic_batch_method_choice,
        base_dir = "./result/scib_metric/mosaic integration",
        task_category = "batchcorrection"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )


  output$mosaic_class_summary_plot <- renderPlot({
    req(length(input$mosaic_class_dataset_choice) > 0)
    req(length(input$mosaic_class_method_choice) >= 2)
    req(length(input$mosaic_class_metric_choice) > 0)

    dataset <- input$mosaic_class_dataset_choice
    metric <- input$mosaic_class_metric_choice
    method <- input$mosaic_class_method_choice

    # 使用task_category = "classification"
    p <- generate_summary_bubble_mosaic(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/mosaic integration",
      task_category = "classification"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_mosaic_class_summary <- downloadHandler(
    filename = function() {
      paste("mosaic_class_summary_", paste(input$mosaic_class_dataset_choice, collapse = "_"), "_",
            paste(input$mosaic_class_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_mosaic(
        ds = input$mosaic_class_dataset_choice,
        select_metric = input$mosaic_class_metric_choice,
        select_method = input$mosaic_class_method_choice,
        base_dir = "./result/scib_metric/mosaic integration",
        task_category = "classification"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## mosaic integration summary End ###################



  #################################
  ### mosaic Dataset-Specific ###
  #################################


  output$mosaic_ds_clustering_file_ui <- renderUI({
    fileInput("mosaic_ds_clustering_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$mosaic_ds_batch_file_ui <- renderUI({
    fileInput("mosaic_ds_batch_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$mosaic_ds_class_file_ui <- renderUI({
    fileInput("mosaic_ds_class_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  mosaic_clustering_file_counter <- reactiveVal(0)
  mosaic_batch_file_counter <- reactiveVal(0)
  mosaic_class_file_counter <- reactiveVal(0)


  output$download_mosaic_cluster_demo_csv <- downloadHandler(
  filename = function() { "mosaic_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/mosaic_demo.csv", file)
  }
  )
  


  output$download_mosaic_batch_demo_csv <- downloadHandler(
  filename = function() { "mosaic_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/mosaic_demo.csv", file)
  }
  )


  output$download_mosaic_class_demo_csv <- downloadHandler(
  filename = function() { "mosaic_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/mosaic_demo.csv", file)
  }
  )
  observeEvent(input$mosaic_ds_dr_data_type,ignoreInit = FALSE, {
      dt <- input$mosaic_ds_dr_data_type
      available_datasets <- mosaic_data_map[[dt]]$datasets
      available_methods <- mosaic_data_map[[dt]]$methods
      updateSelectizeInput(session, "mosaic_ds_dr_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "mosaic_ds_dr_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_ds_dr_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)                          
      updateSelectizeInput(session, "mosaic_ds_dr_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)


      mosaic_clustering_file_counter(mosaic_clustering_file_counter() + 1)
      output$mosaic_ds_clustering_file_ui <- renderUI({
        fileInput(paste0("mosaic_ds_clustering_new_file_", mosaic_clustering_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$mosaic_ds_batch_data_type,ignoreInit = FALSE, {
      dt <- input$mosaic_ds_batch_data_type
      available_datasets <- mosaic_data_map[[dt]]$datasets
      available_methods <- mosaic_data_map[[dt]]$methods
      updateSelectizeInput(session, "mosaic_ds_batch_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "mosaic_ds_batch_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_ds_batch_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_ds_batch_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)

      mosaic_batch_file_counter(mosaic_batch_file_counter() + 1)
      output$mosaic_ds_batch_file_ui <- renderUI({
        fileInput(paste0("mosaic_ds_batch_new_file_", mosaic_batch_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})


  })

  observeEvent(input$mosaic_ds_class_data_type, {
      dt <- input$mosaic_ds_class_data_type
      available_datasets <- mosaic_data_map[[dt]]$datasets
      available_methods <- mosaic_data_map[[dt]]$methods
      updateSelectizeInput(session, "mosaic_ds_class_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "mosaic_ds_class_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_ds_class_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "mosaic_ds_class_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)

      mosaic_class_file_counter(mosaic_class_file_counter() + 1)
      output$mosaic_ds_class_file_ui <- renderUI({
        fileInput(paste0("mosaic_ds_class_new_file_", mosaic_class_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})                          
  })    
  


  observeEvent(input$mosaic_ds_dr_dataset_choice, {
    mosaic_clustering_file_counter(mosaic_clustering_file_counter() + 1)
    output$mosaic_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("mosaic_ds_clustering_new_file_", mosaic_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$mosaic_ds_batch_dataset_choice, {
    mosaic_batch_file_counter(mosaic_batch_file_counter() + 1)
    output$mosaic_ds_batch_file_ui <- renderUI({
      fileInput(paste0("mosaic_ds_batch_new_file_", mosaic_batch_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$mosaic_ds_class_dataset_choice, {
    mosaic_class_file_counter(mosaic_class_file_counter() + 1)
    output$mosaic_ds_class_file_ui <- renderUI({
      fileInput(paste0("mosaic_ds_class_new_file_", mosaic_class_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })
  #### DR and Clustering ####
  output$mosaic_ds_dr_plot <- renderPlot({
    req(length(input$mosaic_ds_dr_dataset_choice) > 0)
    req(length(input$mosaic_ds_dr_method_choice) >= 2)
    req(length(input$mosaic_ds_dr_metric_choice) > 0)
    current_input_id <- paste0("mosaic_ds_clustering_new_file_", mosaic_clustering_file_counter())
    dataset <- input$mosaic_ds_dr_dataset_choice
    metric <- input$mosaic_ds_dr_metric_choice
    method <- input$mosaic_ds_dr_method_choice
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_mosaic(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/mosaic integration", 
      task_category = "clustering",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_mosaic_ds_dr_plot <- downloadHandler(
    filename = function() {
      paste("mosaic_ds_dr_", paste(input$mosaic_ds_dr_dataset_choice, collapse = "_"), "_",
            paste(input$mosaic_ds_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("mosaic_ds_clustering_new_file_", mosaic_clustering_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- generate_individual_bubble_mosaic(
        ds = input$mosaic_ds_dr_dataset_choice,
        select_metric = input$mosaic_ds_dr_metric_choice,
        select_method = input$mosaic_ds_dr_method_choice,
        base_dir = "./result/scib_metric/mosaic integration",
        task_category = "clustering",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  #### Batch Correction ####
  output$mosaic_ds_batch_plot <- renderPlot({
    req(length(input$mosaic_ds_batch_dataset_choice) > 0)
    req(length(input$mosaic_ds_batch_method_choice) >= 2)
    req(length(input$mosaic_ds_batch_metric_choice) > 0)

    dataset <- input$mosaic_ds_batch_dataset_choice
    metric <- input$mosaic_ds_batch_metric_choice
    method <- input$mosaic_ds_batch_method_choice
    current_input_id <- paste0("mosaic_ds_batch_new_file_", mosaic_batch_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }


    p <- generate_individual_bubble_mosaic(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/mosaic integration",
      task_category = "batchcorrection",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_mosaic_ds_batch_plot <- downloadHandler(
    filename = function() {
      paste("mosaic_ds_batch_", paste(input$mosaic_ds_batch_dataset_choice, collapse = "_"), "_",
            paste(input$mosaic_ds_batch_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {

      current_input_id <- paste0("mosaic_ds_batch_new_file_", mosaic_batch_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }      
      p <- generate_individual_bubble_mosaic(
        ds = input$mosaic_ds_batch_dataset_choice,
        select_metric = input$mosaic_ds_batch_metric_choice,
        select_method = input$mosaic_ds_batch_method_choice,
        base_dir = "./result/scib_metric/mosaic integration",
        task_category = "batchcorrection",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  #### Classification ####
  output$mosaic_ds_class_plot <- renderPlot({
    req(length(input$mosaic_ds_class_dataset_choice) > 0)
    req(length(input$mosaic_ds_class_method_choice) >= 2)
    req(length(input$mosaic_ds_class_metric_choice) > 0)

    dataset <- input$mosaic_ds_class_dataset_choice
    metric <- input$mosaic_ds_class_metric_choice
    method <- input$mosaic_ds_class_method_choice

    current_input_id <- paste0("mosaic_ds_class_new_file_", mosaic_class_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_mosaic(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/mosaic integration",
      task_category = "classification",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_mosaic_ds_class_plot <- downloadHandler(
    filename = function() {
      paste("mosaic_ds_class_", paste(input$mosaic_ds_class_dataset_choice, collapse = "_"), "_",
            paste(input$mosaic_ds_class_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("mosaic_ds_class_new_file_", mosaic_class_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_mosaic(
        ds = input$mosaic_ds_class_dataset_choice,
        select_metric = input$mosaic_ds_class_metric_choice,
        select_method = input$mosaic_ds_class_method_choice,
        base_dir = "./result/scib_metric/mosaic integration",
        task_category = "classification",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## mosaic specific End ###################





  ################# Mosaic Imputation Summary #################



    imputation_data_map <- list(
    rna_citeseq_rna = list(
      datasets = c("D51_data1_impute_rna","D51_data2_impute_rna","D52_data1_impute_rna","D52_data2_impute_rna",
                  "D53_data1_impute_rna","D53_data2_impute_rna","D54_data1_impute_rna","D54_data2_impute_rna",
                  "D55_data1_impute_rna","D55_data2_impute_rna"),
      methods = c("StabMap", "moETM", "scMM")
    ),
    rna_citeseq_adt = list(
      datasets = c("D51_data1_impute_adt","D51_data2_impute_adt","D52_data1_impute_adt","D52_data2_impute_adt",
                  "D53_data1_impute_adt","D53_data2_impute_adt","D54_data1_impute_adt","D54_data2_impute_adt",
                  "D55_data1_impute_adt","D55_data2_impute_adt"),
      methods = c("sciPENN","StabMap","totalVI","moETM","scMM")
    ),
    multiome_rna = list(
      datasets = c("D56_data1_impute_rna","D56_data2_impute_rna","D57_data1_impute_rna","D57_data2_impute_rna"),
      methods = c("MultiVI","StabMap","UnitedNet","moETM","scMM")
    ),
    multiome_atac = list(
      datasets = c("D56_data1_impute_atac","D56_data2_impute_atac","D57_data1_impute_atac","D57_data2_impute_atac"),
      methods = c("MultiVI","UnitedNet","moETM","StabMap","scMM")
    )
  )



  observeEvent(input$imputation_clustering_data_type, {
    dt <- input$imputation_clustering_data_type
    available_datasets <- imputation_data_map[[dt]]$datasets
    available_methods <- imputation_data_map[[dt]]$methods
    
    updateSelectizeInput(session, "imputation_clustering_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_clustering_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_clustering_method_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_clustering_method_choice",
                        choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$imputation_classification_data_type, {
    dt <- input$imputation_classification_data_type
    available_datasets <- imputation_data_map[[dt]]$datasets
    available_methods <- imputation_data_map[[dt]]$methods

    updateSelectizeInput(session, "imputation_classification_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_classification_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_classification_method_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_classification_method_choice",
                        choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$imputation_structure_data_type, {
    dt <- input$imputation_structure_data_type
    available_datasets <- imputation_data_map[[dt]]$datasets
    available_methods <- imputation_data_map[[dt]]$methods
    updateSelectizeInput(session, "imputation_structure_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_structure_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_structure_method_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_structure_method_choice",
                        choices = available_methods, selected = character(0),server = TRUE)
  })





  output$imputation_clustering_summary_plot <- renderPlot({
    req(length(input$imputation_clustering_dataset_choice) > 0)
    req(length(input$imputation_clustering_method_choice) >= 2)
    req(length(input$imputation_clustering_metric_choice) > 0)

    p <- generate_summary_bubble_imputation(
      ds = input$imputation_clustering_dataset_choice,
      select_metric = input$imputation_clustering_metric_choice,
      select_method = input$imputation_clustering_method_choice,
      base_dir = "./result/scib_metric/imputation",
      task_category = "clustering"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_imputation_clustering_summary <- downloadHandler(
    filename = function() {
      paste("imputation_clustering_summary_", 
            paste(input$imputation_clustering_dataset_choice, collapse = "_"), "_",
            paste(input$imputation_clustering_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_imputation(
        ds = input$imputation_clustering_dataset_choice,
        select_metric = input$imputation_clustering_metric_choice,
        select_method = input$imputation_clustering_method_choice,
        base_dir = "./result/scib_metric/imputation",
        task_category = "clustering"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  # Classification
  output$imputation_classification_summary_plot <- renderPlot({
    req(length(input$imputation_classification_dataset_choice) > 0)
    req(length(input$imputation_classification_method_choice) >= 2)
    req(length(input$imputation_classification_metric_choice) > 0)

    p <- generate_summary_bubble_imputation(
      ds = input$imputation_classification_dataset_choice,
      select_metric = input$imputation_classification_metric_choice,
      select_method = input$imputation_classification_method_choice,
      base_dir = "./result/scib_metric/imputation",
      task_category = "classification"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_imputation_classification_summary <- downloadHandler(
    filename = function() {
      paste("imputation_classification_summary_", 
            paste(input$imputation_classification_dataset_choice, collapse = "_"), "_",
            paste(input$imputation_classification_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_imputation(
        ds = input$imputation_classification_dataset_choice,
        select_metric = input$imputation_classification_metric_choice,
        select_method = input$imputation_classification_method_choice,
        base_dir = "./result/scib_metric/imputation",
        task_category = "classification"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  # Structure
  output$imputation_structure_summary_plot <- renderPlot({
    req(length(input$imputation_structure_dataset_choice) > 0)
    req(length(input$imputation_structure_method_choice) >= 2)
    req(length(input$imputation_structure_metric_choice) > 0)

    p <- generate_summary_bubble_imputation(
      ds = input$imputation_structure_dataset_choice,
      select_metric = input$imputation_structure_metric_choice,
      select_method = input$imputation_structure_method_choice,
      base_dir = "./result/scib_metric/imputation",
      task_category = "structure"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_imputation_structure_summary <- downloadHandler(
    filename = function() {
      paste("imputation_structure_summary_", 
            paste(input$imputation_structure_dataset_choice, collapse = "_"), "_",
            paste(input$imputation_structure_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_imputation(
        ds = input$imputation_structure_dataset_choice,
        select_metric = input$imputation_structure_metric_choice,
        select_method = input$imputation_structure_method_choice,
        base_dir = "./result/scib_metric/imputation",
        task_category = "structure"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################# Mosaic Imputation Summary End #################


  ################# Mosaic Imputation Dataset-Specific #################


  output$imputation_ds_clustering_file_ui <- renderUI({
    fileInput("imputation_ds_clustering_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$imputation_ds_batch_file_ui <- renderUI({
    fileInput("imputation_ds_batch_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$imputation_ds_structure_file_ui <- renderUI({
    fileInput("imputation_ds_structure_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  imputation_clustering_file_counter <- reactiveVal(0)
  imputation_classification_file_counter <- reactiveVal(0)
  imputation_structure_file_counter <- reactiveVal(0)



  output$download_imputation_cluster_demo_csv <- downloadHandler(
  filename = function() { "imputation_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/imputation_demo.csv", file)
  }
  )
  


  output$download_imputation_classification_demo_csv <- downloadHandler(
  filename = function() { "imputation_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/imputation_demo.csv", file)
  }
  )


  output$download_imputation_structure_demo_csv <- downloadHandler(
  filename = function() { "imputation_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/imputation_demo.csv", file)
  }
  )


  observeEvent(input$imputation_ds_clustering_data_type, {
    dt <- input$imputation_ds_clustering_data_type
    available_datasets <- imputation_data_map[[dt]]$datasets
    available_methods <- imputation_data_map[[dt]]$methods
    
    updateSelectizeInput(session, "imputation_ds_clustering_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_clustering_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_clustering_method_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_clustering_method_choice",
                        choices = available_methods, selected = character(0),server = TRUE)

    imputation_clustering_file_counter(imputation_clustering_file_counter() + 1)
    output$imputation_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("imputation_ds_clustering_new_file_", imputation_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$imputation_ds_classification_data_type, {
    dt <- input$imputation_ds_classification_data_type
    available_datasets <- imputation_data_map[[dt]]$datasets
    available_methods <- imputation_data_map[[dt]]$methods

    updateSelectizeInput(session, "imputation_ds_classification_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_classification_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_classification_method_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_classification_method_choice",
                        choices = available_methods, selected = character(0),server = TRUE)

    imputation_classification_file_counter(imputation_classification_file_counter() + 1)
    output$imputation_ds_batch_file_ui <- renderUI({
      fileInput(paste0("imputation_ds_batch_new_file_", imputation_classification_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$imputation_ds_structure_data_type, {
    dt <- input$imputation_ds_structure_data_type
    available_datasets <- imputation_data_map[[dt]]$datasets
    available_methods <- imputation_data_map[[dt]]$methods

    updateSelectizeInput(session, "imputation_ds_structure_dataset_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_structure_dataset_choice",
                        choices = available_datasets, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_structure_method_choice",
                        choices = NULL, selected = character(0),server = TRUE)
    updateSelectizeInput(session, "imputation_ds_structure_method_choice",
                        choices = available_methods, selected = character(0),server = TRUE)
                      
    imputation_structure_file_counter(imputation_structure_file_counter() + 1)
    output$imputation_ds_structure_file_ui <- renderUI({
      fileInput(paste0("imputation_ds_structure_new_file_", imputation_structure_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})  
  })



  observeEvent(input$imputation_ds_clustering_dataset_choice, {
    imputation_clustering_file_counter(imputation_clustering_file_counter() + 1)
    output$imputation_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("imputation_ds_clustering_new_file_", imputation_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$imputation_ds_classification_dataset_choice, {
    imputation_classification_file_counter(imputation_classification_file_counter() + 1)
    output$imputation_ds_classification_file_ui <- renderUI({
      fileInput(paste0("imputation_ds_classification_new_file_", imputation_classification_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$imputation_ds_structure_dataset_choice, {
    imputation_structure_file_counter(imputation_structure_file_counter() + 1)
    output$imputation_ds_structure_file_ui <- renderUI({
      fileInput(paste0("imputation_ds_structure_new_file_", imputation_structure_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })


  # Clustering
  output$imputation_ds_clustering_plot <- renderPlot({
    req(length(input$imputation_ds_clustering_dataset_choice) > 0)
    req(length(input$imputation_ds_clustering_method_choice) >= 2)
    req(length(input$imputation_ds_clustering_metric_choice) > 0)
    current_input_id <- paste0("imputation_ds_clustering_new_file_", imputation_clustering_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }


    p <- generate_individual_bubble_imputation(
      ds = input$imputation_ds_clustering_dataset_choice,
      select_metric = input$imputation_ds_clustering_metric_choice,
      select_method = input$imputation_ds_clustering_method_choice,
      base_dir = "./result/scib_metric/imputation",
      task_category = "clustering",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names      
    )
    p
  }, height = 540)

  output$download_imputation_ds_clustering_plot <- downloadHandler(
    filename = function() {
      paste("imputation_ds_clustering_", 
            paste(input$imputation_ds_clustering_dataset_choice, collapse = "_"), "_",
            paste(input$imputation_ds_clustering_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("imputation_ds_clustering_new_file_", imputation_clustering_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_imputation(
        ds = input$imputation_ds_clustering_dataset_choice,
        select_metric = input$imputation_ds_clustering_metric_choice,
        select_method = input$imputation_ds_clustering_method_choice,
        base_dir = "./result/scib_metric/imputation",
        task_category = "clustering",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  # Classification
  output$imputation_ds_classification_plot <- renderPlot({
    req(length(input$imputation_ds_classification_dataset_choice) > 0)
    req(length(input$imputation_ds_classification_method_choice) >= 2)
    req(length(input$imputation_ds_classification_metric_choice) > 0)

    current_input_id <- paste0("imputation_ds_classification_new_file_", imputation_classification_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_imputation(
      ds = input$imputation_ds_classification_dataset_choice,
      select_metric = input$imputation_ds_classification_metric_choice,
      select_method = input$imputation_ds_classification_method_choice,
      base_dir = "./result/scib_metric/imputation",
      task_category = "classification",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 540)

  output$download_imputation_ds_classification_plot <- downloadHandler(
    filename = function() {
      paste("imputation_ds_classification_", 
            paste(input$imputation_ds_classification_dataset_choice, collapse = "_"), "_",
            paste(input$imputation_ds_classification_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {


      current_input_id <- paste0("imputation_ds_classification_new_file_", imputation_classification_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }   

      p <- generate_individual_bubble_imputation(
        ds = input$imputation_ds_classification_dataset_choice,
        select_metric = input$imputation_ds_classification_metric_choice,
        select_method = input$imputation_ds_classification_method_choice,
        base_dir = "./result/scib_metric/imputation",
        task_category = "classification",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  # Structure
  output$imputation_ds_structure_plot <- renderPlot({
    req(length(input$imputation_ds_structure_dataset_choice) > 0)
    req(length(input$imputation_ds_structure_method_choice) >= 2)
    req(length(input$imputation_ds_structure_metric_choice) > 0)


    current_input_id <- paste0("imputation_ds_structure_new_file_", imputation_structure_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_imputation(
      ds = input$imputation_ds_structure_dataset_choice,
      select_metric = input$imputation_ds_structure_metric_choice,
      select_method = input$imputation_ds_structure_method_choice,
      base_dir = "./result/scib_metric/imputation",
      task_category = "structure",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 540)

  output$download_imputation_ds_structure_plot <- downloadHandler(
    filename = function() {
      paste("imputation_ds_structure_", 
            paste(input$imputation_ds_structure_dataset_choice, collapse = "_"), "_",
            paste(input$imputation_ds_structure_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("imputation_ds_structure_new_file_", imputation_structure_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_imputation(
        ds = input$imputation_ds_structure_dataset_choice,
        select_metric = input$imputation_ds_structure_metric_choice,
        select_method = input$imputation_ds_structure_method_choice,
        base_dir = "./result/scib_metric/imputation",
        task_category = "structure",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  ################# Mosaic Imputation Dataset-Specific End #################

















  ################## cross integration summary ###################

  cross_data_map <- list(
  multi_rna_adt = list(
      datasets = c("D51", "D52", "D53", "D54", "D55", "SD15", "SD16"),
      methods = c("totalVI", "Multigrate", "StabMap", "sciPENN", "scMDC", "UINMF", "scMoMaT", "MOFA2", "scMM", "Concerto")
    ),
  multi_rna_atac = list(
      datasets = c("D56", "D57", "SD17", "SD18"),
      methods = c("scMDC","Multigrate","UnitedNet","StabMap", "scMoMaT", "MOFA2" , "UINMF", "scMM") 
    ),

  multi_adt_atac = list(
      datasets = c("D58","SD19", "SD20"),
      methods = c("Multigrate", "scMoMaT" , "StabMap", "MOFA2", "UINMF") 
    ),

  multi_rna_adt_atac = list(
      datasets = c("D59","SD21", "SD22"),
      methods = c( "Multigrate", "scMoMaT" ,  "MOFA2", "UINMF", "StabMap") 
    )  
  )
  
  observeEvent(input$cross_dr_data_type, ignoreInit = FALSE, {
      dt <- input$cross_dr_data_type
      available_datasets <- cross_data_map[[dt]]$datasets
      available_methods <- cross_data_map[[dt]]$methods
      updateSelectizeInput(session, "cross_dr_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "cross_dr_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_dr_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_dr_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$cross_batch_data_type,ignoreInit = FALSE, {
      dt <- input$cross_batch_data_type
      available_datasets <- cross_data_map[[dt]]$datasets
      available_methods <- cross_data_map[[dt]]$methods
      updateSelectizeInput(session, "cross_batch_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "cross_batch_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_batch_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_batch_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })

  observeEvent(input$cross_class_data_type,ignoreInit = FALSE, {
      dt <- input$cross_class_data_type
      available_datasets <- cross_data_map[[dt]]$datasets
      available_methods <- cross_data_map[[dt]]$methods
      updateSelectizeInput(session, "cross_class_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "cross_class_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_class_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_class_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)
  })


  output$cross_dr_summary_plot <- renderPlot({
    req(length(input$cross_dr_dataset_choice) > 0)
    req(length(input$cross_dr_method_choice) >= 2)
    req(length(input$cross_dr_metric_choice) > 0)

    dataset <- input$cross_dr_dataset_choice
    metric <- input$cross_dr_metric_choice
    method <- input$cross_dr_method_choice

    # 使用task_category = "clustering"
    p <- generate_summary_bubble_cross(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/cross integration",
      task_category = "clustering"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_cross_dr_summary <- downloadHandler(
    filename = function() {
      paste("cross_dr_summary_", paste(input$cross_dr_dataset_choice, collapse = "_"), "_",
            paste(input$cross_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_cross(
        ds = input$cross_dr_dataset_choice,
        select_metric = input$cross_dr_metric_choice,
        select_method = input$cross_dr_method_choice,
        base_dir = "./result/scib_metric/cross integration",
        task_category = "clustering"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )  


  output$cross_batch_summary_plot <- renderPlot({
    req(length(input$cross_batch_dataset_choice) > 0)
    req(length(input$cross_batch_method_choice) >= 2)
    req(length(input$cross_batch_metric_choice) > 0)

    dataset <- input$cross_batch_dataset_choice
    metric <- input$cross_batch_metric_choice
    method <- input$cross_batch_method_choice

    # 使用task_category = "batchcorrection"
    p <- generate_summary_bubble_cross(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/cross integration",
      task_category = "batchcorrection"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_cross_batch_summary <- downloadHandler(
    filename = function() {
      paste("cross_batch_summary_", paste(input$cross_batch_dataset_choice, collapse = "_"), "_",
            paste(input$cross_batch_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_cross(
        ds = input$cross_batch_dataset_choice,
        select_metric = input$cross_batch_metric_choice,
        select_method = input$cross_batch_method_choice,
        base_dir = "./result/scib_metric/cross integration",
        task_category = "batchcorrection"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )


  output$cross_class_summary_plot <- renderPlot({
    req(length(input$cross_class_dataset_choice) > 0)
    req(length(input$cross_class_method_choice) >= 2)
    req(length(input$cross_class_metric_choice) > 0)

    dataset <- input$cross_class_dataset_choice
    metric <- input$cross_class_metric_choice
    method <- input$cross_class_method_choice

    # 使用task_category = "classification"
    p <- generate_summary_bubble_cross(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/cross integration",
      task_category = "classification"
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_cross_class_summary <- downloadHandler(
    filename = function() {
      paste("cross_class_summary_", paste(input$cross_class_dataset_choice, collapse = "_"), "_",
            paste(input$cross_class_metric_choice, collapse = "_"), ".pdf", sep = "")
    },
    content = function(file) {
      p <- generate_summary_bubble_cross(
        ds = input$cross_class_dataset_choice,
        select_metric = input$cross_class_metric_choice,
        select_method = input$cross_class_method_choice,
        base_dir = "./result/scib_metric/cross integration",
        task_category = "classification"
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )
  ################## cross integration summary End ###################


  #################################
  ### cross Dataset-Specific ###
  #################################

  output$cross_ds_clustering_file_ui <- renderUI({
    fileInput("cross_ds_clustering_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$cross_ds_batch_file_ui <- renderUI({
    fileInput("cross_ds_batch_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$cross_ds_class_file_ui <- renderUI({
    fileInput("cross_ds_class_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })


  cross_clustering_file_counter <- reactiveVal(0)
  cross_batch_file_counter <- reactiveVal(0)
  cross_class_file_counter <- reactiveVal(0)


  output$download_cross_cluster_demo_csv <- downloadHandler(
  filename = function() { "cross_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/cross_demo.csv", file)
  }
  )
  


  output$download_cross_batch_demo_csv <- downloadHandler(
  filename = function() { "cross_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/cross_demo.csv", file)
  }
  )


  output$download_cross_class_demo_csv <- downloadHandler(
  filename = function() { "cross_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/cross_demo.csv", file)
  }
  )


  observeEvent(input$cross_ds_dr_data_type,ignoreInit = FALSE, {
      dt <- input$cross_ds_dr_data_type
      available_datasets <- cross_data_map[[dt]]$datasets
      available_methods <- cross_data_map[[dt]]$methods
      updateSelectizeInput(session, "cross_ds_dr_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "cross_ds_dr_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_ds_dr_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_ds_dr_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)

      cross_clustering_file_counter(cross_clustering_file_counter() + 1)
      output$cross_ds_clustering_file_ui <- renderUI({
        fileInput(paste0("cross_ds_clustering_new_file_", cross_clustering_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$cross_ds_batch_data_type,ignoreInit = FALSE, {
      dt <- input$cross_ds_batch_data_type
      available_datasets <- cross_data_map[[dt]]$datasets
      available_methods <- cross_data_map[[dt]]$methods
      updateSelectizeInput(session, "cross_ds_batch_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "cross_ds_batch_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_ds_batch_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_ds_batch_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)

      cross_batch_file_counter(cross_batch_file_counter() + 1)
      output$cross_ds_batch_file_ui <- renderUI({
        fileInput(paste0("cross_ds_batch_new_file_", cross_batch_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$cross_ds_class_data_type, {
      dt <- input$cross_ds_class_data_type
      available_datasets <- cross_data_map[[dt]]$datasets
      available_methods <- cross_data_map[[dt]]$methods
      updateSelectizeInput(session, "cross_ds_class_dataset_choice",
                          choices = NULL, selected = character(0),server = TRUE)      
      updateSelectizeInput(session, "cross_ds_class_dataset_choice",
                          choices = available_datasets, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_ds_class_method_choice",
                          choices = NULL, selected = character(0),server = TRUE)
      updateSelectizeInput(session, "cross_ds_class_method_choice",
                          choices = available_methods, selected = character(0),server = TRUE)

      cross_class_file_counter(cross_class_file_counter() + 1)
      output$cross_ds_class_file_ui <- renderUI({
        fileInput(paste0("cross_ds_class_new_file_", cross_class_file_counter()),
                  "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })    
  
  observeEvent(input$cross_ds_dr_dataset_choice, {
    cross_clustering_file_counter(cross_clustering_file_counter() + 1)
    output$cross_ds_clustering_file_ui <- renderUI({
      fileInput(paste0("cross_ds_clustering_new_file_", cross_clustering_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$cross_ds_batch_dataset_choice, {
    cross_batch_file_counter(cross_batch_file_counter() + 1)
    output$cross_ds_batch_file_ui <- renderUI({
      fileInput(paste0("cross_ds_batch_new_file_", cross_batch_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  observeEvent(input$cross_ds_class_dataset_choice, {
    cross_class_file_counter(cross_class_file_counter() + 1)
    output$cross_ds_class_file_ui <- renderUI({
      fileInput(paste0("cross_ds_class_new_file_", cross_class_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
    })
  
  })

  #### DR and Clustering ####
  output$cross_ds_dr_plot <- renderPlot({
    req(length(input$cross_ds_dr_dataset_choice) > 0)
    req(length(input$cross_ds_dr_method_choice) >= 2)
    req(length(input$cross_ds_dr_metric_choice) > 0)


    current_input_id <- paste0("cross_ds_clustering_new_file_", cross_clustering_file_counter())
    dataset <- input$cross_ds_dr_dataset_choice
    metric <- input$cross_ds_dr_metric_choice
    method <- input$cross_ds_dr_method_choice


    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    # 调用cross特定的函数
    p <- generate_individual_bubble_cross(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/cross integration", 
      task_category = "clustering"  ,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_cross_ds_dr_plot <- downloadHandler(
    filename = function() {
      paste("cross_ds_dr_", paste(input$cross_ds_dr_dataset_choice, collapse = "_"), "_",
            paste(input$cross_ds_dr_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {

      current_input_id <- paste0("cross_ds_clustering_new_file_", cross_clustering_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_cross(
        ds = input$cross_ds_dr_dataset_choice,
        select_metric = input$cross_ds_dr_metric_choice,
        select_method = input$cross_ds_dr_method_choice,
        base_dir = "./result/scib_metric/cross integration",
        task_category = "clustering",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  #### Batch Correction ####
  output$cross_ds_batch_plot <- renderPlot({
    req(length(input$cross_ds_batch_dataset_choice) > 0)
    req(length(input$cross_ds_batch_method_choice) >= 2)
    req(length(input$cross_ds_batch_metric_choice) > 0)

    dataset <- input$cross_ds_batch_dataset_choice
    metric <- input$cross_ds_batch_metric_choice
    method <- input$cross_ds_batch_method_choice
    current_input_id <- paste0("cross_ds_batch_new_file_", cross_batch_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_cross(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/cross integration",
      task_category = "batchcorrection",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_cross_ds_batch_plot <- downloadHandler(
    filename = function() {
      paste("cross_ds_batch_", paste(input$cross_ds_batch_dataset_choice, collapse = "_"), "_",
            paste(input$cross_ds_batch_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {

      current_input_id <- paste0("cross_ds_batch_new_file_", cross_batch_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_individual_bubble_cross(
        ds = input$cross_ds_batch_dataset_choice,
        select_metric = input$cross_ds_batch_metric_choice,
        select_method = input$cross_ds_batch_method_choice,
        base_dir = "./result/scib_metric/cross integration",
        task_category = "batchcorrection",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )

  #### Classification ####
  output$cross_ds_class_plot <- renderPlot({
    req(length(input$cross_ds_class_dataset_choice) > 0)
    req(length(input$cross_ds_class_method_choice) >= 2)
    req(length(input$cross_ds_class_metric_choice) > 0)

    dataset <- input$cross_ds_class_dataset_choice
    metric <- input$cross_ds_class_metric_choice
    method <- input$cross_ds_class_method_choice

    current_input_id <- paste0("cross_ds_class_new_file_", cross_class_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if(!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_individual_bubble_cross(
      ds = dataset,
      select_metric = metric,
      select_method = method,
      base_dir = "./result/scib_metric/cross integration",
      task_category = "classification",
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )

    p <- p + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
    p
  }, height = 540)

  output$download_cross_ds_class_plot <- downloadHandler(
    filename = function() {
      paste("cross_ds_class_", paste(input$cross_ds_class_dataset_choice, collapse = "_"), "_",
            paste(input$cross_ds_class_metric_choice, collapse = "_"), ".pdf",
            sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("cross_ds_class_new_file_", cross_class_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if(!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- generate_individual_bubble_cross(
        ds = input$cross_ds_class_dataset_choice,
        select_metric = input$cross_ds_class_metric_choice,
        select_method = input$cross_ds_class_method_choice,
        base_dir = "./result/scib_metric/cross integration",
        task_category = "classification",
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 12, height = 16)
    }
  )


  ################## Time memory vertical #######################

  output$vertical_time_file_ui <- renderUI({
    fileInput("vertical_time_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$vertical_memory_file_ui <- renderUI({
    fileInput("vertical_memory_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  vertical_time_file_counter <- reactiveVal(0)
  vertical_memory_file_counter <- reactiveVal(0)

  output$download_vertical_time_demo_csv <- downloadHandler(
  filename = function() { "vertical_time_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )
  


  output$download_vertical_memory_demo_csv <- downloadHandler(
  filename = function() { "vertical_memory_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )

  vertical_tm_method_map <- list(
    rna_adt = c("Concerto", "Matilda","moETM", "MOFA2","Multigrate","sciPENN","scMDC","scMM","scMoMaT","scMSI","Seurat.WNN","totalVI","UINMF","VIMCCA"),
    rna_atac = c("iPOLNG", "Matilda", "MIRA", "moETM", "MOFA2", "Multigrate", 
                "scMDC", "scMM", "scMoMaT", "Seurat.WNN", "UINMF", 
                "UnitedNet", "VIMCCA"),
    rna_adt_atac = c("Matilda", "MOFA2", "Multigrate", "scMoMaT", "UINMF")
  )

  observeEvent(input$vertical_time_data_type, {
    dt <- input$vertical_time_data_type
    available_methods <- vertical_tm_method_map[[dt]]
    
    updateSelectizeInput(session, "vertical_time_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(session, "vertical_time_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )

    vertical_time_file_counter(vertical_time_file_counter() + 1)
    output$vertical_time_file_ui <- renderUI({
      fileInput(paste0("vertical_time_new_file", vertical_time_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})

  })

  observeEvent(input$vertical_memory_data_type, {
    dt <- input$vertical_memory_data_type
    available_methods <- vertical_tm_method_map[[dt]]

    updateSelectizeInput(session, "vertical_memory_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )    

    updateSelectizeInput(session, "vertical_memory_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )

    vertical_memory_file_counter(vertical_memory_file_counter() + 1)
    output$vertical_memory_file_ui <- renderUI({
      fileInput(paste0("vertical_memory_new_file", vertical_memory_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})

  })  

    # Vertical Time plot
  output$vertical_time_plot <- renderPlot({
    req(length(input$vertical_time_method_choice) > 0)
    req(input$vertical_time_data_type)
    current_input_id <- paste0("vertical_time_new_file", vertical_time_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    # 调用时间绘图函数
    p <- generate_time_plot_vertical(
      select_method = input$vertical_time_method_choice,
      data_type = input$vertical_time_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_vertical_time <- downloadHandler(
    filename = function() {
      paste("vertical_time_", input$vertical_time_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("vertical_time_new_file", vertical_time_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- generate_time_plot_vertical(
        select_method = input$vertical_time_method_choice,
        data_type = input$vertical_time_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 10, height = 6)
    }
  )
  
  # Vertical Memory plot
  output$vertical_memory_plot <- renderPlot({
    req(length(input$vertical_memory_method_choice) > 0)
    req(input$vertical_memory_data_type)
    current_input_id <- paste0("vertical_memory_new_file", vertical_memory_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    # 调用内存绘图函数
    p <- generate_memory_plot_vertical(
      select_method = input$vertical_memory_method_choice,
      data_type = input$vertical_memory_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_vertical_memory <- downloadHandler(
    filename = function() {
      paste("vertical_memory_", input$vertical_memory_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("vertical_memory_new_file", vertical_memory_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_memory_plot_vertical(
        select_method = input$vertical_memory_method_choice,
        data_type = input$vertical_memory_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 10, height = 6)
    }
  )

  ################## Time memory vertical End #######################




  ################## Time memory diagonal #######################

  output$diagonal_time_file_ui <- renderUI({
    fileInput("diagonal_time_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$diagonal_memory_file_ui <- renderUI({
    fileInput("diagonal_memory_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  diagonal_time_file_counter <- reactiveVal(0)
  diagonal_memory_file_counter <- reactiveVal(0)

  output$download_diagonal_time_demo_csv <- downloadHandler(
  filename = function() { "diagonal_time_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )

  output$download_diagonal_memory_demo_csv <- downloadHandler(
  filename = function() { "diagonal_memory_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )



  diagonal_tm_method_map <- list(
      rna_atac = c("Conos", "GLUE", "iNMF", "MultiMAP", "online.iNMF", "Portal", "SCALEX", "scBridge", "sciCAN", "scJoint", "Seurat.v3", "Seurat.v5", "uniPort", "VIPCCA"),
      mrna_matac = c("Conos","GLUE","iNMF","online.iNMF","scJoint") 
     
    )

  observeEvent(input$diagonal_time_data_type, {
    dt <- input$diagonal_time_data_type
    available_methods <- diagonal_tm_method_map[[dt]]
    
    updateSelectizeInput(session, "diagonal_time_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(session, "diagonal_time_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )


    diagonal_time_file_counter(diagonal_time_file_counter() + 1)
    output$diagonal_time_file_ui <- renderUI({
      fileInput(paste0("diagonal_time_new_file", diagonal_time_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$diagonal_memory_data_type, {
    dt <- input$diagonal_memory_data_type
    available_methods <- diagonal_tm_method_map[[dt]]

    updateSelectizeInput(session, "diagonal_memory_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )    

    updateSelectizeInput(session, "diagonal_memory_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )


    diagonal_memory_file_counter(diagonal_memory_file_counter() + 1)
    output$diagonal_memory_file_ui <- renderUI({
      fileInput(paste0("diagonal_memory_new_file", diagonal_memory_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })  

  # Diagonal Time Plot
  output$diagonal_time_plot <- renderPlot({
    req(length(input$diagonal_time_method_choice) >= 1)
    req(input$diagonal_time_data_type)
    current_input_id <- paste0("diagonal_time_new_file", diagonal_time_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }    
    p <- generate_time_plot_diagonal(
      select_method = input$diagonal_time_method_choice,
      data_type = input$diagonal_time_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_diagonal_time_plot <- downloadHandler(
    filename = function() {
      paste("diagonal_time_plot_", paste(input$diagonal_time_method_choice, collapse = "_"), "_",
            input$diagonal_time_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("diagonal_time_new_file", diagonal_time_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_time_plot_diagonal(
        select_method = input$diagonal_time_method_choice,
        data_type = input$diagonal_time_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 8, height = 6)
    }
  )
  
  # Diagonal Memory Plot
  output$diagonal_memory_plot <- renderPlot({
    req(length(input$diagonal_memory_method_choice) >= 1)
    req(input$diagonal_memory_data_type)
    current_input_id <- paste0("diagonal_memory_new_file", diagonal_memory_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }    
    # 调用你提供的diagonal的memory函数
    p <- generate_memory_plot_diagonal(
      select_method = input$diagonal_memory_method_choice,
      data_type = input$diagonal_memory_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_diagonal_memory_plot <- downloadHandler(
    filename = function() {
      paste("diagonal_memory_plot_", paste(input$diagonal_memory_method_choice, collapse = "_"), "_",
            input$diagonal_memory_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("diagonal_memory_new_file", diagonal_memory_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_memory_plot_diagonal(
        select_method = input$diagonal_memory_method_choice,
        data_type = input$diagonal_memory_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 8, height = 6)
    }
  )

  ################## Time memory diagonal End #######################

    ################## Time memory mosaic #######################
  output$mosaic_time_file_ui <- renderUI({
    fileInput("mosaic_time_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$mosaic_memory_file_ui <- renderUI({
    fileInput("mosaic_memory_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  mosaic_time_file_counter <- reactiveVal(0)
  mosaic_memory_file_counter <- reactiveVal(0)

  output$download_mosaic_time_demo_csv <- downloadHandler(
  filename = function() { "mosaic_time_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )
  


  output$download_mosaic_memory_demo_csv <- downloadHandler(
  filename = function() { "mosaic_memory_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )


  mosaic_tm_method_map <- list(
  rna_adt =c("StabMap", "scMoMaT", "Multigrate"),
  rna_atac =c("MultiVI","Cobolt","scMoMaT","Multigrate","StabMap", "SMILE") ,
  fle_w_sha = c("StabMap", "UINMF", "Multigrate", "scMoMaT"),
  fle_wo_sha = c("StabMap", "Multigrate", "scMoMaT") 
    )  


  observeEvent(input$mosaic_time_data_type, {
    dt <- input$mosaic_time_data_type
    available_methods <- mosaic_tm_method_map[[dt]]
    
    updateSelectizeInput(session, "mosaic_time_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(session, "mosaic_time_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )

    mosaic_time_file_counter(mosaic_time_file_counter() + 1)
    output$mosaic_time_file_ui <- renderUI({
      fileInput(paste0("mosaic_time_new_file", mosaic_time_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$mosaic_memory_data_type, {
    dt <- input$mosaic_memory_data_type
    available_methods <- mosaic_tm_method_map[[dt]]

    updateSelectizeInput(session, "mosaic_memory_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )    

    updateSelectizeInput(session, "mosaic_memory_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )


    mosaic_memory_file_counter(mosaic_memory_file_counter() + 1)
    output$mosaic_memory_file_ui <- renderUI({
      fileInput(paste0("mosaic_memory_new_file", mosaic_memory_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })  

  # mosaic Time Plot
  output$mosaic_time_plot <- renderPlot({
    req(length(input$mosaic_time_method_choice) >= 1)
    req(input$mosaic_time_data_type)
    current_input_id <- paste0("mosaic_time_new_file", mosaic_time_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    p <- generate_time_plot_mosaic(
      select_method = input$mosaic_time_method_choice,
      data_type = input$mosaic_time_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_mosaic_time_plot <- downloadHandler(
    filename = function() {
      paste("mosaic_time_plot_", paste(input$mosaic_time_method_choice, collapse = "_"), "_",
            input$mosaic_time_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("mosaic_time_new_file", mosaic_time_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- generate_time_plot_mosaic(
        select_method = input$mosaic_time_method_choice,
        data_type = input$mosaic_time_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 8, height = 6)
    }
  )
  
  # mosaic Memory Plot
  output$mosaic_memory_plot <- renderPlot({
    req(length(input$mosaic_memory_method_choice) >= 1)
    req(input$mosaic_memory_data_type)
    current_input_id <- paste0("mosaic_memory_new_file", mosaic_memory_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }

    # 调用你提供的mosaic的memory函数
    p <- generate_memory_plot_mosaic(
      select_method = input$mosaic_memory_method_choice,
      data_type = input$mosaic_memory_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_mosaic_memory_plot <- downloadHandler(
    filename = function() {
      paste("mosaic_memory_plot_", paste(input$mosaic_memory_method_choice, collapse = "_"), "_",
            input$mosaic_memory_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("mosaic_memory_new_file", mosaic_memory_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_memory_plot_mosaic(
        select_method = input$mosaic_memory_method_choice,
        data_type = input$mosaic_memory_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 8, height = 6)
    }
  )

  ################## Time memory mosaic End #######################


  ################## Time memory cross #######################

  output$cross_time_file_ui <- renderUI({
    fileInput("cross_time_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  output$cross_memory_file_ui <- renderUI({
    fileInput("cross_memory_new_file", "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)
  })

  cross_time_file_counter <- reactiveVal(0)
  cross_memory_file_counter <- reactiveVal(0)

  output$download_cross_time_demo_csv <- downloadHandler(
  filename = function() { "cross_time_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )
  


  output$download_cross_memory_demo_csv <- downloadHandler(
  filename = function() { "cross_memory_demo.csv" },
  content = function(file) {
    file.copy("./result/demo_data/time_memory_demo.csv", file)
  }
  )
  cross_tm_method_map <- list(
  rna_adt =c("totalVI", "Multigrate", "StabMap", "sciPENN", "scMDC", "UINMF", "scMoMaT", "MOFA2", "scMM", "Concerto"),
  rna_atac =c("scMDC","Multigrate","UnitedNet","StabMap", "scMoMaT", "MOFA2" , "UINMF", "scMM"),
  adt_atac = c("Multigrate", "scMoMaT" , "StabMap", "MOFA2", "UINMF"),
  rna_adt_atac = c("Multigrate", "scMoMaT" , "StabMap", "MOFA2", "UINMF") ,
  spatial = c("GPSA", "PASTE_center","PASTE_pairwise", "SPIRAL") 
    )  


  observeEvent(input$cross_time_data_type, {
    dt <- input$cross_time_data_type
    available_methods <- cross_tm_method_map[[dt]]
    
    updateSelectizeInput(session, "cross_time_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(session, "cross_time_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )

    cross_time_file_counter(cross_time_file_counter() + 1)
    output$cross_time_file_ui <- renderUI({
      fileInput(paste0("cross_time_new_file", cross_time_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })

  observeEvent(input$cross_memory_data_type, {
    dt <- input$cross_memory_data_type
    available_methods <- cross_tm_method_map[[dt]]

    updateSelectizeInput(session, "cross_memory_method_choice",
      choices = NULL,
      selected = character(0),
      server = TRUE
    )    

    updateSelectizeInput(session, "cross_memory_method_choice",
      choices = available_methods,
      selected = character(0),
      server = TRUE
    )

    cross_memory_file_counter(cross_memory_file_counter() + 1)
    output$cross_memory_file_ui <- renderUI({
      fileInput(paste0("cross_memory_new_file", cross_memory_file_counter()),
                "Upload New Method(s) CSV (Optional):", accept = ".csv", multiple = TRUE)})
  })  

  # cross Time Plot
  output$cross_time_plot <- renderPlot({
    req(length(input$cross_time_method_choice) >= 1)
    req(input$cross_time_data_type)
    current_input_id <- paste0("cross_time_new_file", cross_time_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }    
    p <- generate_time_plot_cross(
      select_method = input$cross_time_method_choice,
      data_type = input$cross_time_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_cross_time_plot <- downloadHandler(
    filename = function() {
      paste("cross_time_plot_", paste(input$cross_time_method_choice, collapse = "_"), "_",
            input$cross_time_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("cross_time_new_file", cross_time_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }

      p <- generate_time_plot_cross(
        select_method = input$cross_time_method_choice,
        data_type = input$cross_time_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 8, height = 6)
    }
  )
  
  # cross Memory Plot
  output$cross_memory_plot <- renderPlot({
    req(length(input$cross_memory_method_choice) >= 1)
    req(input$cross_memory_data_type)
    current_input_id <- paste0("cross_memory_new_file", cross_memory_file_counter())
    new_file_paths <- NA
    new_file_names <- NA
    if (!is.null(input[[current_input_id]])){
      new_file_paths <- input[[current_input_id]]$datapath
      new_file_names <- input[[current_input_id]]$name
    }
    
    # 调用你提供的cross的memory函数
    p <- generate_memory_plot_cross(
      select_method = input$cross_memory_method_choice,
      data_type = input$cross_memory_data_type,
      new_file_paths = new_file_paths,
      new_file_names = new_file_names
    )
    p
  }, height = 430)
  
  output$download_cross_memory_plot <- downloadHandler(
    filename = function() {
      paste("cross_memory_plot_", paste(input$cross_memory_method_choice, collapse = "_"), "_",
            input$cross_memory_data_type, ".pdf", sep = "")
    },
    content = function(file) {
      current_input_id <- paste0("cross_memory_new_file", cross_memory_file_counter())
      new_file_paths <- NA
      new_file_names <- NA
      if (!is.null(input[[current_input_id]])){
        new_file_paths <- input[[current_input_id]]$datapath
        new_file_names <- input[[current_input_id]]$name
      }
      p <- generate_memory_plot_cross(
        select_method = input$cross_memory_method_choice,
        data_type = input$cross_memory_data_type,
        new_file_paths = new_file_paths,
        new_file_names = new_file_names
      )
      ggsave(file, plot = p, width = 8, height = 6)
    }
  )

  ################## Time memory cross End #######################


}