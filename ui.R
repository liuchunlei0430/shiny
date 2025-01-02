library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)

ui <- dashboardPage(
  title = "MultiBench",
  dashboardHeader(
    title = tags$div(
      tags$span("MultiBench", style = "font-size:20px; font-weight:bold;"),
      style = "display:inline-block; vertical-align:middle;"
    ),
    tags$li(
      class = "dropdown",
      tags$img(
        src = "plots/logo_cmri_usyd.png",
        style = "height:30px; margin-top:10px; margin-right:10px; display:inline-block;"
      )
    )
  ),
  skin = "black",
  dashboardSidebar(
    sidebarMenu(
      id = 'tab',
      menuItem("Introduction", tabName = "introduction",
               menuSubItem("Background", tabName = "background"),
               menuSubItem("Data Source", tabName = "source"),
               menuSubItem("Dataset", tabName = "dataset"),
               menuSubItem("Method", tabName = "method"),
               menuSubItem("Evaluation/Criteria", tabName = "evaluation")
      ),
      menuItem("Benchmarking", tabName = "benchmarks",
              menuSubItem("Summary Performance", tabName = "summary"),
              menuSubItem("Dataset-Specific Performance", tabName = "scib"),  
              menuSubItem("Time&Memory", tabName = "timememory")
      ),
      menuItem("Submit & Compare", tabName = "compare",
               menuSubItem("Dataset-specific Performance", tabName = "dataset_compare"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(HTML("
        function toggleSection(header) {
          var content = header.nextElementSibling;
          var icon = header.querySelector('.toggle-icon');
          
          if (content.style.display === 'none') {
            content.style.display = 'block';
            icon.textContent = '▼';
          } else {
            content.style.display = 'none';
            icon.textContent = '▶';
          }
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "introduction",
              h2("Introduction"),
              p("Introduction Part")
      ),
      tabItem(tabName = "background",
        imageOutput("background_image", height = "990px")
      ),
      tabItem(tabName = "source",
        fluidRow(
          column(
            width = 4,
            valueBoxOutput("total_datasource")
          )
        ),
        fluidRow(
          column(
            width = 4,
            box(
              title = "Protocol Distribution",
              status = "primary",
              solidHeader = TRUE,
              style = "height: 420px;",  # 父容器固定一点高度
              width = 12,
              # Plotly输出，填满父容器
              plotlyOutput("protocol_plot", height = "80%", width = "100%"),
              
              # 把按钮稍微往下移一点，比如加 margin-top
              div(style = "margin-top: 10px;",
                  actionButton("zoom_plot", "View Full Size")
              )
            )
          ),
          column(
            width = 8,
            box(
              title = "Data Table", status = "primary", solidHeader = TRUE,
              width = 12,
              style = "overflow-x: auto;",
              fluidRow(
                column(6,
                       selectInput("modality_filter", "Select Modality:",
                                   choices = c("All"))
                ),
                column(6,
                       textInput("search_text", "Search:", "")
                )
              ),
              DTOutput("data_source")
            )
          )
        )
      ),
      tabItem(tabName = "dataset",
        fluidRow(
          column(12,
            box(
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              title = "Dataset Overview",
              
              # 添加标签页
              tabsetPanel(
                id = "dataset_integration_type",
                
                # 垂直整合标签页
                tabPanel("Vertical Integration",
                  fluidRow(
                    column(12,
                      wellPanel(
                        fluidRow(
                          column(2, selectInput("vertical_data_source", "Data Source:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("vertical_protocol", "Protocol:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("vertical_modality", "Modality:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("vertical_species", "Species:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("vertical_tissue", "Tissue:", choices = c("All" = ""), selected = ""))
                        )
                      ),
                      DTOutput("vertical_dataset_table")
                    )
                  )
                ),
                
                # 对角整合标签页
                tabPanel("Diagonal Integration",
                  fluidRow(
                    column(12,
                      wellPanel(
                        fluidRow(
                          column(2, selectInput("diagonal_data_source", "Data Source:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("diagonal_protocol", "Protocol:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("diagonal_modality", "Modality:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("diagonal_species", "Species:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("diagonal_tissue", "Tissue:", choices = c("All" = ""), selected = ""))
                        )
                      ),
                      DTOutput("diagonal_dataset_table")
                    )
                  )
                ),
                
                # 马赛克整合标签页
                tabPanel("Mosaic Integration",
                  fluidRow(
                    column(12,
                      wellPanel(
                        fluidRow(
                          column(2, selectInput("mosaic_data_source", "Data Source:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("mosaic_protocol", "Protocol:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("mosaic_modality", "Modality:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("mosaic_species", "Species:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("mosaic_tissue", "Tissue:", choices = c("All" = ""), selected = ""))
                        )
                      ),
                      DTOutput("mosaic_dataset_table")
                    )
                  )
                ),
                
                # 交叉整合标签页
                tabPanel("Cross Integration",
                  fluidRow(
                    column(12,
                      wellPanel(
                        fluidRow(
                          column(2, selectInput("cross_data_source", "Data Source:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("cross_protocol", "Protocol:", choices = c("All" = ""), selected = "")),
                          column(2, selectInput("cross_modality", "Modality:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("cross_species", "Species:", choices = c("All" = ""), selected = "")),
                          column(3, selectInput("cross_tissue", "Tissue:", choices = c("All" = ""), selected = ""))
                        )
                      ),
                      DTOutput("cross_dataset_table")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(tabName = "method",
            fluidRow(
            column(12,
                box(
                title = "Filters", status = "primary", solidHeader = TRUE,
                width = 12,
                fluidRow(
                    column(2, selectInput("peak_gene", "Peak/Gene Activity", 
                                        choices = NULL)),
                    column(2, selectInput("output_type", "Output Type", 
                                        choices = NULL)),
                    column(2, selectInput("prog_lang", "Programming Language", 
                                        choices = NULL)),
                    column(2, selectInput("deep_learning", "Deep Learning", 
                                        choices = NULL)),
                    column(2, selectInput("cell_type", "Cell Type Required", 
                                        choices = NULL))
                ),
                fluidRow(
                    column(4, selectizeInput("data_structure", "Data Structure", 
                                        choices = NULL, multiple = TRUE)),
                    column(4, selectizeInput("integration_cat", "Integration Categories", 
                                        choices = NULL, multiple = TRUE)),
                    column(4, selectizeInput("task_cat", "Task Categories", 
                                        choices = NULL, multiple = TRUE))
                )
                )
            )
            ),
            fluidRow(
            column(12,
                box(
                title = "Methods Table", status = "primary", solidHeader = TRUE,
                width = 12,
                style = "overflow-x: auto;", 
                DTOutput("method_table")
                )
            )
            )
      ),
      tabItem(tabName = "evaluation",
        fluidRow(
          column(12,
            box(
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              title = "Metric Overview",
              tabsetPanel(
                id = "metric_tabs",
                # 注意：确保这些值与数据中的 Task 列完全匹配
                tabPanel("Dimension reduction", value = "Dimension reduction"),
                tabPanel("Batch correction", value = "Batch correction"),
                tabPanel("Clustering", value = "Clustering"),
                tabPanel("Classification", value = "Classification"),
                tabPanel("Feature selection", value = "Feature selection"),
                tabPanel("Imputation", value = "Imputation"),
                tabPanel("Spatial registration", value = "Spatial registration")
              ),
              DTOutput("metric_table"),
              tags$script(HTML("
                function showDetails(btn) {
                  var row = $(btn).closest('tr');
                  var table = $('#metric_table').DataTable();
                  var data = table.row(row).data();
                  
                  // 从原始数据获取详情
                  var description = '这里是描述'; // 需要从数据中获取
                  var advantages = '这里是优点';   // 需要从数据中获取
                  var disadvantages = '这里是缺点'; // 需要从数据中获取
                  
                  var content = `
                    <h4>Description:</h4>
                    <p>${description}</p>
                    <h4>Advantages:</h4>
                    <p>${advantages}</p>
                    <h4>Disadvantages:</h4>
                    <p>${disadvantages}</p>
                  `;
                  
                  $('#detailsContent').html(content);
                  $('#detailsModal').modal('show');
                }
              "))
            )
          )
        )
      ),
      tabItem(tabName = "summary",
              tabBox(
                width = 12,
                id = "integration_tabs",
                # Vertical Integration Tab
                tabPanel("Vertical Integration",
                  # 在 vertical Integration 标签页中使用 tabsetPanel 创建子标签页
                  tabsetPanel(
                    # 1. Dimension reduction and Clustering
                      tabPanel("DR and Clustering",
                        fluidRow(
                          column(4,
                            box(
                              title = "Selection",
                              width = NULL,
                              status = "primary",
                              solidHeader = TRUE,
                              
                              # 单选框：Data Type选择
                              radioButtons("vertical_dr_data_type", "Select Data Type:",
                                choices = c("RNA+ADT" = "rna_adt", 
                                            "RNA+ATAC" = "rna_atac", 
                                            "RNA+ADT+ATAC" = "rna_adt_atac"),
                                selected = "rna_adt"),
                              
                              selectizeInput("vertical_dr_dataset_choice",
                                "Select Datasets (min 2):",
                                choices = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","SD1","SD2"), # 默认先给RNA+ADT
                                multiple = TRUE,
                                options = list(
                                  placeholder = 'Select datasets...',
                                  plugins = list('remove_button')
                                )
                              ),
                              
                              selectizeInput("vertical_dr_method_choice",
                                "Select Methods (min 2):",
                                choices = c("Concerto", "Matilda", "moETM", "MOFA2", "Multigrate", 
                                            "sciPENN", "scMDC", "scMM", "scMoMaT", "scMSI", 
                                            "Seurat.WNN", "totalVI", "UINMF", "VIMCCA"), # RNA+ADT下的方法
                                multiple = TRUE,
                                options = list(
                                  placeholder = 'Select methods...',
                                  plugins = list('remove_button'),
                                  minItems = 2
                                )
                              ),
                              
                              selectizeInput("vertical_dr_metric_choice",
                                "Select Metrics:",
                                choices = c("cLISI","iFI","iASW", "ASW","ARI","NMI"), 
                                multiple = TRUE,
                                options = list(
                                  placeholder = 'Select metrics...',
                                  plugins = list('remove_button')
                                )
                              )
                            )
                          ),
                          column(8,
                            box(
                              title = "Dimension Reduction & Clustering Summary",
                              width = NULL,
                              status = "primary",
                              solidHeader = TRUE,
                              plotOutput("vertical_dr_summary_plot", height = "540px"),
                              downloadButton("download_vertical_dr_summary", "Download Plot")
                            )
                          )
                        )
                      )
                    )
                  ),



                  tabPanel("Vertical (Feature Selection)",
                              tabsetPanel(
                                # Clustering
                                tabPanel("Clustering",
                                        fluidRow(
                                          column(4,
                                                  box(
                                                    title = "Selection",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,

                                                    radioButtons("fs_clustering_data_type", "Select Data Type:",
                                                                  choices = c("RNA+ADT" = "rna_adt",
                                                                              "RNA+ATAC" = "rna_atac",
                                                                              "RNA+ADT+ATAC" = "rna_adt_atac"),
                                                                  selected = "rna_adt"),               
                                                    # dataset选择
                                                    selectizeInput("fs_clustering_dataset_choice",
                                                                  "Select Datasets (min 2):",
                                                                  choices = NULL, # 示例
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select datasets...',
                                                                    plugins = list('remove_button'),
                                                                    minItems = 2
                                                                  )
                                                    ),
                                                    # method选择
                                                    selectizeInput("fs_clustering_method_choice",
                                                                  "Select Methods (min 2):",
                                                                  choices = c("scMoMaT","MOFA2","Matilda"), # 示例方法
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select methods...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    # metric选择
                                                    selectizeInput("fs_clustering_metric_choice",
                                                                  "Select Metrics:",
                                                                  choices = c("ARI","NMI","ASW","iASW","iFI"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select metrics...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    # topN选择
                                                    selectizeInput("fs_clustering_topN_choice",
                                                                  "Select topN:",
                                                                  choices = c("top5","top10","top20"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select topN...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    # modality选择
                                                    selectizeInput("fs_clustering_modality_choice",
                                                                  "Select Modality:",
                                                                  choices = NULL, #示例
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select modality...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    )
                                                  )
                                          ),
                                          column(8,
                                                  box(
                                                    title = "Clustering Summary",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,
                                                    plotOutput("fs_clustering_summary_plot", height = "540px"),
                                                    downloadButton("download_fs_clustering_summary", "Download Plot")
                                                  )
                                          )
                                        )
                                ),
                                # Classification
                                tabPanel("Classification",
                                        fluidRow(
                                          column(4,
                                                  box(
                                                    title = "Selection",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,

                                                    radioButtons("fs_classification_data_type", "Select Data Type:",
                                                                  choices = c("RNA+ADT" = "rna_adt",
                                                                              "RNA+ATAC" = "rna_atac",
                                                                              "RNA+ADT+ATAC" = "rna_adt_atac"),
                                                                  selected = "rna_adt"),   
                                                    selectizeInput("fs_classification_dataset_choice",
                                                                  "Select Datasets (min 2):",
                                                                  choices =NULL,
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select datasets...',
                                                                    plugins = list('remove_button'),
                                                                    minItems = 2
                                                                  )
                                                    ),
                                                    selectizeInput("fs_classification_method_choice",
                                                                  "Select Methods (min 2):",
                                                                  choices = c("scMoMaT","MOFA2","Matilda"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select methods...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    selectizeInput("fs_classification_metric_choice",
                                                                  "Select Metrics:",
                                                                  choices = c("Average Accuracy","Overall Accuracy","F1"="f1_score","Specificity" = "specificity","Sensitivity" = "sensitivity"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select metrics...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    selectizeInput("fs_classification_topN_choice",
                                                                  "Select topN:",
                                                                  choices = c("top5","top10","top20"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select topN...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    selectizeInput("fs_classification_modality_choice",
                                                                  "Select Modality:",
                                                                  choices = NULL,
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select modality...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    )
                                                  )
                                          ),
                                          column(8,
                                                  box(
                                                    title = "Classification Summary",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,
                                                    plotOutput("fs_classification_summary_plot", height = "540px"),
                                                    downloadButton("download_fs_classification_summary", "Download Plot")
                                                  )
                                          )
                                        )
                                ),
                                # Repro
                                tabPanel("Repro",
                                        fluidRow(
                                          column(4,
                                                  box(
                                                    title = "Selection",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,

                                                    radioButtons("fs_repro_data_type", "Select Data Type:",
                                                                  choices = c("RNA+ADT" = "rna_adt",
                                                                              "RNA+ATAC" = "rna_atac",
                                                                              "RNA+ADT+ATAC" = "rna_adt_atac"),
                                                                  selected = "rna_adt"),  
                                                    selectizeInput("fs_repro_dataset_choice",
                                                                  "Select Datasets (min 2):",
                                                                  choices = NULL,
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select datasets...',
                                                                    plugins = list('remove_button'),
                                                                    minItems = 2
                                                                  )
                                                    ),
                                                    selectizeInput("fs_repro_method_choice",
                                                                  "Select Methods (min 2):",
                                                                  choices = c("scMoMaT","MOFA2","Matilda"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select methods...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    selectizeInput("fs_repro_metric_choice",
                                                                  "Select Metrics:",
                                                                  choices = c("MO" = "Intersection","MC" = "Correlation"), # 示例
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select metrics...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    selectizeInput("fs_repro_topN_choice",
                                                                  "Select topN:",
                                                                  choices = c("top5","top10","top20"),
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select topN...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    ),
                                                    selectizeInput("fs_repro_modality_choice",
                                                                  "Select Modality:",
                                                                  choices = NULL,
                                                                  multiple = TRUE,
                                                                  options = list(
                                                                    placeholder = 'Select modality...',
                                                                    plugins = list('remove_button')
                                                                  )
                                                    )
                                                  )
                                          ),
                                          column(8,
                                                  box(
                                                    title = "Reproducibility Summary",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,
                                                    plotOutput("fs_repro_summary_plot", height = "540px"),
                                                    downloadButton("download_fs_repro_summary", "Download Plot")
                                                  )
                                          )
                                        )
                                )
                              )
                    ),
                # Diagonal Integration Tab
                tabPanel("Diagonal Integration",
                  tabsetPanel(
                    # 1. Dimension reduction and Clustering
                    tabPanel("DR and Clustering",
                        fluidRow(
                          column(4,
                                box(
                                  title = "Selection",
                                  width = NULL,
                                  status = "primary",
                                  solidHeader = TRUE,
                                radioButtons("diagonal_dr_data_type", "Select Data Type:",
                                                    choices = c("[RNA, ATAC]" = "rna_atac",
                                                                "[RNA(Multi),ATAC(Multi)]" = "multi_multi"),
                                                    selected = "rna_atac", inline = FALSE),
                                selectizeInput("diagonal_dr_dataset_choice",
                                  "Select Datasets (min 2):",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select datasets...',
                                    plugins = list('remove_button')
                                  )
                                ),
                                selectizeInput("diagonal_dr_method_choice",
                                  "Select Methods (min 2):",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select methods...',
                                    plugins = list('remove_button'),
                                    minItems = 2
                                  )
                                ),
                                selectizeInput("diagonal_dr_metric_choice",
                                  "Select Metrics:",
                                  choices = c("cLISI","ARI","NMI","ASW","iFI","iASW"), # 根据实际指标修改
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select metrics...',
                                    plugins = list('remove_button')
                                  )
                                )
                              )
                            ),
                            column(8,
                              box(
                                title = "Dimension Reduction & Clustering Summary",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("diagonal_dr_summary_plot", height = "540px"),
                                downloadButton("download_diagonal_dr_summary", "Download Plot")
                              )
                            )
                          )
                        ),
                    # 2. Batch Correction
                    tabPanel("Batch Correction",
                      fluidRow(
                        column(4,
                          box(
                            title = "Selection",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            radioButtons("diagonal_batch_data_type", "Select Data Type:",
                                    choices = c("[RNA,ATAC]" = "rna_atac",
                                                "[RNA(Multi),ATAC(Multi)]" = "multi_multi"),
                                    selected = "rna_atac", inline = FALSE),
                            selectizeInput("diagonal_batch_dataset_choice",
                              "Select Datasets (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select datasets...',
                                plugins = list('remove_button')
                              )
                            ),

                            selectizeInput("diagonal_batch_method_choice",
                              "Select Methods (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select methods...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            # Batch correction相关的metric
                            selectizeInput("diagonal_batch_metric_choice",
                              "Select Metrics:",
                              choices = c("GC","ARI_batch",  "NMI_batch", "ASW_batch", "kBET", "iLISI"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select metrics...',
                                plugins = list('remove_button')
                              )
                            )
                          )
                        ),
                        column(8,
                          box(
                            title = "Batch Correction Summary",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("diagonal_batch_summary_plot", height = "540px"),
                            downloadButton("download_diagonal_batch_summary", "Download Plot")
                          )
                        )
                      )
                    ),
                    tabPanel("Classification",
                      fluidRow(
                        column(4,
                          box(
                            title = "Selection",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            radioButtons("diagonal_class_data_type", "Select Data Type:",
                                        choices = c("[RNA,ATAC]" = "rna_atac",
                                                    "[RNA(Multi),ATAC(Multi)]" = "multi_multi"),
                                        selected = "rna_atac", inline = FALSE),
                            selectizeInput("diagonal_class_dataset_choice",
                              "Select Datasets (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select datasets...',
                                plugins = list('remove_button')
                              )
                            ),

                            selectizeInput("diagonal_class_method_choice",
                              "Select Methods (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select methods...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            # Classification相关的metric
                            selectizeInput("diagonal_class_metric_choice",
                              "Select Metrics:",
                              choices = c("Average Accuracy","Overall Accuracy",  "F1" = "f1_score", "Sens" = "sensitivity", "Spec" = "specificity"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select metrics...',
                                plugins = list('remove_button')
                              )
                            )
                          )
                        ),
                        column(8,
                          box(
                            title = "Classification Summary",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("diagonal_class_summary_plot", height = "540px"),
                            downloadButton("download_diagonal_class_summary", "Download Plot")
                          )
                        )
                      )
                    )
                  )
                ),
                # Mosaic Integration Tab
                tabPanel("Mosaic Integration",
                  tabsetPanel(
                    # 1. Dimension reduction and Clustering
                    tabPanel("DR and Clustering",
                        fluidRow(
                          column(4,
                                box(
                                  title = "Selection",
                                  width = NULL,
                                  status = "primary",
                                  solidHeader = TRUE,
                                radioButtons("mosaic_dr_data_type", "Select Data Type:",
                                    choices = c("[RNA,RNA+ADT,ADT]" = "rna_rnaadt_adt",
                                                "[RNA,RNA+ATAC,ATAC]" = "rna_rnaatac_atac",
                                                "Mixed_With_Shared_Modality" = "mixed_wi_share",
                                                "Mixed_Without_Shared_Modality" = "mixed_wo_share"),
                                    selected = "rna_rnaadt_adt", inline = FALSE),
                                selectizeInput("mosaic_dr_dataset_choice",
                                  "Select Datasets (min 2):",
                                  choices =NULL,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select datasets...',
                                    plugins = list('remove_button')
                                  )
                                ),
                                selectizeInput("mosaic_dr_method_choice",
                                  "Select Methods (min 2):",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select methods...',
                                    plugins = list('remove_button'),
                                    minItems = 2
                                  )
                                ),
                                selectizeInput("mosaic_dr_metric_choice",
                                  "Select Metrics:",
                                  choices = c("cLISI","ARI",  "NMI", "ASW", "iASW", "iFI"), 
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select metrics...',
                                    plugins = list('remove_button')
                                  )
                                )
                              )
                            ),
                            column(8,
                              box(
                                title = "Dimension Reduction & Clustering Summary",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("mosaic_dr_summary_plot", height = "540px"),
                                downloadButton("download_mosaic_dr_summary", "Download Plot")
                              )
                            )
                          )
                        ),
                    # 2. Batch Correction
                    tabPanel("Batch Correction",
                      fluidRow(
                        column(4,
                          box(
                            title = "Selection",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            radioButtons("mosaic_batch_data_type", "Select Data Type:",
                                    choices = c("[RNA,RNA+ADT,ADT]" = "rna_rnaadt_adt",
                                                "[RNA,RNA+ATAC,ATAC]" = "rna_rnaatac_atac",
                                                "Mixed_With_Shared_Modality" = "mixed_wi_share",
                                                "Mixed_Without_Shared_Modality" = "mixed_wo_share"),
                                    selected = "rna_rnaadt_adt", inline = FALSE),       
                            selectizeInput("mosaic_batch_dataset_choice",
                              "Select Datasets (min 2):",
                              choices = c("D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48", "D49", "D50", "SD11", "SD12", "SD13", "SD14"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select datasets...',
                                plugins = list('remove_button')
                              )
                            ),

                            selectizeInput("mosaic_batch_method_choice",
                              "Select Methods (min 2):",
                              choices = c("MultiVI", "Cobolt", "iNMF", "StabMap", "scMoMaT", "Multigrate", "SMILE"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select methods...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            selectizeInput("mosaic_batch_metric_choice",
                              "Select Metrics:",
                              choices = c("GC","ARI_batch",  "NMI_batch", "ASW_batch", "kBET", "iLISI"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select metrics...',
                                plugins = list('remove_button')
                              )
                            )
                          )
                        ),
                        column(8,
                          box(
                            title = "Batch Correction Summary",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("mosaic_batch_summary_plot", height = "540px"),
                            downloadButton("download_mosaic_batch_summary", "Download Plot")
                          )
                        )
                      )
                    ),
                    tabPanel("Classification",
                      fluidRow(
                        column(4,
                          box(
                            title = "Selection",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            radioButtons("mosaic_class_data_type", "Select Data Type:",
                                    choices = c("[RNA,RNA+ADT,ADT]" = "rna_rnaadt_adt",
                                                "[RNA,RNA+ATAC,ATAC]" = "rna_rnaatac_atac",
                                                "Mixed_With_Shared_Modality" = "mixed_wi_share",
                                                "Mixed_Without_Shared_Modality" = "mixed_wo_share"),
                                    selected = "rna_rnaadt_adt", inline = FALSE),
                            selectizeInput("mosaic_class_dataset_choice",
                              "Select Datasets (min 2):",
                              choices = c("D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48", "D49", "D50", "SD11", "SD12", "SD13", "SD14"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select datasets...',
                                plugins = list('remove_button')
                              )
                            ),

                            selectizeInput("mosaic_class_method_choice",
                              "Select Methods (min 2):",
                              choices =  c("MultiVI", "Cobolt", "iNMF", "StabMap", "scMoMaT", "Multigrate", "Multigrate.ori",  "StabMap.ori","SMILE","SMILE.ori"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select methods...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            # Classification相关的metric
                            selectizeInput("mosaic_class_metric_choice",
                              "Select Metrics:",
                              choices = c("Average Accuracy" = "Average Accuracy" ,"Overall Accuracy" = "Overall Accuracy",  "F1" = "f1_score", "Sens" = "sensitivity", "Spec" = "specificity"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select metrics...',
                                plugins = list('remove_button')
                              )
                            )
                          )
                        ),
                        column(8,
                          box(
                            title = "Classification Summary",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("mosaic_class_summary_plot", height = "540px"),
                            downloadButton("download_mosaic_class_summary", "Download Plot")
                          )
                        )
                      )
                    )
                  )
                ),
                tabPanel("Mosaic (Imputation)",
                                tabsetPanel(
                                  tabPanel("Clustering",
                                            fluidRow(
                                              column(4,
                                                    box(
                                                      title = "Selection",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      radioButtons("imputation_clustering_data_type", "Select Data Type:",
                                                                          choices = c("Impute RNA (CITE-seq reference)" = "rna_citeseq_rna",
                                                                                      "Impute ADT (CITE-seq reference)" = "rna_citeseq_adt",
                                                                                      "Impute RNA (Multiome reference)" = "multiome_rna",
                                                                                      "Impute ATAC (Multiome reference)" = "multiome_atac"),
                                                                          selected = "rna_citeseq_rna"),
                                                      selectizeInput("imputation_clustering_dataset_choice",
                                                                      "Select Datasets (min 2):",
                                                                      choices = NULL,
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select datasets...',
                                                                        plugins = list('remove_button'),
                                                                        minItems = 2
                                                                      )
                                                      ),
                                                      selectizeInput("imputation_clustering_method_choice",
                                                                      "Select Methods (min 2):",
                                                                      choices = NULL,
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select methods...',
                                                                        plugins = list('remove_button'),
                                                                        minItems = 2
                                                                      )
                                                      ),
                                                      selectizeInput("imputation_clustering_metric_choice",
                                                                      "Select Metrics:",
                                                                      choices = c("ARI","NMI","ASW","iASW","iFI"),
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select metrics...',
                                                                        plugins = list('remove_button')
                                                                      )
                                                      )
                                                    )
                                              ),
                                              column(8,
                                                    box(
                                                      title = "Clustering Summary",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      plotOutput("imputation_clustering_summary_plot", height = "540px"),
                                                      downloadButton("download_imputation_clustering_summary", "Download Plot")
                                                    )
                                              )
                                            )
                                  ),
                                  tabPanel("Classification",
                                            fluidRow(
                                              column(4,
                                                    box(
                                                      title = "Selection",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,


                                                      radioButtons("imputation_classification_data_type", "Select Data Type:",
                                                            choices = c("Impute RNA (CITE-seq reference)" = "rna_citeseq_rna",
                                                                        "Impute ADT (CITE-seq reference)" = "rna_citeseq_adt",
                                                                        "Impute RNA (Multiome reference)" = "multiome_rna",
                                                                        "Impute ATAC (Multiome reference)" = "multiome_atac"),
                                                            selected = "rna_citeseq_rna"),
                                                      selectizeInput("imputation_classification_dataset_choice",
                                                                      "Select Datasets (min 2):",
                                                                      choices = NULL,
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select datasets...',
                                                                        plugins = list('remove_button')
                                                                      )
                                                      ),
                                                      selectizeInput("imputation_classification_method_choice",
                                                                      "Select Methods (min 2):",
                                                                      choices = NULL,
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select methods...',
                                                                        plugins = list('remove_button'),
                                                                        minItems = 2
                                                                      )
                                                      ),
                                                      selectizeInput("imputation_classification_metric_choice",
                                                                      "Select Metrics:",
                                                                      choices = c("Average Accuracy","Overall Accuracy","F1","Spec","Sens"),
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select metrics...',
                                                                        plugins = list('remove_button')
                                                                      )
                                                      )
                                                    )
                                              ),
                                              column(8,
                                                    box(
                                                      title = "Classification Summary",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      plotOutput("imputation_classification_summary_plot", height = "540px"),
                                                      downloadButton("download_imputation_classification_summary", "Download Plot")
                                                    )
                                              )
                                            )
                                  ),
                                  tabPanel("Structure",
                                            fluidRow(
                                              column(4,
                                                    box(
                                                      title = "Selection",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,

                                                      radioButtons("imputation_structure_data_type", "Select Data Type:",
                                                            choices = c("Impute RNA (CITE-seq reference)" = "rna_citeseq_rna",
                                                                        "Impute ADT (CITE-seq reference)" = "rna_citeseq_adt",
                                                                        "Impute RNA (Multiome reference)" = "multiome_rna",
                                                                        "Impute ATAC (Multiome reference)" = "multiome_atac"),
                                                            selected = "rna_citeseq_rna"),
                                                      selectizeInput("imputation_structure_dataset_choice",
                                                                      "Select Datasets (min 2):",
                                                                      choices = NULL, 
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select datasets...',
                                                                        plugins = list('remove_button'),
                                                                        minItems = 2
                                                                      )
                                                      ),
                                                      selectizeInput("imputation_structure_method_choice",
                                                                      "Select Methods (min 2):",
                                                                      choices = NULL,
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select methods...',
                                                                        plugins = list('remove_button'),
                                                                        minItems = 2
                                                                      )
                                                      ),
                                                      selectizeInput("imputation_structure_metric_choice",
                                                                      "Select Metrics:",
                                                                      choices = c("de_cor","hvg_cor","smse"),
                                                                      multiple = TRUE,
                                                                      options = list(
                                                                        placeholder = 'Select metrics...',
                                                                        plugins = list('remove_button')
                                                                      )
                                                      )
                                                    )
                                              ),
                                              column(8,
                                                    box(
                                                      title = "Structure Summary",
                                                      width = NULL,
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      plotOutput("imputation_structure_summary_plot", height = "540px"),
                                                      downloadButton("download_imputation_structure_summary", "Download Plot")
                                                    )
                                              )
                                            )
                                  )
                                )
                  ),
                # Cross Integration Tab
                tabPanel("Cross Integration",
                  # 在 Cross Integration 标签页中使用 tabsetPanel 创建子标签页
                  tabsetPanel(
                    # 1. Dimension reduction and Clustering
                    tabPanel("DR and Clustering",
                        fluidRow(
                          column(4,
                                box(
                                  title = "Selection",
                                  width = NULL,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  radioButtons("cross_dr_data_type", "Select Data Type:",
                                    choices = c("Multiple RNA+ADT" = "multi_rna_adt",
                                                "Multiple RNA+ATAC" = "multi_rna_atac",
                                                "Multiple ADT+ATAC" = "multi_adt_atac",
                                                "Multiple RNA+ADT+ATAC" = "multi_rna_adt_atac"),
                                    selected = "multi_rna_adt", inline = FALSE),
                                selectizeInput("cross_dr_dataset_choice",
                                  "Select Datasets (min 2):",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select datasets...',
                                    plugins = list('remove_button'),
                                    minItems = 2
                                  )
                                ),
                                selectizeInput("cross_dr_method_choice",
                                  "Select Methods (min 2):",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select methods...',
                                    plugins = list('remove_button'),
                                    minItems = 2
                                  )
                                ),
                                selectizeInput("cross_dr_metric_choice",
                                  "Select Metrics:",
                                  choices = c("cLISI","ARI",  "NMI", "ASW", "iASW", "iFI"), 
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select metrics...',
                                    plugins = list('remove_button')
                                  )
                                )
                              )
                            ),
                            column(8,
                              box(
                                title = "Dimension Reduction & Clustering Summary",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("cross_dr_summary_plot", height = "540px"),
                                downloadButton("download_cross_dr_summary", "Download Plot")
                              )
                            )
                          )
                        ),
                    # 2. Batch Correction
                    tabPanel("Batch Correction",
                      fluidRow(
                        column(4,
                          box(
                            title = "Selection",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            radioButtons("cross_batch_data_type", "Select Data Type:",
                                choices = c("Multiple RNA+ADT" = "multi_rna_adt",
                                            "Multiple RNA+ATAC" = "multi_rna_atac",
                                            "Multiple ADT+ATAC" = "multi_adt_atac",
                                            "Multiple RNA+ADT+ATAC" = "multi_rna_adt_atac"),
                                selected = "multi_rna_adt", inline = FALSE),         
                            selectizeInput("cross_batch_dataset_choice",
                              "Select Datasets (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select datasets...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            selectizeInput("cross_batch_method_choice",
                              "Select Methods (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select methods...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            selectizeInput("cross_batch_metric_choice",
                              "Select Metrics:",
                              choices = c("GC","ARI_batch",  "NMI_batch", "ASW_batch", "kBET", "iLISI","PCR"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select metrics...',
                                plugins = list('remove_button')
                              )
                            )
                          )
                        ),
                        column(8,
                          box(
                            title = "Batch Correction Summary",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("cross_batch_summary_plot", height = "540px"),
                            downloadButton("download_cross_batch_summary", "Download Plot")
                          )
                        )
                      )
                    ),
                    tabPanel("Classification",
                      fluidRow(
                        column(4,
                          box(
                            title = "Selection",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            radioButtons("cross_class_data_type", "Select Data Type:",
                                choices = c("Multiple RNA+ADT" = "multi_rna_adt",
                                            "Multiple RNA+ATAC" = "multi_rna_atac",
                                            "Multiple ADT+ATAC" = "multi_adt_atac",
                                            "Multiple RNA+ADT+ATAC" = "multi_rna_adt_atac"),
                                selected = "multi_rna_adt", inline = FALSE),  
                            selectizeInput("cross_class_dataset_choice",
                              "Select Datasets (min 2):",
                              choices = NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select datasets...',
                                plugins = list('remove_button')
                              )
                            ),

                            selectizeInput("cross_class_method_choice",
                              "Select Methods (min 2):",
                              choices =  NULL, 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select methods...',
                                plugins = list('remove_button'),
                                minItems = 2
                              )
                            ),

                            # Classification相关的metric
                            selectizeInput("cross_class_metric_choice",
                              "Select Metrics:",
                              choices = c("Average Accuracy","Overall Accuracy",  "F1" = "f1_score",  "Sens" = "sensitivity", "Spec" = "specificity"), 
                              multiple = TRUE,
                              options = list(
                                placeholder = 'Select metrics...',
                                plugins = list('remove_button')
                              )
                            )
                          )
                        ),
                        column(8,
                          box(
                            title = "Classification Summary",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("cross_class_summary_plot", height = "540px"),
                            downloadButton("download_cross_class_summary", "Download Plot")
                          )
                        )
                      )
                    )
                  )
                ),
                tabPanel("Cross (Spatial Registration)",
                  tabsetPanel(
                    tabPanel("Spatial Registration",
                          fluidRow(
                            column(4,
                              box(
                                title = "Selection",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("spatial_dataset_choice",
                                  "Select Datasets (min 2):",
                                  choices = c("D60_patient10", "D60_patient2", "D60_patient5", "D60_patient9", 
                                              "D61_donor1", "D61_donor2", "D61_donor3", "D62", "D63", 
                                              "D64_donor10", "D64_donor2", "D64_donor4"),
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select datasets...',
                                    plugins = list('remove_button')
                                  )
                                ),
                                selectizeInput("spatial_method_choice",
                                  "Select Methods (min 2):",
                                  choices = c("PASTEpairwise", "PASTEcentre", "PASTE2", "SPIRAL", "GPSA"),
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select methods...',
                                    plugins = list('remove_button'),
                                    minItems = 2
                                  )
                                ),
                                selectizeInput("spatial_metric_choice",
                                  "Select Metrics:",
                                  choices = c("LTARI", "PAA", "SCS"),
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Select metrics...',
                                    plugins = list('remove_button')
                                  )
                                )
                              )
                            ),
                            column(8,
                              box(
                                title = "Spatial Registration Summary",
                                width = NULL,
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("spatial_summary_plot", height = "540px"),
                                downloadButton("download_spatial_summary", "Download Plot")
                              )
                            )
                          )
                        )
                      )
                )              
          )
      ),
      tabItem(tabName = "scib",
        # div(
        #   style = "margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-left: 5px solid #17a2b8;",
        #   p("Please select at least two methods to compare their performance.")
        # ),

        # 使用tabBox来划分vertical, diagonal, mosaic, cross
        tabBox(
          width = 12,
          id = "scib_integration_tabs",

          tabPanel("Vertical Integration",
            # 与summary保持一致，这里只提供DR and Clustering子标签
            tabsetPanel(
              tabPanel("DR and Clustering",
                fluidRow(
                  column(4,
                    box(
                      title = "Selection",
                      width = NULL,
                      status = "primary",
                      solidHeader = TRUE,

                      # Data type选择
                      selectInput("vertical_ds_data_type", "Select Data Type:", 
                                  choices = c("RNA+ADT" = "rna_adt",
                                              "RNA+ATAC" = "rna_atac",
                                              "RNA+ADT+ATAC" = "rna_adt_atac"),
                                  selected = "rna_adt"),

                      # Dataset选择（待observeEvent动态更新）
                      selectizeInput("vertical_ds_dataset_choice", "Select Dataset:",
                                    choices = NULL, multiple = FALSE,
                                    options = list(placeholder = 'Select a dataset...',
                                                    plugins = list('remove_button'))),

                      # Method选择（待observeEvent动态更新）
                      selectizeInput("vertical_ds_method_choice", "Select Methods (min 2):",
                                    choices = NULL, multiple = TRUE,
                                    options = list(placeholder = 'Select methods...',
                                                    plugins = list('remove_button'),
                                                    minItems = 2)),

                      # Metric选择
                      selectizeInput("vertical_ds_metric_choice", "Select Metrics:",
                                    choices = c("cLISI","iFI","iASW","ASW","ARI","NMI"),
                                    multiple = TRUE,
                                    options = list(placeholder = 'Select metrics...',
                                                    plugins = list('remove_button'))),

                      # 文件上传
                      uiOutput("vertical_ds_file_ui"), downloadButton("download_vertical_demo_csv", "Download Demo CSV")
                    )
                  ),
                  column(8,
                    box(
                      title = "Dimension Reduction & Clustering",
                      width = NULL,
                      status = "primary",
                      solidHeader = TRUE,
                      plotOutput("vertical_ds_plot", height = "540px"),
                      downloadButton("download_vertical_ds_plot", "Download Plot")
                    )
                  )
                )
              )
            )
          ),


          tabPanel("Vertical (Feature Selection)",
                    tabsetPanel(
                      # Clustering
                      tabPanel("Clustering",
                              fluidRow(
                                column(4,
                                        box(
                                          title = "Selection", width = NULL,
                                          status = "primary", solidHeader = TRUE,

                                          radioButtons("fs_ds_clustering_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt",
                                                          "RNA+ATAC" = "rna_atac",
                                                          "RNA+ADT+ATAC" = "rna_adt_atac"),
                                              selected = "rna_adt", inline= TRUE),  
                                          selectizeInput("fs_ds_clustering_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL, # 示例
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_clustering_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = c("scMoMaT","MOFA2","Matilda"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button')

                                                        )
                                          ),
                                          selectizeInput("fs_ds_clustering_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("ARI","NMI","ASW","iASW","iFI"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_clustering_topN_choice",
                                                        "Select topN:",
                                                        choices = c("top5","top10","top20"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select topN...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_clustering_modality_choice",
                                                        "Select Modality:",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select modality...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          uiOutput("fs_ds_clustering_file_ui"),
                                          downloadButton("download_fs_clustering_demo_csv", "Download Demo CSV")
                                        )
                                ),
                                column(8,
                                        box(
                                          title = "Clustering",
                                          width = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          plotOutput("fs_ds_clustering_plot", height = "540px"),
                                          downloadButton("download_fs_ds_clustering_plot", "Download Plot")
                                        )
                                )
                              )
                      ),
                      # Classification
                      tabPanel("Classification",
                              fluidRow(
                                column(4,
                                        box(
                                          title = "Selection", width = NULL,
                                          status = "primary", solidHeader = TRUE,


                                          radioButtons("fs_ds_classification_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt",
                                                          "RNA+ATAC" = "rna_atac",
                                                          "RNA+ADT+ATAC" = "rna_adt_atac"),
                                              selected = "rna_adt", inline= TRUE),  
                                          selectizeInput("fs_ds_classification_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL, 
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_classification_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = c("scMoMaT","MOFA2","Matilda"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_classification_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("Average Accuracy","Overall Accuracy", "F1" = "f1_score", "Spec"="specificity", "Sens"="sensitivity"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_classification_topN_choice",
                                                        "Select topN:",
                                                        choices = c("top5","top10","top20"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select topN...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_classification_modality_choice",
                                                        "Select Modality:",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select modality...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          uiOutput("fs_ds_classification_file_ui"),
                                          downloadButton("download_fs_classification_demo_csv", "Download Demo CSV")
                                        )
                                ),
                                column(8,
                                        box(
                                          title = "Classification",
                                          width = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          plotOutput("fs_ds_classification_plot", height = "540px"),
                                          downloadButton("download_fs_ds_classification_plot", "Download Plot")
                                        )
                                )
                              )
                      ),
                      # Repro
                      tabPanel("Repro",
                              fluidRow(
                                column(4,
                                        box(
                                          title = "Selection", width = NULL,
                                          status = "primary", solidHeader = TRUE,
                                          radioButtons("fs_ds_repro_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt",
                                                          "RNA+ATAC" = "rna_atac",
                                                          "RNA+ADT+ATAC" = "rna_adt_atac"),
                                              selected = "rna_adt", inline= TRUE),  

                                          selectizeInput("fs_ds_repro_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL, 
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_repro_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = c("scMoMaT","MOFA2","Matilda"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_repro_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("MO","MC"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_repro_topN_choice",
                                                        "Select topN:",
                                                        choices = c("top5","top10","top20"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select topN...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("fs_ds_repro_modality_choice",
                                                        "Select Modality:",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select modality...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          uiOutput("fs_ds_repro_file_ui"),
                                          downloadButton("download_fs_repro_demo_csv", "Download Demo CSV")
                                        )
                                ),
                                column(8,
                                        box(
                                          title = "Reproducibility",
                                          width = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          plotOutput("fs_ds_repro_plot", height = "540px"),
                                          downloadButton("download_fs_ds_repro_plot", "Download Plot")
                                        )
                                )
                              )
                      )
                    )
          ),
          # Diagonal dataset-specific
          tabPanel("Diagonal Integration",
                   tabsetPanel(
                     tabPanel("DR and Clustering",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("diagonal_ds_dr_data_type", "Select Data Type:",
                                                    choices = c("[RNA,ATAC]" = "rna_atac",
                                                                "[RNA(Multi),ATAC(Multi)]" = "multi_multi"),
                                                    selected = "rna_atac", inline = FALSE),

                                         selectizeInput("diagonal_ds_dr_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("diagonal_ds_dr_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("diagonal_ds_dr_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("cLISI","ARI","NMI","ASW","iFI","iASW"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                          uiOutput("diagonal_ds_clustering_file_ui"),
                                          downloadButton("download_diagonal_cluster_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Dimension Reduction & Clustering",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("diagonal_ds_dr_plot", height = "540px"),
                                         downloadButton("download_diagonal_ds_dr_plot", "Download Plot")
                                       )
                                )
                              )
                     ),
                     tabPanel("Batch Correction",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("diagonal_ds_batch_data_type", "Select Data Type:",
                                                    choices = c("[RNA,ATAC]" = "rna_atac",
                                                                "[RNA(Multi),ATAC(Multi)]" = "multi_multi"),
                                                    selected = "rna_atac", inline = FALSE),
                                         selectizeInput("diagonal_ds_batch_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("diagonal_ds_batch_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("diagonal_ds_batch_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("GC","ARI_batch",  "NMI_batch", "ASW_batch", "kBET", "iLISI"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                          uiOutput("diagonal_ds_batch_file_ui"),
                                          downloadButton("download_diagonal_batch_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Batch Correction",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("diagonal_ds_batch_plot", height = "540px"),
                                         downloadButton("download_diagonal_ds_batch_plot", "Download Plot")
                                       )
                                )
                              )
                     ),
                     tabPanel("Classification",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("diagonal_ds_class_data_type", "Select Data Type:",
                                                    choices = c("[RNA,ATAC]" = "rna_atac",
                                                                "[RNA(Multi),ATAC(Multi)]" = "multi_multi"),
                                                    selected = "rna_atac", inline = FALSE),
                                         selectizeInput("diagonal_ds_class_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("diagonal_ds_class_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("diagonal_ds_class_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("Average Accuracy","Overall Accuracy",  "F1" = "f1_score", "Sens" = "sensitivity", "Spec" = "specificity"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                          uiOutput("diagonal_ds_class_file_ui"),
                                          downloadButton("download_diagonal_class_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Classification",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("diagonal_ds_class_plot", height = "540px"),
                                         downloadButton("download_diagonal_ds_class_plot", "Download Plot")
                                       )
                                )
                              )
                     )
                   )
          ),

          # Mosaic dataset-specific
          # 与Diagonal类似，有DR and Clustering, Batch Correction, Classification
          tabPanel("Mosaic Integration",
                   tabsetPanel(
                     tabPanel("DR and Clustering",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("mosaic_ds_dr_data_type", "Select Data Type:",
                                          choices = c("[RNA,RNA+ADT,ADT]" = "rna_rnaadt_adt",
                                                      "[RNA,RNA+ATAC,ATAC]" = "rna_rnaatac_atac",
                                                      "Mixed_With_Shared_Modality" = "mixed_wi_share",
                                                      "Mixed_Without_Shared_Modality" = "mixed_wo_share"),
                                          selected = "rna_rnaadt_adt", inline = FALSE),


                                         selectizeInput("mosaic_ds_dr_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("mosaic_ds_dr_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("mosaic_ds_dr_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("cLISI","ARI","NMI","ASW","iASW","iFI"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("mosaic_ds_clustering_file_ui"),
                                         downloadButton("download_mosaic_cluster_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Dimension Reduction & Clustering",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("mosaic_ds_dr_plot", height = "540px"),
                                         downloadButton("download_mosaic_ds_dr_plot", "Download Plot")
                                       )
                                )
                              )
                     ),
                     tabPanel("Batch Correction",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("mosaic_ds_batch_data_type", "Select Data Type:",
                                          choices = c("[RNA,RNA+ADT,ADT]" = "rna_rnaadt_adt",
                                                      "[RNA,RNA+ATAC,ATAC]" = "rna_rnaatac_atac",
                                                      "Mixed_With_Shared_Modality" = "mixed_wi_share",
                                                      "Mixed_Without_Shared_Modality" = "mixed_wo_share"),
                                          selected = "rna_rnaadt_adt", inline = FALSE),
                                         selectizeInput("mosaic_ds_batch_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = c("D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48", "D49", "D50", "SD11", "SD12", "SD13", "SD14"),
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("mosaic_ds_batch_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = c("MultiVI", "Cobolt", "iNMF", "StabMap", "scMoMaT", "Multigrate", "SMILE"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("mosaic_ds_batch_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("GC","ARI_batch", "NMI_batch","ASW_batch", "kBET", "iLISI"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("mosaic_ds_batch_file_ui"),
                                         downloadButton("download_mosaic_batch_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Batch Correction",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("mosaic_ds_batch_plot", height = "540px"),
                                         downloadButton("download_mosaic_ds_batch_plot", "Download Plot")
                                       )
                                )
                              )
                     ),
                     tabPanel("Classification",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,

                                         radioButtons("mosaic_ds_class_data_type", "Select Data Type:",
                                          choices = c("[RNA,RNA+ADT,ADT]" = "rna_rnaadt_adt",
                                                      "[RNA,RNA+ATAC,ATAC]" = "rna_rnaatac_atac",
                                                      "Mixed_With_Shared_Modality" = "mixed_wi_share",
                                                      "Mixed_Without_Shared_Modality" = "mixed_wo_share"),
                                          selected = "rna_rnaadt_adt", inline = FALSE),
                                         selectizeInput("mosaic_ds_class_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = c("D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48", "D49", "D50", "SD11", "SD12", "SD13", "SD14"),
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("mosaic_ds_class_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices =  c("MultiVI", "Cobolt", "iNMF", "StabMap", "scMoMaT", "Multigrate", "Multigrate.ori", "StabMap.ori","SMILE","SMILE.ori"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("mosaic_ds_class_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("Average Accuracy","Overall Accuracy", "F1" = "f1_score", "Sens" = "sensitivity", "Spec" = "specificity"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("mosaic_ds_class_file_ui"),
                                         downloadButton("download_mosaic_class_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Classification",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("mosaic_ds_class_plot", height = "540px"),
                                         downloadButton("download_mosaic_ds_class_plot", "Download Plot")
                                       )
                                )
                              )
                     )
                   )
          ),
          tabPanel("Mosaic (Imputation)",
                    tabsetPanel(
                      tabPanel("Clustering",
                              fluidRow(
                                column(4,
                                        box(
                                          title = "Selection", width = NULL,
                                          status = "primary", solidHeader = TRUE,



                                          radioButtons("imputation_ds_clustering_data_type", "Select Data Type:",
                                                    choices = c("Impute RNA (CITE-seq reference)" = "rna_citeseq_rna",
                                                                "Impute ADT (CITE-seq reference)" = "rna_citeseq_adt",
                                                                "Impute RNA (Multiome reference)" = "multiome_rna",
                                                                "Impute ATAC (Multiome reference)" = "multiome_atac"),
                                                    selected = "rna_citeseq_rna"),
                                          selectizeInput("imputation_ds_clustering_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL, 
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("imputation_ds_clustering_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                          ),
                                          selectizeInput("imputation_ds_clustering_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("ARI","NMI","ASW","iASW","iFI"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          uiOutput("imputation_ds_clustering_file_ui"),
                                          downloadButton("download_imputation_cluster_demo_csv", "Download Demo CSV")
                                        )
                                ),
                                column(8,
                                        box(
                                          title = "Clustering",
                                          width = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          plotOutput("imputation_ds_clustering_plot", height = "540px"),
                                          downloadButton("download_imputation_ds_clustering_plot", "Download Plot")
                                        )
                                )
                              )
                      ),
                      tabPanel("Classification",
                              fluidRow(
                                column(4,
                                        box(
                                          title = "Selection", width = NULL,
                                          status = "primary", solidHeader = TRUE,

                                          radioButtons("imputation_ds_classification_data_type", "Select Data Type:",
                                                    choices = c("Impute RNA (CITE-seq reference)" = "rna_citeseq_rna",
                                                                "Impute ADT (CITE-seq reference)" = "rna_citeseq_adt",
                                                                "Impute RNA (Multiome reference)" = "multiome_rna",
                                                                "Impute ATAC (Multiome reference)" = "multiome_atac"),
                                                    selected = "rna_citeseq_rna"),
                                          selectizeInput("imputation_ds_classification_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL, 
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("imputation_ds_classification_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                          ),
                                          selectizeInput("imputation_ds_classification_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("Average Accuracy","Overall Accuracy","F1"="f1_score","Spec"="specificity","Sens"="sensitivity"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          uiOutput("imputation_ds_classification_file_ui"),
                                          downloadButton("download_imputation_classification_demo_csv", "Download Demo CSV")
                                        )
                                ),
                                column(8,
                                        box(
                                          title = "Classification",
                                          width = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          plotOutput("imputation_ds_classification_plot", height = "540px"),
                                          downloadButton("download_imputation_ds_classification_plot", "Download Plot")
                                        )
                                )
                              )
                      ),
                      tabPanel("Structure",
                              fluidRow(
                                column(4,
                                        box(
                                          title = "Selection", width = NULL,
                                          status = "primary", solidHeader = TRUE,
                                          radioButtons("imputation_ds_structure_data_type", "Select Data Type:",
                                                    choices = c("Impute RNA (CITE-seq reference)" = "rna_citeseq_rna",
                                                                "Impute ADT (CITE-seq reference)" = "rna_citeseq_adt",
                                                                "Impute RNA (Multiome reference)" = "multiome_rna",
                                                                "Impute ATAC (Multiome reference)" = "multiome_atac"),
                                                    selected = "rna_citeseq_rna"),
                                          selectizeInput("imputation_ds_structure_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL, 
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          selectizeInput("imputation_ds_structure_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                          ),
                                          selectizeInput("imputation_ds_structure_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("de_cor","hvg_cor","smse"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                          ),
                                          uiOutput("imputation_ds_structure_file_ui"),
                                          downloadButton("download_imputation_structure_demo_csv", "Download Demo CSV")                           
                                        )
                                ),
                                column(8,
                                        box(
                                          title = "Structure",
                                          width = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          plotOutput("imputation_ds_structure_plot", height = "540px"),
                                          downloadButton("download_imputation_ds_structure_plot", "Download Plot")
                                        )
                                )
                              )
                      )
                    )
          ),
          # Cross dataset-specific
          # 有DR and Clustering, Batch Correction, Classification, Spatial Registration
          tabPanel("Cross Integration",
                   tabsetPanel(
                     tabPanel("DR and Clustering",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("cross_ds_dr_data_type", "Select Data Type:",
                                            choices = c("Multiple RNA+ADT" = "multi_rna_adt",
                                                        "Multiple RNA+ATAC" = "multi_rna_atac",
                                                        "Multiple ADT+ATAC" = "multi_adt_atac",
                                                        "Multiple RNA+ADT+ATAC" = "multi_rna_adt_atac"),
                                            selected = "multi_rna_adt", inline = FALSE),  
                                         selectizeInput("cross_ds_dr_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("cross_ds_dr_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("cross_ds_dr_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("cLISI","ARI","NMI","ASW","iASW","iFI"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("cross_ds_clustering_file_ui"),
                                         downloadButton("download_cross_cluster_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Dimension Reduction & Clustering",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("cross_ds_dr_plot", height = "540px"),
                                         downloadButton("download_cross_ds_dr_plot", "Download Plot")
                                       )
                                )
                              )
                     ),
                     tabPanel("Batch Correction",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("cross_ds_batch_data_type", "Select Data Type:",
                                            choices = c("Multiple RNA+ADT" = "multi_rna_adt",
                                                        "Multiple RNA+ATAC" = "multi_rna_atac",
                                                        "Multiple ADT+ATAC" = "multi_adt_atac",
                                                        "Multiple RNA+ADT+ATAC" = "multi_rna_adt_atac"),
                                            selected = "multi_rna_adt", inline = FALSE),  
                                         selectizeInput("cross_ds_batch_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("cross_ds_batch_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("cross_ds_batch_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("GC","ARI_batch","NMI_batch","ASW_batch","kBET","iLISI","PCR"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("cross_ds_batch_file_ui"),
                                         downloadButton("download_cross_batch_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Batch Correction",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("cross_ds_batch_plot", height = "540px"),
                                         downloadButton("download_cross_ds_batch_plot", "Download Plot")
                                       )
                                )
                              )
                     ),
                     tabPanel("Classification",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         radioButtons("cross_ds_class_data_type", "Select Data Type:",
                                            choices = c("Multiple RNA+ADT" = "multi_rna_adt",
                                                        "Multiple RNA+ATAC" = "multi_rna_atac",
                                                        "Multiple ADT+ATAC" = "multi_adt_atac",
                                                        "Multiple RNA+ADT+ATAC" = "multi_rna_adt_atac"),
                                            selected = "multi_rna_adt", inline = FALSE),  
                                         selectizeInput("cross_ds_class_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = NULL,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("cross_ds_class_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices =  NULL,
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("cross_ds_class_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("Average Accuracy","Overall Accuracy","F1" = "f1_score", "Sens" = "sensitivity", "Spec" = "specificity"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("cross_ds_class_file_ui"),
                                         downloadButton("download_cross_class_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Classification",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("cross_ds_class_plot", height = "540px"),
                                         downloadButton("download_cross_ds_class_plot", "Download Plot")
                                       )
                                )
                              )
                     )
                   )
          ),
          tabPanel("Cross (Spatial Registration)",
            tabsetPanel(
                     tabPanel("Spatial Registration",
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Selection",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         selectizeInput("cross_ds_spatial_dataset_choice",
                                                        "Select Dataset:",
                                                        choices = c("D60_patient10", "D60_patient2", "D60_patient5", "D60_patient9", 
                                                                    "D61_donor1", "D61_donor2", "D61_donor3", "D62", "D63", 
                                                                    "D64_donor10", "D64_donor2", "D64_donor4"),
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = 'Select a dataset...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         selectizeInput("cross_ds_spatial_method_choice",
                                                        "Select Methods (min 2):",
                                                        choices = c("PASTEpairwise", "PASTEcentre", "PASTE2", "SPIRAL", "GPSA"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select methods...',
                                                          plugins = list('remove_button'),
                                                          minItems = 2
                                                        )
                                         ),
                                         selectizeInput("cross_ds_spatial_metric_choice",
                                                        "Select Metrics:",
                                                        choices = c("LTARI", "PAA", "SCS"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = 'Select metrics...',
                                                          plugins = list('remove_button')
                                                        )
                                         ),
                                         uiOutput("spatial_file_ui"),
                                         downloadButton("download_spatial_demo_csv", "Download Demo CSV")
                                       )
                                ),
                                column(8,
                                       box(
                                         title = "Spatial Registration",
                                         width = NULL,
                                         status = "primary",
                                         solidHeader = TRUE,
                                         plotOutput("cross_ds_spatial_plot", height = "540px"),
                                         downloadButton("download_cross_ds_spatial_plot", "Download Plot")
                                       )
                                )
                              )
                     )
            )
          )
        )
      ),
      tabItem(tabName = "timememory",
        tabBox(
          width = 12,
          id = "timememory_tabs",
          
          # Vertical Integration Tab for Time & Memory
          tabPanel("Vertical Integration",
            fluidRow(
              # 左侧为Time部分
              column(width = 6,
                box(
                  title = "Time",
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  

                  
                  selectInput("vertical_time_data_type",
                    "Select Data Type:",
                    choices = c("RNA+ADT" = "rna_adt",
                                "RNA+ATAC" = "rna_atac",
                                "RNA+ADT+ATAC" = "rna_adt_atac"),
                    selected = "rna_adt"
                  ),
                  # 选择方法和数据类型
                  selectizeInput("vertical_time_method_choice",
                    "Select Methods (min 1):",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      placeholder = 'Select one or more methods...',
                      plugins = list('remove_button')
                    )
                  ),
                  uiOutput("vertical_time_file_ui"),
                  downloadButton("download_vertical_time_demo_csv", "Download Demo CSV"),
                                    
                  # 绘图输出
                  plotOutput("vertical_time_plot", height = "430px"),
                  downloadButton("download_vertical_time", "Download Time Plot")
                )
              ),
              
              # 右侧为Memory部分
              column(width = 6,
                box(
                  title = "Memory",
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  

                  
                  selectInput("vertical_memory_data_type",
                    "Select Data Type:",
                    choices = c("RNA+ADT" = "rna_adt",
                                "RNA+ATAC" = "rna_atac",
                                "RNA+ADT+ATAC" = "rna_adt_atac"),
                    selected = "rna_adt"
                  ),
                  # 选择方法和数据类型
                  selectizeInput("vertical_memory_method_choice",
                    "Select Methods (min 1):",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      placeholder = 'Select one or more methods...',
                      plugins = list('remove_button')
                    )
                  ), 
                  uiOutput("vertical_memory_file_ui"),
                  downloadButton("download_vertical_memory_demo_csv", "Download Demo CSV"),

                  # 绘图输出
                  plotOutput("vertical_memory_plot", height = "430px"),
                  downloadButton("download_vertical_memory", "Download Memory Plot")
                )
              )
            )
          ),
          

          tabPanel("Diagonal Integration",
            fluidRow(
              # 左边Time panel
              column(width = 6,
                    box(
                      title = "Time",
                      width = NULL,
                      status = "primary",
                      solidHeader = TRUE,
                      selectInput("diagonal_time_data_type",
                                  "Select Data Type:",
                                  choices = c(
                                              "RNA+ATAC" = "rna_atac",
                                              "RNA(Multi)+ATAC(Multi)" = "mrna_matac"),
                                  selected = "rna_atac"),

                      selectizeInput("diagonal_time_method_choice",
                                      "Select Methods (min 1):",
                                      choices = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        placeholder = 'Select methods...',
                                        plugins = list('remove_button'),
                                        minItems = 1
                                      )
                      ), 
                      uiOutput("diagonal_time_file_ui"),
                      downloadButton("download_diagonal_time_demo_csv", "Download Demo CSV"),                   
                      plotOutput("diagonal_time_plot", height = "430px"),
                      downloadButton("download_diagonal_time_plot", "Download Time Plot")
                    )
              ),
              # 右边Memory panel
              column(width = 6,
                    box(
                      title = "Memory",
                      width = NULL,
                      status = "primary",
                      solidHeader = TRUE,

                      # 数据类型选择
                      selectInput("diagonal_memory_data_type",
                                  "Select Data Type:",
                                  choices = c(
                                              "RNA+ATAC" = "rna_atac",
                                              "RNA(Multi)+ATAC(Multi)" = "mrna_matac"),
                                  selected = "rna_atac"),
                      # 方法选择
                      selectizeInput("diagonal_memory_method_choice",
                                      "Select Methods (min 1):",
                                      choices = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        placeholder = 'Select methods...',
                                        plugins = list('remove_button'),
                                        minItems = 1
                                      )
                      ),
                      uiOutput("diagonal_memory_file_ui"),
                      downloadButton("download_diagonal_memory_demo_csv", "Download Demo CSV"),                       
                      plotOutput("diagonal_memory_plot", height = "430px"),
                      downloadButton("download_diagonal_memory_plot", "Download Memory Plot")
                    )
              )
            )
          ),
          tabPanel("Mosaic integration",
                    fluidRow(
                      column(6,
                            box(
                              title = "Time", width = NULL, status = "primary", solidHeader = TRUE,

                              selectInput("mosaic_time_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt", "RNA+ATAC" ="rna_atac", "Mixed_With_Shared_Modality" = "fle_w_sha", "Mixed_Without_Shared_Modality" ="fle_wo_sha"),
                                              selected = "rna_adt"),
                              selectizeInput("mosaic_time_method_choice", "Select Methods (min 1):",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = list(placeholder = 'Select methods...', plugins = list('remove_button'))
                              ),   
                              uiOutput("mosaic_time_file_ui"),
                              downloadButton("download_mosaic_time_demo_csv", "Download Demo CSV"),                                            
                              plotOutput("mosaic_time_plot", height = "430px")
                            )
                      ),
                      column(6,
                            box(
                              title = "Memory", width = NULL, status = "primary", solidHeader = TRUE,

                              selectizeInput("mosaic_memory_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt", "RNA+ATAC" ="rna_atac", "Mixed_With_Shared_Modality" = "fle_w_sha", "Mixed_Without_Shared_Modality" ="fle_wo_sha"),
                                              multiple = FALSE),
                              selectizeInput("mosaic_memory_method_choice", "Select Methods (min 1):",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = list(placeholder = 'Select methods...', plugins = list('remove_button'))
                              ), 
                              uiOutput("mosaic_memory_file_ui"),
                              downloadButton("download_mosaic_memory_demo_csv", "Download Demo CSV"),                                                
                              plotOutput("mosaic_memory_plot", height = "430px")
                            )
                      )
                    )
          ),
          tabPanel("Cross integration",
                    fluidRow(
                      column(6,
                            box(
                              title = "Time", width = NULL, status = "primary", solidHeader = TRUE,

                              selectizeInput("cross_time_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt","RNA+ATAC" = "rna_atac", "ADT+ATAC"= "adt_atac", "RNA+ADT+ATAC"="rna_adt_atac","Spatial" = "spatial"),
                                              multiple = FALSE),
                              selectizeInput("cross_time_method_choice", "Select Methods (min 1):",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = list(placeholder = 'Select methods...', plugins = list('remove_button'))
                              ),      
                              uiOutput("cross_time_file_ui"),
                              downloadButton("download_cross_time_demo_csv", "Download Demo CSV"),                                         
                              plotOutput("cross_time_plot", height = "430px")
                            )
                      ),
                      column(6,
                            box(
                              title = "Memory", width = NULL, status = "primary", solidHeader = TRUE,

                              selectizeInput("cross_memory_data_type", "Select Data Type:",
                                              choices = c("RNA+ADT" = "rna_adt","RNA+ATAC" = "rna_atac", "ADT+ATAC"= "adt_atac", "RNA+ADT+ATAC"="rna_adt_atac","Spatial" = "spatial"),
                                              multiple = FALSE),
                              selectizeInput("cross_memory_method_choice", "Select Methods (min 1):",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = list(placeholder = 'Select methods...', plugins = list('remove_button'))
                              ),
                              uiOutput("cross_memory_file_ui"),
                              downloadButton("download_cross_memory_demo_csv", "Download Demo CSV"),                                                
                              plotOutput("cross_memory_plot", height = "430px")
                            )
                      )
                    )
          )
        )
      )
      # tabItem(tabName = "results",
      #         h2("Experiments Results"),
      #         p("Experiments Results")
      # )
    )
  )
)