#----- Load Libraries -----
pk_list = c(
  'shiny', 'shinyjs',  'shinythemes', 'shinypop',
  'tidyverse', 'data.table',
  'stringi', 'skimr',
  'ggdist', 'gghalves', 'grid', 'gridExtra', 'patchwork', 
  'ExpDes'
)
for(i in 1:length(pk_list)){
  if(!requireNamespace(pk_list[i], quietly = T)){
    install.packages(pk_list[i])
    remotes::install_github('dreamRs/shinypop')
  }
  lapply(pk_list[i], require, character.only = T)
}

#----- General Config -----
options(shiny.maxRequestSize = 1 * 1024 ^ 2)
temp_folder = './Temp_Files'
dir.create(temp_folder, recursive = T)
threads_to_use = 1

#----- Functions -----
source(paste0('./File_System/X_na_ij_index.R'))
source(paste0('./File_System/X_outlier_ij_index.R'))
source(paste0('./File_System/X_raincloud_numfeat.R'))

#----- UI -----
ui = fluidPage(
  useShinyjs(),
  uiOutput('page_Start'),
  theme = shinythemes::shinytheme('slate'),
  use_notiflix_notify(position = 'right-top', timeout = 5000, closeButton = T, width = '20%'),
  use_notiflix_report(),
)

#---- Server ----
server = function(input, output, session){
  #----- Clean temp folder when close/stop -----
  onStop(function()
    unlink(temp_folder, recursive = T)
  )
  onStop(function()
    unlink('Rplots.pdf')
  )
  
  #----- Main Page -----
  output$page_Start = renderUI({
    navbarPage(
      id = 'MainTabs', title = 'Correlation', fluid = T, collapsible = F, 
      theme = shinythemes::shinytheme('slate'),
      
      tabPanel('Statistic Test',  uiOutput('page_StatisticTest'))
    )
  })
  
  #----- ST - Correlation -----
  rv_st_correlation = reactiveValues(data = NULL)
  observe({
    req(input$load_data_st_correlation)
    rv_st_correlation$data = fread(input$load_data_st_correlation$datapath)
  })
  
  #----- Main Page - Correlation -----
  output$page_StatisticTest = renderUI({
    sidebarLayout(
      sidebarPanel(
        #----- Panel Left -----
        div(style = "margin-left: 5%; margin-right: 5%; display: inline-block;
            width: 90%; text-align: center;",
        ),
        width = 3,
        #----- Slots -----
        uiOutput('Statistic_Test_slot_2'),
        uiOutput('Statistic_Test_Load_Data'), uiOutput('Statistic_Test_slot_4'),
        uiOutput('Statistic_Test_slot_5'), uiOutput('Statistic_Test_slot_6'),
        uiOutput('Statistic_Test_slot_7'), uiOutput('Statistic_Test_Run_Clear_Buttons'),
        #----- Notification -----
        tags$head(
          tags$style(
            '.shiny-notification {position: fixed;top:30%;left:50%} '
          )
        )
      ),
      #----- Main Panel -----
      mainPanel(
        tags$style(
          HTML('.dataTables_wrapper .dataTables_length,
              .dataTables_wrapper .dataTables_filter,
              .dataTables_wrapper .dataTables_info,
              .dataTables_wrapper .dataTables_processing,
              .dataTables_wrapper .dataTables_paginate {color: white;}
              thead {color: white;}
              tbody {color: white;}'
          )
        ),
        #----- Output Panel -----
        uiOutput('Statistic_Test_panel'),
        fluid = T
      )
    )
  })
  
  #----- Statistic_Test_slot_2: Type -----
  output$Statistic_Test_slot_2 = renderUI({
    #----- Correlation type -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
            width: 98%; text-align: center; margin-top: -10%;',
        selectInput(
          inputId = 'type_st', label = NULL,
            choices = list(
              'Pearson',
              'Kendall',
              'Spearman'
            ),
          selected = 'Pearson'
        )
    )
  })
  
  #----- Statistic_Test_Load_Data -----
  output$Statistic_Test_Load_Data = renderUI({
    #----- Correlation -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
        width: 98%; text-align: center; margin-top: 0%; margin-botton: - 15%;',
        fileInput(
          inputId = 'load_data_st_correlation', label = NULL, placeholder = 'Upload your csv file',
          accept = '.csv', multiple = F
        )
    )
  })
  
  #----- Statistic_Test_slot_4: Example file -----
  output$Statistic_Test_slot_4 = renderUI({
    #----- Correlation -----
    Example_File_st_correlation = fread(paste0('./Example_Files/st_correlation.csv'), header = T, sep = ',')
    output$correlation_downloadData = downloadHandler(
      filename = 'File_Example.csv',
      content = function(file){
        write.csv(Example_File_st_correlation, file, row.names = F, quote = F)
      }
    )
    tags$hr()
    div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
        width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
        downloadLink(
          outputId = 'correlation_downloadData',
          label = tags$span(style = 'color:green', 'File_Example.csv')
        ),
        tags$br(),tags$br()
    )
  })
  
  #----- Statistic_Test_slot_5: Report Outliers -----
  output$Statistic_Test_slot_5 = renderUI({
    #----- Correlation -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
      width: 98%; text-align: center; margin-top: -2%;',
        sliderInput(
          inputId = 'outlier_iqr_st_correlation', label = 'Outliers [IQR rate]',
          min = 0.25, max = 2.5, value = 1.5, step = 0.25, ticks = F
        )
    )
  })
  #----- Statistic_Test_slot_6 -----
  output$Statistic_Test_slot_6 = renderUI({
    #----- Correlation - CutOff -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
      width: 98%; text-align: center; margin-top: -2%;',
        sliderInput(
          inputId = 'cutoff_st_correlation', label = 'Correlation CutOff',
          min = 0.05, max = 1, value = 0.7, step = 0.05, ticks = F
        )
    )
  })
  
  #----- Statistic_Test_slot_7 -----
  output$Statistic_Test_slot_7 = renderUI({
    # Empty slot
  })
  
  #----- Statistic_Test_Run_Clear_Buttons -----
  output$Statistic_Test_Run_Clear_Buttons = renderUI({
    #----- Correlation -----
    sidebarLayout(
      div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
        width: 40%; text-align: center; margin-top: -5%;',
          actionButton(
            inputId = 'run_st_correlation', label = 'Run', style = 'width: 100%;'
          )
      ),
      div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
        width: 40%; text-align: center; margin-top: -5%; margin-botton: 5%;',
          actionButton(
            inputId = 'clear_st_correlation', label = 'Clear', style = 'width: 100%;',
            disabled = T
          )
      )
    )
  })
  
  #----- Statistic Test - Panel Tabs -----
  output$Statistic_Test_panel = renderUI({
    #----- Correlation -----
    tabsetPanel(
      tabPanel('Input Data', DT::dataTableOutput('Tab1_st_correlation')),
      tabPanel('Summary', verbatimTextOutput('Tab2_st_correlation')),
      tabPanel('Results', verbatimTextOutput('Tab3_st_correlation')),
      tabPanel('Data Distribution', plotOutput('Tab4_st_correlation')),
      tabPanel('Outlier Report', DT::dataTableOutput('Tab5_st_correlation'))
    )
  })
  
  #----- Clear Correlation Script -----
  observeEvent(
    input$clear_st_correlation, {
      #----- Delete files -----
      fs = list.files(temp_folder, full.names = T, pattern = 'st_correlation_.+')
      if(length(fs) != 0){
        unlink(fs)
      }
      
      #----- Reset Box Data Input -----
      reset('load_data_st_correlation')
      rv_st_correlation$data = NULL
      
      #----- Clear Input Data Tab -----
      output$Tab1_st_correlation = DT::renderDataTable({
        NULL
      })
      
      #----- Clear Summary Tab -----
      output$Tab2_st_correlation = renderText({
        NULL
      })
      
      #----- Clear Results Tab -----
      output$Tab3_st_correlation = renderText({
        NULL
      })
      
      #----- Clear Data Distribution Tab -----
      output$Tab4_st_correlation = renderPlot({
        NULL
      })
      
      #----- Clear Outlier Report Tab -----
      output$Tab5_st_correlation = DT::renderDataTable({
        NULL
      })
      
      #----- Statistic_Test_slot_5: Report Outliers -----
      output$Statistic_Test_slot_5 = renderUI({
        #----- Correlation -----
        div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
          width: 98%; text-align: center; margin-top: -2%;',
            sliderInput(
              inputId = 'outlier_iqr_st_correlation', label = 'Outliers [IQR rate]',
              min = 0.25, max = 2.5, value = 1.5, step = 0.25, ticks = F
            )
        )
      })
      
      #----- Statistic_Test_slot_6 -----
      output$Statistic_Test_slot_6 = renderUI({
        #----- Correlation - CutOff -----
        div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
          width: 98%; text-align: center; margin-top: -2%;',
            sliderInput(
              inputId = 'cutoff_st_correlation', label = 'Correlation CutOff',
              min = 0.05, max = 1, value = 0.7, step = 0.05, ticks = F
            )
        )
      })
      
      #----- Statistic_Test_slot_7 -----
      output$Statistic_Test_slot_7 = renderUI({
        # Empty slot
      })
      
      nx_report_warning('Reloading', 'All data and analysis was deleted!')
    }
  )
  
  #----- Run Correlation Script -----
  observeEvent(
    input$run_st_correlation, {
      # After click 'RUN', disable options
      observeEvent(input$run_st_correlation, {
        if(!is.null(rv_st_correlation$data)){
          shinyjs::disable('run_st_correlation')
          shinyjs::enable('clear_st_correlation')
        }
      })
      # After click 'CLEAR', enable options
      observeEvent(input$clear_st_correlation, {
        shinyjs::enable('run_st_correlation')
        shinyjs::disable('clear_st_correlation')
      })
      
      # Check Correlation option
      
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_correlation$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_correlation$data) == 1){
                rv_st_correlation$data = fread(input$load_data_st_correlation$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_correlation$data) == 1){
                rv_st_correlation$data = fread(input$load_data_st_correlation$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_correlation$data)
              
              # Check dataset structure p1 >>>>>>> for data with 3 columns! <<<<<<<<
              if(ncol(InputTab) != 3){
                nx_report_error('File Error!', 'Your dataset do not have 3 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 3]))
                status_treat = unique(InputTab[, 2])
                
                InputTab = InputTab %>%
                  spread(names(InputTab)[2], names(InputTab)[3])
                InputTab = as.data.frame(InputTab)
                InputTab_2 = InputTab[, -1]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab
                    })
                    
                    output$Tab1_st_correlation = DT::renderDataTable({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          dt_input_data()
                        }
                      )
                    }, rownames = F,
                    options = list(
                      pageLength = 9, searching = F, lengthChange = F,
                      columnDefs = list(
                        list(
                          targets = '_all', className = 'dt-center'
                        )
                      )
                    )
                    )
                    
                    #>---- EDA - Summary -----
                    r1 = skimr::skim(InputTab_2)
                    sink(paste0(temp_folder, '/st_correlation_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_correlation = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      round(cor(InputTab_2, use = 'pairwise.complete.obs', method = stri_trans_tolower(input$type_st)), 2)
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_correlation_results.txt'))
                    })
                    
                    output$Tab3_st_correlation = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_2
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_correlation_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_correlation = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA, Outlier detection and High Correlation) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_2, input$outlier_iqr_st_correlation)

                      Cor_Matrix = cor(InputTab_2, use = 'pairwise.complete.obs', method = stri_trans_tolower(input$type_st))

                      HCF = caret::findCorrelation(Cor_Matrix, cutoff = input$cutoff_st_correlation, exact = F, names = T)
                      if(length(HCF) != 0){
                        Detection_C = data.frame(Instance = 0, Feature = HCF, Type = 'HighCorrFeat')
                        Detection = bind_rows(Detection, Detection_C)
                      }
                      
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_correlation_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_correlation = DT::renderDataTable({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          Detection_Tab()
                        }
                      )
                    }, rownames = F,
                    options = list(
                      pageLength = 9, searching = F, lengthChange = F,
                      columnDefs = list(
                        list(
                          targets = '_all', className = 'dt-center'
                        )
                      )
                    )
                    )
                    
                    #----- Finish -----
                    nx_report_success('Success!', 'All tasks completed!.')
                  }else{
                    nx_report_error('Treatment Error!', 'Treatment column have just 1 level! See the File Example (You can use it as template).')
                  }
                }else{
                  nx_report_error('Response Error!', 'Response column is not numeric data! See the File Example (You can use it as template).')
                }
              }
              
            }
            
          }
        )
        
      
      
         
    }
  )
  
}

#----- Run App -----
shinyApp(
  ui, 
  server,
  onStart = function() {
    cat("Correlation setup\n")
    
    
    onStop(function() {
      cat("Correlation stoped!\n")
    })
  }
  
)
