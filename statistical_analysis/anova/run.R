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
      id = 'MainTabs', title = 'ANOVA', fluid = T, collapsible = F, 
      theme = shinythemes::shinytheme('slate'),
      
      tabPanel('Statistic Test',  uiOutput('page_StatisticTest'))
    )
  })
  
  #----- ST - ANOVA -----
  rv_st_anova = reactiveValues(data = NULL)
  observe({
    req(input$load_data_st_anova)
    rv_st_anova$data = fread(input$load_data_st_anova$datapath)
  })
  
  #----- Main Page - ANOVA -----
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
    #----- ANOVA type -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
            width: 98%; text-align: center; margin-top: -10%;',
        selectInput(
          inputId = 'type_st', label = NULL,
          choices = list(
            'CRD - One Factor',
            'CRD - Double Factorial',
            'RBD - One Factor',
            'RBD - Double Factorial',
            'Split-plots in CRD',
            'Split-plots in RBD',
            'Latin Square'
          ),
          selected = 'CRD - One Factor'
        )
    )
  })
  
  #----- Statistic_Test_Load_Data -----
  output$Statistic_Test_Load_Data = renderUI({
    #----- ANOVA -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
        width: 98%; text-align: center; margin-top: -5%; margin-botton: - 15%;',
        fileInput(
          inputId = 'load_data_st_anova', label = NULL, placeholder = 'Upload your csv file',
          accept = '.csv', multiple = F
        )
    )
  })
  
  #----- Statistic_Test_slot_4: Example file -----
  output$Statistic_Test_slot_4 = renderUI({
    #----- ANOVA -----
    #----- CRD - One Factor -----
    if(input$type_st == 'CRD - One Factor'){
      Example_File_st_anova_crd_one_factor = fread(paste0('./Example_Files/st_anova_crd_one_factor.csv'))
      output$anova_download_data = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_crd_one_factor, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_download_data',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
    #----- CRD - Double Factorial -----
    else if(input$type_st == 'CRD - Double Factorial'){
      Example_File_st_anova_crd_double_factorial = fread(paste0('./Example_Files/st_anova_crd_double_factorial.csv'))
      output$anova_downloadData = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_crd_double_factorial, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_downloadData',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
    #----- RBD - One Factor -----
    else if(input$type_st == 'RBD - One Factor'){
      Example_File_st_anova_rbd_one_factor = fread(paste0('./Example_Files/st_anova_rbd_one_factor.csv'))
      output$anova_download_data = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_rbd_one_factor, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_download_data',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
    #----- RBD - Double Factorial -----
    else if(input$type_st == 'RBD - Double Factorial'){
      Example_File_st_anova_rbd_double_factorial = fread(paste0('./Example_Files/st_anova_rbd_double_factorial.csv'))
      output$anova_download_data = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_rbd_double_factorial, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_download_data',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
    #----- Split-plots in CRD -----
    else if(input$type_st == 'Split-plots in CRD'){
      Example_File_st_anova_split_plots_in_crd = fread(paste0('./Example_Files/st_anova_split_plots_in_crd.csv'))
      output$anova_download_data = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_split_plots_in_crd, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_download_data',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
    #----- Split-plots in RBD -----
    else if(input$type_st == 'Split-plots in RBD'){
      Example_File_st_anova_split_plots_in_rbd = fread(paste0('./Example_Files/st_anova_split_plots_in_rbd.csv'))
      output$anova_download_data = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_split_plots_in_rbd, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_download_data',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
    #----- Latin Square -----
    else if(input$type_st == 'Latin Square'){
      Example_File_st_anova_latin_square = fread(paste0('./Example_Files/st_anova_latin_square.csv'))
      output$anova_download_data = downloadHandler(
        filename = 'File_Example.csv',
        content = function(file){
          write.csv(Example_File_st_anova_latin_square, file, row.names = F, quote = F)
        }
      )
      tags$hr()
      div(style = 'margin-left: 10%; margin-right: 10%; display: inline-block;
          width: 80%; text-align: center; color: green; margin-top: -20%; margin-botton: -15%;',
          downloadLink(
            outputId = 'anova_download_data',
            label = tags$span(style = 'color:green', 'File_Example.csv')
          ),
          tags$br(),tags$br()
      )
    }
  })
  
  #----- Statistic_Test_slot_5: Report Outliers -----
  output$Statistic_Test_slot_5 = renderUI({
    #----- ANOVA -----
    div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
      width: 98%; text-align: center; margin-top: -2%;',
        sliderInput(
          inputId = 'outlier_iqr_st_anova', label = 'Outliers [IQR rate]',
          min = 0.25, max = 2.5, value = 1.5, step = 0.25, ticks = F
        )
    )
  })
  #----- Statistic_Test_slot_6 -----
  output$Statistic_Test_slot_6 = renderUI({
    #----- ANOVA - T_Signif/F_Signif -----
    sidebarLayout(
      div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
        width: 40%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'sigT_st_anova', label = 'T-Test Significance',
            choices = list(
              '0.01' = 0.01, '0.025' = 0.025, '0.05' = 0.05, '0.10' = 0.10
            ), selected = '0.05'
          )
      ),
      div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
        width: 40%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'sigF_st_anova', label = 'F-Test Significance',
            choices = list(
              '0.01' = 0.01, '0.025' = 0.025, '0.05' = 0.05, '0.10' = 0.10
            ), selected = '0.05'
          )
      )
    )
  })
  
  #----- Statistic_Test_slot_7 -----
  output$Statistic_Test_slot_7 = renderUI({
    #----- ANOVA - MultComp/Homocedasticity -----
    #----- CRD - One Factor -----
    if(input$type_st == 'CRD - One Factor'){
      sidebarLayout(
        div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
          width: 40%; text-align: center; margin-top: -2%;',
            selectInput(
              inputId = 'mcomp_st_anova', label = 'Multi-Comp',
              choices = list(
                'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
              ), selected = 'Tukey'
            )
        ),
        div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
          width: 40%; text-align: center; margin-top: -2%;',
            selectInput(
              inputId = 'homoced_st_anova', label = 'Homocedasticity',
              choices = list(
                'Bartlett', 'Levene', 'Samiuddin'
              ), selected = 'Bartlett'
            )
        )
      )
    }
    
    #----- CRD - Double Factorial -----
    else if(input$type_st == 'CRD - Double Factorial'){
      div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'mcomp_st_anova', label = 'Multi-Comp',
            choices = list(
              'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
            ), selected = 'Tukey'
          )
      )
    }
    
    #----- RBD - One Factor -----
    else if(input$type_st == 'RBD - One Factor'){
      sidebarLayout(
        div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
          width: 30%; text-align: center; margin-top: -2%;',
            selectInput(
              inputId = 'mcomp_st_anova', label = 'Multi-Comp',
              choices = list(
                'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
              ), selected = 'Tukey'
            )
        ),
        div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
          width: 50%; text-align: center; margin-top: -2%;',
            selectInput(
              inputId = 'homoced_st_anova', label = 'Homocedasticity',
              choices = list(
                'ONeillMathews', 'AnscombeTukey'
              ), selected = 'ONeillMathews'
            )
        )
      )
    }
    
    #----- RBD - Double Factorial -----
    else if(input$type_st == 'RBD - Double Factorial'){
      div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'mcomp_st_anova', label = 'Multi-Comp',
            choices = list(
              'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
            ), selected = 'Tukey'
          )
      )
    }
    #----- Split-plots in CRD -----
    else if(input$type_st == 'Split-plots in CRD'){
      div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'mcomp_st_anova', label = 'Multi-Comp',
            choices = list(
              'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
            ), selected = 'Tukey'
          )
      )
    }
    #----- Split-plots in RBD -----
    else if(input$type_st == 'Split-plots in RBD'){
      div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'mcomp_st_anova', label = 'Multi-Comp',
            choices = list(
              'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
            ), selected = 'Tukey'
          )
      )
    }
    
    #----- Latin Square -----
    else if(input$type_st == 'Latin Square'){
      div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
          selectInput(
            inputId = 'mcomp_st_anova', label = 'Multi-Comp',
            choices = list(
              'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
            ), selected = 'Tukey'
          )
      )
    }
  })
  
  #----- Statistic_Test_Run_Clear_Buttons -----
  output$Statistic_Test_Run_Clear_Buttons = renderUI({
    #----- ANOVA -----
    sidebarLayout(
      div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
        width: 40%; text-align: center; margin-top: -5%;',
          actionButton(
            inputId = 'run_st_anova', label = 'Run', style = 'width: 100%;'
          )
      ),
      div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
        width: 40%; text-align: center; margin-top: -5%; margin-botton: 5%;',
          actionButton(
            inputId = 'clear_st_anova', label = 'Clear', style = 'width: 100%;',
            disabled = T
          )
      )
    )
  })
  
  #----- Statistic Test - Panel Tabs -----
  output$Statistic_Test_panel = renderUI({
    #----- ANOVA -----
    tabsetPanel(
      tabPanel('Input Data', DT::dataTableOutput('Tab1_st_anova')),
      tabPanel('Summary', verbatimTextOutput('Tab2_st_anova')),
      tabPanel('Results', verbatimTextOutput('Tab3_st_anova')),
      tabPanel('Data Distribution', plotOutput('Tab4_st_anova')),
      tabPanel('Outlier Report', DT::dataTableOutput('Tab5_st_anova'))
    )
  })
  
  #----- Clear ANOVA Script -----
  observeEvent(
    input$clear_st_anova, {
      #----- Delete files -----
      fs = list.files(temp_folder, full.names = T, pattern = 'st_anova_.+')
      if(length(fs) != 0){
        unlink(fs)
      }
      
      #----- Reset Box Data Input -----
      reset('load_data_st_anova')
      rv_st_anova$data = NULL
      
      #----- Clear Input Data Tab -----
      output$Tab1_st_anova = DT::renderDataTable({
        NULL
      })
      
      #----- Clear Summary Tab -----
      output$Tab2_st_anova = renderText({
        NULL
      })
      
      #----- Clear Results Tab -----
      output$Tab3_st_anova = renderText({
        NULL
      })
      
      #----- Clear Data Distribution Tab -----
      output$Tab4_st_anova = renderPlot({
        NULL
      })
      
      #----- Clear Outlier Report Tab -----
      output$Tab5_st_anova = DT::renderDataTable({
        NULL
      })
      
      #----- Statistic_Test_slot_5: Report Outliers -----
      output$Statistic_Test_slot_5 = renderUI({
        #----- ANOVA -----
        div(style = 'margin-left: 1%; margin-right: 1%; display: inline-block;
          width: 98%; text-align: center; margin-top: -2%;',
            sliderInput(
              inputId = 'outlier_iqr_st_anova', label = 'Outliers [IQR rate]',
              min = 0.25, max = 2.5, value = 1.5, step = 0.25, ticks = F
            )
        )
      })
      
      #----- Statistic_Test_slot_6 -----
      output$Statistic_Test_slot_6 = renderUI({
        #----- ANOVA - T_Signif/ F_Signif -----
        sidebarLayout(
          div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
            width: 40%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'sigT_st_anova', label = 'T-Test Significance',
                choices = list(
                  '0.01' = 0.01, '0.025' = 0.025, '0.05' = 0.05, '0.10' = 0.10
                ), selected = '0.05'
              )
          ),
          div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
            width: 40%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'sigF_st_anova', label = 'F-Test Significance',
                choices = list(
                  '0.01' = 0.01, '0.025' = 0.025, '0.05' = 0.05, '0.10' = 0.10
                ), selected = '0.05'
              )
          )
        )
      })
      
      #----- Statistic_Test_slot_7 -----
      output$Statistic_Test_slot_7 = renderUI({
        #----- ANOVA - MultComp/Homocedasticity -----
        #----- CRD - One Factor -----
        if(input$type_st == 'CRD - One Factor'){
          sidebarLayout(
            div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
          width: 40%; text-align: center; margin-top: -2%;',
                selectInput(
                  inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                  choices = list(
                    'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                  ), selected = 'Tukey'
                )
            ),
            div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
          width: 40%; text-align: center; margin-top: -2%;',
                selectInput(
                  inputId = 'homoced_st_anova', label = 'Homocedasticity',
                  choices = list(
                    'Bartlett', 'Levene', 'Samiuddin', 'ONeillMathews'
                  ), selected = 'Bartlett'
                )
            )
          )
        }
        
        #----- CRD - Double Factorial -----
        else if(input$type_st == 'CRD - Double Factorial'){
          div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                choices = list(
                  'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                ), selected = 'Tukey'
              )
          )
        }
        
        #----- RBD - One Factor -----
        else if(input$type_st == 'RBD - One Factor'){
          sidebarLayout(
            div(style = 'margin-left: 5%; margin-right: 4%; display: inline-block;
          width: 30%; text-align: center; margin-top: -2%;',
                selectInput(
                  inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                  choices = list(
                    'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                  ), selected = 'Tukey'
                )
            ),
            div(style = 'margin-left: 4%; margin-right: 5%; display: inline-block;
          width: 50%; text-align: center; margin-top: -2%;',
                selectInput(
                  inputId = 'homoced_st_anova', label = 'Homocedasticity',
                  choices = list(
                    'ONeillMathews',  'AnscombeTukey'
                  ), selected = 'ONeillMathews'
                )
            )
          )
        }
        
        #----- RBD - Double Factorial -----
        else if(input$type_st == 'RBD - Double Factorial'){
          div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                choices = list(
                  'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                ), selected = 'Tukey'
              )
          )
        }
        #----- Split-plots in CRD -----
        else if(input$type_st == 'Split-plots in CRD'){
          div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                choices = list(
                  'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                ), selected = 'Tukey'
              )
          )
        }
        #----- Split-plots in RBD -----
        else if(input$type_st == 'Split-plots in RBD'){
          div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                choices = list(
                  'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                ), selected = 'Tukey'
              )
          )
        }
        
        #----- Latin Square -----
        else if(input$type_st == 'Latin Square'){
          div(style = 'margin-left: 5%; margin-right: 5%; display: inline-block;
        width: 90%; text-align: center; margin-top: -2%;',
              selectInput(
                inputId = 'mcomp_st_anova', label = 'Multi-Comp',
                choices = list(
                  'Duncan', 'LSD', 'LSDB', 'SK', 'SNK', 'Tukey', 'CCBoot'
                ), selected = 'Tukey'
              )
          )
        }
      })
      
      nx_report_warning('Reloading', 'All data and analysis was deleted!')
    }
  )
  
  #----- Run ANOVA Script -----
  observeEvent(
    input$run_st_anova, {
      # After click 'RUN', disable options
      observeEvent(input$run_st_anova, {
        shinyjs::disable('type_st')
        shinyjs::disable('run_st_anova')
        shinyjs::enable('clear_st_anova')
      })
      # After click 'CLEAR', enable options
      observeEvent(input$clear_st_anova, {
        shinyjs::enable('type_st')
        shinyjs::enable('run_st_anova')
        shinyjs::disable('clear_st_anova')
      })
      
      # Check Anova option
      #----- CRD - One Factor -----
      if(input$type_st == 'CRD - One Factor'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 3 columns! <<<<<<<<
              if(ncol(InputTab) != 3){
                nx_report_error('File Error!', 'Your dataset do not have 3 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 3]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                status_treat = unique(InputTab[, 2])
                
                InputTab_2 = InputTab %>%
                  spread(names(InputTab)[2], names(InputTab)[3])
                
                InputTab_3 = as.data.frame(InputTab_2[, -1])
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::crd(InputTab[, 2], InputTab[, 3], quali = T, nl = F,
                                  sigT = as.numeric(input$sigT_st_anova),
                                  sigF = as.numeric(input$sigF_st_anova),
                                  mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova),
                                  hvar = stringi::stri_trans_tolower(input$homoced_st_anova)
                      )
                    })
                    
                    
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          Detection_Tab()[,c(1:2)]
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
      
      #----- CRD - Double Factorial -----
      else if(input$type_st == 'CRD - Double Factorial'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 3 columns! <<<<<<<<
              if(ncol(InputTab) != 4){
                nx_report_error('File Error!', 'Your dataset do not have 4 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 4]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                InputTab[, 3] = as.factor(InputTab[, 3])
                status_treat_1 = unique(InputTab[, 2])
                status_treat_2 = unique(InputTab[, 3])
                
                TempTab = InputTab %>%
                  spread(names(InputTab)[2], names(InputTab)[4])
                
                InputTab_2 = data.frame(Repet = unique(TempTab[, 1]))
                for(i in 1:length(status_treat_2)){
                  Temp_New_Tab = subset(TempTab, TempTab[, 2] == status_treat_2[i])[, -c(1,2)]
                  names(Temp_New_Tab) = stri_join(names(Temp_New_Tab), status_treat_2[i])
                  InputTab_2 = bind_cols(InputTab_2, Temp_New_Tab)
                }
                
                InputTab_3 = InputTab_2[, -1]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat_1) > 1 & length(status_treat_2) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::fat2.crd(InputTab[, 2], InputTab[, 3], InputTab[, 4], quali = c(T, T), 
                                       sigT = as.numeric(input$sigT_st_anova), 
                                       sigF = as.numeric(input$sigF_st_anova), 
                                       mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova), 
                                       fac.names = c(names(InputTab)[2], names(InputTab)[3])
                                       
                      )
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_anova_results.txt'))
                    })
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
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
                    nx_report_error('Treatment Error!', 'Some treatment column have just 1 level! See the File Example (You can use it as template).')
                  }
                }else{
                  nx_report_error('Response Error!', 'Response column is not numeric data! See the File Example (You can use it as template).')
                }
              }
              
            }
            
          }
        )
        
      }
      
      #----- RBD - One Factor -----
      else if(input$type_st == 'RBD - One Factor'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 4 columns! <<<<<<<<
              if(ncol(InputTab) != 4){
                nx_report_error('File Error!', 'Your dataset do not have 4 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 4]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                InputTab[, 3] = as.factor(InputTab[, 3])
                status_block = unique(InputTab[, 2])
                status_treat_1 = unique(InputTab[, 3])
                
                TempTab = InputTab %>%
                  spread(names(InputTab)[2], names(InputTab)[4])
                
                InputTab_2 = data.frame(Repet = unique(TempTab[, 1]))
                for(i in 1:length(status_treat_1)){
                  Temp_New_Tab = subset(TempTab, TempTab[, 2] == status_treat_1[i])[, -c(1,2)]
                  names(Temp_New_Tab) = stri_join(names(Temp_New_Tab), status_treat_1[i])
                  InputTab_2 = bind_cols(InputTab_2, Temp_New_Tab)
                }
                
                InputTab_3 = InputTab_2[, -1]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_block) > 1 & length(status_treat_2) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::rbd(InputTab[, 2], InputTab[, 3], InputTab[, 4],
                                  quali = T, nl = F,
                                  sigT = as.numeric(input$sigT_st_anova),
                                  sigF = as.numeric(input$sigF_st_anova),
                                  mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova),
                                  hvar = stringi::stri_trans_tolower(input$homoced_st_anova)
                      )
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_anova_results.txt'))
                    })
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
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
                    nx_report_error('Treatment Error!', 'Treatment or Block column have just 1 level! See the File Example (You can use it as template).')
                  }
                }else{
                  nx_report_error('Response Error!', 'Response column is not numeric data! See the File Example (You can use it as template).')
                }
              }
              
            }
            
          }
        )
        
      }
      
      #----- RBD - Double Factorial -----
      else if(input$type_st == 'RBD - Double Factorial'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 5 columns! <<<<<<<<
              if(ncol(InputTab) != 5){
                nx_report_error('File Error!', 'Your dataset do not have 5 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 5]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                InputTab[, 3] = as.factor(InputTab[, 3])
                InputTab[, 4] = as.factor(InputTab[, 4])
                status_block = unique(InputTab[, 2])
                status_treat_1 = unique(InputTab[, 3])
                status_treat_2 = unique(InputTab[, 4])
                
                
                TempTab = InputTab %>%
                  spread(names(InputTab)[3], names(InputTab)[5])
                
                InputTab_2 = data.frame(Repet = rep(unique(TempTab[, 1]), each = length(status_block)), Bl = rep(c(1,2), length(unique(TempTab[, 1]))))
                for(i in 1:length(status_treat_2)){
                  Temp_New_Tab = subset(TempTab, TempTab[, 3] == status_treat_2[i])[, -c(1:3)]
                  names(Temp_New_Tab) = stri_join(names(Temp_New_Tab), status_treat_2[i])
                  InputTab_2 = bind_cols(InputTab_2, Temp_New_Tab)
                }
                InputTab_2 = InputTab_2[order(InputTab_2[, 2]), ]
                
                InputTab_3 = InputTab_2[, -1]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat_1) > 1 & length(status_treat_2) > 1 & length(status_block) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::fat2.rbd(InputTab[, 3], InputTab[, 4], InputTab[, 2], InputTab[, 5], quali = c(T, T),
                                       sigT = as.numeric(input$sigT_st_anova),
                                       sigF = as.numeric(input$sigF_st_anova),
                                       mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova),
                                       c(names(InputTab)[3], names(InputTab)[4])
                      )
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_anova_results.txt'))
                    })
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
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
                    nx_report_error('Treatment Error!', 'Some treatment or block column have just 1 level! See the File Example (You can use it as template).')
                  }
                }else{
                  nx_report_error('Response Error!', 'Response column is not numeric data! See the File Example (You can use it as template).')
                }
              }
              
            }
            
          }
        )
        
      }
      
      #----- Split-plots in CRD -----
      else if(input$type_st == 'Split-plots in CRD'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 4 columns! <<<<<<<<
              if(ncol(InputTab) != 4){
                nx_report_error('File Error!', 'Your dataset do not have 4 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 4]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                InputTab[, 3] = as.factor(InputTab[, 3])
                status_treat_1 = unique(InputTab[, 2])
                status_treat_2 = unique(InputTab[, 3])
                
                TempTab = InputTab %>%
                  spread(names(InputTab)[2], names(InputTab)[4])
                
                InputTab_2 = data.frame(Repet = rep(unique(TempTab[, 1])))
                for(i in 1:length(status_treat_2)){
                  Temp_New_Tab = subset(TempTab, TempTab[, 2] == status_treat_2[i])[, -c(1:2)]
                  names(Temp_New_Tab) = stri_join(names(Temp_New_Tab), status_treat_2[i])
                  InputTab_2 = bind_cols(InputTab_2, Temp_New_Tab)
                }
                
                InputTab_3 = InputTab_2[, -1]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat_1) > 1 & length(status_treat_2) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::split2.crd(InputTab[, 2], InputTab[, 3], InputTab[, 1], InputTab[, 4], quali = c(T, T),
                                         sigT = as.numeric(input$sigT_st_anova),
                                         sigF = as.numeric(input$sigF_st_anova),
                                         mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova),
                                         c(names(InputTab)[3], names(InputTab)[4])
                      )
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_anova_results.txt'))
                    })
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
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
                    nx_report_error('Treatment Error!', 'Some treatment column have just 1 level! See the File Example (You can use it as template).')
                  }
                }else{
                  nx_report_error('Response Error!', 'Response column is not numeric data! See the File Example (You can use it as template).')
                }
              }
              
            }
            
          }
        )
        
      }
      
      #----- Split-plots in RBD -----
      else if(input$type_st == 'Split-plots in RBD'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 5 columns! <<<<<<<<
              if(ncol(InputTab) != 5){
                nx_report_error('File Error!', 'Your dataset do not have 5 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 5]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                InputTab[, 3] = as.factor(InputTab[, 3])
                InputTab[, 4] = as.factor(InputTab[, 4])
                status_block = unique(InputTab[, 2])
                status_treat_1 = unique(InputTab[, 3])
                status_treat_2 = unique(InputTab[, 4])
                
                
                TempTab = InputTab %>%
                  spread(names(InputTab)[3], names(InputTab)[5])
                
                InputTab_2 = data.frame(Repet = rep(unique(TempTab[, 1]), each = length(status_block)), Bl = rep(c(1,2), length(unique(TempTab[, 1]))))
                for(i in 1:length(status_treat_2)){
                  Temp_New_Tab = subset(TempTab, TempTab[, 3] == status_treat_2[i])[, -c(1:3)]
                  names(Temp_New_Tab) = stri_join(names(Temp_New_Tab), status_treat_2[i])
                  InputTab_2 = bind_cols(InputTab_2, Temp_New_Tab)
                }
                InputTab_2 = InputTab_2[order(InputTab_2[, 2]), ]
                
                InputTab_3 = InputTab_2[, -1]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat_1) > 1 & length(status_treat_2) > 1 & length(status_block) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::split2.rbd(InputTab[, 3], InputTab[, 4], InputTab[, 2], InputTab[, 5], quali = c(T, T),
                                         sigT = as.numeric(input$sigT_st_anova),
                                         sigF = as.numeric(input$sigF_st_anova),
                                         mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova),
                                         c(names(InputTab)[3], names(InputTab)[4])
                      )
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_anova_results.txt'))
                    })
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
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
                    nx_report_error('Treatment Error!', 'Some treatment or block column have just 1 level! See the File Example (You can use it as template).')
                  }
                }else{
                  nx_report_error('Response Error!', 'Response column is not numeric data! See the File Example (You can use it as template).')
                }
              }
              
            }
            
          }
        )
        
      }
      
      #----- Latin Square -----
      else if(input$type_st == 'Latin Square'){
        withProgress(
          message = 'Processing...', value = 0, {
            # Check upload process
            if(is.null(rv_st_anova$data)){
              nx_report_error('File Error!', 'You need upload a CSV file!')
            }else{
              # Read dataset correction
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = ';', nThread = threads_to_use)
              }
              if(ncol(rv_st_anova$data) == 1){
                rv_st_anova$data = fread(input$load_data_st_anova$datapath, header = T, sep = '\t', nThread = threads_to_use)
              }
              InputTab = as.data.frame(rv_st_anova$data)
              
              # Check dataset structure p1 >>>>>>> for data with 5 columns! <<<<<<<<
              if(ncol(InputTab) != 5){
                nx_report_error('File Error!', 'Your dataset do not have 5 columns! See the File Example (You can use it as template).')
              }else{
                status_res = grep(TRUE, is.numeric(InputTab[, 5]))
                InputTab[, 2] = as.factor(InputTab[, 2])
                InputTab[, 3] = as.factor(InputTab[, 3])
                InputTab[, 4] = as.factor(InputTab[, 4])
                status_treat = unique(InputTab[, 2])
                
                InputTab_2 = InputTab %>%
                  spread(names(InputTab)[2], names(InputTab)[5])
                
                InputTab_3 = InputTab_2[, -c(1:3)]
                
                # Check dataset structure p2
                if(length(status_res) == 1){
                  if(length(status_treat) > 1){
                    # Features names to lower and remove whitespace
                    names(InputTab) = stringi::stri_trans_tolower(names(InputTab))
                    names(InputTab) = gsub(' ', '_', names(InputTab))
                    
                    #>---- Data Table: Input data ----
                    dt_input_data = reactive({
                      InputTab_2
                    })
                    
                    output$Tab1_st_anova = DT::renderDataTable({
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
                    r1 = skimr::skim(InputTab_3)
                    sink(paste0(temp_folder, '/st_anova_summary.txt'))
                    print(r1)
                    sink()
                    
                    output$Tab2_st_anova = renderPrint({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          r1
                        }
                      )
                    })
                    
                    #>---- EDA - Results -----
                    results_txt = renderPrint({
                      ExpDes::latsd(InputTab[, 2], InputTab[, 3], InputTab[, 4], InputTab[, 5], quali = T,
                                    sigT = as.numeric(input$sigT_st_anova),
                                    sigF = as.numeric(input$sigF_st_anova),
                                    mcomp = stringi::stri_trans_tolower(input$mcomp_st_anova)
                      )
                    })
                    
                    observe({
                      writeLines(results_txt(), paste0(temp_folder, '/st_anova_results.txt'))
                    })
                    
                    output$Tab3_st_anova = renderText({
                      withProgress(
                        message = 'Processing...', value = 0, {
                          results_txt()
                        }
                      )
                    })
                    
                    #>---- EDA - Data Distribution (RainCloud Plot) ----
                    num_dt = InputTab_3
                    plist = list()
                    for(i in 1:ncol(num_dt)){
                      plist[[i]] = raincloud_numfeat(num_dt, i)
                      plot(plist[[i]])
                      dev.off()
                      pdf(paste0(temp_folder, '/st_anova_', names(num_dt)[i], '_data_distribution.pdf'))
                      plot(plist[[i]])
                      dev.off()
                    }
                    
                    output$Tab4_st_anova = renderPlot(
                      width = 600, height = 400 * (ncol(num_dt) / 2),{
                        withProgress(
                          message = 'Processing...', value = 0, {
                            do.call(grid.arrange, c(plist, ncol = 2))
                          }
                        )
                      })
                    
                    #>---- EDA - Report (NA and Outlier detection) -----
                    Detection_Tab = reactive({
                      Detection = outlier_index(InputTab_3, input$outlier_iqr_st_anova)
                      return(Detection)
                    })
                    
                    observe({
                      fwrite(Detection_Tab(), paste0(temp_folder, '/st_anova_report.csv'), nThread = threads_to_use)
                    })
                    
                    output$Tab5_st_anova = DT::renderDataTable({
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
      
    }
  )
  
}

#----- Run App -----
shinyApp(
  ui, 
  server,
  onStart = function() {
    cat("ANOVA setup\n")
    
    
    onStop(function() {
      cat("ANOVA stoped!\n")
    })
  }
  
)
