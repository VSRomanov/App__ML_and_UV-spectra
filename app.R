#---------------------------------------------------------------
#Shiny_App__Our_UV_device/app.R


#---------------------------------------------------------------


#rm(list = ls()) # cleaning all the variables in the Environment
#.rs.restartR() # cleaning the memory/RAM



all_libraries <- c("shiny", "shinyanimate", "DT", "tidyr", "dplyr", "ChemoSpec", "baseline", 
                   "IDPmisc", "R.utils", "caret", "glmnet")
for (library_name in all_libraries) {
  if(!require(library_name, character.only = T)){
    install.packages(library_name, dependencies = T)
  }
}



library(shiny)
library(shinyanimate)
library(DT)
library(tidyr)  # because of 'pivot_longer' function
library(dplyr)  # because of '%>%' operator
library(ChemoSpec)
library(R.utils)  # because it is necessary for 'ChemoSpec::matrix2SpectraObject()' function
library(baseline)  # because of 'baselineSpectra()' function
library(IDPmisc)  # because of 'rfbaseline' algorithm for the baseline correction ('baselineSpectra()' function)
library(caret)  # because it is necessary for 'best_Machine.RData' file
library(glmnet)  # because it is necessary for 'best_Machine.RData' file



########################  Settings  ######################################################################

## is it version for the server (shinyapps.io) or for the local computer?
#version <- "local_computer"
version <- "server"


# Linux or Windows?
OS <- "linux"
#OS <- "windows"

## Selecting the folder with the Machine to use ('positive'_vs_'negative' normalization samples)
normalization_samples <- "h3_vs_h7"
#normalization_samples <- "h4_vs_h6"
#normalization_samples <- "h4_vs_h7"


## Emissions to consider
#emissions_to_use <- range(300, 900)
#emissions_to_use <- range(300, 400)
emissions_to_use <- range(330, 400)
#emissions_to_use <- range(340, 380)


## Algorithm for the baseline correction
#baseline_method <- "als"
#baseline_method <- "modpolyfit"
#baseline_method <- "linear"
#baseline_method <- "peakDetection"     #Note: Sometimes there are errors with our spectra with this method.
baseline_method <- "rfbaseline"
#baseline_method <- "rollingBall"    #Note: For this method, additional arguments 'wm' & 'ws' (for example: wm=10, ws=6) are needed.

###############################################################################################################


### Import the Machine & settings
load('best_Machine.RData')


if (version == "local_computer") {
  if (OS == "linux") {
    setwd("/home/vasilyromanov/Documents/Shiny_App/Shiny_App__Our_UV_device")
  } else if (OS == "windows") {
    setwd("C:\\Users\\Vasily Romanov\\Desktop\\Shiny_App\\Shiny_App__Our_UV_device")
  }
}

###############################################################################################################



# A function to introduce line breaks in the App
linebreaks <- function(n){HTML(strrep(br(), n))}



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  shinyanimate::withAnim(),

  linebreaks(1),
  
  splitLayout(cellWidths = c(320, 130, 300),
              shiny::tags$img(src = "UKJ_logo.jpg", heigth = 200, width = 200),
              
              # "Demo" button:
              shiny::actionButton(inputId = "go_demo", label = "Demo Mode", 
                                  class = "btn-success", icon("file"), 
                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              shinyjs::hidden(p(id = "text1", "Restart the app to activate the Demo Mode"))),

  
  
  shiny::titlePanel("Testing spectra samples"),
  
  # "Analyse" button
  shiny::actionButton(inputId = "go", label = "Analyse", class = "btn-success",
                      icon("file-upload")),
  
  
  linebreaks(2),
  
  shiny::sidebarLayout(
    # Sidebar panel for the files input ----
    shiny::sidebarPanel(
      shiny::fileInput(inputId = "positive_control",
                       label = "Upload a positive control .csv file", 
                       accept = ".csv"), #multiple = T),
      shiny::fileInput(inputId = "negative_control", 
                       label = "Upload a negative control .csv file", 
                       accept = ".csv"), #multiple = T),
      shiny::fileInput(inputId = "blanks", label = "Upload blank .csv files", 
                       accept = ".csv", multiple = T),
      shiny::fileInput(inputId = "samples", label = "Upload sample .csv files", 
                       accept = ".csv", multiple = T),
      shiny::helpText("Uploading of multiple files does not work on older browsers, including Internet Explorer 9 and earlier")
    ),
    
    shiny::mainPanel(
      shiny::uiOutput("tb")
    )
  )
)




# Define server logic required ----
server <- function(input,output, session){
  
  # This reactive function will take the inputs from UI.R and use them for 
  # read.table() to read the data from the file. It returns the dataset in the 
  # form of a dataframe. file$datapath -> gives the path of the file
  Spectra_data <- shiny::eventReactive(c(input$go, input$go_demo), {
      
    # for the function output:
    spectra <- vector(mode = "list", length = 3)
    
    
    ################# Cleaning and formatting samples to analyse ##############
    
    if (input$go) {
      ### Checking of the uploaded files
      positive_control <- input$positive_control    
      negative_control <- input$negative_control
      blanks <- input$blanks
      samples <- input$samples
      
      shiny::req(positive_control, negative_control, blanks, samples)
      
      if (is.null(positive_control) || is.null(negative_control) ||
         is.null(blanks) || is.null(samples)) {return()}
      
      
      
      ext1 <- tools::file_ext(positive_control$datapath)
      ext2 <- tools::file_ext(negative_control$datapath)
      
      shiny::validate(need(ext1 == "csv", "Please upload a .csv file as the positive control"),
                      need(ext2 == "csv", "Please upload a .csv file as the negative control"))
      
      for (a in 1:base::length(blanks[,1])) {
        ext3 <- tools::file_ext(blanks[[a, 'datapath']])
        shiny::validate(need(ext3 == "csv", "Please upload all blank files as .csv"))
      }    
      for (b in 1:base::length(samples[,1])) {
        ext4 <- tools::file_ext(samples[[b, 'datapath']])
        shiny::validate(need(ext4 == "csv", "Please upload all sample files as .csv"))
      }
      
      
      files.datapath <- c(positive_control$datapath, negative_control$datapath, 
                          base::unlist(blanks[, 'datapath'], recursive = F), 
                          base::unlist(samples[, 'datapath'], recursive = F))
      files.name <- c(positive_control$name, negative_control$name, 
                      base::unlist(blanks[, 'name'], recursive = F), 
                      base::unlist(samples[, 'name'], recursive = F))
      
    } else if (input$go_demo) {
      if (OS == "linux") {
        dir_demo <- 'Demo_mode/'
      } else if (OS == "windows") {
        dir_demo <- 'Demo_mode\\'
      }
      
      positive_control <- list.files(dir_demo, pattern = 'positive_control__', 
                                     full.names = TRUE)
      negative_control <- list.files(dir_demo, pattern = 'negative_control__', 
                                     full.names = TRUE)
      blanks <- list.files(dir_demo, pattern = 'blank__', full.names = TRUE)
      samples <- list.files(dir_demo, pattern = '*.csv$', 
                            full.names = TRUE)[!list.files(dir_demo, pattern = '*.csv$',
                                                           full.names = TRUE) %in% c(positive_control,
                                                                                     negative_control,
                                                                                     blanks)]      
      
      
      files.datapath <- c(positive_control, negative_control, blanks, samples)
    }

    
    ### Iteration through all files to analyse
    for(j in 1:base::length(files.datapath)){
      
      if (input$go) {
        shinyanimate::startAnim(session, 'go', 'flash')
      } else if (input$go_demo) {
        shinyanimate::startAnim(session, 'go_demo', 'flash')
      }
      
      # to find the correct rows to read the table
      skip_rows <- grep("Average Count", readLines(files.datapath[j]))

      rawdata.j <- read.csv2(file=files.datapath[j], skip = skip_rows, header = FALSE)
      base::rownames(rawdata.j) <- rawdata.j[,1]

      
          
      
      ### Note: NOT the conditions for the final dataset:
      # if a column consists only of noise (low counts in the Raman spectrum: 
      # there is a problem with the device sometimes), then remove it:
      rawdata.j <- rawdata.j[,which(base::colSums(rawdata.j[c(1:30),]) > 5000)]
      
      if (ncol(rawdata.j) == 11) {
        base::colnames(rawdata.j) <- c("Emission", "Measurement_01", 
                                       "Measurement_02", "Measurement_03", 
                                       "Measurement_04", "Measurement_05",
                                       "Measurement_06", "Measurement_07", 
                                       "Measurement_08", "Measurement_09",
                                       "Measurement_10")
        
      } else if (ncol(rawdata.j) == 12) {
        base::colnames(rawdata.j) <- c("Emission", "Measurement_00", 
                                       "Measurement_01", "Measurement_02", 
                                       "Measurement_03", "Measurement_04",
                                       "Measurement_05", "Measurement_06", 
                                       "Measurement_07", "Measurement_08",
                                       "Measurement_09", "Measurement_10")
        rawdata.j <- rawdata.j[,-2]
      }
      

      
      
      ### Spectra
      # to create a temporary directory (in a working directory) for 
      # the spectra cleaning:
      if (version == "local_computer") {
        dir_temp <- "Shiny_App_Temp"
#        dir.create(dir_temp)
        
        if (OS == "linux") {
          file_path_precleaned <- paste0(dir_temp, "/Precleaned.csv")
        } else if (OS == "windows") {
          file_path_precleaned <- paste0(dir_temp, "\\Precleaned.csv")
        }
        
      } else if (version == "server") {
        dir_temp <- paste0(session$token, "/Shiny_App_Temp")
        file_path_precleaned <- paste0(dir_temp, "/Precleaned.csv")        
        
        dir.create(session$token)
      }

      dir.create(dir_temp)
      file.create(file_path_precleaned)
      
      write.table(rawdata.j, file = file_path_precleaned, append = FALSE, sep = "\t",
                  dec = ".", row.names = FALSE, col.names = TRUE)
      
      
      
      ssp <- ChemoSpec::matrix2SpectraObject(gr.crit = base::colnames(rawdata.j)[-1],
                                             gr.cols = "Col12", freq.unit = "nm",
                                             int.unit = "counts",
                                             in.file = file_path_precleaned)
      
      
      
      
      ### Baseline drift correction
      ssp_corrected <- ChemoSpec::baselineSpectra(ssp, int = FALSE, 
                                                  method = baseline_method,
                                                  retC = TRUE)
      
      ssp.all_Measurements_together <- t(base::rbind(ssp_corrected$freq,
                                                     ssp_corrected$data))
      base::colnames(ssp.all_Measurements_together) <- base::colnames(rawdata.j)
      
      cumulative_measurements.j <- pivot_longer(as.data.frame(ssp.all_Measurements_together),
                                                cols = c(-Emission),
                                                names_to = "Measurements", 
                                                values_to = "Counts")
      
      # changing features into 'Emission/Measurements' structure
      cumulative_measurements.j <- data.frame(cumulative_measurements.j[,c(1,2)],
                                              apply(cumulative_measurements.j[,c(1,2)],
                                                    1, paste0, collapse = "_/_"),
                                              Counts = cumulative_measurements.j[,3])
      if (input$go) {
        base::colnames(cumulative_measurements.j) <- c("Emission", "Measurement",
                                                       "Feature", 
                                                       paste(basename(files.name[j])))
      } else if (input$go_demo) {
        base::colnames(cumulative_measurements.j) <- c("Emission", "Measurement",
                                                       "Feature", files.datapath[j])
      }
      
      # to keep only the selected Emission range
      cumulative_measurements.j <- cumulative_measurements.j[which(as.numeric(cumulative_measurements.j$Emission) > emissions_to_use[1] 
                                                                   & as.numeric(cumulative_measurements.j$Emission) < emissions_to_use[2]),]
      
      
      # cumulative table of means of all samples
      if (j == 1) {
        ssp_corrected_Measurements.cumulative <- cumulative_measurements.j
      } else {
        ssp_corrected_Measurements.cumulative <- base::cbind(ssp_corrected_Measurements.cumulative,
                                                             cumulative_measurements.j[,4])
        if (input$go) {
          base::colnames(ssp_corrected_Measurements.cumulative)[ncol(ssp_corrected_Measurements.cumulative)] <- paste(basename(files.name[j]))
        } else if (input$go_demo) {
          base::colnames(ssp_corrected_Measurements.cumulative)[ncol(ssp_corrected_Measurements.cumulative)] <- files.datapath[j]
        }
      }
    }
    
    spectra[[1]] <- ssp_corrected_Measurements.cumulative    
    
#    print(head(ssp_corrected_Measurements.cumulative))
#    print(head(colnames(ssp_corrected_Measurements.cumulative) %in% blanks))
    
    
    ### Background Correction, Centered spectra, Variance-scaled spectra
    if (input$go) {
      mean_blank.final_table <- base::rowMeans(ssp_corrected_Measurements.cumulative[colnames(ssp_corrected_Measurements.cumulative) %in% base::unlist(blanks[, 'name'], recursive = F)])
      minus_blank_spectra.final_table <- base::cbind(Feature = ssp_corrected_Measurements.cumulative$Feature,
                                                     ssp_corrected_Measurements.cumulative[,c(-1,-2,-3)] - mean_blank.final_table)
      minus_blank_spectra.final_table <- minus_blank_spectra.final_table[, base::setdiff(base::colnames(minus_blank_spectra.final_table),
                                                                                        base::unlist(blanks[, 'name'], recursive = F))]
      
      calibration_samples <- minus_blank_spectra.final_table[,c(1, which(positive_control$name == base::colnames(minus_blank_spectra.final_table)),
                                                                which(negative_control$name == base::colnames(minus_blank_spectra.final_table)))]
    } else if (input$go_demo) {
      mean_blank.final_table <- base::rowMeans(ssp_corrected_Measurements.cumulative[colnames(ssp_corrected_Measurements.cumulative) %in% blanks])
      minus_blank_spectra.final_table <- base::cbind(Feature = ssp_corrected_Measurements.cumulative$Feature,
                                                     ssp_corrected_Measurements.cumulative[,c(-1,-2,-3)] - mean_blank.final_table)
      minus_blank_spectra.final_table <- minus_blank_spectra.final_table[, base::setdiff(base::colnames(minus_blank_spectra.final_table),
                                                                                        blanks)]
      
      calibration_samples <- minus_blank_spectra.final_table[, c(1, which(positive_control == base::colnames(minus_blank_spectra.final_table)),
                                                                which(negative_control == base::colnames(minus_blank_spectra.final_table)))]
      
    }
    


    # z-score transformation of all features together from only selected samples (of each folder)
    total_calibration_samples <- pivot_longer(calibration_samples, cols = c(-1),
                                              names_to = "samples", 
                                              values_to = "scores") -> z_score_total_calibration_samples

    mean_total_calibration_samples <- base::mean(total_calibration_samples$scores,
                                                 na.rm = TRUE)
    sd_total_calibration_samples <- stats::sd(total_calibration_samples$scores,
                                              na.rm = TRUE)
    
    
    z_scored_spectra <- base::cbind(Feature = as.character(minus_blank_spectra.final_table[,1]),
                              sapply(minus_blank_spectra.final_table[, base::setdiff(base::colnames(minus_blank_spectra.final_table),
                                                                                     base::colnames(calibration_samples))],
                                     function(x){
                                       y <- ((as.numeric(x) - mean_total_calibration_samples) / sd_total_calibration_samples)
                                       return(y)
                                       }))
    
    spectra[[2]] <- z_scored_spectra  
    
    
    
    analyzing_data <- z_scored_spectra
    
    ### Data formating
    analyzing_data_transposed <- as.data.frame(t(apply(analyzing_data[,-1], 2, 
                                                       function(x) as.numeric(as.character(x)))))
    base::colnames(analyzing_data_transposed) <- as.character(analyzing_data[,1])
    data_for_Machine <- base::cbind(Samples=base::colnames(analyzing_data[,-1]),
                                    analyzing_data_transposed)
    
    spectra[[3]] <- data_for_Machine

    
    # to delete the temporary directory
    if (version == "local_computer") {
      unlink(dir_temp, recursive = TRUE)
    } else if (version == "server") {
      unlink(session$token, recursive = TRUE)
    }

    
    return(spectra)
    
  }, ignoreInit = TRUE)
  

  
  
  ## This reactive outputs contain the datasets and display them in the table format:
  output$table <- renderTable({
    if(is.null(Spectra_data()[[1]])){return ()}
    Spectra_data()[[1]]
  })
  

  output$z_scores <- renderTable({
    if(is.null(Spectra_data()[[2]])){return ()}
    Spectra_data()[[2]]
  })
  
  
  output$dataset_for_ML <- renderTable({
    if(is.null(Spectra_data()[[3]])){return ()}
    Spectra_data()[[3]]
  })
 
  
  output$tt <- DT::renderDT({
    if(is.null(Spectra_data())){return ()}
    
    
    
    ################# Testing samples by the Machine ##############
    
    data_for_Machine.predict <- Spectra_data()[[3]]
    data_for_Machine.predict <- data_for_Machine.predict[,-1]
    
    ### Test Set Prediction
    analyzing_test_pred <- predict(best_Machine, newdata = data_for_Machine.predict)
    
    # probability numbers:
    analyzing_test_pred_prob <- predict(best_Machine, newdata = data_for_Machine.predict,
                                        type = "prob")      
    # the result table:
    analyzing_test_pred_prob_table <- data.frame(Sample=Spectra_data()[[3]][,1],
                                                 analyzing_test_pred_prob, 
                                                 Prediction=analyzing_test_pred)
    base::colnames(analyzing_test_pred_prob_table)[2] <- "Probability of \"low\""
    base::colnames(analyzing_test_pred_prob_table)[3] <- "Probability of \"high\""


    analyzing_test_pred_prob_table <- DT::datatable(analyzing_test_pred_prob_table) %>%
      DT::formatRound("Probability of \"low\"", digits = 4) %>%
      DT::formatRound("Probability of \"high\"", digits = 4) %>%
      DT::formatStyle("Prediction", fontWeight = "bold",
                      color = DT::styleEqual(c('low','high'), c('green', 'red')))
    
    
    analyzing_test_pred_prob_table
  })
  

  
  
  # The following renderUI is used to dynamically generate the tabsets when 
  # the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(Spectra_data()))
      shiny::tags$img(src = "UKJ_logo.jpg", heigth = 200, width = 200)
    else
      tabsetPanel(tabPanel("Prediction", DT::DTOutput("tt")),
                  tabPanel("Sample Raw Data", tableOutput("table")), 
                  tabPanel("Z-scores", tableOutput("z_scores")), 
                  tabPanel("Dataset for ML", tableOutput("dataset_for_ML")))
  })
  
  
  observeEvent(input$go, {
    shinyjs::disable("go_demo")
    shinyjs::show("text1")
  })
  
}



shinyApp(ui = ui, server = server)
