#GUI for formatting HIC files


#' Graphical applications for formatting, auto-validation, manual-validation and final export of HIC database data
#'
#' Three graphical user interfaces (GUI) for the processing of the Flemish Hydrological Information Center (HIC) data.
#' These graphical apps will walk you through the entire work flow of data import, validation/calibration and data export, in an intuitive visual manner without the need for coding.
#' This is the only method of manual validation and calibration and final export.
#'
#' Either enter "format" to format HIC database data, "auto" to auto-validate formatted data, or "manual" to manually check and calibrate auto-validated data against reference site locations (if available) and export final zrx files for upload back into the HIC database.
#'
#' The formatting step and the auto validation step can be done in R code using the functions HIC.Continuous.Data.Import.Format(), spk.DespikingWorkflow.CSVfileBatchProcess(), and HIC.PPFDAutoValidation.CSVfileBatchProcess().
#' The manual validation, calibration and final export must be done using this graphical app.
#'
#' \subsection{Recomended workflow}
#' First: format the csv files that are exported from the HIC database - HIC.App.format()
#'
#' Second: auto-validate the formatted files - HIC.App.auto()
#'
#' Third: manual-validate/calibrate the files and export the data - HIC.App.manual()
#'
#' Files can be batch processed at each step so there is no need to run through all the steps for each individual file.
#'
#' \subsection{Manual Tutorial}
#' \url{https://github.com/pgelsomini/HICbioclean/blob/master/MANUAL-TUTORIAL-HICbioclean.-Rpackage.pdf}
#'

#' @return Three shiny apps
#' @export
#'
#' @examples
#' HIC.App.format() # to format the HIC database output csv files
#' HIC.App.auto() # to auto-validate the formatted data
#' HIC.App.manual() # to manually check the auto-validated data
HIC.App.format <- function(){
  require(shiny)
  shinyApp(
    ui <- fluidPage(
      titlePanel("GUI for HIC.Continuous.Data.Import.Format() function"),
      mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel("Function",
                             fluidRow(h2("HIC database file formatting")),
                             fluidRow("moves metadata from header, adds a numeric datetime column and converts to csv"),
                             fluidRow("Give the folder directory of text files from the HIC database export and all the files within that folder will be formatted properly for use in the other functions."),
                             fluidRow(column(12,HTML('Enter the path to your folder of text files using only forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\\). If the folder is in your working directory, then you only need to enter in the folder name.'))),
                             textInput('HICInputDirectory','Input directory',NULL),
                             fluidRow(column(12,HTML('Enter the path to your output folder where you want the formated csv files to be saved using only forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\\). If the folder is in your working directory, then you only need to enter in the folder name.'))),
                             fluidRow(column(12,"If this folder doesn't exist, then it will be created.")),
                             textInput('HICOutputDirectory','Output directory',NULL),
                             actionButton('HICRun','Run'),
                             p(),
                             fluidRow(h2("WISKI maintenance file formatting")),
                             fluidRow("converts from Excel file to csv file and adds a numeric datetime column"),
                             fluidRow("Give the folder directory of text files from the WISKI maintenance file export and all the files within that folder will be formatted properly for use in the other functions."),
                             fluidRow(column(12,HTML('Enter the path to your folder of text files using only forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\\). If the folder is in your working directory, then you only need to enter in the folder name.'))),
                             textInput('HICInputDirectory2','Input directory',NULL),
                             fluidRow(column(12,HTML('Enter the path to your output folder where you want the formated csv files to be saved using only forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\\). If the folder is in your working directory, then you only need to enter in the folder name.'))),
                             fluidRow(column(12,"If this folder doesn't exist, then it will be created.")),
                             textInput('HICOutputDirectory2','Output directory',NULL),
                             actionButton('HICRun2','Run')


                    ),
                    tabPanel("Help",
                             HTML('<h3><a href="https://github.com/pgelsomini/HICbioclean/blob/775ccbaa22d12a78736f07a3391f4f12e62eb2bf/MANUAL-TOTORIAL-HICbioclean.-Rpackage.pdf" target="_blank" rel="noopener noreferrer">Link to manual and tutorial for this app</a></h3>'),
                             fluidRow(h4('Your working directory')),
                             fluidRow(verbatimTextOutput("workingdirectory")),


                             htmlOutput("inc1"), #get html help documents from package directory
                             htmlOutput("inc2"),
                             htmlOutput("inc3")
                    )
        )
      )


    ),

    server <- function(input, output,session){
      session$onSessionEnded(function() { #stops the app when the window closes
        stopApp()
      })
      observeEvent(input$HICRun,{
        withProgress(message = "Process running: formatting continuous HIC data",{
          HIC.Continuous.Data.Import.Format(input$HICInputDirectory,input$HICOutputDirectory, Data.sep = "\t", Meta.sep =":", Data.header.line = NULL, Dec = ".",DateFormat="%d/%m/%Y",TimeZone = "Etc/GMT-1",OneYearDataSet = F,ParamNameColmn = "Parameter.Name",StationNoColmn = "Station.Number",DateColmn="Date",TimeColmn="Time")
        })
      })
      observeEvent(input$HICRun2,{
        withProgress(message = "Process running: formatting WISKI maintenance data",{
          HIC.maint(input$HICInputDirectory2,input$HICOutputDirectory2)
        })
      })

      #show working directory in text box
      output$workingdirectory <- renderText({
        getwd()
      })

      #Get help documents from package directory
      getPage<-function(x) {
        htmlDir <- system.file("html", x , package = "HICbioclean")
        return(includeHTML(htmlDir))
      }
      output$inc1<-renderUI({
        getPage("HIC.App.format.html")
      })
      output$inc2<-renderUI({
        getPage("HIC.Continuous.Data.Import.Format.html")
      })
      output$inc3<-renderUI({
        getPage("HIC.maint.html")
      })

    }

  )
}
