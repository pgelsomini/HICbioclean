#Shiny app for checking the autovalidation
#Pali Gelsominin ECOBE 2020

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
HIC.App.auto <- function(){
  require(shiny)
  require(colourpicker)


  shinyApp(
    ui <- fluidPage(
      titlePanel("Continuous Data Auto Validation"),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Despiking",
                             fluidRow(h4('Auto-validation')),
                             tabsetPanel(type = 'tabs',
                                         tabPanel('Data source',
                                                  fluidRow(strong('Place all data to auto-despike into one folder as CSV file tables.')),
                                                  fluidRow('The data values must be numeric. Date and time should be both in the same column with no time zone corrections (e.g. 13:20 +2   the +2 is a time zone correction). Datetime may also be numeric. If there are no interruptions in the sampling causing data gaps, then a datetime is not needed.'),
                                                  checkboxInput("pickup",HTML('<strong>Data already formatted</strong> Check this box if you are loading data that was previously processed and formatted using this app. Column names and formatting will be automatically filled in.'),value = F),
                                                  checkboxInput("add.original.data.dspk",strong("Add original data to output data files"),value = T),
                                                  tabsetPanel(type = 'tabs',
                                                              tabPanel('CSV files',
                                                                       fluidRow(column(12,HTML('<strong>Directory path for data folder *</strong> Enter the path to your folder of csv files using only forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\\). If the folder is in your working directory, then you only need to enter in the folder name.'))),
                                                                       textInput("InputDirectory.dspk",NULL,NA),
                                                                       fluidRow(column(12,HTML('<strong>CSV file format</strong> If Excel was used to generated or edited the csv files, then the separator may be (;) and the decimal bay be (,)'))),
                                                                       fluidRow(column(12,
                                                                                       splitLayout( #this keeps everything together
                                                                                         cellWidths = c('18%','18%','18%','18%','28%'),
                                                                                         selectInput("sep.dspk",NULL,c(',',';')),
                                                                                         strong(': separator'),
                                                                                         selectInput("dec.dspk",NULL,c('.',',')),
                                                                                         strong(': decimal'),
                                                                                         checkboxInput("header.dspk",strong("table header"),value = T),
                                                                                         tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))) #so that the dropdown menus are not cut off when they leave the block
                                                                                       )
                                                                       )),


                                                              ),
                                                              tabPanel('Value data',
                                                                       conditionalPanel(condition = "!input.pickup",
                                                                                        textInput("valuenamedspk","Value column*","Value"),
                                                                                        checkboxInput("valcoltypedspk", HTML("<strong>is column number</strong> farthest left is 1"),value = F),
                                                                                        fluidRow(column(12,HTML('<strong>NA numeric code</strong> All entries in the CSV file of this value will be converted to NA.'))),
                                                                                        numericInput("Naval.dspk",NULL,-777),
                                                                       ),
                                                                       conditionalPanel(condition = "input.pickup",
                                                                                        'You are loading previously processed data so the data has already been formatted. There is no need to enter data columns or formatting settings.'
                                                                       )
                                                              ),



                                                              tabPanel('Datetime data',
                                                                       conditionalPanel(condition = "!input.pickup",
                                                                                        fluidRow(strong('If you have no interruptions in your data then a datetime is not necessary, leave blank.')),
                                                                                        p(),
                                                                                        textInput("datetimenamedspk","Datetime column","DateTimeUnix"),
                                                                                        checkboxInput("datetimecoltypedspk",HTML("<strong>is column number</strong> farthest left is 1"),value = F),
                                                                                        fluidRow(column(12,HTML('<strong>Datetime format</strong> e.g. %Y-%m-%d %H:%M:%S is "2018-12-23 15:25:12". See strptime() documentation for additional help. Ignore if datetime is numeric.'))),
                                                                                        textInput("datetime.format.dspk",NULL,NULL),
                                                                                        fluidRow(column(12,HTML('<strong>Datetime time zone</strong> UTC+1 is Etc/GMT-1 and UTC-6 is Etc/GMT+6 with the sign corresponding to the mathematical correction. See strptime() documentation for additional help. Ignore if datetime is numeric.'))),
                                                                                        selectInput("timezone.dspk",NULL,OlsonNames(), selected = "Etc/GMT-1")
                                                                       ),
                                                                       conditionalPanel(condition = "input.pickup",
                                                                                        fluidRow('You are loading previously processed data so the datetimes have already been formatted. There is no need to enter data columns or formatting settings.'),
                                                                                        p()
                                                                       ),
                                                                       fluidRow(column(12,HTML('<strong>Sampling interval</strong> If there are irregularities in the sampling interval, so that it cannot be calculated from the data by taking the mode of the time difference between samples, then enter it below. Make sure to enter this in the unit of your datetime. If the original datetime was a character string, then it will be converted to UNIX seconds.'))),
                                                                       numericInput("sampling.interval.dspk",NULL,NULL),
                                                              )
                                                  ),

                                         ),
                                         tabPanel('Run Auto-despiking',
                                                  p(),
                                                  actionButton("Despike","Click to run despiking"),
                                                  tags$head(tags$style(HTML('#Despike{background-color:orange}'))),
                                                  p(),
                                                  checkboxInput('PPFDdespikedspk',strong('Despiking paired PPFD data and calculating exponential light attenuation coefficient kd'),value = F),
                                                  conditionalPanel(condition = 'input.PPFDdespikedspk',
                                                                   fluidRow(strong('When despiking PPFD and calculating kd there is a set workflow and steps cannot be selected.'))
                                                  ),
                                                  conditionalPanel(condition = '!input.PPFDdespikedspk',
                                                                   fluidRow(strong('Select the workflow steps you wish to run')),
                                                                   p(),
                                                                   fluidRow(column(12,('(Mandatory step) Pre-process: Data formatting'))),
                                                                   checkboxInput("step1dspk",("Step 1: Min/max filter"),value = T),
                                                                   checkboxInput("step2dspk",("Step 2: Despike"),value = T),
                                                                   checkboxInput("step3dspk",("Step 3: Gap interpolation"),value = T),
                                                  ),
                                                  conditionalPanel(condition = "input.step1dspk|input.PPFDdespikedspk",
                                                                   fluidRow((('All values outside of min and max will be deleted. You can leave one or both blank if there is no min or max.'))),
                                                                   numericInput("min.dspk","Min reasonable value",NULL),
                                                                   numericInput("max.dspk","Max reasonable value",NULL),
                                                                   checkboxInput('ConditionalMinMaxdspk','Conditional Min Max', value = T),
                                                                   conditionalPanel(condition = "input.ConditionalMinMaxdspk",
                                                                                    fluidRow("Conditional min max values for batch processing files"),
                                                                                    fluidRow("Add the column name in your data that contains the meta data that you wish to base the conditional min max values on.
                                                                                         It will only take the first value at the top of this column so be sure that this value is the same for the whole datatable.
                                                                                         This function is designed to aid batch processing with different files having different units or different sites with different criteria
                                                                                         and that is why it only takes the top value in the column. More colplex conditional min max filtering cannot be done with this app."),
                                                                                    p(),
                                                                                    fluidRow('If the no condition is met, then it will take the non-conditional min max values entered above. If no values are entered, then it will take infinity as the min and max.'),
                                                                                    textInput('conditionalMinMaxColumndspk','Column name containing meta data for conditional min max','Parameter.Name'),
                                                                                    checkboxInput('condMinMaxColumnIsNumdspk','Is a column number',value = F),
                                                                                    textInput('conditionalMinMaxValuesdspk','list of your meta data values (example: Temp,Weight,Volume)','DO,pH,chfyla,PPFD1,PPFD'),
                                                                                    textInput('conditionalMinMaxMindspk', 'list of your minimum values (example: -30,0,0)','0,0,0,0,0'),
                                                                                    textInput('conditionalMinMaxMaxdspk', 'list of your maximum values (example: 100,500,1000)','30,15,1000,2000,2000')
                                                                   )

                                                  ),
                                                  conditionalPanel(condition = "input.step3dspk|input.PPFDdespikedspk",
                                                                   fluidRow(column(12,HTML('<strong>You may leave this field blank and the maximum number of decimal places in your data will be taken as the precision.</strong> '))),
                                                                   fluidRow(column(12,HTML('<strong>A custom precision may be entered as follows.</strong> examples: 0.566 would be ‘precision = 0.001’ 1200 would be ‘precision = 100’ Measurements with steps of 5 would be ‘precision = 5’ Measurements to the nearest half unit would be ‘precision = 0.5’'))),
                                                                   numericInput("precision.dspk",NULL,NULL),
                                                                   fluidRow(column(12,HTML('<strong>Max gap to linear interpolate</strong> Make sure to enter the gap width in the same time unit as your datetime. If the original data was character, then it has been converted to UNIX seconds. If no datetime was provided, then the unit is in samples.'))),
                                                                   numericInput("max.gap.dspk",NULL,3600),'default 3600 seconds = 1 hour'

                                                  ),
                                                  conditionalPanel(condition = "input.PPFDdespikedspk",
                                                                   fluidRow(strong('PPFD despiking inputs')),
                                                                   numericInput('kddlupperdspk','kd detection limit upper sensor', 1),
                                                                   numericInput('kddllower','kd detection limit lower sensor', 0.25),
                                                                   numericInput('distsensorsdspk','Distance between PPFD sensors (m)', 0.4),
                                                                   textInput('uppersensorIDdspk','Upper sensor parameter ID', 'PPFD1'),
                                                                   textInput('lowersensorIDdspk','Lower sensor parameter ID', 'PPFD')
                                                  ),
                                                  tabsetPanel(type = 'pills',
                                                              tabPanel('hide additional options'),
                                                              tabPanel('Despiking method options',
                                                                       numericInput("threshold.dspk","Despike threshold",3),
                                                                       selectInput("method.dspk","Despike method",c('median','mean')),
                                                                       fluidRow('The spike removal algorithm is by default (threshold = 3, method = "median")
                                                                            all sample points that are more than the threshold 3 median absolute deviations
                                                                            (the scale factor 1.4826 is used assuming normal distribution) from the median
                                                                            of the 10 surrounding data points (5 before and 5 after) are automatically deleted.
                                                                            If there are data gaps and time stamps on the samples, then the algorithm will look
                                                                            5 sampling intervals before and 5 afterwards. The threshold of 3 can be changed.
                                                                            You can set the method to "mean" to use standard deviations and mean for the algorithm
                                                                            instead of the default median absolute deviations and median. However median is a much
                                                                            more robust statistic for handling outliers.')
                                                              ),
                                                              tabPanel('State of value codes',
                                                                       fluidRow('State of value codes are added to the
                                                                   data to keep track of how each value was handled during the auto-despiking
                                                                   process (defaults: 110 unchecked, 80 auto good value, 91 deleted during min/max filter,
                                                                   92 deleted during despiking, 93 interpolated gap).'),
                                                                       numericInput("sovuncheckeddspk","Unchecked state of value code",110),
                                                                       numericInput("sovminmaxdspk","Min/max filter deleted state of value code",91),
                                                                       numericInput("sovdespikedspk","Spike deleted state of value code",92),
                                                                       numericInput("sovgooddspk","Good state of value code",80),
                                                                       numericInput("sovinterpoldspk","Linear interpolated state of value code",93),
                                                                       fluidRow('Additional state of value codes for despiking PPFD data'),
                                                                       numericInput("sovdeleteotherdspk","deleted, spike in other sensor state of value code",94),
                                                                       numericInput("sovnotdeletebothdspk","not deleted, spike in both sensors state of value code",95),
                                                                       numericInput("sovdeletedkdspikedspk","deleted, spike in kd state of value code",96),
                                                                       numericInput("sovdeletedkdnegativedspk","deleted, negative kd state of value code",97),
                                                                       numericInput("sovnodatadspk","no data",255),
                                                              )
                                                  )
                                         )
                             )
                    ),




                    tabPanel("Check",
                             fluidRow(h4('Visual check of auto-validation results')),
                             fluidRow('Enter the parent directory (autodespike##############) inside your working directory containing the subfolders for each step that were generated using this app or the dspk.DespikingWorkflow.CSVfileBatchProcess() function. Enter the path to your folder of csv files using only forward-slashes(/) no back-slashes(\\). If the folder is in your working directory, then you only need to enter in the folder name.'),
                             fluidRow(checkboxInput('checkPPFDdata','Check PPFD data',value = F)),
                             fluidRow(actionButton('lastdirectory','load the data from the last despiking')),
                             tags$head(tags$style(HTML('#lastdirectory{background-color:orange}'))),
                             fluidRow(textInput("directory", "enter directory",NULL,width = 1000)
                             ),
                             fluidRow(
                               uiOutput("dynamicdropdown")
                             ),
                             fluidRow(
                               strong('file path')
                             ),
                             fluidRow(verbatimTextOutput("info")
                             ),
                             fluidRow(
                               'Select "loadfile" to upload and graph the data from the file in the above dropdown menu. For a more streamlined and accurate workflow, after loading and checking the first file, you can select "load next file" to upload and graph the data in the following file, to avoid accidentally skipping a file. The program will tell you when you have reached your last file, so keep clicking "load next file" till you are done.'
                             ),
                             fluidRow(
                               actionButton('loadfile','load file'),
                               actionButton('nextt','load next file'),
                               uiOutput("resetbutton")
                             ),
                             fluidRow('Select a rectangle on the graph and double click to zoom to that extent. Double click again to zoom back to full extent.'),
                             conditionalPanel(condition = 'input.checkPPFDdata',fluidRow('The x-axis will only zoom back to full extent if you double click the top graph. Double clicking the lower graphs will only zoom the y-axis of the double clicked graph back to full extent.')),
                             fluidRow(plotOutput("plot1", brush = brushOpts(
                               id = "plot_brush",
                               resetOnNew = TRUE),
                               dblclick = "plot_dblclick")
                             ),
                             actionButton('zoomout','Zoom out times 2'),
                             'select a rectangle and double click to zoom in. Double click to zoom out to full extent.',
                             conditionalPanel(condition = 'input.checkPPFDdata',
                                              fluidRow(plotOutput("plot2", brush = brushOpts(
                                                id = "plot_brush2",
                                                resetOnNew = TRUE),
                                                dblclick = "plot_dblclick2")
                                              ),
                                              actionButton('zoomout2','Zoom out times 2'),
                                              'select a rectangle and double click to zoom in. Double click to zoom out to y-axis full extent.',
                                              fluidRow(plotOutput("plot3", brush = brushOpts(
                                                id = "plot_brush3",
                                                resetOnNew = TRUE),
                                                dblclick = "plot_dblclick3")
                                              ),
                                              actionButton('zoomout3','Zoom out times 2'),
                                              'select a rectangle and double click to zoom in. Double click to zoom out to y-axis full extent.'
                             ),
                             fluidRow(
                               splitLayout(
                                 cellWidths = c('25%','25%','50%'),
                                 checkboxInput('lockxaxis','Lock x-axis', value = F),
                                 checkboxInput('lockyaxis','Lock y-axis', value = F),
                                 checkboxInput('showdeletednoninterpolated','show deleted but not reinterpolated data',value = F)
                               )
                             ),
                             fluidRow(
                               checkboxInput('AxisIsUNIXsec','Convert x-axis seconds to datetime',value = T),
                             ),
                             fluidRow(
                               strong('Interactive graph legend:'), 'colors and options can be changed, but "render graph with new options" button must be pressed after selecting new options'
                             ),
                             fluidRow(
                               actionButton('graph', 'render graph with new options')
                             ),
                             fluidRow(
                               column(4,numericInput("pointsize","Point Size (0.5)",1)),
                               column(4,checkboxInput('legend','add legend to graph',value = F)),
                               column(4,selectInput('legendlocal','legend location', c('topleft','top','topright','right','bottomright','bottom','bottomleft','left','center')))
                             ),
                             fluidRow(
                               column(4, colourInput("col1",'auto good',value = "black")),
                               column(4, colourInput("col2",'min max delete',value = "purple")),
                               column(4, colourInput("col3",'auto spike delete',value = "red"))
                             ),
                             fluidRow(
                               #conditionalPanel(condition = '!input.checkPPFDdata',column(4, colourInput("col4",'auto interpolate',value = "green"))),
                               column(4, colourInput("col5",'unchecked',value = "gray")),
                               column(4, colourInput("col11",'missing data, interpolated', value = "brown")),
                               column(4, colourInput("col6",'other',value = "yellow"))
                             ),
                             conditionalPanel(condition = 'input.checkPPFDdata',
                                              fluidRow(
                                                column(4, colourInput("col7",'deleted, spike in other sensor', value = "pink")),
                                                column(4, colourInput("col8",'not deleted, spike in both sensors', value = "blue")),
                                              ),
                                              fluidRow(
                                                column(4, colourInput("col9",'deleted, spike in kd',value = "green")),
                                                column(4, colourInput("col10",'deleted, negative kd',value = "orange"))
                                              )
                             ),

                    ),
                    tabPanel('Help',
                             HTML('<h3><a href="https://github.com/pgelsomini/HICbioclean/blob/775ccbaa22d12a78736f07a3391f4f12e62eb2bf/MANUAL-TOTORIAL-HICbioclean.-Rpackage.pdf" target="_blank" rel="noopener noreferrer">Link to manual and tutorial for this app</a></h3>'),
                             fluidRow(h4('Your working directory')),
                             fluidRow(verbatimTextOutput("workingdirectory")),
                             htmlOutput("inc1"), #get html help documents from package directory
                             htmlOutput("inc2"),
                             htmlOutput("inc3")

                    )
        ),
        hr(),
        fluidRow('Script written by Pali Felice Gelsomini'),
        fluidRow('University of Antwerp: The Ecosystem Management Research Group (ECOBE)'),
        fluidRow('August 2020'),
        fluidRow('contact: palifelice.gelsomini@uantwerpen.be')
      )
    ),

    server <- function(input, output, session) {
      session$onSessionEnded(function() { #stops the app when the window closes
        stopApp()
      })
      options(shiny.maxRequestSize=800*1024^2) #expands maximum file size for upload from 5mb to 800mb

      #Get help documents from package directory
      getPage<-function(x) {
        htmlDir <- system.file("html", x , package = "HICbioclean")
        return(includeHTML(htmlDir))
      }
      output$inc1<-renderUI({
        getPage("HIC.App.auto.html")
      })
      output$inc2<-renderUI({
        getPage("dspk.DespikingWorkflow.CSVfileBatchProcess.html")
      })
      output$inc3<-renderUI({
        getPage("HIC.PPFDAutoValidation.CSVfileBatchProcess.html")
      })

      #update directory input to the latest auto generated direcory from the despiking function
      observeEvent(input$lastdirectory,{
        updateTextInput(session,'directory',value = lastfolder$path)

      })


      #directory paths and file names
      filename <- reactiveValues( files = NULL, dir0 = NULL, path = NULL, n=NULL, gfiles = NULL)
      observe({
        if(input$checkPPFDdata){
          filename$dir0 <- paste0(getwd(),'/', input$directory,'/step4Despikekd.FinalData')
        }else{
          filename$dir0 <- paste0(getwd(),'/', input$directory,'/preprocFormat')
        }
        lastfile$value <- F
        lastfile$step <- F
      })
      observe({filename$files <- list.files(filename$dir0)})
      observe({filename$path <- paste0(filename$dir0, '/', filename$files[as.numeric(input$filen)])})


      #drop down meneau for selecting file
      output$dynamicdropdown <- renderUI({
        if(dir.exists(filename$dir0)&length(filename$files)>0){
          files <- seq(from = 1, to = length(filename$files), by = 1)
          names(files) <- filename$files
        } else {files <- character(0)}

        selectInput("filen","select CSV file name", files,width = 1000)

      })


      #show file path in text box
      output$info <- renderText({
        if(lastfile$value){'No more files to process'}else{
          filename$path
        }
      })

      #show working directory in text box
      output$workingdirectory <- renderText({
        getwd()
      })



      #get data----------
      #variables for storing data
      vals0 <- reactiveValues(tCont = NULL,valCont = NULL,state = NULL,valContx = NULL,statex = NULL,valConty = NULL,statey = NULL,valorigx = NULL,valorigy = NULL,valorigkd = NULL) #formatted data
      vals1 <- reactiveValues(tCont = NULL,valCont = NULL,state = NULL) #min max filtered data
      vals2 <- reactiveValues(tCont = NULL,valCont = NULL,state = NULL) #despiked data
      vals3 <- reactiveValues(tCont = NULL,valCont = NULL,state = NULL) #auto interpolated data

      #load data function
      loaddata <- function(d=NULL){
        if(!is.null(input$filen)&!is.null(input$directory)) {
          if(is.null(d)){d<-input$filen}
          filename$n <- as.numeric(d)
          filename$gfiles <- filename$files
          withProgress(message = "Processing: load data", value =0, {
            if(input$checkPPFDdata){
              dir0 <- paste0(getwd(),'/', input$directory,'/step4Despikekd.FinalData')
              if(dir.exists(dir0)){
                file0 <- paste0(dir0,'/', list.files(dir0)[as.numeric(d)])
                table <- read.csv(file0,sep = ',', dec = '.')
                vals0$state <- table[["dspk.StateOfValue"]]
                vals0$tCont <- table[["dspk.DateTimeNum"]]
                vals0$valCont <- table[["dspk.kd"]]
                vals0$valContx <- table[["dspk.Values.x"]]
                vals0$valConty <- table[["dspk.Values.y"]]
                vals0$statex <- table[["dspk.StateOfValue.x"]]
                vals0$statey <- table[["dspk.StateOfValue.y"]]
                vals0$valorigx <- table[["orig.values.x"]]
                vals0$valorigy <- table[["orig.values.y"]]
                vals0$valorigkd <- table[["kd"]]

              }else{showNotification('No subdirectorie "step4Despikekd.FinalData" found: This app can only graph data that was outputed by this app or the dspk.DespikingWorkflow.CSVfileBatchProcess() function and HIC.PPFDAutoValidation.CSVfileBatchProcess() function in R', type = 'error')}
            }else{
              dirb <- paste0(getwd(),'/', input$directory)
              dir0 <- paste0(dirb,'/preprocFormat')
              dir1 <- paste0(dirb,'/step1MinMax')
              dir2 <- paste0(dirb,'/step2Despike')
              dir3 <- paste0(dirb,'/step3Interpol.FinalData')

              a<-0
              if(dir.exists(dir0)){
                file0 <- paste0(dir0,'/', list.files(dir0)[as.numeric(d)])
                table <- read.csv(file0,sep = ',', dec = '.')
                vals0$state <- table[["dspk.StateOfValue"]]
                vals0$tCont <- table[["dspk.DateTimeNum"]]
                vals0$valCont <- table[["dspk.Values"]]
                incProgress(1)
              }else{showNotification('No subdirectorie "preprocFormat" found: This app can only graph data that was outputed by this app or the dspk.DespikingWorkflow.CSVfileBatchProcess() function and HIC.PPFDAutoValidation.CSVfileBatchProcess() function  in R', type = 'error')}
              incProgress(1/4)
              if(dir.exists(dir1)){
                file1 <- paste0(dir1,'/', list.files(dir1)[as.numeric(d)])
                table <- read.csv(file1,sep = ',', dec = '.')
                vals1$state <- table[["dspk.StateOfValue"]]
                vals1$tCont <- table[["dspk.DateTimeNum"]]
                vals1$valCont <- table[["dspk.Values"]]
                a<-1
              }
              incProgress(1/4)
              if(dir.exists(dir2)){
                file2 <- paste0(dir2,'/', list.files(dir2)[as.numeric(d)])
                table <- read.csv(file2,sep = ',', dec = '.')
                vals2$state <- table[["dspk.StateOfValue"]]
                vals2$tCont <- table[["dspk.DateTimeNum"]]
                vals2$valCont <- table[["dspk.Values"]]
              }else if(a==1){
                vals2$state <- vals1$state
                vals2$tCont <- vals1$tCont
                vals2$valCont <- vals1$valCont
              }else if(a==0){
                vals2$state <- vals0$state
                vals2$tCont <- vals0$tCont
                vals2$valCont <- vals0$valCont
              }
              incProgress(1/4)
              if(dir.exists(dir3)){
                file3 <- paste0(dir3,'/', list.files(dir3)[as.numeric(d)])
                table <- read.csv(file3,sep = ',', dec = '.')
                vals3$state <- table[["dspk.StateOfValue"]]
                vals3$tCont <- table[["dspk.DateTimeNum"]]
                vals3$valCont <- table[["dspk.Values"]]
              }else{
                vals3$state <- vals0$state
                vals3$tCont <- NULL
                vals3$valCont <- NULL
              }
              incProgress(1/4)
            }
          })
        }
      }

      #load file button
      observeEvent(input$loadfile,{if(lastfile$value == F){loaddata()}})

      #load next file button
      lastfile <- reactiveValues(value = F,step = F) #place holder for if last file was loaded already
      observeEvent(input$nextt,{
        lastfile$step <- T
        x <- as.numeric(input$filen)
        x <- x + 1
        dir0 <- filename$dir0
        if(dir.exists(dir0) & x > length(filename$files)){
          lastfile$value <- T
          x <- character(0)
        }else{
          names(x) <- filename$files[x]
        }
        updateSelectInput(session, "filen",choices = x,selected = x)
        if(lastfile$value == F){loaddata(d=x)}
      })

      #resetting the drop down menu button: as it gets locked when you use the load next button
      output$resetbutton <- renderUI({
        if(lastfile$step) {
          fluidPage(
            fluidRow('If you use the "load next file" button, then the file dropdown meanu will lock. You need to press the "reset file dropdown menu" button to unlock it and set it back to the first file. If you have reached the end of the list of files using "load next file" the program will tell you, so do not worry.'),
            fluidRow(actionButton('reset', 'reset file dropdown menu'))
          )
        }
      })

      observeEvent(input$reset,{
        if(dir.exists(filename$dir0)&length(filename$files)>0){
          files <- seq(from = 1, to = length(filename$files), by = 1)
          names(files) <- filename$files
        } else {files <- character(0)}

        updateSelectInput(session, "filen",choices = files,selected = 1)

        lastfile$value <- F
        lastfile$step <- F
      })

      #observe({
      #  x <- input$directory
      #  isolate(lastfile$value) <- F
      #  isolate(lastfile$step) <- F
      #})


      #make plot continuous values---------------

      #function for reformating the x axes from UNIX time to Date time. This is much faster than reformating all the data
      datetimeform = function(x){format(as.POSIXct(x, origin = "1970-01-01", tz= "Etc/GMT-1"), "%b-%d-%y %H:%M")}
      #function for setting State levels
      f1= function(x){

        ifelse(x==input$sovgooddspk,'auto good',
               ifelse(x==input$sovminmaxdspk,'min max delete',
                      ifelse(x==input$sovdespikedspk,'auto spike delete',
                             ifelse(x==input$sovinterpoldspk,'auto interpolate',
                                    ifelse(x==input$sovuncheckeddspk,'unchecked',
                                           ifelse(x==input$sovdeleteotherdspk, 'deleted spike in other sensor',
                                                  ifelse(x==input$sovnotdeletebothdspk, 'not deleted spike in both sensors',
                                                         ifelse(x==input$sovdeletedkdspikedspk, 'deleted kd spike',
                                                                ifelse(x==input$sovdeletedkdnegativedspk, 'deleted negative kd',
                                                                       ifelse(x==input$sovnodatadspk,'missing data interpolated','other'))))))))))
      }
      #94 deleted, spike in other sensor 7 sovdeleteotherdspk
      #95 not deleted, spike in both sensors 8 sovnotdeletebothdspk
      #96 deleted, spike in kd 9 sovdeletedkdspikedspk
      #97 deleted, negative kd 10 sovdeletedkdnegativedspk
      #variable for storing zoom extent coordinates
      ranges <- reactiveValues(x = NULL, y = NULL, yu = NULL, yl = NULL)
      observe(if(!input$lockxaxis){ranges$x <- range(vals0$tCont, na.rm = T, finite=T)})
      observe(if(!input$lockyaxis){ranges$y <- range( vals0$valCont, na.rm = T, finite=T)})
      observe(if(!input$lockyaxis){ranges$yu <- range( vals0$valContx, na.rm = T, finite=T)})
      observe(if(!input$lockyaxis){ranges$yl <- range( vals0$valConty, na.rm = T, finite=T)})

      #setting factor levels for state of value that are understandable for continuous graph
      State <- function(x){
        a <- as.factor(x) #set state of value codes as a factor
        b <- as.numeric(levels(a)) #put the levels of the state of value codes into a vector
        c <- sapply(b, f1) #convert that vector of levels which are originally numbers into descriptions
        levels(a) <- c #set the state of value codes levels to the descriptive ones
        return(a)
      }


      output$plot1 <- renderPlot({
        input$graph

        if(is.null(vals0$tCont)){
          plot.new()
          title('no data')
        }else{
          if(input$checkPPFDdata){
            #graphing upper sensor

            #vals0$state <- table[["dspk.StateOfValue"]]
            #vals0$tCont <- table[["dspk.DateTimeNum"]]
            #vals0$valCont <- table[["dspk.kd"]]
            #vals0$valContx <- table[["dspk.Values.x"]]
            #vals0$valConty <- table[["dspk.Values.y"]]
            #vals0$statex <- table[["dspk.StateOfValue.x"]]
            #vals0$statey <- table[["dspk.StateOfValue.y"]]
            #vals0$valorigx <- table[["orig.values.x"]]
            #vals0$valorigy <- table[["orig.values.y"]]
            #vals0$valorigkd <- table[["kd"]]

            #setting State of values
            State2 <- State(vals0$statex)
            # get the range for the x and y axis
            xrange <- ranges$x
            yrange <- ranges$yu
            #y axis label
            ylabel = 'PPFD upper sensor'
            #get y values
            valy = vals0$valContx
            #get original y values
            valorigy = vals0$valorigx

            #setting up color scale
            col.names <- c('auto good','min max delete','auto spike delete','unchecked','other','deleted spike in other sensor','not deleted spike in both sensors','deleted kd spike','deleted negative kd','missing data interpolated')
            isolate({Colors <- c(input$col1,input$col2,input$col3,input$col4,input$col5,input$col6,input$col7,input$col8,input$col9,input$col10,input$col11)})
            names(Colors) <- col.names

            #graphing plot
            # factor levels
            Statelevels <- levels(as.factor(c(levels(State2))))
            nState <- length(Statelevels)

            # set up the plot
            #setup x-axis
            if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab=ylabel, xaxt = "n" )
              step <- round(diff(xrange)/5, digits = 0)
              marks <- seq(xrange[1]+step,xrange[2]-step,step)
              lab <- datetimeform(marks)
              axis(1, at = marks, labels = lab)
            }else{
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab=ylabel )
            }

            # add lines
            #add thin gray lin over whole original dataset
            lines(vals0$tCont, valorigy, type="l", lwd=0.5, col='gray')
            #add despiked dataset with state of values color coding
            for (i in 1:nState) {
              con <- State2 == Statelevels[i]
              subtime <- vals0$tCont[con]
              subval <- valy[con]
              lines(subtime, subval, type="p", pch=16, cex=isolate(input$pointsize),
                    col=Colors[Statelevels[i]])
              if(input$showdeletednoninterpolated){
                con2 <- is.na(subval)
                subvalorig <- valorigy[con]
                subvalorig <- subvalorig[con2]
                subtime <- subtime[con2]
                lines(subtime, subvalorig, type="p", pch=3, cex=isolate(input$pointsize),
                      col=Colors[Statelevels[i]])
              }
            }

            # add a title and subtitle
            title(filename$gfiles[as.numeric(filename$n)])

            # add a legend
            cols <- Colors[Statelevels[1]]
            for (i in 2:nState){
              cols <- c(cols,Colors[Statelevels[i]])
            }
            if(isolate(input$legend)){
              legend(isolate(input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                     pch=16, title='State of value')
            }
          }else{
            #setting State of values
            State2 <- State(vals2$state)
            State3 <- State(vals3$state)

            #setting up color scale
            col.names <- c('auto good','min max delete','auto spike delete','unchecked','other','deleted spike in other sensor','not deleted spike in both sensors','deleted kd spike','deleted negative kd','missing data interpolated')
            isolate({Colors <- c(input$col1,input$col2,input$col3,input$col4,input$col5,input$col6,input$col7,input$col8,input$col9,input$col10,input$col11)})
            names(Colors) <- col.names

            #graphing plot
            # factor levels
            Statelevels <- levels(as.factor(c(levels(State2),levels(State3))))
            nState <- length(Statelevels)

            # get the range for the x and y axis
            xrange <- ranges$x
            yrange <- ranges$y

            # set up the plot
            #setup x-axis
            if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab="Value", xaxt = "n" )
              step <- round(diff(xrange)/5, digits = 0)
              marks <- seq(xrange[1]+step,xrange[2]-step,step)
              lab <- datetimeform(marks)
              axis(1, at = marks, labels = lab)
            }else{
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab="Value" )
            }

            # add lines
            #add thin gray lin over whole original dataset
            lines(vals0$tCont, vals0$valCont, type="l", lwd=0.5, col='gray')
            #add original dataset with state of values from after despiking
            #for (i in 1:nState) {
            #  con <- State2 == Statelevels[i]
            #  subtime <- vals0$tCont[con]
            #  subval <- vals0$valCont[con]
            #  lines(subtime, subval, type="p", pch=16, cex=isolate(input$pointsize),
            #        col=Colors[Statelevels[i]])
            #}
            #add auto interpolated values
            #if(!input$nointerpolate){
            #  con <- State3 == 'auto interpolate'
            #  subtime <- vals3$tCont[con]
            #  subval <- vals3$valCont[con]
            #  lines(subtime, subval, type="p", pch=16, cex=isolate(input$pointsize),
            #        col=Colors['auto interpolate'])
            #}
            #add auto interpolated dataset with state of values from after despiking
            for (i in 1:nState) {
              con <- State2 == Statelevels[i]
              subtime <- vals3$tCont[con]
              subval <- vals3$valCont[con]
              lines(subtime, subval, type="p", pch=16, cex=isolate(input$pointsize),
                    col=Colors[Statelevels[i]])
              if(input$showdeletednoninterpolated){
                con2 <- is.na(subval)
                subvalorig <- vals0$valCont[con]
                subvalorig <- subvalorig[con2]
                subtime <- subtime[con2]
                lines(subtime, subvalorig, type="p", pch=3, cex=isolate(input$pointsize),
                      col=Colors[Statelevels[i]])
              }
            }

            # add a title and subtitle
            title(filename$gfiles[as.numeric(filename$n)])

            # add a legend
            cols <- Colors[Statelevels[1]]
            for (i in 2:nState){
              cols <- c(cols,Colors[Statelevels[i]])
            }
            #xrange[1], yrange[2],
            if(isolate(input$legend)){
              legend(isolate(input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                     pch=16, title='State of value')
            }
          }
        }

      })

      output$plot2 <- renderPlot({
        input$graph

        if(is.null(vals0$tCont)){
          plot.new()
          title('no data')
        }else{
          if(input$checkPPFDdata){
            #graphing lower sensor

            #vals0$state <- table[["dspk.StateOfValue"]]
            #vals0$tCont <- table[["dspk.DateTimeNum"]]
            #vals0$valCont <- table[["dspk.kd"]]
            #vals0$valContx <- table[["dspk.Values.x"]]
            #vals0$valConty <- table[["dspk.Values.y"]]
            #vals0$statex <- table[["dspk.StateOfValue.x"]]
            #vals0$statey <- table[["dspk.StateOfValue.y"]]
            #vals0$valorigx <- table[["orig.values.x"]]
            #vals0$valorigy <- table[["orig.values.y"]]
            #vals0$valorigkd <- table[["kd"]]

            #setting State of values
            State2 <- State(vals0$statey)
            # get the range for the x and y axis
            xrange <- ranges$x
            yrange <- ranges$yl
            #y axis label
            ylabel = 'PPFD lower sensor'
            #get y values
            valy = vals0$valConty
            #get original y values
            valorigy = vals0$valorigy

            #setting up color scale
            col.names <- c('auto good','min max delete','auto spike delete','unchecked','other','deleted spike in other sensor','not deleted spike in both sensors','deleted kd spike','deleted negative kd','missing data interpolated')
            isolate({Colors <- c(input$col1,input$col2,input$col3,input$col4,input$col5,input$col6,input$col7,input$col8,input$col9,input$col10,input$col11)})
            names(Colors) <- col.names

            #graphing plot
            # factor levels
            Statelevels <- levels(as.factor(c(levels(State2))))
            nState <- length(Statelevels)

            # set up the plot
            #setup x-axis
            if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab=ylabel, xaxt = "n" )
              step <- round(diff(xrange)/5, digits = 0)
              marks <- seq(xrange[1]+step,xrange[2]-step,step)
              lab <- datetimeform(marks)
              axis(1, at = marks, labels = lab)
            }else{
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab=ylabel )
            }

            # add lines
            #add thin gray lin over whole original dataset
            lines(vals0$tCont, valorigy, type="l", lwd=0.5, col='gray')
            #add despiked dataset with state of values color coding
            for (i in 1:nState) {
              con <- State2 == Statelevels[i]
              subtime <- vals0$tCont[con]
              subval <- valy[con]
              lines(subtime, subval, type="p", pch=16, cex=isolate(input$pointsize),
                    col=Colors[Statelevels[i]])
              if(input$showdeletednoninterpolated){
                con2 <- is.na(subval)
                subvalorig <- valorigy[con]
                subvalorig <- subvalorig[con2]
                subtime <- subtime[con2]
                lines(subtime, subvalorig, type="p", pch=3, cex=isolate(input$pointsize),
                      col=Colors[Statelevels[i]])
              }
            }

            # add a title and subtitle
            title(filename$gfiles[as.numeric(filename$n)])

            # add a legend
            cols <- Colors[Statelevels[1]]
            for (i in 2:nState){
              cols <- c(cols,Colors[Statelevels[i]])
            }
            if(isolate(input$legend)){
              legend(isolate(input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                     pch=16, title='State of value')
            }
          }
        }
      })

      output$plot3 <- renderPlot({
        input$graph

        if(is.null(vals0$tCont)){
          plot.new()
          title('no data')
        }else{
          if(input$checkPPFDdata){
            #graphing kd

            #vals0$state <- table[["dspk.StateOfValue"]]
            #vals0$tCont <- table[["dspk.DateTimeNum"]]
            #vals0$valCont <- table[["dspk.kd"]]
            #vals0$valContx <- table[["dspk.Values.x"]]
            #vals0$valConty <- table[["dspk.Values.y"]]
            #vals0$statex <- table[["dspk.StateOfValue.x"]]
            #vals0$statey <- table[["dspk.StateOfValue.y"]]
            #vals0$valorigx <- table[["orig.values.x"]]
            #vals0$valorigy <- table[["orig.values.y"]]
            #vals0$valorigkd <- table[["kd"]]

            #setting State of values
            State2 <- State(vals0$state)
            # get the range for the x and y axis
            xrange <- ranges$x
            yrange <- ranges$y
            #y axis label
            ylabel = 'kd'
            #get y values
            valy = vals0$valCont
            #get original y values
            valorigy = vals0$valorigkd

            #setting up color scale
            col.names <- c('auto good','min max delete','auto spike delete','unchecked','other','deleted spike in other sensor','not deleted spike in both sensors','deleted kd spike','deleted negative kd','missing data interpolated')
            isolate({Colors <- c(input$col1,input$col2,input$col3,input$col4,input$col5,input$col6,input$col7,input$col8,input$col9,input$col10,input$col11)})
            names(Colors) <- col.names

            #graphing plot
            # factor levels
            Statelevels <- levels(as.factor(c(levels(State2))))
            nState <- length(Statelevels)

            # set up the plot
            #setup x-axis
            if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab=ylabel, xaxt = "n" )
              step <- round(diff(xrange)/5, digits = 0)
              marks <- seq(xrange[1]+step,xrange[2]-step,step)
              lab <- datetimeform(marks)
              axis(1, at = marks, labels = lab)
            }else{
              plot(xrange, yrange, type="n", xlab="Time",
                   ylab=ylabel )
            }

            # add lines
            #add thin gray lin over whole original dataset
            lines(vals0$tCont, valorigy, type="l", lwd=0.5, col='gray')
            #add despiked dataset with state of values color coding
            for (i in 1:nState) {
              con <- State2 == Statelevels[i]
              subtime <- vals0$tCont[con]
              subval <- valy[con]
              lines(subtime, subval, type="p", pch=16, cex=isolate(input$pointsize),
                    col=Colors[Statelevels[i]])
              if(input$showdeletednoninterpolated){
                con2 <- is.na(subval)
                subvalorig <- valorigy[con]
                subvalorig <- subvalorig[con2]
                subtime <- subtime[con2]
                lines(subtime, subvalorig, type="p", pch=3, cex=isolate(input$pointsize),
                      col=Colors[Statelevels[i]])
              }
            }

            # add a title and subtitle
            title(filename$gfiles[as.numeric(filename$n)])

            # add a legend
            cols <- Colors[Statelevels[1]]
            for (i in 2:nState){
              cols <- c(cols,Colors[Statelevels[i]])
            }
            if(isolate(input$legend)){
              legend(isolate(input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                     pch=16, title='State of value')
            }
          }
        }
      })


      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
          if(!input$lockxaxis){ranges$x <- c(brush$xmin, brush$xmax)}
          if(input$checkPPFDdata){
            if(!input$lockyaxis){ranges$yu <- c(brush$ymin, brush$ymax)}
          }else{
            if(!input$lockyaxis){ranges$y <- c(brush$ymin, brush$ymax)}
          }
        } else {
          if(!input$lockxaxis){ranges$x <- range(vals0$tCont, na.rm = T,finite=T)}
          if(input$checkPPFDdata){
            if(!input$lockyaxis){ranges$yu <- range( vals0$valContx, na.rm = T,finite=T)}
          }else{
            if(!input$lockyaxis){ranges$y <- range( vals0$valCont, na.rm = T,finite=T)}
          }
        }
      })
      observeEvent(input$plot_dblclick2, {
        brush <- input$plot_brush2
        if (!is.null(brush)) {
          if(!input$lockxaxis){ranges$x <- c(brush$xmin, brush$xmax)}
          if(!input$lockyaxis){ranges$yl <- c(brush$ymin, brush$ymax)}
        } else {
          #if(!input$lockxaxis){ranges$x <- range(vals0$tCont, na.rm = T,finite=T)}
          if(!input$lockyaxis){ranges$yl <- range( vals0$valConty, na.rm = T,finite=T)}
        }
      })
      observeEvent(input$plot_dblclick3, {
        brush <- input$plot_brush3
        if (!is.null(brush)) {
          if(!input$lockxaxis){ranges$x <- c(brush$xmin, brush$xmax)}
          if(!input$lockyaxis){ranges$y <- c(brush$ymin, brush$ymax)}
        } else {
          #if(!input$lockxaxis){ranges$x <- range(vals0$tCont, na.rm = T,finite=T)}
          if(!input$lockyaxis){ranges$y <- range( vals0$valCont, na.rm = T,finite=T)}
        }
      })
      observeEvent(input$zoomout,{
        ff <- function(x){
          return(c(x[1]-((x[2]-x[1])/2),x[2]+((x[2]-x[1])/2)))
        }
        if(!input$lockxaxis){
          ranges$x <- ff(ranges$x)
        }
        if(!input$lockyaxis){
          if(input$checkPPFDdata){
            ranges$yu <- ff(ranges$yu)
          }else{
            ranges$y <- ff(ranges$y)
          }
        }
      })
      observeEvent(input$zoomout2,{
        ff <- function(x){
          return(c(x[1]-((x[2]-x[1])/2),x[2]+((x[2]-x[1])/2)))
        }
        if(!input$lockxaxis){
          ranges$x <- ff(ranges$x)
        }
        if(!input$lockyaxis){
          ranges$yl <- ff(ranges$yl)
        }
      })
      observeEvent(input$zoomout3,{
        ff <- function(x){
          return(c(x[1]-((x[2]-x[1])/2),x[2]+((x[2]-x[1])/2)))
        }
        if(!input$lockxaxis){
          ranges$x <- ff(ranges$x)
        }
        if(!input$lockyaxis){
          ranges$y <- ff(ranges$y)
        }
      })

      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #---------------------------------------------------------------
      #--------------------------------------------------------------
      #auto despiking------------------------------------------------------ -------

      #variable for saving the output directory into so that it can be loaded into the graphing page
      lastfolder <- reactiveValues(path = NULL)

      #getting either the column name or the column number depending on what entry is selected or if pickup processing is selected
      columnnames <- reactiveValues(value = NULL, datetime = NULL)
      observe({
        if(input$pickup){
          columnnames$value <-"dspk.Values"
          columnnames$datetime <-"dspk.DateTimeNum"
        }else{
          if(input$valcoltypedspk){
            columnnames$value <- as.numeric(input$valuenamedspk)
          }else{
            columnnames$value <- input$valuenamedspk
          }
          if(input$datetimecoltypedspk){
            columnnames$datetime <- as.numeric(input$datetimenamedspk)
          }else{
            columnnames$datetime <- input$datetimenamedspk
          }
        }
      })



      #Full work flow saving into CSV files--------------------------------------------
      observeEvent(input$Despike,{

        #requirments check
        if(!dir.exists(input$InputDirectory.dspk)){
          showNotification('Enter a valid directory in data source', type = 'error')
          return(NULL)}
        if(columnnames$value==""){
          showNotification('Enter a Value column in data source', type = 'error')
          return(NULL)}


        #step1 = input$step1dspk
        #step2 = input$step2dspk
        #step3 = input$step3dspk
        steps <- NULL
        if(input$step1dspk) {steps <- c(steps,1)}
        if(input$step2dspk) {steps <- c(steps,2)}
        if(input$step3dspk) {steps <- c(steps,3)}
        input.directory = input$InputDirectory.dspk
        sep = input$sep.dspk
        dec = input$dec.dspk
        header = input$header.dspk
        Data = NULL
        DataUpper = NULL
        DataLower = NULL
        #formating function
        Value = columnnames$value
        val.NAvalue = input$Naval.dspk ; if(is.na(val.NAvalue))val.NAvalue<-NULL
        unchecked.state.of.value.code = input$sovuncheckeddspk
        NA.state.of.value.code = input$sovnodatadspk
        add.original.data = input$add.original.data.dspk
        DateTime = columnnames$datetime ; if(is.na(DateTime))DateTime<-NULL ; if(DateTime=='')DateTime<-NULL
        datetime.format = input$datetime.format.dspk ; if(datetime.format=='')datetime.format<-NULL
        datetime.timezone = input$timezone.dspk
        #min max
        Min = input$min.dspk ; if(is.na(Min)){Min<-(-Inf)}
        Max = input$max.dspk ; if(is.na(Max)){Max<-Inf}
        if(input$condMinMaxColumnIsNumdspk){#the column number was entered
          ConditionalMinMaxColumn <- as.numeric(input$conditionalMinMaxColumndspk) #CSV.table[[conditionalMinMaxColumn]][1] #get the first value in the metadata column
        }else{
          ConditionalMinMaxColumn <- input$conditionalMinMaxColumndspk #CSV.table[[conditionalMinMaxColumn]][1] #get the first value in the metadata column
        }
        if(is.na(ConditionalMinMaxColumn)){ConditionalMinMaxColumn<-NULL};if(ConditionalMinMaxColumn==""){ConditionalMinMaxColumn<-NULL}
        if(!input$ConditionalMinMaxdspk){ConditionalMinMaxColumn<-NULL}
        ConditionalMinMaxValues <- eval(parse(text = paste0('as.character(quote(c(',input$conditionalMinMaxValuesdspk,')))[-1]'))) #code for turning "a,b,c" into "a","b","c"
        if(length(ConditionalMinMaxValues)==0){ConditionalMinMaxValues<-NULL}
        ConditionalMin <- as.numeric(eval(parse(text = paste0('c(',input$conditionalMinMaxMindspk,')')))) #vector of mins
        if(length(ConditionalMin)==0){ConditionalMin<-NULL}
        ConditionalMax <- as.numeric(eval(parse(text = paste0('c(',input$conditionalMinMaxMaxdspk,')')))) #vector of maxs
        if(length(ConditionalMax)==0){ConditionalMax<-NULL}
        minmax.state.of.value.code = input$sovminmaxdspk
        #Despike
        sampling.interval = input$sampling.interval.dspk ; if(is.na(sampling.interval))sampling.interval<-NULL
        despiked.state.of.value.code = input$sovdespikedspk
        good.state.of.value.code = input$sovgooddspk
        despike.threshold = input$threshold.dspk ; if(is.na(despike.threshold)){showNotification('Despiking threshold was left blank. It was set to 3.',type = 'error') ; despike.threshold<-3}
        despike.Method = input$method.dspk
        #interpolate
        precision = input$precision.dspk ; if(is.na(precision))precision<-NULL
        max.gap = input$max.gap.dspk ; if(is.na(max.gap)){max.gap<-Inf}
        interpolated.state.of.value.code = input$sovinterpoldspk
        #PPFD specific variables
        DeletedSpikeOtherSensor.state.of.value.code = input$sovdeleteotherdspk
        NotDeletedSpikeBothSensors.state.of.value.code = input$sovnotdeletebothdspk
        kdDespiked.state.of.value.code = input$sovdeletedkdspikedspk
        DeletedNegativeKd.state.of.value.code = input$sovdeletedkdnegativedspk
        kddlupper = input$kddlupperdspk ; if(is.na(kddlupper))kddlupper<- -Inf
        kddllower = input$kddllower ; if(is.na(kddllower))kddllower<- -Inf
        Dist.Sensors = input$distsensorsdspk
        UpperSensorParameterName = input$uppersensorIDdspk
        LowerSensorParameterName = input$lowersensorIDdspk
        if((UpperSensorParameterName==''|LowerSensorParameterName=='')&input$PPFDdespikedspk){
          showNotification('If despiking PPFD, then the upper and lower sensors parameter ID which is found in the csv file name must be filled in.', type = 'error')
          return(NULL)
        }

        if(input$PPFDdespikedspk){
          #------------PPFD data despiking----------

          #below is a copy of the function HIC.PPFDAutoValidation.CSVfileBatchProcess()

          #function to identify type of input and covert to text conserving quotes and vectors
          #It is importaint that you know exactly what was entered into the function arguments
          itqv <- function(x){
            fff<-function(x){
              if(is.null(x)){return("NULL")}
              else if(is.na(x)){return("NA")}
              else if(is.character(x)){return(paste0("'",x,"'"))}
              else{return(x)}
            }
            if(length(x)>1){return(paste0("c(",paste(lapply(x, fff),collapse = ","),")"))
            }else if(length(x)<1&!is.null(x)){return("''")
            }else{return(fff(x))}
          }

          logdata <- '----Log file for PPFD autovalidation batch process function HIC.PPFDAutoValidation.CSVfileBatchProcess()-----'
          logdata <- rbind(logdata,'-------------------------------------------------------------------------------------------------------------')
          logdata <- rbind(logdata,'Below you will find a quick overview of the function, a list of the arguments provided to the function and a work log')
          logdata <- rbind(logdata,'showing the output directory, original CSV files processed, summaries of each step, files-specific calculated arguments and error reports.')
          logdata <- rbind(logdata,'For support and bug reporting please contact PaliFelice.Gelsomini@uantwerpen.be')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,paste('Date and time of data processing:',format(Sys.time(),format = '%Y.%m.%d %H:%M:%S %Z')))
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'---Overview---')
          logdata <- rbind(logdata,'Despiking workflow: preproces-formatting, min max filter on PPFD, despiking PPFD, all values deleted in one sensor dataset must be deleted in the other aswell, ')
          logdata <- rbind(logdata,'restore original values where spikes are in both sensor datasets, data gap interpolation PPFD, calculate light attenuation coefficient kd, delete PPFD values where kd is negative, ')
          logdata <- rbind(logdata,'remove kd values where PPFD is below detection limit, despike kd and delete those spikes from both the PPFD data and the kd data, data gap interpolation PPFD again, ')
          logdata <- rbind(logdata,'calculate light attenuation coefficient  kd again, delete kd values where PPFD is below detection limit.')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,paste('On CSV files in directory:',input.directory))
          if(is.null(input.directory))logdata <- rbind(logdata,paste('On dataframe from R environment:', ifelse(is.data.frame(DataUpper),deparse(substitute(DataUpper)),itqv(DataUpper)),'and',ifelse(is.data.frame(DataLower),deparse(substitute(DataLower)),itqv(DataLower))))
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'---Function Arguments:---')
          logdata <- rbind(logdata,paste('input.directory =', itqv(input.directory),', DataUpper =',ifelse(is.data.frame(DataUpper),deparse(substitute(DataUpper)),itqv(DataUpper)),', DataLower =',ifelse(is.data.frame(DataLower),deparse(substitute(DataLower)),itqv(DataLower))))
          logdata <- rbind(logdata,paste('Preprocessing:'))
          logdata <- rbind(logdata,paste('UpperSensorParameterName =',itqv(UpperSensorParameterName), ', LowerSensorParameterName =', itqv(LowerSensorParameterName)))
          logdata <- rbind(logdata,paste('sep =',itqv(sep),', dec =', itqv(dec), ', header =', itqv(header),', add.original.data =', itqv(add.original.data)))
          logdata <- rbind(logdata,paste('Value =',itqv(Value), ', val.NAvalue =',itqv(val.NAvalue)))
          logdata <- rbind(logdata,paste('unchecked.state.of.value.code =', itqv(unchecked.state.of.value.code), ', NA.state.of.value.code =', itqv(NA.state.of.value.code)))
          logdata <- rbind(logdata,paste('DateTime =', itqv(DateTime), ', datetime.format =', itqv(datetime.format), ', datetime.timezone =', itqv(datetime.timezone)))
          logdata <- rbind(logdata,paste('Min max filter:'))
          logdata <- rbind(logdata,paste('ConditionalMinMaxColumn =', itqv(ConditionalMinMaxColumn), ', ConditionalMinMaxValues =', itqv(ConditionalMinMaxValues), ', ConditionalMin =', itqv(ConditionalMin), ', ConditionalMax =', itqv(ConditionalMax)))
          logdata <- rbind(logdata,paste('Min =', itqv(Min), ', Max =', itqv(Max), ', minmax.state.of.value.code =', itqv(minmax.state.of.value.code)))
          logdata <- rbind(logdata,paste('Despiking:'))
          logdata <- rbind(logdata,paste('sampling.interval =', itqv(sampling.interval), ', despiked.state.of.value.code =', itqv(despiked.state.of.value.code), ', good.state.of.value.code =', itqv(good.state.of.value.code), ', despike.threshold =', itqv(despike.threshold), ', despike.Method =', itqv(despike.Method)))
          logdata <- rbind(logdata,paste('DeletedSpikeOtherSensor.state.of.value.code =',itqv(DeletedSpikeOtherSensor.state.of.value.code), ', NotDeletedSpikeBothSensors.state.of.value.code =',itqv(NotDeletedSpikeBothSensors.state.of.value.code)))
          logdata <- rbind(logdata,paste('Step 3 data gap interpolation:'))
          logdata <- rbind(logdata,paste('precision =', itqv(precision), ', max.gap =', itqv(max.gap)))
          logdata <- rbind(logdata,'Light attenuation coefficient  kd:')
          logdata <- rbind(logdata,paste('kddlupper =',itqv(kddlupper), ', kddllower =',itqv(kddllower), ', Dist.Sensors =',itqv(Dist.Sensors)))
          logdata <- rbind(logdata,paste('kdDespiked.state.of.value.code =',itqv(kdDespiked.state.of.value.code), ', DeletedNegativeKd.state.of.value.code =',itqv(DeletedNegativeKd.state.of.value.code)))
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'---Work log---')



          if(is.null(DateTime)){sampling.interval <- 1} #if no datetime data is provided then one will be made by numbering the records, a sampling interval is then not applicable. It needs to be set to one here because the despike function won't know to remove it since a datetime gets created during the formatting function.

          message('--PPFD data auto validation batch proces--')
          if(is.null(input.directory)){ #if no directory is given
            nfiles = 2                  #then we can only process one file
            files <- c('InputDataUpper','InputDataLower')
            if(is.null(DataUpper)|is.null(DataLower)){stop('if no directory of CSV files is given, then seperate dataframes for both the upper and lower sensors must be provided')}
          }else{
            #list files in directory
            files <- list.files(input.directory)
            #number of files in the directory
            f <- length(files)
          }
          files = list.files(input.directory)
          nfiles = length(files) #number of files in the directory
          if(!(nfiles%%2==0)) stop('Uneven number of files in the input directory. This function processes pairs of upper and lower PPFD sensors. Please make sure that all files are present in the input directory.')
          logdata <- rbind(logdata,paste('Files in input directory:', paste(files,collapse = ',')))
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')


          #create a unique directory with the current time to save data into
          CurrentTime <- format(Sys.time(),"%Y%m%d%H%M%S")
          #create directory if it doen't exist, else add a number to the end till it doen't exist
          dir0 <- paste0("autoPPFDdespike",CurrentTime)
          if(dir.exists(dir0)){
            dup <- 1
            while(dir.exists(paste0(dir0,'_',dup))){dup <- dup+1} #loop till directory is unique
            dir0 <- paste0(dir0,'_',dup)
          }
          dir.create(dir0)
          message(paste("created directory:",dir0))
          logdata <- rbind(logdata,paste('All data saved into output directory:', dir0))
          logdata <- rbind(logdata,'')
          logdata <- rbind(logdata,'')

          dir1 <- paste0(dir0,"/preprocFormat")
          dir.create(dir1)
          dir3 <- paste0(dir0,"/step2Despike")
          dir.create(dir3)
          dir4 <- paste0(dir0,"/step3Interpol.kd")
          dir.create(dir4)
          dir5 <- paste0(dir0,"/step4Despikekd.FinalData")
          dir.create(dir5)

          withProgress(message = "Process: autodespiking",max = nfiles/2,{

            for (i in 1:(nfiles/2)) { #nfiles/2 because the files are processed in pairs
              tryCatch({   #in case of error, catch error and tell me the file name
                message('-------------------')
                message('-------------------')
                message('-------------------')
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'----------------------------')
                message(paste('processing file set',i,'of',nfiles/2))
                #load data
                incProgress(amount = 1/8, message = paste("loading data: file set",i,"of",nfiles/2))
                if(!is.null(input.directory)){ #if directory is given
                  warningfunc <- function(x){
                    showNotification(paste("Formatting: File: ",files[i*2-1],'and',files[i*2]," Warning: ",x), type = "error", duration = NULL)
                    logdata <<- rbind(logdata,paste("File caused warning:",files[i*2-1],'and',files[i*2]))
                    logdata <<- rbind(logdata,paste("Original warning:",x))
                  }
                  UpperAndLowerSensors = c(UpperSensorParameterName,LowerSensorParameterName)
                  if(!(strsplit(files[i*2], "_", fixed = TRUE)[[1]][[1]]==strsplit(files[i*2-1], "_", fixed = TRUE)[[1]][[1]])){
                    warningfunc('Station Names do not match. This function takes all files in a folder and processes them 2 by 2 with the assumption that the upper and lower PPFD sensors will have identical station names, and thus will be placed next to eachother when the csv files are ordered alphabetically. Check that only PPFD data files are in the folder, that each pair of sensors has the same station name and that all pairs of sensors are complete. If the station name and the parameter name are not separated by an _ in the file name then this can also cause this warning.') #Split the file at the first decimal, this should be the station name. Are the station names the same at the two sites?
                  }
                  if(sort(UpperAndLowerSensors)[1]==LowerSensorParameterName){ #if lower sensor come first alphabetically
                    upperfirst<-F
                    if(!grepl(paste0(UpperSensorParameterName,'_'),files[i*2],fixed = T)|!grepl(paste0(LowerSensorParameterName,'_'),files[i*2-1],fixed = T)) {
                      warningfunc('Sensor parameter name not found in file name. Check that only PPFD data is in the folder and that the same parameter names are used for the upper and lower sensors at each site. If the station name and the parameter name are not separated by an _ in the file name then this can also cause this warning.') #check that the second file alphabetically is the upper sensor
                    }
                    T2 = read.csv(paste0(input.directory, '/', files[i*2-1]), sep = sep, dec = dec, header = header) # should be lower sensor
                    T1 = read.csv(paste0(input.directory, '/', files[i*2]), sep = sep, dec = dec, header = header)  # should be upper sensor
                    logdata <- rbind(logdata,paste('Processing upper sensor file:',files[i*2],'and lower sensor file:',files[i*2-1]))
                  }else{
                    upperfirst<-T
                    if(!grepl(paste0(UpperSensorParameterName,'_'),files[i*2-1],fixed = T)|!grepl(paste0(LowerSensorParameterName,'_'),files[i*2],fixed = T)) {
                      warningfunc('Sensor parameter name not found in file name. Check that only PPFD data is in the folder and that the same parameter names are used for the upper and lower sensors at each site. If the station name and the parameter name are not separated by an _ in the file name then this can also cause this warning.') #check that the second file alphabetically is the upper sensor
                    }
                    T1 = read.csv(paste0(input.directory, '/', files[i*2-1]), sep = sep, dec = dec, header = header) # should be upper sensor
                    T2 = read.csv(paste0(input.directory, '/', files[i*2]), sep = sep, dec = dec, header = header)  # should be lower sensor
                    logdata <- rbind(logdata,paste('Processing upper sensor file:',files[i*2-1],'and lower sensor file:',files[i*2]))
                  }
                }else{ #if no directory is given
                  upperfirst<-T
                  T1 = DataUpper
                  T2 = DataLower
                  if(!is.data.frame(T1)|!is.data.frame(T2)){stop('Entered DataUpper and DataLower must be dataframes')}
                  if(is.null(input.directory))logdata <- rbind(logdata,paste('Loaded dataframe from R environment:', ifelse(is.data.frame(DataUpper),deparse(substitute(DataUpper)),itqv(DataUpper)),'and',ifelse(is.data.frame(DataLower),deparse(substitute(DataLower)),itqv(DataLower))))
                }

                #format data
                incProgress(amount = 1/8, message = paste("Formatting data: file set",i,"of",nfiles/2))
                message('formatting data')
                FT1 = dspk.TableFormatting(Data = T1, Value = Value, DateTime = DateTime, datetime.format = datetime.format, datetime.timezone = datetime.timezone, NAvalue = val.NAvalue, state.of.value.code.na = NA.state.of.value.code, state.of.value.code = unchecked.state.of.value.code, add.original.data = add.original.data)
                FT2 = dspk.TableFormatting(Data = T2, Value = Value, DateTime = DateTime, datetime.format = datetime.format, datetime.timezone = datetime.timezone, NAvalue = val.NAvalue, state.of.value.code.na = NA.state.of.value.code, state.of.value.code = unchecked.state.of.value.code, add.original.data = add.original.data)

                #save formatted data
                message('saving formatted data')
                if(upperfirst){
                  write.csv(FT1,paste0(dir1,'/',files[i*2-1],'.formatted.csv'),row.names = F)
                  write.csv(FT2,paste0(dir1,'/',files[i*2],'.formatted.csv'),row.names = F)
                }else{
                  write.csv(FT1,paste0(dir1,'/',files[i*2],'.formatted.csv'),row.names = F)
                  write.csv(FT2,paste0(dir1,'/',files[i*2-1],'.formatted.csv'),row.names = F)
                }

                #save original data values into new columns for later graphical checks and restoring double spikes
                FT1$orig.values = FT1$dspk.Values
                FT2$orig.values = FT2$dspk.Values

                #min max filter
                incProgress(amount = 1/8, message = paste("Min Max filter PPFD: file set",i,"of",nfiles/2))
                message('min max filter')
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'Min max filter PPFD')
                if(is.null(Min)){Min = -Inf}
                if(is.null(Max)){Max = Inf}
                #--conditional min max--
                #if all the conditional variables are filled in and the given vectors are all the same length
                if(!is.null(ConditionalMinMaxColumn)&!is.null(ConditionalMinMaxValues)&!is.null(ConditionalMin)&!is.null(ConditionalMax)&
                   length(ConditionalMinMaxValues)==length(ConditionalMin)&length(ConditionalMax)==length(ConditionalMin)){
                  conditionalvalue <- as.character(dspk.vectorize(ConditionalMinMaxColumn, Data = FT1)[1])#get the conditional value out of the datatable
                  minmaxindex <- which(ConditionalMinMaxValues==conditionalvalue)
                  if(length(minmaxindex)==1){ #check that there is only and atleast one mach
                    Min1<-ConditionalMin[minmaxindex]
                    Max1<-ConditionalMax[minmaxindex]
                  }else{
                    Min1 <- Min
                    Max1 <- Max
                  }
                  if(is.na(Min1)){Min1 = Min}
                  if(is.na(Max1)){Max1 = Max}
                  message(paste('conditional min max value:',conditionalvalue))
                  logdata <- rbind(logdata,paste('conditional min max value:',conditionalvalue))
                  message(paste('conditional min:',Min1))
                  logdata <- rbind(logdata,paste('conditional min:',Min1))
                  message(paste('conditional min:',Max1))
                  logdata <- rbind(logdata,paste('conditional min:',Max1))
                }else{
                  Min1 = Min
                  Max1 = Max
                }
                #if the conditional vector returns an NA because for example the current parameter was not given in the given vectors, then take the non conditional min and max
                if(is.na(Min1)){Min1 = Min}
                if(is.na(Max1)){Max1 = Max}
                Min1 = (as.numeric(Min1))
                Max1 = (as.numeric(Max1))
                MMT1 = dspk.MinMaxfilter(Data = FT1, Value = "dspk.Values", Min = Min1, Max =Max1, State.of.value.data = "dspk.StateOfValue", state.of.value.code = minmax.state.of.value.code, NAvalue = NULL,logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(MMT1$logdata))))
                MMT1 <- as.data.frame(MMT1$data)
                MMT2 = dspk.MinMaxfilter(Data = FT2, Value = "dspk.Values", Min = Min1, Max =Max1, State.of.value.data = "dspk.StateOfValue", state.of.value.code = minmax.state.of.value.code, NAvalue = NULL,logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(MMT2$logdata))))
                MMT2 <- as.data.frame(MMT2$data)

                #spike filter
                incProgress(amount = 1/8, message = paste("Despike PPFD: file set",i,"of",nfiles/2))
                message('despiking')
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'Despike PPFD')
                SFT1 = dspk.Spikefilter(Value =  MMT1$dspk.Values, NumDateTime = FT1$dspk.DateTimeNum, sampling.interval = sampling.interval, State.of.value.data = MMT1$dspk.StateOfValue, state.of.value.code = despiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method,logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(SFT1$logdata))))
                SFT1 <- as.data.frame(SFT1$data)
                SFT2 = dspk.Spikefilter(Value =  MMT2$dspk.Values, NumDateTime = FT2$dspk.DateTimeNum, sampling.interval = sampling.interval, State.of.value.data = MMT2$dspk.StateOfValue, state.of.value.code = despiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method,logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(SFT2$logdata))))
                logdata <- rbind(logdata,'')
                SFT2 <- as.data.frame(SFT2$data)

                #save despiked values to formatted dataset
                FT1$dspk.Values = SFT1$dspk.Values
                FT2$dspk.Values = SFT2$dspk.Values
                FT1$dspk.StateOfValue = SFT1$dspk.StateOfValue
                FT2$dspk.StateOfValue = SFT2$dspk.StateOfValue

                #merge data sets by time to garentee matchup of data
                message('merging upper and lower sensor datasets')
                MergeT = merge(x = FT1, y= FT2, by = 'dspk.DateTimeNum')

                #if a variable was removed in one table then delete in both
                message('Deleting value in both upper and lower sensor datasets where spike was found in only one sensor')
                con_tab1_minmaxdspk = (MergeT$dspk.StateOfValue.x == minmax.state.of.value.code|MergeT$dspk.StateOfValue.x == despiked.state.of.value.code) # variables deleted in table 1
                con_tab2_minmaxdspk = (MergeT$dspk.StateOfValue.y == minmax.state.of.value.code|MergeT$dspk.StateOfValue.y == despiked.state.of.value.code) # variables deleted in table 2
                con_tab1 = con_tab2_minmaxdspk & !con_tab1_minmaxdspk #all values that have been deleted from table 2 but not from table 1
                con_tab2 = con_tab1_minmaxdspk & !con_tab2_minmaxdspk#all values that have been deleted from table 1 but not from table 2
                MergeT$dspk.StateOfValue.x[con_tab1] = DeletedSpikeOtherSensor.state.of.value.code #change state of value to DeletedSpikeOtherSensor.state.of.value.code for samples that were deleted in the other table
                MergeT$dspk.StateOfValue.y[con_tab2] = DeletedSpikeOtherSensor.state.of.value.code
                MergeT$dspk.Values.x[con_tab1] = NA #delete those values that were deleted in the other table
                MergeT$dspk.Values.y[con_tab2] = NA
                logdata <- rbind(logdata,paste(sum(con_tab1),'values were deleted (min/max and spikes) from the lower sensor but not the upper sensor. These values are now also deleted from the upper sensor.'))
                logdata <- rbind(logdata,paste(sum(con_tab2),'values were deleted (min/max and spikes) from the upper sensor but not the lower sensor. These values are now also deleted from the lower sensor.'))

                #if a record was despiked in both sensors then don't delete them
                message('Restoring original values to spikes found in both upper and lower sensors')
                con_tab1_dspk = (MergeT$dspk.StateOfValue.x == despiked.state.of.value.code) # records despiked in table 1
                con_tab2_dspk = (MergeT$dspk.StateOfValue.y == despiked.state.of.value.code) # records despiked in table 2
                con = con_tab1_dspk & con_tab2_dspk
                MergeT$dspk.StateOfValue.x[con] = NotDeletedSpikeBothSensors.state.of.value.code #change state of value to NotDeletedSpikeBothSensors.state.of.value.code for samples that were spikes in both tables
                MergeT$dspk.StateOfValue.y[con] = NotDeletedSpikeBothSensors.state.of.value.code
                MergeT$dspk.Values.x[con] = MergeT$orig.values.x[con] #reinstate the original values
                MergeT$dspk.Values.y[con] = MergeT$orig.values.y[con]
                logdata <- rbind(logdata,paste(sum(con),'values were spikes in both the upper and lower sensor datasets. Their original values have been restored.'))

                #save despiked data
                message('saving initial PPFD despiked data')
                write.csv(MergeT,paste0(dir3,'/merged.',files[i*2-1],'.minmax.despiked.csv'),row.names = F)

                #gap interpolation of gaps
                incProgress(amount = 1/8, message = paste("Interpolate gaps PPFD: file set",i,"of",nfiles/2))
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'interpolating gaps in PPFD data')
                message('interpolating gaps in PPFD data')
                GIT1 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.x', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.x', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(GIT1$logdata))))
                GIT1 <- as.data.frame(GIT1$data)
                GIT2 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.y', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.y', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(GIT2$logdata))))
                GIT2 <- as.data.frame(GIT2$data)

                #save interpolated data to the merged table
                MergeT$dspk.Values.x = GIT1$dspk.Values
                MergeT$dspk.Values.y = GIT2$dspk.Values
                #I will not add the state of value codes from the interpolation to the new data so that i can see where the interpolated data came from

                #calculating kd light attenuation coeficient
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'Calculate kd')
                message('calculating kd between upper and lower sensors')
                # kd = 1/Δz*ln(E1/E2) where Δz is the distance between sensors 40cm and E1 is the upper sensor and E2 is the lower sensor
                #MergeT$PPFDdiff = MergeT$dspk.Values.x - MergeT$dspk.Values.y
                MergeT$kd = 1/Dist.Sensors*log(MergeT$dspk.Values.x / MergeT$dspk.Values.y) #calculating light attenuation coeficient

                #save initial kd data
                message('saving initial interpolated and kd data')
                write.csv(MergeT,paste0(dir4,'/merged.',files[i*2-1],'.minmax.despiked.interpol.diff.kd.csv'),row.names = F)

                #Delete all PPFD data values where kd is negative
                #light cannot be greater lower in the water column
                message('deleting all PPFD values where kd is negative')
                con <- MergeT$kd < 0 & !is.na(MergeT$kd)
                MergeT$dspk.kd <- MergeT$kd #save original kd into a new cloumn
                MergeT$dspk.kd[con] <- NA
                MergeT$dspk.Values.x[con] <- NA
                MergeT$dspk.Values.y[con] <- NA
                MergeT$dspk.StateOfValue.x[con] <- DeletedNegativeKd.state.of.value.code
                MergeT$dspk.StateOfValue.y[con] <- DeletedNegativeKd.state.of.value.code
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,paste(sum(con),'kd values were negative. These data points were deleted from the PPFD data.'))

                ##do not report all kd values where upper sensor is less than 1 and lower sensor is less than 0.25
                message('removing all kd values where PPFD is outside of detection limits for kd')
                con <- (MergeT$dspk.Values.x < kddlupper | MergeT$dspk.Values.y < kddllower ) & !is.na(MergeT$dspk.Values.x) & !is.na(MergeT$dspk.Values.y)
                MergeT$dspk.kd[con] <- NA
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,paste(sum(con),'values had PPFD values below the detection limits ( kddllower =',kddllower,', kddlupper =',kddlupper,') for calculating reliable kd values. These kd values were deleted.'))

                #despike kd and delete those spikes also from PPFD
                incProgress(amount = 1/8, message = paste("Despike kd: file set",i,"of",nfiles/2))
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'despike kd and remove PPFD values where kd spikes')
                message('despiking kd and remove PPFD values where kd spikes')
                kddspk <- dspk.Spikefilter(Value =  MergeT$dspk.kd, NumDateTime = MergeT$dspk.DateTimeNum, sampling.interval = sampling.interval, State.of.value.data = MergeT$dspk.StateOfValue.x, state.of.value.code = kdDespiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method, logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(kddspk$logdata))))
                kddspk <- as.data.frame(kddspk$data)
                con <- kddspk$dspk.StateOfValue == kdDespiked.state.of.value.code #spike in kd

                MergeT$dspk.kd[con] <- NA
                MergeT$dspk.Values.x[con] <- NA
                MergeT$dspk.Values.y[con] <- NA
                MergeT$dspk.StateOfValue.x[con] <- kdDespiked.state.of.value.code
                MergeT$dspk.StateOfValue.y[con] <- kdDespiked.state.of.value.code

                #make a unified state of value for kd
                #255 missing
                #110 not evaluated
                #80 good
                #94 deleted (either min/max or spike in one sensor)
                #95 spike in both sensors not deleted
                #96 deleted spike in kd
                #97 negative kd value
                MergeT$dspk.StateOfValue <- MergeT$dspk.StateOfValue.x
                con <- MergeT$dspk.StateOfValue.x == minmax.state.of.value.code |MergeT$dspk.StateOfValue.x == despiked.state.of.value.code
                MergeT$dspk.StateOfValue[con]<- DeletedSpikeOtherSensor.state.of.value.code #rename all the deleted records during the PPFD despikign DeletedSpikeOtherSensor.state.of.value.code

                #interpolate PPFD again
                incProgress(amount = 1/8, message = paste("Interpolate PPFD again: file set",i,"of",nfiles/2))
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'interpolate PPFD data gaps again')
                message('interpolating PPFD data gaps again')
                GIT1 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.x', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.x', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(GIT1$logdata))))
                GIT1 <- as.data.frame(GIT1$data)
                GIT2 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.y', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.y', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
                logdata <- rbind(logdata,t(t(unlist(GIT2$logdata))))
                GIT2 <- as.data.frame(GIT2$data)
                #save interpolated data to the merged table
                MergeT$dspk.Values.x = GIT1$dspk.Values
                MergeT$dspk.Values.y = GIT2$dspk.Values
                #I will not add the state of value codes from the interpolation to the new data so that i can see where the interpolated data came from

                #calculating kd light attenuation coeficient again
                # kd = 1/Δz*ln(E1/E2) where Δz is the distance between sensors 40cm and E1 is the upper sensor and E2 is the lower sensor
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'Recalculate kd')
                message('recalculating kd between upper and lower sensors')
                #MergeT$dspk.PPFDdiff = MergeT$dspk.Values.x - MergeT$dspk.Values.y
                MergeT$dspk.kd <- 1/Dist.Sensors*log(MergeT$dspk.Values.x / MergeT$dspk.Values.y) #calculating light attenuation coeficient
                MergeT$dspk.kd <- round(MergeT$dspk.kd, digits = 4)

                ##do not report all kd values where upper sensor is less than 1 and lower sensor is less than 0.25
                message('removing all kd values where PPFD is outside of detection limits for kd')
                con <- (MergeT$dspk.Values.x < kddlupper | MergeT$dspk.Values.y < kddllower) & !is.na(MergeT$dspk.Values.x) & !is.na(MergeT$dspk.Values.y)& !is.na(MergeT$dspk.kd)
                MergeT$dspk.kd[con] <- NA
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,paste(sum(con),'values had PPFD values below the detection limits (kddllower =',kddllower,',kddlupper =',kddlupper,') for calculating reliable kd values. These kd values were deleted.'))
                logdata <- rbind(logdata,'')
                logdata <- rbind(logdata,'')

                #save final data
                incProgress(amount = 1/8, message = paste("Save final data: file set",i,"of",nfiles/2))
                message('saving final dataset')
                write.csv(MergeT,paste0(dir5,'/merged.',files[i*2-1],'.minmax.despiked.interpol.diff.kd.kddespiked.csv'),row.names = F)


              },
              #in case of error give file name and error message
              error=function(e){
                showNotification(paste("Formatting: File: ",files[i*2-1],'and',files[i*2]," Error: ", e$message), type = "error", duration = NULL)
                logdata <<- rbind(logdata,paste("File caused error:",files[i*2-1],'and',files[i*2]))
                logdata <<- rbind(logdata,paste("Original error:",e))
              }
              )#end of try catch

            }
          })#end with progress

          fileConn<-file(paste0(dir0,"/FunctionLogFile.txt"))
          writeLines(logdata, fileConn)
          close(fileConn)


        }else{


          #-------------Not PPFD data despiking------------
          #


          #below is a copy of the dspk.DespikingWorkflow.CSVfileBatchProcess() function

          withProgress(message = "Process: autodespiking",max = length(steps),{

            #function to identify type of input and covert to text conserving quotes and vectors
            #It is importaint that you know exactly what was entered into the function arguments
            itqv <- function(x){
              fff<-function(x){
                if(is.null(x)){return("NULL")}
                else if(is.na(x)){return("NA")}
                else if(is.character(x)){return(paste0("'",x,"'"))}
                else{return(x)}
              }
              if(length(x)>1){return(paste0("c(",paste(lapply(x, fff),collapse = ","),")"))
              }else if(length(x)<1&!is.null(x)){return("''")
              }else{return(fff(x))}
            }

            logdata <- '----Log file for batch process despiking function dspk.DespikingWorkflow.CSVfileBatchProcess()-----'
            logdata <- rbind(logdata,'---------------------------------------------------------------------------------------------------')
            logdata <- rbind(logdata,'Below you will find a quick overview of the function, a list of the arguments provided to the function and a work log')
            logdata <- rbind(logdata,'showing the output directory, original CSV files processed, summaries of each step, files-specific calculated arguments and error reports.')
            logdata <- rbind(logdata,'For support and bug reporting please contact PaliFelice.Gelsomini@uantwerpen.be')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,paste('Date and time of data processing:',format(Sys.time(),format = '%Y.%m.%d %H:%M:%S %Z')))
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'---Overview---')
            logdata <- rbind(logdata,'Despiking workflow: preproces-formatting, step 1 min max filter, step 2 despiking and step 3 data gap interpolation')
            logdata <- rbind(logdata,paste('Performing preproces and steps:',paste(steps,collapse = ',')))
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,paste('On CSV files in directory:',input.directory))
            if(is.null(input.directory))logdata <- rbind(logdata,paste('On dataframe from R environment:', ifelse(is.data.frame(Data),deparse(substitute(Data)),itqv(Data))))
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'---Function Arguments:---')
            logdata <- rbind(logdata,paste('steps =',itqv(steps)))
            logdata <- rbind(logdata,paste('input.directory =', itqv(input.directory),', Data =',ifelse(is.data.frame(Data),deparse(substitute(Data)),itqv(Data))))
            logdata <- rbind(logdata,paste('preprocessing:'))
            logdata <- rbind(logdata,paste('sep =',itqv(sep),', dec =', itqv(dec), ', header =', itqv(header),', add.original.data =', itqv(add.original.data)))
            logdata <- rbind(logdata,paste('Value =',itqv(Value), ', val.NAvalue =',itqv(val.NAvalue)))
            logdata <- rbind(logdata,paste('unchecked.state.of.value.code =', itqv(unchecked.state.of.value.code), ', NA.state.of.value.code =', itqv(NA.state.of.value.code)))
            logdata <- rbind(logdata,paste('DateTime =', itqv(DateTime), ', datetime.format =', itqv(datetime.format), ', datetime.timezone =', itqv(datetime.timezone)))
            logdata <- rbind(logdata,paste('Step 1 min max filter:'))
            logdata <- rbind(logdata,paste('ConditionalMinMaxColumn =', itqv(ConditionalMinMaxColumn), ', ConditionalMinMaxValues =', itqv(ConditionalMinMaxValues), ', ConditionalMin =', itqv(ConditionalMin), ', ConditionalMax =', itqv(ConditionalMax)))
            logdata <- rbind(logdata,paste('Min =', itqv(Min), ', Max =', itqv(Max), ', minmax.state.of.value.code =', itqv(minmax.state.of.value.code)))
            logdata <- rbind(logdata,paste('Step 2 despiking:'))
            logdata <- rbind(logdata,paste('sampling.interval =', itqv(sampling.interval), ', despiked.state.of.value.code =', itqv(despiked.state.of.value.code), ', good.state.of.value.code =', itqv(good.state.of.value.code), ', despike.threshold =', itqv(despike.threshold), ', despike.Method =', itqv(despike.Method)))
            logdata <- rbind(logdata,paste('Step 3 data gap interpolation:'))
            logdata <- rbind(logdata,paste('precision =', itqv(precision), ', max.gap =', itqv(max.gap), ', interpolated.state.of.value.code =', itqv(interpolated.state.of.value.code)))

            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'---Work log---')

            #check that steps are entered correctly
            if(!is.vector(steps))  stop('steps must be entered in as a numeric vector of one through three e.g. c(1,2,3)')
            if(!is.numeric(steps)) stop('steps must be entered in as a numeric vector of one through three e.g. c(1,2,3)')
            message('performing steps:')
            message(steps)


            #create a unique directory with the current time to save data into
            CurrentTime <- format(Sys.time(),"%Y%m%d%H%M%S")
            #create directory if it doen't exist, else add a number to the end till it doen't exist
            dir0 <- paste0("autodespike",CurrentTime)
            if(dir.exists(dir0)){
              dup <- 1
              while(dir.exists(paste0(dir0,'_',dup))){dup <- dup+1}
              dir0 <- paste0(dir0,'_',dup)
            }
            dir.create(dir0)
            message(paste("created directory:",dir0))
            logdata <- rbind(logdata,paste("All data saved into output directory:",dir0))

            dir1 <- paste0(dir0,"/preprocFormat")
            dir2 <- paste0(dir0,"/step1MinMax")
            dir3 <- paste0(dir0,"/step2Despike")
            dir4 <- paste0(dir0,"/step3Interpol.FinalData")

            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')


            #Formatting--------------
            #Note check if the table is already formated
            output.directory <- dir1
            directory<-input.directory
            file.name.note <- '.formatted'
            NAvalue = ''
            if(is.null(directory)){ #if no directory is given
              f<-1                  #then we can only process one file
            }else{
              #list files in directory
              files <- list.files(directory)
              logdata <- rbind(logdata,paste('Files in input directory:',paste(files,collapse = ',')))
              #number of files in the directory
              f <- length(files)
            }

            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')
            message('--------------');message('--------------');message('--------------')

            for(J in 1:f){  #load all the files in the directory. Files 1 to f
              tryCatch({   #in case of error, catch error and tell me the file name
                message(paste("Pre-process: File formatting: file",J,"of",f))
                incProgress(amount = 1/f, message = paste("Pre-process: File formatting: file",J,"of",f))
                logdata <- rbind(logdata,'')


                if(!is.null(directory)){
                  #path for file number J
                  CONfile <- paste(directory,files[J],sep = "/")

                  #--file data--
                  #load the Data table from the text file
                  CSV.table <- read.table(CONfile, sep= sep, header = header,dec = dec)
                  logdata <- rbind(logdata,paste('Preprocess: formatting: file',files[J]))
                }else{
                  files <- "InputData"
                  CSV.table <- Data
                  if(!(is.null(CSV.table)|is.data.frame(CSV.table))){stop('"Data =" input must either be NULL or a dataframe.')}
                  if(is.null(input.directory))logdata <- rbind(logdata,paste('Loaded dataframe from R environment:', ifelse(is.data.frame(Data),deparse(substitute(Data)),itqv(Data))))
                }
                #--run the batched process and save it to a new table--
                New.table <- dspk.TableFormatting(Data = CSV.table, Value = Value, DateTime = DateTime, NAvalue = val.NAvalue, datetime.format = datetime.format, datetime.timezone = datetime.timezone, state.of.value.code = unchecked.state.of.value.code, state.of.value.code.na = NA.state.of.value.code, add.original.data = add.original.data)


                #--save data to csv's--
                #create directory output.directory if it doen't exist
                if(!dir.exists(output.directory)){
                  dir.create(output.directory)
                  message(paste("created directory:",output.directory))
                }
                write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

              },
              #in case of error give file name and error message
              error=function(e){
                showNotification(paste("Formatting: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
                logdata <<- rbind(logdata,paste("File caused error:",files[J]))
                logdata <<- rbind(logdata,paste("Original error:",e))
              }
              )#end of try catch
            }#end of for loop
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')


            #min max-----------
            if(1 %in% steps){
              directory = dir1
              output.directory = dir2
              file.name.note = '.minmax'
              sep = ','
              dec = '.'
              header = T
              NAvalue = ''
              message('--------------');message('--------------');message('--------------')

              #list files in directory
              files <- list.files(directory)
              #number of files in the directory
              f <- length(files)
              for(J in 1:f){  #load all the files in the directory. Files 1 to f
                tryCatch({   #in case of error, catch error and tell me the file name
                  message(paste("Step 1: min/max: file",J,"of",f))
                  incProgress(amount = 1/f, message = paste("Step 1: min/max: file",J,"of",f))
                  logdata <- rbind(logdata,'')
                  logdata <- rbind(logdata,paste("Step 1: min/max: file ",files[J]))

                  #path for file number J
                  CONfile <- paste(directory,files[J],sep = "/")

                  #--file data--
                  #load the Data table from the text file
                  CSV.table <- read.table(CONfile, sep= sep, header = header,dec = dec)

                  #--conditional min max--
                  #if all the conditional variables are filled in and the given vectors are all the same length
                  if(is.null(Min)){Min = -Inf}
                  if(is.null(Max)){Max = Inf}
                  if(!is.null(ConditionalMinMaxColumn)&!is.null(ConditionalMinMaxValues)&!is.null(ConditionalMin)&!is.null(ConditionalMax)&
                     length(ConditionalMinMaxValues)==length(ConditionalMin)&length(ConditionalMax)==length(ConditionalMin)){
                    conditionalvalue <- as.character(dspk.vectorize(ConditionalMinMaxColumn, Data = CSV.table)[1])#get the conditional value out of the datatable
                    minmaxindex <- which(ConditionalMinMaxValues==conditionalvalue)
                    if(length(minmaxindex)==1){ #check that there is only and atleast one mach
                      Min1<-ConditionalMin[minmaxindex]
                      Max1<-ConditionalMax[minmaxindex]
                    }else{
                      Min1 <- Min
                      Max1 <- Max
                    }
                    if(is.na(Min1)){Min1 = Min}
                    if(is.na(Max1)){Max1 = Max}
                    message(paste('conditional min max value:',conditionalvalue))
                    logdata <- rbind(logdata,paste('conditional min max value:',conditionalvalue))
                    message(paste('conditional min:',Min1))
                    logdata <- rbind(logdata,paste('conditional min:',Min1))
                    message(paste('conditional min:',Max1))
                    logdata <- rbind(logdata,paste('conditional min:',Max1))
                  }else{
                    Min1 = Min
                    Max1 = Max
                  }
                  #if the conditional vector returns an NA because for example the current parameter was not given in the given vectors, then take the non conditional min and max
                  if(is.na(Min1)){Min1 = Min}
                  if(is.na(Max1)){Max1 = Max}
                  Min1 = (as.numeric(Min1))
                  Max1 = (as.numeric(Max1))

                  #--run the batched process and save it to a new table--
                  New.table <- dspk.MinMaxfilter(Data = CSV.table, Value = "dspk.Values", Min = Min1, Max = Max1, State.of.value.data = "dspk.StateOfValue", state.of.value.code = minmax.state.of.value.code, NAvalue = NULL,logoutput = T)
                  logdata <- rbind(logdata,t(t(unlist(New.table$logdata))))
                  New.table <- as.data.frame(New.table$data)
                  #!!!!!!!!!This is a work around for not adding all the data from the before function
                  CSV.table$dspk.Values <- New.table$dspk.Values
                  CSV.table$dspk.StateOfValue <- New.table$dspk.StateOfValue
                  New.table <- CSV.table

                  #--save data to csv's--
                  #create directory output.directory if it doen't exist
                  if(!dir.exists(output.directory)){
                    dir.create(output.directory)
                    message(paste("created directory:",output.directory))
                  }
                  write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

                },
                #in case of error give file name and error message
                error=function(e){
                  showNotification(paste("Formatting: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
                  logdata <<- rbind(logdata,paste("File caused error:",files[J]))
                  logdata <<- rbind(logdata,paste("Original error:",e))
                }
                )#end of try catch
              }#end of for loop
            }
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')


            #despike-------
            if(2 %in% steps){
              if(1 %in% steps){directory = dir2}else{directory = dir1}
              output.directory = dir3
              file.name.note = '.despiked'
              sep = ','
              dec = '.'
              header = T
              NAvalue = ''

              message('--------------');message('--------------');message('--------------')

              #list files in directory
              files <- list.files(directory)
              #number of files in the directory
              f <- length(files)
              for(J in 1:f){  #load all the files in the directory. Files 1 to f
                tryCatch({   #in case of error, catch error and tell me the file name
                  message(paste("Step 2: despike: file",J,"of",f))
                  incProgress(amount = 1/f, message = paste("Step 2: despike: file",J,"of",f))
                  logdata <- rbind(logdata,'')
                  logdata <- rbind(logdata,paste("Step 2: despike: file ",files[J]))

                  #path for file number J
                  CONfile <- paste(directory,files[J],sep = "/")

                  #--file data--
                  #load the Data table from the text file
                  CSV.table <- read.table(CONfile, sep= sep, header = header,dec = dec)

                  #--get sampling interval--
                  if(is.null(DateTime)){ #if a date time was not given, then the datetime comumn is just the samples numbered so the interval is 1
                    sampling.interval2<-1 #this would be done by the subfunction dspk.Spikefilter but it needs to be done here first since technically a datatime was entered into that subfunction from the previous step
                  }else{sampling.interval2<-sampling.interval}

                  #--run the batched process and save it to a new table--
                  New.table <- dspk.Spikefilter(Data = CSV.table, Value = "dspk.Values", NumDateTime = "dspk.DateTimeNum", sampling.interval = sampling.interval2, State.of.value.data = "dspk.StateOfValue", state.of.value.code = despiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method,logoutput = T)
                  logdata <- rbind(logdata,t(t(unlist(New.table$logdata))))
                  New.table <- as.data.frame(New.table$data)
                  #!!!!!!!!!This is a work around for not adding all the data from the before function
                  CSV.table$dspk.Values <- New.table$dspk.Values
                  CSV.table$dspk.StateOfValue <- New.table$dspk.StateOfValue
                  New.table <- CSV.table

                  #--save data to csv's--
                  #create directory output.directory if it doen't exist
                  if(!dir.exists(output.directory)){
                    dir.create(output.directory)
                    message(paste("created directory:",output.directory))
                  }
                  write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

                },
                #in case of error give file name and error message
                error=function(e){
                  showNotification(paste("Formatting: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
                  logdata <<- rbind(logdata,paste("File caused error:",files[J]))
                  logdata <<- rbind(logdata,paste("Original error:",e))
                }
                )#end of try catch
              }#end of for loop
            }
            logdata <- rbind(logdata,'')
            logdata <- rbind(logdata,'')


            #interpolate----------
            if(3 %in% steps){

              if(2 %in% steps){
                directory = dir3
              }else if((1 %in% steps)){
                directory = dir2
              }else{
                directory = dir1
              }
              output.directory = dir4
              file.name.note = '.interpol'
              sep = ','
              dec = '.'
              header = T
              NAvalue = ''

              message('--------------');message('--------------');message('--------------')

              #list files in directory
              files <- list.files(directory)
              #number of files in the directory
              f <- length(files)
              for(J in 1:f){  #load all the files in the directory. Files 1 to f
                tryCatch({   #in case of error, catch error and tell me the file name
                  logdata <- rbind(logdata,'')
                  message(paste("Step 3: gap interpolation: file",J,"of",f))
                  incProgress(amount = 1/f, message = paste("Step 3: gap interpolation: file",J,"of",f))
                  logdata <- rbind(logdata,paste("Step 3: gap interpolation: file ",files[J]))

                  #path for file number J
                  CONfile <- paste(directory,files[J],sep = "/")

                  #--file data--
                  #load the Data table from the text file
                  CSV.table <- read.table(CONfile, sep= sep, header = header,dec = dec)

                  #--run the batched process and save it to a new table--
                  New.table <- dspk.DataGapInterpolation(Data = CSV.table, Value = "dspk.Values", precision = precision, NumDateTime = "dspk.DateTimeNum", max.gap = max.gap, State.of.value.data = "dspk.StateOfValue", state.of.value.code = interpolated.state.of.value.code, NAvalue = NULL,logoutput = T)
                  logdata <- rbind(logdata,t(t(unlist(New.table$logdata))))
                  New.table <- as.data.frame(New.table$data)
                  #!!!!!!!!!This is a work around for not adding all the data from the before function
                  CSV.table$dspk.Values <- New.table$dspk.Values
                  #CSV.table$dspk.StateOfValue <- New.table$dspk.StateOfValue  I am not going to code the interpolations so we know what they were before being interpolated
                  New.table <- CSV.table

                  #--save data to csv's--
                  #create directory output.directory if it doen't exist
                  if(!dir.exists(output.directory)){
                    dir.create(output.directory)
                    message(paste("created directory:",output.directory))
                  }
                  write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

                },
                #in case of error give file name and error message
                error=function(e){
                  showNotification(paste("Formatting: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
                  logdata <<- rbind(logdata,paste("File caused error:",files[J]))
                  logdata <<- rbind(logdata,paste("Original error:",e))
                }
                )#end of try catch
              }#end of for loop
            }


            fileConn<-file(paste0(dir0,"/FunctionLogFile.txt"))
            writeLines(logdata, fileConn)
            close(fileConn)
          })
        }

        lastfolder$path <- dir0

















      })
      observeEvent(input$somethingthatwillneverhappenm,{


        nsteps <- step1+step2+step3+1
        withProgress(message = "Process: autodespiking",max = nsteps,{
          #create a unique directory with the current time to save data into
          CurrentTime <- format(Sys.time(),"%Y%m%d%H%M%S")
          #create directory if it doen't exist, else add a number to the end till it doen't exist
          dir0 <- paste0("autodespike",CurrentTime)
          if(dir.exists(dir0)){
            dup <- 1
            while(dir.exists(paste0(dir0,'_',dup))){dup <- dup+1}
            dir0 <- paste0(dir0,'_',dup)
          }
          dir.create(dir0)
          message(paste("created directory:",dir0))

          dir1 <- paste0(dir0,"/preprocFormat")
          dir2 <- paste0(dir0,"/step1MinMax")
          dir3 <- paste0(dir0,"/step2Despike")
          dir4 <- paste0(dir0,"/step3Interpol.FinalData")


          #Formatting--------------
          #Note check if the table is already formated
          output.directory = dir1
          directory=input.directory
          file.name.note = '.formatted'
          NAvalue = ''
          if(is.null(directory)){ #if no directory is given
            f<-1                  #then we can only process one file
          }else{
            #list files in directory
            files <- list.files(directory)
            #number of files in the directory
            f <- length(files)
          }
          message('--------------')
          message('--------------')
          message('--------------')
          for(J in 1:f){  #load all the files in the directory. Files 1 to f
            tryCatch({   #in case of error, catch error and tell me the file name
              incProgress(amount = 1/f, message = paste("Pre-process: File formatting: file",J,"of",f))

              if(!is.null(directory)){
                #path for file number J
                CONfile <- paste(directory,files[J],sep = "/")

                #--file data--
                #load the Data table from the text file
                CSV.table <- read.table(CONfile, sep= sep, na.strings = c(NAvalue, "NA"), header = header,dec = dec)
              }else{
                files <- "InputData"
                CSV.table <- NULL}
              #--run the batched process and save it to a new table--
              New.table <- dspk.TableFormatting(Data = CSV.table, Value = Value, DateTime = DateTime, NAvalue = val.NAvalue, datetime.format = datetime.format, datetime.timezone = datetime.timezone, state.of.value.code = unchecked.state.of.value.code, add.original.data = add.original.data)


              #--save data to csv's--
              #create directory output.directory if it doen't exist
              if(!dir.exists(output.directory)){
                dir.create(output.directory)
                message(paste("created directory:",output.directory))
              }
              write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

            },
            #in case of error give file name and error message
            error=function(e){
              showNotification(paste("Formatting: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
            }
            )#end of try catch
          }#end of for loop


          #min max-----------
          if(step1){
            directory = dir1
            output.directory = dir2
            file.name.note = '.minmax'
            sep = ','
            dec = '.'
            header = T
            NAvalue = ''
            message('--------------')
            message('--------------')
            message('--------------')
            #list files in directory
            files <- list.files(directory)
            #number of files in the directory
            f <- length(files)
            for(J in 1:f){  #load all the files in the directory. Files 1 to f
              tryCatch({   #in case of error, catch error and tell me the file name
                incProgress(amount = 1/f, message = paste("Step 1: min/max: file",J,"of",f))

                #path for file number J
                CONfile <- paste(directory,files[J],sep = "/")

                #--file data--
                #load the Data table from the text file
                CSV.table <- read.table(CONfile, sep= sep, na.strings = c(NAvalue, "NA"), header = header,dec = dec)

                #--Conditionald min max--
                if(input$ConditionalMinMaxdspk){
                  tryCatch({ #try to that if there is an error it doesn't cras the app
                    if(input$condMinMaxColumnIsNumdspk){#the column number was entered
                      convalue <- dspk.vectorize(as.numeric(input$conditionalMinMaxColumndspk), Data = CSV.table)[1] #CSV.table[[conditionalMinMaxColumn]][1] #get the first value in the metadata column
                    }else{
                      convalue <- dspk.vectorize(input$conditionalMinMaxColumndspk, Data = CSV.table)[1] #CSV.table[[conditionalMinMaxColumn]][1] #get the first value in the metadata column
                    }
                    convalues <- eval(parse(text = paste0('as.character(quote(c(',input$conditionalMinMaxValuesdspk,')))[-1]'))) #code for turning "a,b,c" into "a","b","c"
                    conmin <- as.numeric(eval(parse(text = paste0('c(',input$conditionalMinMaxMindspk,')')))) #vector of mins
                    conmax <- as.numeric(eval(parse(text = paste0('c(',input$conditionalMinMaxMaxdspk,')')))) #vector of maxs
                    conindex <- match(convalue,convalues) #finding the vector indes of the record that maches that found in the data table
                    if(!is.na(conindex)){ #if there was a mach found with that from in the datatable
                      Min1 <- conmin[conindex]
                      Max1 <- conmax[conindex]
                      if(is.na(Min1)|is.na(Max1)){ #if no min or max was found then just take the default values
                        showNotification('Check that the lists of conditional mins and maxs are of equal lengths. Default values were taken instead of conditional values.',type = 'error')
                        Min1<-Min
                        Max1<-Max
                      }else{
                        showNotification(paste('Conditional Min Max with value:',convalue,'min:',Min1,'max:',Max1))
                      }

                    }else{
                      Min1<-Min
                      Max1<-Max
                    }
                  },
                  error=function(e){ #incase of error, take the given non conditional min max values
                    showNotification(paste("Min/Max conditional error: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
                    Min1<-Min
                    Max1<-Max
                  })
                }else{
                  Min1<-Min
                  Max1<-Max
                }

                #--run the batched process and save it to a new table--
                New.table <- dspk.MinMaxfilter(Data = CSV.table, Value = "dspk.Values", Min = Min1, Max = Max1, State.of.value.data = "dspk.StateOfValue", state.of.value.code = minmax.state.of.value.code, NAvalue = NULL)
                #!!!!!!!!!This is a work around for not adding all the data from the before function
                CSV.table$dspk.Values <- New.table$dspk.Values
                CSV.table$dspk.StateOfValue <- New.table$dspk.StateOfValue
                New.table <- CSV.table

                #--save data to csv's--
                #create directory output.directory if it doen't exist
                if(!dir.exists(output.directory)){
                  dir.create(output.directory)
                  message(paste("created directory:",output.directory))
                }
                write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

              },
              #in case of error give file name and error message
              error=function(e){
                showNotification(paste("Min/Max filter: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)

              }
              )#end of try catch
            }#end of for loop
          }



          #despike-------
          if(step2){
            if(step1){directory = dir2}else{directory = dir1}
            output.directory = dir3
            file.name.note = '.despiked'
            sep = ','
            dec = '.'
            header = T
            NAvalue = ''
            message('--------------')
            message('--------------')
            message('--------------')
            #list files in directory
            files <- list.files(directory)
            #number of files in the directory
            f <- length(files)
            for(J in 1:f){  #load all the files in the directory. Files 1 to f
              tryCatch({   #in case of error, catch error and tell me the file name
                incProgress(amount = 1/f, message = paste("Step 2: despike: file",J,"of",f))

                #path for file number J
                CONfile <- paste(directory,files[J],sep = "/")

                #--file data--
                #load the Data table from the text file
                CSV.table <- read.table(CONfile, sep= sep, na.strings = c(NAvalue, "NA"), header = header,dec = dec)

                #--get sampling interval--
                if(is.na(sampling.interval)){
                  a <- CSV.table$dspk.DateTimeNum #save time to vector
                  a <- na.omit(a[-1]-a[-length(a)]) #subtract times from eachother shifted by one
                  sampling.interval2<-dspk.getmode(a[a>0]) #find the minimum time difference that is greater than 0
                  showNotification(paste('sampling interval calculated as to be:',sampling.interval2))
                }else{sampling.interval2<-sampling.interval}
                if(is.na(DateTime)){ #no date time data was given so sampling interval is just 1 since the records are numbered consecutivly
                  sampling.interval2<-1
                }

                #--run the batched process and save it to a new table--
                New.table <- dspk.Spikefilter(Data = CSV.table, Value = "dspk.Values", NumDateTime = "dspk.DateTimeNum", sampling.interval = sampling.interval2, State.of.value.data = "dspk.StateOfValue", state.of.value.code = despiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method)
                #!!!!!!!!!This is a work around for not adding all the data from the before function
                CSV.table$dspk.Values <- New.table$dspk.Values
                CSV.table$dspk.StateOfValue <- New.table$dspk.StateOfValue
                New.table <- CSV.table

                #--save data to csv's--
                #create directory output.directory if it doen't exist
                if(!dir.exists(output.directory)){
                  dir.create(output.directory)
                  message(paste("created directory:",output.directory))
                }
                write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

              },
              #in case of error give file name and error message
              error=function(e){
                showNotification(paste("Despiking: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)
              }
              )#end of try catch
            }#end of for loop
          }

          #interpolate----------
          if(step3){

            if(step2){
              directory = dir3
            }else if((step1)){
              directory = dir2
            }else{
              directory = dir1
            }
            output.directory = dir4
            file.name.note = '.interpol'
            sep = ','
            dec = '.'
            header = T
            NAvalue = ''
            message('--------------')
            message('--------------')
            message('--------------')
            #list files in directory
            files <- list.files(directory)
            #number of files in the directory
            f <- length(files)
            for(J in 1:f){  #load all the files in the directory. Files 1 to f
              tryCatch({   #in case of error, catch error and tell me the file name
                incProgress(amount = 1/f, message = paste("Step 3: gap interpolation: file",J,"of",f))

                #path for file number J
                CONfile <- paste(directory,files[J],sep = "/")

                #--file data--
                #load the Data table from the text file
                CSV.table <- read.table(CONfile, sep= sep, na.strings = c(NAvalue, "NA"), header = header,dec = dec)

                #--find decimal places--
                if(is.na(precision)){ #if no precision was provided
                  valsint <- CSV.table$dspk.Values #save the values into a vector
                  havedecimals <- grepl('.',format(valsint,scientific = F),fixed = T)
                  if(any(havedecimals)){#do any of the values have decimal points?
                    ndecimals <- function(x){tryCatch({nchar(strsplit(format(x,scientific = F), ".", fixed = TRUE)[[1]][[2]])},error=function(e){return(0)})}

                    precision1<-eval(parse(text = paste0('1e-', max(unlist( lapply(valsint[havedecimals],ndecimals) )) )))
                  }else{
                    precision1<-1 #round to no decimal places
                  }
                  showNotification(paste('No data precision provided for rounding. Data precision taken to be',precision1))
                }else{ precision1<- precision}



                #--run the batched process and save it to a new table--
                New.table <- dspk.DataGapInterpolation(Data = CSV.table, Value = "dspk.Values", precision = precision1, NumDateTime = "dspk.DateTimeNum", max.gap = max.gap, State.of.value.data = "dspk.StateOfValue", state.of.value.code = interpolated.state.of.value.code, NAvalue = NULL)
                #!!!!!!!!!This is a work around for not adding all the data from the before function
                CSV.table$dspk.Values <- New.table$dspk.Values
                CSV.table$dspk.StateOfValue <- New.table$dspk.StateOfValue
                New.table <- CSV.table

                #--save data to csv's--
                #create directory output.directory if it doen't exist
                if(!dir.exists(output.directory)){
                  dir.create(output.directory)
                  message(paste("created directory:",output.directory))
                }
                write.csv(New.table, file = paste0(output.directory,"/",files[J],file.name.note,".csv"),row.names = F)

              },
              #in case of error give file name and error message
              error=function(e){
                showNotification(paste("Interpolation: File: ",files[J]," Error: ", e$message), type = "error", duration = NULL)

              }
              )#end of try catch
            }#end of for loop
          }
          message('--------------')
          message('--------------')
          message('--------------')

        })

        lastfolder$path <- dir0
      })


    }

  )
}








