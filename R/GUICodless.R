#Shiny App for manual data cleaning of Cont continuous data
#Pali Gelsomini ECOBE 2020


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
HIC.App.manual <- function(){
  require(shiny)
  require(colourpicker)

  markedgroupnumbering <- c(1,2,3,4,5,6,7,8,9)
  names(markedgroupnumbering) <- c('1 Marked to be deleted',2,3,4,5,6,7,8,9)

  shinyApp(
    ui <- fluidPage(
      titlePanel("Continuous Data Manual Validation and Calibration"),
      mainPanel(
        checkboxInput('PPFDdata','Working with PPFD data',value = F),
        tabsetPanel(type = "tabs",
                    tabPanel("File upload",
                             tabsetPanel(type="pills",
                                         tabPanel("Files",
                                                  fileInput("Contfile", "Choose Continuous CSV File",
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  tableOutput("ContDataHead"),
                                                  fileInput("Perifile", "Choose Periodic CSV File",
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  tableOutput("PeriDataHead"),
                                                  fileInput("Maintfile", "Choose sensor maintenance CSV File",
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  tableOutput("MaintDataHead")
                                         ),
                                         tabPanel("Options",
                                                  tabsetPanel(type="tabs",
                                                              tabPanel("Continuous data file",
                                                                       textInput("Contsep","csv file column separator",","),
                                                                       textInput("Contdec","csv file decimal indicator","."),
                                                                       textInput("tContcol","Cont continuous UNIX time column","dspk.DateTimeNum"),
                                                                       textInput("timezone","Time zone (GMT+1 = Etc/GMT-1)","Etc/GMT-1"),
                                                                       textInput("valcol","Continuous Variable column","dspk.Values"),
                                                                       textInput("statecol","State of value column","dspk.StateOfValue"),
                                                                       textInput("StNamContcol","Station name column for continuous dataset","Station.Name"),
                                                                       textInput("StNoContcol","Sation number column for continuous dataset","Station.Number"),
                                                                       textInput("parContcol","Parameter name column for continuous dataset","Parameter.Name"),
                                                                       textInput("parUnitContcol","Parameter unit column for continuous dataset","Parameter.Unit"),


                                                                       textInput("par1Cont","Parameter 1 name in continuous dataset","DO"),
                                                                       textInput("par2Cont","Parameter 2 name in continuous dataset","Chfyla"),
                                                                       textInput("par3Cont","Parameter 3 name in continuous dataset","pH"),
                                                                       textInput("par4Cont","Parameter 4 name in continuous dataset","PPFD1"),
                                                                       textInput("par5Cont","Parameter 5 name in continuous dataset",""),
                                                                       textInput("par6Cont","Parameter 6 name in continuous dataset",""),
                                                                       textInput("par7Cont","Parameter 7 name in continuous dataset",""),
                                                                       textInput("par8Cont","Parameter 8 name in continuous dataset",""),
                                                                       textInput("par9Cont","Parameter 9 name in continuous dataset",""),
                                                                       textInput("par10Cont","Parameter 10 name in continuous dataset","")),

                                                              tabPanel("Periodic data file",
                                                                       textInput("Perisep","csv file column separator",";"),
                                                                       textInput("Peridec","csv file decimal indicator","."),
                                                                       textInput("tPericol","Peri periodic time column","ReadingDate"),
                                                                       textInput("StNamPericol","Station name column for periodic dataset","StationName"),
                                                                       textInput("PeriParamcol","Column name for parameter","ParameterName"),
                                                                       textInput('PeriValuecol',"Column name for values","ReadingValue"),
                                                                       textInput("par1Pericol","Parameter 1 for periodic dataset","Oxygen"),
                                                                       textInput("par2Pericol","Parameter 2 for periodic dataset","Chlorophyll a"),
                                                                       textInput("par3Pericol","Parameter 3 for periodic dataset","pH"),
                                                                       textInput("par4Pericol","Parameter 4 for periodic dataset","light attenuation coefficient"),
                                                                       textInput("par5Pericol","Parameter 5 for periodic dataset",NULL),
                                                                       textInput("par6Pericol","Parameter 6 for periodic dataset",NULL),
                                                                       textInput("par7Pericol","Parameter 7 for periodic dataset",NULL),
                                                                       textInput("par8Pericol","Parameter 8 for periodic dataset",NULL),
                                                                       textInput("par9Pericol","Parameter 9 for periodic dataset",NULL),
                                                                       textInput("par10Pericol","Parameter 10 for periodic dataset",NULL),
                                                              ),
                                                              tabPanel("Maintenance data file",
                                                                       textInput("Maintsep","csv file column separator",","),
                                                                       textInput("Maintdec","csv file decimal indicator","."),
                                                                       textInput("tMaintcol","Maintenance UNIX time column","DateTimeUNIX"),
                                                                       textInput("diviceMaintcol","Divice column in maintenance records","Toestel"),
                                                                       textInput("actionMaintcol","Action type column in maintenance records","Staaltype")
                                                              ),
                                                              tabPanel("PPFD data",
                                                                       numericInput('DistSensors','PPFD sensor pair distance in meters',0.4),
                                                                       numericInput('kddlupper','Light attenuation coefficient kd detection limit upper sensor',1),
                                                                       numericInput('kddllower','Light attenuation coefficient kd detection limit upper sensor',0.25),
                                                                       textInput("kdparname","Light attenuation coefficient parameter name",'kd'),
                                                                       textInput("PPFDUname","PPFD upper sensor parameter name","PPFD1"),
                                                                       textInput("PPFDLname","PPFD lower sensor parameter name","PPFD")
                                                              ),
                                                              tabPanel("Graph",
                                                                       tabsetPanel(type = "tabs",
                                                                                   tabPanel("State of Value Continuous",
                                                                                            fluidRow(
                                                                                              column(6,"state of value num codes"),
                                                                                              column(6,"Graphing preferences")
                                                                                            ),
                                                                                            fluidRow(
                                                                                              column(2,"min"),
                                                                                              column(2,"max"),
                                                                                              column(2,"format"),
                                                                                              column(4,"name"),
                                                                                              column(2,"color")
                                                                                            ),
                                                                                            fluidRow("Validation work codes"),
                                                                                            fluidRow(column(2,numericInput("min1",NULL,80)),column(2,numericInput("max1",NULL,80)),column(2,numericInput("for1",NULL,NULL)),column(2,"auto good"),column(2,textInput("state1",NULL,"auto good")),column(2,colourInput("col1",NULL,value = "#696969"))),
                                                                                            fluidRow(column(2,numericInput("min2",NULL,91)),column(2,numericInput("max2",NULL,91)),column(2,numericInput("for2",NULL,NULL)),column(2,"min max filter deleted"),column(2,textInput("state2",NULL,"min max filter deleted")),column(2,colourInput("col2",NULL,value = "#e9967a"))),
                                                                                            fluidRow(column(2,numericInput("min3",NULL,92)),column(2,numericInput("max3",NULL,92)),column(2,numericInput("for3",NULL,NULL)),column(2,"spike filter deleted"),column(2,textInput("state3",NULL,"spike filter deleted")),column(2,colourInput("col3",NULL,value = "#ee1289"))),
                                                                                            fluidRow(column(2,numericInput("min4",NULL,93)),column(2,numericInput("max4",NULL,93)),column(2,numericInput("for4",NULL,NULL)),column(2,"manual interpolated"),column(2,textInput("state4",NULL,"manual interpolated")),column(2,colourInput("col4",NULL,value = "#66cd00"))),
                                                                                            fluidRow(column(2,numericInput("min5",NULL,99)),column(2,numericInput("max5",NULL,99)),column(2,numericInput("for5",NULL,99)),column(2,"manual delete"),column(2,textInput("state5",NULL,"manual delete")),column(2,colourInput("col5",NULL,value = "#009acd"))),
                                                                                            fluidRow("PPFD despiking specific codes"),
                                                                                            fluidRow(column(2,numericInput("min30",NULL,94)),column(2,numericInput("max30",NULL,94)),column(2,numericInput("for30",NULL,NULL)),column(4,textInput("state30",NULL,"deleted, deleted in other sensor")),column(2,colourInput("col30",NULL,value = "#66CD00"))),
                                                                                            fluidRow(column(2,numericInput("min31",NULL,95)),column(2,numericInput("max31",NULL,95)),column(2,numericInput("for31",NULL,NULL)),column(4,textInput("state31",NULL,"not deleted, spike in both sensors")),column(2,colourInput("col31",NULL,value = "#8D00CF"))),
                                                                                            fluidRow(column(2,numericInput("min32",NULL,96)),column(2,numericInput("max32",NULL,96)),column(2,numericInput("for32",NULL,NULL)),column(4,textInput("state32",NULL,"deleted, negative kd value")),column(2,colourInput("col32",NULL,value = "#66CD00"))),
                                                                                            fluidRow(column(2,numericInput("min33",NULL,97)),column(2,numericInput("max33",NULL,97)),column(2,numericInput("for33",NULL,NULL)),column(4,textInput("state33",NULL,"deleted, kd spike")),column(2,colourInput("col33",NULL,value = "#66CD00"))),
                                                                                            fluidRow("marked grouping codes for marking sections of data which can then be recoded to another category or can have custom mathematical corrections applied to them"),
                                                                                            fluidRow(column(2,numericInput("min21",NULL,81)),column(2,"+8"),column(6,"marked group 1 (marked to be deleted)"),column(2,colourInput("col21",NULL,value = "#d73027"))),
                                                                                            fluidRow(column(4,"min +1"),column(2,"marked group 2"),column(2,textInput("state21",NULL,"marked group")),column(2,"2"),column(2,colourInput("col22",NULL,value = "#f46d43"))),
                                                                                            fluidRow(column(4,"min +2"),column(4,"marked group 3"),column(2,"3"),column(2,colourInput("col23",NULL,value = "#fdae61"))),
                                                                                            fluidRow(column(4,"min +3"),column(4,"marked group 4"),column(2,"4"),column(2,colourInput("col24",NULL,value = "#fee090"))),
                                                                                            fluidRow(column(4,"min +4"),column(4,"marked group 5"),column(2,"5"),column(2,colourInput("col25",NULL,value = "#abd9e9"))),
                                                                                            fluidRow(column(4,"min +5"),column(4,"marked group 6"),column(2,"6"),column(2,colourInput("col26",NULL,value = "#74add1"))),
                                                                                            fluidRow(column(4,"min +6"),column(4,"marked group 7"),column(2,"7"),column(2,colourInput("col27",NULL,value = "#4575b4"))),
                                                                                            fluidRow(column(4,"min +7"),column(4,"marked group 8"),column(2,"8"),column(2,colourInput("col28",NULL,value = "#542788"))),
                                                                                            fluidRow(column(4,"min +8"),column(4,"marked group 9"),column(2,"9"),column(2,colourInput("col29",NULL,value = "#8073ac"))),
                                                                                            fluidRow("Codes list for fully validated data. These will be the codes that will be present in the final exported dataset."),
                                                                                            fluidRow(column(2,numericInput("min9",NULL,10)),column(2,numericInput("max9",NULL,19)),column(2,numericInput("for9",NULL,11)),column(2,"good"),column(2,textInput("state9",NULL,"good")),column(2,colourInput("col9",NULL,value = "#000000"))),
                                                                                            fluidRow(column(2,numericInput("min10",NULL,20)),column(2,numericInput("max10",NULL,29)),column(2,numericInput("for10",NULL,21)),column(4,textInput("state10",NULL,"good calc")),column(2,colourInput("col10",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min11",NULL,30)),column(2,numericInput("max11",NULL,39)),column(2,numericInput("for11",NULL,31)),column(2,"estimate"),column(2,textInput("state11",NULL,"estimate")),column(2,colourInput("col11",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min12",NULL,40)),column(2,numericInput("max12",NULL,49)),column(2,numericInput("for12",NULL,41)),column(4,textInput("state12",NULL,"estimate calc")),column(2,colourInput("col12",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min13",NULL,60)),column(2,numericInput("max13",NULL,69)),column(2,numericInput("for13",NULL,61)),column(4,textInput("state13",NULL,"suspect")),column(2,colourInput("col13",NULL,value = "#ff0000"))),
                                                                                            fluidRow(column(2,numericInput("min14",NULL,70)),column(2,numericInput("max14",NULL,79)),column(2,numericInput("for14",NULL,71)),column(4,textInput("state14",NULL,"suspect calc")),column(2,colourInput("col14",NULL,value = "#ff0000"))),
                                                                                            fluidRow(column(2,numericInput("min16",NULL,255)),column(2,numericInput("max16",NULL,255)),column(2,numericInput("for16",NULL,255)),column(4,textInput("state16",NULL,"missing")),column(2,colourInput("col16",NULL,value = "#009acd"))),
                                                                                            fluidRow("HIC work codes"),
                                                                                            fluidRow(column(2,numericInput("min6",NULL,111)),column(2,numericInput("max6",NULL,111)),column(2,numericInput("for6",NULL,NULL)),column(4,textInput("state6",NULL,"HIC auto good")),column(2,colourInput("col6",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min7",NULL,116)),column(2,numericInput("max7",NULL,116)),column(2,numericInput("for7",NULL,NULL)),column(4,textInput("state7",NULL,"HIC auto interpolated")),column(2,colourInput("col7",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min8",NULL,110)),column(2,numericInput("max8",NULL,179)),column(2,numericInput("for8",NULL,NULL)),column(4,textInput("state8",NULL,"unchecked")),column(2,colourInput("col8",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min15",NULL,220)),column(2,numericInput("max15",NULL,224)),column(2,numericInput("for15",NULL,NULL)),column(4,textInput("state15",NULL,"unknown import")),column(2,colourInput("col15",NULL,value = "#009acd"))),
                                                                                            fluidRow("Other HIC codes"),
                                                                                            fluidRow(column(2,numericInput("min17",NULL,-1)),column(2,numericInput("max17",NULL,-1)),column(2,numericInput("for17",NULL,NULL)),column(4,textInput("state17",NULL,"missing")),column(2,colourInput("col17",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min18",NULL,6)),column(2,numericInput("max18",NULL,6)),column(2,numericInput("for18",NULL,NULL)),column(4,textInput("state18",NULL,"external good")),column(2,colourInput("col18",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min19",NULL,7)),column(2,numericInput("max19",NULL,7)),column(2,numericInput("for19",NULL,NULL)),column(4,textInput("state19",NULL,"external estimate")),column(2,colourInput("col19",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min20",NULL,8)),column(2,numericInput("max20",NULL,8)),column(2,numericInput("for20",NULL,NULL)),column(4,textInput("state20",NULL,"external suspect")),column(2,colourInput("col20",NULL,value = "#009acd"))),
                                                                                            fluidRow("Custom codes"),
                                                                                            fluidRow(column(2,numericInput("min34",NULL,-1000)),column(2,numericInput("max34",NULL,-1000)),column(2,numericInput("for34",NULL,NULL)),column(4,textInput("state34",NULL,"fill in custom state of value")),column(2,colourInput("col34",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min35",NULL,-1000)),column(2,numericInput("max35",NULL,-1000)),column(2,numericInput("for35",NULL,NULL)),column(4,textInput("state35",NULL,"fill in custom state of value")),column(2,colourInput("col35",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min36",NULL,-1000)),column(2,numericInput("max36",NULL,-1000)),column(2,numericInput("for36",NULL,NULL)),column(4,textInput("state36",NULL,"fill in custom state of value")),column(2,colourInput("col36",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min37",NULL,-1000)),column(2,numericInput("max37",NULL,-1000)),column(2,numericInput("for37",NULL,NULL)),column(4,textInput("state37",NULL,"fill in custom state of value")),column(2,colourInput("col37",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min38",NULL,-1000)),column(2,numericInput("max38",NULL,-1000)),column(2,numericInput("for38",NULL,NULL)),column(4,textInput("state38",NULL,"fill in custom state of value")),column(2,colourInput("col38",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min39",NULL,-1000)),column(2,numericInput("max39",NULL,-1000)),column(2,numericInput("for39",NULL,NULL)),column(4,textInput("state39",NULL,"fill in custom state of value")),column(2,colourInput("col39",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min40",NULL,-1000)),column(2,numericInput("max40",NULL,-1000)),column(2,numericInput("for40",NULL,NULL)),column(4,textInput("state40",NULL,"fill in custom state of value")),column(2,colourInput("col40",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min41",NULL,-1000)),column(2,numericInput("max41",NULL,-1000)),column(2,numericInput("for41",NULL,NULL)),column(4,textInput("state41",NULL,"fill in custom state of value")),column(2,colourInput("col41",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min42",NULL,-1000)),column(2,numericInput("max42",NULL,-1000)),column(2,numericInput("for42",NULL,NULL)),column(4,textInput("state42",NULL,"fill in custom state of value")),column(2,colourInput("col42",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min43",NULL,-1000)),column(2,numericInput("max43",NULL,-1000)),column(2,numericInput("for43",NULL,NULL)),column(4,textInput("state43",NULL,"fill in custom state of value")),column(2,colourInput("col43",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min44",NULL,-1000)),column(2,numericInput("max44",NULL,-1000)),column(2,numericInput("for44",NULL,NULL)),column(4,textInput("state44",NULL,"fill in custom state of value")),column(2,colourInput("col44",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min45",NULL,-1000)),column(2,numericInput("max45",NULL,-1000)),column(2,numericInput("for45",NULL,NULL)),column(4,textInput("state45",NULL,"fill in custom state of value")),column(2,colourInput("col45",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min46",NULL,-1000)),column(2,numericInput("max46",NULL,-1000)),column(2,numericInput("for46",NULL,NULL)),column(4,textInput("state46",NULL,"fill in custom state of value")),column(2,colourInput("col46",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min47",NULL,-1000)),column(2,numericInput("max47",NULL,-1000)),column(2,numericInput("for47",NULL,NULL)),column(4,textInput("state47",NULL,"fill in custom state of value")),column(2,colourInput("col47",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min48",NULL,-1000)),column(2,numericInput("max48",NULL,-1000)),column(2,numericInput("for48",NULL,NULL)),column(4,textInput("state48",NULL,"fill in custom state of value")),column(2,colourInput("col48",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,numericInput("min49",NULL,-1000)),column(2,numericInput("max49",NULL,-1000)),column(2,numericInput("for49",NULL,NULL)),column(4,textInput("state49",NULL,"fill in custom state of value")),column(2,colourInput("col49",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(2,""),column(4,""),column(4,textInput("stateOther",NULL,"other")),column(2,colourInput("colOther",NULL,value = "#009acd")))
                                                                                   ),
                                                                                   tabPanel("Other key features",
                                                                                            fluidRow("Periodic data"),
                                                                                            fluidRow("Continuous sensor maintenance data"),
                                                                                            fluidRow(column(5,"factor level"),column(5,"color")),
                                                                                            fluidRow(column(5,textInput("MaintFact1",NULL,value = "MPS Reiniging")),column(5,colourInput("Maintcol1",NULL,value = "#0bdee6"))),
                                                                                            fluidRow(column(5,textInput("MaintFact2",NULL,value = "MPS Ophaling")),column(5,colourInput("Maintcol2",NULL,value = "#eb4034"))),
                                                                                            fluidRow(column(5,textInput("MaintFact3",NULL,value = "MPS Plaatsing")),column(5,colourInput("Maintcol3",NULL,value = "#eb4034"))),
                                                                                            fluidRow(column(5,textInput("MaintFact4",NULL,value = "custom")),column(5,colourInput("Maintcol4",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(5,textInput("MaintFact5",NULL,value = "custom")),column(5,colourInput("Maintcol5",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(5,textInput("MaintFact6",NULL,value = "custom")),column(5,colourInput("Maintcol6",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(5,textInput("MaintFact7",NULL,value = "custom")),column(5,colourInput("Maintcol7",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(5,textInput("MaintFact8",NULL,value = "custom")),column(5,colourInput("Maintcol8",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(5,textInput("MaintFact9",NULL,value = "custom")),column(5,colourInput("Maintcol9",NULL,value = "#009acd"))),
                                                                                            fluidRow(column(5,textInput("MaintFact10",NULL,value = "custom")),column(5,colourInput("Maintcol10",NULL,value = "#009acd")))
                                                                                   )
                                                                       )
                                                              )
                                                  )
                                         )

                             )
                    ),
                    #tabPanel("Auto spike filtering"

                    #),
                    tabPanel("Manual inspection",
                             fluidRow(plotOutput("plot1", brush = brushOpts(
                               id = "plot_brush",
                               resetOnNew = TRUE),
                               dblclick = "plot_dblclick")
                             ),
                             conditionalPanel(condition = 'input.PPFDdata',
                                              fluidRow(plotOutput("plot4", brush = brushOpts(
                                                id = "plot4_brush",
                                                resetOnNew = TRUE),
                                                dblclick = "plot4_dblclick")
                                              ),
                                              fluidRow(plotOutput("plot3", brush = brushOpts(
                                                id = "plot3_brush",
                                                resetOnNew = TRUE),
                                                dblclick = "plot3_dblclick")
                                              )),
                             fluidRow('Draw box on graph and double click to zoom in to drawn box. Double click on graph to zoom out to full extent'),
                             fluidRow(splitLayout(
                               checkboxInput('lockxaxis','Lock x-axis', value = F),
                               checkboxInput('lockyaxis','Lock y-axis', value = F),
                               actionButton('zoomout_toggle','zoom out 2x'),
                               tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))) #to fix issue with dropdown menus not working in split layout
                             )),
                             hr(),
                             fluidRow(
                               column(12,(actionButton("reset_toggle", "reset original data")),
                                      (actionButton("save_toggle", "save progress")),
                                      (actionButton("undo_toggle", "undo till last save")))
                             ),
                             hr(),
                             fluidRow(splitLayout(
                               (actionButton("marked_toggle", "tag points as marked")),
                               (selectInput("marked_tag", "marked grouping for later recalibration", markedgroupnumbering)))
                             ),
                             hr(),
                             fluidRow(actionButton("good_toggle", "tag points as good")),
                             hr(),
                             fluidRow(splitLayout(
                               actionButton("ctc.Brush.reclass_toggl", "Reclass points within brush"),
                               numericInput("brushfrom","from state of value",NULL),
                               numericInput("brushto","to state of value",NULL)
                             )),
                             hr(),
                             fluidRow(h5("!!!Interpolate as your last step before exporting!!! Gaps interpolated during the manual check get labeled as estimate. If you don't do this as the final step you may accidentally overwrite the estimate label.")),
                             tags$head(tags$style('h5 {color:red}')), #colors all heading 5 texts as red
                             fluidRow(
                               column(4,actionButton("interpolate_toggle","interpolate gaps"),actionButton("interpolateBrush_toggle", "interpolate gaps in brushed box"),),
                               column(6,numericInput("maxgap_interpolate","max time gap of interpolation in minutes",60))
                             ),
                             fluidRow(h2('Graphing preferences')),
                             fluidRow(
                               checkboxInput('AxisIsUNIXsec','Convert x-axis seconds to datetime',value = T),
                             ),
                             fluidRow(splitLayout(checkboxInput('legend','add legend to graph',value = F),
                                                  selectInput('legendlocal','legend location', c('topleft','top','topright','right','bottomright','bottom','bottomleft','left','center'))
                             )),
                             fluidRow(splitLayout(
                               numericInput("pointsize","Point Size",1),
                               checkboxInput("periodic_check", "Periodic Data",value = T),
                               checkboxInput("maint_check", "Maintenance Data",value = T),
                               checkboxInput("maintID_check", "Sensor ID Data",value = T)
                             )),
                             fluidRow(strong('See "File upload" >> "Options" >> "Graph" for legend entry names and colors options')),
                             p(),
                             fluidRow(verbatimTextOutput("info"))
                    ),
                    tabPanel("Correlation and calibration",
                             plotOutput("plot2", brush = brushOpts(
                               id = "plot2_brush",
                               resetOnNew = TRUE),
                               dblclick = "plot2_dblclick"),
                             splitLayout(
                               checkboxInput('legend2','add legend to graph',value = F),
                               selectInput('legendlocal2','legend location', c('topleft','top','topright','right','bottomright','bottom','bottomleft','left','center'))
                             ),
                             selectInput("calgroup","The marked group do you wish to calibrate (0 means not marked)",c(0,1,2,3,4,5,6,7,8,9),selected = 0),
                             tags$head(tags$style(HTML('#calgroup{background-color:orange}'))),
                             hr(),
                             fluidRow(strong('Calibration formulas based on the selected group. If no marked group is selected (you selected 0 above) then the calibration formulas will be based on all non-marked data that is not labeled as "suspect" or "suspect calc". However when you click the button "calibrate points" the points labeled "suspect" and "suspect calc" will still be calibrated.')),
                             verbatimTextOutput("formula"),
                             verbatimTextOutput("formulaNoInt"),
                             checkboxInput("cal.nonsus_check","use formula with no y-intercept", value = T),
                             actionButton("cal.nonsus_toggle","Auto calibrate points"),
                             hr(),
                             textInput("cal.nonsus_input", "Enter calibration formula here manually as a function of x with base R operators. If this is blank then the above automatic calibration formulas will be used.",placeholder = "example: (5 + 6*log(x)^3)/2"),
                             actionButton("cal.manual_toggle", "Manual calibrate points"),
                             tableOutput("CorTable")
                    ),
                    tabPanel("Reclassifying state of value codes",
                             fluidRow(column(10,"'Marked Grouping' --->>> 'Marked Grouping'")),
                             fluidRow(splitLayout(actionButton("sts.reclass_toggle","Reclassify"),
                                                  selectInput("sts1.sus_tag","from Group", c(1,2,3,4,5,6,7,8,9)),
                                                  selectInput("sts2.sus_tag","to Group", c(1,2,3,4,5,6,7,8,9)))
                             ),
                             fluidRow(column(10,"'Marked Grouping' --->>> Non-work-class state of value")),
                             fluidRow(splitLayout(actionButton("sto.reclass_toggle","Reclassify"),
                                                  selectInput("sto.sus_tag","Marked Group", c(1,2,3,4,5,6,7,8,9)),
                                                  uiOutput("sto.state.class"),
                                                  verbatimTextOutput("sto.code"))
                             ),
                             fluidRow(column(10,"'Marked Grouping' --->>> 'Manual Delete'")),
                             fluidRow(splitLayout(actionButton("s.delete_toggle","Delete"),
                                                  selectInput("s.delete.sus_tag","from Group", c(1,2,3,4,5,6,7,8,9))),
                             ),
                             fluidRow(column(10,"Custom state of value --->>> Custom state of value")),
                             fluidRow(splitLayout(actionButton("ctc.reclass_toggle","Reclassify"),
                                                  numericInput("ct","from class code",NULL),
                                                  numericInput("tc","to class code",NULL))
                             ),
                             fluidRow(column(10,"All work-classes except 'Marked Groupings' --->>> 'Good' state of value")),
                             fluidRow(column(2,actionButton("wtg.reclass_toggle","Reclassify"))),
                             fluidRow(tableOutput("StateOfValueTable"))
                    ),
                    tabPanel("Export",
                             fluidRow(
                               "Working Directory",
                               verbatimTextOutput("workingDirectory1")


                             ),
                             fluidRow(
                               textInput("subDirectory","Sub directory to save work log into","DataCleaning")
                             ),
                             fluidRow(
                               "Export Correlation Table",
                               textInput("subDirectory_cor","Sub directory to save correlation table into","DataCleaning"),
                               textInput("Note_cor","Note to add to start of correlation table file name","CorrelationTable_"),
                               #actionButton("export_cor_toggle", "Export Correlation Table")
                             ),
                             fluidRow(
                               "Export zrx file for HIC database import",
                               textInput("zrxoutupdirectory","Sub directory to save the zrx files into","CleanedDataZRX")
                             ),
                             fluidRow(
                               "Export Continuous Data Table",
                               textInput("subDirectory_con","Sub directory to save data table into","CleanedDataSet"),
                               textInput("Note_con","Note to add to start of file name","ContinuousData_"),
                               actionButton("export_con_toggle", "Click to Export Continuous Data csv, Continuous Data zrx, Correlation Table and Work Log"),
                               checkboxInput("deleteworklog","delete work log upon export", value = T),
                               tags$head(tags$style(HTML('#export_con_toggle{background-color:orange}')))
                             )


                    ),
                    tabPanel("Work log",
                             "Working Directory",
                             #verbatimTextOutput("workingDirectory"),
                             actionButton("clear.worklog_toggle", "Clear work log"),
                             #actionButton("export.worklog_toggle", "Export work log"),
                             tableOutput("worklog.table")
                    ),
                    tabPanel("Help",
                             HTML('<h3><a href="https://github.com/pgelsomini/HICbioclean/blob/775ccbaa22d12a78736f07a3391f4f12e62eb2bf/MANUAL-TOTORIAL-HICbioclean.-Rpackage.pdf" target="_blank" rel="noopener noreferrer">Link to manual and tutorial for this app</a></h3>'),
                             fluidRow(h4('Your working directory')),
                             fluidRow(verbatimTextOutput("workingdirectory2")),
                             htmlOutput("inc1"),
                    )


        )
      )
    ),


    #datetimeform = function(x){format(as.POSIXct(x, origin = "1970-01-01", tz= tzone), "%b-%d-%y")}


    server <- function(input, output,session) {
      session$onSessionEnded(function() { #stops the app when the window closes
        stopApp()
      })
      options(shiny.maxRequestSize=800*1024^2) #expands maximum file size for upload from 5mb to 800mb

      #Working directory--------------------------------------------------------------------------

      output$workingDirectory <- renderText(getwd())
      output$workingDirectory1 <- renderText(getwd())
      output$workingdirectory2 <- renderText(getwd())

      #Help---------------
      #Get help documents from package directory
      getPage<-function(x) {
        htmlDir <- system.file("html", x , package = "HICbioclean")
        return(includeHTML(htmlDir))
      }
      output$inc1<-renderUI({
        getPage("HIC.App.manual.html")
      })

      #Work log table------------------------------------------------------------------------------
      work <- reactiveValues(
        log = "GUI Started"
      )

      output$worklog.table <- renderTable(work$log)

      observeEvent(input$clear.worklog_toggle,{
        work$log = "Work log cleared"})

      exportworklog <- function(systime=NULL){
        if(is.null(systime))systime<-Sys.time()
        isolate(work$log <- rbind(work$log,paste("Work log exported at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        if (input$subDirectory != "") {#add a / to the end of the sub directory if present
          tryCatch({
            subdir <- paste0(input$subDirectory,"/")
            if(!dir.exists(input$subDirectory))dir.create(input$subDirectory)
          },
          #in case of error give file name and error message
          error=function(cond){
            subdir <- ""
            isolate(work$log <- rbind(work$log,paste("Failed to create new directory:",input$subDirectory))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
            return(subdir)
          }
          )

        }else{subdir <- input$subDirectory}
        write.csv(work$log,file = paste0(subdir,"WorkLog_",input$Contfile$name,"_",format(systime, "%Y%m%d_%H%M%S"),".csv"))
      }

      observeEvent(input$export.worklog_toggle,{exportworklog()})

      #file upload----------------------------------------------------------------

      #Cont continuous data-------------------------------------------------------
      #upload data
      df <- reactive({
        Contfile <- input$Contfile
        if(is.null(Contfile))
          return(NULL)
        table <- read.csv(Contfile$datapath,sep = input$Contsep, dec = input$Contdec)
        #tryCatch({table[[input$tContcol]]<-as.POSIXct(table[[input$tContcol]], tz = "Etc/GMT-1")})
        return(table)
      })

      #dataframe to be saved to
      df_saved <- reactiveValues(df=NULL)
      observe({df_saved$df <- df()})


      #vectorize data
      vals <- reactiveValues(
        tCont = NULL,
        tzone = NULL,
        valCont = NULL,
        state = NULL,
        StNamCont= NULL,
        StNoCont= NULL,
        parCont= NULL,
        parUnitCont= NULL,
        data = df
      )
      origcolumns <- list(statecol=NULL,tContcol=NULL)
      observeEvent(input$PPFDdata,{
        if(input$PPFDdata){
          origcolumns$statecol <<- input$statecol
          origcolumns$tContcol <<- input$tContcol
          origcolumns$valcol <<- input$valcol
          origcolumns$StNamContcol <<- input$StNamContcol
          origcolumns$StNoContcol <<- input$StNoContcol
          origcolumns$parContcol <<- input$parContcol
          origcolumns$parUnitContcol <<- input$parUnitContcol
          updateTextInput(session, 'statecol',value = "dspk.StateOfValue")
          updateTextInput(session, 'tContcol',value = "dspk.DateTimeNum")
          updateTextInput(session, 'valcol',value = "dspk.kd")
          updateTextInput(session, 'StNamContcol',value = paste0(input$StNamContcol,'.x'))
          updateTextInput(session, 'StNoContcol',value = paste0(input$StNoContcol,'.x'))
          updateTextInput(session, 'parContcol',value = paste0(input$parContcol,'.x'))
          updateTextInput(session, 'parUnitContcol',value = paste0(input$parUnitContcol,'.x'))

        }else{
          if(!is.null(origcolumns$statecol)){
            updateTextInput(session, 'statecol',value = origcolumns$statecol)
            updateTextInput(session, 'tContcol',value = origcolumns$tContcol)
            updateTextInput(session, 'valcol',value = origcolumns$valcol)
            updateTextInput(session, 'StNamContcol',value = origcolumns$StNamContcol)
            updateTextInput(session, 'StNoContcol',value = origcolumns$StNoContcol)
            updateTextInput(session, 'parContcol',value = origcolumns$parContcol)
            updateTextInput(session, 'parUnitContcol',value = origcolumns$parUnitContcol)
          }
        }
      })
      observe({vals$state <-  df()[[input$statecol]]})
      observe({vals$tzone <- input$timezone})
      observe({vals$tCont <- df()[[input$tContcol]]})
      observe({vals$valCont <- df()[[input$valcol]]})
      observe({vals$StNamCont <- (df()[[input$StNamContcol]])[1]})
      observe({vals$StNoCont <- (df()[[input$StNoContcol]])[1]})
      observe({vals$parCont <- (df()[[input$parContcol]])[1]})
      observe({vals$parUnitCont <- (df()[[input$parUnitContcol]])[1]})
      observe({vals$valContL <- df()[["dspk.Values.y"]]})
      observe({vals$valContU <- df()[["dspk.Values.x"]]})

      #Make data table headsor displaying under file upload
      output$ContDataHead <- renderTable({
        Contfile <- input$Contfile
        isolate(work$log <- NULL)
        isolate(work$log <- rbind(work$log,paste("loaded continuous data file:",Contfile$name))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        return(head(df(),n=2))
      })


      #Exporting augmented data table
      observeEvent(input$export_con_toggle,{
        withProgress(message = "Process running: exporting augmented continuous data table", value = 0,{ #code for showing a progress bar and message in shiny app when this function is run
          systime <- Sys.time()
          finaldata <- exportzrxfile(systime=systime) #format final data for export and make zrx file for use at the HIC

          #preparing table
          datatable <- df()

          if(!input$PPFDdata){
            datatable[[input$statecol]] <- finaldata$sov #vals$state
            incProgress(1/3) #increment counter for the progress bar
            datatable[[input$valcol]] <- finaldata$values #vals$valCont
            incProgress(1/3) #increment counter for the progress bar
          }
          if(input$PPFDdata){
            datatable[[input$statecol]] <- finaldata$sovkd #vals$state
            incProgress(1/3) #increment counter for the progress bar
            datatable[[input$valcol]] <- finaldata$values #vals$valCont
            incProgress(1/3) #increment counter for the progress bar
            datatable[["dspk.Values.y"]] <- finaldata$valuesL
            datatable[["dspk.Values.x"]] <- finaldata$valuesU
            datatable[["dspk.StateOfValue.y"]] <- finaldata$sovL
            datatable[["dspk.StateOfValue.x"]] <- finaldata$sovU
          }

          if (input$subDirectory_con != "") {#add a / to the end of the sub directory if present
            tryCatch({
              subdir <- paste0(input$subDirectory_con,"/")
              if(!dir.exists(input$subDirectory_con))dir.create(input$subDirectory_con)
            },
            #in case of error give file name and error message
            error=function(cond){
              subdir <- ""
              isolate(work$log <- rbind(work$log,paste("Failed to create new directory:",input$subDirectory_con))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
              return(subdir)
            }
            )

          }else{subdir <- input$subDirectory_con}
          write.csv(datatable,file = paste0(subdir,input$Note_con,input$Contfile$name,"_",format(systime, "%Y%m%d_%H%M%S"),".csv"),row.names = F)
          isolate(work$log <- rbind(work$log,paste("continuous data exported to",paste0(subdir,input$Note_con,input$Contfile$name,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv"),"at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
          try(exportcorelationtable(systime),silent = T)
          exportworklog(systime)
          if(input$deleteworklog)work$log <- NULL
        })
      })

      #exporting function for zrx file
      exportzrxfile <- function(systime=NULL,precision=NULL){
        if(is.null(systime))systime<-Sys.time()
        #formatting data for HIC
        times <- vals$tCont #get times
        values <- vals$valCont #get values
        if(input$PPFDdata){ #get PPFD data values too
          valuesU <- vals$valContU
          valuesL <- vals$valContL
        }
        sov <- vals$state #get state of values
        stationno <- vals$StNoCont #get station number
        parameter <- vals$parCont #get parameter


        times <- format(as.POSIXct(times, origin = "1970-01-01", tz=  "Etc/GMT-1"), "%Y%m%d%H%M") #convert the UNIX time to datetime format in timezone UTC+1

        if(!input$PPFDdata){ #if not PPFD data
          #turn all original NA values back to -777 and missing data value 255
          #turn all new NA values to -88888 and attribute quality flag 161
          originalsov <- df()[[input$statecol]]

          con <- originalsov == input$for16 & !is.na(originalsov) & is.na(values) #all values that are missing now and were originally missing
          values[con] <- -777
          sov[con] <- input$for16

          con <- is.na(values) #all the values that are still NA should be ones that I deleted and were not originally missing
          values[con] <- -88888
          sov[con] <- 161

          #turn manual interpolate to estimate state of value
          con <- sov == input$min4
          sov[con] <- input$for11

          #turn all remaining values that are not already set as final export values to the good state of value
          con <- (!(sov >= input$min9 & sov <= input$max14)|is.na(sov)) & values != -777 & values != -88888
          sov[con] <- input$for9
        }


        if(input$PPFDdata){ #if working with PPFD data
          #turn all original NA values back to -777 and missing data value 255
          #turn all new NA values to -88888 and attribute quality flag 161
          originalsov <- df()[[input$statecol]]
          originaldataU <- df()[["orig.values.x"]]
          originaldataL <- df()[["orig.values.y"]]

          sovkd <- sov #make individual state of values for each of the parameters
          sovU <- sov
          sovL <- sov

          #if original data is NA and current data is also NA then -777 and missing data state of value
          #for kd missing in either PPFD datasets
          con <- is.na(values)  & (is.na(originaldataU)|is.na(originaldataL))
          values[con] <- -777
          sovkd[con] <- input$for16 #missing state of value
          con <- is.na(valuesU)  & is.na(originaldataU)
          valuesU[con] <- -777
          sovU[con] <- input$for16
          con <- is.na(valuesL)  & is.na(originaldataL)
          valuesL[con] <- -777
          sovL[con] <- input$for16

          #the remaining NA values in the PPFD must have been deleted manually
          con <- is.na(valuesU)|is.na(valuesL) #if PPFD data is NA, don't look for NA in the kd data
          values[con] <- -88888
          sovkd[con] <- 161
          con <- is.na(valuesU)
          valuesU[con] <- -88888
          sovU[con] <- 161
          con <- is.na(valuesL)
          valuesL[con] <- -88888
          sovL[con] <- 161

          #all remaining na values in kd where calculation of kd was not possible but data was not removed
          con<-is.na(values)
          values[con]<- -777
          #don't change state of values

          #turn manual interpolate to estimate state of value
          con <- sovkd == input$max4
          sovU[con] <- input$for11 #estimate
          sovL[con] <- input$for11
          sovkd[con] <- input$for12 #estimate calc

          con <- sovkd == input$for13 #is tagged as suspect
          sovkd[con] <- input$for14 #suspect calc state of value for kd since it is a calculation

          con <- sovkd == input$for11 #is tagged as estimate (was calibrated)
          sovU[con] <- input$for13 #tag PPFD as suspect because it wasn't transformed
          sovL[con] <- input$for13
          sovkd[con] <- input$for12 #estimate calc state of value for kd

          con <- sovkd == input$for9 #is good
          sovkd[con] <- input$for10 #good calc state of value for kd


          #turn all remaining values that are not already set as final export values to the good state of value
          con <- (!((sovU >= input$min9 & sovU <= input$max14)|sovU >= input$for16 |sovU == 161) |is.na(sovU))
          sovU[con] <- input$for9 #good state of value
          con <- (!((sovL >= input$min9 & sovL <= input$max14)|sovL >= input$for16 |sovL == 161) |is.na(sovL))
          sovL[con] <- input$for9
          con <- (!((sovkd >= input$min9 & sovkd <= input$max14)|sovkd >= input$for16 |sovkd == 161) |is.na(sovkd))
          sovkd[con] <- input$for10 #good calc state of value for kd since kd is a calculation

          #values that are missing from the other sensor will be labeled as suspect since the sensors are paired and much of the despiking gets done with this assumption
          conU <- valuesU == -777 | valuesU == -88888
          conL <- valuesL == -777 | valuesL == -88888
          conJustOne <- xor(conU,conL)
          con <- !conU & conL  #conJustOne
          sovU[con] <- input$for13 #suspect state of value
          con <- !conL & conU  #conJustOne
          sovL[con] <- input$for13 #suspect state of value


        }

        #format the decimal places in the values
        #getting decimal precision
        decimalplacesfomatting <- function(values,precision){
          if(is.null(precision)){ #if no precision was provided
            havedecimals <- grepl('.',format(values,scientific = F),fixed = T)
            if(any(havedecimals)){#do any of the values have decimal points?
              ndecimals <- function(x){tryCatch({nchar(strsplit(format(x,scientific = F), ".", fixed = TRUE)[[1]][[2]])},error=function(e){return(0)})}

              precision<-max(unlist( lapply(values[havedecimals],ndecimals) ))
            }else{
              precision<-0 #round to no decimal places
            }
            message(paste('No data precision provided for rounding for zrx file output. Number of decimal places taken to be',precision))
            isolate(work$log <- rbind(work$log,paste('No data precision provided for rounding for zrx file output. Number of decimal places taken to be',precision))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
          }else{
            isolate(work$log <- rbind(work$log,paste('Data precision provided for rounding for zrx file output. Number of decimal places given as',precision))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
          }
          values <- format(values, nsmall = precision)
          return(values)
        }
        if(!input$PPFDdata)values <- decimalplacesfomatting(values,precision)
        if(input$PPFDdata){
          values <- decimalplacesfomatting(values,precision=NULL)
          valuesU <- decimalplacesfomatting(valuesU,precision)
          valuesL <- decimalplacesfomatting(valuesL,precision)
        }

        #data rows
        if(!input$PPFDdata){
          datarows <- t(t(paste0(times,'\t',values,'\t',sov))) #paste the data together tab separated, then make a table then pivot the table
          finaldata <- data.frame(values,sov)
        }
        if(input$PPFDdata){
          datarowskd <- t(t(paste0(times,'\t',values,'\t',sovkd)))
          datarowsU <- t(t(paste0(times,'\t',valuesU,'\t',sovU)))
          datarowsL <- t(t(paste0(times,'\t',valuesL,'\t',sovL)))
          finaldata <- data.frame(values,sovkd,valuesU,sovU,valuesL,sovL)
        }


        #make Header
        #get name parameter code
        makeheader <- function(parameter){
          zrxcodes <- zrxFileStationCodes #read.csv('zrxFileStationCodes.csv',sep = ',',dec = '.')
          concode <- which(zrxcodes[,1]==stationno & zrxcodes[,2]==parameter)[1]
          if(is.na(concode)){
            showNotification('The combination of site number and parameter name was not found in zrxFileStationCodes. The current site number and parameter will be used as the code.', duration = 10)
            code <- paste0(stationno,"-",parameter)
            unit <- vals$parUnitCont #get unite
          }else{
            code <- zrxcodes[,3][concode]
            unit <- zrxcodes[,4][concode]
          }

          header <- paste0('#REXCHANGE',code,'|*|RINVAL-777|*|')
          header <- rbind(header,paste0('#TZUTC+1|*|CUNIT',unit,'|*|'))
          header <- rbind(header,'#LAYOUT(timestamp,value,primary_status)|*|')
          return(header)
        }
        if(!input$PPFDdata) header <- makeheader(parameter)
        if(input$PPFDdata){
          headerkd <- makeheader(parameter=input$kdparname)
          headerU <- makeheader(parameter=input$PPFDUname)
          headerL <- makeheader(parameter=input$PPFDLname)
        }

        if(!dir.exists(input$zrxoutupdirectory)){ #if the directory doesn't exist then make it
          dir.create(input$zrxoutupdirectory)
        }
        writezrxhicfile <- function(parameter,header,datarows){
          fileConn<-file(paste0(input$zrxoutupdirectory,"/",stationno,"_",parameter,'_',format(systime, "%Y%m%d_%H%M%S"),".zrx"), open = "wt") #creates a file and the open = wt means we can keep writing lines to it
          writeLines(header, fileConn) #writing header
          writeLines(datarows, fileConn)#writing values
          close(fileConn)
          isolate(work$log <- rbind(work$log,paste("continuous data exported to zrx file for HIC database import in ",paste0(input$zrxoutupdirectory,"/",stationno,"_",parameter,'_',format(systime, "%Y%m%d_%H%M%S"),".zrx"),"at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        }
        if(!input$PPFDdata) writezrxhicfile(parameter=parameter,header=header,datarows=datarows)
        if(input$PPFDdata){
          writezrxhicfile(parameter=input$kdparname,header=headerkd,datarows=datarowskd)
          writezrxhicfile(parameter=input$PPFDUname,header=headerU,datarows=datarowsU)
          writezrxhicfile(parameter=input$PPFDLname,header=headerL,datarows=datarowsL)
        }
        return(finaldata)

      }

      #Peri periodic data-------------------------------------------------------
      #upload data
      par1Cont <- reactive(input$par1Cont) #I need to make reactive elements of the parameter codes otherwise they won't update if i change them in the options
      par2Cont <- reactive(input$par2Cont)
      par3Cont <- reactive(input$par3Cont)
      par4Cont <- reactive(input$par4Cont)
      par5Cont <- reactive(input$par5Cont)
      par6Cont <- reactive(input$par6Cont)
      par7Cont <- reactive(input$par7Cont)
      par8Cont <- reactive(input$par8Cont)
      par9Cont <- reactive(input$par9Cont)
      par10Cont <- reactive(input$par10Cont)
      Peri <- reactive({ #save the datatable into a reactive element
        Perifile <- input$Perifile
        if(is.null(Perifile)) return(NULL)
        linktab<-NULL
        try(linktab <- ReferenceSiteLinkage) #read.csv("ReferenceSiteLinkage.csv",sep = ',',dec = '.'))
        if(is.null(linktab)){showNotification('Counld not find the reference site linkage data. Try reinstalling HICbioclean package')} #Please place the file "ReferenceSiteLinkage.csv" into your working directory',type = 'error')}
        table <- read.csv(Perifile$datapath,sep = input$Perisep, dec = input$Peridec)

        table <- subset(table,table[[input$StNamPericol]]==linktab$MonthlySites[which(linktab$ContinuousSites==vals$StNoCont)]) #subset table to just have the site from the continuous data


        if(is.null(vals$parCont)) return(NULL)
        if(vals$parCont==par1Cont()){
          param <- input$par1Pericol
        }else if(vals$parCont==par2Cont()){
          param <- input$par2Pericol
        }else if(vals$parCont==par3Cont()){
          param <- input$par3Pericol
        }else if(vals$parCont==par4Cont()){
          param <- input$par4Pericol
        }else if(vals$parCont==par5Cont()){
          param <- input$par5Pericol
        }else if(vals$parCont==par6Cont()){
          param <- input$par6Pericol
        }else if(vals$parCont==par7Cont()){
          param <- input$par7Pericol
        }else if(vals$parCont==par8Cont()){
          param <- input$par8Pericol
        }else if(vals$parCont==par9Cont()){
          param <- input$par9Pericol
        }else if(vals$parCont==par10Cont()){
          param <- input$par10Pericol
        }else{
          param <- NULL}

        table <- subset(table, table[[input$PeriParamcol]]==param)

        return(table)
      })

      #vectorize data
      valsPeri <- reactiveValues( #create reactive values to store vectors in
        tPeri = NULL,
        StNamPeri= NULL,
        valPeri= NULL,
        data = Peri
      )
      observe({valsPeri$tPeri <- as.numeric(as.POSIXct(as.character(Peri()[[input$tPericol]]),format = "%d/%m/%Y %H:%M", tz = "Etc/GMT-1"))
      }) #save the data columns into the reactive values
      observe({valsPeri$StNamPeri <- (Peri()[[input$StNamPericol]])[1]
      })
      observe({valsPeri$valPeri <- Peri()[[input$PeriValuecol]]
      })




      #Make data table headsor displaying under file upload
      output$PeriDataHead <- renderTable({
        Perifile <- input$Perifile
        isolate(work$log <- rbind(work$log,paste("loaded periodic data file:",Perifile$name))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        return(head(Peri(),n=2))
      })

      #Correlation table---------------------------------------------------

      continuous.mach.to.periodic <- reactive({
        periodictime <- valsPeri$tPeri
        continuoustime <- vals$tCont[!is.na(vals$tCont)]#there cannot be NA in the vector
        return(findInterval(periodictime,continuoustime))
      }) #index numbers of the price is right matches for the continuous time matched to the periodic time

      cortab <- reactive({
        con <- !is.na(vals$tCont) #only non-NA times from continuous data
        valCont <- vals$valCont[con]
        state <- vals$state[con]
        tCont <- vals$tCont[con]

        valPeri <- valsPeri$valPeri #periodic data
        tPeri <- valsPeri$tPeri
        cortable <- data.frame(tPeri,valPeri)
        #if (!is.null(valsPeri$bdlPeri)) {
        #  cortable <- cbind(cortable,valsPeri$bdlPeri)
        #  names(cortable)[3]<-"bdlPeri"
        #}
        cortable$valCont <- NA #add the continuous data columns
        cortable$state <- NA
        cortable$tCont <- NA
        cortable$corID <- row.names(cortable) #number the data points for graphing


        for (i in 1:nrow(cortable)) {
          j <- continuous.mach.to.periodic()[i] #match index
          if(j!=0){
            if((cortable$tPeri[i]-tCont[j]>tCont[j+1]-tCont[j])|!is.na(tCont[j+1])){ #if the time difference to the previous point is greater than to the second point and the second point exists then take the second point
              j<-j+1 #take the next value
            }
            if(abs(cortable$tPeri[i]-tCont[j])<901){  #is the match withing the past 15 minutes?
              cortable$valCont[i] <- valCont[j] #add the continuous data values where it matches
              cortable$state[i] <- state[j]
              cortable$tCont[i] <- tCont[j]
            }
          }
        }

        return(cortable)
      })

      #saves the point numbers to a new table so that it doesn't recalculate these everytime data is changed on the big graph
      corIDs <- reactive(cortab()[,c("tPeri","valPeri","corID")])

      #a subset of just the data from the selected marked group for making corelation and calibration calculations
      goodtab <- reactive({
        vec <- cortab()$state
        grp <- input$calgroup
        if(grp==0){ #if 0 then take all non marked and non suspect values
          cond <- vec!=input$min20&(vec<input$min13|vec>input$max14)&(vec<input$min21|vec>(input$min21+8)) #not marked or suspect
        }else{ #else take all marked values in chosen group
          cond <- vec== input$min21+as.numeric(grp)-1 & !is.na(vec)
        }
        goodvalCont <- cortab()$valCont[cond]
        goodvalPeri <- cortab()$valPeri[cond]
        df <- data.frame(goodvalCont,goodvalPeri)

        return(df)

      })

      #display the correlation table on the GUI
      output$CorTable <- renderTable({tryCatch({cortab()},
                                               error = function(e){return(NULL)})
      })


      #export correlation table to csv
      exportcorelationtable <- function(systime=NULL){
        if(is.null(systime))systime<-Sys.time()
        withProgress(message = "Process running: exporting corelation table", value = 0,{ #code for showing a progress bar and message in shiny app when this function is run
          if (input$subDirectory_cor != "") {#add a / to the end of the sub directory if present
            tryCatch({
              subdir <- paste0(input$subDirectory_cor,"/")
              if(!dir.exists(input$subDirectory_cor))dir.create(input$subDirectory_cor)
            },
            #in case of error give file name and error message
            error=function(cond){
              subdir <- ""
              isolate(work$log <- rbind(work$log,paste("Failed to create new directory:",input$subDirectory_cor))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
              return(subdir)
            }
            )

          }else{subdir <- input$subDirectory_con}
          write.csv(cortab(),file = paste0(subdir,input$Note_cor,input$Contfile$name,"_",format(systime, "%Y%m%d_%H%M%S"),".csv"))
          isolate(work$log <- rbind(work$log,paste("corelation data exported to",paste0(subdir,input$Note_cor,input$Contfile$name,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv"),"at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        })
      }

      observeEvent(input$export_cor_toggle,{exportcorelationtable()})

      #maintenance data-------------------------------------------------------
      #upload data
      Maint <- reactive({
        Maintfile <- input$Maintfile
        if(is.null(Maintfile))
          return(NULL)
        table <- read.csv(Maintfile$datapath,sep = input$Maintsep, dec = input$Maintdec)
        return(table)
      })

      #vectorize data
      valsMaint <- reactiveValues(
        tMaint = NULL,
        diviceMaint= NULL,
        actionMaint= NULL,
        data = Maint
      )
      observe({valsMaint$tMaint <- Maint()[[input$tMaintcol]]})
      observe({valsMaint$diviceMaint <- as.factor(Maint()[[input$diviceMaintcol]])})
      observe({valsMaint$actionMaint <- as.factor(Maint()[[input$actionMaintcol]])})


      #Make data table headsor displaying under file upload
      output$MaintDataHead <- renderTable({
        Maintfile <- input$Maintfile
        isolate(work$log <- rbind(work$log,paste("loaded sensor maintenance data file:",Maintfile$name))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        return(head(Maint(),n=2))})



      #Graphs--------------------------------------------------------------------------------------


      #function for reformating the x axes from UNIX time to Date time. This is much faster than reformating all the data
      datetimeform = function(x){format(as.POSIXct(x, origin = "1970-01-01", tz= input$timezone), "%b-%d-%y %H:%M")}
      #function for setting State levels
      f1= function(x){
        #incProgress(1/length(stateofval)) #to track the progress of the function since it is a quite long running function
        ifelse(x>=input$min1&x<=input$max1,as.character(input$state1),
               ifelse(x>=input$min2&x<=input$max2,as.character(input$state2),
                      ifelse(x>=input$min3&x<=input$max3,as.character(input$state3),
                             ifelse(x>=input$min4&x<=input$max4,as.character(input$state4),
                                    ifelse(x>=input$min5&x<=input$max5,as.character(input$state5),
                                           ifelse(x>=input$min6&x<=input$max6,as.character(input$state6),
                                                  ifelse(x>=input$min7&x<=input$max7,as.character(input$state7),
                                                         ifelse(x>=input$min8&x<=input$max8,as.character(input$state8),
                                                                ifelse(x>=input$min9&x<=input$max9,as.character(input$state9),
                                                                       ifelse(x>=input$min10&x<=input$max10,as.character(input$state10),
                                                                              ifelse(x>=input$min11&x<=input$max11,as.character(input$state11),
                                                                                     ifelse(x>=input$min12&x<=input$max12,as.character(input$state12),
                                                                                            ifelse(x>=input$min13&x<=input$max13,as.character(input$state13),
                                                                                                   ifelse(x>=input$min14&x<=input$max14,as.character(input$state14),
                                                                                                          ifelse(x>=input$min15&x<=input$max15,as.character(input$state15),
                                                                                                                 ifelse(x>=input$min16&x<=input$max16,as.character(input$state16),
                                                                                                                        ifelse(x>=input$min17&x<=input$max17,as.character(input$state17),
                                                                                                                               ifelse(x>=input$min18&x<=input$max18,as.character(input$state18),
                                                                                                                                      ifelse(x>=input$min19&x<=input$max19,as.character(input$state19),
                                                                                                                                             ifelse(x>=input$min20&x<=input$max20,as.character(input$state20),
                                                                                                                                                    ifelse(x==input$min21,'to be deleted',
                                                                                                                                                           ifelse(x>=(input$min21+1)&x<=(input$min21+8),paste(as.character(input$state21),(x-input$min21+1)), #marked groupings
                                                                                                                                                                  ifelse(x>=input$min30&x<=input$max30,as.character(input$state30),
                                                                                                                                                                         ifelse(x>=input$min31&x<=input$max31,as.character(input$state31),
                                                                                                                                                                                ifelse(x>=input$min32&x<=input$max32,as.character(input$state32),
                                                                                                                                                                                       ifelse(x>=input$min33&x<=input$max33,as.character(input$state33),
                                                                                                                                                                                              ifelse(x>=input$min34&x<=input$max34,as.character(input$state34),
                                                                                                                                                                                                     ifelse(x>=input$min35&x<=input$max35,as.character(input$state35),
                                                                                                                                                                                                            ifelse(x>=input$min36&x<=input$max36,as.character(input$state36),
                                                                                                                                                                                                                   ifelse(x>=input$min37&x<=input$max37,as.character(input$state37),
                                                                                                                                                                                                                          ifelse(x>=input$min38&x<=input$max38,as.character(input$state38),
                                                                                                                                                                                                                                 ifelse(x>=input$min39&x<=input$max39,as.character(input$state39),
                                                                                                                                                                                                                                        ifelse(x>=input$min40&x<=input$max40,as.character(input$state40),
                                                                                                                                                                                                                                               ifelse(x>=input$min41&x<=input$max41,as.character(input$state41),
                                                                                                                                                                                                                                                      ifelse(x>=input$min42&x<=input$max42,as.character(input$state42),
                                                                                                                                                                                                                                                             ifelse(x>=input$min43&x<=input$max43,as.character(input$state43),
                                                                                                                                                                                                                                                                    ifelse(x>=input$min44&x<=input$max44,as.character(input$state44),
                                                                                                                                                                                                                                                                           ifelse(x>=input$min45&x<=input$max45,as.character(input$state45),
                                                                                                                                                                                                                                                                                  ifelse(x>=input$min46&x<=input$max46,as.character(input$state46),
                                                                                                                                                                                                                                                                                         ifelse(x>=input$min47&x<=input$max47,as.character(input$state47),
                                                                                                                                                                                                                                                                                                ifelse(x>=input$min48&x<=input$max48,as.character(input$state48),
                                                                                                                                                                                                                                                                                                       ifelse(x>=input$min49&x<=input$max49,as.character(input$state49),as.character(input$stateOther)))))))))))))))))))))))))))))))))))))))))))

      }



      #make color scheme for the "State of value" and "sensor maintenence"
      col.scheme.names <- reactive(c(as.character(input$state1),
                                     as.character(input$state2),
                                     as.character(input$state3),
                                     as.character(input$state4),
                                     as.character(input$state5),
                                     as.character(input$state6),
                                     as.character(input$state7),
                                     as.character(input$state8),
                                     as.character(input$state9),
                                     as.character(input$state10),
                                     as.character(input$state11),
                                     as.character(input$state12),
                                     as.character(input$state13),
                                     as.character(input$state14),
                                     as.character(input$state15),
                                     as.character(input$state16),
                                     as.character(input$state17),
                                     as.character(input$state18),
                                     as.character(input$state19),
                                     as.character(input$state20),
                                     'to be deleted',#paste(as.character(input$state21),1),
                                     paste(as.character(input$state21),2),
                                     paste(as.character(input$state21),3),
                                     paste(as.character(input$state21),4),
                                     paste(as.character(input$state21),5),
                                     paste(as.character(input$state21),6),
                                     paste(as.character(input$state21),7),
                                     paste(as.character(input$state21),8),
                                     paste(as.character(input$state21),9),
                                     as.character(input$state30),
                                     as.character(input$state31),
                                     as.character(input$state32),
                                     as.character(input$state33),
                                     as.character(input$state34),
                                     as.character(input$state35),
                                     as.character(input$state36),
                                     as.character(input$state37),
                                     as.character(input$state38),
                                     as.character(input$state39),
                                     as.character(input$state40),
                                     as.character(input$state41),
                                     as.character(input$state42),
                                     as.character(input$state43),
                                     as.character(input$state44),
                                     as.character(input$state45),
                                     as.character(input$state46),
                                     as.character(input$state47),
                                     as.character(input$state48),
                                     as.character(input$state49),
                                     as.character(input$stateOther),
                                     as.character(input$MaintFact1),
                                     as.character(input$MaintFact2),
                                     as.character(input$MaintFact3),
                                     as.character(input$MaintFact4),
                                     as.character(input$MaintFact5),
                                     as.character(input$MaintFact6),
                                     as.character(input$MaintFact7),
                                     as.character(input$MaintFact8),
                                     as.character(input$MaintFact9),
                                     as.character(input$MaintFact10)))
      #color scale for ECOBE data validation
      myColors <- reactive(c(as.character(input$col1),
                             as.character(input$col2),
                             as.character(input$col3),
                             as.character(input$col4),
                             as.character(input$col5),
                             as.character(input$col6),
                             as.character(input$col7),
                             as.character(input$col8),
                             as.character(input$col9),
                             as.character(input$col10),
                             as.character(input$col11),
                             as.character(input$col12),
                             as.character(input$col13),
                             as.character(input$col14),
                             as.character(input$col15),
                             as.character(input$col16),
                             as.character(input$col17),
                             as.character(input$col18),
                             as.character(input$col19),
                             as.character(input$col20),
                             as.character(input$col21),
                             as.character(input$col22),
                             as.character(input$col23),
                             as.character(input$col24),
                             as.character(input$col25),
                             as.character(input$col26),
                             as.character(input$col27),
                             as.character(input$col28),
                             as.character(input$col29),
                             as.character(input$col30),
                             as.character(input$col31),
                             as.character(input$col32),
                             as.character(input$col33),
                             as.character(input$col34),
                             as.character(input$col35),
                             as.character(input$col36),
                             as.character(input$col37),
                             as.character(input$col38),
                             as.character(input$col39),
                             as.character(input$col40),
                             as.character(input$col41),
                             as.character(input$col42),
                             as.character(input$col43),
                             as.character(input$col44),
                             as.character(input$col45),
                             as.character(input$col46),
                             as.character(input$col47),
                             as.character(input$col48),
                             as.character(input$col49),
                             as.character(input$colOther),
                             as.character(input$Maintcol1),
                             as.character(input$Maintcol2),
                             as.character(input$Maintcol3),
                             as.character(input$Maintcol4),
                             as.character(input$Maintcol5),
                             as.character(input$Maintcol6),
                             as.character(input$Maintcol7),
                             as.character(input$Maintcol8),
                             as.character(input$Maintcol9),
                             as.character(input$Maintcol10)))



      #make plot continuous values

      #variable for storing zoom extent coordinates
      ranges <- reactiveValues(x = NULL, y = NULL, y3=NULL, y4=NULL)
      observe(if(isolate(!input$lockxaxis)){ranges$x <- range(vals$tCont, na.rm = T, finite = T)})
      observe(if(isolate(!input$lockyaxis)){ranges$y <- range( vals$valCont, na.rm = T, finite = T)})
      observe(if(isolate(!input$lockyaxis)){ranges$y3 <- range( vals$valContL, na.rm = T, finite = T)})
      observe(if(isolate(!input$lockyaxis)){ranges$y4 <- range( vals$valContU, na.rm = T, finite = T)})

      #setting factor levels for state of value that are understandable for continuous graph
      State <- function(x){
        a <- as.factor(x) #set state of value codes as a factor
        b <- as.numeric(levels(a)) #put the levels of the state of value codes into a vector
        c <- sapply(b, f1) #convert that vector of levels which are originally numbers into descriptions
        levels(a) <- c #set the state of value codes levels to the descriptive ones
        return(a)
      }


      output$plot1 <- renderPlot({
        if(is.null(vals$tCont)){
          plot.new()
          title('no data')
        }else{
          #setting State of values
          State <- State(vals$state)

          #setting up color scale
          col.names <- col.scheme.names()
          Colors <- myColors()
          names(Colors) <- col.names
          #colScale <- scale_colour_manual(name = "State of values",values = Colors)

          #graphing plot
          # factor levels
          Statelevels <- levels(as.factor(levels(State)))
          nState <- length(Statelevels)

          # get the range for the x and y axis
          xrange <- ranges$x
          yrange <- ranges$y

          # set up the plot
          #setup x-axis
          if(input$PPFDdata){labelname <- 'kd'}else{labelname <- vals$parCont}
          if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
            plot(xrange, yrange, type="n", xlab="Time",
                 ylab=labelname, xaxt = "n" )
            step <- round(diff(xrange)/4, digits = 0)
            marks <- seq(xrange[1]+step,xrange[2]-step,step)
            lab <- datetimeform(marks)
            axis(1, at = marks, labels = lab)
          }else{
            plot(xrange, yrange, type="n", xlab="Time",
                 ylab=labelname )
          }

          # add lines
          #add thin gray line over whole working dataset
          lines(vals$tCont, vals$valCont, type="l", lwd=0.5, col='gray')
          #add working dataset with state of values
          for (i in 1:nState) {
            con <- State == Statelevels[i]
            subtime <- vals$tCont[con]
            subval <- vals$valCont[con]
            lines(subtime, subval, type="p", pch=16, cex=(input$pointsize),
                  col=Colors[Statelevels[i]])
          }
          #add periodic values
          if(input$periodic_check==T){
            lines(valsPeri$tPeri, valsPeri$valPeri, type="p", pch=16, cex=3,
                  col='black')
            try(text(x=corIDs()$tPeri, y=corIDs()$valPeri, labels = corIDs()$corID, col = 'white'),silent = T)
          }
          #add maintenence values vertial lines
          if(input$maint_check==T){
            actionMaintlevels <- levels(valsMaint$actionMaint)
            nactionMaint <- length(actionMaintlevels)
            for (i in 1:nactionMaint) {
              con <- valsMaint$actionMaint == actionMaintlevels[i]
              subtime <- valsMaint$tMaint[con]
              abline(v=subtime, lwd = 1,
                     col=Colors[actionMaintlevels[i]])
            }
          }
          # add a title and subtitle
          title(paste(vals$StNamCont,vals$StNoCont))

          # add a legend
          cols <- Colors[Statelevels[1]]
          for (i in 2:nState){
            cols <- c(cols,Colors[Statelevels[i]])
          }
          if((input$legend)){
            maincheck <- F
            if(input$maint_check){
              if(nactionMaint!=0){maincheck <- T}
            }
            if(!maincheck){
              legend((input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                     pch=16, title='State of value')
            }else{
              linetypes <- c(rep(0,length(Statelevels)),1,1)
              pointtypes <- c(rep(16,length(Statelevels)),NA,NA)
              legend((input$legendlocal),legend = c(Statelevels,'sensor cleaning','sensor replacment'), cex=0.8, col=c(cols,'blue','red'),
                     pch=pointtypes, lty =linetypes,  title='State of value')
            }
          }

        }
      })

      output$plot3 <- renderPlot({
        if(is.null(vals$tCont)){
          plot.new()
          title('no data')
        }else{
          #setting State of values
          State <- State(vals$state)

          #setting up color scale
          col.names <- col.scheme.names()
          Colors <- myColors()
          names(Colors) <- col.names
          #colScale <- scale_colour_manual(name = "State of values",values = Colors)

          #graphing plot
          # factor levels
          Statelevels <- levels(as.factor(levels(State)))
          nState <- length(Statelevels)

          # get the range for the x and y axis
          xrange <- ranges$x
          yrange <- ranges$y3

          # set up the plot
          #setup x-axis
          if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
            plot(xrange, yrange, type="n", xlab="Time",
                 ylab="PPFD lower sensor", xaxt = "n" )
            step <- round(diff(xrange)/4, digits = 0)
            marks <- seq(xrange[1]+step,xrange[2]-step,step)
            lab <- datetimeform(marks)
            axis(1, at = marks, labels = lab)
          }else{
            plot(xrange, yrange, type="n", xlab="Time",
                 ylab="PPFD lower sensor" )
          }

          # add lines
          #add thin gray line over whole working dataset
          lines(vals$tCont, vals$valContL, type="l", lwd=0.5, col='gray')
          #add working dataset with state of values
          for (i in 1:nState) {
            con <- State == Statelevels[i]
            subtime <- vals$tCont[con]
            subval <- vals$valContL[con]
            lines(subtime, subval, type="p", pch=16, cex=(input$pointsize),
                  col=Colors[Statelevels[i]])
          }

          # add a title and subtitle
          title(paste(vals$StNamCont,vals$StNoCont))

          # add a legend
          cols <- Colors[Statelevels[1]]
          for (i in 2:nState){
            cols <- c(cols,Colors[Statelevels[i]])
          }
          if((input$legend)){
            legend((input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                   pch=16, title='State of value')
          }

        }
      })

      output$plot4 <- renderPlot({
        if(is.null(vals$tCont)){
          plot.new()
          title('no data')
        }else{
          #setting State of values
          State <- State(vals$state)

          #setting up color scale
          col.names <- col.scheme.names()
          Colors <- myColors()
          names(Colors) <- col.names
          #colScale <- scale_colour_manual(name = "State of values",values = Colors)

          #graphing plot
          # factor levels
          Statelevels <- levels(as.factor(levels(State)))
          nState <- length(Statelevels)

          # get the range for the x and y axis
          xrange <- ranges$x
          yrange <- ranges$y4

          # set up the plot
          #setup x-axis
          if(input$AxisIsUNIXsec){ #shall we convert UNIX seconds to datetime on the axis labels
            plot(xrange, yrange, type="n", xlab="Time",
                 ylab="PPFD upper sensor", xaxt = "n" )
            step <- round(diff(xrange)/4, digits = 0)
            marks <- seq(xrange[1]+step,xrange[2]-step,step)
            lab <- datetimeform(marks)
            axis(1, at = marks, labels = lab)
          }else{
            plot(xrange, yrange, type="n", xlab="Time",
                 ylab="PPFD upper sensor" )
          }

          # add lines
          #add thin gray line over whole working dataset
          lines(vals$tCont, vals$valContU, type="l", lwd=0.5, col='gray')
          #add working dataset with state of values
          for (i in 1:nState) {
            con <- State == Statelevels[i]
            subtime <- vals$tCont[con]
            subval <- vals$valContU[con]
            lines(subtime, subval, type="p", pch=16, cex=(input$pointsize),
                  col=Colors[Statelevels[i]])
          }

          # add a title and subtitle
          title(paste(vals$StNamCont,vals$StNoCont))

          # add a legend
          cols <- Colors[Statelevels[1]]
          for (i in 2:nState){
            cols <- c(cols,Colors[Statelevels[i]])
          }
          if((input$legend)){
            legend((input$legendlocal),legend = Statelevels, cex=0.8, col=cols,
                   pch=16, title='State of value')
          }

        }
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
          if(!input$lockxaxis){ranges$x <- c(brush$xmin, brush$xmax)}
          if(!input$lockyaxis){ranges$y <- c(brush$ymin, brush$ymax)}

        } else {
          if(!input$lockxaxis){ranges$x <- range(vals$tCont, na.rm = T, finite = T)}
          if(!input$lockyaxis){ranges$y <- range( vals$valCont, na.rm = T, finite = T)}
        }
      })

      observeEvent(input$plot3_dblclick, {
        brush <- input$plot3_brush
        if (!is.null(brush)) {
          if(!input$lockxaxis){ranges$x <- c(brush$xmin, brush$xmax)}
          if(!input$lockyaxis){ranges$y3 <- c(brush$ymin, brush$ymax)}

        } else {
          if(!input$lockxaxis){ranges$x <- range(vals$tCont, na.rm = T, finite = T)}
          if(!input$lockyaxis){ranges$y3 <- range( vals$valContL, na.rm = T, finite = T)}
        }
      })

      observeEvent(input$plot4_dblclick, {
        brush <- input$plot4_brush
        if (!is.null(brush)) {
          if(!input$lockxaxis){ranges$x <- c(brush$xmin, brush$xmax)}
          if(!input$lockyaxis){ranges$y4 <- c(brush$ymin, brush$ymax)}

        } else {
          if(!input$lockxaxis){ranges$x <- range(vals$tCont, na.rm = T, finite = T)}
          if(!input$lockyaxis){ranges$y4 <- range( vals$valContU, na.rm = T, finite = T)}
        }
      })

      observeEvent(input$zoomout_toggle,{
        if(!input$lockxaxis){ranges$x <- c(ranges$x[1]-abs(ranges$x[2]-ranges$x[1]), ranges$x[2]+abs(ranges$x[2]-ranges$x[1]))}
        if(!input$lockyaxis){ranges$y <- c(ranges$y[1]-abs(ranges$y[2]-ranges$y[1]), ranges$y[2]+abs(ranges$y[2]-ranges$y[1]))}
        if(!input$lockyaxis){ranges$y3 <- c(ranges$y3[1]-abs(ranges$y3[2]-ranges$y3[1]), ranges$y3[2]+abs(ranges$y3[2]-ranges$y3[1]))}
        if(!input$lockyaxis){ranges$y4 <- c(ranges$y4[1]-abs(ranges$y4[2]-ranges$y4[1]), ranges$y4[2]+abs(ranges$y4[2]-ranges$y4[1]))}
      })



      #make plot correlation graph

      #variable for storing zoom extent coordinates
      ranges2 <- reactiveValues(x = NULL, y = NULL)
      observe(ranges2$y <- try(range(cortab()$valPeri, na.rm = T, finite = T), silent = T))
      observe(ranges2$x <- try(range(cortab()$valCont, na.rm = T, finite = T), silent = T))

      #function for calibration of continuous data
      lm_eqn <- function(x,y){
        x[is.infinite(x)]<-NA #remove infinite values from the calibration equation
        y[is.infinite(y)]<-NA
        m <- lm(y ~ x);
        eq <- paste0("y = ",format(unname(coef(m)[1]), digits = 5)," + ",format(unname(coef(m)[2]), digits = 5),"*x    r.squared = ",format(summary(m)$r.squared, digits = 3))
        output <- list(eqn = as.character(as.expression(eq)), a = format(unname(coef(m)[1]), digits = 5),b = format(unname(coef(m)[2]), digits = 5))
        return(output);
      }
      lm_eqn.NoInt <- function(x,y){
        x[is.infinite(x)]<-NA #remove infinite values from the calibration equation
        y[is.infinite(y)]<-NA
        m <- lm(y ~ x-1);
        eq <- paste0("y = ",format(unname(coef(m)[1]), digits = 5),"*x    r.squared = ",format(summary(m)$r.squared, digits = 3))
        output <- list(eqn = as.character(as.expression(eq)),a=0, b = format(unname(coef(m)[1]), digits = 5))
        return(output);
      }


      output$plot2 <- renderPlot({

        tryCatch({
          #setting State of values
          State <- State(cortab()$state)

          #setting up color scale
          col.names <- col.scheme.names()
          Colors <- myColors()
          names(Colors) <- col.names

          #graphing plot
          # factor levels
          Statelevels <- levels(as.factor(levels(State)))
          nState <- length(Statelevels)

          # get the range for the x and y axis
          xrange <- ranges2$x
          yrange <- ranges2$y

          # set up the plot
          #setup x-axis
          plot(xrange, yrange, type="n", xlab="Continuous data",
               ylab="Periodic data lab tested" )

          # add lines
          #add thin gray line for the correlation equation
          if(input$cal.nonsus_check==F){ #calibrate with y intercept
            eqn <- lm_eqn(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)
          }else{eqn <- lm_eqn.NoInt(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)}
          abline(a = eqn$a, b = eqn$b, lwd=0.5, col='gray')

          #add corelation dataset with state of values
          for (i in 1:nState) {
            con <- State == Statelevels[i]
            suby <- cortab()$valPeri[con]
            subx <- cortab()$valCont[con]
            lines(subx, suby, type="p", pch=16, cex=3,
                  col=Colors[Statelevels[i]])
          }

          #add corelation point IDs
          try(text(x=cortab()$valCont, y=cortab()$valPeri, labels = cortab()$corID, col = 'white'),silent = T)

          # add a title and subtitle
          title(main = paste(valsPeri$StNamPeri[1],"vs",vals$StNoCont),
                sub = paste('Regression line for',ifelse(input$calgroup==0,'non-marked data points',paste('marked group',input$calgroup)),ifelse(input$cal.nonsus_check,'with no y-intercept','')))

          # add a legend
          cols <- Colors[Statelevels[1]]
          for (i in 2:nState){
            cols <- c(cols,Colors[Statelevels[i]])
          }
          if((input$legend2)){
            legend((input$legendlocal2),legend = Statelevels, cex=0.8, col=cols,
                   pch=16, title='State of value')
          }
        },
        error = function(e){
          plot.new()
          print(e)
          title('no reference data to make calibration graph. Check that the selected marked group is present in your data.')
        }
        )
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot2_dblclick, {
        brush <- input$plot2_brush
        if (!is.null(brush)) {
          ranges2$x <- c(brush$xmin, brush$xmax)
          ranges2$y <- c(brush$ymin, brush$ymax)

        } else {
          ranges2$y <- try(range(cortab()$valPeri, na.rm = T, finite = T),silent = T)
          ranges2$x <- try(range(cortab()$valCont, na.rm = T, finite = T),silent = T)

        }
      })

      output$formula <- renderText({
        tryCatch({lm_eqn(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)$eqn},
                 error=function(e){return("No reference data to calculate calibration formulas")})
      })
      output$formulaNoInt <- renderText({
        tryCatch({lm_eqn.NoInt(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)$eqn},
                 error=function(e){return("No reference data to calculate calibration formulas")})
      })

      #save time of calibration for info text ouput on the continuous graph page. If you don't save the time then it will just print the time that you open the other page
      time <- reactiveValues(time = NULL)

      #calibrate data in the selected "marked grouping" or all non marked values. Data labeled "suspect" or "suspect calc" was not used in calculating the calibration but i will still be calibrated
      observeEvent(input$cal.nonsus_toggle, {
        withProgress(message = 'Process: Performing mathematical transformation on dataset',{
          grp <- input$calgroup
          vec <- vals$state
          if(grp==0){ #if 0 then take all non marked values
            cond <- vec<input$min21|vec>(input$min21+8) #not marked
          }else{ #else take all marked values in chosen group
            cond <- vec== input$min21+as.numeric(grp)-1 & !is.na(vec)
          }
          if(sum(cond)==0){
            showNotification('There is no data in the selected marked-grouping. Please select a different grouping', type = 'error')
            return(NULL)
          }

          a<-NULL;b<-NULL
          try(silent = T,
              if(input$cal.nonsus_check==F){ #calibrate with y intercept
                a<-as.numeric(lm_eqn(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)$a)
                b<-as.numeric(lm_eqn(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)$b)
              }else{ #calibrate with no y intercept
                a<-as.numeric(lm_eqn.NoInt(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)$a)
                b<-as.numeric(lm_eqn.NoInt(y=goodtab()$goodvalPeri, x=goodtab()$goodvalCont)$b)
              }
          )

          f<-function(x){
            output<-a+b*x
            return(output)
          }
          val <- vals$valCont[cond]
          time$time <- Sys.time() #saving system time of operation for work log

          if(is.null(a)|is.null(b)){
            showNotification("Could not calculate a linear regression. Please use the manuall calibration. Or check that you have the correct marked-group selected.", type = 'error')
          }else{
            val <- sapply(val,f)
            isolate(work$log <- rbind(work$log,paste("Calibrate all data in 'Marked Grouping'",grp," with",a,"+",b,"* x","at",time$time,". (Group 0 means all not inside a marked grouping. Suspect values were not used for calculating the calibration but they were calibrated.)"))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reactive function reevaluate for ever
          }

          vals$valCont[cond] <- val

        })
      })
      observeEvent(input$cal.manual_toggle, {
        withProgress(message = 'Process: Performing mathematical transformation on dataset',{
          grp <- input$calgroup
          vec <- vals$state
          if(grp==0){ #if 0 then take all non marked values
            cond <- vec<input$min21|vec>(input$min21+8) #not marked
          }else{ #else take all marked values in chosen group
            cond <- vec== input$min21+as.numeric(grp)-1 & !is.na(vec)
          }
          if(sum(cond)==0){
            showNotification('There is no data in the selected marked-grouping. Please select a different grouping', type = 'error')
            return(NULL)
          }

          fman<-function(x){
            output<-eval(parse(text=as.character(input$cal.nonsus_input)))
          }
          val <- vals$valCont[cond]
          time$time <- Sys.time() #saving system time of operation for work log

          tryCatch({
            val <- sapply(val,fman)
            isolate(work$log <- rbind(work$log,paste("Calibrate all data in 'Marked Grouping'",grp," with",as.character(input$cal.nonsus_input),"at",time$time,". (Group 0 means all not inside a marked grouping.)"))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reactive function reevaluate for ever
          },
          error=function(e){
            showNotification(paste("Error in manual calibration entry: ", e$message), type = "error", duration = NULL)
          }
          )

          vals$valCont[cond] <- val

        })
      })


      #Cont Data manipulation button controls---------------------------------------------------------------

      #reset original data
      observeEvent(input$reset_toggle, {
        vals$state <- df()[[input$statecol]]
        vals$valCont <- df()[[input$valcol]]
        if(input$PPFDdata){
          vals$valContL <- df()[["dspk.Values.y"]]
          vals$valContU <- df()[["dspk.Values.x"]]
        }
        output$info <- renderText(paste("reset to original data at",Sys.time()))
        isolate(work$log <- rbind(work$log,paste("reset to original data at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })

      #save progress
      observeEvent(input$save_toggle,{
        df_saved$df[[input$statecol]]<-vals$state
        df_saved$df[[input$valcol]]<-vals$valCont
        if(input$PPFDdata){
          df_saved$df[["dspk.Values.y"]]<-vals$valContL
          df_saved$df[["dspk.Values.x"]]<-vals$valContU
        }
        output$info <- renderText(paste("progress saved at",Sys.time()))
        isolate(work$log <- rbind(work$log,paste("progress saved at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })

      #undo till last save
      observeEvent(input$undo_toggle, {
        vals$state <- df_saved$df[[input$statecol]]
        vals$valCont <- df_saved$df[[input$valcol]]
        if(input$PPFDdata){
          vals$valContL <- df_saved$df[["dspk.Values.y"]]
          vals$valContU <- df_saved$df[["dspk.Values.x"]]
        }
        output$info <- renderText(paste("undo till last save at",Sys.time()))
        isolate(work$log <- rbind(work$log,paste("undo till last save at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })

      #tag point as marked group
      observeEvent(input$marked_toggle, {
        x <- vals$tCont
        y <- vals$valCont
        dafrm <- data.frame(x,y)
        res <- brushedPoints(dafrm, input$plot_brush,  xvar = "x", yvar = "y", allRows = TRUE)
        res <- res$selected_
        if(input$PPFDdata){
          y3 <- vals$valContL
          y4 <- vals$valContU
          dafrm3 <- data.frame(x,y3)
          dafrm4 <- data.frame(x,y4)
          res3 <- brushedPoints(dafrm3, input$plot3_brush,  xvar = "x", yvar = "y3", allRows = TRUE)
          res4 <- brushedPoints(dafrm4, input$plot4_brush,  xvar = "x", yvar = "y4", allRows = TRUE)
          res <- as.logical(res+res3$selected_+res4$selected_)
        }

        vals$state[res] <- as.numeric(input$min21)-1+as.numeric(input$marked_tag)
        output$info <- renderText(paste("points tagged as marked in grouping",as.numeric(input$marked_tag),"at",Sys.time()))
        isolate(work$log <- rbind(work$log,paste("points tagged as marked in grouping",as.numeric(input$marked_tag),"and state of value changed to",as.numeric(input$min21)-1+as.numeric(input$marked_tag),": data range xmin =",datetimeform(as.numeric(input$plot_brush$xmin)),"xmax =",datetimeform(as.numeric(input$plot_brush$xmax)),"ymin =",(as.numeric(input$plot_brush$ymin)),"ymax =",(as.numeric(input$plot_brush$ymax)),"at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })


      #tag brushed points as good
      observeEvent(input$good_toggle, {
        x <- vals$tCont
        y <- vals$valCont
        dafrm <- data.frame(x,y)
        res <- brushedPoints(dafrm, input$plot_brush,  xvar = "x", yvar = "y", allRows = TRUE)
        res <- res$selected_
        if(input$PPFDdata){
          y3 <- vals$valContL
          y4 <- vals$valContU
          dafrm3 <- data.frame(x,y3)
          dafrm4 <- data.frame(x,y4)
          res3 <- brushedPoints(dafrm3, input$plot3_brush,  xvar = "x", yvar = "y3", allRows = TRUE)
          res4 <- brushedPoints(dafrm4, input$plot4_brush,  xvar = "x", yvar = "y4", allRows = TRUE)
          res <- as.logical(res+res3$selected_+res4$selected_)
        }

        vals$state[res] <- as.numeric(input$for9)
        infotag  <-  paste(": data range xmin =",datetimeform(as.numeric(input$plot_brush$xmin)),"xmax =",datetimeform(as.numeric(input$plot_brush$xmax)),"ymin =",(as.numeric(input$plot_brush$ymin)),"ymax =",(as.numeric(input$plot_brush$ymax)),"at",Sys.time())
        output$info <- renderText(paste("points tagged as good",infotag))
        isolate(work$log <- rbind(work$log,paste("points tagged as good and state of value changed to",input$for9,": data range xmin =",datetimeform(as.numeric(input$plot_brush$xmin)),"xmax =",datetimeform(as.numeric(input$plot_brush$xmax)),"ymin =",(as.numeric(input$plot_brush$ymin)),"ymax =",(as.numeric(input$plot_brush$ymax)),"at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })


      #Reclassify marked group as an Other State of Value
      output$sto.state.class <- renderUI({ #this code renders the drop down list that takes it's listings from the graphing preference page of state of values
        dropdown <- c(input$for9,input$for10,input$for11,input$for12,input$for13,input$for14,input$for34,input$for35,input$for36,input$for37,input$for38,input$for39,input$for40,input$for41,input$for42,input$for43,input$for44,input$for45,input$for46,input$for47,input$for48,input$for49)
        names(dropdown) <- c(input$state9,input$state10,input$state11,input$state12,input$state13,input$state14,input$state34,input$state35,input$state36,input$state37,input$state38,input$state39,input$state40,input$state41,input$state42,input$state43,input$state44,input$state45,input$state46,input$state47,input$state48,input$state49)
        if(input$PPFDdata){
          dropdown <- c(input$for9,input$for11,input$for13,input$for34,input$for35,input$for36,input$for37,input$for38,input$for39,input$for40,input$for41,input$for42,input$for43,input$for44,input$for45,input$for46,input$for47,input$for48,input$for49)
          names(dropdown) <- c(input$state9,input$state11,input$state13,input$state34,input$state35,input$state36,input$state37,input$state38,input$state39,input$state40,input$state41,input$state42,input$state43,input$state44,input$state45,input$state46,input$state47,input$state48,input$state49)

        }
        selectInput("sto.state.class.val", "Non-work-class State of Value", dropdown)
      })
      output$sto.code <- renderText(input$sto.state.class.val) #this gives the numeric code of the selected state of value
      observeEvent(input$sto.reclass_toggle,{ #this is the code to reclassify the spesified marked grouping
        susgrp <- as.numeric(input$min21)-1+as.numeric(input$sto.sus_tag)#to get the marked goup number
        con <- vals$state == susgrp&!is.na(vals$state)
        vals$state[con] <- as.numeric(input$sto.state.class.val)
        infotag  <-  paste("at",Sys.time())
        output$info <- renderText(paste("points from marked Group",input$sto.sus_tag,"code",susgrp,"reclassified as",input$sto.state.class.val,infotag))
        isolate(work$log <- rbind(work$log,paste("points from marked Group",input$sto.sus_tag,"code",susgrp,"reclassified as",input$sto.state.class.val,"at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })



      #reclassify all work classes to Good state of value
      observeEvent(input$wtg.reclass_toggle,{
        con <- !is.na(vals$valCont)&(!(vals$state %in% c(input$min21,as.numeric(input$min21)+1,as.numeric(input$min21)+2,as.numeric(input$min21)+3,as.numeric(input$min21)+4,as.numeric(input$min21)+5,as.numeric(input$min21)+6,as.numeric(input$min21)+7,as.numeric(input$min21)+8,input$min4,input$for9,input$for10,input$for11,input$for12,input$for13,input$for14,input$for30,input$for31,input$for32,input$for33,input$for34,input$for35,input$for36,input$for37,input$for38,input$for39,input$for40,input$for41,input$for42,input$for43,input$for44,input$for45,input$for46,input$for47,input$for48,input$for49))|is.na(vals$state))
        vals$state[con] <- input$for9
        infotag  <-  paste("at",Sys.time())
        output$info <- renderText(paste("reclassify all work classes to Good state of value code",input$for9,infotag))
        isolate(work$log <- rbind(work$log,paste("reclassify all work classes to Good state of value code",input$for9,infotag))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })


      #reclassify custom state of value number code to custom state of value number code
      observeEvent(input$ctc.reclass_toggle,{
        if(!(is.null(input$ct)|is.na(input$ct)|is.null(input$tc)|is.na(input$tc))){
          ct <- as.numeric(input$ct)
          con <- vals$state == as.integer(ct)&!is.na(vals$state)
          vals$state[con] <- as.numeric(input$tc)
          infotag  <-  paste("at",Sys.time())
          output$info <- renderText(paste("reclassify reclassify custom state of value class",input$ct,"to",input$tc,infotag))
          isolate(work$log <- rbind(work$log,paste("reclassify reclassify custom state of value class",input$ct,"to",input$tc,infotag))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        }
      })

      #Brush reclassify custom state of value number code to custom state of value number code
      observeEvent(input$ctc.Brush.reclass_toggl,{
        if(!(is.null(input$brushfrom)|is.na(input$brushfrom)|is.null(input$brushto)|is.na(input$brushto))){
          x <- vals$tCont
          y <- vals$valCont
          dafrm <- data.frame(x,y)
          res <- brushedPoints(dafrm, input$plot_brush,  xvar = "x", yvar = "y", allRows = TRUE)
          res <- res$selected_
          if(input$PPFDdata){
            y3 <- vals$valContL
            y4 <- vals$valContU
            dafrm3 <- data.frame(x,y3)
            dafrm4 <- data.frame(x,y4)
            res3 <- brushedPoints(dafrm3, input$plot3_brush,  xvar = "x", yvar = "y3", allRows = TRUE)
            res4 <- brushedPoints(dafrm4, input$plot4_brush,  xvar = "x", yvar = "y4", allRows = TRUE)
            res <- as.logical(res+res3$selected_+res4$selected_)
          }

          input$brushfrom
          ct <- as.numeric(input$brushfrom)
          con <- (vals$state == as.integer(ct)&!is.na(vals$state))&res
          vals$state[con] <- as.numeric(input$brushto)
          infotag  <-  paste("at",Sys.time())
          output$info <- renderText(paste("reclassify reclassify custom state of value class",input$brushfrom,"to",input$brushto,infotag))
          isolate(work$log <- rbind(work$log,paste("reclassify reclassify custom state of value class",input$brushfrom,"to",input$brushto,infotag))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
        }
      })


      #Reclassify marked Group as an Other marked Group
      observeEvent(input$sts.reclass_toggle,{ #this is the code to reclassify the specified marked grouping
        susgrp1 <- as.numeric(input$min21)-1+as.numeric(input$sts1.sus_tag)#to get the marked group number
        susgrp2 <- as.numeric(input$min21)-1+as.numeric(input$sts2.sus_tag)#to get the marked group number
        con <- vals$state == susgrp1&!is.na(vals$state)
        vals$state[con] <- as.numeric(susgrp2)
        infotag  <-  paste("at",Sys.time())
        output$info <- renderText(paste("points from marked Group",input$sts1.sus_tag,"reclassified as marked Group",input$sts2.sus_tag,infotag))
        isolate(work$log <- rbind(work$log,paste("points from marked Group",input$sts1.sus_tag,"code",susgrp1,"reclassified as marked Group",input$sts2.sus_tag,"code",susgrp2,infotag))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })

      #Delete marked Group
      observeEvent(input$s.delete_toggle,{ #this is the code to reclassify the spesified marked grouping
        susgrp1 <- as.numeric(input$min21)-1+as.numeric(input$s.delete.sus_tag)#to get the marked goup number
        con <- vals$state == susgrp1&!is.na(vals$state)
        vals$valCont[con] <- NA
        if(input$PPFDdata){
          vals$valContL[con] <- NA
          vals$valContU[con] <- NA
        }
        vals$state[con] <- as.numeric(input$for5)
        infotag  <-  paste("at",Sys.time())
        output$info <- renderText(paste("points from marked Group",input$sts1.sus_tag,"manualy deleted",infotag))
        isolate(work$log <- rbind(work$log,paste("points from marked Group",input$sts1.sus_tag,"code",susgrp1, "manualy deleted code",input$for5,infotag))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })

      #State of value table on the Reclassify page
      statetab <- reactive({
        State.of.Value <- as.integer(levels(as.factor(vals$state)))
        Legend.Label <- sapply(State.of.Value,f1)
        df <- data.frame(State.of.Value,Legend.Label)
        return(df)
      })
      output$StateOfValueTable <- renderTable(statetab())





      #interpolate data-------
      observeEvent(input$interpolate_toggle, {
        withProgress(message = 'Process running: interpolate data',{
          statecode <- as.numeric(input$max4) #tagged as manual interpolate
          if(!input$PPFDdata){
            inter <- dspk.DataGapInterpolation(Value=vals$valCont, precision = NULL, NumDateTime=vals$tCont, max.gap = input$maxgap_interpolate*60, State.of.value.data = vals$state, state.of.value.code = statecode)
            vals$state <- inter$dspk.StateOfValue
            vals$valCont <- inter$dspk.Values
          }
          if(input$PPFDdata){
            inter <- dspk.DataGapInterpolation(Value=vals$valContL, precision = NULL, NumDateTime=vals$tCont, max.gap = input$maxgap_interpolate*60, State.of.value.data = vals$state, state.of.value.code = statecode)
            vals$valContL <- inter$dspk.Values
            vals$state <- inter$dspk.StateOfValue
            inter <- dspk.DataGapInterpolation(Value=vals$valContU, precision = NULL, NumDateTime=vals$tCont, max.gap = input$maxgap_interpolate*60, State.of.value.data = vals$state, state.of.value.code = statecode)
            vals$valContU <- inter$dspk.Values

            Dist.Sensors <- input$DistSensors ;kddlupper = input$kddlupper ;kddllower = input$kddllower
            vals$valCont = round( 1/Dist.Sensors*log(vals$valContU / vals$valContL), digits = 4) #calculating light attenuation coefficient
            ##do not report all kd values where upper sensor is less than 1 and lower sensor is less than 0.25
            con <- (vals$valContU < kddlupper | vals$valContL < kddllower) & !is.na(vals$valContU) & !is.na(vals$valContL)& !is.na(vals$valCont)
            vals$valCont[con] <- NA
          }
        })

        output$info <- renderText(paste("interpolation of data gaps of",input$maxgap_interpolate," minutes or less at",Sys.time()))
        isolate(work$log <- rbind(work$log,paste("interpolation of data gaps of", input$maxgap_interpolate ,"minutes or less at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })

      #interpolate data in brushed area
      observeEvent(input$interpolateBrush_toggle, {
        withProgress(message = 'Process running: interpolate data in brushed area',{
          statecode <- as.numeric(input$max4) #tagged as manual interpolate

          #dftemp <- data.frame(vals$valCont,vals$tCont)
          #res <- brushedPoints(df(), input$plot_brush,  xvar = input$tContcol, yvar = input$valcol, allRows = TRUE) #collecting the brush info from graph

          con <- vals$tCont>as.numeric(input$plot_brush$xmin)&vals$tCont<as.numeric(input$plot_brush$xmax)&!is.na(vals$tCont)
          if(input$PPFDdata){

            con3 <- vals$tCont>as.numeric(input$plot3_brush$xmin)&vals$tCont<as.numeric(input$plot3_brush$xmax)&!is.na(vals$tCont)
            con4 <- vals$tCont>as.numeric(input$plot4_brush$xmin)&vals$tCont<as.numeric(input$plot4_brush$xmax)&!is.na(vals$tCont)
            if(length(con)==0&length(con3)==0&length(con4)==0)return(NULL) #if there are no brushed boxes do nothing
            if(length(con)==0) con<-F
            if(length(con3)==0) con3<-F
            if(length(con4)==0) con4<-F

            con <- con|con3|con4
          }

          valcont <- vals$valCont[con]#[res$selected_] #subsetting to just have the brushed data
          tcont <- vals$tCont[con]#[res$selected_]
          stcont <- vals$state[con]#[res$selected_]
          if(!input$PPFDdata){
            inter <- dspk.DataGapInterpolation(Value=valcont, precision = NULL, NumDateTime=tcont, max.gap = input$maxgap_interpolate*60, State.of.value.data = stcont, state.of.value.code = statecode)
            vals$state[con] <- inter$dspk.StateOfValue
            vals$valCont[con] <- inter$dspk.Values
          }
          if(input$PPFDdata){
            valcont <- vals$valContL[con]#[res$selected_] #subsetting to just have the brushed data
            inter <- dspk.DataGapInterpolation(Value=valcont, precision = NULL, NumDateTime=tcont, max.gap = input$maxgap_interpolate*60, State.of.value.data = stcont, state.of.value.code = statecode)
            vals$valContL[con] <- inter$dspk.Values
            vals$state[con] <- inter$dspk.StateOfValue
            valcont <- vals$valContU[con]#[res$selected_] #subsetting to just have the brushed data
            inter <- dspk.DataGapInterpolation(Value=valcont, precision = NULL, NumDateTime=tcont, max.gap = input$maxgap_interpolate*60, State.of.value.data = stcont, state.of.value.code = statecode)
            vals$valContU[con] <- inter$dspk.Values

            Dist.Sensors <- input$DistSensors ;kddlupper = input$kddlupper ;kddllower = input$kddllower
            vals$valCont[con] = round( 1/Dist.Sensors*log(vals$valContU[con] / vals$valContL[con]) ,digits = 4) #calculating light attenuation coefficient
            ##do not report all kd values where upper sensor is less than 1 and lower sensor is less than 0.25
            con1 <- con&((vals$valContU < kddlupper | vals$valContL < kddllower) & !is.na(vals$valContU) & !is.na(vals$valContL)& !is.na(vals$valCont))
            vals$valCont[con1] <- NA
          }
        })
        output$info <- renderText(paste("interpolation of data gaps of",input$maxgap_interpolate,"minutes or less within brush at",Sys.time()))
        isolate(work$log <- rbind(work$log,paste("interpolation of data gaps of",input$maxgap_interpolate ,"minutes or less within brush at",Sys.time()))) #adds a row to the log table of what was done. Needs to be in isolate() so that it wont make the reavtive function reevaluate for ever
      })




    }

  )
}




