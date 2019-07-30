ui <- tagList(
  useShinyjs(),
  inlineCSS(list(.red = "background: snow")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/styles2.css")
  ),
  tags$script('
              $(document).on("keydown", function (e) {
              Shiny.onInputChange("keypressed", e.which);
              });
              '),
  
  
  # get selected text for Code tab ------------------------------------------
  
  
  tags$script('
                function getSelectionText() {
                    var text = "";
                    if (window.getSelection) {
                        text = window.getSelection().toString();
                    } else if (document.selection) {
                        text = document.selection.createRange().text;
                    }
                    return text;
                }

        document.onmouseup = document.onkeyup = document.onselectionchange = function() {
            var selection = getSelectionText();
            Shiny.onInputChange("highlightedText", selection);
        };
        '),
  tags$script(
    'Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("selectBoxValue-selectized").focus();
                                  });'
  ),
  
  
  # fluid page --------------------------------------------------------------
  
  
  
  fluidPage(
    theme = shinytheme("flatly"), style = "background-color:#445544 !important;",
    
    
    div(
      id = "loading-content",
      h2("Getting ready for causal mapping...")
    ),
    conditionalPanel("!input.crowd",
                     
                     div(
                       
                       div(
                         
                         
                         fluidRow(                             #the main panels are resizable
                           (column(4,
                          #   style =
                          #     "padding:0",
                             
                             id = "app-content",
                             a(h4("Causal Mapping", style = "display:inline-block;color:white;margin-right:8px"), href = "."),
                             img(src = "img/logo.gif", height = "20px", style = "display:inline-block;"),
                             a(icon("question-circle"),href="http://www.pogol.net/_causal_mapping/index.html", target="_blank",height = "20px", style = "display:inline-block;margin-left:20px"),
                             
                             hr(style = "margin-top:5px"),
                             
                             # alerts if user requests correct or incorrect permalink
                             bsAlert("bsmsg"),
                             bsAlert("found"),
                             bsAlert("notfound"),
                             
          
                                                    # uiOutput("test"),
                             uiOutput("savebut"),
                             uiOutput("savedMsg"),
                             # uiOutput("savedMsg"),
                             hr(),
                             # div(verbatimTextOutput("keypr"),style="background-color:white"),
                             tabsetPanel(
                               id = "sides", type = "tabs", selected = "Code",
                               
                               # ui statements----
                               
                               tabPanel("",icon=icon("upload"),                        # user can upload set of statements to process, and optionally also pre-existing node and edge lists
                                        style = glue(";border-radius:10px"),
                                        
                                        bsCollapse(
                                          id = "uploads", open = "Upload statements",
                                          bsCollapsePanel(
                                            title = "Upload statements",
                                            # uiOutput("statementsButton"),
                                            fileInput("up.statements", NULL,
                                                      multiple = FALSE, width = NULL,
                                                      accept = c(
                                                        "text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"
                                                      )
                                            )
                                          ),
                                          bsCollapsePanel(
                                            title = "View and edit statements",
                                            # p("Paste your statements into the space below. The table will expand if you paste more rows."),
                                            # p("If you want more columns, right-click to create them first.")
                                            # ,
                                            p("The first column is the text. You can use other columns for attributes like age or gender."),
                                            # p("You can change the column names."),
                                         p() ),
                                          bsCollapsePanel(
                                            "Upload variables and arrows",
                                            p("Nodes table must contain a column called label"),
                                            fileInput("up.nodes", "Upload variables",
                                                      multiple = FALSE, width = NULL,
                                                      accept = c(
                                                        "text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"
                                                      )
                                            ),
                                            p("Arrows table must contain columns from and to and optionally N"),
                                            fileInput("up.edges", "Upload arrows",
                                                      multiple = FALSE, width = NULL,
                                                      accept = c(
                                                        "text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"
                                                      )
                                            ),
                                            checkboxInput("use.labels", "Use labels instead of row numbers",value = T)
                                          )
                                        )
                               ),
                               
                               # ui coding----                             
                               #  most important panel. in a state of flux at the moment. in most cases user will be looking at / scrolling through pieces of text and will "code" them aka use the information to create arrows/edges
                               tabPanel("",icon=icon("highlighter"),
                                        value = "Code", 
                                        
                                        # checkboxInput("showPage","Code one sentence")
                                        # ,
                                        uiOutput("pagerBig"),
                                        uiOutput("displayStatementPanel"),
                                 # uiOutput("edgeBut"),
                                        
                                        uiOutput("varForm"),
                                 
                                   div(
                                     # uiOutput("selectbox"),
                                     # uiOutput("selectbox2"),
                                     # uiOutput("addNewNodeButton"),
                                     uiOutput("selectBoxButtons"),
                                     # uiOutput("fromStackInfo"),
                                     uiOutput("combineLink"),
                                     # uiOutput("edgeInfo"),
                                     uiOutput("add_edges_widget"),
                                     uiOutput("combo"),
                                     style="background-color:#DDFFDD;border:1px gray solid;padding:3px"
                                   )
                               ),
                               tabPanel(value="Variables","",                      # user can directly edit the nodes table. still functional but will probably be dropped
                                        style = glue("background-color:{rgb(0.99,1,0.97)};;border-radius:10px"), icon = icon("boxes"),
                                        # h3("View and edit variables"),
                                        div(actionButton("nodeTableUp", "Update"),style="display:inline-block"),
                                        div(textInput("nodeTableFilter", NULL,width = "160px",placeholder="Filter..."),style="display:inline-block;margin-left:20px"),
                                        div(checkboxInput("nodeTableAddCol", "Add columns"),style="display:inline-block;margin-left:20px"),
                                        div(checkboxInput("nodeTableColE", "Edit columns"),style="display:inline-block;margin-left:20px"),
                                        conditionalPanel("input.nodeTableAddCol",
                                                         uiOutput("nodeTableAddCol")),
                                        rHandsontableOutput("nodeTable"),
                                        uiOutput("combine_button")
                                        # ,
                                        #        uiOutput("formMakr")
                               ),
                               tabPanel("",                             # user can directly edit the edges aka arrows table. still functional but will probably be dropped
                                        value = "Arrows", icon = icon("arrow-right"),
                                        # h3("View and edit arrows"),
                                        actionButton("edgeTableUp", "Update"),
                                        rHandsontableOutput("edgeTable"),
                                        div(
                                          style = "display:inline-block;padding-left:20px;font-size:4px;width:150px",
                                          checkboxInput("addColBut", "Edit columns")
                                        )
                               ),
                               
                               
                               # ui display----
                               
                               tabPanel("",value="Display",    # more of a settings panel 
                                        icon = icon("palette"),
                                   uiOutput("upConditionalBut"),
                                 uiOutput("condFormattingOutput")
                               ),
                               
                               tabPanel("",value="Settings",    # more of a settings panel 
                                        icon = icon("cog"),
                                        
                                        # selectInput("layout","layout",choices=c("Sugiyama"="layout_with_sugiyama", "circle"="layout_in_circle"),selected = "layout_with_sugiyama")
                                        # ,
                                        # uiOutput("filters"),                         # transient filters to filter out some nodes/edges, values are not stored in settings csv files
                                        uiOutput("inputs"),                         # user-friendlier widgets which are produced instead of rows of the settings tables if user has written "slider" etc in the last column of settings tables
                                        bsCollapse(
                                          id = "display", open =
                                            "Advanced",
                                          bsCollapsePanel(
                                            "Advanced",
                                            actionButton("settingsTableGlobalUp","Update"),
                                            rHandsontableOutput("settingsTableGlobal"),                         # global settings
                                            # hr(),
                                            # actionButton("settingsTableUp","Update"),
                                            # rHandsontableOutput("settingsTable"),                         # set things like size and colour of items 
                                            hr()
                                            # htmlOutput("overview"),
                                            # hr()
                                          )
                                        ),
                                        bsCollapsePanel(
                                          "Easy",
                                          actionButton("autoMerge", "Auto-suggest clusters")                         # not importatn
                                        ),
                                 
                                 bsCollapsePanel(                    #this is just a utility from visnetwork which I will drop at some point
                                   "Advanced options",
                                   p("Development only"),
                                   icon("exclamation-triangle"),
                                   p(id = "advancedAnchor")
                                 )
                                 
                                 
                                 
                                        # ,
                               ),
                               
                               tabPanel("", icon=icon("warehouse"),div(style = ""),                         # library of existing projects stored locally as csv files. clicking loads up the project 
                                        style = "",
                                        uiOutput("gallery")
                               ),
                               tabPanel("", icon=icon("download"),div(style = ""),
                                        style = "",
                                        uiOutput("downloads")
                               )
                             )
                             
                           )),
                           
                           # main panel----
                           column(8,
                           id="mainPanel",  
                             # style = "border-left:2px dotted black",
                             # pushbar_deps(),
                             # br(),
                             # pushbar(
                             #   
                             #   # h4("HELLO"),
                             #   id = "myPushbar", # add id to get event
                               # uiOutput("push"),
                               # from="right",
                               # actionButton("close", "Close")
                             # ),
                             uiOutput("filters"),
                             uiOutput("widthControlOutput"),
                             
                             conditionalPanel("!input.crowd",style="background-color:white;border-radius:5px",                         # input.crowd is part of an alternative, "crowdsourced" phone-friendly version of the interface which is not important at moment
                                              
                                              uiOutput("floatingWidgets"),
                               
                                              
                                              
                                              
                             withSpinner(jqui_resizable(visNetworkOutput("net", height = "85vh", width="1250px")),type = 5)                         # the main network viz. 
                                              
                                 
                                              #   )
                                              # )
                             )
                             ,
                             
                             tabsetPanel(
                               tabPanel("Quotes",
                                 
                                 uiOutput("quotesOutput")
                               ),
                               
                               tabPanel("Statements",
                                 
                                 uiOutput("push")
                               ),
                               
                                tabPanel("Description",
                                  uiOutput("description"),
                                  # tags$hr(),
                                  uiOutput("blog"),                          # additional narrative abou the project stored in the settings csv
                                  textOutput("info")
                                ),
                                tabPanel("Legend",
                                plotOutput("colourLegend")
                                  ),
                                tabPanel("Report",
                               
                             formattableOutput("reportTable", width = "100%", height = "0")
                             )
                             )
                           
                           )
                         )
                       )
                     ),
                     div(actionButton("Interrupt", "Interrupt",
                     ), style = "position:fixed;bottom:0;right:10px")
    ),
    conditionalPanel("input.crowd",
                     uiOutput("add_edges_widget2"),
                     visNetworkOutput("net2", height = "75vh", width = "auto")
                     
                     # p("lkj")
    ),
    div(checkboxInput("crowd","Simplified view for crowdsourcing?"),style="color:#EEFFEE;position:fixed;bottom:0")
  )
)
