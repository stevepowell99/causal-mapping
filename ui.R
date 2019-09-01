ui <- tagList(
  useShinyjs(),
  use_bs_tooltip(),
  inlineCSS(list(.red = "background: snow")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/styles2.css")
  ),

# keypress, not used at moment --------------------------------------------


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

# refocus for selectize input ---------------------------------------------


  
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
                       
                             img(id="watermark",src = "img/watermark-medium.png", height = "10px", width="10px",style = "visibility:hidden;"),
                       div(
                         
                         
                         fluidRow(
                           (column(4,
                             
                             id = "app-content",
                             a(h4("Causal Mapping", style = "display:inline-block;color:white;margin-right:8px"), href = "."),
                             img(id="logo",src = "img/logo.gif", height = "20px", style = "display:inline-block;"),
                             a(icon("question-circle"),href="http://www.pogol.net/_causal_mapping/index.html", target="_blank",height = "20px", style = "display:inline-block;margin-left:20px"),
                             
                             
                             div((dropdownButton(
                               tagList(
                               actionButton("settingsTableGlobalUp","Update")
                                 ,
                               
                               rHandsontableOutput("settingsTableGlobal")
                               )
                               , icon = icon("cog"), tooltip = "settings", circle=F,size = "xs")), style = "display:inline-block;width:8%"),
                             
                             div((dropdownButton(
                               tagList(fileInput("up.statements", NULL,
                                 multiple = FALSE, width = NULL,
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"
                                 )
                               )
                               )
                               , icon = icon("upload"), tooltip = "settings", circle=F,size = "xs")), style = "display:inline-block;width:8%"),
                             
                             div((dropdownButton(
                               tagList(
                                 uiOutput("downloads")
                               )
                               , icon = icon("download"), tooltip = "settings", circle=F,size = "xs")), style = "display:inline-block;width:8%"),
                             
                             
                             hr(style = "margin-top:5px"),
                             
                             # alerts if user requests correct or incorrect permalink
                             bsAlert("bsmsg"),
                             bsAlert("found"),
                             bsAlert("notfound"),
                             bsAlert("recoveryNewer"),
                             bsAlert("recoveryVersion"),
                             
          
                             uiOutput("savebut") %>%
                               bs_embed_tooltip(title = "Save a version"),
                             uiOutput("savedMsg"),
                             hr(),
                             tabsetPanel(
                               id = "sides", type = "tabs", selected = "Code",
                               
                               # ui statements----
                               
                               
                               # ui coding----                             
                               #  most important panel. in most cases user will be looking at / scrolling through pieces of text and will "code" them aka use the information to create arrows/edges
                               tabPanel(span("",title="Code",icon("highlighter")),
                                        value = "Code", 
                                        
                                        uiOutput("pagerBig"),
                                        uiOutput("displayStatementPanel"),
                                        
                                        uiOutput("varForm"),
                                 
                                   div(
                                     uiOutput("selectBoxButtons"),
                                     uiOutput("combineLink"),
                                     uiOutput("add_edges_widget"),
                                     uiOutput("combo"),
                                     style="background-color:#DDFFDD;border:1px gray solid;padding:3px"
                                   )
                               ),
                               
                               
                               # ui display----
                               
                               tabPanel("",value="Display",    # conditional formatting 
                                 icon = icon("palette"),
                                 
                                 bs_accordion("filterx") %>% 
                                   bs_append("Filters",
                                     uiOutput("filters")) %>% 
                                   bs_append("Clusters",
                                     uiOutput("filterscluster")
                                   )
                                 %>% bs_append("Formatting",
                                   tagList(uiOutput("upConditionalBut"),
                                     uiOutput("condFormattingOutput")
                                   ))
                               ),
                               tabPanel(value="Variables","",                      # user can directly edit the nodes table. still functional but will probably be dropped
                                        style = glue("background-color:{rgb(0.99,1,0.97)};;border-radius:10px"), icon = icon("boxes"),
                                        div(actionButton("nodeTableUp", "Update"),style="display:inline-block"),
                                        div(textInput("nodeTableFilter", NULL,width = "160px",placeholder="Filter..."),style="display:inline-block;margin-left:20px"),
                                        div(checkboxInput("nodeTableAddCol", "Add columns"),style="display:inline-block;margin-left:20px"),
                                        div(checkboxInput("nodeTableColE", "Edit columns"),style="display:inline-block;margin-left:20px"),
                                        conditionalPanel("input.nodeTableAddCol",
                                                         uiOutput("nodeTableAddCol")),
                                        rHandsontableOutput("nodeTable"),
                                        uiOutput("combine_button"),
                                 actionButton("autoMerge", "Auto-suggest clusters")                         # not importatn
                                 
                               ),
                               tabPanel("",                             # user can directly edit the edges aka arrows table. still functional but will probably be dropped
                                        value = "Arrows", icon = icon("arrow-right"),
                                        actionButton("edgeTableUp", "Update"),
                                        rHandsontableOutput("edgeTable"),
                                        div(
                                          style = "display:inline-block;padding-left:20px;font-size:4px;width:150px",
                                          checkboxInput("addColBut", "Edit columns")
                                        )
                               )
                             )
                             
                           )),
                           
                           # main panel----
                           column(8,
                           id="mainPanel",  
                             uiOutput("widthControlOutput"),
                             bsTooltip("widthControlOutput", "title", placement = "bottom", trigger = "hover",
                               options = NULL),
                             
                             conditionalPanel("!input.crowd",style="background-color:white;border-radius:5px",                         # input.crowd is part of an alternative, "crowdsourced" phone-friendly version of the interface which is not important at moment
                                              
                                              uiOutput("floatingWidgets"),
                               
                                              
                                              
                                              
                             conditionalPanel("input.sides=='Code'",withSpinner(
                               (visNetworkOutput("netCoding", height = "85vh", width="1250px"))
                               ,type = 5))                         # the main network viz. 
                               ,
                             conditionalPanel("input.sides=='Display'",withSpinner(
                               (visNetworkOutput("net", height = "85vh", width="1250px"))
                               ,type = 5))                         # the main network viz. 
                                              
                                 
                             )
                             ,
                             
                             tabsetPanel(
                               tabPanel("Quotes",
                                 
                                 uiOutput("quotesOutput")
                               ),
                               
                               tabPanel("Statements",
                                 
                                 uiOutput("statementsPanel")
                               ),
                               
                                tabPanel("Description",
                                  uiOutput("description"),# additional narrative abou the project stored in the settings csv
                                  uiOutput("blog"),                          
                                  textOutput("info")
                                ),
                                tabPanel("Legend",
                                plotOutput("colourLegend")
                                  ),
                                tabPanel("Live Reports",
                               
                                  formattableOutput("reportTable", width = "100%", height = "0")
                             ),
                                tabPanel("Reports",
                               
                             formattableOutput("reportTable2", width = "100%", height = "0")
                                  ,
                             formattableOutput("reportTable3", width = "100%", height = "0")
                                  ,
                             formattableOutput("reportTable4", width = "100%", height = "0")
                                  ,
                             formattableOutput("reportTable5", width = "100%", height = "0")
                                  ,
                             plotOutput("reportPlot1", width = "100%")
                             )
                             )
                           
                           )
                         )
                       )
                     ),
                     div(actionButton("Interrupt", "Interrupt"), style = "position:fixed;bottom:0;right:10px")
    ),
    conditionalPanel("input.crowd",
                     uiOutput("add_edges_widget2"),
                     visNetworkOutput("net2", height = "75vh", width = "auto")
                     
                     # p("lkj")
    ),
    div(checkboxInput("crowd","Simplified view for crowdsourcing?"),style="color:#EEFFEE;position:fixed;bottom:0")
  )
)
