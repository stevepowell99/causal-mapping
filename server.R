server <- function(input, output, session) {


  # TODO pool package to manage
  
  
  
  # initialising ------------------------------------------------------------
  
  
  track_usage(storage_mode = store_json(path = "logs/"))
  
  
  
  
  # autoInvalidate <- reactiveTimer(2000)

  # reactive values ---------------------------------------------------------

  values <- reactiveValues() # nearly all reactive values are stored in values$...
  values$statements <- default.statements
  values$sources <- default.sources
  # values$clickArrow <- F # no idea
  # values$crowd <- F


  values$settingsConditional <- defaultSettingsConditional
  values$settingsGlobal <- defaultSettingsGlobal

  
  # browser()
  
  source("modules/user.r",local=T)
  source("modules/coding.r",local=T)
  source("modules/display.r",local=T)
  source("modules/settingsGlobal.r",local=T)
  source("modules/downloads.r",local=T)
  source("modules/reports.r",local=T)
  source("modules/tableWidgets.r",local=T)
  source("modules/mainPanelWidgets.r",local=T)
  source("modules/renderNetwork.r",local=T)
  source("modules/aggregate.r",local=T)
  

    # the useful "interrupt" button in bottom righthand corner
  observeEvent(input$Interrupt, {
    browser()
  })

  

  output$keypr <- renderPrint({
    input$keypressed
  })


  
  

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
}
