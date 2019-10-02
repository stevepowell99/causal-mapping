
# main panel  -----------------------------------------------------------
# description below graph
output$description <- renderUI({
  x <- findset("diagramdescription",values)
  if (x != "") {
    div(p(x), style = "padding:10px;background-color:whitesmoke;margin-top:10px;border-radius:5px")
  } else {
    ""
  }
})

output$widthControlOutput <- renderUI({
  actionButton("widthControl", NULL, icon = icon("arrows-h")) %>% bs_embed_tooltip("Change the width of the panels")
})

observeEvent(input$widthControl, {
  toggleClass("app-content", "col7")
  toggleClass("mainPanel", "col5")
  toggleClass("net", "maindivnet-small")
})


