
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


# colorLegend -------------------------------------------------------------
# just a placeholder right now

output$colourLegend <- renderPlot({
  emptyplot(main = "Percentage of women mentioning each factor and each link                 Percentage of younger people mentioning each factor", adj = 0)
  colorlegend(
    posx = c(0, 0.1),
    col = intpalette(c("blue", "red"), 100),
    zlim = c(0, 100), zval = c(0, 25, 50, 75, 100)
  )
  
  colorlegend(
    posx = c(0.5, 0.6),
    col = intpalette(c("black", "white"), 100),
    zlim = c(0, 100), zval = c(0, 25, 50, 75, 100)
  )
})
