

# observe save png button -------------------------------------------------------------
#  saves a png and html file which are not yet downloadable
# downloads panel  -----------------------------------------------------------

output$downloads <- renderUI({
  # browser()
  doNotification("Creating library list")
  if (T) {
    # name <- gsub("www/", "", fileFromURL())
    tagList(
      # h4("Download your files"),
      # h5("CSV files"),
      # lapply(csvlist, function(x) {
      #   tagList(a(href = paste0(name, "-", x, ".csv"), x), hr())
      # }),
      h5("Graphic files"),
      div(
        
        if(req(input$sides)=="Display")actionButton("png", "Create new files", icon = icon("picture")),
        style = "display:inline-block;margin-right:50px;width:10px"
      ),
      hr(),
      tagList(
        a(href = paste0("tmp", "", ".html"), "interactive html file"), hr(),
        a(href = paste0("tmp", "", ".png"), "high-quality png file"), hr()
      )
    )
  }
})


observeEvent(input$png, {
  # browser()
  doNotification("Saving file", 2)
  fn <- paste0("tmp", ".html")
  # browser()
  visSave(values$net, fn, selfcontained = T)
  doNotification("Saved file", 2)
  file.copy(fn, paste0("www/", fn), overwrite = T) # because there is a bug with htmlwidgets saving to other directories
  # file.remove(fn)
  webshot::webshot(file = paste0("www/",fn, ".png"), url = paste0("www/",fn, ".html"))
  doNotification("Saved png", 2)
})

