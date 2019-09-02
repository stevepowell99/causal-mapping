# RENDER visnetwork----
# finally we use values$grafAgg2 to generate the viz

observe( {
  # browser()
  
  req(values$grafAgg2)
  req(input$sides)
  if (!is.null(values$grafAgg2) & input$sides=="Display"){
  vga <- req(values$grafAgg2)
  this_tab <- isolate(input$sides)
  vals <- values$settingsGlobal
  
  fvw <- ifelse(this_tab=="Code",findset("variablecoding.width",vals),findset("variablewidth",vals))
  
  
  doNotification("started viz")
  # browser()
  
  if (is.null(values$pag)) {
    values$pag <- 1
  }
  # browser()
  vn <- render_network(vga,vals)
  # browser()
  vn$x$nodes <- dag_layout(vn$x$nodes)
  
  
  values$net <- vn
  
  doNotification("Produced viz")}
})
  

observe( {
  if (!is.null(values$codingGraf) & input$sides=="Code"){
  vga <- req(values$codingGraf)
  this_tab <- isolate(input$sides)
  vals <- values$settingsGlobal
  
  
  
  doNotification("started coding viz")
  # browser()
  
  if (is.null(values$pag)) {
    values$pag <- 1
  }
  # browser()
  if(vga %>% edges_as_tibble() %>% nrow %>% `>`(0)){
  vn <- render_network(vga,vals)
  # browser()
  
  vn$x$nodes <- dag_layout(vn$x$nodes)
  
  
  values$netCoding <- vn
  
  doNotification("Produced viz")}
  } else doNotification("No edges")
})
  
observe({
  output$netCoding <- renderVisNetwork({
    doNotification("render coding viz")
    values$netCoding
  })
  })

observe({
  output$net <- renderVisNetwork({
    doNotification("render viz")
    values$net
  })
})
  



