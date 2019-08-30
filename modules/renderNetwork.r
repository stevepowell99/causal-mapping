# RENDER visnetwork----
# finally we use values$grafAgg2 to generate the viz

observe(if (!is.null(values$grafAgg2)) {
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
  
  doNotification("Produced viz")
})
  
observe({
  output$netCoding <- renderVisNetwork({
    doNotification("render coding viz")
    values$net
  })
  })

observe({
  output$net <- renderVisNetwork({
    doNotification("render viz")
    values$net
  })
})
  



