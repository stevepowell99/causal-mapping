
# Display / settings panel ----


output$condFormattingOutput <- renderUI({
  
  gr <- xc("label frequency sex Positive notForwards older female ava avp")
  gr <- c(colnames_for_sum,colnames_for_mean)
  
  vals <- values$settingsConditional %>%
    mutate(type = if_else(str_detect(attribute, "node"), "node", "edge"))
  
  lapply(seq_along(all_attributes), function(n) {
    thisAttribute <- all_attributes[n]
    thisType <- ifelse(n > length(node_names), "edge", "node")
    if (is.na(thisAttribute)) browser()
    vals2 <- vals %>% filter(attribute == thisAttribute)
    attribute_clean <- str_replace_all(thisAttribute, "\\.", "_") # because of js condition later
    
    div(
      div(
        p(thisAttribute %>% str_replace_all("_|\\."," "), style = "width:160px"),
        style = "display:inline-block;vertical-align:top"
      ),
      
      
      
      if (thisAttribute %>% str_detect("color")) {
        div(
          colourInput(paste0("conditional_value_", thisAttribute),
            label = NULL,
            palette = "limited",
            showColour = "background",
            value = vals2 %>% pull(value),
            allowedCols = allcols1
          ),
          style = "display:inline-block;vertical-align:top;width:50px"
        )
      } else {
        div(
          textInput(paste0("conditional_value_", thisAttribute), NULL, value = vals2 %>% pull(value), width = "120px"),
          style = "display:inline-block;vertical-align:top;width:100px",class="conditional_text"
        )
      },
      
      div(
        selectInput(paste0("conditional_selector_", attribute_clean), label = NULL, choices = c("always", "conditional on ..."), selected = vals2 %>% pull(selector), width = "200px"),
        style = "display:inline-block;vertical-align:top"
      ),
      conditionalPanel(
        paste0("input.conditional_selector_", attribute_clean, '=="conditional on ..."'),
        div(
          div(
            selectInput(paste0("conditional_var_", thisAttribute), label = NULL, choices = gr, selected = vals2 %>% pull(var), width = "200px")
            ,
            style = "display:inline-block;vertical-align:top"
          ),
          
          div(
            selectInput(paste0("agg_type_", thisAttribute), label = NULL, choices = xc("sum mean"), width = "120px"),
            style = "display:inline-block;vertical-align:top"
          ),
          
          
          div(
            p("up to"),
            style = "display:inline-block;vertical-align:top"
          ),
          if (thisAttribute %>% str_detect("color")) {
            div(
              colourInput(paste0("conditional_value2_", thisAttribute),
                label = NULL,
                palette = "limited",
                showColour = "background",
                value = vals2 %>% pull(value2),
                allowedCols = allcols1
              ),
              style = "display:inline-block;vertical-align:top;width:50px"
            )
          } else {
            div(
              textInput(paste0("conditional_value2_", thisAttribute), NULL, value = vals2 %>% pull(value2), width = "120px"),
              style = "display:inline-block;vertical-align:top"
            )
          },
          style = "display:inline-block;"
        ),style = "display:inline-block;background-color:#EEFFEE;margin-left:20px;padding:10px"
      )
      # ,
      # hr(style = "margin:5px")
      ,class="conditional-row")
  })
})


output$upConditionalBut <- renderUI({
  actionButton("upConditional", "Update")
})

observeEvent(input$upConditional, ignoreInit = T, {
  # browser()
  values$settingsConditional <- make_settingsConditional(input, values$settingsConditional)
})



# filters, don't work at the moment ---------------------------------------


output$filterscluster <- renderUI({
  clusters <- values$graf %>%
    nodes_as_tibble() %>%
    pull(cluster) %>%
    unique()
  # browser()
  tagList(
    if (!is.null(clusters) && length(clusters) > 1) {
      tagList(
        div(checkboxGroupButtons("filterscluster", "cluster", choices = sort(values$graf %>% nodes_as_tibble() %>% pull(cluster) %>% unique())), style = "display:inline-block;vertical-align:top")
      )
    }
  )
})
observe(if(req(input$sides)=="Display") {
  cols <- req(values$codeGraf) %>% 
    edges_as_tibble() %>% 
    select(auto_group,everything()) %>% 
    colnames
  output$filters <- renderUI({
    tagList(
      actionButton("filtersUp","Update"),
      hr(),
      lapply(cols, function(y) {
        x <- (values$codeGraf %>% edges_as_tibble())[[y]]
        u <- unique(x) %>% na.omit()
        if (length(u) > 1 & length(u) < 12 & max(nchar(u)) < 20) {
          div(checkboxGroupButtons(paste0("filters", y), y, choices = sort(u), selected = u), style = "display:inline-block;vertical-align:top")
        }
      })
    )
  })
  })
  
observeEvent(input$filtersUp,if(req(input$sides)=="Display"  & req(values$codeGraf) %>% edges_as_tibble() %>% nrow %>% `>`(0)) {
  doNotification("Filtering")
  # browser()
  edges <- req(values$codeGraf) %>% edges_as_tibble()
  filterStore <- edges
  filterStore[T] <- T
for(y in c(colnames(edges))) {
      x <- edges[[y]]
      u <- unique(x) %>% na.omit()
      if (length(u) > 1 & length(u) < 12 & max(nchar(u)) < 20) {
        filter <- input[[paste0("filters", y)]]
        # browser()
        if(!is.null(filter) & !all(edges[[y]] %in% filter)) {
          # edges <- edges[edges[[y]] %in% filter,]
          filterStore[,y]=(edges[[y]] %in% filter)
          # browser()   #this is the wrong way to do it TODO, can't just delete the rows, might need them back, need to offer an array
          }
      }
    }
# if(!is.null(filterStore)){
  # if((which((filterStore %>% colMeans)!=1) %>% length)>0) {
    
    # noTrues=colMeans(filterStore)==0
    # filterStore=filterStore[,-noTrues]  # don't want any filters which are all off
    
filterVec <- rowMeans(filterStore,na.rm=T)==1

  # } else filterVec=T
  # browser()
if(!any(filterVec)){
  filterVec=T
  doNotification("This filter excludes all")
}
  
  valuesCoding$filterVec <- filterVec
})

