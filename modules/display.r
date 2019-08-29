
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

output$filters <- renderUI({
  tagList(
    lapply(c(colnames(values$statements_extra)), function(y) {
      x <- values$statements_extra[[y]]
      u <- unique(x) %>% na.omit()
      if (length(u) > 1 & length(u) < 12 & max(nchar(u)) < 20) {
        div(checkboxGroupButtons(paste0("filters", y), y, choices = sort(u), selected = u), style = "display:inline-block;vertical-align:top")
      }
    })
  )
})

