# options -----------------------------------------------------------------
options(shiny.port = 1111)
options(shiny.autoreload = T)
options(shiny.autoreload.pattern = glob2rx("ui.R|global.R|server.R")) #TODO dev only
doNotificationLevel <- 0 # notification popups. level=1 is debugging and level=2 is user.
options(stringsAsFactors = F)

# source ------------------------------------------------------------------

source("combo_functions.r")
# source("functions/find_cycles.r")

# libs --------------------------------------------------------------------


library(RMariaDB)

# library(whereami) #TODO testing only


library(shinythemes)
library(shinylogs)
library(stringi)
library(future)
plan(multiprocess)
library(shinyWidgets)
library(webshot)
# library(readxl)
library(scales)
library(shinyjqui)
# library(tidyr)
# library(stringr)
library("shinyBS") ## replace with bsplus TODO
library(bsplus)
library(shinyjs)
library(glue)
library(r2d3)
library(tidyverse)
library(rhandsontable)
library(shinyPagerUI)
# library(networkD3)
library(tidygraph)
# library(googledrive)
# library(googlesheets4)
library(shinycssloaders)
# library(rdrop2)
library(igraph) # for find cycles

require(visNetwork)
require(plotly) # rgba
library(colourpicker)
library(ggraph)
library(DiagrammeR)
library(rpivotTable)
library(shape)
library(stringi) # just for highlight_text
library(formattable)


# attempt to bypass lack of local storage at shinyapps, both dropb and gsheets, but only local version works properly --------


# storage <- "dropbox"
# storage <- "gsheets"
# storage <- "local"


# if (storage == "local" | storage == "gsheets") {
#   file__exists <- file.exists
#   read__csv <- read_csv
#   write__csv <- write_csv
# } else if (storage == "dropbox") {
#   file__exists <- drop_exists
#   read__csv <- drop_read_csv
#   write__csv <- function(obj, path) {
#     # browser()
#     write_csv(obj, path = path)
#     drop_upload(path, "www") # note folder is hard-coded TODO
#     file.remove(path)
#   }
# }

# functions ----------------------------------------------------------------



cleanfun=function(tex){
  tex %>% 
    str_remove("\\]") %>% 
    str_remove("\\[") %>% 
    str_remove("\\'") %>% 
    str_remove('\\"') %>% 
    gsub("[^[:alnum:]/-\\?[:space:]]","",.) %>% 
    strip_symbols()
  
}

strip_symbols <- function(vec) vec %>%
    str_remove_all("\\n|\\r") %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("\\'", "")

tolower_trim <- function(vec) vec %>%
    tolower() %>%
    str_trim() %>%
    strip_symbols()
tidy_colnames <- function(df) df %>% rename_all(tolower_trim)

id.finder <- function(label, node.df) {
  sapply(label, function(x) {
    (node.df$label == x) %>%
      which() %>%
      first()
  })
}


# get_user_from_query <- function(url){
#   str_remove(url,"\\/.*$")
# }
# get_title_from_query <- function(url){
#   str_remove(url,"^.*?\\/")
# }
# 
# 
# upload_to_gdrive <- function(gdriveRoot,vc,vals,it){
#   doNotification("Uploading to google drive", 2)
# # browser()
#   loggedUserFolder=reactiveVal(drive_ls(gdriveRoot,type="folder",pattern=paste0("^",get_user_from_query(vc),"$")))
#   saveRDS(vals,file = paste0("www/",vc))
#   drive_upload(media = paste0("www/",vc), path=loggedUserFolder(),name = paste0(it))
#   doNotification("Finished uploading to google drive", 2)
# 
# 
# }

generalise_sprintf <- function(str, tex, ...) {

}


make_labels <- function(tex, wrap=33,df,  sep = "<br>",type="html") {
  wrap <- replace_na(wrap,"")
  wrap[wrap==""] <- 33
  wrap <- as.numeric(wrap)
  
  cols <- tex %>%
    str_extract_all("\\!\\w*") %>%
    `[[`(1) %>%
    str_remove("!")

  tex2 <- tex

  # didn't need the loop but can't remember how to pass a list to sprintf TODO

  for (i in cols) {
    tex2 <- str_replace(tex2, "!\\w*", "%s")

    if (i %in% colnames(df)) tex2 <- sprintf(tex2, unlist(df[, i]))
  }
  tex2 %>% 
    str_replace_all( "///", ifelse(type=="html","<br>","\n")) %>%
    str_wrap(wrap) %>%
    str_replace_all( "NA", "") %>%
    str_trim()
  # TODO the /// wrapping does not work anywhere!
}


# findset function to transfer user settings to values$settings. I simplified it -----------

findset <- function(tex, v ) {
  # if(tex=="variablewrap") browser()
  
  x <- v %>%
    bind_rows(defaultSettingsGlobal) %>%
    group_by(type, setting) %>%
    summarise_all(.funs = funs(first)) %>%
    mutate_all(replaceNA) %>%
    mutate(labs = paste0(type, setting)) %>%
    filter(labs == tex) %>%
    pull(value) %>%
    last()
  
  if (is.na(x)) {
    doNotification(paste0(x, " is not in settings"))
  }
  x
}


# }


put_in_node_list <- function(vec,all){
  c((1:all %in% unlist(vec[,1])),(1:all %in% unlist(vec[,2]))) %>% as.numeric  # this only looks at nodes involved, should really be an association matrix not a vector. 
}                           # alsdoesn't take strength into account TODO


create_statement_groups <- function(ved){
  
  res <- ved  %>% 
    select(from,to) %>%
    split(ved$statement_id) %>% 
    map(put_in_node_list,max(c(ved$from,ved$to))) %>% 
    bind_rows %>% 
    t
  
  
  
  # hmethods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  # methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  # 
  # plots=methods[c(1,2,5,6)] %>% map (~ hclust(dist(res,method=.x),method="mcquitty") %>% plot)
  # plots=methods[c(1,2,5,6)] %>% map (~ hclust(dist(res,method=.x),method="complete") %>% plot)
  # plots=hmethods[] %>% map (~ hclust(dist(res,method="binary"),method=.x) %>% plot)
  # plots=methods[5] %>% map (~ hclust(dist(res,method=.x),method="mcquitty") %>% plot)
  
  # clus <- hclust(dist(t(res))) %>% plot
  clus <- hclust(dist((res),method="binary"),method="ward.D") 
  # plot(clus)
  
  
  redo <- tibble(statement_group=clus %>% cutree(k=3)) %>% rownames_to_column(var="statement_id") %>% mutate(statement_id=as.integer(statement_id))
  
  left_join(ved,redo,by="statement_id")
  
}


export_edgelist_adjacency <- function(gr) {
  gr %>%
    edges_as_tibble() %>%
    select(from, to) %>%
    mutate(value = 1) %>%
    spread(from, value, fill = 0)
}

find_dist <- function(gr, parent) {
  gr %>%
    activate(nodes) %>%
    mutate(pars = (local_members(id, mode = "in", mindist = 1)), hh = map(pars, parent), dis = node_distance_to(unlist(hh))) %>%
    nodes_as_tibble() %>%
    select(dis) %>%
    unlist(recursive = F)
}

### given a graph, tells you the distance of a loop from each node to the xth parent.
### these two almost works but doesn't give right results??

find_dist_all <- function(gr) {
  gr <- gr %>%
    activate(nodes) %>%
    mutate(id = row_number())

  # find max nr of parents
  maxParents <- gr %>%
    activate(nodes) %>%
    mutate(id = row_number(), pars = (local_members(id, mode = "in", mindist = 1))) %>%
    nodes_as_tibble() %>%
    select(pars) %>%
    unlist(recursive = F) %>%
    map(length) %>%
    unlist() %>%
    max()
  df <- map(1:maxParents, ~ find_dist(gr, .)) %>% data.frame()
  colnames(df) <- paste0("x", 1:ncol(df))
  df[df == Inf] <- 0
  rowSums(df)
}


sort_short <- function(x) (x) %>%
    paste0("x") %>%
    as.factor() %>%
    as.numeric()
# like rank but with no gaps
##
prepare_vg <- function(graf) {
  # browser()
  graf %>%
    activate(nodes) %>%
    mutate(id = row_number(), origID = id) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(value = if_else(value == "", 0, value)) %>%
    mutate(value = replace_na(value, 0))
}

prepare_ved <- function(ved) {
  ved %>%
    mutate_at(vars(strength, trust), funs(as.numeric)) %>%
    mutate(combo.type = ifelse(is.na(combo.type), "", combo.type)) %>%
    mutate(label = ifelse(is.na(label), "", label)) %>%
    mutate(definition.type = ifelse(is.na(definition.type), "", definition.type))
}

prepare_vno <- function(vno) {
  vno %>%
    mutate(cluster = ifelse(is.na(cluster), "", cluster)) %>%
    mutate(clusterLabel = ifelse(is.na(clusterLabel), "", clusterLabel))
}

# ved_join_statements <- function(ved, statements) {
#   
#   ved <- ved %>%
#     left_join(statements, by = "statement")
#   ved
# }


merge_nodes <- function(vno, ved) {
  # browser()
  vno <- vno %>%
    mutate(id = row_number()) %>%
    mutate(cluster = ifelse(cluster == "", NA, cluster)) %>%
    group_by(cluster) %>%
    mutate(clusterid = first(id)) %>%
    mutate(clusterid = ifelse(is.na(cluster), id, clusterid)) %>%
    ungroup() %>%
    group_by(clusterid) %>%
    mutate(clusterLength = n()) %>%
    mutate(clusterLabel = clusterLabel %>% replaceNA()) %>%
    mutate(label = ifelse(clusterLength < 2, label, ifelse(clusterLabel != "", clusterLabel, paste0("Cluster: ", label)))) %>%
    # mutate_if_sw(is.numeric, .funs = list(sum = sumfun, mean = meanfun)) %>%
    ungroup()

  ved <- ved %>%
    mutate(from = vno$clusterid[findfirst(from, vno$id)]) %>%
    mutate(to = vno$clusterid[findfirst(to, vno$id)])
  # browser()
  tmp.graf <- tbl_graph(vno, ved) %>%
    N_() %>%
    filter(id == clusterid)

  vno <- tmp.graf %>% nodes_as_tibble()
  ved <- tmp.graf %>% edges_as_tibble()

  list(vno, ved)
}


large <- ""
small <- ""
highlight_text <- function(large, smallvec, start = "<a href='.'>", stop = "</a>") {
  # browser()
  if(length(large)>0 & length(smallvec)>0){
  for (small in smallvec) {
  small <- str_remove_all(small," *\\[.*?\\] *")
    if (length(nchar(small)) > 0) {
      if (str_detect(large, small) && nchar(small) > 2) {
        where <- str_locate(large, small)
        stringi::stri_sub(large, where[1], where[1] - 1) <- start
        stringi::stri_sub(large, where[2] + 13, where[2] + 12) <- stop
        large
      }
    }
  }
  large
  } else ""
}



# format_edges <- function(df,input)df

add_opacity <- function(colorname, opacity) {
  col2rgb(colorname) %>%
    as.vector() %>%
    c(opacity) %>%
    paste0(collapse = ",") %>%
    paste0("rgba(", ., ")")
}

add_opacity_vec <- function(colvec, opacity) sapply(colvec, add_opacity, opacity)



mutate_if_sw <- function(...) suppressMessages(mutate_if(...))

doNotification <- function(text, level = 1, ...) {
  if (level > doNotificationLevel) showNotification(glue(text), ...)
  write(paste0(text), paste0("", "log.txt"), append = T)
}


allNum <- function(vec) {
  vec <- replaceNA(vec)
  s <- str_detect(vec, "^[-*\\.0-9]*$")
  all(s, na.rm = T) & !all(vec == "")
}

xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

replaceNA <- function(df) {
  df[is.na(df)] <- ""
  df
}


inv <- function(ed) { # inverts an edgelist. For Rick's suggestion for cacluating a node metric
  ed %>%
    select(from, to) %>%
    unlist() %>%
    unique() %>%
    expand.grid(from = ., to = ., stringsAsFactors = F) %>%
    bind_rows(ed) %>%
    select(from, to) %>%
    group_by(from, to) %>%
    mutate(n = paste0(from, to)) %>%
    group_by(from, to) %>%
    mutate(n = n()) %>%
    mutate(present = ifelse(n < 2, 0, 1)) %>%
    select(-n)
}

inv_multi <- function(df) { # For Rick's suggestion for cacluating a node metric
  # browser()
  stats <- df %>%
    split(.$statement_id) %>%
    purrr::map_dfr(inv) %>%
    group_by(from, to) %>%
    mutate(absent = sum(1 - present), present = sum(present), num = n(), avp = round(present / num, 2), ava = round(absent / num, 2)) %>%
    summarise_all(dplyr::first)

  df %>% left_join(stats, by = c("from", "to"))
}

infer <- function(gr) { # sets levels of downstream variables
  gr <- gr %>%
    activate(nodes) %>%
    mutate(source = node_is_source()) %>%
    mutate(priorLevel = level) %>%
    mutate(xid = row_number())

  # the vector of endog variables
  ids <- gr %>%
    activate(nodes) %>%
    pull(source) %>%
    which()

  empties <- gr %>%
    activate(nodes) %>%
    mutate(empty = source & is.na(level)) %>%
    pull(empty) %>%
    which()

  # ridiculous palaver cause can't call bfs directly
  # makes a long vector of all the nodes we will need to visit
  ranks <- vector(length = 0)

  for (y in ids) {
    # browser()
    gr %>%
      activate(nodes) %>%
      mutate(rankx = bfs_rank(root = y)) %>%
      filter(!is.na(rankx)) %>%
      arrange(rankx) %>%
      pull(xid) -> rk

    ranks <- c(ranks, rk[-1])
  }

  # browser()
  # gridd=list(length=0)
  # combs for making a list of all the endogenous variables with no values set
  combs <- lapply(empties, function(x) 0:1) %>% expand.grid()

  colnames(combs) <- empties

  gr1 <- gr

  levs <- NULL # vector(length=length(combs))
  if (nrow(combs) > 0) { # if there are any NA exog vars, we need to build a grid
    for (e in 1:nrow(combs)) {
      if (length(empties) > 0) {
        # browser()
        row <- combs[e, ]
        gr1 %>% nodes_as_tibble() -> jj
        jj$level[as.numeric(names(combs))] <- row
        gr1 <- gr1 %>%
          activate(nodes) %>%
          mutate(level = unlist(jj$level))
      }
      for (r in ranks) {
        # browser()
        gr1 <- gr1 %>%
          activate(nodes) %>%
          mutate(level = ifelse(r != xid, level,
            do.call(.N()$fun[r], list(.N()$level[.E()$from[.E()$to == r]]))
          ))
      }
      levs <- cbind(levs, (gr1 %>% nodes_as_tibble() %>% pull(level)))
    } # browser()
  }
  else {
    # browser()
    for (r in ranks) {
      # browser()
      gr1 <- gr1 %>%
        activate(nodes) %>%
        mutate(
          level = ifelse(r != xid, level, 2)
          # mutate(level = ifelse(r!=xid,level,
          #   do.call(.N()$fun[r],list(.N()$level,c(.N()$level[.E()$from[.E()$to==r]])))
          # )
        )

      # gr1=gr1 %>%
      #   activate(nodes) %>%
      #   mutate(level = do())
    }
    # levs=cbind(levs,(gr1 %>% nodes_as_tibble %>% pull(level)))
  }
  gr1
  # means=rowMeans(levs,na.rm=T)
  #   gr = gr %>% activate(nodes) %>%
  #     mutate(level=levs)
  #
  # gr
}


render_network <- function(vga,vals){
  
  visNetwork(
    nodes =
      vga %>%
      nodes_as_tibble() %>% 
      mutate(id = row_number()),
    edges =
      vga %>% edges_as_tibble() %>%
      mutate(id = row_number()),
    
    main =
      findset("diagramtitle",vals),
    submain =
      findset("diagramsubtitle",vals),
    background = findset("diagrambackground", v = vals)
  )  %>%
    visInteraction(
      dragNodes = T,
      dragView = T,
      zoomView = T,
      navigationButtons = F,
      multiselect = T
    ) %>%
    visInteraction(
      tooltipStyle = "position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:500px;word-break: break-all",
      hoverConnectedEdges = F,
      keyboard = F, # would be nice for navigation but interferes with text editing
      selectConnectedEdges = F
    ) %>%
    visOptions(
      manipulation = F,
      collapse = F,
      highlightNearest = list(
        enabled = T,
        degree = if (findset("diagramdownarrows",vals) %>% as.logical()) list(from = 0, to = 19) else list(from = 19, to = 0),
        # degree = ifelse(input$widgetDownArrows,list(from=0,to=19),list(from=19,to=0)),
        hover = T,
        labelOnly = F,
        algorithm = "hierarchical"
      ),
      selectedBy = if (!("cluster" %in% colnames(vga %>% nodes_as_tibble()))) "" else ifelse((vga %>% nodes_as_tibble() %>% pull(cluster) %>% replace_na("") %>% `==`("") %>% all()), "", "cluster"),
      nodesIdSelection = F
    ) %>%
    visEvents(select = "function(data) {
                Shiny.onInputChange('net_selected', data.nodes);
                Shiny.onInputChange('net_selectedEdges', data.edges);
                ;}"
    ) %>%
    visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, type = "full") %>% 
    visNodes(
      shadow = list(enabled = T, size = 10),
      # widthConstraint = if ("" == fvw) NULL else as.numeric(fvw), # ,300-(levels*10),#,(300*levels)-9,
      hidden = F, # findset("variablehidden",global=F) %>% as.logical(),
      scaling = list(label = list(enabled = F)),
      shape = findset("variableshape", v = vals),
      group = T, # findset("variablegroup",global=F),
      
      physics = findset("diagramphysics", v = vals)
    ) %>%
    visEdges(
      smooth = T,
      arrowStrikethrough = T,
      shadow =
        list(enabled = F, size = 5),
      hoverWidth = 8, #' function (width) {return width*50;}',
      selectionWidth = 8, # sqrt(nrow(vn$x$nodes)),
      physics = F,
      # color=list(highlight="#000000"),
      arrows =
        list(middle = list(type = "circle", scaleFactor = .5), from = list(type = "circle", scaleFactor = 0.2)),
      # ,
      # dashes = findset("arrowdashes") %>% as.logical()
    )
}


dag_layout <- function(nods){
  
  
  tmp <- nods$x
  nods$x <- nods$y
  nods$y <- tmp
  # nods$color <- 'rgba(120,132,114,.7)'
  vnxn <- nods
  levels <- (length(unique(vnxn$x)))
  maxLen <- vnxn %>%
    group_by(x) %>%
    summarise(len = n()) %>%
    max()
  
  vnxn <- vnxn %>%
    group_by(x) %>%
    mutate(ran = min_rank(y), len = n(), y = rescale(ran, to = c(-1, 1)) * sqrt(len / maxLen) + rnorm(1, 0, .1 / len)) %>%
    ungroup() # had to put in a tiny bit of rnorm to stop some artefacts in visnetwork when nodes have same y
  # mutate(len=n(),ran=min_rank(y)-.5,y=ran*levels/(len*3))
  vnxn <- vnxn %>%
    mutate(x = 1 - scale(x) * 1, y = scale(y)) # 1 is the proportion. not sure why i added this line
  
  vnxn$x <- 1-vnxn$x
  vnxn
  
}




# helpers for tidygraph
N_ <- function(gr) gr %>% activate(nodes)
E_ <- function(gr) gr %>% activate(edges)
nodes_as_tibble <- function(gr) gr %>%
    activate(nodes) %>%
    as_tibble()
edges_as_tibble <- function(gr) gr %>%
    activate(edges) %>%
    as_tibble()

findfirst <- function(vec, vec2) {
  sapply(vec, function(x) {
    which(x == vec2) %>% first()
  })
}

merge_edges <- function(ved,this_tab,vals){
  
  if (this_tab != "Code" & findset("arrowmerge", v = vals) %>% as.logical()) {
    ved <- ved %>%
      group_by(from, to)
    
    # legend <- paste0(legend, "</br>Multiple arrows between pairs of variables collapsed into one")
  } else {
# add different curve to each edge in coterminal sets of edges--------------
    ved <- ved %>%
      group_by(from, to) %>%
      mutate(smooth.type = "continuous") %>%
      mutate(smooth.roundness = seq(from = 0, to = .8, length.out = n())) %>%
      mutate(smooth.enabled = TRUE) %>%
      ungroup() %>%
      group_by(row_number())
  }
  
  doNotification("merge edge aggregation")
  # browser()
  ved <- ved %>%
    mutate_at(vars(matches(paste_colnames(colnames_for_concat))),funs(catfun)) %>% 
    mutate_at(vars(matches(paste_colnames(colnames_for_sum))),funs(sumfun)) %>% 
    mutate_at(vars(matches(paste_colnames(colnames_for_mean))),funs(meanfun)) %>% 
    mutate(frequency = n())
  
  ved <- ved %>%
    summarise_all(.funs = funs(first))
  
  
  
  
  ved <- ved %>%
    ungroup() 
  # %>%
  #   mutate(title = paste0(frequency, gsub("[^[:alnum:][:space:]]", "", label), separate = "- "))
  
  
  
  # if ("N" %in% colnames(ved)) {
  #   ved <- ved %>%
  #     mutate(frequency = N)
  # }
  
  ved
}

join_nodes_and_edges <- function(vno, ved) {
  matchCols <- paste_colnames(unique(c(colnames_for_concat,colnames_for_sum,colnames_for_mean)))
  ved.from <- ved %>%
    mutate(id = from) %>%
    group_by(id) %>%
    select(matches(matchCols)) %>% 
    # mutate_if_sw(is.numeric, .funs = list(sum = sumfun, mean = meanfun)) %>%
    # mutate_if_sw(is.character, .funs = catfun) %>%
    mutate_at(vars(matches(paste_colnames(colnames_for_concat))),funs(catfun)) %>% 
    mutate_at(vars(matches(paste_colnames(colnames_for_sum))),funs(sumfun)) %>% 
    mutate_at(vars(matches(paste_colnames(colnames_for_mean))),funs(meanfun)) %>% 
    
    summarise_all(.funs = funs(first)) %>%
    ungroup() %>%
    rename_all(function(x) paste0("from.", x)) %>%
    rename(id = from.id)
# browser()
  vno <- vno %>%
    mutate(id = row_number()) %>%
    left_join(ved.from, by = "id")

  ved.to <- ved %>%
    mutate(id = to) %>%
    group_by(id) %>%
    select(matches(matchCols)) %>% 
    # mutate_if_sw(is.numeric, .funs = list(sum = sumfun, mean = meanfun)) %>%
    # mutate_if_sw(is.character, .funs = catfun) %>%
    mutate_at(vars(matches(paste_colnames(colnames_for_concat))),funs(catfun)) %>% 
    mutate_at(vars(matches(paste_colnames(colnames_for_sum))),funs(sumfun)) %>% 
    mutate_at(vars(matches(paste_colnames(colnames_for_mean))),funs(meanfun)) %>% 
    summarise_all(.funs = funs(first)) %>%
    ungroup() %>%
    rename_all(function(x) paste0("to.", x)) %>%
    rename(id = to.id)
  
  vno <- vno %>%
    mutate(id = row_number()) %>%
    left_join(ved.to, by = "id")
  
  # add from and to scores for nodes
  
for(col in (intersect(colnames(ved),(colnames_for_sum)) %>% setdiff("label"))){
  vno[,col] <- rowSums(vno[,c(paste0("from.",col),paste0("to.",col))],na.rm=T)
}  
  
  
for(col in (intersect(colnames(ved),(colnames_for_concat)) %>% setdiff("label"))){
  
  vno <- vno %>% 
    unite(!!col,paste0("from.",col),paste0("to.",col))
}  

  

for(col in (intersect(colnames(ved),(colnames_for_mean)) %>% setdiff("label"))){
  vno[,col] <- rowMeans(vno[,c(paste0("from.",col),paste0("to.",col))],na.rm=T)
}
  
  


  # vnofrom <- vno %>%
  #   select(starts_with("from.")) %>%
  #   select_if(is.numeric) %>%
  #   as.matrix()
  # 
  # vnoto <- vno %>%
  #   select(starts_with("to.")) %>%
  #   select_if(is.numeric) %>%
  #   as.matrix()
  # 
  # vnomean <- apply(simplify2array(list(vnofrom, vnoto)), c(1, 2), meanfun) %>% as.tibble()
  # vnosum <- apply(simplify2array(list(vnofrom, vnoto)), c(1, 2), sumfun) %>% as.tibble()
  # 
  # colnames(vnomean) <- str_remove_all(colnames(vnoto), "^to.") %>% paste0("mean_", .)
  # colnames(vnosum) <- str_remove_all(colnames(vnoto), "^to.") %>% paste0("sum_", .)
  # 
  # vno <- bind_cols(vno, vnomean, vnosum)

  # browser()
  vno %>%
    ungroup() 
 
}


set_text_contrast_color <- function(color) {
  ifelse(mean(col2rgb(color)) > 127, "black", "white")
}


format_nodes_and_edges <- function(df, inp, type, vsc) {
  # browser()
  if(nrow(df)>0){
  if (type == "node") namelist <- node_names else namelist <- edge_names
  for (attribute_short in namelist) {
    attribute <- paste0(type, "_", attribute_short)
    attribute_clean <- attribute %>% str_replace("\\.", "_")


    row <- vsc[vsc$attribute == attribute, ]
    row_clean <- vsc[vsc$attribute == attribute_clean, ]
    floor <- row$value

    # browser()
    var <- df %>% pull(row$var)
    if (0 != sum(as.numeric(var), na.rm = T)) var <- as.numeric(var) else var <- as.numeric(factor(var))
    if (row$selector == "conditional on ...") {
      ceiling <- row$value2
      if (str_detect(attribute, "color")) {
        # browser()
        if (any(is.na(var))) {
          doNotification("Missing values for colour fade replaced with means")
          var[is.na(var)] <- mean(var, na.rm = T)
        }
        
        var <- var-min(var,na.rm=T)
        # if(max(var, na.rm = T)==0) browser()
        # browser()
        
        maxvar <- max(var, na.rm = T)
        if(maxvar!=0){
          
        cat(glue("{attribute} ceiling {ceiling} floor {floor} max {max(var, na.rm = T)}"))
        if(nrow(df)>0)df[, attribute_short] <- colorRamp(c(floor, ceiling))(var / maxvar) %>%
          as.tibble() %>%
          mutate(xxx = (rgb(V1, V2, V3, maxColorValue = 255))) %>%
          pull(xxx)
        } else doNotification("All data for this formatting is the same")
        
      } else {
# browser()
        floor <- as.numeric(floor)
        ceiling <- as.numeric(ceiling)
        df[, attribute_short] <- (var / max(var, na.rm = T)) * ((ceiling - floor)) + floor
      }
    }
    else {
        cat(glue("{attribute_short} floor {floor} "))
      # if(attribute_short=="color")
        # browser()
      df[, attribute_short] <- floor
    }
  }
  }
  df
}



make_quip_stats <- function(graf) {
  browser()
  graf
}

send_to_sql <- function(values,con,user,project,table){
  # if(table=="settingsGlobal")browser()
doNotification(glue("Sending {table} to sql database"))
  dbExecute(con,glue("DELETE FROM {table} WHERE user = '{user}' AND project='{project}'"))
  temp <- values[[table]] %>% 
    mutate(user=user,project=project) %>% 
    select(user,project,everything())
  dbAppendTable(con,table,temp)
}

delete_from_sql <- function(con,user,project){
  for(c in csvlist) dbExecute(con,glue("DELETE FROM {c} WHERE user = '{user}' AND project='{project}'"))
  doNotification(glue("Deleted project {project}"))
}
# updateEdges <- function(con,user,project,edges){
#   dbExecute(con,glue("DELETE FROM edges WHERE user = '{user}' AND project='{project}'"))
#   edges <- edges %>% 
#     mutate(user=user,project=project) %>% 
#     select(user,project,everything())
#   dbAppendTable(con,"edges",edges)
#   
# }


get_user_from_query <- function(url){
  str_remove(url,"\\/.*$")
}
get_project_from_query <- function(url){
  str_remove(url,"^.*?\\/")
}


make_settingsConditional <- function(inp, vs) {


  if (is.null(inp[[paste0("conditional_value_", all_attributes[[1]])]])) {
    if (is.null(vs)) {
      defaultSettingsConditional
    } else {
      vs
    }
  }
  else {
    lis <- lapply(all_attributes, function(attribute) {
      attribute <- c(
        attribute = attribute,
        conditional_value_ = inp[[paste0("conditional_value_", attribute)]],
        conditional_selector_ = inp[[paste0("conditional_selector_", str_replace(attribute, "\\.", "_"))]],
        conditional_var_ = inp[[paste0("conditional_var_", attribute)]],
        conditional_value2_ = inp[[paste0("conditional_value2_", attribute)]]
      )
    })

    names(lis) <- paste0("X", 1:length(lis))

    liss <- bind_rows(lis)
    lisss <- t(liss)
    colnames(lisss) <- xc("attribute value selector var value2")
    lisss %>% as.tibble()
  }
}

refresh_and_filter_net <- function(tmp, vpag, iot) {   # also for the refresh button. refocusses graph on the current statement, removes any half-made arrows etc
  vno <- tmp %>% nodes_as_tibble()
  ved <- tmp %>% edges_as_tibble()
  vf <- ved %>%
    group_by(from) %>%
    summarise(fstat = paste0(statement_id, collapse = ",")) %>%
    mutate(id = from)

  vt <- ved %>%
    group_by(to) %>%
    summarise(tstat = paste0(statement_id, collapse = ",")) %>%
    mutate(id = to)

  vno <- vno %>%
    mutate(id = row_number()) %>%
    left_join(vf) %>%
    left_join(vt) %>%
    mutate(fstat = replace_na(fstat, "")) %>%
    mutate(tstat = replace_na(tstat, "")) %>%
    unite("statement_id", c("fstat", "tstat"), sep = ",")



  # browser()
  if (!("statement_id" %in% colnames(vno))) vno$statement_id <- 1

  if (!is.null(vpag) & nrow(vno) > 0) {
    ids <- vno %>%
      mutate(sel = ifelse(str_detect(statement_id, paste0("(,|^)", as.character(vpag), "(,|$)")), T, F)) %>%
      pull(sel)
    # browser()
    yesids <- ids %>% which()
    noids <- ids %>%
      `!`() %>%
      which()


    eids <- ved %>%
      mutate(hit = vpag == statement_id) %>%
      pull(hit)


    yeseids <- eids %>% which()
    noeids <- eids %>%
      `!`() %>%
      which()


    if (iot) {
      visNetworkProxy("netCoding") %>% # don't forget the ids come from values$grafAgg but the network is values$grafAgg2
        visSetSelection(unselectAll = TRUE)
    } else {
      ids <- rep(T, nrow(vno))
      eids <- rep(T, nrow(ved))

      visNetworkProxy("netCoding") %>% # don't forget the ids come from values$grafAgg but the network is values$grafAgg2
        visSelectNodes(id = yesids)
    }
    if (nrow(vno) > 0) {
      visNetworkProxy("netCoding") %>% # don't forget the ids come from values$grafAgg but the network is values$grafAgg2
        visUpdateNodes(nodes = tibble(id = 1:nrow(vno), hidden = !ids))
    }
    if (nrow(ved) > 0) {
      visNetworkProxy("netCoding") %>% # don't forget the ids come from values$grafAgg but the network is values$grafAgg2
        visUpdateEdges(edges = tibble(id = 1:nrow(ved), hidden = !eids))
    }
    visNetworkProxy("netCoding") %>%
      visFit(animation = list(duration = 500)) %>%
      visSetSelection(unselectAll = TRUE) %>%
      visSelectNodes(id = F)
  }
}





# https://stackoverflow.com/questions/31034730/graph-analysis-identify-loop-paths


# breadth first search of paths and unique loops
get_loops <- function(adj, paths, maxlen){
  # tracking the actual path length:
  maxlen <- maxlen - 1
  nxt_paths <- list()
  # iterating over all paths:
  for(path in paths$paths){
    # iterating neighbors of the last vertex in the path:
    for(nxt in adj[[path[length(path)]]]){
      # attaching the next vertex to the path:
      nxt_path <- c(path, nxt)
      if(path[1] == nxt & min(path) == nxt){
        # the next vertex is the starting vertex, we found a loop
        # we keep the loop only if the starting vertex has the 
        # lowest vertex id, to avoid having the same loops 
        # more than once
        paths$loops <- c(paths$loops, list(nxt_path))
        # if you don't need the starting vertex included 
        # at the end:
        # paths$loops <- c(paths$loops, list(path))
      }else if(!(nxt %in% path)){
        # keep the path only if we don't create 
        # an internal loop in the path
        nxt_paths <- c(nxt_paths, list(nxt_path))
      }
    }
  }
  # paths grown by one step:
  paths$paths <- nxt_paths
  if(maxlen == 0){
    # the final return when maximum search length reached
    return(paths)
  }else{
    # recursive return, to grow paths further
    return(get_loops(adj, paths, maxlen))
  }
}

adj <- list()
loops <- list()
# the maximum length to limit computation time on large graphs
# maximum could be vcount(graph), but that might take for ages
maxlen <- 4
# 
# g <- erdos.renyi.game(n = 100, p.or.m = 0.04)
# # creating an adjacency list
# for(v in V(g)){
#   # for directed graphs use the 'mode' argument of neighbors() 
#   # according to your needs ('in', 'out' or 'all')
#   adj[[as.numeric(v)]] <- neighbors(g, v)
# }


find_cycles <- function(adj){
  for(start in seq(length(adj))){
    loops <- c(loops, get_loops(adj, list(paths = list(c(start)), 
      loops = list()), maxlen)$loops)
    
  }
  loops
  
}

# get_loops_all(g)
# recursive search of loops 
# for each vertex as candidate starting point

# create_notable()




# constants ----

# **provide default nodes and edges if necessary ----

defaultNodes <- data.frame(
  # id = 1:2,
  # color.background=c("","") ,
  # color.border=c("","") ,
  # color.highlight.border = c("","") ,
  # color.highlight.background = c("","") ,
  # font.size=1:20,
  # frequency="1" ,
  # borderWidth=1:20,
  # shape=rep(20,"box",20)
  label = xc("one two"),
  details = xc("one two"),
  group = c("", ""),
  col1 = c("", ""),
  cluster = c("", ""),
  value = 0,
  level = 0,
  fun = "sumfun",
  type = ("<U+25E8>"),
  is_context = c(F, F),
  clusterLabel = c("", ""),
  stringsAsFactors = F
)



node_names <- xc("color.background color.border font.color font.size borderWidth")
edge_names <- xc("color font.color font.size width")

all_attributes <- c(paste0("node_", node_names), paste0("edge_", edge_names))

# conditional_attributes_color <- xc("font.color color.background color.border color")

writeLines("", "log.txt") # just to open up a fresh file


csvlist <- xc("nodes edges statements settingsConditional settingsGlobal statements_extra")


# generate some nice colours

allcols1 <- lapply(xc("Greys Set1 Pastel1 Reds Oranges Blues Greens Purples"), function(x) RColorBrewer::brewer.pal(8, x)) %>%
  # c(RColorBrewer::brewer.pal(8,"Pastel1"),.) %>%
  # c(RColorBrewer::brewer.pal(8,"Set1"),.) %>%
  c(xc("#FFFFFF black")) %>%
  unlist()

allcols2 <- lapply(xc("Greys Reds Blues Greens Oranges Purples"), function(x) RColorBrewer::brewer.pal(9, x)[c(2, 4, 6, 8)]) %>%
  c(xc("#FFFFFF"), .) %>%
  unlist()

allcols <- allcols2

allcols <- c(allcols, paste0(allcols, "55"))

colnams <- lapply(xc("Grey Red Blue Green Orange Purple"), function(x) xc("light mid1 mid2 dark") %>% paste0(., " ", x)) %>%
  c(xc("White"), .) %>%
  unlist()

names(allcols) <- c(colnams, paste0(colnams, " hazy"))

shapelist <- c("box", "circle", "square", "triangle", "dot", "star", "ellipse", "database", "text", "diamond")
# 
# default.sources <- tibble(
#   "source_id" =
#     rep("1", 1),
#   "key" =
#     rep("key", 1),
#   "value" =
#     rep("1", 1),
# )
default.statements_extra <- tibble(
  "statement_id" =
    rep(1, 1),
  "key" =
    rep("key", "1"),
  "value" =
    rep("1", "1"),
)

default.statements <- data.frame(
  "text" =
    rep("Some text", 1),
  "statement_id" =
    rep(1, 1),
  "source_id" =
    rep("1", 1),
  stringsAsFactors = FALSE
)


defaultEdges <- data.frame(
  # id = "1",
  from = 1,
  to = 2,
  label = "",
  strength = .5,
  trust = .5,
  sensitivity = .5,
  specificity = .5,
  statement_id = 1,
  package = "",
  packageNote = "",
  quote = "",
  # full.quote = "",
  combo.type = "",
  definition.type = "",
  # frequency="1",
  # width="" ,
  # color="" ,
  # color.opacity="",
  # blah=.5
  stringsAsFactors = F
)
conditionlist <-
  c("always", "if...", "conditional on:")



comparisonlist <-
  c("equal", "not equal", ">", "<", "contains")

ft <- c(F, T)
op <- (0:10) / 10

valuelist <- list(
  "-" =
    "-",
  "direction" =
    c("Left-to-right" = "LR", "Right-to-left" = "RL", "Top-to-bottom" = "TB", "Bottom-to-top" = "BT"),
  "dir" =
    xc("forward back both"),
  "ranksep" =
    ((1:12) / 6)^2 %>% round(1),
  "nodesep" =
    xc("1 2 3 4"),
  "borderWidth" =
    1:9,
  "rank" =
    1:15,
  "penwidth" =
    1:15,
  "layout" =
    xc("dot neato twopi circo"),
  "fixedsize" =
    xc("true false"),
  "fixedsize" =
    xc("true false"),
  "shape" =
    shapelist,
  "hidden" =
    ft,
  "dashes" =
    ft,
  "opacity" =
    op,
  "color.opacity" =
    op,
  "group" =
    1:20,
  "bgcolor" =
    allcols,
  "background" =
    allcols,
  "color" =
    allcols,
  "border" =
    allcols,
  "color.background" =
    allcols,
  "color.border" =
    allcols,
  "color.background.highlight" =
    allcols,
  "color.border.highlight" =
    allcols,
  "background" =
    allcols,
  "color" =
    allcols,
  "font.color" =
    allcols,
  "fillcolor" =
    allcols,
  "font.size" =
    ((1:20) / 2)^2,
  "fontname" =
    xc("Helvetica Times Arial Courier"),
  "font.name" =
    xc("Helvetica Times Arial Courier"),
  "wrap" =
    (1:20) * 5,
  "combo.type" =
    xc("AND OR"),
  "style" =
    c("solid", "dotted", "dashed", "invisible" = "invis"),
  "height" =
    (1:9) / 10,
  "size" =
    (1:6)^2,
  "width" =
    1:9,
  "font.size" =
    1:9,
  "label" =
    "Placeholder for legend",
  "labelloc" =
    xc("b t"),
  "labeljust" =
    xc("l r c"),
  "nodeSpacing" =
    ((1:20) / 4)^2,
  "levelSeparation" = ((1:20) / 4)^2
)

defaultSettingsConditional <- read_csv("defaultSettingsConditional.csv")
defaultSettingsGlobal <- read_csv("defaultSettingsGlobal.csv")




colnames_for_concat=xc("quote text label details statement_id domain")
colnames_for_sum=xc("frequency notForwards")
colnames_for_mean=c("sex", "Positive", "older", "female", "ava", "avp", "attributionExplicit", "attributionValence","What is the education of the main respondent?")
userlist <- xc("free Steve BSDR")

# query_modal <- modalDialog(
#   title = "Select user",
#   selectInput('input_user','Who are you:',userlist),
#   easyClose = T,            #TODO revert for production
#   footer = tagList(
#     actionButton("logon", "Log on")
#   )
# )


paste_colnames <- function(vec) vec %>% paste0(collapse="|")
# paste_colnames <- function(str) str %>% str_split("\\|") %>% `[[`(1)
