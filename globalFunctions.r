### PUT ME BACK INTO GLOBAL. I AM ONLY HERE TO SAVE RESTARTING EVERY TIME

cleanfun=function(tex){
  tex %>% 
    str_remove("\\]") %>% 
    str_remove("\\[") %>% 
    str_remove("\\'") %>% 
    str_remove('\\"') %>% 
    # gsub("[^[:alnum:]/-\\?[:space:]]","",.) %>% 
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


get_node_column <- function(gr,col="label"){
  gr %>% 
    nodes_as_tibble %>% 
    pull(col)
  
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


create_auto_groups <- function(ved){
  # browser()  
  
  # ved$auto_group_key <- ved$`Household code` %>% as.character
  ved$auto_group_key <- ved$statement_id %>% as.character
  
  res <- ved  %>% 
    select(from,to) %>%
    split(ved$auto_group_key) %>% 
    # split(ved$statement_id) %>% 
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
  
  
  redo <- data.frame(auto_group=clus %>% cutree(k=3)) %>% 
    rownames_to_column(var="auto_group_key")   
  # %>%
  #   mutate(auto_group_key=as.character(auto_group_key))
  
  left_join(ved,redo,by="auto_group_key")
  
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
    mutate(value = replace_na(value, 0)) %>% 
    mutate(original_is_driver=node_is_source(),original_is_outcome=node_is_sink())
  
  
}

prepare_ved <- function(ved) {
  ved %>%
    mutate_at(vars(strength, trust), funs(as.numeric)) %>%
    mutate(combo.type = ifelse(is.na(combo.type), "", combo.type)) %>%
    mutate(label = ifelse(is.na(label), "", label)) %>%
    mutate(definition.type = ifelse(is.na(definition.type), "", definition.type)) %>% 
    mutate(arrows.middle.enabled = as.logical(F)) %>%
    mutate(arrows.to = as.logical(T)) %>%
    mutate(wstrength = strength * trust)
  
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
    mutate(clusterLabel = cluster %>% replaceNA()) %>%
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


mixcol <- function(n,selected,pal=rainbow,clicked){ #returns a single rainbow colour, maybe a mix of several
  # browser()
  
  # if(length(selected)==0)return ("white") else return("blue")
  palet <- pal(n)
  
  
  if(length(selected)>1){
    result <- palet[selected] %>% 
      col2rgb %>% 
      rowMeans
    
    result <- rgb(result[1],result[2],result[3], maxColorValue = 256) %>% 
      paste0("55")
    
  } else if(length(selected)==1){
    result <- replace_na(palet[selected],"#FFFFFF") %>% 
    str_sub(1,7) %>% 
    paste0("55")
  }
  else result="FFFFFF"
    
  if(clicked %in% selected) paste0(result,";text-decoration: underline") else result
    
}


large <- ""
small <- ""
highlight_text <- function(large, smallvec, selectedEdge) {
  # browser()
  hits=tibble(start=rep(0,length(smallvec)),stop=rep(0,length(smallvec)))
  large <- str_remove_all(large," *\\[.*?\\] *") %>% cleanfun %>% strip_symbols
  if(is.na(large))large <- ""
  if(length(large)>0 & length(smallvec)>0){
    for (s in seq_along(smallvec)) {
      
      small <- str_remove_all(smallvec[s]," *\\[.*?\\] *") %>% cleanfun %>% strip_symbols
      if (length(nchar(small)) > 0) {
        # if (str_detect((large), (small))) {
        # where <- str_locate((large), (small))
        if (str_detect((large), escapeRegex(small))) {
          where <- str_locate((large), escapeRegex(small))
          hits[s,1]=where[1]
          hits[s,2]=where[2]
        }
      }
    }
    # browser()
    # cat(hits)
    edges=NULL
    result="<span>"
    for(char in 1:nchar(large)){
      # browser()
      if(char %in% (hits$start)) {
        edges <- c(edges,which(char == (hits$start))) %>% unique}
      if(char %in% (hits$stop)) {
        edges <- setdiff(edges,which(char == (hits$stop)))
      } 
      
      # if(!is.null(edges))browser()
      # cat(edges)
      colour <- mixcol(nrow(hits),selected = edges,clicked=selectedEdge) 
      # colour <- if(length(edges)>0) mixcol(nrow(hits),selected = edges) else "#FFFFFF"
      # browser()
      result <- c(result,
        ifelse(char %in% (hits$start),
          paste0("</span><span style='background-color:",colour,"'>"),""),
        substr(large,char,char)
      )
      result <- c(result,
        ifelse(char %in% (hits$stop),
          paste0("</span><span style='background-color:",colour,"'>"),"")
      )
    }
    
    # browser()
    result       %>%
      c("</span>") %>%
      paste0(collapse="")
    
    # browser()
    # hits 
  } else ""
}


# highlight_text <- function(large, smallvec, start = "<span>", stop = "</span>") {
#   # browser()
#   if(length(large)>0 & length(smallvec)>0){
#     for (small in smallvec) {
#       small <- str_remove_all(small," *\\[.*?\\] *") %>% cleanfun %>% strip_symbols
#       large <- str_remove_all(large," *\\[.*?\\] *") %>% cleanfun %>% strip_symbols
#       if (length(nchar(small)) > 0) {
#         if (str_detect(large, small) && nchar(small) > 2) {
#           where <- str_locate(large, small)
#           stringi::stri_sub(large, where[1], where[1] - 1) <- start
#           stringi::stri_sub(large, where[2] + 7, where[2] + 6) <- stop
#           large
#         }
#       }
#     }
#     large
#   } else ""
# }
# 

# highlight_text <- function(large, smallvec, start = "<a href='.'>", stop = "</a>") {
#   # browser()
#   if(length(large)>0 & length(smallvec)>0){
#     for (small in smallvec) {
#       small <- str_remove_all(small," *\\[.*?\\] *")
#       if (length(nchar(small)) > 0) {
#         if (str_detect(large, small) && nchar(small) > 2) {
#           where <- str_locate(large, small)
#           stringi::stri_sub(large, where[1], where[1] - 1) <- start
#           stringi::stri_sub(large, where[2] + 13, where[2] + 12) <- stop
#           large
#         }
#       }
#     }
#     large
#   } else ""
# }
# 


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


render_network <- function(vga,vals,type){
  coding <- type=="Coding"
  
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
      # hover =T,
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
      highlightNearest = if(coding) F else list(
        enabled = T,
        # degree=99,
        degree = list(from=9,to=9),#if (findset("diagramdownarrows",vals) %>% as.logical()) list(from = 0, to = 19) else list(from = 19, to = 0),
        hover = T,
        labelOnly = T,
        algorithm = "hierarchical"
        # algorithm = "all"
      ),
      selectedBy = if (!("cluster" %in% colnames(vga %>% nodes_as_tibble()))) "" else ifelse((vga %>% nodes_as_tibble() %>% pull(cluster) %>% replace_na("") %>% `==`("") %>% all()), "", "cluster"),
      nodesIdSelection = F
    ) %>%
    visEvents(select = "function(data) {
                Shiny.onInputChange('net_selected', data.nodes);
                Shiny.onInputChange('net_selectedEdges', data.edges);
                ;}",
      , hoverEdge = "function(edges) {
    Shiny.onInputChange('net_hoverEdges', edges);
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
      # color=list(highlight="red"),
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
        list(middle = list(type = "circle", scaleFactor = .5)),
      # list(middle = list(type = "circle", scaleFactor = .5), from = list(type = "circle", scaleFactor = 0.2)),
      # ,
      # dashes = findset("arrowdashes") %>% as.logical()
    )
}













convert_rawGraf_to_codeGraf <- function(vgraf,vstat,vstate,vals){
  
  # prepare statements, split columns ----
  
  
  if(is.null(vstate)){
    vstate <- vstat %>%
      unique %>% 
      spread(key,value,convert=T) 
  }
  
  doNotification("starting aggregation")
  legend <- ""
  
  
  # browser()
  tmp <- prepare_vg(vgraf)
  
  
  tmp <- tmp %>% 
    activate(nodes) %>% 
    mutate(degree=centrality_degree(mode="total")) %>% 
    mutate(title=degree) 
  
  
  
  
  vno <- tmp %>% nodes_as_tibble()
  ved <- tmp %>% edges_as_tibble()
  
  ved <- prepare_ved(ved)
  
  
  
  ved <- ved %>%
    left_join(vstat, by = "statement_id") %>% 
    left_join(vstate,by = "statement_id")
  
  # merge edges but don't, curve them instead -------------------------------------------------------------
  
  
  ved <- merge_edges(ved,"Code",vals)
  
  
  # create statement groups -------------------------------------------------
  
  if(nrow(ved)>70)ved <- create_auto_groups(ved) else {
    if(nrow(ved)>0)ved$auto_group <- 1
    # doNotification("Not enough edges to cluster")
  }     
  
  # browser()
  
  vno$font.color <- "#eeeeee"
  vno$color.background <- mygreen
  vno$color.highlight.border <- "blue"
  vno$color.highlight.background <- "#aaddaa"
  vno$font.size <- findset("variablecoding.font.size",vals)
  
  
  if(nrow(ved)>0)ved$width <- 3
  # ved$title <- "ved"
  # ved$title <- ved$quote
  # ved$label <- ved$quote
  # ved$color <- ifelse(ved$quote!="",mygreen,"red")
  # infer ----
  
  tbl_graph(vno,ved)
  
}


convert_codeGraf_to_displayGraf <- function(tmp,filterVec,vals,this_tab,input,vsetcond){
  
  # tmp <-  
  
  vno <- tmp %>% nodes_as_tibble()
  
  # browser()
  ved <- tmp %>% edges_as_tibble() %>% 
    filter(filterVec)
  
  if (findset("variableinfer", v = vals) %>% as.logical()) {
    tmp <- infer(tmp)
    legend <- paste0(legend, "</br>Causal inference carried out")
  }
  
  
  # merge nodes -------------------------------------------------------------
  
  
  
  if ((findset("variablemerge",vals) %>% as.logical()) & this_tab != "Code") { # need to convert to and froms in edge df
    
    x <- merge_nodes(vno, ved)
    vno <- x[[1]]
    ved <- x[[2]]
  }
  
  
  # ved rick inv_multi --------------------------------
  
  if (("from" %in% colnames(ved)) && as.logical(findset("arrowabsence",vals))) { # todo findset
    
    doNotification("rick aggregation")
    
    if (all(is.na(ved$statement_id))) ved$statement_id <- 1
    # browser()
    ved <- ved %>%
      inv_multi()
  }
  
  
  # ved join statements--------------------------------
  # browser()
  # if (is.null(values$statements$source_id)) values$statements <- values$statements %>% mutate(source_id = 1)
  
  
  
  # quip stats by question/domain---------------
  
  
  if ("source_id" %in% colnames(ved) && "question" %in% colnames(ved)) {
    
    # browser()
    ved <- ved %>%
      group_by(from, to, question) %>%
      mutate(citation_count = length(unique(source))) %>%
      ungroup() %>%
      group_by(from, to) %>%
      mutate(respondent_count = length(unique(source))) %>%
      ungroup() %>%
      mutate(citation_intensity = citation_count / respondent_count)
  }
  
  # merge edges -------------------------------------------------------------
  
  
  ved <- merge_edges(ved,this_tab,vals)
  
  doNotification("min freq aggregation")
  
  
  # browser()
  # browser()
  if (input$sides != "Code") {
    # if (this_tab != "Code") {
    
    
    # join edges with nodes ---------------------------------------------------
    
    
    doNotification("join to edges aggregation")
    
    
    if (findset("variablejoinedges", v = vals) %>% as.logical()) { # todo, should list any functions. variablejoinedges is pointless
      
      vno <- join_nodes_and_edges(vno, ved)
      
      
      # doNotification("merging nodes and arrows")
    }
    
    
    
    # minimum freq for vars and edges
    # browser()
    mf <- findset("variableminimum.frequency", v = vals) %>% as.numeric()
    if (this_tab != "Code" && mf > 0) {
      # values$mergedGraf <- tbl_graph(vno, ved) 
      
      tmp <- tbl_graph(vno, ved)  %>%
        N_() %>%
        filter(frequency > mf)
      
      vno <- tmp %>% nodes_as_tibble()
      ved <- tmp %>% edges_as_tibble()  %>%
        filter(frequency > findset("arrowminimum.frequency", v = vals))
      
      
    }
  }
  
  
  doNotification("format aggregation")
  
  
  
  tmp <- tbl_graph(vno, ved)
  # browser()
  
  # layout just for notForwards ----------------
  
  
  layout <- create_layout(tmp, layout = "sugiyama") %>%
    select(x, y, id)
  
  tmp <- tmp %>%
    activate(nodes) %>%
    left_join(layout, by = "id")
  
  tmp <- tmp %>%
    activate(edges) %>%
    mutate(fromLevel = .N()$y[from], toLevel = .N()$y[to], notForwards = fromLevel >= toLevel)
  
  
  vno <- tmp %>% nodes_as_tibble()
  ved <- tmp %>% edges_as_tibble()
  
  # browser()
  # if (this_tab != "Code") { 
  vno <- vno %>%
    format_nodes_and_edges(input, type = "node", vsetcond)
  
  
  ved <- ved %>%
    format_nodes_and_edges(input, type = "edge", vsetcond)
  # } else {
  # browser()
  
  # }
  ### make sure text is visibile when highlighted
  vno <- vno %>%
    mutate(color.highlight.background = set_text_contrast_color(font.color)) %>%
    mutate(color.background = add_opacity_vec(color.background, as.numeric(findset("variableopacity", v = vals))))
  
  
  # margin--------
  # browser()
  vno <- vno %>%
    mutate(margin = 10) # decent approximation
  
  
  
  # rationalise----
  
  doNotification("final aggregation")
  
  # hard-coded formatting ----
  # browser()
  ved <- ved %>%
    mutate(label = replace_na(label, "")) %>%
    mutate(label = ifelse(strength < 0, paste0("<U+0001F500> ", label), label)) %>%
    # mutate(combo.type <- replace_na(combo.type,"")) %>%
    # mutate(arrows.middle.enabled = ifelse(combo.type == "", F, T)) %>%
    # mutate(arrows.middle.enabled = 0) %>%
    # mutate(label = paste0(label, ifelse(arrows.middle.enabled, paste0(" ", combo.type), ""))) %>%
    mutate(dashes = str_remove_all(definition.type,",") != "") %>%
    mutate(arrows.to = definition.type != "Defined, undirected")
  
  
  # labels ----
  vno <- vno %>%
    mutate(label = if_else(value > 0, paste0(label, " <U+2665>"), label)) %>%
    mutate(label = if_else(value < 0, paste0(label, " <U+2639>"), label)) 
  # browser()
  vno <- vno %>%
    mutate(label = make_labels(findset("variablelabel", v = vals),findset("variablewrap", v = vals), vno,type="none"))
  
  vno <- vno %>%
    mutate(title = make_labels(findset("variabletooltip", v = vals),findset("variablewrap", v = vals), vno,type="html"))
  
  
  ved <- ved %>%
    mutate(
      title = make_labels(findset("arrowtooltip", v = vals),findset("arrowrap", v = vals), ved,type="html"),
      label = make_labels(findset("arrowlabel", v = vals),findset("arrowwrap", v = vals), ved,type="none")
    )
  
  
  # fontsize ceiling so it doesn't crash. doesn't take into account if there are very long words
  # if (this_tab != "Code")      vno <- vno %>% mutate(font.size =  findset("variablecoding.font.size",v = vals)
  # )
  # browser()
  
  tmp <- tbl_graph(vno, ved)
  
  
  
  # tidygraph calculations --------------------------------------------------
  
  tmp <- tmp %>% 
    mutate(is_driver=node_is_source(),is_outcome=node_is_sink())
  
  # autogroup
  
  if (findset("variableautogroup", v = vals)) {
    tmp <- tmp %>%
      N_() %>%
      mutate(group = group_walktrap())
  }
  
  
  tmp
  
  # browser()
  
  
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


format_nodes_and_edges <- function(df, inp, type, vsc,vsetcond) {
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
  # if(table=="statements_extra") browser()
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

refresh_and_filter_net <- function(tmp, vpag, iot,fromStack=NULL,reveal=NULL) {   
  # also for the refresh button. refocusses graph on the current statement, removes any half-made arrows etc
  # this part just works out which edges and nodes belong to this statement 
  # browser()
  
  vno <- tmp %>% nodes_as_tibble()
  ved <- tmp %>% edges_as_tibble()
  # browser()
  
  ved <- ved %>% 
    mutate(hidden=!((statement_id == vpag) | !iot))
  
  
  
  
  theseIDs <- ved %>% 
    filter(!hidden) %>% 
    select(from,to) %>% 
    unlist %>% 
    c(as.numeric(fromStack),reveal) %>% 
    unique
  
  # browser()
  
  vno <- vno %>% 
    mutate(id=row_number(),hidden=!(id %in% theseIDs),
      color.background=if_else(id %in% as.numeric(fromStack),"blue",mygreen),
      font.size=15+5*(sqrt(nrow(vno)))) 
  
  
  
  
  ved <- ved %>% 
    group_by(statement_id) %>% 
    mutate(rainbow=row_number())
  
  # if (!("statement_id" %in% colnames(vno))) vno$statement_id <- 1
  
  if (!is.null(vpag) & nrow(vno) > 0) {
    
    if (nrow(vno) > 0) {
      # browser()
      # nods <- tibble(id = 1:nrow(vno), hidden = !ids,color=if_else(id %in% as.numeric(fromStack),"blue",mygreen),font.size=15+5*(sqrt(nrow(vno))))  #TODO THIS IS A TOTAL HACK
      
      # visNetworkProxy("codeNet") %>% 
      nrv <- nrow(ved)
      nrvh <- sum(!(ved$hidden)&(ved$quote!=""),na.rm=T)
      # ved <- ved %>% 
      #   mutate(colrs=ifelse(ved$strength<0,"red",ifelse(iot & !hidden,"mygreen"))
      # colours=
      # rainbow(nrvh)[1:nrvh]
      
      
      
      
      if (nrv > 0) {
        # browser()
        
        newedges <- tibble(id=1:nrow(ved),dashes=ifelse(ved$definition.type=="",F,T),hidden=ved$hidden,
          font.size=36,selectionWidth=18,
          color=ifelse(ved$hidden | !iot,ifelse(ved$strength<0,"red","mygreen"),rainbow(nrvh)[ved$rainbow]),
          width=ifelse(ved$quote!="",5,20))
        
        visNetworkProxy("codeNet") %>% 
          visUpdateNodes(nodes = vno) %>% 
          visUpdateEdges(edges = newedges) %>%
          # visUpdateEdges(edges = tibble(id=1:nrow(ved),hidden=ved$hidden,label=ifelse(ved$quote=="","red",""))) %>%
          # visUpdateEdges(edges = tibble(id=1:nrow(ved),hidden=ved$hidden,color=ifelse(ved$quote=="","red",mygreen),label=ifelse(ved$strength<0,"MINUS",ved$label))) %>%
          visFit(animation = list(duration = 500)) 
      } else {
        
        visNetworkProxy("codeNet") %>% 
          visUpdateNodes(nodes = vno) %>% 
          visFit(animation = list(duration = 500)) 
        
      }
      
    }
  }
}


mycol <- function(pal,num){
  pal[num]
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

