# options -----------------------------------------------------------------
options(shiny.port = 1111)
# options(shiny.trace = T)
options(shiny.autoreload = T)
options(shiny.autoreload.pattern = glob2rx("ui.R|global.R|server.R"))
doNotificationLevel=2     #notification popups. level=1 is debugging and level=2 is user.
options(stringsAsFactors = F)

# source ------------------------------------------------------------------

source("combo_functions.r")
source("functions/find_cycles.r")

# libs --------------------------------------------------------------------



library(shinythemes)
library(shinyWidgets)
library(webshot)
library(readxl)
library(scales)
library(shinyjqui)
library(tidyr)
library(stringr)
library("shinyBS")
library(shinyjs)
library(glue)
library(r2d3)
library(tidyverse)
library(rhandsontable)
library(shinyPagerUI)
library(networkD3)
library(tidygraph)
library(googledrive)
library(googlesheets4)
library(shinycssloaders)
library(rdrop2)

require(visNetwork)
require(plotly) # rgba
library(colourpicker)
library(ggraph)
library(DiagrammeR)
library(rpivotTable)  
library(shape)
library(stringi)   # just for highlight_text
library(formattable)


# attempt to bypass lack of local storage at shinyapps, both dropb --------


storage <- "dropbox"
storage <- "gsheets"
storage <- "local"


if(storage=="local" | storage=="gsheets"){
  file__exists <- file.exists
  read__csv <- read_csv
  write__csv <- write_csv
} else if(storage=="dropbox"){
  file__exists <- drop_exists
  read__csv <- drop_read_csv
  write__csv <- function(obj,path){
    # browser()
    write_csv(obj,path = path)
    drop_upload(path,"www")      # note folder is hard-coded TODO
    file.remove(path)
    
  }
}

# functions ----------------------------------------------------------------
strip_symbols <- function(vec) vec %>% str_remove_all("\\n|\\r") %>% str_replace_all("\\s+", " ") %>% str_replace_all("\\'", "")

tolower_trim <- function(vec) vec %>% tolower() %>% str_trim %>% strip_symbols
tidy_colnames <- function(df) df %>%  rename_all(tolower_trim)

id.finder <- function(label, node.df) {
  sapply(label, function(x) {
    (node.df$label == x) %>% which() %>% first()
  })
}



generalise_sprintf <- function(str,tex,...){
  
}


make_labels=function(tex,df,sep="<br>"){
  
  
  cols <- tex %>% 
    str_extract_all("\\!\\w*") %>% `[[`(1) %>%  
    str_remove("!")
  
  tex2=tex
  
  # didn't need the loop but can't remember how to pass a list to sprintf TODO
  
  for(i in cols){
  
  tex2 <- str_replace(tex2,"!\\w*","%s")
    
     if(i %in% colnames(df))tex2 <- sprintf(tex2, unlist(df[,i]) )
    
  }
  tex2
}






export_edgelist_adjacency <- function(gr){
  gr %>% 
    edges_as_tibble() %>% 
    select(from,to) %>% 
    mutate(value=1) %>% 
    spread(from,value,fill=0)
}

find_dist <- function(gr,parent){
  gr %>% activate(nodes) %>%
    mutate(pars=(local_members(id,mode="in",mindist = 1)),hh= map(pars,parent),dis=node_distance_to(unlist(hh))) %>%
    nodes_as_tibble %>%
    select(dis)  %>% unlist(recursive = F)
  }

### given a graph, tells you the distance of a loop from each node to the xth parent.
### these two almost works but doesn't give right results??

find_dist_all <- function(gr){
  gr <- gr %>% activate(nodes) %>%
    mutate(id=row_number())
  
      # find max nr of parents
  maxParents <- gr %>% activate(nodes) %>%
    mutate(id=row_number(),pars=(local_members(id,mode="in",mindist = 1))) %>%
    nodes_as_tibble %>% select(pars)  %>%
    unlist(recursive = F) %>%
    map(length) %>%
    unlist %>%
    max
  df <- map(1:maxParents,~find_dist(gr,.)) %>% data.frame
  colnames(df)=paste0("x",1:ncol(df))
  df[df==Inf]=0
  rowSums(df)
}


sort_short <- function(x)  (x)%>% paste0("x") %>% as.factor() %>% as.numeric
# like rank but with no gaps
## 
prepare_vg <- function(graf){
  # browser()
  graf %>% 
    activate(nodes) %>% 
    mutate(id = row_number(), origID = id) %>% 
    mutate(value=as.numeric(value)) %>% 
    mutate(value=if_else(value=="",0,value)) %>% 
    mutate(value=replace_na(value,0)) 
}

prepare_ved <- function(ved){
  ved %>%
    mutate_at(vars(strength, trust), funs(as.numeric)) %>%
    mutate(combo.type = ifelse(is.na(combo.type), "", combo.type)) %>%
    mutate(label = ifelse(is.na(label), "", label)) %>%
    mutate(definition.type = ifelse(is.na(definition.type), "", definition.type))
}

prepare_vno <- function(vno){
  vno %>%
    mutate(cluster = ifelse(is.na(cluster), "", cluster)) %>%
    mutate(clusterLabel = ifelse(is.na(clusterLabel), "", clusterLabel))
}

ved_join <- function(ved, statements){
  ved <- ved %>%
    left_join(statements, by = "statement")
  ved
}


merge_nodes <- function(vno,ved){
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
    # mutate(value=mean(value,na.rm=T)) %>%    
    # mutate(valueSum=sum(value,na.rm=T)) %>%    
    mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
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
  
  list(vno,ved)
  # values$tmp.graf <- tmp.graf   #for permanent cluster reduction button
  
  # legend <- paste0(legend, "</br>Variables merged according to user-defined clusters")
  
}


large="er asdfjk klasdf";  small="asdf"

highlight_text <- function(large,smallvec,start="<a href='.'>",stop="</a>"){
  
  for(small in smallvec){
  if(length(nchar(small))>0){
  if(str_detect(large,small) && nchar(small)>2){
  where <- str_locate(large,small)
  stringi::stri_sub(large,where[1],where[1]-1)<-start
  stringi::stri_sub(large,where[2]+13,where[2]+12)<-stop
  large
  } 
    }
  
  }
  large
  }



# format_edges <- function(df,input)df

get_rgb_from_color_ramp <- function(mat){
  
}





mutate_if_sw=function(...)suppressMessages(mutate_if(...))

doNotification <- function(text,level=1,...) {
  if(level>doNotificationLevel)showNotification(text,...)
  write(paste0(text), paste0("","log.txt"), append = T)
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


inv=function(ed){   #inverts an edgelist. For Rick's suggestion for cacluating a node metric
  ed %>% 
    select(from,to) %>% 
    unlist %>% 
    unique %>% 
    expand.grid(from=.,to=.,stringsAsFactors = F) %>% 
    bind_rows(ed) %>% 
    select(from,to) %>% 
    group_by(from,to) %>% 
    mutate(n=paste0(from,to)) %>% 
    group_by(from,to) %>% 
    mutate(n=n()) %>% 
    mutate(present=ifelse(n<2,0,1)) %>% 
    select(-n)
  
}

inv_multi=function(df){   #For Rick's suggestion for cacluating a node metric
  # browser()
  stats <- df %>%
    split(.$statement) %>%
    purrr::map_dfr(inv) %>% 
    group_by(from,to) %>% 
    mutate(absent=sum(1-present),present=sum(present),num=n(),avp=round(present/num,2),ava=round(absent/num,2)) %>% 
    summarise_all(dplyr::first)
  
  df %>% left_join(stats,by=c("from","to"))
  
}

infer=function(gr){                         # sets levels of downstream variables
  gr=gr %>% activate(nodes) %>% 
    mutate(source=node_is_source()) %>% 
    mutate(priorLevel=level) %>% 
    mutate(xid=row_number())
  
  # the vector of endog variables
  ids=gr %>% 
    activate(nodes) %>% 
    pull(source) %>% 
    which
  
  empties=gr %>% 
    activate(nodes) %>% 
    mutate(empty=source & is.na(level)) %>% 
    pull(empty) %>% 
    which
  
  # ridiculous palaver cause can't call bfs directly  
  # makes a long vector of all the nodes we will need to visit
  ranks=vector(length=0)
  
  for(y in ids){
    # browser()
    gr %>% 
      activate(nodes) %>% 
      mutate(rankx=bfs_rank(root=y)) %>% 
      filter(!is.na(rankx)) %>% 
      arrange(rankx) %>% 
      pull(xid) -> rk
    
    ranks=c(ranks,rk[-1])
    
  }
  
  # browser()
  # gridd=list(length=0)
  # combs for making a list of all the endogenous variables with no values set
  combs=lapply(empties,function(x)0:1) %>% expand.grid()
  
  colnames(combs)=empties
  
  gr1=gr 
  
  levs=NULL#vector(length=length(combs))
  if(nrow(combs)>0){                   # if there are any NA exog vars, we need to build a grid
  for(e in 1:nrow(combs)){
    if(length(empties)>0){
      # browser()
      row=combs[e,]
      gr1 %>% nodes_as_tibble ->jj
      jj$level[as.numeric(names(combs))]=row
      gr1=gr1 %>% activate(nodes) %>% 
        mutate(level=unlist(jj$level))
      
    } 
    for(r in ranks){
      # browser()
      gr1=  gr1 %>% 
        activate(nodes) %>% 
        mutate(level = ifelse(r!=xid,level,
                              do.call(.N()$fun[r],list(.N()$level[.E()$from[.E()$to==r]]))
        )
        )
      
    }
    levs=cbind(levs,(gr1 %>% nodes_as_tibble %>% pull(level)))
  }  # browser()
  }
  else {
      # browser()
    for(r in ranks){
# browser()
      gr1 =  gr1 %>% 
        activate(nodes) %>% 
        mutate(level = ifelse(r!=xid,level,2
        )
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


# helpers for tidygraph
N_ <- function(gr) gr %>% activate(nodes)
E_ <- function(gr) gr %>% activate(edges)
nodes_as_tibble <- function(gr) gr %>% activate(nodes) %>% as_tibble()
edges_as_tibble <- function(gr) gr %>% activate(edges) %>% as_tibble()

findfirst <- function(vec, vec2) {
  sapply(vec, function(x) {
    which(x == vec2) %>% first()
  })
}



join_nodes_and_edges <- function(vno,ved){
  ved.from <- ved %>%
    mutate(id = from) %>%
    group_by(id) %>%
    mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
    mutate_if_sw(is.character, .funs = catfun) %>%
    summarise_all(.funs = funs(first)) %>%
    ungroup() %>%
    rename_all(function(x)paste0("from.",x)) %>% 
    rename(id=from.id)
  
  vno <- vno %>%
    mutate(id = row_number()) %>%
    left_join(ved.from, by = "id")
  
  ved.to <- ved %>%
    mutate(id = to) %>%
    group_by(id) %>%
    mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
    mutate_if_sw(is.character, .funs = catfun) %>%
    summarise_all(.funs = funs(first)) %>%
    ungroup() %>%
    rename_all(function(x)paste0("to.",x)) %>% 
    rename(id=to.id)
  
  vno <- vno %>%
    mutate(id = row_number()) %>%
    left_join(ved.to, by = "id")
  
  # add from and to scores for nodes
  
  vnofrom <- vno %>% 
    select(starts_with("from.")) %>% 
    select_if(is.numeric) %>% 
    as.matrix
  
  vnoto <- vno %>% 
    select(starts_with("to.")) %>% 
    select_if(is.numeric) %>% 
    as.matrix
  
  vnomean=apply(simplify2array(list(vnofrom,vnoto)),c(1,2),meanfun) %>% as.tibble()
  vnosum=apply(simplify2array(list(vnofrom,vnoto)),c(1,2),sumfun) %>% as.tibble()
  
  colnames(vnomean)=str_remove_all(colnames(vnoto),"^to.") %>% paste0("mean_",.)
  colnames(vnosum)=str_remove_all(colnames(vnoto),"^to.") %>% paste0("sum_",.)
  
  vno=bind_cols(vno,vnomean,vnosum)
  
  # browser()
  vno <- vno %>%
    ungroup() %>%
    mutate(
      frequency = sum_frequency_sum,
      trust = sum_trust_sum,
      strength = sum_strength_sum_sum,
      wstrength = sum_wstrength_sum_sum) %>% 
    mutate(
      edgelabels = paste0(from.label, to.label),
      statement = paste0(from.statement, to.statement),
      quote = paste0(from.quote, to.quote)
    ) 
  
  
  
  
  if("mean_key1_mean_mean" %in% colnames(vno))vno=vno %>% mutate(key1=mean_key1_mean_mean) 
  if("mean_key2_mean_mean" %in% colnames(vno))vno=vno %>% mutate(key2=mean_key2_mean_mean) 
  
  vno
}


set_text_contrast_color <- function(color) { 
  ifelse( mean(col2rgb(color)) > 127, "black", "white") 
} 


format_nodes_and_edges <- function(df,inp,type){
  # if (type=="nodes") \type_names = node_names else type_names = edge_names
  # browser()
  # req(inp$conditional_selector_node_color.background)
  req(inp$conditional_value_node_font.color)
  if(type=="node")namelist <- node_names else namelist <- edge_names
  for(attribute_short in namelist){
    attribute <- paste0(type,"_",attribute_short)
    attribute_clean <- attribute %>% str_replace("\\.","_")
    
    # attribute_stripped <- str_remove_all(attribute,"node_") %>% str_remove_all("edge_")
    
    
    floor <- inp[[paste0("conditional_value_",attribute)]] 
    
    var <- inp[[paste0("conditional_var_",attribute)]] 
    var <- df %>% pull(var)
    if(0!=sum(as.numeric(var),na.rm=T)) var <- as.numeric(var) else var <- as.numeric(factor(var))
    # browser()
    if(inp[[paste0("conditional_selector_",attribute_clean)]]=="conditional on ...") {
    ceiling <- inp[[paste0("conditional_value2_",attribute)]] 
      if(str_detect(attribute,"color")){
        # browser()
        if(any(is.na(var))){
          doNotification("Missing values for colour fade replaced with means")
          var[is.na(var)] <- mean(var,na.rm=T)
        }
        df[,attribute_short] <- colorRamp(c(floor,ceiling))(var/max(var,na.rm=T)) %>% 
          as.tibble %>% 
          mutate(xxx=(rgb(V1,V2,V3,maxColorValue = 255))) %>% 
          pull(xxx)
      } else  {
        floor <- as.numeric(floor)
        ceiling <- as.numeric(ceiling)
        df[,attribute_short] <- (var/max(var,na.rm=T))*((ceiling-floor))  + floor
      }
    }
    else {
      
      df[,attribute_short] <- floor
    }
  }
  df
}



make_quip_stats <- function(graf){
  browser()
  graf
}



make_settingsConditional <- function(inp){
  # browser()
  lis <- lapply(all_attributes,function(attribute){
    attribute=c(
      attribute=attribute
      ,
      conditional_value_=inp[[paste0('conditional_value_', attribute)]]
      ,
      conditional_selector_=inp[[paste0('conditional_selector_', str_replace(attribute,"\\.","_"))]]
      ,
      conditional_var_=inp[[paste0('conditional_var_', attribute)]]
      ,
      conditional_value2_=inp[[paste0('conditional_value2_', attribute)]]
    )
  }) 
  
  names(lis)=paste0("X",1:length(lis))
  
    liss <- bind_rows(lis)
    # liss[nrow(liss)+1,] <- c(rep("node",length(node_names)),rep("edge",length(edge_names)))
    lisss <- t(liss)
    colnames(lisss) <- xc("attribute value selector var value2")
    lisss %>% as.tibble
}


# constants ----


node_names=xc("color.background color.border font.color font.size borderWidth")
edge_names=xc("color font.color font.size width")

all_attributes=c(paste0("node_",node_names),paste0("edge_",edge_names))

# conditional_attributes_color <- xc("font.color color.background color.border color")

writeLines("", "log.txt") # just to open up a fresh file


csvlist <- xc("nodes edges settingsConditional statements settingsGlobal")


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

default.statements <- data.frame(
  "text" =
    rep("Some text", 1),
  "key1" =
    rep("", 1),
  "key2" =
    rep("", 1),
  "key3" =
    rep("", 1),
  "key4" =
    rep("", 1),
  "key5" =
    rep("", 1),
  "key6" =
    rep("", 1),
  "key7" =
    rep("", 1),
  "statement" =
    rep(1, 1),
  stringsAsFactors = FALSE
)


defaultEdges <- data.frame(
  # id = "1",
  from =  1,
  to =  2,
  label = "",
  strength = .5,
  trust =  .5,
  sensitivity = .5,
  specificity =  .5,
  statement =  1,
  quote =  "",
  full.quote =  "",
  combo.type = "",
  definition.type =  "",
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

