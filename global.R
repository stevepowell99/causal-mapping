# options -----------------------------------------------------------------
options(shiny.port = 1111)
doNotificationLevel=0     #notification popups. level=1 is debugging and level=2 is user.
options(stringsAsFactors = F)

# source ------------------------------------------------------------------

source("combo_functions.r")

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

library(rdrop2)

require(visNetwork)
require(plotly) # rgba
library(colourpicker)
library(ggraph)
library(DiagrammeR)
library(rpivotTable)  
library(shape)



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

mutate_if_sw=function(...)suppressMessages(mutate_if(...))

doNotification <- function(text,level=1) {
  if(level>doNotificationLevel)showNotification(text)
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
  ranks=vector(length=0)
  
  for(y in ids){
    # browser()
    gr %>% 
      activate(nodes) %>% 
      mutate(rank=bfs_rank(root=y)) %>% 
      filter(!is.na(rank)) %>% 
      arrange(rank) %>% 
      pull(xid) -> rk
    ranks=c(ranks,rk[-1])
    
  }
  
  # gridd=list(length=0)
  
  combs=lapply(empties,function(x)0:1) %>% expand.grid()
  
  colnames(combs)=empties
  
  gr1=gr 
  
  levs=NULL#vector(length=length(combs))
  
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
      gr1=  gr1 %>% 
        activate(nodes) %>% 
        mutate(level = ifelse(r!=xid,level,
                              do.call(.N()$fun[r],list(.N()$level[.E()$from[.E()$to==r]]))
        )
        )
      
    }
    levs=cbind(levs,(gr1 %>% nodes_as_tibble %>% pull(level)))
  }  # browser()
 
  if(length(empties)>0){
    means=rowMeans(levs,na.rm=T)
    gr = gr %>% activate(nodes) %>% 
      mutate(level=means)
  } else gr = gr1
  
  gr
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



# constants ----

writeLines("", "log.txt") # just to open up a fresh file


csvlist <- xc("nodes edges settings statements settingsGlobal")


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

defaultSettings <- read_csv("defaultSettings.csv")
defaultSettingsGlobal <- read_csv("defaultSettingsGlobal.csv")

