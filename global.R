# options -----------------------------------------------------------------
options(shiny.port = 1111)
options(shiny.autoreload = T)
options(shiny.autoreload.pattern = glob2rx("ui.R|global.R|server.R")) #TODO dev only
doNotificationLevel <- 0 # notification popups. level=1 is debugging and level=2 is user.
options(stringsAsFactors = F)

# source ------------------------------------------------------------------

# source("functions/find_cycles.r")

# libs --------------------------------------------------------------------

xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}



library(RMariaDB)

# library(whereami) #TODO testing only


library(shinythemes)
library(shinylogs)
library(stringi)
library(future)
library(Hmisc)
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




# constants ----

# **provide default nodes and edges if necessary ----

mygreen <- "#226622"

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
  note = "",
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

query_modal <- modalDialog(
  title = "Select user",
  selectInput('input_user','Who are you:',userlist),
  easyClose = T,            #TODO revert for production
  footer = tagList(
    actionButton("logon", "Log on")
  )
)


paste_colnames <- function(vec) vec %>% paste0(collapse="|")
# paste_colnames <- function(str) str %>% str_split("\\|") %>% `[[`(1)


