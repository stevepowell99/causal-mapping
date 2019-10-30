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


generalise_sprintf <- function(str, tex, ...) {
  
}

spacer <- function(width=100){
  div(class="myelement",style=paste0("width:",width,"px"))
}

max_button <- function(max){
  actionButton("pager_max",max) %>% div(class="pager_class")
  # selectInput("pager_max",label = NULL,choices=max,selected = max) %>% div(class="pager_class",style="max-width:110px;")
}
current_button <- function(current,max){
  selectInput("pager__page_current",label = NULL,choices=1:max,selected = current) %>% div(class="pager_class",style="width:110px;")
}

one_button <- function()actionButton("pager_one","1")  %>% div(class="pager_class")
nxt_button <- function()actionButton("pager_nxt",icon("arrow-right"))  %>% div(class="pager_class")
prv_button <- function()actionButton("pager_prv",icon("arrow-left"))  %>% div(class="pager_class")

pager2 <- function(current,max){
  # browser()

  if(is.null(max)){
    return("no statements")
  } 
  else if(is.na(max)){
    return("no statements")
  }
  # else if(max==1){
  #   return(current_button(current,max))
  # }
  # else if(max==current){
  #   return(tagList(one_button(),prv_button(),current_button(current,max)))
  # }
  # else if(1==current){
  # return(tagList(current_button(current,max),nxt_button(),max_button(max)))
  # }
  # else 
    if(T){
    # if(max>current){
  content <- tagList(one_button(),prv_button(),current_button(current,max),nxt_button(),max_button(max))
  return(content)
  }
}






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

codings_with_statements <- function(values){
  values$statements %>% 
    left_join(values$codings,by="statement_id")
}

mixcol <- function(n,selected,pal=rainbow,clicked){ #returns a single rainbow colour, maybe a mix of several
  # browser()
  palet <- pal(n)
  
  
  if(length(selected)>1){
    result <- palet[selected] %>% 
      col2rgb %>% 
      rowMeans
    
    result <- rgb(result[1],result[2],result[3], maxColorValue = 256) %>% 
      paste0("55")
    
  } else 
    result <- replace_na(palet[selected],"#FFFFFF") %>% 
    str_sub(1,7) %>% 
    paste0("55")
  
  
  if(is.null(clicked)) result else  {
    if(clicked %in% selected) paste0(result,";text-decoration: underline") else result
    }
    
}

highlight_found_text <- function(large,small){
    str_replace_all(tolower(large),paste0("(",tolower(small),")"),"<span style='background-color:yellow'>\\1</span>")
  }


split_contiguous <- function(string,df){
  
  colnames=colnames(df)
  # browser()
  
  vec=df[,1] %>% unlist
  df=df[,-1]
  
  if(vec[1]!=1) {vec <- c(1,vec);df <- rbind(df[1,],df);df[1,] <- NA}
  # if(vec[length(vec)]!=nchar(string)) vec <- c(vec,nchar(string)+1) else vec <- c(vec,nchar(string)+1) 
  vec <- c(vec,nchar(string)+1) 
  # att <- c(att,NA) 
  p1 <- vec[-length(vec)]
  p2 <- vec[-1]-1
  # df <- df[-nrow(df),]
  res <- cbind(strings=stri_sub(string,p1,p2),df)
  colnames(res) <- colnames
  res
}

highlight_text <- function(large, smallvec, selectedEdge=NULL,codevec=seq_along(smallvec)) {
  # browser()
  hits=tibble(start=rep(0,length(smallvec)),stop=rep(0,length(smallvec)),codevec=codevec)
  large <- str_remove_all(large," *\\[.*?\\] *") %>% cleanfun %>% strip_symbols %>% 
  replace_na(,"")
if(length(large)>0 & length(smallvec)>0){
    for (s in seq_along(smallvec)) {
      
      small <- str_remove_all(smallvec[s]," *\\[.*?\\] *") %>% cleanfun %>% strip_symbols
      if (length(nchar(small)) > 0) {
        # if (str_detect((large), (small))) {
        # where <- str_locate((large), (small))
        if (str_detect((large), escapeRegex(small))) {
          where <- str_locate((large), escapeRegex(small))
          hits[s,1]=where[1]
          hits[s,2]=where[2]+1
        }
      }
    }
    edges=NULL
    codes=NULL
    hits$startcols=""
    hits$startedges=""
    hits$startcodes=""
    hits$stopcols=""
    hits$stopedges=""
    hits$stopcodes=""
    result="<span>"
    # for(char in 1:2){
    for(char in 1:nchar(large)){
      # browser()
      if(char %in% (hits$start)) {
        edge <- which(char == (hits$start))
        edges <- c(edges,edge) %>% unique
        codes <- c(codes,codevec[char == (hits$start)]) %>% unique
        hits$startcols[edge]=mixcol(nrow(hits),selected = edges,clicked=selectedEdge)
        hits$startedges[edge]=paste0(edges,collapse=" | ")
        hits$startcodes[edge]=paste0(codes,collapse=" | ")
        }
      if(char %in% (hits$stop)) {
        edge <- which(char == (hits$stop))
        edges <- setdiff(edges,edge)
        codes <- setdiff(codes,codevec[char == (hits$stop)])
        hits$stopcols[edge]=mixcol(nrow(hits),selected = edges,clicked=selectedEdge)
        hits$stopedges[edge]=paste0(edges,collapse=" | ")
        hits$stopcodes[edge]=paste0(codes,collapse=" | ")
      } 
    }
    
    
    starthits <- hits %>% select(starts_with("start")) %>% select(position=1,cols=2,edges=3,codes=4)
    stophits <- hits %>% select(starts_with("stop")) %>% select(position=1,cols=2,edges=3,codes=4)
    
    
    
    allhits <- rbind(starthits,stophits) %>% 
      arrange(position)

      split_contiguous(large,allhits) %>% 
    pmap(~span(..1,style=paste0("background-color: ",..2)),class=paste0("'",..3,"'"),title=paste0("'",..3,"'"))

    } else ""
}



join_codings_to_statements <- function(values){
  values$statements %>%
    left_join(values$codings,by="statement_id") 
}


mutate_if_sw <- function(...) suppressMessages(mutate_if(...))

doNotification <- function(text, level = 1, ...) {
  if (level > doNotificationLevel) showNotification(glue(text), ...)
  write(paste0(text), paste0("", "log.txt"), append = T)
}


xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}




set_text_contrast_color <- function(color) {
  ifelse(mean(col2rgb(color)) > 127, "black", "white")
}



send_to_sql <- function(values,con,user,project,table){
  # if(table=="statements_extra") browser()
  # browser()
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




mycol <- function(pal,num){
  pal[num]
}


make_label_list <- function(label) div(div(class="myelement"),div(label,class="myelement"))
# make_label_list <- function(label) div(div(switchInput(paste0("label_list_",label),NULL,onLabel = "EDIT",offLabel = "EDIT?"),class="myelement"),div(label,class="myelement"))
  
