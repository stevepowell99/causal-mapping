# gfun1=function(funname){
#   funfun4(funname) %>% 
#     filter(strength2==0) %>% 
#     filter(B2==0) %>% 
#     mutate(fun=as.character(fun)) %>% 
#     ggplot(aes(B,E,group=ep,colour=ep))+
#     # geom_point()+
#     geom_jitter(size=3,height = .005,width = .005)+
#     # geom_line(size=1)+
#     xlab("Level of influence variable")+
#     ylab("Posterior value of consequence variable")+
#     ggtitle(funname)+
#     guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
# }
# 
# gfun1("NECC")
# gfun1("SUFF")
# gfun1("PLUS")
# gfun1("MINUS")
# 
# gfun2=function(funname){
#   funfun4(funname) %>%
#     ggplot(aes(B,unlist(E),group=ep,colour=ep))+
#     # geom_point()+
#     geom_jitter(size=3,height = .005,width = .005)+
#     geom_line(size=1)+
#     xlab("Level of influence variable")+
#     ylab("Posterior value of consequence variable")+
#     facet_grid(.~strength)+
#     ggtitle(funname,sub="                             strength of influence")+
#     guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
# }
# 
# # scens=(0:4)/4
# gfun3("NECC")
# gfun2("SUFF")
# gfun2("PLUS")
# gfun2("MINUS")
# # main file for combo functions


# aggregation functions ---------------------------------------------------



mutfun <- function(vec) {
  if (allNum(vec)) {
    sum(as.numeric(vec), na.rm = T)
  } else {
    first(vec)
  }
}

catfun <- function(x) {
  x <- na.omit(x)
  x <- x[x != ""]
  paste0(x, collapse = ",")
}

andfun <- function(x) {
  x <- na.omit(x)
  x <- x[x != ""]
  all(as.logical(x)) %>% as.numeric
}

orfun <- function(x) {
  x <- na.omit(x)
  x <- x[x != ""]
  any(as.logical(x)) %>% as.numeric
}

sumfun <- function(x) {
  # x[is.na(x)]=0
  # x <- sum(as.numeric(x), na.rm = T) %>% round(2)
  # x
  # x[1]
  # 
  sum(x,na.rm=T) 
}

meanfun <- function(x) {
  # x[is.na(x)]=0
  # x <- mean(as.numeric(x), na.rm = T) %>% round(2)
  # x
  # x[1]
  mean(x,na.rm=T) 
}

catfun=function(x)paste0(x,collapse=",")









SUM <- function(v,list){
  list %>% unlist %>% sum(na.rm=t)
}

MIN <- function(Ep,list){
  list %>% unlist %>% min(na.rm=t)
}

MAX <- function(Ep,list){
  list %>% unlist %>% max(na.rm=t)
}





# new versions -----------------------------

PLUS <- function(EP,B) B
MINUS <- function(EP,B) 1-B
NECC <- function(EP,B) B*EP

SUFF = function(Ep,B){
 1-(Ep-1)*(B-1)
}

MAKE <- function(list,strengthList,EP){
  lapply( seq_along(list),function(i){
   (list[[i]] - EP) *strengthList[[i]]
  }
  )
}

calculate_increments <- function(list,strengthList,EP){
  lapply( seq_along(list),function(i){
   (desig(list[[i]]) - desig(EP)) *strengthList[[i]]
  }
  )
}

# it is commutative
SOFTADD <- function(list){
tot <- list[1]
if(length(list)>0){
list <- list[-1]
for(i in list){
  if(i>0)   tot <- tot + (1-tot)*i
  else  tot <- tot + (tot)*i
    }
}
tot

  }
# not right with negative increments


desig <- function(num){
  (((num - .5)*.9999999999999)+.5) %>% sigmoid(inverse=T)
}

desig2 <- function(num){
  (((num - .5)*.9999999999999)+.5) %>% sigmoid(inverse=T)
}

# when something is at 1 you can't shift it
SOFTADD3 = function(EP,list){
  sigmoid(EP,inverse=T) %>% sum(unlist(list),na.rm=T) %>% sigmoid
}
SOFTADD2 = function(list){
  sum(unlist(list),na.rm=T) %>% sigmoid
}
SOFTADD4 = function(list){
  list %>% map(desig) %>% unlist %>% sum %>% sigmoid
}

SOFTADD5 = function(list){
  list %>% unlist %>% sum %>% sigmoid
}




hitme=function(list,strengthList,Ep){
  tot <- Ep
  for ( i in seq_along(list)){
      x = list[[i]]
      s = strengthList[[i]]
      
      if(x>tot){
        tot <- tot + (1-tot)*(x)*s
      } else {
        tot <- tot + (0-tot)*(tot-x)*s
        
      }
  }
  tot
}


sad <- function(x,y){
  x + (1-x)*y
}

sad2 <- function(x,l){
  for(i in seq_along(l)){
    y <- l[[i]]
  x <- x + (1-x)*y
  }
  x
}

sad3 <- function(l){
  x <- l[[1]]
  l <- l[-1]
  if(length(l)>0){
  for(i in seq_along(l)){
    y <- l[[i]]
  if(y>x) x <- x + (1-x)*y else stop()
  }
    }
  x
}

sadx <- function(l){
  # browser()
  x <- l[[1]]
  l <- l[-1]
  if(length(l)>0){
    pos <- l[l>+x]
    neg <- l[l<x]
    if(length(pos)>0){
      postot <- x
      for(i in seq_along(pos)){
        postot <- postot + ((1-postot)*pos[[i]])^2
      }
        cat(postot)
        cat("\n")
    } else postot <- 0 
    if(length(neg)>0){
      negtot <- 1-x                           # we are going to invert everything
      for(i in seq_along(neg)){
        negtot <- negtot + ((1-negtot)*(1-neg[[i]]))^2
      }
      cat(negtot)
        cat("\n")
    }else negtot <- 0
    
    
    }
  x + ((1-x)*(postot)) - (x*(negtot))
}


sadx <- function(l){
  
}



# sad4 <- function(ep,l){
#   central_ten <- mean(l,na.rm=T)
#   total <- mean(l-ep) %>% 
#     sum(ep) %>%
#     min(1) %>% 
#     max(-1) %>% 
#     unclass
#  # browser()
#   cat(paste0(central_ten," / ",total))
#   sum(central_ten,(total),na.rm=T)/2
# }


# given a list of values resulting from some other functions, plus ep, work out some combination.
# sad6 <- function(ep,l,strengths=rep(1,length(l))){
#   l <- (l[[1]])
#   strengths <- unlist(strengths)
#   diffs <- (l-ep)*strengths 
#   negs <- diffs[diffs<0] %>% sum
#   poss <- diffs[diffs>=0] %>% sum
#   (poss+negs+ep) %>% 
#     min(1) %>% 
#     max(0)
#   }# sucks but works


# 
# weighted.mean.2=function(x,w) {
#   m=weighted.mean(x,w)
#   if(is.na(m)) m <- NULL
#   m
# }

# combine_functions <- function(ep,l,strengths=rep(1,length(l))){
#   # browser()
#   l <- (l[[1]])
#   strengths <- unlist(strengths)
#   diffs <- (l-ep)*strengths 
#   negs <- diffs[diffs<0] %>% sum %>% max(-1)
#   poss <- diffs[diffs>=0] %>% sum %>% min(1)
#   both <- c(sum(poss+negs)+ep,weighted.mean(l,strengths)) %>% na.omit
#   mean(both) %>%
#     min(1) %>% 
#     max(0)
# }# sucks but works too


#   <- function(funname,ep,package,strengths){
#   combine_functions(ep,NECC(ep,package),strengths)
# }

# get_function_values_from_list <- function(ep,lis){ # takes ep and a list of lists of funnames and values and strengths, and calculates Epost for each 
#     # browser()
#   vals <- lapply(lis,function(i){
#     do.call(i$funname,list(ep,i$values))
#   }
#   )
#   sad6(ep,vals,lapply(lis,function(x)x$strengths))
#   # combine_functions(ep,vals,lapply(lis,function(x)x$strengths))
# }



funfun2=function(fun,Ep=scens,B=scens,s=scens){
  gri=expand.grid(fun=fun,Ep=Ep,B=B,strength=s)  
    # browser()
  E <- lapply(1:nrow(gri),function(x){
    row=gri[x,]
  get_function_values_from_list(row$Ep,list(list(funname=row$fun %>% as.character,values=(row$B),strengths=row$strength)))
  })
  
  gri$E=E %>% unclass
  gri
}


funfun3=function(fun,Ep=scens,B=scens,s=scens){
  gri=expand.grid(fun=fun,Ep=Ep,B=B,strength=s,fun2="NECC",B2=scens,strength2=1)  
  # browser()
  E <- lapply(1:nrow(gri),function(x){
    row=gri[x,]
    get_function_values_from_list(row$Ep,list(list(funname=row$fun %>% as.character,values=(row$B),strengths=row$strength),list(funname=row$fun2 %>% as.character,values=(row$B2),strengths=row$strength2)))
  })
  
  gri$E=E %>% unclass
  gri
}


scens=(0:4)/4

# gfun1=function(funname){
#   funfun2(funname,Ep = scens,B = scens,s = scens) %>% 
#     filter(strength==1) %>% 
#     mutate(fun=as.character(fun)) %>% 
#     mutate(E=unlist(E)) %>% 
#     ggplot(aes(B,E,group=Ep,colour=Ep))+
#     # geom_point()+
#     geom_jitter(size=3,height = .005,width = .005)+
#     # geom_line(size=1)+
#     xlab("Level of influence variable")+
#     ylab("Posterior value of consequence variable")+
#     ggtitle(funname)+
#     guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
# }
# 
# 
# gfun2=function(funname){
#   funfun2(funname,Ep=scens,B=scens,s=scens) %>%
#     ggplot(aes(B,unlist(E),group=Ep,colour=Ep))+
#     # geom_point()+
#     geom_jitter(size=3,height = .005,width = .005)+
#     geom_line(size=1)+
#     xlab("Level of influence variable")+
#     ylab("Posterior value of consequence variable")+
#     facet_grid(.~strength)+
#     ggtitle(funname,sub="                             strength of influence")+
#     guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
# }

gfun3=function(funname,funname2="NECC"){
  funfun3(funname,Ep=scens,B=scens,s=scens) %>%
    ggplot(aes(strength,unlist(E),group=Ep,colour=Ep))+
    # geom_point()+
    geom_jitter(size=3,height = .005,width = .005)+
    geom_line(size=1)+
    xlab("Level of influence variable")+
    ylab("Posterior value of consequence variable")+
    facet_grid(B2~B)+
    ggtitle(funname,sub="                             strength of influence")+
    guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
}

# scens=(0:4)/4

# df=tibble(strength=0:1,B=c(1,1),funname=c("NECC", "SUFF"))


# get_values_from_funlistOLD <- function(ep,df){
#   # browser()
# df %>% 
#   rowwise %>% 
#     mutate(E = do.call(funname,list(ep,B))) %>%
#   ungroup %>% 
#     mutate(positive = (funname %in% xc("PLUS SUFF"))) %>% 
#     mutate(diff = (E-ep)*strength) %>% 
#     mutate(drag = ifelse(positive,ep+diff,ep-diff)*strength) %>%
#     pull(diff) %>% 
#     # pull(drag) %>% 
#     # sum(na.rm=T) %>% 
#     mean(na.rm=T) %>% 
#     sum(ep) %>% 
#     min(1) %>% 
#     max(0)
# }

wmean=function(x,w,residual=.5){
  residual_strength=1-sum(abs(w),na.rm=T)
  # if(residual_strength>=0){
  #   x=c(residual,x)
  #   w=c(residual,w)
  # }
  weighted.mean(x,w,na.rm=T)
}



get_values_from_funlist <- function(ep,df){
  # browser()
df %>% 
  rowwise %>% 
    mutate(E = do.call(funname,list(ep,B))) %>%
    mutate(diff = (E-ep)*strength) %>%
  ungroup %>% 
    pull(diff) %>% 
    sum(ep) %>%
    # wmean(df$strength,ep) %>% 
    min(1) %>% 
    max(0)
}


scens=(0:4)/4

funfun4=function(fun,funname2,ep=scens,B=scens,strength=scens){
  # browser()
  gri=expand.grid(ep=ep,fun=fun,B=B,strength=scens,fun2=funname2,B2=scens,strength2=1,stringsAsFactors=F)  
  # gri %>% 
  #   rowwise() %>% 
  #   mutate(E=do.call(fun,list(B=B,)))
  
  E <- lapply(1:nrow(gri),function(x){
    row=gri[x,]
    get_values_from_funlist(row$ep,tibble(B=c(row$B,row$B2),funname=c(row$fun,row$fun2),strength=c(row$strength,row$strength2)))
  })
  
  gri$E=E %>% unlist
  gri
}

funfun4a=function(fun,ep=scens,B=scens,strength=scens){
  # browser()
  gri=expand.grid(ep=ep,fun=fun,B=B,strength=strength,stringsAsFactors=F)  
  
  E <- lapply(1:nrow(gri),function(x){
    row=gri[x,]
    get_values_from_funlist(row$ep,tibble(B=c(row$B),funname=c(row$fun),strength=c(row$strength)))
  })
  
  gri$E=E %>% unlist
  gri
}

gfun1x=function(funname){
  funfun4a(funname) %>% 
    ggplot(aes(strength,E,colour=ep,group=ep))+
    geom_point()+
    facet_grid(cols=vars(B))+
    geom_line(size=1)+
    geom_jitter(size=3,height = .005,width = .005)+
    xlab("Value (facets) / strength (axis) of influence variable")+
    ylab("Posterior value of consequence variable")+
    ggtitle(funname)+
    theme(axis.text.x=element_text(size=8))+
    guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
}

gfun3=function(funname,funname2){
  # browser()
  # browser()
  funfun4(funname,funname2) %>%
    ggplot(aes(strength,E,group=ep,colour=ep))+
    geom_jitter(size=3,height = .005,width = .005)+
    geom_line(size=1)+
    xlab("Value (facets) / strength (axis) of influence variable 1")+
    ylab("Posterior value of consequence variable")+
    facet_grid(rows=vars(B2),cols=vars(B),as.table = F)+
    theme(axis.text.x=element_text(size=8))+
    ggtitle(paste0("Columns = influence variable 1, function: ",funname,"                      Rows: influence variable 2, function: ",funname2),sub=" Rows show values of variable 2, strength of variable 2 is fixed at 1")+
    guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
}

