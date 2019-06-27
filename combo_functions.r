# main file for combo functions


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

# catfun=function(x)paste0(x,collapse=",")

# group_if=function(df,cond,vars1,vars2){
# dots = sapply(if(cond)vars1 else vars2, . %>% {as.formula(paste0('~', .))})
# group_by_(df,.dots=dots)
# }
#
#




scens=(0:4)/4


funfun=function(fun,Ep,B,s){
  expand.grid(Ep=Ep,B=B,strength=s) %>%
    mutate(E =  UQ(sym(fun))(Ep,B,strength))
}



gfun2OLD=function(funname){
  funfun(funname,scens,scens,scens) %>%
    ggplot(aes(B,E,colour=strength,group=strength))+
    geom_jitter(height = .01,width = .01)+
    geom_line(size=1)+
    facet_grid(.~Ep)+
    xlab("Level of influence variable")+
    ylab("Posterior value of consequence variable")+
    ggtitle(funname,sub="                           Prior value of consequence variable")
}


gfun1=function(funname){
  funfun(funname,scens,scens,scens) %>%
    filter(strength==1) %>% 
    ggplot(aes(B,E,group=Ep,colour=Ep))+
    # geom_point()+
    geom_jitter(size=3,height = .005,width = .005)+
    # geom_line(size=1)+
    xlab("Level of influence variable")+
    ylab("Posterior value of consequence variable")+
    ggtitle(funname)+
    guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
}


gfun2=function(funname){
  funfun(funname,scens,scens,scens) %>%
    ggplot(aes(B,E,group=Ep,colour=Ep))+
    # geom_point()+
    geom_jitter(size=3,height = .005,width = .005)+
    geom_line(size=1)+
    xlab("Level of influence variable")+
    ylab("Posterior value of consequence variable")+
    facet_grid(.~strength)+
    ggtitle(funname,sub="                             strength of influence")+
    guides(colour=guide_legend(title="Prior value of consequence variable"),reverse=T)
}



NOT = function(Ep,B,s=1){
  (1-B)*s + Ep*(1-s)
}




SQUARE = function(Ep,B,s=1){
  (B^2)*s + Ep*(1-s)
}

SAME = function(Ep,B,s=1){
  B*s + Ep*(1-s)
}

NOT = function(Ep,B,s=1){
  (1-B)*s + Ep*(1-s)
}

NECC = function(Ep,B,s=1){
  ((Ep)*B)*s + Ep*(1-s)
}

SUFF = function(Ep,B,s=1){
  (1-((Ep-1)*(B-1)))*s  + Ep*(1-s)
}



# gfun2("SAME")
# gfun2("NOT")
# gfun2("SQUARE")
# gfun2("NECC")
# gfun2("SUFF")
# 



# MIN = function(Ep,B,s=1){
#   pmin(Ep,B*s)
# }
# 
# MIN2 = function(Ep,B,s=1){
#   pmin(Ep,B)*s
# }
# 
# MAX = function(Ep,B,s=1){
#   pmax(Ep,B*s)
# }
# 
# 
# 
# AND = function(Ep,V,s=1){
#   res = pmin(V[[1]],V[[2]])
#   Ep+(1-Ep)*s*res
# }
# 
