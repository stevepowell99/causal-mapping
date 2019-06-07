# main file for combo functions





scens=(0:4)/4


funfun=function(fun,Ep,B,s){
  expand.grid(Ep=Ep,B=B,strength=s) %>%
    mutate(E =  UQ(sym(fun))(Ep,B,strength))
}



gfun2=function(funname){
  funfun(funname,scens,scens,scens) %>%
    ggplot(aes(B,E,colour=strength,group=strength))+
    geom_jitter(height = .01,width = .01)+
    geom_line(size=1)+
    facet_grid(.~Ep)+
    xlab("Level of influence variable")+
    ylab("Posterior value of consequence variable")+
    ggtitle(funname,sub="                           Prior value of consequence variable")
}

SAMEgravity = function(Ep,B,s=1){
  s*B
}


SA = function(Ep,B,s=1){
  Ep+(1-Ep)*s*B
}


NOT = function(Ep,B,s=1){
  Ep+(1-Ep)*s*(1-B)
}


SQ = function(Ep,B,s=1){
  Ep+(1-Ep)*s*B^2
}



NECC = function(Ep,B,s=1){
  Ep+(1-Ep)*s*B
}

MIN = function(Ep,B,s=1){
  pmin(Ep,B*s)
}

MIN2 = function(Ep,B,s=1){
  pmin(Ep,B)*s
}

MAX = function(Ep,B,s=1){
  pmax(Ep,B*s)
}



AND = function(Ep,V,s=1){
  res = pmin(V[[1]],V[[2]])
  Ep+(1-Ep)*s*res
}

