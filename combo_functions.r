library(sigmoid)
library(betareg)


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
DIV <- function(EP,B) EP/B

SUFF = function(Ep,B){
 1-(Ep-1)*(B-1)
}


desig2 <- function(num){
  (((num - .5)*.9999999999999)+.5) %>% sigmoid(inverse=T)
}

# when something is at 1 you can't shift it






scens=(0:4)/4


wmean=function(x,w,residual=.5){
  residual_strength=1-sum(abs(w),na.rm=T)

    weighted.mean(x,w,na.rm=T)
}




mean_clever <- function(vec){
  pos <- vec[vec>0] 
  neg <- vec[vec<=0] 
  sum(pos)+sum(neg)
}



get_values_from_funlist <- function(ep, df, type = "hard", summ = "mean") {
  # browser()
  
  remainder <- df$strength %>% 
    sum() %>% 
    max(0) %>% 
    min(1)
  
  df = df %>%
    rowwise %>%
    mutate(E = do.call(funname, list(ep, B))) %>%
    mutate(diff = (E - ep) * strength) %>%
    ungroup() 
  
  if(summ=="smart"){
    df %>% 
      mutate(diff = (E-.5)*strength) %>% 
      pull(diff) %>% 
      sum(ep*(1-remainder))
  }
  else {
    val <- df %>%
    pull(diff)
  
  if (summ == "sum") {
    val <- val %>%
      sum(ep)
  } else
    if (summ == "mean") {
      val <- val %>%
        mean() %>%
        sum(ep)
    } else
      if (summ == "clever") {
        val <- val %>%
          mean_clever() %>%
          sum(ep)
      }
  
  if (type == "hard") {
    val %>%
      max(0) %>%
      min(1)
  } else
    
    if (type == "sigmoid") {
      val %>%
        sigmoid()
    }
  }
  
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
funfun4=function(fun,funname2,ep=scens,B=scens,strength=scens,type="hard",summ="sum"){
  # browser()
  gri=expand.grid(ep=ep,fun=fun,B=B,strength=scens,fun2=funname2,B2=scens,strength2=scens,stringsAsFactors=F)  
  
  E <- lapply(1:nrow(gri),function(x){
    row=gri[x,]
    get_values_from_funlist(row$ep,tibble(B=c(row$B,row$B2),funname=c(row$fun,row$fun2),strength=c(row$strength,row$strength2)),type=type,summ=summ)
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

gfun3=function(funname,funname2,type="hard",summ="mean"){
  funfun4(funname,funname2,type=type,summ=summ) %>%
    ggplot(aes(ep,E,group=interaction(strength,strength2),colour=factor(strength),linetype=factor(strength2)))+
    geom_jitter(size=3,height = .009,width = .009)+
    geom_line(size=1)+
    xlab("Facets: Value of influence variable 1 \nAxis: prior value of consequence variable")+
    ylab("Posterior value of consequence variable")+
    facet_grid(rows=vars(B2),cols=vars(B),as.table = F)+
    scale_linetype_manual(values=c("dotted","dashed","solid"))+
    scale_colour_manual(values=c("lightskyblue2","skyblue3","deepskyblue4"))+
    theme(axis.text.x=element_text(size=8))+
    ggtitle(paste0("Columns: Influence variable 1, function: ",funname,"\n",
                     "Rows: Influence variable 2, function: ",funname2,"\n",
                      "Plots: Prior vs posterior value of consequence variable")
    ,sub=paste0("Aggregation type = ",type,". Summary function = ",summ))+
    guides(colour=guide_legend(title="Strength of first influence"),reverse=F)+
    guides(linetype=guide_legend(title="Strength of second influence"),reverse=F)
}

# dd <- DiscreteDistribution(supp = c(1:5)/5, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
# 
# NECC(dd,dd) %>% plot(to.draw.arg=c("d"))
