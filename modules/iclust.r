observe({ 

    

vge <- readRDS("kill") %>% 
  edges_as_tibble()


vgeeft <- vge %>% select(from,to)

all <- vgeeft %>% unlist %>% max
# grid <- expand.grid(1:all,1:all
# grid <- expand.grid(1:all,1:all)

# is_row_equal <- function(row,matrix){
#   (matrix ==row) %>% rowSums %>% `==`(2) %>% any
# }
# 
# find_slice_in_grid <- function(slice,grid){
# lapply(1:nrow(grid),function(r){
#   is_row_equal(unlist(grid[r,]),slice)
# }) %>% unlist
# }


put_in_node_list <- function(vec,all){
  c((1:all %in% unlist(vec[,1])),(1:all %in% unlist(vec[,2]))) %>% as.numeric  # this only looks at nodes involved, should really be an association matrix not a vector. 
}                           # alsdoesn't take strength into account TODO


create_similar_statements <- function(ved){
  
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
 
