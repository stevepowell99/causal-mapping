
library(tidyverse)
library(RMariaDB)
library(RSQLite)
conremote <-  DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306)
conlocal <-  DBI::dbConnect(RSQLite::SQLite(),"CMA")
conl <-  DBI::dbConnect(RSQLite::SQLite(),"CMA")

db_list_tables(conl)

DBI::dbDisconnect(conl)
DBI::dbDisconnect(conM)


plist=xc("SLP4genderMore SLX5 Nepal3 Malawi2 rick multiple-statements-per-source")

lis=list()
for(u in userlist){
for(p in plist){
  fil <- paste0("www/",u,"/",p,"-settingsConditional.csv")
  
if(file.exists(fil)){
  csv <- read_csv(fil)
csv$project <- p
csv$user <- u
# csv$cluster <- as.numeric(csv$cluster)
# if(is.null(csv$source__id))csv$source__id="1"
# csv$source__id <- as.character(csv$source__id)
# if(is.null(csv$question))csv$question="1"
# csv$question <- as.character(csv$question)
csv <- csv %>% 
  select(user,project,everything())
lis[[fil]] <- csv
  }
  }
}

settingsConditional <- bind_rows(lis) 

copy_to(con, settingsConditional,temporary=F,overwrite=T)
  

lis=list()
for(u in userlist){
for(p in plist){

  fil=paste0(u,p)
  lis[[fil]] <- tibble(user=u,project=p,source_id=1,key="key",value="value")
  
}
}

sources <- bind_rows(lis) 

copy_to(con, sources,temporary=F,overwrite=T)




copy_to(con, sources,temporary=F,append=T)


dbDisconnect(con)


# s <- paste0("CREATE DATABASE CMA ")
conremote <-  DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306)
conlocal <- dbConnect(RSQLite::SQLite(), "CMA")
dbListTables(conl)

DBI::dbExecute(con, as.character('set character set "utf8"'))
s="ALTER DATABASE CMA CHARACTER SET utf8 COLLATE utf8_general_ci;"

dbExecute(conl,"ALTER TABLE sources
  MODIFY source_id varchar(50)")

# Load the mtcars as an R data frame put the row names as a column, and print the header.
data("mtcars")
mtcars$car_names <- rownames(mtcars)
rownames(mtcars) <- c()
head(mtcars)
# Create a connection to our new database, CarsDB.db
# you can check that the .db file has been created on your working directory
conn <-  DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306)

conn=conM

nodes <- dbGetQuery(conlocal,"SELECT * FROM nodes") %>% collect()
copy_to(conremote,nodes,temporary=F,overwrite=T)
edges <- dbGetQuery(conlocal,"SELECT * FROM edges") %>% collect()
copy_to(conremote,edges,temporary=F,overwrite=T)
statements <- dbGetQuery(conlocal,"SELECT * FROM statements") %>% collect()
copy_to(conremote,statements,temporary=F,overwrite=T)
statements_extra <- dbGetQuery(conlocal,"SELECT * FROM statements_extra") %>% collect()
copy_to(conremote,statements_extra,temporary=F,overwrite=T)
settingsGlobal <- dbGetQuery(conlocal,"SELECT * FROM settingsGlobal") %>% collect()
copy_to(conremote,settingsGlobal,temporary=F,overwrite=T)
settingsConditional <- dbGetQuery(conlocal,"SELECT * FROM settingsConditional") %>% collect()
copy_to(conremote,settingsConditional,temporary=F,overwrite=T)









dbWriteTable(conn, "cars_data", mtcars)



con <- dbConnect(RSQLite::SQLite(), "CMA")
db_list_tables(con)



dbGetQuery(conl, "SELECT * FROM statements")






# import foreign proj -----------------------------------------------------

temps=list()
for(c in csvlist){
  temps[[c]]=read_csv(glue("import/{c}.csv"))
  send_to_sql(temps,con,"BSDR","sace",c)
  
}


