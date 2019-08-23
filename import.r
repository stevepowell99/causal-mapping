
library(tidyverse)
library(RMariaDB)
con <-  DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306)


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
con <-  DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306)

DBI::dbExecute(con, as.character('set character set "utf8"'))
s="ALTER DATABASE CMA CHARACTER SET utf8 COLLATE utf8_general_ci;"



s="SHOW TABLES;"
rs <- 
  dbSendQuery(con, s)
df <-  fetch(rs, n = -1)



tbl(con, 'nodes') %>% 
  select(cyl,mpg)


iris[1,6]="Jel čuješ samo što hoćeš"
iris4=iris[1:3,]
copy_to(con, iris4)



dbSendQuery(con,"INSERT INTO iris4 (V6)
    VALUES ('čuješ✅')")

tbl(con, 'iris4') %>% 
  filter(is.na(Sepal.Width))


rs <- dbSendStatement(con,"INSERT INTO nodes (user, project, label) VALUES ('Steve', 'SLX5', 'xxyyzzaa')")
