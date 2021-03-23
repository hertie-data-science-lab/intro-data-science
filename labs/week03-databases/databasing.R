# on the web
# https://cjad.nottingham.ac.uk

# in the shell

# SELECT * FROM dataentry_keyword LIMIT(10);

## Join

# SELECT d.title, d.date, d.pdfversion, c.name FROM 
#   dataentry_document AS d LEFT JOIN dataentry_country AS c 
# ON country_id = c.id LIMIT (10);

## old skool
## https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html

library(DBI)

# "hang on tightly"
con <- dbConnect(RSQLite::SQLite(), dbname = "data/db.sqlite3")

dbListTables(con)
# dbWriteTable(con, "mtcars", mtcars)
# dbListTables(con)

dbListFields(con, "dataentry_keyword")
# dbReadTable(con, "mtcars")

res <- dbSendQuery(con, "SELECT * FROM dataentry_keyword")
dbFetch(res, n = 5)
dbClearResult(res) # empty out res

# while(!dbHasCompleted(res)){
#  chunk <- dbFetch(res, n = 5)
#  print(nrow(chunk))
#}

# "let go lightly"
dbDisconnect(con) 

# object relational bridge
library(dplyr)
con <- dbConnect(RSQLite::SQLite(), dbname = "data/db.sqlite3")
db_document <- tbl(con, "dataentry_document")
db_country <- tbl(con, "dataentry_country")
db_document 

res <- left_join(db_document, db_country, by = c("country_id" = "id")) %>%
  select(title, date, pdfversion, name)
res
nrow(res)

show_query(res) # yikes

res %>% 
  group_by(name) %>% 
  summarize(docs = n())

collect(res) # finally materialized

