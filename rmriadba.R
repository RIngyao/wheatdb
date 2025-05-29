install.packages("RMariaDB")
install.packages("DBI")

library(DBI)
library(RMariaDB)


# RMariaDB::MariaDB()
con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "wheatdb",
  user = "user1",
  password = "user1@2024"
  
)

query <- "SELECT * FROM vcf_table WHERE IMPACT = 'MODIFIER'"
output <- dbSendQuery(con, sqlInterpolate(con, query))
dbFetch(output)



# # user 
# chr <- input$chromosome
# gene <- input$geneID
# start <- input$start
# end <- input$end
# 
# install.packages("pool")
# 
# library(pool)
# # DB and RMariaDB
# # get user and password
# # userName <- Sys.getenv("DB_USER")
# # userName
# 
# # Sys.getenv("DB_PASS")
# 
# # config for connection
# # con <- DB::dbconnect(
# #   username=user1
# # )
# 
# con <- dbpool(
#   
#   dbname="wheatdb",
#   user = "sksk",
#   password =""
# )
# 
# if(input == "gene" ){
#  js<- dbgetquery(con, sqlInterpolate(con, "select * from vcf_table where Gene_ID = ?", Gene_ID = gene))
# }else if(input == "chr"){
#   js <- dbGetQuery(
#     con,
#     sqlInterpolate(con,
#       "SELECT * FROM vcf_table WHERE CHROM = ?CHROM AND start = ?start AND end = ?end",
#       CHROM = chr,
#       start = start,
#       end = end
#     )
#   )
#   
# }
# 

