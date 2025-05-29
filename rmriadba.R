install.packages("RMariaDB")
install.packages("DBI")
install.packages("pool")

library(DBI)
library(RMariaDB)


# RMariaDB::MariaDB()
con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "wheatdb",
  user = "user1",
  password = "user1@2024"

)

dbListTables(con)
dbListFields(con, "snp_table_final")

# dbReadTable(con, "vcf_table")

# dbGetQuery(con, sqlInterpolate(con, "SELECT CHROM, POS FROM vcf_table WHERE CHROM = 'Chr1A' AND POS >= 766767 AND POS <= 988788"))
query <- "SELECT *
           FROM vcf_snpeffect
          WHERE CHROM = 'Chr1A'
          AND START >= 173189 AND END <= 271429"
goutput <- dbGetQuery(con, sqlInterpolate(con, query))
print(output)

 dbDisconnect(con)



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

