# Conector Spotfire - Acesso a dados na AWS
# Rodrigo Eiras
# rodrigo.eiras@iesbrazil.com.br


library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "sa-east-1")
blist <- bucketlist()
b <- 'iesbrazil'
objects <- get_bucket(b)
df <- get_bucket_df(b)

arquivo.csv <- data.frame(s3read_using(read.csv, object="databases/PerfectFormat.csv", bucket=b))
