# Variables --------------------------------------------------------------------
print("LOGGING ROW 012: Start loading variables.")
# User specific RUN_ID for aws session

string <- toString(Sys.getenv())
print("LOGGING ROW 016: {string}")

run_id <- Sys.getenv("RUN_ID")

if (run_id==""){
  path_prefix <- ""
} else {
  print(glue("LOGGING ROW 020 {Sys.time()}-RUND_ID -> {run_id}"))
  path_prefix <- glue("/data/{run_id}/")
}

# AWS S3 project bucket --------------------------------------------------------
S3_Bucket_Name <- "schatsi-nlp-io"

Sys.getenv("AWS_ACCESS_KEY_ID")
Sys.getenv("AWS_SECRET_ACCESS_KEY")
Sys.getenv("AWS_DEFAULT_REGION")
  
# Dev code ---------------------------------------------------------------------
# creation of unique AWS S3 sub-folder for ml output
# put_folder(path, bucket = S3_Bucket_Name)
# x <- get_bucket(S3_Bucket_Name, prefix = inputF)
# put_object(file = charToRaw(toString(dataSet0WC)), object = file, bucket = S3_Bucket_Name)
