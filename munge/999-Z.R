# ------------------------------------------------------------------------------
# 999. finish processing                                                    ---
# ------------------------------------------------------------------------------
################################################################################
## Step 99.00: VERSION HISTORY                                               ###
################################################################################
z999.version = "1.0.0"
z999.ModDate = as.Date("2023-05-09")
################################################################################
## Step 99.00 create object table                                            ###
################################################################################
dtObj           <- setDT(lsos(), keep.rownames = T)[]
names(dtObj)[1] <- "Name" ### rename data.table column
lsObj           <- list(dtObj[Type == 'data.table' & Length_Rows == 0][,1])
ls_dt           <- list(dtObj[Type == 'data.table'][,1])
# ------------------------------------------------------------------------------
dt_dt           <- data.table::tables()
# ------------------------------------------------------------------------------
################################################################################
## Step 99.01 access hidden attribute in R data frame  https://is.gd/zenrph ###
################################################################################
df  <-  ls()[sapply(ls(), function(x) is.data.frame(get(x)) | is.xts(get(x)))]
l   <-  ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
sapply(l, function(x) names(l))
################################################################################
# create a data.table with names of the data.tables chat GPT
################################################################################
dt_names           <- dtObj[Type == 'data.table'][,1]
names(dt_names)[1] <- "table_name"
################################################################################
# markdown tables
################################################################################
# loop through each row of the data.table and extract the column names
markdown_tables <- lapply(dt_names$table_name, function(name) {
  dt <- get(name)  # get the data.table by name
  table_md <- knitr::kable(colnames(dt), format = "markdown")  # create a markdown table of the column names
  # prepend the table name to the markdown table
  paste(paste0("**Table '", name, "' column names:**"), table_md, sep = "\n")
})
# concatenate the markdown tables into a single markdown file
md_file <- paste(markdown_tables, collapse = "\n\n")
# write the markdown file to disk
writeLines(md_file, "table_column_names.md")
################################################################################
# html tables
################################################################################
html_tables <- lapply(dt_names$table_name, function(name) {
  dt <- get(name)  # get the data.table by name
  table_md <- knitr::kable(colnames(dt), format = "html")  # create a markdown table of the column names
  # prepend the table name to the markdown table
  paste(paste0("**Table '", name, "' column names:**"), table_md, sep = "\n")
})
html_file <- paste(html_tables, collapse = "\n\n")
writeLines(html_file, "table_column_names.html")
################################################################################
## Step 99.02 remove unwanted data.frames; e.g. 'SQL' in its name            ###
################################################################################
# rm(list = ls()[grepl("(SQL|X2016Tuition)", ls())])
################################################################################
## Step 99.98: PROCESSING TIME                                               ###
################################################################################
finish.time = Sys.time()
time = finish.time - start.time
print(finish.time - start.time)
cat("Finished in",timetaken(started.at),"\n")
################################################################################
## Step 99.99: VERSION HISTORY                                               ###
################################################################################
z999.version             <- "1.0.0"
z999.ModDate             <- as.Date("2023-05-05")
# ------------------------------------------------------------------------------
# 2023.05.05 - v.1.0.0                               http://tinyurl.com/y54k8gsw
# 1st release                                        http://tinyurl.com/yx9w8vje
# ------------------------------------------------------------------------------

#...............................................................................
