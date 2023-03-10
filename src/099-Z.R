################################################################################
## Step 99.00: VERSION HISTORY                                               ###
################################################################################
z99.version = "1.0.0"
z99.ModDate = as.Date("2020-07-10")
################################################################################
## Step 99.00 create object table                                            ###
################################################################################
dtObj<-setDT(lsos(), keep.rownames = T)[]
names(dtObj)[1] <- "Name" ### rename data.table column
lsObj<-list(dtObj[Type == 'data.table' & Length_Rows == 0][,1])
################################################################################
## Step 99.01 access hidden attribute in R data frame  https://is.gd/zenrph ###
################################################################################
df  <-  ls()[sapply(ls(), function(x) is.data.frame(get(x)) | is.xts(get(x)))]
l   <-  ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
sapply(l, function(x) names(l))
################################################################################
## Step 99.02 remove unwanted data.frames; e.g. 'SQL' in its name            ###
################################################################################
rm(list = ls()[grepl("(SQL|X2016Tuition)", ls())])
################################################################################
## Step 99.03: PURRRfect TIME                                                ###
################################################################################
dx_blob %>% map_df(~ (data.frame(
  n_distinct = n_distinct(.x),
  class = class(.x)
)),
.id = "variable"
)
# ------------------------------------------------------------------------------
dx_blob[C.P == "C", ] %>% map_df(~ (data.frame(
  n_distinct = n_distinct(.x),
  class = class(.x)
)),
.id = "variable"
)
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
z99.version             <- "1.0.0"
z99.ModDate             <- as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.03.01 - v.1.0.0                               http://tinyurl.com/y54k8gsw
# 1st release                                        http://tinyurl.com/yx9w8vje
# ------------------------------------------------------------------------------
