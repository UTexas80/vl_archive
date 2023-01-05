dx_blob$EXPDAY       <- as.Date(dx_blob$EXPDAY, format('%m/%d/%Y'))
# ------------------------------------------------------------------------------
setkey(dx_blob,      EXPDAY)
setkey(dx_date_exp,  EXPDAY)
# ------------------------------------------------------------------------------
dx_blob[dx_date_exp[ day == 6 & week == 3, ][1,1]][,c(10,12)]
dx_tkr_stk           <- dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1]][,c(10,12)]
# ------------------------------------------------------------------------------
setkey(dx_tkr_stk,   TKR)
setkey(dx_ticker,    TKR)
# ------------------------------------------------------------------------------
s_minus_0            <- unique(dx_ticker[dx_tkr_stk])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
s_minus_1            <- unique(dx_ticker[dx_tkr_stk])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
s_plus_0             <- unique(dx_ticker[dx_tkr_stk])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)]
s_plus_1             <- unique(dx_ticker[dx_tkr_stk])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)]
# ------------------------------------------------------------------------------
dx_s_minus_0         <- unique(dx_ticker[dx_tkr_stk])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
dx_s_minus_1         <- unique(dx_ticker[dx_tkr_stk])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
dx_s_minus_2         <- unique(dx_ticker[dx_tkr_stk])[CMPRICE >= STRIKE, STRIKE[.N - 2], by = .(TKR)]
dx_s_plus_0          <- unique(dx_ticker[dx_tkr_stk])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)][,c(1,3)]
dx_s_plus_1          <- unique(dx_ticker[dx_tkr_stk])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)][,c(1,3)]
dx_s_plus_2          <- unique(dx_ticker[dx_tkr_stk])[CMPRICE <= STRIKE, .SD[3],         by = .(TKR)][,c(1,3)]
# ------------------------------------------------------------------------------
names(dx_s_minus_0)  [1:2]  <- c("TKR", "STRIKE")
names(dx_s_minus_1)  [1:2]  <- c("TKR", "STRIKE")
names(dx_s_minus_2)  [1:2]  <- c("TKR", "STRIKE")
names(dx_s_plus_0)   [1:2]  <- c("TKR", "STRIKE")
names(dx_s_plus_1)   [1:2]  <- c("TKR", "STRIKE")
names(dx_s_plus_2)   [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
setkey(dx_s_minus_0, TKR)
setkey(dx_s_minus_1, TKR)
setkey(dx_s_minus_2, TKR)
# ------------------------------------------------------------------------------
setkey(dx_s_plus_0,  TKR)
setkey(dx_s_plus_1,  TKR)
setkey(dx_s_plus_2,  TKR)
# ------------------------------------------------------------------------------
setkey(dx_ticker,    TKR)
# ------------------------------------------------------------------------------
fun_butterfly <- function(blob){
    
#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# create a stub template dataframe (initial 1 time setup)
# https://tinyurl.com/wda2yyuh
# ------------------------------------------------------------------------------
if (z == TRUE) {
#  g[[paste0("dt_bfly")]] <<- dx_blob[.0, 0]
  g[[paste0("dt_bfly")]]  <<- blob[.0,0]               # stub template dataframe 
#  g[[paste0("dt_bfly")]]  <<- blob[.0]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, TKR        := unique(blob[, "TKR"])]
  g[[paste0("dt_bfly")]][, CMRK       := unique(blob[, "CMRK"])]
  g[[paste0("dt_bfly")]][, TechRank   := unique(blob[, "TechRank"])]
  g[[paste0("dt_bfly")]][, X.vCM      := unique(blob[, "X.vCM"])]
  g[[paste0("dt_bfly")]][, CMPRICE    := unique(blob[, "CMPRICE"])]  
  g[[paste0("dt_bfly")]][, HISTVO     := unique(blob[, "HISTVO"])] 
  g[[paste0("dt_bfly")]][, VOLF       := unique(blob[, "VOLF"])]
  g[[paste0("dt_bfly")]][, vol_diff   := ((dt_bfly[,7] - dt_bfly[,6])/dt_bfly[,6])*100] 
  g[[paste0("dt_bfly")]][, EXPDAY     := unique(blob[, "EXPDAY"])]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, id_minus_1 := blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]] # OPTKR
  g[[paste0("dt_bfly")]][, id_minus_0 := blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]  
  g[[paste0("dt_bfly")]][, id_plus_0  := blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
  g[[paste0("dt_bfly")]][, id_plus_1  := blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, cost       := 
   -blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
    blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
    blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
    blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, s_minus_1  := unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
  g[[paste0("dt_bfly")]][, s_minus_0  := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  g[[paste0("dt_bfly")]][, s_plus_0   := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  g[[paste0("dt_bfly")]][, s_plus_1   := unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_iotm  := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
  g[[paste0("dt_bfly")]][, s_plus_0_iotm   := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
# ------------------------------------------------------------------------------
# CMPRICE diff from nearest +/- strike
# ------------------------------------------------------------------------------  
# g[[paste0("dt_bfly")]][, id_minus_0_stk
#                        := unique(dt_bfly[,"dx_s_minus_0"][blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"]]
# g[[paste0("dt_bfly")]][, id_plus_0_stk
#                        := unique(dt_bfly[,"dx_s_plus_0"] [blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"]]
# ------------------------------------------------------------------------------    
  g[[paste0("dt_bfly")]][, id_minus_0_stk_diff
                         := ((CMPRICE -
                                unique(dt_bfly[,"id_minus_0"][blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"])
                             / CMPRICE)*100]
  g[[paste0("dt_bfly")]][, id_plus_0_stk_diff
                         := ((CMPRICE - 
                                unique(dt_bfly[,"id_plus_0"] [blob, on = .(id_plus_0 = OPTKR), nomatch = 0])[,"STRIKE"])
                             / CMPRICE)*100]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, date_run   := unique(blob[, "date_run"])]
# g[[paste0("dt_bfly")]][, id         := .I]
# ------------------------------------------------------------------------------
  z <<- FALSE
# ------------------------------------------------------------------------------
} else {
#  browser()
  g[[paste0("dt_bfly")]] <<- rbind(g[[paste0("dt_bfly")]],
  data.table(
# ------------------------------------------------------------------------------
  unique(blob[, "TKR"]),
  unique(blob[, "CMRK"]),
  unique(blob[, "TechRank"]),
  unique(blob[, "X.vCM"]),
  unique(blob[, "CMPRICE"]),
  unique(blob[, "HISTVO"]), 
  unique(blob[, "VOLF"]),
  ((dt_bfly[,7] - dt_bfly[,6])/dt_bfly[,6])*100,
  unique(blob[, "EXPDAY"]),
# ------------------------------------------------------------------------------
  unique(blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]), # OPTKR
  unique(blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  # ------------------------------------------------------------------------------
sum(
 -unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]), # ASK
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]), # BID
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
 -unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
  na.rm = TRUE
  ),
# ------------------------------------------------------------------------------
  unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
  unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
#...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
# ------------------------------------------------------------------------------
# CMPRICE diff from nearest +/- strike
# ------------------------------------------------------------------------------
# as.double(((unique(blob$CMPRICE) - unique(dt_bfly[.N,"id_minus_0"][blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"])/unique(blob$CMPRICE))*100),
# as.double(((unique(blob$CMPRICE) - unique(dt_bfly[.N,"id_plus_0"][blob,  on = .(id_plus_0  = OPTKR), nomatch = 0])[,"STRIKE"])/unique(blob$CMPRICE))*100),
# ------------------------------------------------------------------------------
(sum(
 unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 8]),
 -unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
  na.rm = TRUE))/unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
(sum(
 unique(blob[C.P == "C", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 8]),
 -unique(blob[C.P == "C", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
  na.rm = TRUE))/unique(blob[C.P == "C", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
# ------------------------------------------------------------------------------
  unique(blob[, "date_run"])
#  dt_bfly[, .I]
  ),
# ------------------------------------------------------------------------------
  use.names = FALSE)
browser()
  }
}
# ------------------------------------------------------------------------------
butterfly <- dx_blob[
  dx_date_exp[day == 6 & week == 3, ][1, 1]
][
  order(OPTKR, -date_run),
] %>%
  split(., by = c("TKR")) %>%
  map(., fun_butterfly)
# ------------------------------------------------------------------------------
# mapply(fun_butterfly, dx_blob)

# fun_butterfly <- function(nm){}


# dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1]][,c(10,12)]
# dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1], ]
# dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1], .SD, .SDcols = c(10, 12)]
# dx_blob[, .GRP, TKR]

# zzz<-dcast.data.table(dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1]][,c(10,12)], TKR ~ STRIKE)

# dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1]][,c(10,12)][dx_ticker, on = c(STRIKE="CMPRICE")]
