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
fun_butterfly <- function(dx_blob){
    
#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
# create a stub template dataframe (initial 1 time setup)
# https://tinyurl.com/wda2yyuh
# ------------------------------------------------------------------------------
if (z == TRUE) {
  g[[paste0("dt_bfly")]] <<- dx_blob[.0, 0]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, id                   := .I]
  g[[paste0("dt_bfly")]][, TKR                  := dx_blob[, "TKR"]]
  g[[paste0("dt_bfly")]][, CMRK                 := dx_blob[, "CMRK"]]
  g[[paste0("dt_bfly")]][, TechRank             := dx_blob[, "TechRank"]]
  g[[paste0("dt_bfly")]][, X.vCM                := dx_blob[, "X.vCM"]]
  g[[paste0("dt_bfly")]][, HISTVO               := dx_blob[, "HISTVO"]] 
  g[[paste0("dt_bfly")]][, CMPRICE              := dx_blob[, "CMPRICE"]]
  g[[paste0("dt_bfly")]][, VOLF                 := dx_blob[, "VOLF"]]
  g[[paste0("dt_bfly")]][, Record.Number        := dx_blob[, "Record.Number"]]
  g[[paste0("dt_bfly")]][, EXPDAY               := dx_blob[, "EXPDAY"]]
  g[[paste0("dt_bfly")]][, OI                   := dx_blob[, "OI"]]
  g[[paste0("dt_bfly")]][, VOL                  := dx_blob[, "VOL"]]
  g[[paste0("dt_bfly")]][, BID                  := dx_blob[, "BID"]]
  g[[paste0("dt_bfly")]][, ASK                  := dx_blob[, "ASK"]]
  g[[paste0("dt_bfly")]][, EST                  := dx_blob[, "EST"]]
  g[[paste0("dt_bfly")]][, DLTA                 := dx_blob[, "DLTA"]]
  g[[paste0("dt_bfly")]][, Bid.oprk             := dx_blob[, "Bid.oprk"]]
  g[[paste0("dt_bfly")]][, Ask.oprk             := dx_blob[, "Ask.oprk"]]
  g[[paste0("dt_bfly")]][, Bid.UN.OV            := dx_blob[, "Bid.UN.OV"]]
  g[[paste0("dt_bfly")]][, Ask.UN.OV            := dx_blob[, "Ask.UN.OV"]]
  g[[paste0("dt_bfly")]][, RVOPT                := dx_blob[, "RVOPT"]]
  g[[paste0("dt_bfly")]][, X10.                 := dx_blob[, "X10."]]
  g[[paste0("dt_bfly")]][, X.10.                := dx_blob[, "X.10."]]
  g[[paste0("dt_bfly")]][, I.OTM                := dx_blob[, "I.OTM"]]
  g[[paste0("dt_bfly")]][, THETA                := dx_blob[, "THETA"]]
  g[[paste0("dt_bfly")]][, Bid.Implied          := dx_blob[, "Bid.Implied"]]
  g[[paste0("dt_bfly")]][, Ask.Implied          := dx_blob[, "Ask.Implied"]]
  g[[paste0("dt_bfly")]][, VOLFADJ              := dx_blob[, "VOLFADJ"]]
  g[[paste0("dt_bfly")]][, PrtbStrk.VOLF        := dx_blob[, "PrtbStrk.VOLF"]]
  g[[paste0("dt_bfly")]][, Bid.PrtbStrk.Implied := dx_blob[, "Bid.PrtbStrk.Implied"]]
  g[[paste0("dt_bfly")]][, Ask.PrtbStrk.Implied := dx_blob[, "Ask.PrtbStrk.Implied"]]
  g[[paste0("dt_bfly")]][, PctDble              := dx_blob[, "PctDble"]]
  g[[paste0("dt_bfly")]][, ROBUY                := dx_blob[, "ROBUY"]]
  g[[paste0("dt_bfly")]][, ROWRT                := dx_blob[, "ROWRT"]]
  g[[paste0("dt_bfly")]][, ROCCMP               := dx_blob[, "ROCCMP"]]
  g[[paste0("dt_bfly")]][, GAMMA                := dx_blob[, "GAMMA"]]
  g[[paste0("dt_bfly")]][, VEGA                 := dx_blob[, "VEGA"]]
  g[[paste0("dt_bfly")]][, RHO                  := dx_blob[, "RHO"]]
  g[[paste0("dt_bfly")]][, TimPrPct             := dx_blob[, "TimPrPct"]]
  g[[paste0("dt_bfly")]][, `BID/ASKPcnt`        := dx_blob[, "'BID/ASKPcnt'"]]
  g[[paste0("dt_bfly")]][, date_run             := dx_blob[, "date_run"]]
# -----------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, id_minus_0 := dx_blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]] # OPTKR
  g[[paste0("dt_bfly")]][, id_minus_1 := dx_blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
  g[[paste0("dt_bfly")]][, id_plus_0  := dx_blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
  g[[paste0("dt_bfly")]][, id_plus_1  := dx_blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]  
} else {
  dx_blob[TKR == "A" & C.P == "P", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13] # OPTKR
  dx_blob[TKR == "A" & C.P == "C", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]

  dx_blob[TKR == "A" & C.P == "P", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]
  dx_blob[TKR == "A" & C.P == "C", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]
  # ------------------------------------------------------------------------------
 -dx_blob[TKR == "A" & C.P == "P", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
  dx_blob[TKR == "A" & C.P == "C", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
  dx_blob[TKR == "A" & C.P == "P", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
  dx_blob[TKR == "A" & C.P == "C", ][dx_date_exp[day == 6 & week == 3, ][1, 1]][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]
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
