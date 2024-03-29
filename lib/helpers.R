helper.function <- function()
{
  return(1)
}

################################################################################
# Check the directory
################################################################################
dirCheck       <- function(mainDir, subDir) {
    if (!dir.exists(file.path(mainDir, subDir))) {
        dir.create(file.path(mainDir, subDir))
    }
}
################################################################################
# Calculate the payoff for each scenario
################################################################################
fun_payoff     <<- function(){
# ------------------------------------------------------------------------------
# Define the parameters
# ------------------------------------------------------------------------------  
  current_stock_price <- 117.9
  upper_call_strike <- 125
  lower_call_strike <- 115
  lower_put_strike <- 110
  upper_put_strike <- 120
  net_credit <- 7.35
  num_simulations <- 10000
  implied_volatility <- 0.316
  risk_free_rate <- 0.0543
  days_to_expiration <- 39
# ------------------------------------------------------------------------------  
# Calculate time to expiration in years
# ------------------------------------------------------------------------------  
  time_to_expiration <- days_to_expiration / 365
# ------------------------------------------------------------------------------
# Generate random stock price scenarios
# ------------------------------------------------------------------------------  
  set.seed(123) # For reproducibility
  z <- rnorm(num_simulations)
  simulated_stock_prices <- current_stock_price * exp((risk_free_rate - 0.5 * implied_volatility^2) * time_to_expiration + implied_volatility * sqrt(time_to_expiration) * z)
# ------------------------------------------------------------------------------
payoffs <- numeric(num_simulations)
for (i in 1:num_simulations) {
  payoff <- ifelse(
    simulated_stock_prices[i] < lower_put_strike,
    net_credit - (lower_put_strike - simulated_stock_prices[i]),
    ifelse(
      simulated_stock_prices[i] >= lower_put_strike & simulated_stock_prices[i] <= upper_put_strike,
      net_credit,
      ifelse(
        simulated_stock_prices[i] > upper_put_strike & simulated_stock_prices[i] < lower_call_strike,
        0,
        ifelse(
          simulated_stock_prices[i] >= lower_call_strike & simulated_stock_prices[i] <= upper_call_strike,
          net_credit,
          net_credit - (simulated_stock_prices[i] - upper_call_strike)
        )
      )
    )
  )
  payoffs[i] <- payoff
}  
}
################################################################################
# Mean Reversion                                    https://tinyurl.com/64dpjxh9
################################################################################
mean_reversion <- function(data, ticker){

  # Read the data
  df = data[[ticker]]

  # Compute indicators
  HLC = HLC(df)
  sma150 = SMA(Cl(df), n=150)
  atr10 = ATR(HLC, n=10, maType='EMA')
  rsi3 = RSI(Cl(df), n=3)

  # join them to the main data
  df = merge(df, sma150)
  df = merge(df, atr10$atr)
  df = merge(df, rsi3)
  df$atr = df$atr / Cl(df)
  df$close = Cl(df)

  # Select relevant columns
  keep_cols = c("SMA", "atr", "rsi", "close")
  df = df[, keep_cols]
  names(df)[1:3] = c("sma150", "atr10", "rsi3")

  # Get the most recent observation
  df = tail(df, 1)

  # Convert the output as a data table
  dt = as.data.table(df)
  dt[, ticker:=ticker]
  setnames(dt, "index", "date")

  dt
}
################################################################################
# DataFrameMaker                                    https://tinyurl.com/yyctoayr
################################################################################
DataFrameMaker  <- function(col,row){
  dd <- matrix(nrow = row, ncol = col)
  for (i in 1:row) {
    for (j in 1:col) {
      dd[i, j] = (j*i)
    }
  }
  return(as.data.frame(dd))
}
################################################################################
# Replicate left, mid, right formulas               https://tinyurl.com/yyq62obr
################################################################################
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

clean_col_names <- function(dt_name) {
  dt <- as.data.table( as.table(sapply(dtTables[NAME %like% paste0(dt_name,"*") ,][,1], function(x) x)))[,3]
  for(i in 1:nrow(dt)) {
    dt[i,] <<- clean_names( dt[i,])
  }  
}
################################################################################
## Tricks to manage the available memory in an R session                     ### 
## https://tinyurl.com/yxcttpsa
################################################################################
# improved list of objects -----------------------------------------------------
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           format(utils::object.size(x), units = "auto") })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Length_Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=100) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

# http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
dupsBetweenGroups <- function (df, idcol) {
    # df: the data frame
    # idcol: the column which identifies the group each row belongs to

    # Get the data columns to use for finding matches
    datacols <- setdiff(names(df), idcol)

    # Sort by idcol, then datacols. Save order so we can undo the sorting later.
    sortorder <- do.call(order, df)
    df <- df[sortorder,]

    # Find duplicates within each id group (first copy not marked)
    dupWithin <- duplicated(df)

    # With duplicates within each group filtered out, find duplicates between groups. 
    # Need to scan up and down with duplicated() because first copy is not marked.
    dupBetween = rep(NA, nrow(df))
    dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
    dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]

    # ============= Replace NA's with previous non-NA value ==============
    # This is why we sorted earlier - it was necessary to do this part efficiently

    # Get indexes of non-NA's
    goodIdx <- !is.na(dupBetween)

    # These are the non-NA values from x only
    # Add a leading NA for later use when we index into this vector
    goodVals <- c(NA, dupBetween[goodIdx])

    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx)+1

    # The original vector, now with gaps filled
    dupBetween <- goodVals[fillIdx]

    # Undo the original sort
    dupBetween[sortorder] <- dupBetween

    # Return the vector of which entries are duplicated across groups
    return(dupBetween)
}

################################################################################
## Clean, Consistent Column Names               https://tinyurl.com/yy3wo8rq ###
################################################################################
clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data

  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)

  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  
  n <- gsub("_+", "_", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

################################################################################
## unnest a list in a data.frame        https://www.r-bloggers.com/?p=188439 ###
################################################################################
library(rlang)
unnest_dt <- function(tbl, col) {

  tbl <- as.data.table(tbl)
  col <- ensyms(col)
  clnms <- syms(setdiff(colnames(tbl), as.character(col)))
  tbl <- as.data.table(tbl)
  tbl <- eval(
    expr(tbl[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  colnames(tbl) <- c(as.character(clnms), as.character(col))
  tbl
}
################################################################################
##  Split intermixed name's into first, middle, last                         ###
##                                      https://www.r-bloggers.com/?p=188402 ###
################################################################################
fml <- function(mangled_names) {
  titles <- c("MASTER", "MR", "MISS", "MRS", "MS",
              "MX", "JR", "SR", "M", "SIR", "GENTLEMAN",
              "SIRE", "MISTRESS", "MADAM", "DAME", "LORD",
              "LADY", "ESQ", "EXCELLENCY","EXCELLENCE",
              "HER", "HIS", "HONOUR", "THE",
              "HONOURABLE", "HONORABLE", "HON", "JUDGE")
  mangled_names %>% sapply(function(name) {
    split <- str_split(name, " ") %>% unlist
    original_length <- length(split)
    split <- split[which(!split %>%
                           toupper %>%
                           str_replace_all('[^A-Z]','')
                         %in% titles)]
    case_when(
      (length(split) < original_length) &
        (length(split) == 1) ~  c(NA,
                                  NA,
                                  split[1]),
      length(split) == 1 ~ c(split[1],NA,NA),
      length(split) == 2 ~ c(split[1],NA,
                             split[2]),
      length(split) == 3 ~ c(split[1],
                             split[2],
                             split[3]),
      length(split) > 3 ~ c(split[1],
                            paste(split[2:(length(split)-1)],
                                  collapse = "-"),
                            split[length(split)])
    )
  }) %>% t %>% return
}

# Running functions sequentially
# How to run multiple functions one after another?
`%@%` <- function(x, f) eval.parent(as.call(append(as.list(substitute(f)), list(x), 1))) # https://tinyurl.com/qo443mb
# f(trend_ind); f(dT.metrics); f(dT.rules) # https://tinyurl.com/rkmef5n


rbind.na <- function (..., deparse.level = 1) 
{
    na <- nargs() - (!missing(deparse.level))
    deparse.level <- as.integer(deparse.level)
    stopifnot(0 <= deparse.level, deparse.level <= 2)
    argl <- list(...)
    while (na > 0 && is.null(argl[[na]])) {
        argl <- argl[-na]
        na <- na - 1
    }    
    if (na == 0) 
        return(NULL)
    if (na == 1) {
        if (isS4(..1)) 
            return(rbind2(..1))
        else return(matrix(..., nrow = 1)) ##.Internal(rbind(deparse.level, ...)))
    }
    if (deparse.level) {
        symarg <- as.list(sys.call()[-1L])[1L:na]
        Nms <- function(i) {
            if (is.null(r <- names(symarg[i])) || r == "") {
                if (is.symbol(r <- symarg[[i]]) || deparse.level == 
                  2) 
                  deparse(r)
            }
            else r
        }
    }
    
    ## deactivated, otherwise no fill in with two arguments
    if (na == 0) {
        r <- argl[[2]]
        fix.na <- FALSE
    }
    else {
        nrs <- unname(lapply(argl, ncol))
        iV <- sapply(nrs, is.null)
        fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
        ## deactivated, otherwise data will be recycled
        #if (fix.na) {
        #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
        #    argl[[na]] <- rbind(rep(argl[[na]], length.out = nr), 
        #        deparse.level = 0)
        #}
        if (deparse.level) {
            if (fix.na) 
                fix.na <- !is.null(Nna <- Nms(na))
            if (!is.null(nmi <- names(argl))) 
                iV <- iV & (nmi == "")
            ii <- if (fix.na) 
                2:(na - 1)
            else 2:na
            if (any(iV[ii])) {
                for (i in ii[iV[ii]]) if (!is.null(nmi <- Nms(i))) 
                  names(argl)[i] <- nmi
            }
        }
        
        ## filling with NA's to maximum occuring ncols
        nCol <- as.numeric(sapply(argl, function(x) if (is.null(ncol(x))) length(x)
                                                    else ncol(x)))
        maxCol <- max(nCol, na.rm = TRUE)  
        argl <- lapply(argl, function(x)  if (is.null(ncol(x))) c(x, rep(NA, maxCol - length(x)))
                                          else cbind(x, matrix(, nrow(x), maxCol - ncol(x))))  
        
        ## create a common name vector from the
        ## column names of all 'argl' items
        namesVEC <- rep(NA, maxCol)  
        for (i in 1:length(argl)) {
          CN <- colnames(argl[[i]])          
          m <- !(CN %in% namesVEC)
          namesVEC[m] <- CN[m]          
        }  
        
        ## make all column names from common 'namesVEC'
        for (j in 1:length(argl)) {    
          if (!is.null(ncol(argl[[j]]))) colnames(argl[[j]]) <- namesVEC
        }
        
        r <- do.call(rbind, c(argl[-1L], list(deparse.level = deparse.level)))        
    }
    
    d2 <- dim(r)
    
    ## make all column names from common 'namesVEC'
    colnames(r) <- colnames(argl[[1]])
    
    r <- rbind2(argl[[1]], r)
        
    if (deparse.level == 0) 
        return(r)
    ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
    ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
    if (ism1 && ism2) 
        return(r)
    Nrow <- function(x) {
        d <- dim(x)
        if (length(d) == 2L) 
            d[1L]
        else as.integer(length(x) > 0L)
    }
    nn1 <- !is.null(N1 <- if ((l1 <- Nrow(..1)) && !ism1) Nms(1))
    nn2 <- !is.null(N2 <- if (na == 2 && Nrow(..2) && !ism2) Nms(2))
    if (nn1 || nn2 || fix.na) {
        if (is.null(rownames(r))) 
            rownames(r) <- rep.int("", nrow(r))
        setN <- function(i, nams) rownames(r)[i] <<- if (is.null(nams)) 
            ""
        else nams
        if (nn1) 
            setN(1, N1)
        if (nn2) 
            setN(1 + l1, N2)
        if (fix.na) 
            setN(nrow(r), Nna)
    }
    r
}
