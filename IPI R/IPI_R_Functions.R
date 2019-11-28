# Initiate Environment
ipiFolder <- path.expand(file.path('~', 'IPI International', 'IPI R'))
ipiSourceFiles <- path.expand(file.path('~', 'Source Files'))

if (!dir.exists(ipiSourceFiles)) {
  dir.create(ipiSourceFiles)
}

# Clean up R Project Window
ipi.cleanUp <- function() {
  try(
    {
      library(pacman)
      p_unload(all)                   # Easier: clears all add-ons
    }
  )
  # Clear packages
  try(
    detach("package:datasets", unload = TRUE) # For base
  )
  # Clear plots
  try(
    dev.off() # But only if there IS a plot
  )
  # Clear console
  cat("\014") # Ctrl+l
  #Clear Global environment leaving this function only
  rm(
    list = ls(envir = .GlobalEnv)[
      which(!ls(envir = .GlobalEnv) %in% c(
        "ipi.cleanUp", "ipi.sqlDelimImport" , "ipi.colSum", "ipi.getDateFormat", "ipiFolder", "ipiSourceFiles",
        "ipi.buildDeck", "ipi.blackJack"
      ))
      ], envir = .GlobalEnv
  )
  library(IPI)
}

# Sum up specified columns
ipi.colSum <- function(inp, cols, hdrs) {
  newGroup <- as.data.frame(cols)
  newGroup <- aggregate(inp, sum, by = newGroup)
  sel <- as.table(match(inp, cols))
  names(sel) <- colnames(inp)
  names(newGroup) <- c(hdrs, as.list(paste0("sum of ", colnames(inp))))
  newGroup <- newGroup[order(newGroup[,1]),]
  status <- sum(inp) == sum(newGroup[paste0("sum of ", names(sel))])
  totals <- data.frame(rbind(1:length(colnames(inp))))
  names(totals) <- colnames(inp)
  for (i in colnames(inp)) {
    totals[i] <- sum(newGroup[[paste0("sum of ", names(sel[i]))]])
  }
  res <- list(newGroup, status, totals)
  names(res) <- c("RecordSet", "Status", "Totals")
  res
}

# Get date formats
ipi.getDateFormat <- function(flPth) {
  gsub("  ", "\t",
"Value  Description
%a  Abbreviated weekday name
%A  Full weekday name
%b  Abbreviated month name
%B  Full month name
%c  Date and time, locale specific
%d  Day of the month as decimal number (01-31)
%H  Hours as decimal number (00-23) on the 24 hour clock
%I  Hours as decimal number (01-12) on the 12 hour clock
%j  Day of year as decimal number (0-366)
%m  Month as decimal number (0-11)
%M  Minute as decimal number (00-59)
%p  AM/PM indicator in the locale
%S  Second as decimal number (00-61, allowing for two 'leap seconds')
%U  Week of the year (00-52) uing the first Sunday as day 1 of week 1
%w  Weekday as decimal number (0-6, Sunday is 0)
%W  Week of the year (00-53) using the first Monday as day 1 of week 1
%x  Date, locale-specific
%X  Time, locale-specific
%Y  Year with century
%y  Year 2without century
%Z  Time zone as a character string (output only)"
  ) -> clp
  if (!dir.exists(dirname(flPth))) {
    dir.create(dirname(flPth))
  }
  if (!file.exists(flPth)) {
    write.table(clp, flPth, row.names = F, quote = F, col.names = F)
  }
  read.table(flPth, sep = "\t", header = T) -> fl
  fix(fl)
}

# Load file into SQL Server
ipi.sqlDelimImport <- function(svr, db, tbl, prms = list(...), ...) {
  try(
    library(data.table)
  )
  try(
    library(sqldf)
  )
  try(
    library(odbc)
  )
  if (length(prms) == 0) prms = list(a = 0)
  spr <- ifelse(!is.na(unlist(prms)['sep']), prms$sep, '\t')
  if (is.na(unlist(prms)['nrows'])) {
    df <- fread(file.choose(), sep = spr, showProgress = TRUE)
  } else {
    df <- fread(file.choose(), sep = spr, nrows = prms$nrows, showProgress = TRUE)
  }
  ifelse(!is.na(unlist(prms)['cnames']) | !is.na(unlist(prms)['cnames1']), colnames(df) <- prms$cnames, '')
  con <- dbConnect(
    odbc(),
    Driver = "SQL Server Native Client 11.0",
    Server = svr,
    Database = db,
    trusted_connection = 'yes'
  )
  types <- list(...)
  tryCatch(
    {
      dbWriteTable(
        conn = con,
        name = tbl,
        value = df,
        field.types = types
      )
    },
    error = function(cond) {
      message(paste(
        "If the column size is too big, then add the column name and column type as the last parament. \ne.g.",
        "ipi.sqlTSVImport(
            'YourSqlServer',
            'YourSqlDatabase',
            'YourSqlTable',
            list(
              cnames = c('ColumnNames'),    default: 1st row
              nrows = 'rows',               default: Read All
              sep = ','                     default: '\t'
            ),
            'Column1 Name' = 'nvarchar(max)',
            'Column2Name' = 'nvarchar(10000)',
            moreColumns....
            )\n",
        cond
      ))
      return(NULL)
    },
    finally = {
      dbDisconnect(conn = con)
    }
  )
}

# Build Card Deck
ipi.buildDeck <- function() {
  face <- c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace")
  suit <- rep(c("spades", "clubs", "diamonds", "hearts"), each = 13)
  value <- 13:1
  card = file.path(ipiSourceFiles,'gif', paste0(face, "-of-", suit, '.gif'))
  deck <- as.data.frame(cbind(face, suit, value, card))
  deck$value <- as.numeric(value)
  deck
}

# Play Blackjack
ipi.blackJack <- function(player = "You") {
  require(magick)
  require(stringr)
  require(stringi)

  DECK <- ipi.buildDeck()
  DECK$value[DECK$value > 10] <- 10

  wins <- data.frame()
  player = c(player, "Dealer")
  players = length(player)
  mLen = 520
  cWidth = 260 / 33
  iWidth = 72 * (12/72)
  height = 180 * length(player)

  for (i in str_length(player)) {
    len = round((i / 33) * 260)
    if (len > mLen) {
      mLen = len
    }
  }

  BlackJack <- function(playerNo) {
    ifelse(
      sum(dealt$value[dealt$Player == player[playerNo]]) + 10 < 22 &&
        length(dealt$value[dealt$Player == player[playerNo] & dealt$face == 'ace']) > 0,
      sum(dealt$value[dealt$Player == player[playerNo]]) + 10,
      sum(dealt$value[dealt$Player == player[playerNo]])
    ) == 21
  }

  Deal <- function(playerNo = 1) {
    if (all(dealt$Stick[dealt$Player == player[playerNo]] == FALSE)) {
      card = cbind(Player= player[playerNo], deck[!paste(deck$face, deck$suit) %in% paste(dealt$face,dealt$suit),][1,])
      assign("dealt", rbind(dealt, cbind(card, data.frame(Stick = FALSE, Bust = FALSE))), envir = parent.env(environment()))
      if (sum(dealt$value[dealt$Player == player[playerNo]]) > 21) {
        dealt$Bust[dealt$Player == player[playerNo]] <<- TRUE
        dealt$Stick[dealt$Player == player[playerNo]] <<- TRUE
      } else if (BlackJack(playerNo)) {
        dealt$Stick[dealt$Player == player[playerNo]] <<- TRUE
        dealt$value[dealt$Player == player[playerNo] & dealt$value == 1][1] <<- 11
      }
      Show()
    } else {
      print("This player has already stuck!")
    }
  }

  Dealer <- function() {
    if (all(dealt$Bust[!dealt$Player == 'Dealer'])) {
      Stick(length(player))
    } else
    if (BlackJack(length(player))) {
      Stick(length(player))
    } else if (
      scores$Score[scores$Player == 'Dealer'] <= 11 &&
      scores$Score[scores$Player == 'Dealer'] + 10 < 22 &&
      !any(dealt$face[dealt$Player == 'Dealer'] == 'ace')
    ) {
      Deal(length(player))
    } else if (
      (
        scores$Score[scores$Player == 'Dealer'] + 10 < 22 &&
        any(dealt$face[dealt$Player == 'Dealer'] == 'ace') &&
        min(scores$Score[scores$Player != 'Dealer']) <= scores$Score[scores$Player == 'Dealer'] + 10
      ) ||
      min(scores$Score[scores$Player != 'Dealer']) <= scores$Score[scores$Player == 'Dealer']
    ) {
      Stick(length(player))
    } else if (
      ((
        scores$Score[scores$Player == 'Dealer'] + 10 < 22 &&
        any(dealt$face[dealt$Player == 'Dealer'] == 'ace') &&
        !min(scores$Score[scores$Player != 'Dealer']) <= scores$Score[scores$Player == 'Dealer'] + 10
      ) ||
      (
        !min(scores$Score[scores$Player != 'Dealer']) <= scores$Score[scores$Player == 'Dealer']
      )) &&
      !max(scores$Score[scores$Player != 'Dealer']) > 21
    ) {
      Deal(length(player))
    } else {
      Stick(length(player))
    }
  }

  Games <- function() {
    if (length(wins) > 0) {
      scored <- aggregate(list(Won = wins$Won, Drawn = wins$Drawn, Beat_Dealer = wins$BeatDealer), list(Player = wins$Player), sum)
      scored
    } else {
      print("No Games have been finished yet!")
    }
  }

  Play <- function() {
    if (exists("dealt")) {
      dealt <<- dealt[0,]
    } else {
      assign("dealt", data.frame(), env = parent.env(environment()))
    }

    for (i in 1:2) {
      for (j in 1:length(player)) {
        if (exists("dealt")) {
          card = cbind(Player= player[j], deck[!paste(deck$face, deck$suit) %in% paste(dealt$face,dealt$suit),][1,])
          dealt <<- rbind(dealt, cbind(card, data.frame(Stick = FALSE, Bust = FALSE)))
        } else {
          card = cbind(Player= player[j], deck[1,])
          dealt <<- cbind(card, data.frame(Stick = FALSE, Bust = FALSE))
        }
      }
    }
    for (j in 1:length(player)) {
      if (BlackJack(j)) {
        dealt$Stick[dealt$Player == player[j]] <<- TRUE
        dealt$value[dealt$Player == player[j] & dealt$face == 'ace'] <<- 11
      }
      Show()
    }
  }

  Show <- function() {
    StuckOrBust()
    for (i in 1:length(player)) {
      msg <- sprintf("%s (Player No. %d) %s %d%s points%s",
                     player[i],
                     i,
                     ifelse(player[i] %in% c("I", "You", "We"), "have", "has"),
                     sum(dealt$value[dealt$Player == player[i]]),
                     ifelse(sum(dealt$value[dealt$Player == player[i]]) < 12 &
                              length(dealt$value[dealt$Player == player[i] & dealt$value == 1]) > 0,
                            paste(' or ', sum(dealt$value[dealt$Player == player[i]]) + 10), ''),
                     scores$Status[scores$Player == player[i]]
      )
      len <- paste0('+', (mLen - (str_length(msg) * cWidth)) / 2, '+0')
      pos = '+0+30'
      if (i == 1) {
        img <- image_composite(image_annotate(image_blank(mLen, 140, "white"), msg, location = len, size = 15),
                               image_append(image_read(file.path(dealt$card[dealt$Player == player[i]]))),
                               offset = pos
        )
      } else if (i < length(player) || all(dealt$Stick[dealt$Player != 'Dealer'] == TRUE)) {
        img <- image_append(c(img,image_composite(image_annotate(image_blank(mLen, 72 * i, "white"), msg, location = len, size = 15),
                                  image_append(image_read(file.path(dealt$card[dealt$Player == player[i]]))),
                                  offset = pos)
        ), stack = T)
      } else {
        msg <- sprintf("%s (Player No. %d)",
                       player[i],
                       i
        )
        len <- paste0('+', (mLen - (str_length('?') * cWidth)) / 2, '+0')
        hidden <- image_append(
          rep(
            image_annotate(image_blank(72, 72, "white"), "?", location = '+35+35', size = 25),
            length(dealt$card[dealt$Player == player[i]])
          )
        )
        img <- image_append(c(img,image_composite(image_annotate(image_blank(mLen, 72 * i, "white"), msg, location = len, size = 15),
                                                  hidden,
                                                  offset = pos)
        ), stack = T)
      }
    }
    disp <- image_composite(image_blank(mLen, height, "white"), img, offset = '+0+0')
    print(disp)
    if (
      all(dealt$Stick[dealt$Player != 'Dealer'] == TRUE) &&
      any(dealt$Stick[dealt$Player == 'Dealer'] == FALSE) &&
      any(dealt$Bust[dealt$Player == 'Dealer'] == FALSE) &&
      player[i] == 'Dealer') {
      Dealer()
    }
    if (all(dealt$Stick == TRUE) && Game == 0) {
      Game <<- Game + 1
      for (i in 1:length(player)) {
        if (
          any(
            scores$Status[scores$Player == player[i]] %in%
            c('. You Win!', '. Dealer Wins!')
          )
        ) {
          assign("wins", rbind(wins, data.frame(Player = player[i], Won = 1, Drawn = 0, BeatDealer = 0)), envir = parent.env(environment()))
        } else if (
          scores$Status[scores$Player == player[i]] == '. You Draw!'
        ) {
          assign("wins", rbind(wins, data.frame(Player = player[i], Won = 0, Drawn = 1, BeatDealer = 0)), envir = parent.env(environment()))
        } else if (
          scores$Status[scores$Player == player[i]] == '. You beat the Dealer!'
        ) {
          assign("wins", rbind(wins, data.frame(Player = player[i], Won = 0, Drawn = 0, BeatDealer = 1)), envir = parent.env(environment()))
        }
      }
    }
  }

  Shuffle <- function() {
    assign("Game", 0, envir = parent.env(environment()))
    random <- sample(1:52, size = 52)
    assign("playerNo", 1, envir = parent.env(environment()))
    assign("deck", DECK[random,], envir = parent.env(environment()))
    if (exists("dealt")) {
      Play()
    }
  }

  Stick <- function(playerNo = 1) {
    if (all(dealt$Stick[dealt$Player == player[playerNo]] == FALSE)) {
      dealt$Stick[dealt$Player == player[playerNo]] <<- TRUE
      if (
        sum(dealt$value[dealt$Player == player[playerNo]]) + 10 < 22 &&
        length(dealt$value[dealt$Player == player[playerNo] & dealt$value == 1]) > 0
      ) {
        dealt$value[dealt$Player == player[playerNo] & dealt$value == 1][1] <<- 11
      }
      Show()
    } else {
      print("This player has already stuck!")
    }
  }

  StuckOrBust <- function() {
    assign(
      "scores",
      cbind(aggregate(list(Score = dealt$value), list(Player = dealt$Player), sum), Status = '', stringsAsFactors = F),
      env = parent.env(environment())
    )
    for (i in 1:length(player)) {
      scores$Status[scores$Player == player[i]] <<- if (any(dealt$Bust[dealt$Player == player[i]] == TRUE)) {
        paste(ifelse(player[i] == 'Dealer', '. Dealer is', '. You are'), 'Bust')
      } else if (any(dealt$Stick[dealt$Player == player[i]] == TRUE)) {
        paste(ifelse(player[i] == 'Dealer', '. Dealer has', '. You have'), 'Stuck')
      } else {
        '.'
      }
      scores$Status[scores$Player == player[i]] <<- if(
        all(dealt$Stick == TRUE) &&
        !any(dealt$Bust[dealt$Player == player[i]] == TRUE) &&
        sum(max(scores$Score[scores$Score < 22]) == scores$Score[scores$Score < 22]) == 1 &&
        player[i] %in% scores$Player[scores$Score == max(scores$Score[scores$Score < 22])] &&
        !'Dealer' %in% scores$Player[scores$Score == max(scores$Score[scores$Score < 22])]
      ) {
        ifelse(player[i] == 'Dealer', '. Dealer Wins!', '. You Win!')
      } else if(
        all(dealt$Stick == TRUE) &&
        !any(dealt$Bust[dealt$Player == player[i]] == TRUE) &&
        sum(max(scores$Score[scores$Score < 22]) == scores$Score[scores$Score < 22]) > 1 &&
        player[i] %in% scores$Player[scores$Score == max(scores$Score[scores$Score < 22])] &&
        !'Dealer' %in% scores$Player[scores$Score == max(scores$Score[scores$Score < 22])]
      ) {
        '. You Draw!'
      } else if (
        all(dealt$Stick == TRUE) &&
        !any(dealt$Bust[dealt$Player == player[i]] == TRUE) &&
        !any(dealt$Bust[dealt$Player == 'Dealer'] == TRUE) &&
        'Dealer' %in% scores$Player[scores$Score == max(scores$Score[scores$Score < 22])]
      ) {
        ifelse(player[i] == 'Dealer', '. Dealer Wins!', '. You Lose to the Dealer!')
      } else if(
        all(dealt$Stick == TRUE) &&
        !any(dealt$Bust[dealt$Player == player[i]] == TRUE) &&
        (
          any(dealt$Bust[dealt$Player == 'Dealer'] == TRUE) ||
          scores$Score[scores$Player == 'Dealer'] < scores$Score[scores$Player == player[i]]
        )
      ) {
        '. You beat the Dealer!'
      } else if(
        all(dealt$Stick == TRUE) &&
        !any(dealt$Bust[dealt$Player == player[i]] == TRUE) &&
        !player[i] == 'Dealer' &&
        scores$Score[scores$Player == 'Dealer'] >= scores$Score[scores$Player == player[i]]
      ) {
        '. You Lose to the Dealer!'
      } else {
        scores$Status[scores$Player == player[i]]
      }
    }
  }

  Shuffle()
  Play()
  Show()
  list(deal = Deal, shuffle = Shuffle, games = Games, stick = Stick)
}

# Regenerate initial Traceback call
ipi.traceCall <- function(tb = invisible(traceback())) {
  tb <- tb[Vectorize(function(x) return(tb[x] != "eval(ncl)"))(seq_along(tb))]
  func <- strsplit(unlist(tb[length(tb)]), '[(]')[[1]][1]
  bdy <- paste0(strsplit(unlist(tb[length(tb)]), "[(]")[[1]][-1], collapse = "(")
  bdy <- substr(bdy, 1, nchar(bdy) - 1)
  df <- as.data.frame(strsplit(bdy, ","), col.names = "Names", stringsAsFactors = F)
  df$idx <- NA
  df[, 1] <- trimws(df[, 1])
  for (i in seq_along(df[, 1])) {
    if(length(strsplit(df[i, 1], "=")) > 0) {
      df[i, "idx"] <- match(trimws(strsplit(df[i, 1], "=")[[1]][1]), names(formals(lm)))
    }
  }
  df[is.na(df[,2]), 2] <- (1:nrow(df))[!(1:nrow(df)) %in% df[,2]]
  cl <- call(func, list())
  cl[df[,2]+1] <- parse(text = df[, 1])
  assign("ncl", cl, envir = .GlobalEnv)
}

# Generate Permutations from words
ipi.permute <- function(s) {
  alst <- Vectorize(function(i, s) {list(strsplit(s, "")[[1]][i]);})(1:nchar(s), s)
  h <- lng <- sapply(nchar(s), function(x) {for (i in 1:(x-2)) x <- x + x * i; x})
  w <- nchar(s)
  x <- cbind()
  for (n in seq(w)) {
    h <- floor(h / (w - ifelse(length(x) == 0, 0, ncol(x))))
    b <- 1
    c <- c()
    pop <- function() {
      for (i in seq(lng)) {
        if (!length(x) == 0) {
          for (k in b:w){
            if (!any(duplicated(append(x[i,],k)))){
              b <- k
              break
            }
          }
          if (any(duplicated(append(x[i,],b)))) {
            for (k in 1:b){
              if (!any(duplicated(append(x[i,],k)))){
                b <- k
                break
              }
            }
          }
        }
        c <- append(c, b)
        if (h > 1) {
          if (i %% h == 0) {
            b <- b + 1
          }
        } else {
          b <- b + 1
        }
        if (b > w) {
          b <- 1
        }
      }
      c
    }
    c <- pop()
    if (length(x) == 0) {
      x <- cbind(c)
    } else {
      x <- cbind(x, c)
    }
  }
  x <- data.frame(x, stringsAsFactors = F)
  vecs <- data.frame(
    t(sapply(seq(nrow(x)), function(a) sapply(seq(ncol(x)), function(c) alst[[x[a,c]]]))), stringsAsFactors = F)
  perms <- data.frame(
    Perms = sapply(seq(nrow(x)), function(a) paste0(
      sapply(seq(ncol(x)), function(c) paste0(alst[[x[a,c]]], collapse = "")), collapse = ""))
    , stringsAsFactors = F)
  perms <- data.frame(perms[order(perms),], stringsAsFactors = F)
  if (nrow(unique(perms)) < lng){
    nperms <- perms
    perms <- data.frame(unique(perms[,1]), stringsAsFactors = F)
    names(nperms) <- names(perms) <- s
    list(
      Total = nrow(perms),
      NonUniqueTotal = lng,
      Permutations = perms,
      NonUniquePermutations = nperms,
      Vectors = vecs,
      NumericVectors = x
    )
  } else {
    names(perms) <- s
    list(
      Total = lng,
      Permutations = perms,
      Vectors = vecs,
      NumericVectors = x
    )
  }
}

# Calculate total number of possible permutations from length
ipi.lenPermute <- function(n) {
  # Using n = 50000000 Vectorize User time = 3.02 sapply = 2.90 and Vectorize sytem time = 0.03 and sapply = 0.00
  # Vectorize(x <- function(j) {for(i in 1:(j - 2)) {j <- j + j*i}; j})(n)
  sapply(n, function(x) {for (i in 1:(x-2)) x <- x + x * i; x})
}

# Read SQLite file
ipi.sqliteRead <- function(tbl = "") {
  if (tbl != "") {
    library(DBI)
    db <- ""
    try(db <- file.choose())
    if (db != "") {
      con <- dbConnect(RSQLite::SQLite(), db)
      df <- dbReadTable(con, tbl)
      df <- as.data.frame(df)
      dbDisconnect(con)
      df
    } else {
      print("please select a SQLite file from the file system")
    }
  } else {
    print("Please sepicify a table name to be read")
  }
}

# Preview HTML page
ipi.htmlPreview <- function(pg = ""){
  library(htmltools)
  if (pg == ""){
    pg <- file.choose()
  }
  htmlPage <- readLines(pg)
  html_print(HTML(htmlPage))
}

# Find recursive addition base number
ipi.recursive.base <- function(n) {
  res <- sapply(n, function(x) {
    s <- i <- 1
    while(s < x) {
      if (s + s < x) {
        i <- i + 1
        s <- s + s
      } else if (s + s == x) {
        s <- s + s
        return(list(i, s))
      } else {
        j <- i
        i <- i - 1
        k <- s + s
        return(list(i, s, j, k))
      }
    }
  })
  if (length(res) > 2){
    list(base = res[[1]], closest = res[[2]], outsideBase = res[[3]], outside = res[[4]])
  } else {
    list(base = res[[1]], closest = res[[2]])
  }
}

# Find permutation base number
ipi.permute.base <- function(n) {
  res <- sapply(n, function(x) {
    s <- i <- 1
    while(s < x){
      i <- i + 1
      if (s * i < x){
        s <- s * i
      } else if (s * i == x){
        s <- s * i
        return(list(i, s))
      } else {
        j <- i
        k <- s * i
        i <- i - 1
        return(list(i, s, j, k))
      }
    }
  })
  if (length(res) > 2){
    list(base = res[[1]], closest = res[[2]], outsideBase = res[[3]], outside = res[[4]])
  } else {
    list(base = res[[1]], closest = res[[2]])
  }
}

# Uses c++ and the Rcpp library to check integer matrix for duplicates and returns row numbers of y
ipi.duprow.int.matrix <-function(x, y, z) {
  Rcpp::cppFunction("IntegerVector backlin(IntegerMatrix a, IntegerMatrix B) {
      IntegerMatrix av(a);
      IntegerMatrix Bm(B);
      int i, j, k, n;
      IntegerVector out(av.nrow());
      for (n = 0; n < av.nrow(); n++){
        for (i = 0; i < Bm.nrow(); i++) {
          k = 0;
          for (j = 0; j < Bm.ncol(); j++) {
            if (av(n, j) == Bm(i, j)) k++;
          }
          if (k == Bm.ncol()) {
            out(n) = i + 1;
          }
        }
      }
      return(out);
  }")
  backlin(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
}

# Uses c++ and the Rcpp library to check integer matrix for duplicates and returns row numbers of x
ipi.duprow.int.matrixi <-function(x, y, z) {
  Rcpp::cppFunction("IntegerVector backlin(IntegerMatrix a, IntegerMatrix B) {
      IntegerMatrix av(a);
      IntegerMatrix Bm(B);
      int i, j, k, n;
      IntegerVector out(av.nrow());
      for (n = 0; n < av.nrow(); n++){
        for (i = 0; i < Bm.nrow(); i++) {
          k = 0;
          for (j = 0; j < Bm.ncol(); j++) {
            if (av(n, j) == Bm(i, j)) k++;
          }
          if (k != Bm.ncol()) {
            out(n) = n + 1;
          }
        }
      }
      return(out);
  }")
  backlin(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
}

# Uses c++ and the Rcpp library to check character matrix for duplicates and returns row numbers y
ipi.duprow.char.matrix <- function(x, y, z){
  Rcpp::cppFunction("IntegerVector backlin(CharacterMatrix a, CharacterMatrix B) {
      CharacterMatrix av(a);
      CharacterMatrix Bm(B);
      int i, j, k, n;
      IntegerVector out(av.nrow());
      for (n = 0; n < av.nrow(); n++){
        for (i = 0; i < Bm.nrow(); i++) {
          k = 0;
          for (j = 0; j < Bm.ncol(); j++) {
            if (av(n, j) == Bm(i, j)) k++;
          }
          if (k == Bm.ncol()) {
            out(n) = i + 1;
          }
        }
      }
      return(out);
  }")
  backlin(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
}

# Uses c++ and the Rcpp library to check character matrix for duplicates and returns row numbers x
ipi.duprow.char.matrixi <- function(x, y, z){
  Rcpp::cppFunction("IntegerVector backlin(CharacterMatrix a, CharacterMatrix B) {
      CharacterMatrix av(a);
      CharacterMatrix Bm(B);
      int i, j, k, n;
      IntegerVector out(av.nrow());
      for (n = 0; n < av.nrow(); n++){
        for (i = 0; i < Bm.nrow(); i++) {
          k = 0;
          for (j = 0; j < Bm.ncol(); j++) {
            if (av(n, j) == Bm(i, j)) k++;
          }
          if (k != Bm.ncol()) {
            out(n) = n + 1;
          }
        }
      }
      return(out);
  }")
  backlin(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
}

# Uses c++ and the Rcpp library to check an integer matrix for duplicates and returns true on the 1st match
ipi.dup.int.matrix <- function(x, y, z){
  Rcpp::cppFunction("bool backlin(IntegerMatrix a, IntegerMatrix B) {
      IntegerMatrix av(a);
      IntegerMatrix Bm(B);
      int i, j, k;
      for (i = 0; i < Bm.nrow(); i++) {
        k = 0;
        for (j = 0; j < Bm.ncol(); j++) {
          if (av(0, j) == Bm(i, j)) k++;
        }
        if (k == Bm.ncol()) return true;
      }
      return false;
  }")
  backlin(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
}

# Uses c++ and the Rcpp library to check an character matrix for duplicates and returns true on the 1st match
ipi.dup.char.matrix <- function(x, y, z) {
  Rcpp::cppFunction("bool backlin(CharacterMatrix a, CharacterMatrix B) {
      CharacterMatrix av(a);
      CharacterMatrix Bm(B);
      int i, j, k;
      for (i = 0; i < Bm.nrow(); i++) {
        k = 0;
        for (j = 0; j < Bm.ncol(); j++) {
          if (av(0, j) == Bm(i, j)) k++;
        }
        if (k == Bm.ncol()) return true;
      }
      return false;
  }")
  backlin(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
}

# More stable c++ function that uses Rcpp library to check any type of matrix for duplicates and returns row numbers
ipi.dup.rem <- function(data, ncol, bgn = 0) {
  Rcpp::cppFunction("IntegerVector dup_rem(CharacterVector a, int B) {
      CharacterVector av(a);
      IntegerVector Bm(B);
      int i, j, k, l;
      l = av.size();
      IntegerVector out(av.size());
      for (i = l; i --> B;){
        k = 0;
        for (j = i + 1; j --> 0;) {
          if (av[i] == av[j]) {
            k++;
          }
          if (k > 1) {
            out(i) = i + 1;
            break;
          }
        }
      }
      return(out);
    }")
  dup_rem(apply(matrix(as.matrix(data), ncol = ncol), 1, paste, collapse = ""), bgn)
}

# Avoids loading all functions in a script file by loading only the function that meets the passed in function name
ipi.function.load <- function(fPth , func, envir = .GlobalEnv) {
  flBody <- enquote({parse(fPth, file.info(fPth)$size)})
  for (i in seq(flBody[[2]])) {
    if (as.character(flBody[[2]][i][[1]])[[2]] == func) {
      assign(func, eval(flBody[[2]][i][[1]]), envir = envir)
    }
  }
}
