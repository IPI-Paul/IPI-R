# Keep sakes or hording?
.Internal(.invokeRestart(list(NULL, NULL), NULL))
tools::pskill(Sys.getpid(), SIGINT)

ipi.cleanUp()

# Uses c++ and the Rcpp library to check integer matrix for duplicates and returns row numbers of x
ipi.duprow.int.matrixi <- function(x, y, z) {
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
# Uses c++ and the Rcpp library to check an integer matrix for duplicates and returns true on the 1st match
# Does not work within a package
.ipi.dup.int.matrix <- Rcpp::cppFunction("bool backlin(IntegerMatrix a, IntegerMatrix B) {
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

ipi.dup.int.matrix <- function(x, y, z){
  # tmp <- matrix(as.matrix(y), ncol = z)
  # rows <- nrow(tmp)
  # res <- FALSE
  # mx <- ifelse(120 > rows, rows, 120)
  # for (i in 0:(ceiling(rows/mx)-1)) {
  #   bgn <- (mx * i) + 1
  #   enD <- ceiling(mx * (i + 1))
  #   enD <- ifelse(enD > rows, rows, enD)
  #   if (backlin(matrix(as.matrix(x), ncol = z), matrix(tmp[bgn:enD, ], ncol = z))) {
  #     res <- TRUE
  #     break
  #   }
  # }
  # res
  res <- .ipi.dup.int.matrix(matrix(as.matrix(x), ncol = z), matrix(as,matrix(y), ncol = z))
}

# Uses c++ and the Rcpp library to check an character matrix for duplicates and returns true on the 1st match
# Did not really work because needed to check fullset of both matrices to get duplication
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
  tmp <- matrix(as.matrix(y), ncol = z)
  rows <- nrow(tmp)
  res <- FALSE
  mx <- ifelse(120 > rows, rows, 120)
  for (i in 0:(ceiling(rows/mx)-1)) {
    bgn <- (mx * i) + 1
    enD <- ceiling(mx * (i + 1))
    enD <- ifelse(enD > rows, rows, enD)
    if (backlin(matrix(as.matrix(x), ncol = z), matrix(tmp[bgn:enD, ], ncol = z))) {
      res <- TRUE
      break
    }
  }
  res
}

# sapply version of ccpFunction, but still too slow in performance tests
ipi.duprow.matrixi <- function(x, y, z) {
  res <- c()
  chk <- function(a, b) {
    sapply(seq(nrow(a)),
           function(i) {
             rows <- nrow(b)
             sapply(seq(nrow(b)),
                    function(j) {
                      k <- 0
                      sapply(seq(ncol(b)),
                             function(c) {
                               k <<- k + as.numeric(a[i, c] == b[j, c])
                             }
                      )
                      if (k == ncol(b)) {
                        rows <<- rows -1
                        return
                      }
                    }
             )
             if (rows == nrow(b)){
               res <<- append(res, i)
             }
           }
    )
}
  chk(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
  res
}

# sapply version of ccpFunction, but still too slow in performance tests
ipi.dup.matrixi.old <- function(x, y, z) {
  res <- FALSE
  chk <- function(a, b) {
    sapply(seq(nrow(a)),
           function(i) {
             rows <- nrow(b)
             sapply(seq(nrow(b)),
                    function(j) {
                      k <- 0
                      sapply(seq(ncol(b)),
                             function(c) {
                               k <<- k + as.numeric(a[i, c] == b[j, c])
                             }
                      )
                      print(j)
                      if (k == ncol(b)) {
                        res == TRUE
                        break
                      }
                    }
             )
             if (res){
               res <<- append(res, i)
             }
           }
    )
  }
  chk(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
  res
}

# sapply version of ccpFunction and faster than the one above, but still too slow in performance tests
ipi.dup.matrixi <- function(x, y, z) {
  res <- FALSE
  chk <- function(a, b) {
    for (i in seq(nrow(a))) {
      for (j in seq(nrow(b))) {
        k <- 0
        for(c in seq(ncol(b))) {
          k <- k + as.numeric(a[i, c] == b[j, c])
        }
        if (k == ncol(b)) {
          res <<- TRUE
          break
        }
        if (res){
          break
        }
      }
    }
  }
  chk(matrix(as.matrix(x), ncol = z), matrix(as.matrix(y), ncol = z))
  res
}

# sapply version of ccpFunction and faster than ipi.duprow.matrixi, but still too slow in performance tests
ipi.duprem.matrix <- function(data, ncol) {
  res <- c()
  chk <- function(a) {
    sapply(seq(nrow(a),1),
           function(i) {
             if (nrow(a) - (nrow(a)-i) > 0){
               rows <- 0
               ln <- 0
               sapply(seq((nrow(a) - (nrow(a)-i)),1),
                      function(j) {
                        if (a[i, ] == a[j, ]) {
                          rows <<- rows + 1
                          ln <<- i
                        }
                      }
               )
               if (rows > 1) {
                 res <<- append(res, ln)
               }
             }
           }
    )
  }
  x <- apply(data, 1, paste, collapse = "")
  chk(matrix(as.matrix(x), ncol = 1))
  res
}

# sapply version of ccpFunction and faster than ipi.duprem.matrix, but still too slow in performance tests
ipi.dup.rem.old <- function(data, bgn) {
  res <- c()
  chk <- function(a) {
    sapply(seq(length(a),bgn),
           function(i) {
             if (length(a) - (length(a)-i) > 0){
               rows <- 0
               ln <- 0
               sapply(seq((length(a) - (length(a)-i)),1),
                      function(j) {
                        if (a[i] == a[j]) {
                          rows <<- rows + 1
                          ln <<- i
                        }
                      }
               )
               if (rows > 1) {
                 res <<- append(res, ln)
               }
             }
           }
    )
  }
  x <- apply(data, 1, paste, collapse = "")
  chk(x)
  res
}

# Uses apply, sapply and factor but slows down immensely on larger permutations in performance tests
ipi.dupfac.matrix <- function(data, ncol) {
  #x <- sapply(seq(nrow(data)), function(i) paste0(data[i,1:ncol], collapse = ""))
  #apply(t(matrix(unlist(apply(matrix(attr(unclass(factor(x)), "levels")), 1, strsplit, "")), byrow = T, ncol = ncol)),1, as.numeric)
  x <<- x <- apply(data, 1, paste, collapse = "")
  matrix(sapply(attr(unclass(factor(x)), "levels"), function(i) as.numeric(strsplit(i, "")[[1]])), byrow = T, ncol = ncol)
}

# Tests leading up to ipi.function.load
expr <- expression ({
  hello <- function(a) cat("Hello", a, "\n")
  bye <- function(a) cat("Goodbye", a, "\n")
  list( hello = hello, bye = bye)
})
eval({eval(func[[1]][2][[1]]);do.call(as.character(func[[1]][2][[1]][2][[1]]), list("Paul"))})
eval({eval(func[[1]][3][[1]]);do.call(as.character(func[[1]][3][[1]][2][[1]]), list("Paul"))})

