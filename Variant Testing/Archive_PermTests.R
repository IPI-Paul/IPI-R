# Uses Vectorisation, sample, unique, replicate and data.frame to build permutation list
permTest1 <- function(test) {
  perm1 <- function(s) {
    lst <- Vectorize(x <- function(i, s) {list(strsplit(s, "")[[1]][i]);})(1:nchar(s), s)
    # reps <- Vectorize(x <- function(j) {for(i in 1:(j)) {j <- j + j*i}; j})(nchar(s))
    reps <- sapply(nchar(s), function(x) {for (i in 1:x) x <- x + x * i; x})
    # perm <- Vectorize(x <- function(j) {for(i in 1:(j - 2)) {j <- j + j*i}; j})(nchar(s))
    perm <- sapply(nchar(s), function(x) {for (i in 1:(x-2)) x <- x + x * i; x})
    # pos <- Vectorize(x <- function(j) {for(i in 1:(j - 2)) {j <- j + j*i}; j})(nchar(paste(unique(lst), collapse = "")))
    pos <- sapply(nchar(paste(unique(lst), collapse = "")), function(x) {for (i in 1:(x-2)) x <- x + x * i; x})
    nperms <- unique(data.frame(replicate(reps, paste(sample(seq(lst)), collapse = "")), stringsAsFactors = F)[, 1])
    nperms <- Vectorize(x <- function(n) {
      list(Vectorize(y <- function(i) {as.numeric(strsplit(nperms[n], "")[[1]][i])})(1:nchar(s)))
    })(seq(nperms))
    nperms <- data.frame(Vectorize(x <- function(n) {paste(lst[nperms[[n]]], collapse = "")})(seq(nperms)),
                         stringsAsFactors = F)
    names(nperms) <- s
    if (length(lst) != length(unique(lst))) {
      perms <- data.frame(unique(replicate(reps, paste(sample(lst, replace = FALSE), collapse = ""))), stringsAsFactors = F)
      uperms <- data.frame(unique(replicate(reps, paste(sample(unique(lst), replace = FALSE), collapse = ""))),
                           stringsAsFactors = F)
      names(perms) <- names(uperms) <- s
      list(
        Total = nrow(perms),
        NonUniqueTotal = perm,
        UniqueTotal = pos,
        Permutations = perms,
        NonUniquePermutations = nperms,
        UniquePermutations = uperms
      )
    } else {
      list(
        Total = perm,
        Permutations = nperms
      )
    }
  }
  tryCatch(
    {
      desc <- "Uses Vectorisation, sample and unique to build permutation list"
      print(desc)
      stm <- system.time(w <<- perm1(test))
      cat(paste(capture.output(stm), "\n"))
      chk <- list(unique.rows = nrow(unique(w[[length(w)]])), total.rows = nrow(w[[length(w)]]), 
                  characters = nchar(w[[length(w)]][1,]))
      TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
    }, 
    error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Uses internal function, for loops a data.frame and Vectorised filtering to build perumtaion list
permTest2 <- function(test) {
  perm2 <- function(s) {
    alst <- Vectorize(function(i, s) {list(strsplit(s, "")[[1]][i]);})(1:nchar(s), s)
    lst <- data.frame(rbind(seq(1:nchar(s))))
    lng <- sapply(nchar(s), function(x) {for (i in 1:(x-2)) x <- x + x * i; x})
    x <- data.frame()
    z <- function (y) {
      for (i in seq(y)) {
        n <- append(y[, i], y[, -i])
        names(n) <- paste0("Col", 1:nchar(s))
        m <- n
        dim(m) <- c(1, nchar(s))
        if (nrow(x) == 0) {
          assign("x", data.frame(n), envir = parent.env(environment()))
        } else if (nrow(y[which(y == m || is.na(y)),]) == 0) {
          assign("x", data.frame(rbind(x, n)), envir = parent.env(environment()))
        }
      }
    }
    for (i in 1:lng) {
      if (nrow(x) == 0) {
        z(lst)
      } else {
        z(x[i,])
      }
      if (nrow(x) == lng) {
        break
      }
    }
    perms <- data.frame(
      Vectorize(function(r) {
        paste0(
          Vectorize(z <- function(n) {paste(alst[x[r, n]], collapse = "")})(seq(x)), 
          collapse= "")
      })
      (1:nrow(x)),stringsAsFactors = F)
    if (nrow(unique(perms)) < lng){
      nperms <- perms
      perms <- data.frame(unique(perms[,1]))
      names(nperms) <- names(perms) <- s
      list(
        Total = nrow(perms),
        NonUniqueTotal = lng,
        Permutations = perms,
        NonUniquePermutations = nperms
      )
    } else {
      names(perms) <- s
      list(
        Total = lng,
        Permutations = perms
      )
    }
  }
  tryCatch(
    {
      desc <- "Uses internal function, for loops a data.frame and Vectorised filtering to build perumtaion list"
      print(desc)
      stm <- system.time(w <- perm2(test))
      cat(paste(capture.output(stm), "\n"))
      chk <- list(unique.rows = nrow(unique(w[[length(w)]])), total.rows = nrow(w[[length(w)]]), 
                  characters = nchar(w[[length(w)]][1,]))
      TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
    }, 
    error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Uses for loop, internal function, rbind, data frame and duplicate check with any()
permTest3 <- function(test) {
  ipi.permuteq <- function(s) {
    alst <- Vectorize(function(i, s) {list(strsplit(s, "")[[1]][i]);})(1:nchar(s), s)
    lst <- seq(1:nchar(s))
    lng <- sapply(nchar(s), function(x) {for (i in 1:(x-2)) x <- x + x * i; x})
    x <- data.frame()
    z <- function (y) {
      for (i in seq(y)) {
        n <- paste0(paste0(y[-i], collapse = ""), y[i], collapse = "")
        if (nrow(x) == 0) {
          assign("x", data.frame(col = n, stringsAsFactors = F), envir = parent.env(environment()))
        } else if (!any(x == n) && !n == "NA" && !n == 1) {
          assign("x", data.frame(col = rbind(x, n), stringsAsFactors = F), envir = parent.env(environment()))
        }
      }
    }
    for (i in 1:lng) {
      if (nrow(x) == 0) {
        z(lst)
      } else {
        z(as.list(strsplit(x[i,], ""))[[1]])
      }
      if (nrow(x) == lng) {
        break
      }
    }
    perms <- Vectorize(z <- function(n) {
      list(Vectorize(y <- function(i) {as.numeric(strsplit(x[n,], "")[[1]][i])})(1:nchar(s)))
    })(1:nrow(x))
    perms <- data.frame(Vectorize(z <- function(n) {paste(alst[perms[[n]]], collapse = "")})(seq(perms)),
                        stringsAsFactors = F)
    if (nrow(unique(perms)) < lng){
      nperms <- perms
      perms <- data.frame(unique(perms[,1]), stringsAsFactors = F)
      names(nperms) <- names(perms) <- s
      list(
        Total = nrow(perms),
        NonUniqueTotal = lng,
        Permutations = perms,
        NonUniquePermutations = nperms
      )
    } else {
      names(perms) <- s
      list(
        Total = lng,
        Permutations = perms
      )
    }
  }
  tryCatch({
    desc <- "Uses for loop, internal function, rbind, data frame and duplicate check with any()"
    print(desc)
    stm <- system.time(w <- ipi.permuteq(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w[[length(w)]])), total.rows = nrow(w[[length(w)]]), 
                characters = nchar(w[[length(w)]][1,]))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Fastest so far and currently published. Using matrices, internal function, cbind and any(duplicated())
permTest4 <- function(test) {
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
        NonUniquePermutations = nperms
      )
    } else {
      names(perms) <- s
      list(
        Total = lng,
        Permutations = perms
      )
    }
  }
  tryCatch({
    desc <- "Fastest so far and currently published. Using matrices, internal function, cbind and any(duplicated())"
    print(desc)
    stm <- system.time(w <- ipi.permute(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w[[length(w)]])), total.rows = nrow(w[[length(w)]]), 
                characters = nchar(w[[length(w)]][1,]))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Permutation using sapply and rep, then attempts to use Vetorisation and assign to columns that are not unique
permTest5 <- function(test) {
  perm5 <- function(s){
    w <- nchar(s)
    t <- as.numeric(sapply(nchar(s), function(x) {for (i in 1:(x-2)) x <- x + x * i; x}))
    x <- data.frame(c1 = matrix((sapply(w, function(n) rep(seq(n), each = (t / w)))), ncol = 1))
    x <- cbind(x, 
               data.frame(c2 =
                            matrix(
                              sapply(seq(max(x)), 
                                     function(r) {
                                       rep(seq(max(x))[-r], 
                                           each = (nrow(x)/max(x))/(max(x)-ncol(x)))
                                     }
                              ),
                              ncol = 1
                            )
               )
    )
    colNm <- paste0("Col", ncol(x))
    
    x[as.numeric(rownames(unique(
      x[as.numeric(rownames(x[sapply(seq(nrow(x)), function(a) !1 %in% x[a,]),])),]))), ]
    
    for (i in seq(max(x[,1]))) {
      x[as.numeric(rownames(unique(
        x[as.numeric(rownames(x[sapply(seq(nrow(x)), function(a) !i %in% x[a,]),])),]))), colNm] <- i
    }
    x
    x[as.numeric(rownames(
      unique(x[as.numeric(rownames(
        x[sapply(seq(nrow(x)), function(a) NA %in% x[a,]),])),]))), ]
    
    for (i in seq(max(x[,1]))){ 
      x[as.numeric(rownames(
        unique(x[as.numeric(rownames(
          x[sapply(seq(nrow(x)), function(a) NA %in% x[a,]),])),]))), colNm] <- i
    }
    
    x[as.numeric(rownames(
      unique(x[as.numeric(rownames(
        x[sapply(seq(nrow(x)), function(a) !i %in% x[a,] & any(is.na(x[a,]))),])),]))),]
    
    for (i in seq(max(x[,1]))) {
      x[as.numeric(rownames(
        unique(x[as.numeric(rownames(
          x[sapply(seq(nrow(x)), function(a) !i %in% x[a,] & any(is.na(x[a,]))),])),]))),][1:(max(x[,1])), colNm] <- i
    }
    tryCatch(
      for (i in seq(max(x[,1]))) {
        x[as.numeric(rownames(
          unique(x[as.numeric(rownames(
            x[sapply(seq(nrow(x)), function(a) !i %in% x[a,] & any(is.na(x[a,]))),])),]))),][, colNm] <- i
      }
      , error = function(cond) {zz <- cond}
    )
    x
  }
  tryCatch({
    desc <- "Permutation using sapply and rep, then attempts to use Vetorisation and assign to columns that are not unique"
    print(desc)
    stm <- system.time(w <- perm5(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Permutation using matrices, sapply, rep, unlist, Vectorise and then cbind that fails
permTest6 <- function(test) {
  perm6 <- function(s){
    x <- rbind(strsplit(s,"")[[1]])
    w <- nchar(s)
    t <- as.numeric(sapply(nchar(s), function(x) {for (i in 1:(x-2)) x <- x + x * i; x}))
    h <- floor(t / w)
    x <- data.frame(rep(seq(x), each = h))
    x <- cbind(x,
               if (w - ncol(x) > 1){
                 matrix(sapply(seq(1,t, 2), 
                               function(n) {
                                 unlist(Vectorize(
                                   function(r) {
                                     rp <- sapply(h, function(c) {for (i in seq(ncol(x))) c <- floor(c / (w - i)); c})
                                     c <- cbind(x[x != x[r,]])
                                     c <- rep(x[seq(1,t,t/w),], each = rp)
                                     if (rp > 1){
                                       c <- c[c != x[r,]]
                                     } else {
                                       c <- c[-x[r,]]
                                     }
                                     c <- c[!is.na(c)]
                                   }
                                 )(n)[, 1]
                                 )}
                 ), ncol = 1)[1:t]
               } else {
                 matrix(sapply(seq(1,t,1), 
                               function(n) {
                                 unlist(Vectorize(
                                   function(r) {
                                     rp <- sapply(h, function(c) {for (i in seq(ncol(x))) c <- floor(c / (w - i)); c})
                                     c <- cbind(x[x != x[r,]])
                                     c <- rep(x[seq(1,t,t/w)], each = rp)
                                     if (rp > 1){
                                       c <- c[c != x[r,]]
                                     } else {
                                       c <- c[-x[r,]]
                                     }
                                     c <- c[!is.na(c)]
                                   }
                                 )(n)
                                 )}
                 ), ncol = 1)[1:t]
               }
    )
  }
  tryCatch({
    desc <- "Permutation using matrices, sapply, rep, unlist, Vectorise and then cbind that fails"
    print(desc)
    stm <- system.time(w <- perm6(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Uses internal function, any and sapply to check for duplicates, rbind, a for loop and a while loop
permTest7 <- function(test) {
  perm7 <- function(x) {
    fun <- function(x, c){
      w <- rbind(strsplit(x,"")[[1]])
      z <- w[0]
      p <- 1:(nchar(x)-1)
      for (i in seq(p)){
        n <- append(w[-i],w[i])
        if(!any(sapply(seq(nrow(c)), function(a) all(append(w[-i],w[i]) == c[a,])))){
          z <- rbind(z, append(w[-i],w[i]))
        }
      }
      z
    }
    w <- rbind()
    w <- fun(paste0(seq(strsplit(x, "")[[1]]), collapse = ""), w)
    i <- 1
    while (nrow(w) < sapply(length(w[1,]), function(z) {for (i in 1:(z-2)) z <- z + z * i; z})) {
      w <- rbind(w, fun(paste0(w[i,], collapse = ""), w))
      i <- i + 1
    }
    w
  }
  tryCatch({
    desc <- "Uses internal function, any and sapply to check for duplicates, rbind, a for loop and a while loop"
    print(desc)
    stm <- system.time(w <- perm7(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Fails over 3 characters to permute by cbind and replicate. Checks using rbind on rows prior with while loops
permTest8 <- function(test) {
  perm8 <- function(perm) {
    cols <- nchar(perm)
    rows <- as.numeric(sapply(cols, function(x) {for (i in 1:(x-2)) x <- x + x * i; x})) 
    res <- cbind()
    sz <- 0
    while (sz < cols) {
      len <- Vectorize(
        function(x, y) {
          j <- 1
          for (i in x:(x - y)) {
            j <- j * i
          }
          list(floor(rows / j), j / x)
        })(cols, sz)
      #print(paste("Length:",len[[1]], "Rows:", len[[2]], "Columns:", ncol(res), "Size:", sz))
      vec <- rbind()
      st <- 1
      en <- len[[1]]
      for (i in seq(len[[2]])) {
        for (j in seq(cols)) {
          r <- res[st:en,]
          if (len[[1]] <= 1){
            if (ncol(res) < sz + 1) {
              res <- cbind(res, NA)
            }
            # cat("Bind: ", append(r, j), "\n")
            # cat("Res: ", matrix(res[1:en, ]), "\n")
            i <- 1
            while (j %in% r) {
              j <- j + 1
              if (j > cols) {
                j <- 1
              } 
            }
            # Causes Error in while (nrow(res[duplicated(res), ]) > 0) {argument is of length zero
            #if(en == -1){
              k <- j
              if (en > 1){
                while (
                  length(
                    rbind(res[1:(en - 1),], 
                          append(res[en:en,][!is.na(res[en:en,])], j))[
                            duplicated(
                              rbind(res[1:(en - 1),], 
                                    append(res[en:en,][!is.na(res[en:en,])], j)))
                            ]) > 0 || 
                  j %in% r) {
                  # cat("Duplicated: ", 
                  #     length(
                  #       rbind(res[1:(en - 1),], 
                  #             append(res[en:en,][!is.na(res[en:en,])], j)
                  #             )[duplicated(rbind(res[1:(en - 1),], append(res[en:en,][!is.na(res[en:en,])], j)))]), 
                  #     "\nRes: ",
                  #     paste(capture.output(res[1:(en - 1),]), "\n"), "\nJ: ", 
                  #     paste(capture.output(append(res[en:en,][!is.na(res[en:en,])], j)), "\n"), 
                  #     "\nIn Row: ", j %in% r, "\n" 
                  #     )
                  j <- j + 1
                  if (j > cols) {
                    j <- 1
                  }
                  i <- i + 1
                  if (i == 8) {
                    j <- k
                    break
                  }
                }
              }
            #}
            res[st:en, (sz + 1)] <- j
            #if (en == 10) .Internal(.invokeRestart(list(NULL, NULL), NULL))
          } else {
            n <- ifelse(sz == 0, j, ifelse(is.na((seq(cols)[-r])[j]), (seq(cols)[-r])[1], (seq(cols)[-r])[j]))
            vec <- rbind(vec, matrix(replicate(len[[1]], n), byrow = T, ncol = 1))
          }
          st <- st + len[[1]]
          en <- en + len[[1]]
        }
      }
      if (len[[1]] > 1){
        res <- cbind(res, vec)
        sz <- ncol(res)
      }
      if (sz < ncol(res)) sz <- ncol(res)
      #print(res)
    }
    i <- 1
    while (nrow(res[duplicated(res),]) > 0 ){
      for (i in seq(ncol(res))) res[duplicated(res),] <- append(res[duplicated(res),-i], res[duplicated(res),i], i)
      if (i == 10) {
        break
      }
      i <- I + 1
    }
    res
    #print(res)
  }
  tryCatch(
    {
      desc <- "Fails over 3 characters to permute by cbind and replicate. Checks using rbind on rows prior with while loops"
      print(desc)
      stm <- system.time(w <- perm8(test))
      cat(paste(capture.output(stm), "\n"))
      chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
      TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
    }, 
    error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# In Python with lists runs under 10secs on 10 characters. Applied same logic with for loops, rbind and anyDuplicated.matrix
permTest9 <- function(test) {
  perm9 <- function(x) {
    w <- rbind(seq(strsplit(x,"")[[1]]))
    rows <- sapply(length(w[1,]), function(z) {for (i in 1:(z-2)) z <- z + z * i; z})
    for (l in seq(rows)) {
      for (i in seq(1:(nchar(x) - 1))){
        if (l == 1 && i == 1) {
          z <- rbind(append(w[-i], w[i]))
        } else if (l == 1) {
          z <- rbind(z, append(w[-i], w[i]))
        } else {
          chk <- rbind(z, append(z[l,][-i], z[l,][i]))
          if (!anyDuplicated.matrix(chk)) {
            z <- rbind(z, append(z[l,][-i], z[l,][i]))
            if(nrow(z) == rows) {
              return(z)
            }
          }
        }
      }
      x <<- z
      Status <<- paste(round((nrow(z) / rows) * 100, 2), "%", " Commpleted: ", nrow(z), " of ", rows)
    }
    z
  }
  tryCatch({
    desc <- "In Python with lists runs under 10secs on 10 characters. Applied same logic with for loops, rbind and anyDuplicated.matrix"
    print(desc)
    stm <- system.time(w <- perm9(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Using logic from my Python App, uses data.table internal function, funion, a for loop and a while loop, not as fast as advertised
# Also Note what happens when duplicate characters are used
permTest10 <- function(test) {
  perm10 <- function(x) {
    require(data.table)
    w <- data.table(matrix(seq(strsplit(x, "")[[1]]), ncol = nchar(x)))
    rows <- sapply(length(w[1,]), function(z) {for (i in 1:(z-2)) z <- z + z * i; z})
    main <- function(s) {    
      for (i in seq(1:(nchar(s) - 1))) {
        w <<- funion(w, data.table(matrix(append(as.numeric(strsplit(s, "")[[1]])[-i],as.numeric(strsplit(s, "")[[1]])[i]), ncol = nchar(s))))
      }
      ln <<- ln + 1
    }
    ln <- 1
    while (nrow(w) < rows) {
      main(paste0(w[ln,], collapse = ""))
    }
    w
  }
  tryCatch({
    desc <- "Using logic from my Python App, uses data.table internal function, funion, a for loop and a while loop, not as fast as advertised. Also Note what happens when duplicate characters are used"
    print(desc)
    stm <- system.time(w <- perm10(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# 2nd Fastest so far not currently published. Using matrices, internal function, for loops, cbind and anyDuplicated.matrix()
permTest11 <- function(test) {
  perm11 <- function(s) {
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
              if (!anyDuplicated(append(x[i,],k))) {
                b <- k
                break
              }
            }
            if (anyDuplicated(append(x[i,],b))) {
              for (k in 1:b){
                if (!anyDuplicated(append(x[i,],k))){
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
    perms <- data.frame(
      Perms = sapply(seq(nrow(x)), function(a) paste0(
        sapply(seq(ncol(x)), function(c) paste0(alst[[x[a, c]]], collapse = "")), collapse = ""))
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
        NonUniquePermutations = nperms
      )
    } else {
      names(perms) <- s
      list(
        Total = lng,
        Permutations = perms
      )
    }
  }
  tryCatch({
    desc <- "2nd Fastest so far not currently published. Using matrices, internal function, for loops, cbind and anyDuplicated.matrix()"
    print(desc)
    stm <- system.time(w <- perm11(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w[[length(w)]])), total.rows = nrow(w[[length(w)]]), 
                characters = nchar(w[[length(w)]][1,]))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

# Using logic from my Python App, uses matrices, internal function, a for loop, a while loop, rbind and anyDuplicated.matrix()
permTest12 <- function(test) {
  perm12 <- function(x) {
    w <- matrix(seq(strsplit(x, "")[[1]]), ncol = nchar(x))
    rows <- sapply(length(w[1,]), function(z) {for (i in 1:(z-2)) z <- z + z * i; z})
    main <- function(s) {    
      for (i in seq(1:(ncol(s) - 1))) {
        chk <- rbind(w, append(s[, -i], s[, i]))
        if (!anyDuplicated.matrix(chk)) {
          w <<- rbind(w, append(s[, -i], s[, i]))
          if(nrow(w) == rows) {
            return(w)
          }
        }
      }
      ln <<- ln + 1
      
    }
    ln <- 1
    while (nrow(w) < rows) {
      main(matrix(w[ln,], ncol = nchar(x)))
    }
    w
  }
  tryCatch({
    desc <- "Using logic from my Python App, uses matrices, internal function, a for loop, a while loop, rbind and anyDuplicated.matrix()"
    print(desc)
    stm <- system.time(w <- perm12(test))
    cat(paste(capture.output(stm), "\n"))
    chk <- list(unique.rows = nrow(unique(w)), total.rows = nrow(w), characters = ncol(w))
    TestResult[nrow(TestResult),-(c(1:3,11))] <<- c(desc, chk[2], chk[1], chk[3], round(unname(stm)[1:3],2))
  }, 
  error = function(cond) TestResult[nrow(TestResult), c(4, 11)] <<- c(desc, capture.output(cond))
  )
}

RunTests <- function(tests, funcs) {
  TestResult <<- data.frame(
      "Test Name" = NA, 
      "Tested On" = NA, 
      "Characters" = NA, 
      "Description" = NA, 
      "Total Rows" = NA, 
      "Unique Rows" = NA,
      "Result Characters" = NA,
      "User"= NA, 
      "System" = NA, 
      "Elapsed" = NA, 
      "Error Reason" = NA
    )
  ln <- 1
  for (i in funcs) {
    for (test in tests){
      tNm <- paste0("permTest",i)
      TestResult[ln, 1:3] <<- c(tNm, test, nchar(test))
      cat("running Test", i, ": on character length", nchar(test[[1]]), "\n")
      cat(paste(capture.output(do.call(tNm, list(test))), "\n"))
      ln <- ln + 1
    }
  }
  View(TestResult)
}

RunTests(c("abc", "abbc", "monday"), c(1:12))

