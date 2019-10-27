setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Chapter 1
#   Functions
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
roll()

#   Arguments
roll2 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2(1:12)

roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2()

# Chapter 2
#   Library
library(ggplot2)
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE,
      prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

# Chapter 3
#   Data Frames
deck <- IPI::ipi.buildDeck()
print(file.path(deck$card[1]))

# install.packages("magick")
library(magick)
card1 <- image_read(file.path(deck$card[1]))
card2 <- image_read(file.path(deck$card[2]))
print(image_append(c(card1, card2))) # Prints the image to R's Viewer

library(rstudioapi)
viewer(file.path(deck$card[1]))

# install.packages("caTools")
library(caTools)
image(read.gif(file.path(deck$card[1]))$image)

#   Saving Data
write.csv(deck, file = '~/Source Files/csv/cards.csv', row.names = FALSE)

#   Randomizing
shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}
deck2 <- shuffle(deck)
deal(deck2)

mean(deck$value)
median(deck$value)

# Chapter 6
#   Environments
setup <- function(deck, player = c("You", "R")) {
  require(magick)
  require(stringr)
  require(stringi)
  DECK <- deck
  wins <- data.frame()
  players = length(player)
  playerNo = 1
  mLen = 260
  cWidth = 260 / 33
  iWidth = 72 * (12/72)
  for (i in str_length(player)) {
    len = round(((i + 26) / 33) * 260)
    if (len > mLen) {
      mLen = len
    }
  }

  DEAL <- function() {
    card <- deck[1,]
    assign("deck", deck[-1,], envir = parent.env(environment()))
    msg <- sprintf("%s get%s %s of %s",
                  player[playerNo],
                  ifelse (player[playerNo] %in% c('I', 'You'), '', 's'),
                  card$face,
                  card$suit
                  )
    len <- paste0('+', (mLen - (str_length(msg) * cWidth)) / 2, '+0')
    pos <-  paste0('+', (mLen - (iWidth * cWidth)) / 2, '+30')
    disp <- image_composite(image_annotate(image_blank(mLen, 180, "white"), msg, location = len, size = 15),
                    image_read(file.path(card$card)),
                    offset = pos
    )
    if (card$face == 'jack' & card$suit %in% c('spades', 'clubs')) {
      winner <- sprintf("%s win%s!",
                    player[playerNo],
                    ifelse (player[playerNo] %in% c('I', 'You'), '', 's')
                    )
      len <- paste0('+', (mLen - (str_length(winner) * cWidth)) / 2, '+0')
      print(image_composite(
        disp,
        image_annotate(image_blank(mLen, 20, "beige"), winner, location = len, size = 15),
        offset = '+0+140'
        ))
      assign("wins", rbind(wins, data.frame(Player = player[playerNo], Won = 1)), envir = parent.env(environment()))
    } else {
      print(disp)
    }
    ifelse(playerNo < players,
           assign("playerNo", playerNo + 1, envir = parent.env(environment())),
           assign("playerNo", 1, envir = parent.env(environment()))
    )
    if (card$face == 'jack' & card$suit %in% c('spades', 'clubs')) {
      SHUFFLE()
    }
  }

  SHUFFLE <- function() {
    random <- sample(1:52, size = 52)
    assign("playerNo", 1, envir = parent.env(environment()))
    assign("deck", DECK[random,], envir = parent.env(environment()))
  }

  GAMES <- function() {
    if (length(wins) > 0) {
      scores <- aggregate(wins$Won, wins, sum)[, c(1, 3)]
      colnames(scores) <- colnames(wins[, 1:2])
      scores
    } else {
      print("No Games have been finished yet!")
    }
  }

  list(deal = DEAL, shuffle = SHUFFLE, games = GAMES)
}
cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle
games <- cards$games
shuffle()
deal()


# Chapter 7
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
         )
}

get_symbols()

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
payouts
payouts["DD"]
unname(payouts["DD"]) # returns a copy of the object with the names attribute removed

score <- function(symbols) {
  # identify case
  # same <- symbols[1] == symbols[2] && symbols[3]
  # Or
  same <- all(symbols == symbols[1])
  bars <- symbols %in% c("B", "BB", "BBB")
  
  # get prize
  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) {
    prize <- 5
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries + 1]
  }
  
  # adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds
}

play()

# Chapter 8
one_play <- play()
one_play
attributes(one_play)
attr(one_play, "symbols") <- c("B", "0", "B")
attributes(one_play)
attr(one_play, "symbols")
one_play
one_play + 1

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}

play()
two_play <- play()
two_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}
three_play <- play()
three_play

slot_display <- function(prize) {
  
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  
  # combine symbol with prize as a regular expression
  # \n is regular expression for new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n£")
  
  # display regular expression console without quotes
  cat(string)
}
slot_display(one_play)

methods(print)
class(one_play)
class(one_play) <- "slots"
print.slots <- function(x, ...) {
  cat("I'm using the print.slots method")
}
print(one_play)
one_play

now <- Sys.time()
attributes(now)

print.slots <- function(x, ...) {
  slot_display(x)
}
one_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}
class(play())
play()

play1 <- play()
play2 <- play()
c(play1, play2)
play1[1]


# Chapter 9
die <- c(1, 2, 3, 4, 5, 6)
rolls <- expand.grid(die, die)
rolls
rolls$value <-rolls$Var1 + rolls$Var2
head(rolls, 3)

prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8, "6" = 3/8)
prob

rolls$Var1
prob[rolls$Var1]
rolls$prob1 <- prob[rolls$Var1]
head(rolls, 3)

rolls$prob2 <- prob[rolls$Var2]
head(rolls, 3)

rolls$prob <- rolls$prob1 * rolls$prob2
head(rolls, 3)
sum(rolls$value * rolls$prob)

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
         )
}
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
head(combos, 3)
sum(combos$prob)
symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])
score(symbols)

# Loops
combos$prize <- NA
head(combos, 3)

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
head(combos, 3)
sum(combos$prize * combos$prob)

score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  # identify case
  # since diamonds are wild, only nondiamonds
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cheries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2 ^ diamonds
}
sum(combos$prize * combos$prob)

# While loops
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash -1 + play()
    n <- n + 1
  }
  n
}
plays_till_broke(100)

# repeat loops
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}
plays_till_broke(100)



# Chapter 10 
#     Vectorized Code
abs_loop <- function(vec) {
  for (i in 1:length(vec)) {
    if (vec[i] <0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

abs_set <- function(vec) {
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}

long <- rep(c(-1, 1), 5000000)
system.time(abs_loop(long))
system.time(abs_set(long))
system.time(abs(long))

# Vectorized Functions
change_symbols <- function(vec) {
  for (i in 1:length(vec)) {
    if (vec[i] == 'DD') {
      vec[i] <- 'joker'
    } else if (vec[i] == 'C') {
      vec[i] <- 'ace'
    } else if (vec[i] == '7') {
      vec[i] <- 'king'
    } else if(vec[i] == 'B') {
      vec[i] <- 'queen'
    } else if (vec[i] == 'BB') {
      vec[i] <- 'jack'
    } else if (vec[i] == 'BBB') {
      vec[i] <- 'ten'
    } else {
      vec[i] <- 'nine'
    }
  }
  vec
}
vec <- c('DD', 'C', '7', 'B', 'BB', 'BBB', '0')
change_symbols(vec)
many <- rep(vec, 1000000)
system.time(change_symbols(many))

change_vec <- function(vec) {
  vec[vec == 'DD'] <- 'joker'
  vec[vec == 'C'] <- 'ace'
  vec[vec == '7'] <- 'king'
  vec[vec == 'B'] <- 'queen'
  vec[vec == 'BB'] <- 'jack'
  vec[vec == 'BBB'] <- 'ten'
  vec[vec == '0'] <- 'nine'
  
  vec
}
system.time(change_vec(many))

change_vec2 <- function(vec) {
  tb <- c('DD' = 'joker', 'C' = 'ace', '7' = 'king', 'B' = 'queen', 'BB' = 'jack', 'BBB' = 'ten', '0' = 'nine')
  unname(tb[vec])
}
system.time(change_vec2(many))

system.time(
  {
    output <- rep(NA, 1000000)
    for (i in 1:1000000) {
      output[i] <- i + 1
    }
  }
)

system.time(
  {
    output <- NA
    for (i in 1:1000000) {
      output[i] <- i + 1
    }
  }
)

winnings <- vector(length = 1000000)
for (i in 1:1000000) {
  winnings[i] <- play()
}
mean(winnings)

system.time(for (i in 1:1000000) {
  winnings[i] <- play()
}
)

get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE, 
                prob = c(0.03, 0.03, 0.06, 0.1, 0.2, 0.01, 0.52))
  matrix(vec, ncol = 3)
}
get_many_symbols(5)

play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[, 1], w2 = symb_mat[, 2], w3 = symb_mat[, 3], prize = score_many(symb_mat))
}
plays <- play_many(1000000)
mean(plays$prize)

# Error check
sum(plays$prize[!is.na(plays$prize)])
mean(plays$prize[!is.na(plays$prize)])
length(plays$prize[is.na(plays$prize)])
plays[is.na(plays$prize),]
plays[rowSums(plays[, 1:3] == "DD") == 1 & rowSums(plays[, 1:3] == "C") == 2, ]
plays[rowSums(plays[, 1:3] == "DD") == 2 & rowSums(plays[, 1:3] == "C") == 1, ]
rowSums(plays[is.na(plays$prize),][1, 1:3] == "C")
rowSums(plays[is.na(plays$prize),][1, 1:3] == "DD")

symbols <- matrix(
  c(
    "DD", "DD", "DD",
    "C", "DD", "0", 
    "B", "B", "B", 
    "B", "BB", "BBB", 
    "C", "C", "0", 
    "7", "DD", "DD"
  ), nrow = 6, byrow = TRUE
)

symbols

# symbols should be a matrix with a column for each slot machine window
score_many <- function(symbols) {

  # Step 1: Assign base prize based on cherries and diamonds --------
  # Count the number of cherries and diamonds in each combination
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD")
  
  ## wild diamonds count as cherries
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  
  # ... but not if there are zero real cherries
  # (cherries is coerced to FALSE where cherries == 0)
  prize[!cherries] <- 0
  
  # Step 2: Change prize for combinations that contain three of a kind
  same <- symbols[, 1] == symbols[, 2] & symbols[, 2] == symbols[, 3]
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  # Step 3: Change prize for combinations that contain all bars ------
  bars <- symbols == "B" | symbols == "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars]
  
  # Step 4: Handle wilds ---------------------------------------------
  
  # combos with two diamonds 
  two_wilds <- diamonds == 2
  
  # Identify the nonwild symbol
  one <- two_wilds & symbols[, 1] != symbols[, 2] & symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] & symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] & symbols[, 2] != symbols[, 3]
  
  # Treat as three of a kind
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  # combos with one wild
  one_wild <- diamonds == 1
  
  # Treat as all bars (if appropriate)
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  # Treat as three of a kind (if appropriate)
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  # Step 5: Double prize for every diamond in combo -------------------------
  unname(prize * 2 ^ diamonds)
}

system.time(play_many(10000000))
