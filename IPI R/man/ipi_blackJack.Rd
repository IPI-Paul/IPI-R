<<<<<<< HEAD
\name{ipi.blackJack}
\alias{ipi.blackJack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.blackJack
}
\description{
Play Blackjack with as many users as you like!
}
\usage{
ipi.blackJack(list(usernames))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{
Provide names of users as a list (i.e Paul, Shirley.....). Program automatically adds Dealer.
}
}
\details{
Requires the following packages: magick, stringr and stringi
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{shuffle}{
  Shuffles the deck and deals a new hand to all Players faced up except for the Dealer whose cards are left face down
  until all other players have stuck
  }
  \item{deal}{
  deal(playerNumber) : Deals that player a card until the specify stick(playerNumber) or go Bust!
  }
  \item{stick}{
  stick(playerNumber): Singals no further deals needed by the player and ensures that Dealer finishes the round
  }
  \item{games}{
  games()            : Displays a summary of games Won by Players
  }
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul I Ighofose
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- With deal and stick supply a player number, but deafaults to 1st Player if no number supplied.
cards <- ipi.blackJack(c("Paul", "Other"))
play <- cards$play
stick <- cards$stick
shuffle <- cards$shuffle
deal <- cards$deal
games <- cards$games
deal()
stick()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
=======
\name{ipi.blackJack}
\alias{ipi.blackJack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.blackJack
}
\description{
Play Blackjack with as many users as you like!
}
\usage{
ipi.blackJack(list(usernames))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{
Provide names of users as a list (i.e Paul, Shirley.....). Program automatically adds Dealer.
}
}
\details{
Requires the following packages: magick, stringr and stringi
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{shuffle}{
  Shuffles the deck and deals a new hand to all Players faced up except for the Dealer whose cards are left face down
  until all other players have stuck
  }
  \item{deal}{
  deal(playerNumber) : Deals that player a card until the specify stick(playerNumber) or go Bust!
  }
  \item{stick}{
  stick(playerNumber): Singals no further deals needed by the player and ensures that Dealer finishes the round
  }
  \item{games}{
  games()            : Displays a summary of games Won by Players
  }
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul I Ighofose
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- With deal and stick supply a player number, but deafaults to 1st Player if no number supplied.
cards <- ipi.blackJack(c("Paul", "Other"))
play <- cards$play
stick <- cards$stick
shuffle <- cards$shuffle
deal <- cards$deal
games <- cards$games
deal()
stick()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
>>>>>>> 5863e6f785c1b85928f7351c64dd57201babf032
