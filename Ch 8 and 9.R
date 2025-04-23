# chapter 8

# the S3 system
num <- 1000000000
print(num)

class(num) <- c("POSIXct", "POSIXt")
print(num)

# attributes
attributes(deck)
row.names(deck)
levels(deck) <- c("level 1", "level 2", "level 3")
attributes(deck)

# exercise

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

# new version of play

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}

play()
two_play <- play()
two_play

# generate prize and set its attribute in one step
play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}
three_play <- play()
three_play

# display slot results

slot_display <- function(prize){
  # extract symbols
  symbols <- attr(prize, "symbols")
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  # combine symbol with prize as a regular expression
  # \n is regular expression for new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  # display regular expression in console without quotes
  cat(string)
}
slot_display(one_play)

# generic functions
print(pi)
pi
print(head(deck))
head(deck)

print(play())
play()

# methods
print
print.POSIXct
print.factor

# exercise
print.slots <- function(x, ...) {
  slot_display(x)
}
one_play

# exercise

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}
class(play())
play()

# classes

methods(class = "factor")

play1 <- play()
play1

play2 <- play()
play2

c(play1, play2)

play1[1]
