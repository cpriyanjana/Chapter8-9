# Chapter 8 – S3 System and Custom Classes

# S3 class conversion example
value <- 1e9
print(value)

class(value) <- c("POSIXct", "POSIXt")
print(value)

# Working with attributes
attributes(deck)
row.names(deck)
levels(deck) <- c("basic", "intermediate", "advanced")
attributes(deck)

# Slot machine simulation

run_game <- function() {
  result <- get_symbols()
  print(result)
  calculate_score(result)
}

# store symbols as attribute

run_game <- function() {
  result <- get_symbols()
  prize <- calculate_score(result)
  attr(prize, "result") <- result
  prize
}

run_game()
first_try <- run_game()
first_try

# One-step prize creation with attributes

run_game <- function() {
  result <- get_symbols()
  structure(calculate_score(result), result = result)
}
second_try <- run_game()
second_try

# Display slot outcome with custom formatting

show_result <- function(prize){
  res <- attr(prize, "result")
  combined <- paste(res, collapse = " ")
  output <- paste(combined, prize, sep = "\n=> ")
  cat(output)
}
show_result(first_try)

# Exploring generic functions

print(pi)
pi
print(head(deck))
head(deck)

print(run_game())
run_game()

# Define method for a custom class

print
print.POSIXct
print.factor

# Custom print method for slot game

print.slotgame <- function(x, ...) {
  show_result(x)
}
first_try

# Add class attribute to prize result

run_game <- function() {
  result <- get_symbols()
  structure(calculate_score(result), result = result, class = "slotgame")
}
class(run_game())
run_game()

# Inspect methods associated with factor class

methods(class = "factor")

game1 <- run_game()
game1

game2 <- run_game()
game2

c(game1, game2)

game1[1]
