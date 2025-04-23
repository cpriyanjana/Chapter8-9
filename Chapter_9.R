# Chapter 9

# --- Loops and Expected Values ---

# Define die outcomes
dice <- 1:6
all_rolls <- expand.grid(dice, dice)
all_rolls

# Calculate roll values
all_rolls$total <- all_rolls$Var1 + all_rolls$Var2
head(all_rolls, 3)

# Define custom probability distribution
die_prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8,
              "4" = 1/8, "5" = 1/8, "6" = 3/8)

# Assign probabilities to each roll
all_rolls$p1 <- die_prob[as.character(all_rolls$Var1)]
all_rolls$p2 <- die_prob[as.character(all_rolls$Var2)]
all_rolls$joint_prob <- all_rolls$p1 * all_rolls$p2
head(all_rolls, 3)

# Compute expected value
sum(all_rolls$total * all_rolls$joint_prob)

# --- Slot Machine Combos ---

# Generate combinations (for symbolic wheel)
slot_wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
slot_prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
               "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

# Create full combo set
comb_set <- expand.grid(slot_wheel, slot_wheel, slot_wheel, stringsAsFactors = FALSE)

# Assign probabilities
comb_set$p1 <- slot_prob[comb_set$Var1]
comb_set$p2 <- slot_prob[comb_set$Var2]
comb_set$p3 <- slot_prob[comb_set$Var3]
comb_set$total_prob <- comb_set$p1 * comb_set$p2 * comb_set$p3
head(comb_set, 3)

# Confirm total probability = 1
sum(comb_set$total_prob)

# Sample scoring input
test_symbols <- c(comb_set[1, 1], comb_set[1, 2], comb_set[1, 3])

# Define scoring function
evaluate_spin <- function(symbols) {
  dds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  core <- symbols[symbols != "DD"]
  all_match <- length(unique(core)) == 1
  all_bars <- all(core %in% c("B", "BB", "BBB"))
  
  if (dds == 3) {
    win <- 100
  } else if (all_match) {
    payout_table <- c("7" = 80, "BBB" = 40, "BB" = 25,
                      "B" = 10, "C" = 10, "0" = 0)
    win <- unname(payout_table[core[1]])
  } else if (all_bars) {
    win <- 5
  } else if (cherries > 0) {
    win <- c(0, 2, 5)[cherries + dds + 1]
  } else {
    win <- 0
  }
  
  win * (2^dds)
}

# Score each combo
comb_set$prize <- NA
for (j in 1:nrow(comb_set)) {
  spin <- c(comb_set[j, 1], comb_set[j, 2], comb_set[j, 3])
  comb_set$prize[j] <- evaluate_spin(spin)
}

# Expected winnings
sum(comb_set$prize * comb_set$total_prob)

# --- For Loops ---

for (item in c("Loop", "one", "test", "run")) {
  print("This loop ran.")
}

for (item in c("Loop", "two", "shows", "this")) {
  print(item)
}

letters_out <- character(4)
loop_words <- c("Another", "example", "loop", "here")
for (k in seq_along(loop_words)) {
  letters_out[k] <- loop_words[k]
}
letters_out

# --- While Loop Example ---

simulate_plays <- function(start_cash) {
  funds <- start_cash
  rounds <- 0
  while (funds > 0) {
    funds <- funds - 1 + play()
    rounds <- rounds + 1
  }
  rounds
}
# simulate_plays(100)

# --- Repeat Loop Example ---

repeat_until_broke <- function(start_cash) {
  funds <- start_cash
  spins <- 0
  repeat {
    funds <- funds - 1 + play()
    spins <- spins + 1
    if (funds <= 0) break
  }
  spins
}
# repeat_until_broke(100)
