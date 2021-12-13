# Advent of Code 2021

# https://adventofcode.com/

# Challenge #1.1 ----

# How many measurements are larger than the previous measurement?

oceandeep   <- read.table("inputs/input_d01.txt")
n_cases     <- nrow(oceandeep)
differences <- diff(oceandeep$V1)
result      <- sum(differences > 0)
result
rm(differences, result)

# Challenge #1.2 ----

# Consider sums of a three-measurement sliding window. How many sums are larger 
# than the previous sum?

odeep_aggre <- sum(oceandeep$V1[1:3])

for (i in 2:(n_cases - 2)) {
  odeep_aggre[i] <- sum(oceandeep$V1[i:(i+2)])
}
rm(i)

# Same result but without a loop.

odeep_aggre <- oceandeep$V1[1:(n_cases - 2)] + 
               oceandeep$V1[2:(n_cases - 1)] + 
               oceandeep$V1[3:(n_cases)]

differences <- diff(odeep_aggre)
result      <- sum(differences > 0)
result

rm(list = ls())

# Challenge #2.1 ----

# Your horizontal position and depth both start at 0. The steps above would then
# modify them as follows:
#     forward 5 adds 5 to your horizontal position, a total of 5.
#     down 5 adds 5 to your depth, resulting in a value of 5.
#     forward 8 adds 8 to your horizontal position, a total of 13.
#     up 3 decreases your depth by 3, resulting in a value of 2.
#     down 8 adds 8 to your depth, resulting in a value of 10.
#     forward 2 adds 2 to your horizontal position, a total of 15.
# After following these instructions, you would have a horizontal position of 15 
# and a depth of 10. (Multiplying these together produces 150.)

# Calculate the horizontal position and depth you would have after following the 
# planned course. What do you get if you multiply your final horizontal position 
# by your final depth?

movement <- read.table("inputs/input_d02.txt") 
n_cases  <- nrow(movement)
mov_sum  <- tapply(movement$V2, movement$V1, sum)

result   <- mov_sum["forward"] * (mov_sum["down"] - mov_sum["up"])
result

rm(result, mov_sum)

# Challenge #2.2 ----

# In addition to horizontal position and depth, you'll also need to track a 
# third value, aim, which also starts at 0. The commands also mean something 
# entirely different than you first thought:
#     down X increases your aim by X units.
#     up X decreases your aim by X units.
#     forward X does two things:
#         It increases your horizontal position by X units.
#         It increases your depth by your aim multiplied by X.
# Again note that since you're on a submarine, down and up do the opposite of 
# what you might expect: "down" means aiming in the positive direction.
# Now, the above wires[i, ] does something different:
#     forward 5 adds 5 to your horizontal position, a total of 5. Because your 
#         aim is 0, your depth does not change.
#     down 5 adds 5 to your aim, resulting in a value of 5.
#     forward 8 adds 8 to your horizontal position, a total of 13. Because your 
#         aim is 5, your depth increases by 8*5=40.
#     up 3 decreases your aim by 3, resulting in a value of 2.
#     down 8 adds 8 to your aim, resulting in a value of 10.
#     forward 2 adds 2 to your horizontal position, a total of 15. Because your 
#         aim is 10, your depth increases by 2*10=20 to a total of 60.
# After following these new instructions, you would have a horizontal position 
# of 15 and a depth of 60. (Multiplying these produces 900.)
# Using this new interpretation of the commands, calculate the horizontal 
# position and depth you would have after following the planned course. What do 
# you get if you multiply your final horizontal position by your final depth?

aim <- depth <- position <- 0

for (i in 1:n_cases) {
  if (movement$V1[i] == "forward") {
    position <- position + movement$V2[i]
    depth    <- depth + aim * movement$V2[i]
  }
  if (movement$V1[i] == "down") {
    aim <- aim + movement$V2[i]
  }
  if (movement$V1[i] == "up") {
    aim <- aim - movement$V2[i]
  }
}
rm(i)

result <- position * depth
result

rm(list = ls())

# Challenge #3.1 ----

# Considering only the first bit of each number, there are five 0 bits and seven 
# 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.
# The most common second bit of the numbers in the diagnostic report is 0, so 
# the second bit of the gamma rate is 0.
# The most common value of the third, fourth, and fifth bits are 1, 1, and 0, 
# respectively, and so the final three bits of the gamma rate are 110.
# So, the gamma rate is the binary number 10110, or 22 in decimal.
# The epsilon rate is calculated in a similar way; rather than use the most 
# common bit, the least common bit from each position is used. So, the epsilon 
# rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon 
# rate (9) produces the power consumption, 198.
# Use the binary numbers in your diagnostic report to calculate the gamma rate 
# and epsilon rate, then multiply them together. What is the power consumption 
# of the submarine? (Be sure to represent your answer in decimal, not binary.)

library(compositions)

n_bytes    <- nchar(readLines("inputs/input_d03.txt", 1))
powerdata  <- read.fwf("inputs/input_d03.txt", rep(1, n_bytes))
n_cases    <- nrow(powerdata)
countbytes <- apply(powerdata, 2, sum)

gamma_rate <- paste0(ifelse(countbytes > (n_cases/2), 1, 0), collapse = "")
epsil_rate <- paste0(ifelse(countbytes > (n_cases/2), 0, 1), collapse = "")

result <- unbinary(gamma_rate) * unbinary(epsil_rate)
result

rm(countbytes, gamma_rate, epsil_rate, result)

# Challenge #3.2 ----

# Next, you should verify the life support rating, which can be determined by 
# multiplying the oxygen generator rating by the CO2 scrubber rating.
# Both the oxygen generator rating and the CO2 scrubber rating are values that 
# can be found in your diagnostic report - finding them is the tricky part. Both 
# values are located using a similar process that involves filtering out values 
# until only one remains. Before searching for either rating value, start with 
# the full list of binary numbers from your diagnostic report and consider just 
# the first bit of those numbers. Then:
#     Keep only numbers selected by the bit criteria for the type of rating 
#          value for which you are searching. Discard numbers which do not match 
#          the bit criteria.
#     If you only have one number left, stop; this is the rating value for which 
#          you are searching.
#     Otherwise, repeat the process, considering the next bit to the right.
# The bit criteria depends on which type of rating value you want to find:
#     To find oxygen generator rating, determine the most common value (0 or 1) 
#         in the current bit position, and keep only numbers with that bit in 
#         that position. If 0 and 1 are equally common, keep values with a 1 in 
#         the position being considered.
#     To find CO2 scrubber rating, determine the least common value (0 or 1) in 
#         the current bit position, and keep only numbers with that bit in that 
#         position. If 0 and 1 are equally common, keep values with a 0 in the 
#         position being considered.
# For wires[i, ], to determine the oxygen generator rating value using the same 
# wires[i, ] diagnostic report from above:
#     Start with all 12 numbers and consider only the first bit of each number. 
#         There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers 
#         with a 1 in the first position: 11110, 10110, 10111, 10101, 11100, 
#         10000, and 11001.
#     Then, consider the second bit of the 7 remaining numbers: there are more 
#         0 bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in the 
#         second position: 10110, 10111, 10101, and 10000.
#     In the third position, three of the four numbers have a 1, so keep those 
#         three: 10110, 10111, and 10101.
#     In the fourth position, two of the three numbers have a 1, so keep those 
#         two: 10110 and 10111.
#     In the fifth position, there are an equal number of 0 bits and 1 bits (one 
#         each). So, to find the oxygen generator rating, keep the number with a 
#         1 in that position: 10111.
#     As there is only one number left, stop; the oxygen generator rating is 
#         10111, or 23 in decimal.
# Then, to determine the CO2 scrubber rating value from the same wires[i, ] above:
#     Start again with all 12 numbers and consider only the first bit of each 
#         number. There are fewer 0 bits (5) than 1 bits (7), so keep only the 5 
#         numbers with a 0 in the first position: 00100, 01111, 00111, 00010, 
#         and 01010.
#     Then, consider the second bit of the 5 remaining numbers: there are fewer 
#         1 bits (2) than 0 bits (3), so keep only the 2 numbers with a 1 in the 
#         second position: 01111 and 01010.
#     In the third position, there are an equal number of 0 bits and 1 bits (one 
#         each). So, to find the CO2 scrubber rating, keep the number with a 0 
#         in that position: 01010.
#     As there is only one number left, stop; the CO2 scrubber rating is 01010, 
#         or 10 in decimal.
# Finally, to find the life support rating, multiply the oxygen generator rating 
# (23) by the CO2 scrubber rating (10) to get 230.
# Use the binary numbers in your diagnostic report to calculate the oxygen 
# generator rating and CO2 scrubber rating, then multiply them together. What is 
# the life support rating of the submarine? (Be sure to represent your answer in 
# decimal, not binary.)

oxygen_rate <- powerdata

i <- 0
while (nrow(oxygen_rate) != 1) {
  i <- i + 1
  countbytes <- table(oxygen_rate[, i])
  if (diff(countbytes) != 0) {
    more <- as.numeric(names(which.max(table(oxygen_rate[, i]))))
  } else {
    more <- 1
  }
  oxygen_rate <- oxygen_rate[oxygen_rate[, i] == more, ]
}
rm(i, countbytes, more)

co2_rate <- powerdata

i <- 0
while (nrow(co2_rate) != 1) {
  i <- i + 1
  countbytes <- table(co2_rate[, i])
  if (diff(countbytes) != 0) {
    less <- as.numeric(names(which.min(table(co2_rate[, i]))))
  } else {
    less <- 0
  }
  co2_rate <- co2_rate[co2_rate[, i] == less, ]
}
rm(i, countbytes, less)

oxygen_rate <- paste0(oxygen_rate, collapse = "")
co2_rate    <- paste0(co2_rate, collapse = "")

result <- unbinary(oxygen_rate) * unbinary(co2_rate)
result

rm(list = ls())
detach("package:compositions")

# Challege #4.1 ----

# At this point, the third board wins because it has at least one complete row or 
# column of marked numbers (in this case, the entire top row is marked: 14 21 17 
# 24 4).
# The score of the winning board can now be calculated. Start by finding the sum 
# of all unmarked numbers on that board; in this case, the sum is 188. Then, 
# multiply that sum by the number that was just called when the board won, 24, 
# to get the final score, 188 * 24 = 4512.

# Read drawn numbers
tmp   <- tempfile()
cat(readLines("inputs/input_d04.txt", n = 1), "\n", file = tmp)
draws <- c(as.matrix(read.table(tmp, sep = ",")))
unlink(tmp)
rm(tmp)

# Read boards
boards_stacked <- read.table("inputs/input_d04.txt", skip = 1)
n_rows         <- nrow(boards_stacked)
n_cols         <- ncol(boards_stacked)
n_boards       <- n_rows / n_cols
board_ind      <- rep(1:n_boards, each = n_cols)

boards      <- t(boards_stacked)
dim(boards) <- c(n_cols, n_cols, n_boards)
boards      <- aperm(boards, perm = c(2, 1, 3))
rm(boards_stacked, board_ind, n_rows)

bingo       <- replicate(n_boards, matrix(0, 5, 5))

for (i in 1:length(draws)) {
  bingo[which(boards == draws[i])] <- 1
  if (any(apply(bingo, c(1, 3), sum) == n_cols) |
      any(apply(bingo, c(2, 3), sum) == n_cols)){
    break
  }
}

winner <- which(apply(bingo, 3, function(x) {
  any(apply(x, 1, sum) == n_cols) | any(apply(x, 2, sum) == n_cols)
  }) == TRUE)

result <- sum(boards[,, winner][bingo[,, winner] == 0]) * draws[i]
result

rm(i, winner)

# Challenge #4.2 ----

# On the other hand, it might be wise to try a different strategy: let the giant 
# squid win.
# You aren't sure how many bingo boards a giant squid could play at once, so 
# rather than waste time counting its arms, the safe thing to do is to figure 
# out which board will win last and choose that one. That way, no matter which 
# boards it picks, it will win for sure.
# In the above wires[i, ], the second board is the last to win, which happens after 
# 13 is eventually called and its middle column is completely marked. If you 
# were to keep playing until this point, the second board would have a sum of 
# unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
# Figure out which board will win last. Once it wins, what would its final score 
# be?

bingo       <- replicate(n_boards, matrix(0, 5, 5))
win_order   <- NULL

for (i in 1:length(draws)) {
  bingo[which(boards == draws[i])] <- 1
  
  winners <- which(apply(bingo, 3, function(x) {
    any(apply(x, 1, sum) == n_cols) | any(apply(x, 2, sum) == n_cols)
  }) == TRUE)
  
  win_order <- c(win_order, setdiff(winners, win_order))
  
  if (length(win_order) == n_boards) {
    break
    }
}

loser <- tail(win_order, 1)

result <- sum(boards[,, loser][bingo[,, loser] == 0]) * draws[i]
result

rm(list = ls())

# Challenge 5.1 ----

# You come across a field of hydrothermal vents on the ocean floor! These vents 
# constantly produce large, opaque clouds, so it would be best to avoid them if 
# possible.
# They tend to form in lines; the submarine helpfully produces a list of nearby 
# lines of vents (your puzzle input) for you to review. For wires[i, :
# Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 
# where x1,y1 are the coordinates of one end the line segment and x2,y2 are the 
# coordinates of the other end. These line segments include the points at both 
# ends. In other words:
#   An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
# An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
# For now, only consider horizontal and vertical lines: lines where either
# x1 = x2 or y1 = y2.
# So, the horizontal and vertical lines from the above list would produce the 
# following diagram:
# In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. 
# Each position is shown as the number of lines which cover that point or . if 
# no line covers that point. The top-left pair of 1s, for wires[i, ], comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
# To avoid the most dangerous areas, you need to determine the number of points 
# where at least two lines overlap. In the above wires[i, ], this is anywhere in 
# the diagram with a 2 or larger - a total of 5 points.
# Consider only horizontal and vertical lines. At how many points do at least 
# two lines overlap?
tmp   <- tempfile()
writeLines(gsub(" -> ", ",", readLines("inputs/input_d05.txt")), con = tmp)
vents <- read.table(tmp, sep = ",")
unlink(tmp)
rm(tmp)

# Select horizontal and vertical lines.

hvlines <- vents[, 1] == vents[, 3] | vents[, 2] == vents[, 4]

ventshv <- vents[hvlines, ]
n_vents <- nrow(ventshv)
maxvent <- max(ventshv)

field     <- matrix(0, maxvent, maxvent)

for (i in 1:n_vents){
  field_tmp <- matrix(0, maxvent, maxvent)
  if (ventshv[i, 1] == ventshv[i, 3]) {
    field_tmp[ventshv[i, 2]:ventshv[i, 4], ventshv[i, 1]] <- 1
  } else {
    field_tmp[ventshv[i, 2], ventshv[i, 1]:ventshv[i, 3]] <- 1
  }
  field <- field + field_tmp
}
rm(i, field_tmp)

result <- sum(field >= 2)
result

rm(field, ventshv, hvlines, maxvent, n_vents, result)

# Challenge #5.2 ----

# Unfortunately, considering only horizontal and vertical lines doesn't give you 
# the full picture; you need to also consider diagonal lines.
# Because of the limits of the hydrothermal vent mapping system, the lines in 
# your list will only ever be horizontal, vertical, or a diagonal line at 
# exactly 45 degrees. In other words:
# An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
# An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
# Considering all lines from the above wires[i, ] would now produce the following 
# diagram:
# You still need to determine the number of points where at least two lines 
# overlap. In the above wires[i, ], this is still anywhere in the diagram with a 2 
# or larger - now a total of 12 points.
# Consider all of the lines. At how many points do at least two lines overlap?

n_vents <- nrow(vents)
maxvent <- max(vents)

field     <- matrix(0, maxvent, maxvent)

for (i in 1:n_vents){
  field_tmp   <- matrix(0, maxvent, maxvent)
  coordinates <- cbind(vents[i, 2]:vents[i, 4], 
                       vents[i, 1]:vents[i, 3])
  field_tmp[coordinates] <- 1
  field       <- field + field_tmp
}
rm(i, field_tmp, coordinates)

result <- sum(field >= 2)
result

rm(list = ls())

# Challenge #6.1 ----

# The sea floor is getting steeper. Maybe the sleigh keys got carried this way?
# A massive school of glowing lanternfish swims past. They must spawn quickly to
# reach such large numbers - maybe exponentially quickly? You should model their 
# growth rate to be sure.
# Although you know nothing about this specific species of lanternfish, you make 
# some guesses about their attributes. Surely, each lanternfish creates a new 
# lanternfish once every 7 days.
# However, this process isn't necessarily synchronized between every lanternfish 
# - one lanternfish might have 2 days left until it creates another lanternfish, 
# while another might have 4. So, you can model each fish as a single number 
# that represents the number of days until it creates a new lanternfish.
# Furthermore, you reason, a new lanternfish would surely need slightly longer 
# before it's capable of producing more lanternfish: two more days for its first 
# cycle.
# So, suppose you have a lanternfish with an internal timer value of 3:
#     After one day, its internal timer would become 2.
#     After another day, its internal timer would become 1.
#     After another day, its internal timer would become 0.
#     After another day, its internal timer would reset to 6, and it would 
#         create a new lanternfish with an internal timer of 8.
#     After another day, the first lanternfish would have an internal timer of 
#         5, and the second lanternfish would have an internal timer of 7.
# A lanternfish that creates a new fish resets its timer to 6, not 7 (because 0 
# is included as a valid timer value). The new lanternfish starts with an 
# internal timer of 8 and does not start counting down until the next day.
# Realizing what you're trying to do, the submarine automatically produces a 
# list of the ages of several hundred nearby lanternfish (your puzzle input). 
# For wires[i, ], suppose you were given the following list:
# 3,4,3,1,2
# This list means that the first fish has an internal timer of 3, the second 
# fish has an internal timer of 4, and so on until the fifth fish, which has an 
# internal timer of 2. Simulating these fish over several days would proceed as 
# follows:
# Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each 
# other number decreases by 1 if it was present at the start of the day.
# In this wires[i, ], after 18 days, there are a total of 26 fish. After 80 days, 
# there would be a total of 5934.
# Find a way to simulate lanternfish. How many lanternfish would there be after 
# 80 days?

lanternfish <- read.table("inputs/input_d06.txt", sep = ",")
lanternfish <- c(as.matrix(lanternfish))

lf_pop      <- lanternfish
days        <- 80

i <-  0
repeat {
  i <- i + 1
  if (i == days) {
    break
  }
  lf_pop              <- lf_pop - 1
  new_fish            <- sum(lf_pop == 0)
  lf_pop[lf_pop == 0] <- 7
  lf_pop              <- c(lf_pop, rep(9, new_fish))
}
rm(new_fish, i)

result <- length(lf_pop)
result

rm(days, lf_pop, result)

# Challenge 6.2 ----

# Suppose the lanternfish live forever and have unlimited food and space. Would 
# they take over the entire ocean?
# After 256 days in the wires[i, ] above, there would be a total of 26984457539 
# lanternfish!
# How many lanternfish would there be after 256 days?


#!# Don't run #!#
#!# While this probably gives the result, it requires an obscene amount of RAM.
lf_pop      <- lanternfish
days        <- 256

i <-  0
repeat {
  i <- i + 1
  if (i == days) {
    break
  }
  lf_pop              <- lf_pop - 1
  new_fish            <- sum(lf_pop == 0)
  lf_pop[lf_pop == 0] <- 7
  lf_pop              <- c(lf_pop, rep(9, new_fish))
}
rm(new_fish, i)

result <- length(lf_pop)
result

#!# I checked some tips in Reddit. The trick is to work with groups of fish 
#!# instead of with each fish at a time.

lanternfish <- factor(lanternfish, levels = 0:8)

lf_pop <- as.numeric(table(lanternfish))
days   <- 256

i <-  0
repeat {
  i <- i + 1
  tmp <- lf_pop[c(2:9)]
  tmp[7] <- tmp[7] + lf_pop[1]
  lf_pop <- c(tmp, lf_pop[1])
  lf_pop
  if (i == days) {
    break
  }
}

result <- sum(lf_pop)
sprintf("%1.0f", result)

rm(list = ls())

# Challenge 7.1 ----

# A giant whale has decided your submarine is its next meal, and it's much 
# faster than you are. There's nowhere to run!
# Suddenly, a swarm of crabs (each in its own tiny submarine - it's too deep for 
# them otherwise) zooms in to rescue you! They seem to be preparing to blast a 
# hole in the ocean floor; sensors indicate a massive underground cave system 
# just beyond where they're aiming!
# The crab submarines all need to be aligned before they'll have enough power to 
# blast a large enough hole for your submarine to get through. However, it 
# doesn't look like they'll be aligned before the whale catches you! Maybe you 
# can help?
# There's one major catch - crab submarines can only move horizontally.
# You quickly make a list of the horizontal position of each crab (your puzzle 
# input). Crab submarines have limited fuel, so you need to find a way to make 
# all of their horizontal positions match while requiring them to spend as little 
# fuel as possible.
# For wires[i, ], consider the following horizontal positions:
# 16,1,2,0,4,2,7,1,2,14
# This means there's a crab with horizontal position 16, a crab with horizontal 
# position 1, and so on.
# Each change of 1 step in horizontal position of a single crab costs 1 fuel. 
# You could choose any horizontal position to align them all on, but the one 
# that costs the least fuel is horizontal position 2:
# Move from 16 to 2: 14 fuel
# Move from 1 to 2: 1 fuel
# Move from 2 to 2: 0 fuel
# Move from 0 to 2: 2 fuel
# Move from 4 to 2: 2 fuel
# Move from 2 to 2: 0 fuel
# Move from 7 to 2: 5 fuel
# Move from 1 to 2: 1 fuel
# Move from 2 to 2: 0 fuel
# Move from 14 to 2: 12 fuel
# This costs a total of 37 fuel. This is the cheapest possible outcome; more 
# expensive outcomes include aligning at position 1 (41 fuel), position 3 (39 
# fuel), or position 10 (71 fuel).
# Determine the horizontal position that the crabs can align to using the least 
# fuel possible. How much fuel must they spend to align to that position?

crabs <- read.table("inputs/input_d07.txt", sep = ",")
crabs <- c(as.matrix(crabs))

n_crabs  <- length(crabs)
max_crab <- max(crabs)

fuel <- matrix(1:max_crab, byrow = TRUE, nrow = n_crabs, ncol = max_crab)
fuel <- abs(crabs - fuel)

fuel_consumed <- apply(fuel, 2, sum)

result <- min(fuel_consumed)
result

rm(fuel_consumed, result)

# Challenge 7.2 ----

# The crabs don't seem interested in your proposed solution. Perhaps you 
# misunderstand crab engineering?
# As it turns out, crab submarine engines don't burn fuel at a constant rate. 
# Instead, each change of 1 step in horizontal position costs 1 more unit of 
# fuel than the last: the first step costs 1, the second step costs 2, the third 
# step costs 3, and so on.
# As each crab moves, moving further becomes more expensive. This changes the 
# best horizontal position to align them all on; in the wires[i, ] above, this 
# becomes 5:
# Move from 16 to 5: 66 fuel
# Move from 1 to 5: 10 fuel
# Move from 2 to 5: 6 fuel
# Move from 0 to 5: 15 fuel
# Move from 4 to 5: 1 fuel
# Move from 2 to 5: 6 fuel
# Move from 7 to 5: 3 fuel
# Move from 1 to 5: 10 fuel
# Move from 2 to 5: 6 fuel
# Move from 14 to 5: 45 fuel
# This costs a total of 168 fuel. This is the new cheapest possible outcome; 
# the old alignment position (2) now costs 206 fuel instead.
# Determine the horizontal position that the crabs can align to using the least 
# fuel possible so they can make you an escape route! How much fuel must they 
# spend to align to that position?

totalfuel     <- apply(fuel, c(1, 2), function(x) sum(0:x))

fuel_consumed <- apply(totalfuel, 2, sum)

result <- min(fuel_consumed)
result

rm(list = ls())

# Challenge 8.1 ----

# You barely reach the safety of the cave when the whale smashes into the cave 
# mouth, collapsing it. Sensors indicate another exit to this cave at a much 
# greater depth, so you have no choice but to press on.
# As your submarine slowly makes its way through the cave system, you notice 
# that the four-digit seven-segment displays in your submarine are malfunctioning;
# they must have been damaged during the escape. You'll be in a lot of trouble 
# without them, so you'd better figure out what's wrong.
# Each digit of a seven-segment display is rendered by turning on or off any of 
# seven segments named a through g:
# So, to render a 1, only segments c and f would be turned on; the rest would be
# off. To render a 7, only segments a, c, and f would be turned on.
# The problem is that the signals which control the segments have been mixed up 
# on each display. The submarine is still trying to display numbers by producing 
# output on signal wires a through g, but those wires are connected to segments 
# randomly. Worse, the wire/segment connections are mixed up separately for each 
# four-digit display! (All of the digits within a display use the same 
# connections, though.)
# So, you might know that only signal wires b and g are turned on, but that 
# doesn't mean segments b and g are turned on: the only digit that uses two 
# segments is 1, so it must mean segments c and f are meant to be on. With just 
# that information, you still can't tell which wire (b/g) goes to which segment 
# (c/f). For that, you'll need to collect more information.
# For each display, you watch the changing signals for a while, make a note of 
# all ten unique signal patterns you see, and then write down a single four 
# digit output value (your puzzle input). Using the signal patterns, you should 
# be able to work out which pattern corresponds to which digit.
# For wires[i, ], here is what you might see in a single entry in your notes:
# acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
# cdfeb fcadb cdfeb cdbaf
# (The entry is wrapped here to two lines so it fits; in your notes, it will all 
# be on a single line.)
# Each entry consists of ten unique signal patterns, a | delimiter, and finally 
# the four digit output value. Within an entry, the same wire/segment connections 
# are used (but you don't know what the connections actually are). The unique 
# signal patterns correspond to the ten different ways the submarine tries to 
# render a digit using the current wire/segment connections. Because 7 is the 
# only digit that uses three segments, dab in the above wires[i, ] means that to 
# render a 7, signal lines d, a, and b are on. Because 4 is the only digit that 
# uses four segments, eafb means that to render a 4, signal lines e, a, f, and b 
# are on.
# Using this information, you should be able to work out which combination of 
# signal wires corresponds to each of the ten digits. Then, you can decode the 
# four digit output value. Unfortunately, in the above wires[i, ], all of the digits 
# in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more 
# difficult to deduce.
# 
# For now, focus on the easy digits. Consider this larger wires[i, :
# 
# Because the digits 1, 4, 7, and 8 each use a unique number of segments, you 
# should be able to tell which combinations of signals correspond to those digits. 
# Counting only digits in the output values (the part after | on each line), 
# in the above wires[i, ], there are 26 instances of digits that use a unique number 
# of segments (highlighted above).
# 
# In the output values, how many times do digits 1, 4, 7, or 8 appear?

wires <- read.table("inputs/input_d08.txt")
wires <- wires[, -11]
names(wires) <- c(paste0("digit", 0:9), paste0("output", 1:4))

wires_output <- wires[, 11:14]

segments <- apply(wires_output, c(1, 2), nchar)

result <- sum(segments %in% c(2, 3, 4, 7))
result

rm(wires_output, result, segments)

# Challenge 8.2 ----

# Through a little deduction, you should now be able to determine the remaining 
# digits. Consider again the first wires[i, ] above:
#   acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#   cdfeb fcadb cdfeb cdbaf
# After some careful analysis, the mapping between signal wires and segments 
# only make sense in the following configuration:
# So, the unique signal patterns would correspond to the following digits:
# acedgfb: 8
# cdfbe: 5
# gcdfa: 2
# fbcad: 3
# dab: 7
# cefabd: 9
# cdfgeb: 6
# eafb: 4
# cagedb: 0
# ab: 1
# Then, the four digits of the output value can be decoded:
# cdfeb: 5
# fcadb: 3
# cdfeb: 5
# cdbaf: 3
# Therefore, the output value for this entry is 5353.
# Following this same process for each entry in the second, larger wires[i, ] above,
# the output value of each entry can be determined:
# Adding all of the output values in this larger wires[i, ] produces 61229.
# For each entry, determine all of the wire/segment connections and decode the 
# four-digit output values. What do you get if you add up all of the output 
# values?

library(stringr)

n_wires <- nrow(wires)

decode <- matrix(NA, nrow = n_wires, ncol = 14)

for (i in 1:n_wires) {
  n_segments <- str_length(wires[i, ])
  
  codes <- lapply(str_split(wires[i, ], ""), str_sort)
  
  diff_7 <- sapply(codes, function(x) {
    length(setdiff(x, codes[[which(n_segments == 3)[1]]]))
  })
  
  decode[i, n_segments == 2] <- 1
  decode[i, n_segments == 3] <- 7
  decode[i, n_segments == 4] <- 4
  decode[i, n_segments == 7] <- 8
  decode[i, n_segments == 5 & diff_7 == 2] <- 3
  
  code_9 <- str_sort(union(codes[[which(decode[i, ] == 4)[1]]], 
                           codes[[which(decode[i, ] == 3)[1]]]))
  
  diff_9 <- sapply(codes, function(x) identical(x, code_9))
  
  decode[i, n_segments == 6 & diff_9 == TRUE] <- 9
  
  diff_0 <- sapply(codes, function(x) {
    length(setdiff(x, codes[[which(n_segments == 2)[1]]]))
  })
  
  decode[i, n_segments == 6 & diff_0 == 4 & is.na(decode[i,])] <- 0
  decode[i, n_segments == 6 & is.na(decode[i, ])] <- 6
  
  diff_25 <- sapply(codes, function(x) {
    length(setdiff(x, codes[[which(decode[i, ] == 6)[1]]]))
  })
  
  decode[i, n_segments == 5 & is.na(decode[i, ]) & diff_25 == 0] <- 5
  decode[i, n_segments == 5 & is.na(decode[i, ]) & diff_25 == 1] <- 2
}
rm(i, diff_0, diff_25, diff_7, diff_9, code_9, n_segments)

output <- apply(decode[, 11:14], 1, function(x) {
  as.numeric(paste0(x, collapse = ""))
  })

result <- sum(output)
result

rm(list = ls())
detach("package:stringr")

# End ----
