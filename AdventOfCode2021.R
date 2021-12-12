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
# Now, the above example does something different:
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
# For example, to determine the oxygen generator rating value using the same 
# example diagnostic report from above:
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
# Then, to determine the CO2 scrubber rating value from the same example above:
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
# In the above example, the second board is the last to win, which happens after 
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
# lines of vents (your puzzle input) for you to review. For example:
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
# no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
# To avoid the most dangerous areas, you need to determine the number of points 
# where at least two lines overlap. In the above example, this is anywhere in 
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
# Considering all lines from the above example would now produce the following 
# diagram:
# You still need to determine the number of points where at least two lines 
# overlap. In the above example, this is still anywhere in the diagram with a 2 
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

# End ----
