# Advent of Code 2021

# https://adventofcode.com/

# Challenge #1.1 ----

# How many measurements are larger than the previous measurement?

oceandeep   <- read.table("oceandeep.txt")
differences <- diff(oceandeep$V1)
result      <- sum(differences > 0)
result
rm(differences, result)

# Challenge #1.2 ----

# Consider sums of a three-measurement sliding window. How many sums are larger 
# than the previous sum?

odeep_aggre <- sum(oceandeep$V1[1:3])

for (i in 2:1998) {
  odeep_aggre[i] <- sum(oceandeep$V1[i:(i+2)])
}
rm(i)

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

movement <- read.table("movement.txt") 

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

for (i in 1:1000) {
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

# End ----