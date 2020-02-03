setwd("C:/Users/Kim F/Desktop/Semester 1/R&SQL_for_Business_Analytics/3. Zusammenfassungen & Guidelines/! Ch._19_Functions")
library(tidyverse)
library(lubridate)

# CHT 19 - Functions

#Writing a function has three big advantages over using copy-and-paste:
# 1. You can give a function an evocative name that makes your
# code easier to understand.
# 2. You only need to update code in one place.
# 3. You eliminate the chance of making incidental mistakes when 
# you copy and paste 


# Functions need to have a name (pick good name) ~ rescale01

# List the input  ~ x (if more then add more like x, y, z)
# Code in the body surrounded by curly brackets/braces {}
# It's easier to start with working code and turn it into a function; 
# It's harder to create a function and then try to make it work.

## creating a function
?rnorm
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df
# determine input variable:
x <- df$a
# determine intermediate calculations:
rng <- range(x, na.rm = TRUE)
# range returns a vector containing the min and max of all the given arguments

# determine function with input variable and intermediate calculations
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1]) # [1] refers to min, [2] refer to max value given in the vector
} 

# test the function:
rescale01(df$a)
rescale01(c(0, 67, 10))
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

## if changes occur you only have to make change at one place respectively
# The more repetition you have in your code, 
# the more places you need to remember to update -> try to avoid repitions

df <- tibble::tibble(
  a = 2*rnorm(100),
  b = 3*rnorm(100),
  c = 4*rnorm(100),
  d = 5*rnorm(100)
)
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
df$a
df$b
df$c
df$d

x <- c(1:10, Inf)
rescale01(x)

?finite
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE) # finit = T the min and max of only the finite values is computed
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)


## Practice 19.2.1 RfDS (p. 341)

# 1. Why is TRUE not a parameter to rescale01() ? What would happen if x contained
# a single missing value, and na.rm was FALSE ?

# it is not a parameter as this should be a fixed condition. Otherwise, if one missing 
# value occurs, this will be contagious and the function will return vector of length 0 
# as output

# 2. In the second variant of rescale01() , infinite values are left unchanged. Rewrite
# rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.

#### check nochmal

# 4. function to compute variance of a numeric vector 

# input: numeric vector; name: x
x <- c(1:10)

# function body: 

x <- x[!is.na(x)]
n <- length(x)
mean <- sum(x) / n
sq_error <- (x-mean)^2

#function:

variance <- function (x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mean <- sum(x) / n
  sq_error <- (x-mean)^2
  sum(sq_error)
}

x <- c(1:10)
variance(x)


# Write both_na() , a function that takes two vectors of the same length and returns
# the number of positions that have an NA in both vectors.

# input variables: 
x <- c(1,2, NA, NA)
y <- c(2,3, NA, NA)

# function body: 

(na1 <- sum(is.na(x) == T))
(na2 <- sum(is.na(y) == T))

# function:

both_na <- function(x,y) {
  na1 <- sum(is.na(x) == T)
  na2 <- sum(is.na(y) == T)
  sum(c(na1, na2), na.rm = T)
}

x <- c(1,2,NA)
y <- c(2,3,NA)

both_na(x,y)

## 19.3.1 Exercise (RfDS)

# 1. Read the source code for each of the following three functions, puzzle out what they
# do, and then brainstorm better names

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

# this function controls whether strings all contain
# the same prefix 
# a better name: control_prefix

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

# this function returns null if a vector has one or 0 elements; it returns every 
# vector except the last vector element (expressed by "length ()") otherwise 
# a better name: drop_last

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
?rep
# this function replicates the elements (!) in y as many times as x has elements
x <- c(1,2)
length(x)
y <- c(1,3,4)
f3(x,y)
#  --------------------------------------

## Conditional execution
# The condition must evaluate to either TRUE or FALSE.

add <- function(x, y) {
  z <- x + y
  if (z > 10) {
    seq(1:10)
  } else {
    seq(1:9)
  }
}

add(2, 30)
add(2, 2)

?any
?all
?identical

y <- c(1,2,3)
x <- c(1,2,5)

any(y == x)
identical(y,x)

# Multiple conditions 
# Use || (or) and && (and) to combine multiple logical expressions
# If you do have a logical vector, you can use any() or all() to 
# collapse it to a single value.

calc <- function(x, y, type) {
  if (type == "add") {
    x + y
  } else if (type == "minus") {
    x - y
  } else if (type == "multiply") {
    x * y
  } else if (type == "divide") {
    x / y
  } else {
    stop("Unknown type of operation")
  }
}


calc(2, 3, "multiply")
calc(2, 3, "multiiiiiply")

## switch function as alternative to multiple if-else statements
# if you end up with a very long series of chained if statements, 
# you should consider rewriting. switch() function
?switch()

calc2 <- function(x, y, type) {
  switch(type,
         plus = x + y,
         minus = x - y,
         multiply = x * y,
         divide = x / y,
         stop("Unknown type of operation 2")
  )
}

calc2(2, 3, "multiply")
calc2(2, 3, "multiiiiiply")

# another example: 

?require
require(stats) # stats is a statistical package in R
centre <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

x <- rcauchy(10)
centre(x, "mean")
centre(x, "median")
centre(x, "trimmed")
centre(x, "triiiimmed")

## Cut as second alternative to multiple if-else-statements

#Advantage of cut is that it works on vectors, whereas if only works
#on a single value. 

(temp <- seq(-10, 50, by = 5))

?cut
#(using <=)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = TRUE,
    labels = c("freeezing", "cold", "cool", "warm", "hot"))

#(using < )
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = FALSE,
    labels = c("freeezing", "cold", "cool", "warm", "hot"))



# Exercise 19.4.4 (RfDS)
# 1. What's the difference between if and ifelse() ? Carefully read the help and
# construct three examples that illustrate the key differences.
?ifelse
# ifelse returns values - True or False- (!plural) of each element fulfilling condition
?`if` 
# if only returns one value - True or False - if every element fulfills condition 

# 2. Write a greeting function that says "good morning", "good afternoon", or "good
# evening", depending on the time of day. 
# (Hint: use a time argument that defaults to lubridate::now() .
# That will make it easier to test your function.)

hr_now <- hour(now())

greeting <- function(x) {
  hr_now <- hour(x)
  if(hr_now < 12) {
    print("good morning")
  } else if(between(hr_now, 12, 16)){
    print("good afternoon")
  } else {
    print("good evening")
  } 
}

greeting(lubridate::now())

?between
now()
?hour()

# 3. Implement a fizzbuzz function. It takes a single number as input. If the number is
# divisible by three, it returns "fizz". If it's divisible by five it returns "buzz". 
# If it's divisible by three and five, it returns "fizzbuzz". 
# Otherwise, it returns the number. Make sure you first write working code 
# before you create the function.

# input variables: 
x <- 1 # first input variable: single number 

# intermediate calculations:

dividable_three <- x%%3
dividable_five <- x%%5

fizzbuzz <- function(x) {
  dividable_three <- x%%3
  dividable_five <- x%%5
  if(dividable_three == 0 && dividable_five != 0){
    print("fizz")
  }else if(dividable_five == 0 && dividable_three != 0){
    print("buzz")
  } else if(dividable_three == 0 && dividable_five == 0){
    print("fizzbuzz")
  } else x
}

x <- 3
x <- 15
x <- 10
fizzbuzz(x)


# 6. What does this switch() call do? What happens if x is "e"?

x <- "e"
switch(x,
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

#Function arguments

#Generally, data arguments come first. Detail arguments should go on 
#the end, and usually should have default values. You specify a default 
#value in the same way you call a function with a named argument.
#The default value should almost always be the most common value.

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

## making a function more robust: includ a stop argument

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

# If x and w are not the same length, R will not prompt an error (vector recycling rules):
wt_mean(1:6, 1:3)

# thus: include stop argument
?is.logical(na.rm)
?na.rm
?stopifnot

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w) #vector of FALSE AND TRUE; NA -> TRUE
    x <- x[!miss] #Not TRUE i.e. FALSE i.e. available
    w <- w[!miss] #Dito
  }
  sum(w * x) / sum(w)
} #x and w are local variables.

wt_mean(1:6, 6:1, na.rm = "foo")
wt_mean(1:6, 6:1, na.rm = NA)
wt_mean(1:6, 6:1, na.rm = F)
wt_mean(1:6, 6:1, na.rm = T)
wt_mean(c(1, 2, 3, ., 5, 6), c(6, 5, 4, 3, 2, 1), na.rm = T)
wt_mean(c(1, 2, 3, 4, NA, 6), c(6, 5, 4, 3, NA, 1), na.rm = T)
wt_mean(c(1, 2, 3, 4, NA, 6), c(6, 5, 4, 3, NA, 1), na.rm = F)
wt_mean(c(1, 2, 3, 4, N, 6), c(6, 5, 4, 3, 2, 1), na.rm = T)
wt_mean(c(1, 2, 3, 4, N, 6), c(6, 5, 4, 3, 2, 1), na.rm = TRUE)

is.logical(TRUE)
is.logical(FALSE)
is.logical(T)
is.logical(F)
is.logical(NA) # returns true as well
is.logical(0)
is.logical("FCK")


length(FALSE) == 1
length(TRUE) == 1
length(F) == 1
length(T) == 1
length(c("T", "F")) == 1
length() == 1

# Change the values in the x vector to scope out the values of variables.... 
x <- c(1, NA, 3)
miss <- is.na(x) # | is.na(w)
y <- x[!miss]
miss
x
y

# easier: 
(not_miss <- x[!is.na(x)])

#... dotdotdot
rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

rule("Important output")
rule(12*12*12)
rule(sum(1, 2))
# the elements of above function: 
(title <- paste0("Important output"))
(width <- getOption("width") - nchar(title) - 5)
pad <- "-"
(cat(title, " ", stringr::str_dup(pad, width), "\n", sep = ""))

#paste, paste0: Concatenate vectors after converting to character.
#...	one or more R objects, to be converted to character vectors.

?paste0
getOption("width") 
?getOption # Allow the user to set and examine a variety of global options which affect the way in which R computes and displays its results.
nchar("Important output")
?cat
?stringr::str_dup()
stringr::str_dup("*", 75)

x <- 0
x <- c(0, 2, NA)
x
sum(x, na.rm = TRUE)
sum(x)

## excursus: str_dup

fruit <- c("apple", "pear", "banana")
str_dup(fruit, 2)
str_dup(fruit, 1:3)
str_c("ba", str_dup("na", 0:5))


## Return values -----------------------------------------------------------

check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result) #Explicit return statement
}

check(1)
check(-10)
check(0)


is.function(check) # is a given R object a function object?


## Pipeable functions

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
  #(The functions return values can now be assigned)
  #(*******) doesn't work without this function
}

?invisible #df not printed by default
x <- show_missings(mtcars)
class(x)
dim(x)

# Due to the invisible(df) argument the function remains pipeable (*******)
# View(mtcars)

mtcars %>% 
  show_missings() %>%  # 0 missing
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% # mpg is a column in mtcars
  show_missings() # 18 missing

View(mtcars)

##
qwe <- c(1, 2, NA, NA)
show_miss <- function(qwe) {
  n <- sum(is.na(qwe))
  cat("Missing values: ", n, "\n", "Sqrt of", n^2, sep = " ")
  invisible(qwe) 
}
show_miss(qwe)
#Invisible but still there and pipeable

qweII <- c(1, 2, NA, NA)
show_miss <- function(qweII) {
  n <- sum(is.na(qweII))
  cat("Missing values: ", n, "\n", "Sqrt of", n^2, sep = " ")
  qweII
}
show_miss(qweII) 



# [#The environment of functions]

f <- function(x) {
  x + y
} 
y <- 100
f(10)
y <- 1000
f(10)
#Since y is not defined inside the function, R will look 
#in the environment where the function was defined:

#Don't do this
`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
1 + 2 #WtF - overriding how addition works
table(replicate(1000, 1 + 2))

rm(`+`)

?rm
?replicate




