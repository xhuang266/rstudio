# In this hw1.R assignment, we want students to:
#   - learn how to write a trivial R script
#   - learn how to run an R script via the source() function
#   - learn to answer our "write code ..." questions with R code that
#     isn't too specific (I'll describe this point more in class)
# We're hoping that this small exericse will lead to less trouble on
# hw2.R, which requires a more meaningful R script.
#
# You should replace all occurrences of "..." with your code. If you'd
# like to replace a single "..." with more than one line of code
# before my line (using your own variables), you may certainly do
# that.
#
# Name: Elaine Huang
# Email: xhuang266@wisc.edu

# We'll grade your homework by running this ".R" file via
#   source("hw1.R")

rm(list = ls()) # Remove all objects defined in workspace.

x = c(5, 7)

# Find the sum of the elements of the vector x.

# ... your code here, if you wish ...

answer.1 = sum(x) # ... set this variable correctly (please don't use "5+7"
# or "12": use more general code that will work even if x
# were changed)

cat(sep="", "sum of elements of x is ", answer.1, "\n")

# Write code to generate two random numbers from N(0, 1) and report
# their sum.

# ... your code here, if you wish ...
number1 = runif(1, 0, 1)
number2 = runif(1, 0, 1)

answer.2 = number1 + number2 # ... set this variable correctly

cat(sep="", "sum of two random N(0, 1) numbers is ", answer.2, "\n")