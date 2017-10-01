  #############################################################################
  ## Function to Calculate Monthly Mortgage Payments and Amortization Tables ##
  #############################################################################
  # Author: Thomas Girke
  # Last update: Feb 27, 2007
  # Utility: Calculates monthly and annual loan or mortgage payments, generates amortization tables and plots the results
  # How to run the script:
  #	source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")
  
  # Definitions: 
  #	  P = principal, the initial amount of the loan
  #	  I = annual interest rate
  #	  L = length of the loan in years, or at least the length over which the loan is amortized.
  #	  J = monthly interest in decimal form = I / (12 x 100)
  #	  M = monthly payment; formula: M = P * ( J / (1 - (1 + J) ^ -N))
  #	  N = number of months over which loan is amortized = L x 12
  # see also: http://www.jeacle.ie/mortgage/instructions.html
  
mortgage <- function(P=500000, I=6, L=30) { 
  J <- I/(12 * 100)
  N <- 12 * L
  M <- P*J/(1-(1+J)^(-N))
  monthPay <<- M

  return(M)

}
mortgage(158000, 1.85, 37)
      
  
  
