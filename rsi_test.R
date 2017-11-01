# Attach the quantmod and TTR packages.
# You can install packages via:
# install.packages(c("quantmod","TTR"))
library(quantmod)
library(TTR)
# Pull S&P500 index data from Yahoo! Finance
startDate = as.Date("2012-01-01")
# The beginning of the date range we want to look at
endDate = as.Date("2014-01-01") 
# The end of the date range we want to look at
getSymbols("AAPL", src = "yahoo", from = startDate, to = endDate) 

# Calculate the RSI indicator
rsi <- RSI(Cl(AAPL),2) # Calculate Close-to-Close returns
# (this assumes we open/close our positions
# at each day’s close)
ret <- ROC(Cl(AAPL))
ret[1] <- 0 # Define our position-sizing function
rsi2pos <- function(ind, indIncr=5, posIncr=0.25) {
  # Inputs:
  # ind : indicator vector
  # indIncr : indicator value increments/breakpoints
  # posIncr : position value increments/breakpoints
  
  # Initialize result vector
  size <- rep(0,NROW(ind)) # Long
  size <- ifelse(ind < 4*indIncr, (1-posIncr*3), size)
  size <- ifelse(ind < 3*indIncr, (1-posIncr*2), size)
  size <- ifelse(ind < 2*indIncr, (1-posIncr*1), size)
  size <- ifelse(ind < 1*indIncr, (1-posIncr*0), size) # Short
  size <- ifelse(ind > 100-4*indIncr, 3*posIncr-1, size)
  size <- ifelse(ind > 100-3*indIncr, 2*posIncr-1, size)
  size <- ifelse(ind > 100-2*indIncr, 1*posIncr-1, size)
  size <- ifelse(ind > 100-1*indIncr, 0*posIncr-1, size)
  
  # Today’s position (‘size’) is based on today’s
  # indicator, but we need to apply today’s position
  # to the Close-to-Close return at tomorrow’s close.
  size <- lag(size) # Replace missing signals with no position
  # (generally just at beginning of series)
  size[is.na(size)] <- 0 # Return results
  return(size)
}

# Calculate signals using the ‘rsi2pos()’ function
sig <- rsi2pos(rsi, 5, 0.25) # Break out the long (up) and short (dn) signals
sigup <- ifelse(sig > 0, sig, 0)
sigdn <- ifelse(sig < 0, sig, 0) # Calculate equity curves
eq_up <- cumprod(1+ret*sigup)
eq_dn <- cumprod(1+ret*sigdn)
eq_all <- cumprod(1+ret*sig) # Replicate Michael’s nice chart (again)
png(filename="/home/vadim/dev/r/rsi2_replication.png")
plot.zoo( cbind(eq_up, eq_dn), plot.type="single",
          ylab=c("Long","Short"), col=c("green","red"),
          main="RSI(2) Strategy, with Position Scaling:n 2000-01-03 through 2008-12-07" )
dev.off()


sig <- rsi2pos(rsi, 10, 0.3) # Break out the long (up) and short (dn) signals
sigup <- ifelse(sig > 0, sig, 0)
sigdn <- ifelse(sig < 0, sig, 0) # Calculate equity curves
eq_up <- cumprod(1+ret*sigup)
eq_dn <- cumprod(1+ret*sigdn)
eq_all <- cumprod(1+ret*sig) # Re-plot equity curves using updated values
png(filename="/home/vadim/dev/r/20090501_rsi2_updated.png")
plot.zoo( cbind(eq_up, eq_dn), plot.type="single", ylab=c("Long","Short"), col=c("green","red"), main="Updated RSI(2) Strategy, with Position Scaling:n 2000-01-03 through 2008-12-07")
dev.off()


