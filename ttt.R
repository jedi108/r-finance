install.packages(c("quantmod", "TTR", "PerformanceAnalytics"))


library(quantmod)
library(TTR)
library(PerformanceAnalytics)


getSymbols("^GSPC", from = "2000-01-01")

# Calculate the RSI indicator
rsi <- RSI(Cl(GSPC), 2)

# Calculate Close-to-Close returns
ret <- ROC(Cl(GSPC))
ret[1] <- 0

# This function gives us some standard summary
# statistics for our trades.
tradeStats <- function(signals, returns) {
  sysRet <- signals * returns * 100
  posRet <- sysRet > 0 # Positive rule returns
  negRet <- sysRet < 0 # Negative rule returns
  dat <- cbind(signals, posRet * 100, sysRet[posRet], sysRet[negRet], 1)
  dat <- as.data.frame(dat)
  
  # Aggreate data for summary statistics
  means <- aggregate(dat[, 2:4], by = list(dat[, 1]), mean, na.rm = TRUE)
  medians <-
    aggregate(dat[, 3:4], by = list(dat[, 1]), median, na.rm = TRUE)
  sums <- aggregate(dat[, 5], by = list(dat[, 1]), sum)
  
  colnames(means) <- c("Signal", "% Win", "Mean Win", "Mean Loss")
  colnames(medians) <- c("Signal", "Median Win", "Median Loss")
  colnames(sums) <- c("Signal", "# Trades")
  
  all <- merge(sums, means)
  all <- merge(all, medians)
  
  wl <-
    cbind(abs(all[, "Mean Win"] / all[, "Mean Loss"]),
          abs(all[, "Median Win"] / all[, "Median Loss"]))
  colnames(wl) <- c("Mean W/L", "Median W/L")
  
  all <- cbind(all, wl)
  return(all)
}

# This function determines position size and
# enables us to test several ideas with much
# greater speed and flexibility.
rsi2pos <- function(ind,
                    indIncr = 5,
                    posIncr = 0.25) {
  # Inputs:
  # ind : indicator vector
  # indIncr : indicator value increments/breakpoints
  # posIncr : position value increments/breakpoints
  
  # Initialize result vector
  size <- rep(0, NROW(ind))
  
  # Long
  size <- ifelse(ind < 4 * indIncr, (1 - posIncr * 3), size)
  size <- ifelse(ind < 3 * indIncr, (1 - posIncr * 2), size)
  size <- ifelse(ind < 2 * indIncr, (1 - posIncr * 1), size)
  size <- ifelse(ind < 1 * indIncr, (1 - posIncr * 0), size)
  
  # Short
  size <- ifelse(ind > 100 - 4 * indIncr, 3 * posIncr - 1, size)
  size <- ifelse(ind > 100 - 3 * indIncr, 2 * posIncr - 1, size)
  size <- ifelse(ind > 100 - 2 * indIncr, 1 * posIncr - 1, size)
  size <- ifelse(ind > 100 - 1 * indIncr, 0 * posIncr - 1, size)
  
  # Today"s position ("size") is based on today"s
  # indicator, but we need to apply today"s position
  # to the Close-to-Close return at tomorrow"s close.
  size <- lag(size)
  
  # Replace missing signals with no position
  # (generally just at beginning of series)
  size[is.na(size)] <- 0
  
  # Return results
  return(size)
}

# Calculate signals with the "rsi2pos()" function,
# using 5 as the RSI step: 5, 10, 15, 20, 80, 85, 90, 95
# and 0.25 as the size step: 0.25, 0.50, 0.75, 1.00
sig <- rsi2pos(rsi, 5, 0.25)

# Break out the long (up) and short (dn) signals
sigup <- ifelse(sig > 0, sig, 0)
sigdn <- ifelse(sig < 0, sig, 0)

# Calculate rule returns
ret_up <- ret * sigup
colnames(ret_up) <- "Long System Return"
ret_dn <- ret * sigdn
colnames(ret_dn) <- "Short System Return"
ret_all <- ret * sig
colnames(ret_all) <- "Total System Return"


png(filename = "/home/vadim/dev/r/20090606_rsi2_performance.png", 720, 720)
charts.PerformanceSummary(cbind(ret_up, ret_dn), methods = "none", main =
                            "RSI(2) Performance – RSI steps = 5, Size steps = 0.25")
dev.off()

# Print trade statistics table
cat("nRSI(2) Trade Statistics – RSI steps = 5, Size steps = 0.25n")
print(tradeStats(sig, ret))

# Print drawdown table
cat("nRSI(2) Drawdowns – RSI steps = 5, Size steps = 0.25n")
print(table.Drawdowns(ret_all, top = 10))

# Print downside risk table
cat("nRSI(2) Downside Risk – RSI steps = 5, Size steps = 0.25n")
print(table.DownsideRisk(ret_all))

# Calculate signals with the "rsi2pos()" function
# using new RSI and size step values
sig <- rsi2pos(rsi, 10, 0.3)

# Break out the long (up) and short (dn) signals
sigup <- ifelse(sig > 0, sig, 0)
sigdn <- ifelse(sig < 0, sig, 0)

# Calculate rule returns
ret_up <- ret * sigup
colnames(ret_up) <- "Long System Return"
ret_dn <- ret * sigdn
colnames(ret_dn) <- "Short System Return"
ret_all <- ret * sig
colnames(ret_all) <- "Total System Return"

# Calculate performance statistics
png(filename = "/home/vadim/dev/r/20090606_rsi2_performance_updated.png", 720, 720)
charts.PerformanceSummary(cbind(ret_up, ret_dn), methods = "none",
                          main = "RSI(2) Performance – RSI steps = 10, Size steps = 0.30")
dev.off()

# Print trade statistics table
cat("nRSI(2) Trade Statistics – RSI steps = 10, Size steps = 0.30n")
print(tradeStats(sig, ret))

# Print drawdown table
cat("nRSI(2) Drawdowns – RSI steps = 10, Size steps = 0.30n")
print(table.Drawdowns(ret_all, top = 10))

# Print downside risk table
cat("nRSI(2) Downside Risk – RSI steps = 10, Size steps = 0.30n")
print(table.DownsideRisk(ret_all))
