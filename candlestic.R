install.packages("plotly")
library(plotly)
library(quantmod)

getSymbols("AAPL",src="yahoo")

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

z <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low) %>%
  layout(title = "Basic Candlestick Chart")

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="/home/vadim/r/candlestick-basic/")
chart_link = plot_ly(z)
chart_link

z
