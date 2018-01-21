# This script places buy orders on GDAX for the ETHUSD pair, at generally oversold levels, and then sells at
# overbought levels.



library(rgdax)
library(mailR)


# Build functions
curr_bal_eth <- function(x){
  accounts(api.key = "key", secret = "secret", passphrase = "passphrase")[2,3]
}
curr_bal_usd <- function(x){
  accounts(api.key = "key", secret = "secret", passphrase = "passphrase")[4,3]
}
curr_ema13_api <- function(x){
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  ema13_gdax <- tail(TTR::EMA(df[,5],
                              n = 13),
                     n = 1)
  ema13_gdax
}
curr_ema34_api <- function(x){
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  ema34_gdax <- tail(TTR::EMA(df[,5],
                              n = 34),
                     n = 1)
  ema34_gdax
}
curr_rsi14_api <- function(x){
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  rsi_gdax <- tail(TTR::RSI(df[,5],
                            n = 14),
                   n = 1)
  rsi_gdax
}
bid <- function(x){
  bid <- public_orderbook(product_id = "ETH-USD", level = 1)
  bid <- bid$bids[1]
  bid
}
ask <- function(x){
  ask <- public_orderbook(product_id = "ETH-USD", level = 1)
  ask <- ask$asks[1]
  ask
}
usd_hold <- function(x){
  holds(currency = "USD", "key", "secret", "passphrase")
}  
eth_hold <- function(x){
  holds <- holds(currency = "ETH", "key", "secret", "passphrase")
  holds
}
cancel_orders <- function(x){
  cancel_orders <- cancel_order("key", "secret", "passphrase")
  cancel_orders
}
buy_exe <- function(x){
  # get order size in iterative manner
  order_size <- round(curr_bal_usd()/ask(),3)[1]-0.005
  # place initial order
  while(curr_bal_eth() == 0){
    #order_size <- curr_bal_usd() / ask() - 0.009
    add_order(product_id = "ETH-USD", api.key = "key", secret = "secret", passphrase = "passphrase",
              type="limit", price = bid(), side = "b",  size = order_size )
    # sleep to see if order takes
    Sys.sleep(17)
    # check to see if ETH bal >= order amt
    if(curr_bal_eth() > 0){"buysuccess"}else{
      
      cancel_orders()    # if curr_eth_bal not > 0, cancel order and start over 
    }
  }
}
sell_exe <- function(x){
  # place initial order
  while(curr_bal_eth() > 0){
    add_order("ETH-USD", api.key = "key", secret = "secret", passphrase = "passphrase",
              type="limit", price = ask(), side = "s",  size = curr_bal_eth())
    # sleep to see if order takes
    Sys.sleep(17)
    # check to see if ETH bal >= order amt
    if(curr_bal_eth() == 0){"buysuccess"}else{
      
      cancel_orders()    # if curr_eth_bal not > 0, cancel order and start over
    }
  }
}
position <- (read.csv("C:/users/video/Desktop/position.csv", header = TRUE))[1,2]

# Actual Trading Loop
if(curr_bal_usd() >= 20){    # if have more than $20 USD start loop
  if(curr_ema13_api() < curr_ema34_api() &    # and ema 13 < ema34
     curr_rsi14_api()<= 35){    # and rsi14 <=35
    add_or
    # buy
    
    buy_exe()
    
    # save buy price in csv on desktop
    position <- write.csv(bid(), file = "C:/users/video/Desktop/position.csv")    
    
    # send email
    
    send.mail(from = "email@gmail.com",
              to = c("email@gmail.com"),
              replyTo = c("Reply to someone else <email@gmail.com>"),
              subject = "GDAX ETH Test - Buy",
              body = paste("Your model says buy right now"),
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "your_username", passwd = "your_password", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    # print for logs
    print("buy")
    
  }else{"nobuy"}
}else{"nobuy"}
if(curr_bal_eth() > 0){    # if have more than $0 USD of ETH
  if(curr_ema13_api() > curr_ema34_api() &    # and ema 13 > ema34
     curr_rsi14_api() >= 60 &     # and rsi14 >= 60
     ask() > position) {    # check to see if current ask price is greater than the prev. buy price
    
    # sell
    sell_exe()
    
    # send email
    
    send.mail(from = "email@gmail.com",
              to = c("email@gmail.com"),
              replyTo = c("Reply to someone else <email@gmail.com>"),
              subject = "GDAX ETH Test - Sell",
              body = paste("Your model says sell right now"),
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "your_username", passwd = "your_password", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    # print for logs
    print("sell")
    
  }else{"nosell"}
}else{"nosell"}

# print systime for logs
Sys.time()


