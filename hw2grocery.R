#source("hw2grocery.R")
#rm(list = ls())
# how.many method
how.many = function(item, n.max) {
  line = paste("How many ",item,"?\n1:",sep="")
  item.num = as.numeric(readline(line))
  
  while ((item.num < 0) | (item.num > n.max)) {
    print(" ERROR: too many for the budget")
    line = paste("How many ",item,"?\n1:",sep="")
    item.num = as.numeric(readline(line))
  }
  return(as.numeric(item.num))
}


#grocery method
grocery.list = function(file, budget) {
  # read file
  price.list = read.csv(file, header = FALSE, col.names = c("item", "price"))
  print(price.list)
  # initial money
  money = budget
  # create 3*3 data frame
  pricetable <- data.frame(item= character(0), price = numeric(0),quantity=numeric(0))
  #(pricetable) = c("item","price", "quantity" )
  
  
  for (i in 1: length(price.list$item)) {
    
    if (money > as.numeric(price.list$price[i])) {
      max.num = money %/% as.numeric(price.list$price[i])
      item.num = how.many(price.list$item[i], max.num)
      money = money - item.num * price.list$price[i]
      if (item.num > 0) {
        new.purchase = data.frame(price.list$item[i],price.list$price[i], item.num)
        pricetable = rbind(pricetable,new.purchase)
      }
    }
  }
  
  #add column name and print table
  colnames(pricetable) = c("item","price", "quantity" )
  print(pricetable)
  
  
  final.price = 0
  #find final price
  for (i in 1:length(pricetable$price)) {
    final.price =  final.price + (as.numeric(pricetable[i,2]))*(as.numeric(pricetable[i,3]))
  }
  
  return(as.numeric(final.price))
}



#main method
shopping = grocery.list("groceries.csv",10)
print(paste("Your bill is $", shopping, sep=""))

