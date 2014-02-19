table <- data.frame(read.csv("../../scala-commsim/output.txt",header=F,dec=".",sep=" "))
names(table) <- c("time","agent","net","drugs.bought","drugs.sold","drugs.paid","drugs.got","precursor.bought","precursor.sold","precursor.paid","precursor.got")

plot(table$time[table$agent == "supplier1"], table$precursor.bought[table$agent == "supplier1"],type="l",col="blue")
lines(table$time[table$agent == "supplier1"], table$precursor.sold[table$agent == "supplier1"]-table$precursor.sold[table$agent == "supplier1"][1],col="red")
points(table$time[table$agent == "middleman"], table$precursor.bought[table$agent == "middleman"]-table$precursor.bought[table$agent == "middleman"][1],col="black",pch=0)

plot(table$time[table$agent == "world"], table$drugs.bought[table$agent == "world"]/table$drugs.paid[table$agent == "world"],type="l",col="blue")
lines(table$time[table$agent == "world"], table$drugs.bought[table$agent == "world"],col="red")
points(table$time[table$agent == "middleman"], table$precursor.bought[table$agent == "middleman"]-table$precursor.bought[table$agent == "middleman"][1],col="black",pch=0)

plot(table$time[table$agent == "retailer1"], table$drugs.bought[table$agent == "retailer1"]-table$drugs.sold[table$agent == "retailer1"],type="l",col="blue")
diff(table$drugs.paid[table$agent == "world"])
table$drugs.paid[table$agent == "world"]/table$drugs.bought[table$agent == "world"]
