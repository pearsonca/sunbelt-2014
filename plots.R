# plots.R

# READ IN DATA
fn <- "output.txt" # CHANGE TO CORRECT FILENAME FOR ACTUAL DATA
dat <- read.table(file=fn,sep=" ")

names(dat) <- c("time","agent","net","drugs.bought","drugs.sold","drugs.cost","drugs.revenue",
                "precursor.bought","precursor.sold","precursor.cost","precursor.revenue")
head(dat)
table(dat$agent)

levels(dat$agent) <- c("Cook","Middle man","Retailer","Supplier","Wholesaler","World")


price.calc <- function(focag,data=dat){
  sub <- subset(data,agent==focag)
  sub <- subset(sub,duplicated(sub)==F)
  diffs <- as.data.frame(apply(sub[-(1:3)],2,diff))
  drug.purchase <- diffs$drugs.cost/diffs$drugs.bought
  drug.sale <- diffs$drugs.revenue/diffs$drugs.sold
  prec.purchase <- diffs$precursor.cost/diffs$precursor.bought
  prec.sale <- diffs$precursor.revenue/diffs$precursor.sold
  difftimes <- sub$time[-1]
#  browser()
  return(data.frame(time=difftimes,drug.purchase,drug.sale,prec.purchase,prec.sale))
}

pricedat <- lapply(levels(dat$agent),price.calc)
names(pricedat) <- levels(dat$agent)


colors <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#252525")
names(colors) <- levels(dat$agent)

plot.price <- function(focag,drugs=T,prices=pricedat[[focag]],color=colors[focag],...){
  if(drugs){
    sell <- prices$drug.sale
    buy <- prices$drug.purchase
  }else{
    sell <- prices$prec.sale
    buy <- prices$prec.purchase
  }
#   if(any(sell>0)) lines(prices$time,sell,lty=1,col=color,...)
#   if(any(buy>0)) lines(prices$time,buy,lty=3,col=color,...)
  lines(prices$time,sell,lty=1,col=color,...)
  lines(prices$time,buy,lty=3,col=color,...)
}

par(bty="L",lwd=2,cex.axis=1.5,cex.lab=2,mar=c(7,7,2,2))
# Precursor
YMAXprec <- 4  # NOTE: MAY NEED TO ADJUST YLIM!!
plot(NA,NA,xlim=c(0,100),ylim=c(0,YMAXprec),xlab="",ylab="",main="Product: Precursors")
mtext("Time in months",1,cex=2.5,line=4.5)
mtext("Price (AUS $ per gram)",2,cex=2.5,line=4.5)
#lapply(levels(dat$agent),plot.price,drugs=F,lwd=4)
use.prec <- c(1,2,4)
lapply(levels(dat$agent)[use.prec],plot.price,drugs=F,lwd=4)
legend(80,YMAXprec,bty="n",lty=1,col=colors[use.prec],legend=levels(dat$agent)[use.prec],lwd=4,title="Seller")
legend(60,YMAXprec,bty="n",lty=3,col=colors[use.prec],legend=levels(dat$agent)[use.prec],lwd=4,title="Buyer")

# Drugs
YMAXdrug <- 500 # NOTE: MAY NEED TO ADJUST YLIM!!
plot(NA,NA,xlim=c(0,100),ylim=c(0,YMAXdrug),xlab="",ylab="",main="Product: Drugs")
mtext("Time in months",1,cex=2.5,line=4.5)
mtext("Price (AUS $ per gram)",2,cex=2.5,line=4.5)
lapply(levels(dat$agent),plot.price,drugs=T,lwd=4)
legend(80,YMAXdrug,bty="n",lty=1,col=colors,legend=levels(dat$agent),lwd=4,title="Seller")
legend(60,YMAXdrug,bty="n",lty=3,col=colors,legend=levels(dat$agent),lwd=4,title="Buyer")
