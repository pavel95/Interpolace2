#xs a ys jsou zname body funkce, x jsou hledane body
lagrangeInterpolation<- function(x, xs, ys)
{
  n <- length(xs)
  sum <- 0
  for(i in 1:n)
  {
    pie <- 1 #pi je 1 pro vypocet dalsiho Li(x)
    for(j in 1:n)
    {
      if(i != j)
      {
        #vypoèítáme fundamentální polynom Li(x)
        pie<- pie * (x - xs[j])/(xs[i] - xs[j])
      }
    }
    #pøiète se dalsi cast Li(x)*f(xi)
    sum <- sum + ys[i] * pie  
  }
  return(sum)
}


###TEST SECTION###
#vzorkovani dané funkce (známé body)
xs <- seq(-10,10,by=2) 
ys <- sin(xs)


#vzorkování lagrange
x <- seq(-10,10,by=0.1) 
y <- lagrangeInterpolation(x,xs,ys)

yreal <- sin(x)

#graf
plot(x,y,type="l")  #lagrange 
points(xs,ys,col="red")   #zname body
lines(x,yreal,col="green")    #realna funkce

legend(x=6.3,y=2.8,c("Lagrange","Zadana funkce","Zname body"),
       cex=.8,col=c("black","green","red"), lty=c(1,1,0), pch=c(-1,-1,1))

