divDiff <- function(xs, ys, x)
{
  n <- length(xs)
  q <- matrix(data = 0, n, n)  #q - iniciace matice pomernych diferenci
  q[,1] <- ys   
  f <- q[1,1]
  
  for (i in 2:n)
  {
    fi <- 1
    for (j in i:n) 
    {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (xs[j] - xs[j-i+1])  #spocita diferenci
      
      if (j==i) #vybere diference do rovnice (diagonala matice)
      { 
        
        for(k in 1:(j-1))
        {
          fi <- fi*(x - xs[k])   #spocte zavorky
        }
        fi <- q[i,j] * fi    #vynasobi s diferenci
      }
    }
    f <- f + fi #prida k celku rovnice
  }
  res <- list(f,q)
  names(res) <- c("y","diff")
  return(res)    #vraci list s interpolaci a matici diferenci
}
#testing
xs <- -10:10
ys <- cos(xs2)
x <- seq(-10,10, by = 0.1)
res <- divDiff(xs2,ys2,x2)
y <- res[["y"]]
yreal <- cos(x2)

#graf
plot(x,y,type="l")  #newtom 
points(xs,ys,col="red")   #zname body
lines(x,yreal,col="green", lty = 2)    #realna funkce

legend(x="topright",c("Newton","Zadana funkce","Zname body"),
       cex=.8,col=c("black","green","red"), lty=c(1,2,0), pch=c(-1,-1,1))

