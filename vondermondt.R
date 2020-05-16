vandermonde.matrix <- function( alpha, n )
{
  # Tato funkce vrací m x n matici mocnin vektoru alfa
  #
  # alpha = m dimenze vektor
  # n = stupen polynomu
  
  if ( !is.vector( alpha ) )
    stop( "parametr alpha není vektor" )
  if ( !is.numeric( alpha ) )
    stop( "parametr n není èíselnı vektor" )
  m <- length( alpha )
  V <- matrix( 0, nrow=m, ncol=n )
  V[,1] <- rep( 1, m )
  j <- 2
  while ( j <= n ) {
    x <- alpha ^ ( j - 1 )
    V[,j] <- x
    j <- j + 1
  }
  return( V )
}

horner<-function(x,a) #x- funkcni hodnota v danych bodech, a- poly. fce ve tvaru a+bx+cx^2... (koeficienty)
{
  
  n<-length(a)   #n= stupen polynomu (3=x^2)
  res<-a[n]      #do res ulozim nejvyssi clen
  if(n==1)       #kdyz je polynom stupne 0, neni co pocitat
    return (res)
  for(i in (n-1):1)  
  {
    res<-res*x+a[i]    #algoritmus pocitani zavorek,  x(x(d+c)+b)+a
    #n uz v res je. Vzdy nasobim s x, a zacinam s pricitanim n-1..n=1
  }
  
  return (res)
}

#zname body
xs <- seq(-10,10,by=2) 
ys <- sin(xs)

#nezname body (x osa)
x<- seq(-10,10,by=0.1)

y<-sin(x) #opravdova funkce



A <- vandermonde.matrix(xs, length(xs))   #Vandermodovo matice z hodnot znamych bodu osy X
B <- solve(A,ys)  #interpolacni polynom (koeficienty)

#dosazeni za x do polynomu

res<-B[1]
for(i in 2:length(B))
{
  res<- res + B[i]*(`^`(x,i-1))
}

res2 <- horner(x,B)

#graf
plot(x,res2, type="l") #interpolace
points(xs,ys, col="red") #zname body
lines(x,y, col="green") #opravdova funkce
