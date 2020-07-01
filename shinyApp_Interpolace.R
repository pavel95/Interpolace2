library(shiny)
ui <- fluidPage(
  
  titlePanel("Interpolace"),
  
  sidebarLayout(
    
    sidebarPanel(
      splitLayout(
        numericInput('from','od',-10), numericInput('to','do',10)
      ),
      numericInput('points','Počet známých bodů',15),
      selectInput("funkce", label = "Funkce", 
                  choices = list("sin" = "sin", "cos" = "cos", "e^x" = "e^x", "x^2" = "x^2", "x^3" = "x^3"), 
                  selected = "sin"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Vandermonde", plotOutput("vandermondePlot"), 
          hr(), titlePanel("Vandermondova matice"), tableOutput("vMatrix")), 
        tabPanel("Lagrange", plotOutput("lagrangePlot")),
        tabPanel("Newton", plotOutput("newtonPlot"), 
                 hr(), titlePanel("Matice poměrných diferencí"), verbatimTextOutput("diffMatrix2"))
      )
    )
  )
)


server <- function(input, output, session){
  vandermonde.matrix <- function( alpha, n )
  {
    # Tato funkce vrací m x n matici mocnin vektoru alfa
    #
    # alpha = m dimenze vektor
    # n = stupen polynomu
    
    if ( !is.vector( alpha ) )
      stop( "parametr alpha není vektor" )
    if ( !is.numeric( alpha ) )
      stop( "parametr n není číselný vektor" )
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
  lagrangeInterpolation<- function(x, xs, ys){
    n <- length(xs)
    sum <- 0
    for(i in 1:n){
      pie <- 1 #pi je 1 pro vypocet dalsiho Li(x)
      for(j in 1:n){
        if(i != j){
          #vypočítáme fundamentální polynom Li(x)
          pie<- pie * (x - xs[j])/(xs[i] - xs[j])
        }
      }
      #přičte se dalsi cast Li(x)*f(xi)
      sum <- sum + ys[i] * pie  
    }
    return(sum)
  }
  
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
  
  #zname body
  xsr <- reactive({ seq(input$from,input$to,length.out = input$points) })
  ysr <- reactive({
    switch(input$funkce,
           "sin" = sin(xsr()),
           "cos" = cos(xsr()),
           "e^x" = exp(xsr()),
           "x^2" = xsr()*xsr(),
           "x^3" = xsr()*xsr()*xsr())
  })
  
  #vzorkovani dane interpolace
  xr <- reactive({seq(input$from,input$to,by=0.1)})
  
  #true funkce
  yrealr <- reactive({
    switch(input$funkce,
           "sin" = sin(xr()),
           "cos" = cos(xr()),
           "e^x" = exp(xr()),
           "x^2" = xr()*xr(),
           "x^3" = xr()*xr()*xr())
  })
  
  output$lagrangePlot <- renderPlot({
    
    #vzorkování lagrange
    y <- lagrangeInterpolation(xr(),xsr(),ysr())
    
    plot(xr(),y,type="l", main = paste("n=",input$points))  #lagrange 
    points(xsr(),ysr(),col="red")   #zname body
    lines(xr(),yrealr(),col="green", lty = 2)    #realna funkce
    
    legend(x="topright",c("Lagrange","Zadana funkce","Zname body"),
           cex=.8,col=c("black","green","red"), lty=c(1,2,0), pch=c(-1,-1,1))
  })
  
  output$vandermondePlot <- renderPlot({
    
    A <- vandermonde.matrix(xsr(), length(xsr()))   #Vandermodovo matice z hodnot znamych bodu osy X
    B <- solve(A,ysr())  #interpolacni polynom (koeficienty)
    
    #dosazeni za x do polynomu
    
    res2 <- horner(xr(),B)
    
    #graf
    plot(xr(),res2,type="l", main = paste("n=",input$points))  #vandermonde 
    points(xsr(),ysr(),col="red")   #zname body
    lines(xr(),yrealr(),col="green", lty = 2)    #realna funkce
    
    legend(x="topright",c("Vandermonde","Zadana funkce","Zname body"),
           cex=.8,col=c("black","green","red"), lty=c(1,2,0), pch=c(-1,-1,1))
  })
  
  output$vMatrix <- renderTable({
    vandermonde.matrix(xsr(), length(xsr()))
  })
  
  output$newtonPlot <- renderPlot({
    
    res <- divDiff(xsr(),ysr(),xr())
    y <- res[["y"]]
    yreal <- sin(x)
    
    #graf
    plot(xr(),y,type="l", main = paste("n=",input$points))  #Newton 
    points(xsr(),ysr(),col="red")   #zname body
    lines(xr(),yrealr(),col="green", lty = 2)    #realna funkce
    
    legend(x="topright",c("Newton","Zadana funkce","Zname body"),
           cex=.8,col=c("black","green","red"), lty=c(1,2,0), pch=c(-1,-1,1))
  })
  
  output$diffMatrix2 <- renderPrint({

    res <- divDiff(xsr(),ysr(),xr())
    diff <- res[["diff"]]
    diff
    })
}

shinyApp(ui = ui, server = server)