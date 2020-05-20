library(shiny)

ui <- fluidPage(
  titlePanel("Interpolace"),
  splitLayout(
    numericInput('from','od',-10), numericInput('to','do',10)
  ),
  numericInput('points','Počet známých bodů',11),
  plotOutput('graf')
)


server <- function(input, output, session){
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
  
  
  xsr <- reactive({ seq(input$from,input$to,length.out = input$points) })
  
  output$graf <- renderPlot({
    #vzorkovani dané funkce (známé body)
    xs <- xsr() 
    ys <- sin(xs)
    
    
    #vzorkování lagrange
    x <- seq(input$from,input$to,by=0.1) 
    y <- lagrangeInterpolation(x,xs,ys)
    
    yreal <- sin(x)
    plot(x,y,type="l", main = paste("n=",input$points))  #lagrange 
    points(xs,ys,col="red")   #zname body
    lines(x,yreal,col="green", lty = 2)    #realna funkce
    
    legend(x="topright",c("Lagrange","Zadana funkce","Zname body"),
           cex=.8,col=c("black","green","red"), lty=c(1,2,0), pch=c(-1,-1,1))
  })
}

shinyApp(ui = ui, server = server)