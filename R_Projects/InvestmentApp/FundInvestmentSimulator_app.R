library(ggplot2)
library(shiny)
library(reshape2)

computeBalance <- function(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed) {
  numPeriods = NumYears
  rate = AverageReturn
  vol = AverageVol
  if(ContributionFrequency==1){
    numPeriods = NumYears*12
    rate = AverageReturn/12
    vol = AverageVol/12
  }
  set.seed(RandomSeed)
  simulatedRates <- matrix(rnorm(numPeriods*NumSimulation, mean = rate , sd = vol),nrow=numPeriods, ncol = NumSimulation)
  balance <- matrix(rep(InitialAmount,NumSimulation),nrow = 1,ncol = NumSimulation)
  for(i in 1:numPeriods){
    balance = rbind(balance, balance[i,]*(1+simulatedRates[i,])+PeriodicContribution)
  }
  
  return(balance)
}

plotBalance <- function(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
{
  balance = computeBalance(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
  dat<- melt(balance, varnames = c("Year","Simulation"))
  balanceSummmary = as.matrix(t(apply(balance, 1, quantile, probs = c(0.1, 0.5, 0.9),  na.rm = TRUE)))
  colnames(balanceSummmary) <- c("p10", "p50", "p90")
  bSummary<- melt(balanceSummmary, varnames = c("Year","Simulation"))
  
  ggplot(NULL, aes(x = Year, y = value, group = Simulation)) + 
    geom_line(data=dat,color='grey')+
    geom_line(data=bSummary[bSummary$Simulation=="p10"|bSummary$Simulation=="p90",], color='red', size=1) + 
    geom_line(data=bSummary[bSummary$Simulation=="p50",], color='black', size=1) +
    labs(title="Future Value Simulations : 10 percentile, Median, 90 percentile", subtitle = paste("InitialAmount=",InitialAmount, "; Term=",NumYears, "yr ; Number of Periods=",ifelse(ContributionFrequency==1, NumYears*12, NumYears),"; Simulations: ",NumSimulation,";\nPeriodicContribution=",PeriodicContribution,";ContributionFrequency=",ifelse(ContributionFrequency==1, "Monthly", "Yearly"), sep = ""), x = "Number of Periods")
}

plotDistribution <- function(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
{
  balance = computeBalance(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
  dat1 <- data.frame(FinalBalance = c(as.vector(tail(balance,1))))
  ggplot(dat1, aes(x=FinalBalance)) + 
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title="Future Value Distribution", subtitle = paste("InitialAmount=",InitialAmount, "; Term=",NumYears,"yr; Simulations: ",NumSimulation,";\nPeriodicContribution=",PeriodicContribution,";ContributionFrequency=",ifelse(ContributionFrequency==1, "Monthly", "Yearly"), sep = ""))
}

plotHist <- function(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
{
  balance = computeBalance(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
  dat1 <- data.frame(FinalBalance = c(as.vector(tail(balance,1))))
  ggplot(dat1, aes(x=FinalBalance)) + 
    geom_histogram(alpha=0.2, fill='blue')+
    labs(title="Histogram of Final Balance", subtitle = paste("InitialAmount=",InitialAmount, "; Term=",NumYears,"yr; Simulations: ",NumSimulation,";\nPeriodicContribution=",PeriodicContribution,";ContributionFrequency=",ifelse(ContributionFrequency==1, "Monthly", "Yearly"), sep = "")) 
}


printStats <- function(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
{
  balance = computeBalance(InitialAmount, NumYears,PeriodicContribution,ContributionFrequency,AverageReturn, AverageVol,NumSimulation,RandomSeed)
  finalBalance = as.vector(tail(balance,1))
  print("Summary stats for Future Value:")
  print(summary(finalBalance))
  fv_mean = mean(finalBalance)
  std_dev = sd(finalBalance)
  print(paste("Variance of Future Value:", round(std_dev*std_dev,2), sep=" "))
  print(paste("Std Deviation of Future Value:", round(std_dev,2), sep=" "))
  p95_start  = round( fv_mean - (1.96*std_dev)/sqrt(length(finalBalance)), 2)
  p95_end  = round(fv_mean + (1.96*std_dev)/sqrt(length(finalBalance)), 2)
  print(paste("95% confidence interval for Future Value (assuming normal dist.): [",p95_start,",",p95_end,"]",sep = ""))
}


ui <- fluidPage(
    titlePanel("Investment Simulator App"),
    h4("Shikha Chamoli"),
    hr(),
    fluidRow(
      column(2,
           fluidRow(
             column(10,numericInput("InitialAmount", label = h5("Initial Amount"), value = 1000))
           ),
           fluidRow(
             column(10,sliderInput("NumYears", label = h5("Number of Years"), min = 0, max = 40, value = 10))
           )
    ),
    column(2,
           fluidRow(
             column(10,numericInput("PeriodicContribution", label = h5("Periodic Contribution"), value = 30))
           ),
           fluidRow(
             radioButtons("ContributionFrequency", label = h5("Contribution at the end"),
                          choices = list("of each month" = 1, "of each year" = 2), 
                          selected = 1)
           )
    ),
    column(2,
           fluidRow(
             column(10,numericInput("AverageReturn", label = h5("Avg. Annual Return"), value = 0.10))
           ),
           fluidRow(
             column(10,sliderInput("AverageVol", label = h5("Avg. Annual Volatility"), min = 0, max = 1, value = 0.18))
           )
    ),
    column(2,
           fluidRow(
             column(10,sliderInput("NumSimulation", label = h5("Number of Simulations"), min = 0, max = 1000, value = 500))
           ),
           fluidRow(
             column(10,numericInput("RandomSeed", label = h5("Random Seed"), value = 12345))
           )
    )
  ),
  hr(),
  fluidRow(
    column(10,
           plotOutput("simulationPlot")
    )
  ),
  hr(),
  fluidRow(
    column(5,
      h4("Summary Statistics"),
      verbatimTextOutput("simulationStats")
    ),
    column(5,
           h4("Distribution Analysis"),
           tabsetPanel(
             tabPanel("Density", plotOutput("DistributionPlot")), 
             tabPanel("Histogram", plotOutput("HistogramPlot"))
           )
    )
  )
)

server <- function(input, output) {
  output$simulationPlot <- renderPlot({
    plotBalance(
      input$InitialAmount,
      input$NumYears,
      input$PeriodicContribution,
      input$ContributionFrequency,
      input$AverageReturn,
      input$AverageVol,
      input$NumSimulation,
      input$RandomSeed)
  })
  
  output$simulationStats <- renderPrint({
    printStats(
      input$InitialAmount,
      input$NumYears,
      input$PeriodicContribution,
      input$ContributionFrequency,
      input$AverageReturn,
      input$AverageVol,
      input$NumSimulation,
      input$RandomSeed)
  })
  
  output$DistributionPlot <- renderPlot({
    plotDistribution(
      input$InitialAmount,
      input$NumYears,
      input$PeriodicContribution,
      input$ContributionFrequency,
      input$AverageReturn,
      input$AverageVol,
      input$NumSimulation,
      input$RandomSeed)
  }, height = 250)
  
  output$HistogramPlot <- renderPlot({
    plotHist(
      input$InitialAmount,
      input$NumYears,
      input$PeriodicContribution,
      input$ContributionFrequency,
      input$AverageReturn,
      input$AverageVol,
      input$NumSimulation,
      input$RandomSeed)
  }, height = 250)
}

shinyApp(ui = ui, server = server)