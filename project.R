data_set =read.csv('StudentsPerformance.csv')
#library("shiny")

## DATA CLEANING
dim(data_set)
## missing data :
sum(is.na(data_set))
#duplicate data
duplicated(data_set)
#remove duplicated row
data_set = unique(data_set)
#summary
summary(data_set)
str(data_set)
##ui of charts and summary
ui <- fluidPage(
  # App title ----
  titlePanel("Tabsets"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of observations to generate ----
      sliderInput(inputId = "n",
                  "Number of bins:",
                  value = 20,
                  min = 10,
                  max = 100,
                  step = 10),
      sliderInput("adjust",label = "Bandwidth Adjustment",min=.2,max=2,value = .4,step =.2)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram", plotOutput("Histogram")),
                  tabPanel("Piechart", plotOutput("Piechart")),
                  tabPanel("parplot", plotOutput("parplot")),
                  tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)
##################################################################
server <- function(input, output) {
  ##Histogram chart
  output$Histogram <- renderPlot({
    n <- input$n
    adjust<-input$adjust
    hist(data_set$age,col="lightblue",xlab = "Age",main = "Age Histogram",breaks= n,prob=TRUE)
    lines(density(data_set$age,adjust=as.numeric(adjust)),lwd=2,col="blue")
    })
  ##########################
  ##Pie chart
  output$Piechart <- renderPlot({
    G1=data_set$G1
    G1=sum(G1)
    G2=data_set$G2
    G2=sum(G2)
    G3=data_set$G3
    G3=sum(G3)
    slices<-c(G1,G2,G3)
    lbls<-c("G1","G2","G3")
    pct<-slices/sum(slices)*100
    #calculate percentile
    lbls<-paste(lbls,pct)
    #add percent to labels
    lbls<-paste(lbls,"%",sep = "")
    #add % to labels
    pie(slices,labels = lbls,col = rainbow(length(lbls)),main="pie chart of G")
  })
  ###########################
  ##parplot
  output$parplot <- renderPlot({
    graph<-table(data_set$goout)
  barplot(graph,main="Go_out Distribution")
  Avg_goout_PerDay<-aggregate(data_set[,1],list(data_set$absences), mean)
  })
  ################################
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(data_set)
  })
  ##########################################
}
shinyApp(ui=ui,server=server)