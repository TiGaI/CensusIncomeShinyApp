## app.R ##
library(shinydashboard)
library(ggplot2)
library(wesanderson)
library(dplyr)

# library(googleVis)
# output$googleHist <- renderGvis({
#     gvisHistogram(input$x, options=list(
#         legend="{ position: 'top', maxLines: 2 }",
#         colors="['#5C3292', '#1A8763', '#871B47']"))
# })


header <- dashboardHeader(title = "Census Income Visualization")

sidebar <-     dashboardSidebar(
    sidebarMenu(
        HTML(paste0('<div class="info-card" style="background-image: url("https://media.licdn.com/dms/image/C4D16AQHNnMdK6QrZjw/profile-displaybackgroundimage-shrink_350_1400/0?e=1570060800&v=beta&t=oQvPhA-7OE7uqat5XT1li2n4F9orKNdUlxGrg6endAE"),
    background-repeat: no-repeat;background-size:cover;background-position: 50%;left:0;  right:0; top:0;bottom:0;">
          <div class="info-card-text">
            <h5>Jayce Jiang</h5>
            <h5>NYC Data Science Academy</h5>
            <h5>New York, US</h5>
          </div>
        </div>')
        ),
        menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
        menuItem("Data Visualization", tabName = "datavisual", icon = icon("th")),
        menuItem("Census Income Prediction", tabName = "datapred", icon = icon("th"))
    )
)

body <- dashboardBody(
    # Introduction Tab
  tabItems(
    tabItem(tabName = "introduction",
            h1(style="text-align: center","Census Data Visualization and Machine Learning"),
            h2("Problem Statement: "),
            tags$blockquote(
            p(style="font-size: 22px","Given various features, the aim is visualize the census data set and to
            build a predictive model to determine the income level for people in US. 
              The income levels are binned at below 50K and above 50K.")
            ),
            p(style="font-size: 18px","This is a census data income visualization and machine learning project from NYC
            Data Science Academy. Using weight census data extracted from
              the 1994 and 1995 population surveys conducted by the U.S. Census Bureau, I will use
              Shiny to display correlation graphs within each of the 40+ variables and use simple Support
              Vector Machine(SVM) to predict individual income based on input parameters."),
            h2("Data Visualization: "),
            p(style="font-size: 18px","In this sector, user are able to input their desire numerical and categorical data to display and analysis.
               The Shiny app will display their correlation using various of chart to visualize their correlation with the
               income level.In addition, at the bottom of this section, it shows the parameters and machine learning technique I use to create the machine learning model."),
            h2("Machine Learning Prediction: "),
            p(style="font-size: 18px","In this sector, user are able to select a range of inputs from age, maritial status, education, nationality, race and etc.
               The Shiny app will display the estimated income level based on the simple SVM machine learning model. Below the estimator, I have document the machine learning training process as well as the parameters
              I used to train this SVM Machine Learning model. Based on the result of this model, I was able to achieve a 95.4% accurary on the training dataset after 10 cross validation runs.")
    ),
        tabItem(tabName = "datavisual",
                fluidRow(
                    box(plotOutput("histplot")),
                    box(plotOutput("scattergraph")),
                    box(plotOutput("barCateGraph")),
                    box( height=430,
                    box(
                        title = "Histogram Graph", width = 6,
                        selectInput("x", "Choose numerical column you want to plot the count for the Census Income Dataset:",
                                    colnames(columnName), width = 200)
                    ),
                    box(
                        title = "Scatter chart on numerical variables", width = 6,
                        selectInput("scatterX", "Choose the X axis for the scatter graph:",
                                    colnames(num_train)),
                        selectInput("scatterY", "Choose the Y axis for the scatter graph:",
                                    colnames(num_train), selected = "wage_per_hour")
                    ),
                    box(
                      title = "Bar Count Table on categorical variables", width = 6,
                      selectInput("barCateX", "Choose categorical column for the proportionate table:",
                                  colnames(cat_train), width = 200)     
                    ),
                    box(
                      title = "Bar Proportionate Table on categorical variables", width = 6,
                      selectInput("barX", "Choose categorical column for the proportionate table:",
                                  colnames(cat_train), width = 200)
                    )
                    ),
                    box(plotOutput("barprotable"))
                    
                )
        ),
        # Second tab content
        tabItem(tabName = "datapred",
                h2("Machine Learning Prediction"),
                box(
                  title = "Input Variables", width = 12,
                  selectInput("", "Choose categorical column for the proportionate table:",
                              colnames(cat_train), width = 200, selected = ""),
                  selectInput("", "Choose categorical column for the proportionate table:",
                              colnames(cat_train), width = 200, selected = ""),
                  selectInput("", "Choose categorical column for the proportionate table:",
                              colnames(cat_train), width = 200, selected = ""),
                  selectInput("", "Choose categorical column for the proportionate table:",
                              colnames(cat_train), width = 200, selected = ""),
                  selectInput("", "Choose categorical column for the proportionate table:",
                              colnames(cat_train), width = 200, selected = ""),
                ),
                column(4,
                  actionButton("goButton", "Submit"),
                  wellPanel
                  (
                    h3("Results"),    
                    verbatimTextOutput("value")
                  )
                )
                
                
                
        )
))

ui <- tagList(dashboardPage(header, sidebar, body),
              tags$footer("Please note: this project is still under construction, improvement, and updates as of this time.", align = "center", style = "
                  position:fixed;
                  bottom:0;
                  width:100%;
                  height:50px;   /* Height of the footer */
                  color: white;
                  padding: 10px;
                  background-color: black;
                  z-index: 1000;")
)

barfill <- "#4271AE"
barlines <- "#1F3552"

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    #Do Prediction
    results <- eventReactive(input$goButton,{
        return("result 3")
    })
    #Get Results
    #results <- c("result 1","result 2","result 3");
    output$value <- renderPrint({ results() })
    
    output$histplot<-renderPlot({
        if(exists(input$x)==FALSE){
            ggplot(data=num_train, aes_string(x=input$x))+
            geom_histogram(aes(y=..density.. ,fill = ..count..)) +
            labs(title=paste("Frequency histogram ", input$x)) +
            scale_y_continuous(name = "Count") +
            scale_x_continuous(name = input$x) +
            scale_fill_gradient("Count", low = "blue", high = "red")
        }
        })
    
    output$scattergraph<-renderPlot({
      stringX <- gsub("_", " ", input$scatterX)
      stringY <- gsub("_", " ", input$scatterY)
      
      ggplot(data=num_train, aes_string(x=input$scatterX, y=input$scatterY, color=as.factor(num_train$income_level)))+
        geom_point()+
        scale_x_continuous(stringX) +
        scale_y_continuous(stringY) +
        theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        scale_color_manual(name = "Income Level", labels = c("Less than $50000", "Greater Than $50000"), values=wes_palette(n=2, name="GrandBudapest1"))
      
    })
    
    output$barCateGraph <- renderPlot({
      #barplot
      stringbarCateX <- gsub("_", " ", input$barCateX)
      
      newData <- cat_train %>% 
        group_by_(input$barCateX, "income_level") %>%
        tally()
      
      ggplot(newData, aes_string(x=input$barCateX, y="n", fill=as.factor(newData$income_level)))+
        geom_bar(stat="identity",position="dodge")+
        scale_fill_manual(name="Income Level",
                          labels=c("Less than 50000", "Greater than 50000"), values=wes_palette(n=2, name="Darjeeling1"))+
        xlab(stringbarCateX)+ylab("Count")+ theme(axis.text.x =element_text(angle  = 30,hjust = 1,size=10))
    })
    
    
    output$barprotable <- renderPlot({
      stringBarX <- gsub("_", " ", input$barX)
      ggplot(cat_train, aes_string(input$barX, group = cat_train$income_level)) +
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) +
        scale_y_continuous(labels=scales::percent) +
        ylab("relative Percent") +
        xlab(stringBarX) +
        facet_grid(~income_level) +
        labs(title=paste(stringBarX,"category per income level", sep = " "), size = 15) +
        theme(axis.text.x =element_text(angle  = 35,hjust = 1,size=10))+
        scale_color_manual(values=wes_palette(n=3, name="GrandBudapest1"))
    })
}

shinyApp(ui, server)