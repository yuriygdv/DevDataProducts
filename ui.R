library(shiny)


#### Solving Problems Using Normal Distributions with SLIDER INPUTS  ###



navbarPage("Solving Problems Using Normal Distributions",
           
  tabPanel("Problem 1: Finding Normal Proportions (Probabilities) Given Values",
  
  
  
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Problem 1: Finding Normal Proportions (Probabilities) Given a Value"),
      p("Suppose there is a population described by a Normal distribution with known parameters and you want to find out the probability of drawing an observation below/above a particular value or between two values. This app allows you to interactively calculate such probabilities and visuzlize the process using the original Normal density curve and the corresponding Standardized Normal distribution. To use the app, follow the instructions below."),
      em("Step 1. Choose the parameters of the density curve that defines some Normal distribution of interest: "),
      br(),
      sliderInput("slider1", label = h5("Mean"), min = 0, max = 100, value = 50),
      sliderInput("slider2", label = h5("Standard Deviation"), min = 1, max = 50, value = 10),
      selectInput("select1", label = em("Step 2. What probability from the defined normal distribution do you want to find? (make your selection below)"), 
                  choices = list("Proportion (probability) below some value: P[X<x]" = 1, "Proportion between two values: P[x1<X<x2]" = 2,
                                 "Proportion above some value: [X>x]" = 3), selected = 1),
      sliderInput("slider3", label = em("Step 3. Use the slider to select the range of values of X for which you want to find the Normal probabilities:"), min = 0, max = 250, value = c(0, 1)),
      p("The results of the calculations are displayed in the panel on the right -->")
    ),
    
    
    mainPanel(                         
      
      plotOutput("normalCurve"),
      textOutput("text2"),
      plotOutput("standardNormalCurve")
    )
    
  )
),                  


  tabPanel("Problem 2: Finding Values Given Proportions",
         
         sidebarLayout(
             sidebarPanel(
                 
                 h4("Problem 2: Finding a value when given a proportion"),
                 p("This section of the app solves the inverse problem. Given a Normal population with known parameters, you can find values that split the distribution in certain proportions. Hence, you can find the values that represent particular Normal probabilities. To run the app, first, choose the Mean and the Standard Deviation of the distribution of interest. Then pick a proportion (probability) for which you want to find the value and specify whether the proportion should be at the top, at the bottom, or in the middle of the distribution."),
                 em("Choose the parameters of the density curve that defines some Normal distribution of interest: "),
                 br(),
                 sliderInput("slider12", label = h5("Mean"), min = 0, max = 100, value = 50),
                 sliderInput("slider22", label = h5("Standard Deviation"), min = 1, max = 50, value = 10),
                 sliderInput("slider4", label = h5("Percent (Proportion = Percent/100) of cases / probability"), min = 1, max = 99, value = 1),
                 radioButtons("radio1", label = h5("Choose your option:"), 
                             choices = list("At the top of the distribution" = 1, "At the bottom of the distribution" = 2, "Middle of the distribution" = 3), selected = 1)

             ), 
             
             
             mainPanel(
                 
                 textOutput("text3"),
                 plotOutput("normalCurve2"),
                 plotOutput("standardNormalCurve2")
                 
             ) 
         )     
         
  )            


)              



