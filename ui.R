library(shiny)
require(markdown)

shinyUI(pageWithSidebar(
        
        headerPanel("Central Limit Theorem for Means"),
        
        sidebarPanel(
                radioButtons("dist", "Parent distribution (population):",
                             list("Normal" = "rnorm",
                                  "Uniform" = "runif",
                                  "Exponential" = "rexp",
                                  "Right skewed" = "rlnorm",
                                  "Left skewed" = "rbeta")),
                br(),
                
                uiOutput("mu"),
                uiOutput("sd"),
                uiOutput("min"),
                uiOutput("max"),
                uiOutput("skew"),
                uiOutput("rate"),
                
                sliderInput("n", 
                            "Sample size:", 
                            value = 50,
                            min = 20, 
                            max = 500),
                br(),
                
                sliderInput("k", 
                            "Number of samples:", 
                            value = 200,
                            min = 10, 
                            max = 4000),
                br(),
                helpText(a(href="https://gallery.shinyapps.io/CLT_mean/", target="_blank", "Inspired by"))
                
        ),
        
        mainPanel(
                tabsetPanel(type = "tabs", 
                            tabPanel("Plot", plotOutput("pop.dist"),
                                     br(),
                                     plotOutput("sampling.dist"),
                                     br(),
                                     div(h5(textOutput("CLT.descr"), align = "center"))), 
                            tabPanel("Help", includeMarkdown("readme.md")) 
                ))
                
))