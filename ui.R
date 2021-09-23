library(tidyverse)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)

navbarPage('TOTHEMOON',
           
    navbarMenu('GARCH Modeling',
        
        tabPanel('Bitcoin Prices',
                 
            column(width = 8, dygraphOutput('bitcoin_prices')),
            
            column(width = 4, 
                   
               fluidRow(img(src = 'ACF.png')),
               fluidRow(img(src = 'PACF.png'))
            )
            
        ),
        
        tabPanel('Volatility',
            
            column(width = 8, 
                   dygraphOutput('volatility_series'),
                   tags$br(),
                   tags$ul(
                       tags$li('We estimated a GARCH(1,1) model to forecast the 
                               daily volatility of bitcoin prices.'),
                       tags$br(),
                       tags$li('After this, we estimated an ARMA(3,0) model to 
                               describe the volatility evolution of bitcoin 
                               prices.')
                   )
                   
                   
                   ),
            
            column(width = 4,
               fluidRow(plotOutput('acf_vol', height = '75%')),
               fluidRow(plotOutput('pacf_vol', height = '75%'))
            )
            
        ),
        
        tabPanel('Simulated Price Paths',
            
                 fluidRow(
                     column(width = 8, dygraphOutput('sim_price_path')),
                     column(width = 4, plotOutput('sim_price_hist'))
                 ),
                 
                 fluidRow(
                     column(width = 10, offset = 1,
                     tags$br(),
                     tags$ul(
                         tags$li('Monte Carlo Simulation'),
                         tags$br(),
                         tags$li('The majority of the end values fall between a 
                                 price range of $0-$20,000'),
                         tags$br(),
                         tags$li('The actual price path is one of the more 
                                 extreme price paths')
                     )
                 )
                 )
            
        )
               
    ),
    
    navbarMenu('GBM Modeling',
        
        tabPanel('Maximum Likelihood Estimators',
            
            column(width = 6, uiOutput('gbm_mle1')),
            column(width = 6, uiOutput('gbm_mle2'))
            
            
        ),
        
        tabPanel('Simulated Price Paths',
            
            fluidRow(
                column(width = 6, dygraphOutput('sim_bit_price_path')),
                column(width = 6, plotOutput('sim_bit_histogram'))
            ),
            
            fluidRow(column(offset = 3, width = 9,
                            tags$br(),
                tags$ul(
                tags$li('We see that the majority of the end values in our 
                simulation occur within a price range of $30,000-$60,000.'),
                tags$br(),
                tags$li("This is the most frequent price range for the 
                simulated path. Notice that the actual data's end 
                value is within the range")
                )
                       
            )
            )
            
        ),
        
        tabPanel('Simulated Future Prices',
            
            fluidRow(
                column(width = 6, dygraphOutput('sim_future_prices')),
                column(width = 6, plotOutput('sim_future_histogram'))
            ),
            
            fluidRow(column(offset = 3, width = 9,
                            tags$br(),
                            tags$ul(
                                tags$li("Simulations of future bitcoin prices are 
                                        shown to have dramatic growth which can 
                                        be expected due to the nature of it's 
                                        high volatility."),
                                tags$br(),
                                tags$li('We confirm the three most frequent price
                                        ranges are:',
                                        tags$br(),
                                        tags$ul(
                                            tags$br(),
                                            tags$li('$130,000 - $240,000'),
                                            tags$br(),
                                            tags$li('$240,000 - $350,000'),
                                            tags$br(),
                                            tags$li('$20,000 - $130,000')),
                                
                                )
                            )
                            
                            
            )
            )
            
        )
        
    ),
    
    navbarMenu('Simulator',
               
        tabPanel('Model Premises',
            
            column(width = 6,
            
                   tags$ul(
                       tags$li(tags$h2('Model: Geometric Brownian Motion')),
                       #tags$br(),
                       tags$li(tags$h2('Inputs and Initial Values:'),
                               #tags$br(),
                               tags$ul(
                                   tags$li(tags$h3(withMathJax(helpText('Arbitrary seed: \\(0\\)')))),
                                   tags$li(tags$h3(withMathJax(helpText('Probability of Death: \\(0.1%\\)')))),
                                   tags$li(tags$h3(withMathJax(helpText('Guaranteed Rate of Return: \\(10%\\)')))),
                                   tags$li(tags$h3(withMathJax(helpText('Volatility: \\(1.33 \\times 10^{-3}\\)')))),
                                   tags$li(tags$h3(withMathJax(helpText('Drift: \\(4.58 \\times 10^{-6}\\)')))),
                                   tags$li(tags$h3(withMathJax(helpText('Number of Simulations: \\(1,000\\)'))))
                               ))
                   )
                   
            )
            
        ),
        
        tabPanel('Simulation Results',
            
            sidebarLayout(
                sidebarPanel(width = 3,
                    
                    numericInput(
                        inputId = 'seed',
                        label = 'Seed',
                        value = 0,
                        min = 0,
                        width = '50%',
                        step = 1
                    ),
                    
                    numericInput(
                        inputId = 'pdeath',
                        label = 'Prob. of Death in 3 Months (%)',
                        value = 0.1,
                        min = 0,
                        width = '100%',
                        step = .1
                    ),
                    
                    numericInput(
                        inputId = 'guarantee_rr',
                        label = 'Guarantee Return Rate (%)',
                        value = 10,
                        min = 0,
                        max = 50,
                        width = '100%',
                        step = 1
                    ),
                    
                    numericInput(
                        inputId = 'vol',
                        label = 'Volatility - Minute (10^-3)',
                        value = 1.33,
                        min = 0,
                        max = 10,
                        width = '100%',
                        step = .01
                    ),
                    
                    numericInput(
                        inputId = 'drift',
                        label = 'Drift - Minute (10^-6)',
                        value = 4.58,
                        min = 0, 
                        max = 10,
                        width = '100%',
                        step = .01
                    ),
                    
                    numericInput(
                        inputId = 'nsim', 
                        label = 'Number of Simulations',
                        value = 1000,
                        min = 100,
                        max = 10^6,
                        step = 100,
                        width = '100%'
                    )
                ),
                
                mainPanel(
                    
                    column(width = 9,
                           
                           fluidRow(plotOutput('sim_result')),
                           
                           br(),
                           
                           fluidRow(column(width = 3, offset = 2, valueBoxOutput('total_deaths', width = 12)),
                                    column(width = 3, valueBoxOutput('pct_deaths', width = 12)),
                                    column(width = 3, valueBoxOutput('mean_return', width = 12)))
                           
                    ),
                    
                    column(width = 2, DT::dataTableOutput('sim_table'))
                    
                )
                
            )
            
        )
        
    )
    
)
