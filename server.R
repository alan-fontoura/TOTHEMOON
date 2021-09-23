library(shiny)
library(shinydashboard)
library(xts)
library(dygraphs)
library(DT)

bitcoin_closing_prices <- readRDS('data\\bitcoin_closing_prices.RDS')
bitcoin_data <- readRDS('data\\bitcoin_data.RDS')
bitcoin_data_flat <- readRDS('data\\bitcoin_data_flat.RDS')
bitcoin_log_ret <- readRDS('data\\bitcoin_log_ret.RDS')
dates <- readRDS('data\\dates.RDS')
muli_tuple3 <- readRDS('data\\muli_tuple3.RDS')
muli_tuple5 <- readRDS('data\\muli_tuple5.RDS')
original_tuple <- readRDS('data\\original_tuple.RDS')
simulated_end_future_paths <- readRDS('data\\simulated_end_future_paths.RDS')
simulated_end_price_paths <- readRDS('data\\simulated_end_price_paths.RDS')
simulated_end_values <- readRDS('data\\simulated_end_values.RDS')
vol_pred <- readRDS('data\\vol_pred.RDS')
vol_series <- readRDS('data\\vol_series.RDS')
vol_xts <- readRDS('data\\vol_xts.RDS')
source('simulator.R')

shinyServer(function(input, output) {
    
    output$bitcoin_prices <- renderDygraph({
        
        dygraph(bitcoin_closing_prices, 
                main = 'Bitcoin Closing Prices') %>% 
            dySeries(label = 'Closing Prices')
        
    })

    output$volatility_series <- renderDygraph({
        
        dygraph(vol_xts, main = 'Daily Volatilities - GARCH (1,1)')
        
    })
    
    output$acf_vol <- renderPlot({
        
        acf(vol_series, main = 'AutoCorrelation', ylab = NA)
        
    }, height = 300)
    
    output$pacf_vol <- renderPlot({
        
        pacf(vol_series, main = 'Partial AutoCorrelation', ylab = NA)
        
    }, height = 300)
    
    output$sim_price_path <- renderDygraph({
        
        dygraph(muli_tuple3, main = 'Simulated Bitcoin Price Paths') %>% 
            dyLegend(show = 'never')
        
    })
    
    output$sim_price_hist <- renderPlot({
        
        ggplot(data.frame(sim_end_prices = simulated_end_values),
               aes(x = sim_end_prices)) +
            geom_histogram(fill = '#004165', col = 'white') +
            theme_bw() +
            ggtitle(label = 'Distribution of End Values') +
            xlab('Value') +
            ylab(NULL)
        
    })
    
    output$gbm_mle1 <- renderUI({ 
        
        withMathJax(
            
            helpText('We asssume the price proccess is a Geometric Brownian Motion, with the following SDE:'),
            helpText('$$dX_t = X_t(\\mu dt + \\sigma d W_t)$$'),
            helpText('$$X_{t+\\Delta t}=X_t e^{(\\mu - \\frac{1}{2}\\sigma ^2)\\Delta t +\\sigma (W_{t+\\Delta t}-W_t)}$$'),
            helpText('$$X_{t+\\Delta t}=e^{ln(X_t)+(\\mu - \\frac{1}{2}\\sigma ^2)\\Delta t +\\sigma (W_{t+\\Delta t}-W_t)}$$'),
            helpText('Applying $$W_{t+\\Delta t}-W_t \\stackrel{d}{=} \\sqrt{\\Delta t}Z$$'),
            helpText('We can write $$ln(X_t)+(\\mu - \\frac{1}{2}\\sigma ^2)\\Delta t +\\sigma (W_{t+\\Delta t}-W_t) \\sim N[ln(X_t)+(\\mu-\\frac{1}{2}\\sigma^2)\\Delta t,\\sigma^2 \\Delta t]$$'),
            helpText('Therefore, the price process \\(X_{t+\\Delta t}\\) follows a lognormal distribution such that:'),
            helpText('\\[X_{t+\\Delta t} \\sim LN[ln(X_t)+(\\mu - \\frac{1}{2}\\sigma ^2)\\Delta t,\\sigma^2 \\Delta t]\\]')

        )
        
    })
    
    output$gbm_mle2 <- renderUI({ 
        
        withMathJax(
            
            helpText('Now we can get the score equations with respect to the parameters'),
            helpText('$$\\frac{\\partial}{\\partial \\mu}ln {\\mathcal{L}}(\\widehat{\\mu}) = \\frac{1}{\\sigma^2}\\sum_{i=1}^n [ln(x_i)-ln(x_{i-1})-(\\mu - \\frac{1}{2}\\sigma^2)\\Delta t_i]$$'),
            helpText('$$\\frac{\\partial}{\\partial \\sigma^2}ln {\\mathcal{L}}(\\widehat{\\sigma^2}) = -\\frac{1}{2\\sigma^2}(n-\\frac{1}{\\sigma^2}\\sum_{i=1}^n \\frac{(ln(x_i)-ln(x_{i-1})-(\\widehat{\\mu} - \\frac{1}{2}\\widehat{\\sigma^2})\\Delta t_i)^2}{\\Delta t_i})$$'),
            helpText("Solving these score equations leads us to the respective MLEs"),
            helpText('$$\\widehat{\\mu} = \\frac{1}{2}\\widehat{\\sigma^2}+\\frac{ln(x_n)-ln(x_0)}{t_n-t_0}$$'),
            helpText('$$\\widehat{\\sigma^2} =\\frac{1}{n} \\sum_{i=1}^n  \\frac{(ln(x_i)-ln(x_{i-1})-( \\frac{ln(x_n)-ln(x_0)}{t_n-t_0}) \\Delta t_i)^2}{ \\Delta t_i}$$')
            
        )
        
    })
    
    output$sim_bit_price_path <- renderDygraph({
        
        dygraph(original_tuple, main = 'Simulated Bitcoin Price Paths') %>% 
            dyLegend(show = 'never')
        
    })
    
    output$sim_bit_histogram <- renderPlot({
        
        ggplot(data.frame(sim_end_prices = simulated_end_price_paths),
               aes(x = sim_end_prices)) +
            geom_histogram(fill = '#004165', col = 'white') +
            theme_bw() +
            ggtitle(label = 'Distribution of Simulated End Values') +
            xlab('Value') +
            ylab(NULL)
        
    })
    
    output$sim_future_prices <- renderDygraph({
        
        # CHANGE X AXIS FOR APR/21 TO MAR/22
        
        dygraph(muli_tuple5, main = 'Simulated Bitcoin Future Prices') %>% 
            dyLegend(show = 'never')
        
    })
    
    output$sim_future_histogram <- renderPlot({
        
        ggplot(data.frame(sim_end_prices = simulated_end_future_paths),
               aes(x = sim_end_prices)) +
            geom_histogram(fill = '#004165', col = 'white') +
            theme_bw() +
            ggtitle(label = 'Distribution of Simulated End Future Values') +
            xlab('Value') +
            ylab(NULL)
        
    })
    
    result <- reactive({
        
        set.seed(input$seed)
        
        sim_gbm(input$pdeath / 100, 
                input$guarantee_rr / 100, 
                input$drift * 10^-6, 
                input$vol * 10^-3, 
                input$nsim)
        
    })
    
    output$sim_result <- renderPlot({
        
        ggplot(data.frame(Result = result()[[1]]), aes(x = Result)) +
            geom_histogram(fill = '#004165', col = 'white') +
            theme_bw() +
            xlab('Returns') +
            ylab(NULL) +
            ggtitle('Distribution of Returns')
        
    })
    
    output$sim_table <- DT::renderDataTable({
        
        data.frame(Min = sort(result()[[1]], decreasing = F)[1:10],
                   Max = sort(result()[[1]], decreasing = T)[1:10]) %>% 
            DT::datatable(rownames = F, options = list(dom = 't', pageLength = 10)) %>% 
            DT::formatRound(columns = 1:2)
        
    })
    
    output$total_deaths <- renderValueBox({
        
        valueBox(result()[[2]], 'Total Deaths', color = 'purple')
        
    })
    
    output$pct_deaths <- renderValueBox({
        
        valueBox(paste0(round(result()[[2]] * 100 / input$nsim, 2), ' %'), 
                 '% of Deaths', color = 'yellow')
        
    })
    
    output$mean_return <- renderValueBox({
        
        valueBox(prettyNum(round(mean(result()[[1]]), 0), big.mark = ','), 
                 'Average Return', color = 'green')
        
    })
    
    output$teste <- renderUI({
        
        withMathJax(
            
            
            helpText('$$\\frac{1}{N_t - \\pi}$$')
            
        )
        
    })
    
})
