# Derived from https://github.com/tgouhier/climit

library(shiny)


seed = as.numeric(Sys.time())

shinyServer(function(input, output) {
        
        
        output$mu = renderUI(
                {
                        if (input$dist == "rnorm")
                        {
                                sliderInput("mu",
                                            "Mean",
                                            value = 0,
                                            min = -50,
                                            max = 50)
                        }
                })
        
        output$sd = renderUI(
                {
                        if (input$dist == "rnorm")
                        {
                                sliderInput("sd",
                                            "Standard deviation",
                                            value = 20,
                                            min = 1,
                                            max = 30)
                        }
                })
        
        output$min = renderUI(
                {
                        #print("min")
                        if (input$dist == "runif")
                        {
                                sliderInput("min",
                                            "Lower Bound",
                                            value = 0,
                                            min = 0,
                                            max = 20)
                        }
                })
        
        output$max = renderUI(
                {
                        #print("max")
                        if (input$dist == "runif")
                        {
                                sliderInput("max",
                                            "Upper Bound",
                                            value = 1,
                                            min = 1,
                                            max = 20)
                        }
                })
 
        output$rate = renderUI(
                {
                        #print("rate")
                        if (input$dist == "rexp")
                        {
                                sliderInput("rate",
                                            "Rate",
                                            value = 1,
                                            min = .1,
                                            max = 20,
                                            step = 0.1)
                        }
                })
        
        output$skew = renderUI(
                {
                        #print("skew options")
                        if (input$dist == "rlnorm" | input$dist == "rbeta"){
                                selectInput(inputId = "skew",
                                            label = "Skew",
                                            choices = c("Low skew" = "low",
                                                        "Medium skew" = "med",
                                                        "High skew" = "high"),
                                            selected = "low")
                        }
                })
        
        rand_draw = function(dist, n, mu, sd, min, max, skew, rate) 
        {
                vals = NULL
                if (dist == "rbeta") {
                        skew = ifelse(is.null(input$skew), "med", input$skew) 
                        if (skew == "low"){
                                vals = do.call(dist, list(n=n, shape1=5, shape2=2))
                        }
                        else if (skew == "med"){
                                vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
                        }
                        else if (skew == "high"){
                                vals = do.call(dist, list(n=n, shape1=5, shape2=1)) 
                        }
                }     
                else if (dist == "rnorm"){
                        mu = ifelse(is.null(input$mu), 0, input$mu) 
                        sd = ifelse(is.null(input$sd), 20, input$sd)
                        vals = do.call(dist, list(n=n, mean=mu, sd=sd))
                }    
                else if (dist == "rlnorm"){
                        skew = ifelse(is.null(input$skew), "med", input$skew) 
                        if (skew == "low"){
                                vals = do.call(dist, list(n=n, meanlog=0, sdlog=.25))
                        }
                        else if (skew == "med"){
                                vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
                        }
                        else if (skew == "high"){
                                vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
                        }
                }
                else if (dist == "runif"){
                        min = ifelse(is.null(input$min), 0, input$min) 
                        max = ifelse(is.null(input$max), 0, input$max) 
                        vals = do.call(dist, list(n=n, min=min, max=max))
                }  else if (dist == "rexp") {
                        rate = ifelse(is.null(input$rate), 0, input$rate) 
                        vals = do.call(dist, list(n=n,rate=rate))
                }  
                return(vals)
        }
        
        rep_rand_draw = repeatable(rand_draw)  
        
        parent = reactive({
                n = 1e5
                return(rep_rand_draw(input$dist, n, input$mu, input$sd, input$min, input$max, input$skew, input$rate))
        })
        
        samples = reactive({
                pop = parent()
                n = input$n
                k = input$k
                return(replicate(k, sample(pop, n, replace=TRUE)))
        })
        
        # plot 1   
        output$pop.dist = renderPlot({
                distname = switch(input$dist,
                                  rnorm = "Population distribution: Normal",
                                  rlnorm = "Population distribution: Right skewed",
                                  rbeta = "Population distribution: Left skewed",
                                  runif = "Population distribution: Uniform",
                                  exp = "Population distribution: Exponential")   
                
                pop = parent()
                m_pop =  round(mean(pop),2)
                sd_pop = round(sd(pop),2)
                mu = ifelse(is.null(input$mu), 0, input$mu)
                
                L = NULL
                U = NULL
                
                error = FALSE
                
                if (input$dist == "runif"){
                        L = ifelse(is.null(input$min),0,input$min) ; U = ifelse(is.null(input$max),0,input$max)
                        if (L > U){
                                error = TRUE
                        }
                }
                
                if (error)
                {
                        plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
                        text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
                }
                else{
                        
                        pdens=density(pop)
                        phist=hist(pop, plot=FALSE)
                        if (input$dist == "rnorm"){
                                hist(pop, main=distname, xlab="", breaks=50, freq=FALSE, 
                                     ylim=c(0, max(pdens$y, phist$density)), col="darkblue", border = "white", 
                                     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                                legend_pos = ifelse(mu > 0, "topleft", "topright")
                                legend(legend_pos, inset = 0.025, 
                                       legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))), 
                                       bty = "n", cex = 1.5, text.col = "darkblue", text.font = 2)
                        }
                        if (input$dist == "runif"){
                                hist(pop, main=distname, breaks=50, xlab="", freq=FALSE, 
                                     ylim=c(0, max(pdens$y, phist$density)+.5), col="darkblue", border = "white", 
                                     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                                legend_pos = ifelse(mu > 0, "topleft", "topright")
                                legend(legend_pos, inset = 0.025, 
                                       legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))), 
                                       bty = "n", cex = 1.5, text.col = "darkblue", text.font = 2)
                        }
                        if (input$dist == "rexp"){
                                hist(pop, main=distname, breaks=50, xlab="", freq=FALSE, 
                                     ylim=c(0, max(pdens$y, phist$density)+.5), col="darkblue", border = "white", 
                                     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                                legend_pos = ifelse(mu > 0, "topleft", "topright")
                                legend(legend_pos, inset = 0.025, 
                                       legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))), 
                                       bty = "n", cex = 1.5, text.col = "darkblue", text.font = 2)
                        }
                        if (input$dist == "rlnorm") {
                                hist(pop, main=distname, breaks=50,
                                     xlab="", freq=FALSE, ylim=c(0, max(pdens$y, phist$density)),
                                     col="darkblue", border = "white", 
                                     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                                legend("topright", inset = 0.025, 
                                       legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))), 
                                       bty = "n", cex = 1.5, text.col ="darkblue", text.font = 2)
                        }
                        if (input$dist == "rbeta"){
                                hist(pop, main=distname, breaks=50, xlab="", freq=FALSE, 
                                     ylim=c(0, max(pdens$y, phist$density)+.5), col="darkblue", border = "white", 
                                     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                                legend("topleft", inset = 0.025, 
                                       legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))), 
                                       bty = "n", cex = 1.5, text.col = "darkblue", text.font = 2)
                        }
                        lines(pdens, col="darkblue", lwd=3)
                        box()
                }
        })
        

        # text
        output$CLT.descr = renderText({
                
                L = NULL ; U = NULL ; error = FALSE
                
                if (input$dist == "runif"){
                        L = ifelse(is.null(input$min),0,input$min) ; U = ifelse(is.null(input$max),0,input$max)
                        if (L > U){
                                error = TRUE
                        }
                }
                
                if (error)
                        paste0()
                
                else{
                        pop = parent()
                        m_pop =  round(mean(pop),2)
                        s_pop = round(sd(pop),2)
                        
                        n = input$n
                        se=round(s_pop/sqrt(n),2)
                        paste("According to the Central Limit Theorem (CLT), the distribution of sample means 
          (the sampling distribution) should be nearly normal. The mean of 
          the sampling distribution should be approximately equal to the population mean (", m_pop, ") 
          and the standard error (the standard deviation of
          sample means) should be approximately equal to the SD of the population divided by square root of
          sample size (", s_pop,
                              "/sqrt(",n, ") =", se,").")
                }
        })
        
         # plot 3
        output$sampling.dist = renderPlot({
                
                L = NULL ; U = NULL ; error = FALSE
                
                if (input$dist == "runif"){
                        L = ifelse(is.null(input$min),0,input$min) ; U = ifelse(is.null(input$max),0,input$max)
                        if (L > U){
                                error = TRUE
                        }
                }
                
                if (error)
                        return
                
                else{
                        
                        distname = switch(input$dist,
                                          rnorm = "normal population",
                                          rlnorm  = "right skewed population",
                                          rbeta = "left skewed population",
                                          runif = "uniform population")   
                        
                        
                        n = input$n
                        k = input$k
                        
                        pop = parent()
                        
                        m_pop =  round(mean(pop),2)
                        sd_pop = round(sd(pop),2)
                        
                        ndist = colMeans(samples())
                        
                        m_samp =  round(mean(ndist),2)
                        sd_samp = round(sd(ndist),2)
                        
                        ndens=density(ndist)
                        nhist=hist(ndist, plot=FALSE)
                        
                        myhist <- hist(ndist, breaks=50, main = paste("Sampling distribution:\nDistribution of means of ", k, 
                                                 " random samples, each\nconsisting of ", n, 
                                                 " observations from a ", distname, sep=""),              
                             xlab="Sample means", freq=FALSE,
                             ylim=c(0, max(ndens$y, nhist$density)),
                             col="darkgreen", border = "white", 
                             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                        curve(dnorm(x, mean=m_samp, sd=sd_samp), col="darkblue", lwd=2, add=TRUE, yaxt="n")
                        legend_pos = ifelse(m_samp > 40, "topleft", "topright")
                        legend(legend_pos, inset = 0.025, 
                               legend=bquote(atop("mean of " ~ bar(x)==.(m_samp),"sd of " ~ bar(x) ~ "(SE)" ==.(sd_samp))), 
                               bty = "n", cex = 1.5, text.col = "darkgreen", text.font = 2)
                        lines(ndens, col="darkgreen", lwd=3)
                        box()
                }
        })
        


                })