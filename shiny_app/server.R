library(shiny)
library(tidyverse)
library(patchwork)

shinyServer(function(input, output) {
    
    cor_data <- function(n, r) {
        x <- rnorm(n)
        
        C <- matrix(r, nrow = 2, ncol = 2)
        diag(C) <- 1
        
        C <- chol(C)
        
        y <- rnorm(n)
        df <- cbind(x, y)
        
        df <- df %*% C
        
        return(df)
    }
    
    simulate <- function(n, r, p){
        df <- cor_data(n, r)
        x <- df[ , 1]
        y <- df[ , 2]
        
        x_ms <- ifelse(x < quantile(x, probs = p), 0, ifelse(x > quantile(x, probs = 1 - p), 1, NA))
        
        return(c(cor_p = cor.test(x, y)$p.value,
                 t_p = t.test(y ~ x_ms, var.equal = TRUE)$p.value))
    }
    
    monte_carlo_simulation <- function(n, r, p, n_sims = 1000){
        replicate(n_sims, simulate(n, r, p)) %>%
            t() %>%
            as_tibble() %>%
            mutate(sample_size = n) %>%
            mutate(correlation = r)
    }
    
    plot_cor_data <- function(n, r, alpha = 0.1){
        cor_data(n, r) %>% 
            as_tibble() %>% 
            rename(x = V1,
                   y = V2) %>%
            ggplot(aes(x, y)) + 
            geom_point(alpha=alpha) + 
            geom_smooth(method = "lm") +
            labs(x = "x",
                 y = "y",
                 title = "Relationship in population")
    }
    
    plot_results_bar <- function(tbl, n, r){
        tbl %>%
            filter(correlation %in% r) %>%
            filter(sample_size == n) %>%
            group_by(sample_size, correlation) %>%
            summarize(cor_error = mean(cor_p < 0.05),
                      t_error = mean(t_p < 0.05)) %>%
            ungroup() %>%
            gather(test, error_rate, -sample_size, -correlation) %>%
            mutate(test = fct_recode(test, 
                                     "Correlation" = "cor_error",
                                     "Split and t-test" = "t_error")) %>%
            mutate(correlation = paste0("Correlation = ", correlation)) %>%
            ggplot(aes(test, error_rate, fill = test)) +
            geom_col(position = "dodge") +
            facet_wrap(~correlation) +
            labs(x = NULL,
                 y = "Percent of sig. results",
                 fill = NULL) +
            theme(text = element_text(size=18),
                  legend.position = "none") +
            scale_y_continuous(label = scales::percent_format()) +
            expand_limits(y = c(0, 1)) +
            geom_text(aes(label = round(error_rate * 100, 2)), nudge_y = .05, size = 5)
    }
    
    plot_p_hist <- function(tbl, n, r){
        tbl %>%
            filter(correlation == r) %>%
            filter(sample_size == n) %>%
            gather(test, error_rate, -sample_size, -correlation) %>%
            mutate(test = fct_recode(test, 
                                     "Correlation" = "cor_p",
                                     "Split and t-test" = "t_p")) %>%
            ggplot(aes(error_rate)) +
            geom_histogram(bins = 100) +
            facet_wrap(~test, ncol = 1) +
            geom_vline(xintercept = 0.05, color = "red") +
            labs(x = "p value",
                 y = "Count") +
            theme(text = element_text(size=18)) +
            scale_y_continuous(label = scales::comma_format())
    }

    output$plot <- renderPlot({
        n_sims <- input$n_sims
        r <- ifelse(abs(input$pop_cor) == 1, .99, input$pop_cor)
        n <- input$sample_size
        p <- input$split
        
        
        results <- monte_carlo_simulation(n = n, r = r, p = p, n_sims = n_sims)
        
        p1 <- plot_cor_data(n = 10000, r = r)
        
        p2 <- plot_results_bar(results, n = n, r = r)
        
        p3 <- plot_p_hist(results, n = n, r = r)
        
        (p1 / p2) | p3
    })

})
