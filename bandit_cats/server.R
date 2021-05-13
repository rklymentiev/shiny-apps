library(shiny)
library(tidyverse)
# library(ggcats)
library(png)
library(grid)


shinyServer(function(input, output) {
    
    n <- eventReactive(input$go, {
        input$n
    })
    epsilon <- eventReactive(input$go, {
        input$epsilon
    })
        
    true_rates <- eventReactive(input$go, {
        c(input$rate_A, input$rate_B, input$rate_C)
    })
    
    seed <- eventReactive(input$go, {
        input$seed
    })
    
    gen_data <- reactive({
        if (seed()){
            set.seed(1)
        }
        
        reward_memory <- list(pop = c(), grumpy = c(), toast = c())
        choice_memory <- list(pop = c(), grumpy = c(), toast = c())
        
        # random algorithm
        for (i in 1:n()) {
            choice <- rdunif(n = 1, b = 3, a = 1)
            success <- rbernoulli(n = 1, p = true_rates()[choice])
            choice_memory$pop <- c(choice_memory$pop, choice)
            reward_memory$pop <- c(reward_memory$pop, as.integer(success))
        }
        
        # epsilon greedy algorithm
        eg_memory <- c(0,0,0)
        init_choice <- rdunif(n = 1, b = 3, a = 1)
        success <- rbernoulli(n = 1, p = true_rates()[init_choice])
        eg_memory[init_choice] <- eg_memory[init_choice] + as.integer(success)

        for (i in 1:n()) {
            explore <- rbernoulli(n = 1, p = epsilon())
            if (explore) {
                choice <- rdunif(n = 1, b = 3, a = 1)
            } else {
                choice <- which.max(eg_memory)
            }
            success <- rbernoulli(n = 1, p = true_rates()[choice])
            eg_memory[choice] <- eg_memory[choice] + as.integer(success)

            choice_memory$grumpy <- c(choice_memory$grumpy, choice)
            reward_memory$grumpy <- c(reward_memory$grumpy, as.integer(success))
        }
        
        # thompson sampling

        prior_alpha <- c(1,1,1)
        prior_beta <- c(1,1,1)

        init_choice <- rdunif(n = 1, b = 3, a = 1)
        success <- rbernoulli(n = 1, p = true_rates()[init_choice])

        prior_alpha[init_choice] <- prior_alpha[init_choice] + 1
        prior_beta[init_choice] <- prior_beta[init_choice] + 1 - as.integer(success)

        for (i in 1:n()) {

            rb_1 <- rbeta(n = 1, shape1 = prior_alpha[1], shape2 = prior_beta[1])
            rb_2 <- rbeta(n = 1, shape1 = prior_alpha[2], shape2 = prior_beta[2])
            rb_3 <- rbeta(n = 1, shape1 = prior_alpha[3], shape2 = prior_beta[3])

            choice <- which.max(c(rb_1, rb_2, rb_3))
            success <- rbernoulli(n = 1, p = true_rates()[choice])

            choice_memory$toast <- c(choice_memory$toast, choice)
            reward_memory$toast <- c(reward_memory$toast, as.integer(success))

            prior_alpha[choice] <- prior_alpha[choice] + 1
            prior_beta[choice] <- prior_beta[choice] + 1 - as.integer(success)
        }

        return(list(choice_memory = choice_memory, reward_memory = reward_memory))
        
    })
    
    choice_totals <- reactive({
        
        choice_totals <- data.frame(table(gen_data()$choice_memory$pop)) %>%
        left_join(
            data.frame(table(gen_data()$choice_memory$grumpy)), 
            by = "Var1", suffix = c("_pop", "_grumpy")) %>%
        left_join(
            data.frame(table(gen_data()$choice_memory$toast)), 
            by = "Var1") %>%
        rename(Freq_toast = Freq, choice = Var1) %>%
        pivot_longer(cols = !choice, names_to = "cat") %>%
        mutate(cat = str_remove(cat, "Freq_"),
               choice =  case_when(
                   choice == 1 ~ "A",
                   choice == 2 ~ "B",
                   choice == 3 ~ "C"))
    
        return(choice_totals)
    })
    
    rewards_df <- reactive({
        rewards_df <- data.frame(
            trial = 1:n(), 
            toast = cumsum(gen_data()$reward_memory$toast),
            pop = cumsum(gen_data()$reward_memory$pop),
            grumpy = cumsum(gen_data()$reward_memory$grumpy)) %>% 
            pivot_longer(cols = !trial,
                         names_to = "cat_name") 
        
        # rewards_df$cat <- NA
        # rewards_df$cat[which(rewards_df$cat_name == "toast")] <- "toast"
        # rewards_df$cat[which(rewards_df$cat_name == "grumpy")] <- "grumpy"
        # rewards_df$cat[which(rewards_df$cat_name == "pop")] <- rep(c("pop_close", "pop"), 
        #                                                            length(rewards_df$cat[which(rewards_df$cat_name == "pop")])/2)
        return(rewards_df)
    })
    
    total_rewards <- reactive({
        total_rewards <- rewards_df() %>% 
            group_by(cat_name) %>% 
            summarise(s = last(value))
    })
        
    output$picks_plot <- renderPlot({
        
        toast <- readPNG("toast.png")
        pop <- readPNG("pop.png")
        grumpy <- readPNG("grumpy.png")
        toast_img <- rasterGrob(toast, interpolate=FALSE)
        pop_img <- rasterGrob(pop, interpolate=FALSE)
        grumpy_img <- rasterGrob(grumpy, interpolate=FALSE)
        
        ggplot() +
            geom_bar(
                data=choice_totals(),
                aes(x = cat, y = value, fill = choice),
                color = "black",
                stat="identity",
                position=position_dodge()) +
            annotation_custom(grumpy_img, xmin = 0.5, xmax = 1.5, ymin=n(), ymax=n()+n()*0.2) +
            annotation_custom(pop_img, xmin = 1.5, xmax = 2.5, ymin=n(), ymax=n()+n()*0.2) +
            annotation_custom(toast_img, xmin = 2.5, xmax = 3.5, ymin=n(), ymax=n()+n()*0.2) +
            # geom_cat(
            #     aes(
            #         cat = c("pop","grumpy", "toast"),
            #         y = c(n()+100,n()+100,n()+100),
            #         x = c("pop","grumpy", "toast")),
            #     size = 4) +
            ylim(0, n()+n()*0.2) +
            labs(
                title = "Total Picks",
                y = "Count") +
            scale_fill_brewer(palette="PuOr") +
            theme_linedraw() +
            theme(
                plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
                legend.title = element_text(face = "bold", size = 16),
                legend.text = element_text(face = "bold", size = 12),
                axis.title.y = element_text(face = "bold", size = 16),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
    })
    
    output$reward_plot <- renderPlot({
        
        toast <- readPNG("toast.png")
        pop <- readPNG("pop.png")
        grumpy <- readPNG("grumpy.png")
        toast_img <- rasterGrob(toast, interpolate=FALSE)
        pop_img <- rasterGrob(pop, interpolate=FALSE)
        grumpy_img <- rasterGrob(grumpy, interpolate=FALSE)
        
        ggplot() +
            geom_line(
                data = rewards_df(), 
                aes(
                    x = trial, y = value, 
                    group = cat_name, color = cat_name), 
                size = 2) +
            # geom_cat(aes(cat = cat), size = 5) +
            # transition_reveal(trial) +
            # geom_cat(
            #     aes(
            #         cat = total_rewards()$cat_name, 
            #         y = total_rewards()$s, 
            #         x = rep(n(),3)), 
            #     size = 4) + 
            xlim(0, n()*1.05) +
            labs(
                title = "Cumulative Reward by Trial",
                x = "Trial",
                y = "Reward") +
            ylim(0, max(total_rewards()$s)+30) +
            annotation_custom(
                grumpy_img, xmin = n()*0.95, xmax = n()*1.05, 
                ymin=total_rewards()$s[which(total_rewards()$cat_name == "grumpy")]*0.85, 
                ymax=total_rewards()$s[which(total_rewards()$cat_name == "grumpy")] + n()*0.1) +
            annotation_custom(
                pop_img, xmin = n()*0.95, xmax = n()*1.05, 
                ymin=total_rewards()$s[which(total_rewards()$cat_name == "pop")]*0.9, 
                ymax=total_rewards()$s[which(total_rewards()$cat_name == "pop")] + n()*0.1) +
            annotation_custom(
                toast_img, xmin = n()*0.95, xmax = n()*1.05, 
                ymin=total_rewards()$s[which(total_rewards()$cat_name == "toast")]*0.9, 
                ymax=total_rewards()$s[which(total_rewards()$cat_name == "toast")] + n()*0.1) +
            theme_linedraw() +
            theme(
                legend.position = "none",
                plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
                axis.title.x = element_text(face = "bold", size = 16),
                axis.title.y = element_text(face = "bold", size = 16))
    })
    
    

})