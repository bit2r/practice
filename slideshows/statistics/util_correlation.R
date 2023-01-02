
library(tidyverse)

sd_pop <- function(x){
  
  sqrt(sum((x - mean(x))^2)/(length(x)))  
  
}

create_x_y <- function(num = 20, 
                       spread_x = 20,
                       relationship = .1, 
                       noise = 3
){
  
  tibble(x = rnorm(num, sd = spread_x) + 50 ) %>% 
    mutate(y = relationship * x + 
             rnorm(num, sd = noise)) %>% 
    mutate(mean_x = mean(x)) %>% 
    mutate(mean_y = mean(y)) %>% 
    mutate(area = (x - mean_x)*(y - mean_y)) %>% 
    mutate(mean_area = mean(area)) %>% 
    mutate(quasi_mean_area = area/(n() - 1)) %>%
    mutate(sd_x_sample = sd(x)) %>% 
    mutate(sd_x = sd_pop(x)) %>% 
    mutate(sd_y_sample = sd(y)) %>% 
    mutate(sd_y = sd_pop(y)) %>% 
    mutate(some_x = sd_x) %>% 
    mutate(some_x_sample = sd_x_sample) %>% 
    mutate(some_y = mean_area/some_x) %>% 
    mutate(some_y_sample = sd_y_sample) 
  
}


create_x_y_ttest <- function(num = 20, 
                             spread_x = 20,
                             relationship = .1, 
                             noise = 3
){
  
  tibble(x = sample(c(0,1), num, replace = T) ) %>% 
    mutate(y = relationship * x + 
             rnorm(num, sd = noise)) %>% 
    mutate(mean_x = mean(x)) %>% 
    mutate(mean_y = mean(y)) %>% 
    mutate(area = (x - mean_x)*(y - mean_y)) %>% 
    mutate(mean_area = mean(area)) %>% 
    mutate(quasi_mean_area = area/(n() - 1)) %>%
    mutate(sd_x_sample = sd(x)) %>% 
    mutate(sd_x = sd_pop(x)) %>% 
    mutate(sd_y_sample = sd(y)) %>% 
    mutate(sd_y = sd_pop(y)) %>% 
    mutate(some_x = sd_x) %>% 
    mutate(some_x_sample = sd_x_sample) %>% 
    mutate(some_y = mean_area/some_x) %>% 
    mutate(some_y_sample = sd_y_sample) 
  
}


data_calc_cor <- function(data){
  
  with(cor(x, y), data = data)
  
}

data_create_scatterplot <- function(data, background_color = "palegreen4"){
  
  data %>% 
    ggplot() + 
    theme(legend.position = c(.15, .9)) +
    aes(x = x) + 
    aes(y = y) +
    theme(rect = element_rect(fill = 
                                background_color)) + 
    theme(text = element_text(color = "white", 
                              face = "italic", 
                              size = 15)) +
    theme(panel.background = 
            element_rect(fill = background_color)) +
    theme(legend.key = element_blank()) + 
    theme(legend.title = element_blank()) +
    theme(axis.text = 
            element_text(color = "white")) +
    theme(axis.ticks = 
            element_line(color = "white")) +
    labs(title = NULL) +
    theme(panel.grid = element_blank()) +
    geom_point(size = 3, pch = 21, col = "white", 
               fill = "white", lwd = 3, alpha = .6) + 
    labs(caption = "Statistical Visualization: Gina Reynolds @EvaMaeRey ") 
  
  
}




plot_draw_mean_x <- function(plot){
  
  plot + 
    # 1. mean of x
    geom_rug(aes(y = NULL), col = "white") +
    geom_vline(aes(xintercept = mean(x)), 
               lty = "dashed", col = "white")  
  
  
}


plot_draw_mean_y <- function(plot){
  
  plot + 
    # 2. mean of y
    geom_rug(col = "white", aes(x = NULL)) +
    geom_hline(aes(yintercept = mean(y)), 
               lty = "dashed", col = "white")
  
}


plot_draw_differences_x <- function(plot){
  
  plot + 
    # difference xi mean x
    scale_color_manual(
      breaks = c(FALSE, TRUE), 
      label = c("Negative", "Positive"), 
      values = c("orchid4", "lightgoldenrod3") )  + 
    geom_segment(
      aes(col = x > mean(x), 
          xend = mean(x), yend = y), 
      arrow = arrow(ends = "first", 
                    length = unit(0.1, "inches"))) +
    labs(col = "")
  
  
}


plot_draw_differences_y <- function(plot){
  
  plot +
    # difference yi mean y
    geom_segment(aes(col = y > mean(y), 
                     xend = x, yend = mean(y)), 
                 arrow = arrow(ends = "first", 
                               length = unit(0.1, "inches"))) 
  
}


plot_multiply_differences <- function(plot){
  
  plot + 
    # multiply differences
    aes(fill = area > 0) +
    geom_rect(aes(xmin = mean(x), ymin = mean(y),  
                  ymax = y, xmax = x), 
              alpha = .2  ) +
    labs(fill = "") +
    scale_fill_manual(breaks = c(FALSE, TRUE), 
                      label = c("Negative", "Positive"),  
                      values = c("orchid4", "lightgoldenrod3")) 
  
}

plot_take_average_rectangle <- function(plot, title = "Picture of Covariance"){
  
  plot +      
    # Average areas 
    geom_rect(aes(
      xmin = mean(x),
      ymin = mean(y),
      ymax = mean(some_y) + mean(y),
      xmax = mean(some_x) + mean(x)
    ),
    color = "orange",
    linetype = "dotted",
    fill = "orange",
    lwd = 1.5,
    alpha = .2) +  #BREAK
    labs(title = title)
  
}


plot_edge_of_rectangle <- function(plot, title = "Picture of Standard Deviation"){
  
  plot +      
    # Average areas 
    geom_segment(aes(
      x = mean(x),
      y = mean(y),
      yend = mean(y),
      xend = mean(some_x) + mean(x)
    ),
    color = "firebrick",
    lwd = 2) +  #BREAK
    labs(title = title)
  
}


plot_normalize_x <- function(plot, data){
  
  plot + 
    # Pearsons's Correlation 
    scale_x_continuous(
      sec.axis = sec_axis(~ (. -mean(data$x))/ sd_pop(data$x), 
                          name = "sd x")) +
    geom_vline(xintercept = mean(data$x) + sd_pop(data$x), 
               lty = "dashed", color = "pink") +
    geom_vline(xintercept = mean(data$x) - sd_pop(data$x), 
               lty = "dashed", color = "pink") 
  
}

plot_normalize_y <- function(plot, data, title = "Picture of covariance\n*and* Pearson correlation coefficient"){
  
  plot + 
    scale_y_continuous(
      sec.axis = sec_axis(~ (. -mean(data$y))/ sd_pop(data$y), 
                          name = "sd y")) +
    geom_hline(
      yintercept = mean(data$y) + sd_pop(data$y), 
      lty = "dashed", color = "pink") +
    geom_hline(
      yintercept = mean(data$y) - sd_pop(data$y), 
      lty = "dashed", color = "pink") + 
    labs(title = title) +
    coord_fixed(ratio =  sd_pop(data$x)/sd_pop(data$y) )
  
}


plot_max_correlation <- function(plot, data){
  
  plot + 
    annotate(geom = "rect",
             alpha = .3, 
             xmin = data$mean_x[1], xmax = data$mean_x[1] + data$sd_x[1],
             ymin = data$mean_y[1], ymax = data$mean_y[1] + data$sd_y[1],
             fill = "orange")
  
}


plot_change_ylab_to_x <- function(plot){
  
  plot +
    labs(y = "x")
  
}

plot_correct_aspect_ratio <- function(plot){
  
  plot + 
    coord_equal()
  
}

# 마크다운 ----------

cov_equation <- c("",
                  "## $$\\mu_x$$", 
                  "## $$\\mu_y$$", 
                  "## $$x_i-\\mu_x$$",
                  "## $$y_i-\\mu_y$$",
                  "## $$\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)$$",
                  "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n}$$"
)

var_equation <- c("",
                  "## $$\\mu_x$$", 
                  "## $$\\mu_x$$", 
                  "## $$x_i-\\mu_x$$",
                  "## $$x_i-\\mu_x$$",
                  "## $$\\sum_{i=1}^n (x_i-\\mu_x)^2$$",
                  "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)^2}{n}$$"
)


sd_equation <- c("## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)^2}{n}$$",
                 "## $$\\sqrt\\frac{\\sum_{i=1}^n (x_i-\\mu_x)^2}{n}$$")

cov_to_cor_equation <- c("## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n}$$",
                         "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n*\\sigma_x}$$",
                         "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n*\\sigma_x\\sigma_y}$$",
                         "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n*\\sigma_x\\sigma_y}$$")



