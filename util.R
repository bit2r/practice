library(showtext)
library(tidyverse)
library(NHSRtheme)

xaringanExtra::use_panelset()
# library(nhsrtheme)
# library(NHSRdatasets)
# set default options

options(
  htmltools.dir.version = FALSE, 
  htmltools.preserve.raw = FALSE,
  tibble.width = 60, tibble.print_min = 6,
  crayon.enabled = TRUE
)

knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE,
                      comment="", digits = 3, tidy = FALSE, prompt = FALSE, fig.align = 'center',
                      fig.width = 7.252,
                      fig.height = 4,
                      dpi = 300)

# uncomment the following lines if you want to use the NHS-R theme colours by default
scale_fill_continuous <- partial(scale_fill_nhs, discrete = FALSE)
scale_fill_discrete <- partial(scale_fill_nhs, discrete = TRUE)
scale_colour_continuous <- partial(scale_colour_nhs, discrete = FALSE)
scale_colour_discrete <- partial(scale_colour_nhs, discrete = TRUE)

showtext::showtext_auto()

# GGPLOT THEME ------------------------------------------------------------

theme_election <- function(..., font = "나눔바른펜") {
  
  theme_bw(base_family = font)  %+replace%
    
    theme(
      
      #grid elements
      panel.grid.major.x = element_line(color='gray75'),
      panel.grid.minor.x = element_blank(),
      # axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 30,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        hjust = 0,                #left align        
        size = 17),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 12,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 20),               #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10),
        size = rel(1.5),
        family = font, face="bold"),
      
      axis.text.y = element_text(
        size = rel(1.5),
        colour = "gray35", 
        family = font, face="bold"),
      
      strip.text.x = element_text(
        size = rel(1.5), 
        colour = "black", family = font, face="bold" ),
      
      strip.background = element_rect(fill="gray95"),
      
      legend.position = "top",
      legend.title=element_text(size=12),
      legend.text=element_text(size=10)
      
    )
}
