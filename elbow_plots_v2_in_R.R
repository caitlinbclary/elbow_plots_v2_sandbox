# Evidence and indicator plots (elbow plots) 
# https://github.com/caitlinbclary/elbow_plots_v2_sandbox 

library(dplyr)
library(tidyr)
library(ggplot2)

savedir <- "C:/Users/clary/Biostat Global Dropbox/Caitlin Clary/CBC Projects/Elbow plots 2024 - R/"
datadir <- paste0(savedir, "Test data/")

# TO DO 
# - Parameterize styling 
# - Value checks 
# - Cases and spaces 
# - Legend 

# Set up for fonts 
extrafont::loadfonts()

dat <- read.csv(paste0(datadir, "2024-04-24 Dale testing a crude plot data structure.csv"))

xaxis_max <- 750 

# Set up value labels for Y axis 
doselist <- dat |> 
  select(order, dose) |>  unique() |>  
  arrange(order) |>  pull(dose)

ymax <- max(dat$order, na.rm = TRUE)

dat <- dat |> 
  mutate(
    yregister = order + 0.05,
    ycard = order - 0.05,
    tabcard = ifelse(crude_by_card == "y", "C", "."),
    tabreg = ifelse(crude_by_register == "y", "R", "."),
    tabrec = ifelse(crude_by_recall == "y", "V", "."),
    tabany = ifelse(crude_by_anysource == "y", "Y", "N")
  ) 

for(i in seq_along(unique(dat$respid))){
  
  dat_i <- dat |> filter(respid %in% i)|> 
    arrange(order)
  
  respid_i <- sort(unique(dat_i$respid))
  respid_i_save <- stringr::str_pad(respid_i, width = 6, side = "left", pad = "0")
  
  title_i <- stringr::str_squish(
    sort(unique(
      dat_i[!is.na(dat_i$title) & dat_i$title != "",]$title))[1])
  
  subtitle_i <- stringr::str_squish(
    sort(unique(
      dat_i[!is.na(dat_i$subtitle) & dat_i$subtitle != "",]$subtitle))[1])
  
  footnote_i <- stringr::str_squish(
    sort(unique(
      dat_i[!is.na(dat_i$footnote) & dat_i$footnote != "",]$footnote))[1])
  
  xtitle_i <- stringr::str_squish(
    sort(unique(
      dat_i[!is.na(dat_i$xtitle) & dat_i$xtitle != "",]$xtitle))[1])
  
  dat_i_visit <- dat_i |> 
    select(respid, card_age, register_age) |> 
    pivot_longer(cols = c(card_age, register_age),
                 values_to = "visit_age") |> 
    select(visit_age) |> unique() |> 
    filter(!visit_age == max(dat_i$age_at_survey, na.rm = TRUE),
           !is.na(visit_age)) 
  
  dat_i_ages <- c(dat_i$card_age, dat_i$register_age, dat_i$age_at_survey) |> 
    unique() |> sort() # note that sort drops NAs 
  
  dat_i_ages <- data.frame(age = dat_i_ages) |> 
    mutate(ypos = rep(c(0.15, 0.45), length.out = n()))
  
  ggplot(data = dat_i, aes(x = nominal_age, y = order)) + 
    # Card and register vx visit ages 
    geom_vline(
      data = dat_i_visit, 
      aes(xintercept = visit_age),
      color = "grey80", linewidth = 0.25
    ) + 
    # Age at survey  - dashed line 
    geom_vline(
      data = filter(dat_i, !is.na(age_at_survey)),
      aes(xintercept = age_at_survey),
      color = "grey80", lty = "94", #"longdash", 
      linewidth = 0.25
    ) +
    # Simulate axis lines (suppressed from theme)
    geom_segment(
      data = dat_i,
      aes(x = -10, y = order, xend = xaxis_max + 10, yend = order),
      color = "grey80", linewidth = 0.25) +
    # Ages at vx visits and survey 
    geom_text(
      data = dat_i_ages,
      aes(x = age, 
          y = ypos, 
          label = age),
      size = 2.5, family = "Lato"
    ) +
    # Recall - faded red line 
    geom_rect(
      data = filter(dat_i, recall == "y"), 
      aes(ymin = order - 0.05, 
          ymax = order + 0.05,
          xmin = -5, xmax = xaxis_max + 5
      ),
      fill = "#FFBFBF") +
    # Card tick - faded blue line 
    geom_rect(
      data = filter(dat_i, card_tick == "y"), 
      aes(ymax = order - 0.05, 
          ymin = order - 0.15,
          xmin = -5, xmax = xaxis_max + 5
      ),
      fill = "#BFBFFF") +
    # Register tick - faded green line 
    geom_rect(
      data = filter(dat_i, register_tick == "y"), 
      aes(ymin = order + 0.05, 
          ymax = order + 0.15,
          xmin = -5, xmax = xaxis_max
      ),
      fill = "#BFDFBF") +
    # Schedule indicators 
    geom_point(
      data = dat_i, 
      aes(x = nominal_age, y = order), 
      shape = 108,
      # pch = "|", 
      size = 3
    ) + 

    # Points - Card dates
    geom_point(
      data = filter(dat_i, !is.na(card_age)),
      aes(x = card_age, y = ycard), color = "blue" #"#FF7F00"
    ) + 
    
    # Points - Register dates
    geom_point(
      data = filter(dat_i, !is.na(register_age)),
      aes(x = register_age, y = yregister), color = "#FF7F00" # "blue"
    ) + 
    
    theme_minimal() + 
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, family = "Lato", size = 16),
      plot.subtitle = element_text(hjust = 0.5, family = "Lato", size = 13),
      plot.caption = element_text(hjust = 0, family = "Lato"),
      axis.title = element_text(, family = "Lato"),
      axis.text = element_text(color = "black", family = "Lato"),
      axis.line = element_line(color = "black",
                               linewidth = .7)
    ) + 
    xlab(xtitle_i) + ylab("") + 
    labs(
      title = title_i,
      subtitle = subtitle_i,
      caption = footnote_i
    ) + 
    geom_text(
      aes(x = xaxis_max + 25, y = order, label = tabcard),
      size = 3, family = "Lato"
    ) + 
    geom_text(
      aes(x = xaxis_max + 50, y = order, label = tabreg),
      size = 3, family = "Lato"
    ) + 
    geom_text(
      aes(x = xaxis_max + 75, y = order, label = tabrec),
      size = 3, family = "Lato"
    ) + 
    geom_text(
      aes(x = xaxis_max + 100, y = order, label = tabany),
      size = 3, family = "Lato"
    ) + 
    geom_text(
      data = filter(dat_i, recall %in% "dnk"),
      aes(x = (xaxis_max/2), y = order + 0.2, label = "DNK"),
      size = 2.5, family = "Lato"
    ) + 
    scale_y_continuous(
      breaks = dat_i$order, labels = dat_i$dose,
      limits = c(min(dat_i$order - 1), max(dat_i$order + 0.2))
    ) +
    scale_x_continuous(
      breaks = seq(0, xaxis_max, by = 100)
    ) 
  
  ggsave(
    filename = paste0(savedir,
                      "crude_plot_for_respid_", respid_i_save, ".png"),
    width = 2000, height = 1400,  units = "px"
  )
}
