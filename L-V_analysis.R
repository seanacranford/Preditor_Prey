##=============================================================================================================
##
## Study of Preditor-Prey Model
##
##=============================================================================================================
##
## References:
## https://mc-stan.org/users/documentation/case-studies/lotka-volterra-predator-prey.html#data-lynx-and-hare-pelts-in-canada
## https://github.com/stan-dev/example-models/blob/master/knitr/lotka-volterra/lotka-volterra-predator-prey.Rmd
## https://rdrr.io/github/jarioksa/ecostudy/man/lynxhare.html
##
##=============================================================================================================

library(tidyverse)
library(gridExtra)
library(reshape)
library(tufte)

library(knitr)
knitr::opts_chunk$set(cache = TRUE)
knitr::opts_chunk$set(tidy = FALSE, cache.extra = packageVersion('tufte'))
knitr::opts_chunk$set(comment = "")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE))

ggtheme_tufte <- function() {
  theme(plot.background =
          element_rect(fill = "#fffff8",
                       colour = "#fffff8",
                       size = 0.5,
                       linetype = "solid"),
        plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =
          element_rect(fill = "#fffff8",
                       colour = "#fffff8",
                       size = 0.5,
                       linetype = "solid"),
        panel.grid.major = element_line(colour = "white", size = 1, linetype="dashed"),
        # blank(),
        panel.grid.minor = element_blank(),
        legend.box.background =
          element_rect(fill = "#fffff8",
                       colour = "#fffff8",
                       linetype = "solid"),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Palatino", size = 16),
        axis.title.x = element_text(family = "Palatino", size = 20,
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(family = "Palatino", size = 18,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),
        strip.background = element_rect(fill = "#fffff8",
                                        colour = "#fffff8",
                                        linetype = "solid"),
        strip.text = element_text(family = "Palatino", size = 16),
        legend.text = element_text(family = "Palatino", size = 16),
        legend.title = element_text(family = "Palatino", size = 16,
                                    margin = margin(b = 5)),
        legend.background = element_rect(fill = "#fffff8",
                                         colour = "#fffff8",
                                         linetype = "solid"),
        legend.key = element_rect(fill = "#fffff8",
                                  colour = "#fffff8",
                                  linetype = "solid")
  )
}

printf <- function(msg = "%5.3f", ...) {
  cat(sprintf(msg, ...))
}

##=============================================================================================================

## import data
lynx_hare_df <-
  read.csv("From_Stan/hudson-bay-lynx-hare.csv",
           comment.char="#")

##=============================================================================================================

##format data
lynx_hare_melted_df <- melt(as.matrix(lynx_hare_df[, 2:3]))
colnames(lynx_hare_melted_df) <- c("year", "species", "pelts")
lynx_hare_melted_df$year <-
  lynx_hare_melted_df$year +
  rep(1899, length(lynx_hare_melted_df$year))
knitr::kable(lynx_hare_melted_df[c(1:2, 21:22, 41:42), ],
             full_width = FALSE,
             col.names = c('year', 'species', 'pelts in thousands'),
             align = c('r', 'c', 'r'),
             caption = "Example rows (with their indexes) from the long-form data frame
                        for number of pelts taken by the Hudson's Bay Company
                        in the years 1900 to 1920 (in thousands)."
)

##=============================================================================================================

## plots
ggplot(data = lynx_hare_melted_df,
       aes(x = year, y = pelts, color = species)) +
  geom_vline(xintercept = 1900, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(size = 0.75) +
  geom_point(size = 1.5) +
  ylab("pelts (thousands)") +
  ggtheme_tufte()

ggplot(data = lynx_hare_df,
       aes(x = Lynx, y = Hare, color = Year)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  geom_path(arrow = arrow(angle = 15, length = unit(0.15, "inches"))) +
  geom_point(size = 1.5) +
  xlab("lynx pelts (thousands)") +
  ylab("hare pelts (thousands)") +
  ggtheme_tufte() +
  theme(legend.position="none")







