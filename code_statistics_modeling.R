## ----setup, include=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ---- results="hide", message=FALSE, warning=FALSE, cache=FALSE-------------------

# PRELIMINARY ------------------------------------------------------------------

# Theme for plotting
theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.margin=margin(0, 0, 0, 0),
          legend.box.margin=margin(-7,-7,-7,-7), 
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"))
}

# Load the packages
sensobol::load_packages(c("sensobol", "tidyverse", "data.table", "scales"))



## ---- fig.height=3, fig.width=3---------------------------------------------------

# FIGURE 1 #####################################################################

# Read datasets ---------------------------------
dt <- fread("dataset1.csv")[, type:= "one"]
dt2 <- fread("dataset2.csv")[, type:= "two"]
dt3 <- fread("dataset3.csv")[, type:= "three"]

# Plot ------------------------------------------
rbind(dt, dt2, dt3) %>%
  ggplot(., aes(V1, V2, group = type, color = type)) +
  geom_line(linewidth = 1) +
  labs(x = "Model complexity", y = "Model error") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme_AP() +
  theme(legend.position = "none") +
  annotate("text", x = 0.15, y = 0.3, label = "Error due \n to bias", size = 3) +
  annotate("text", x = 0.85, y = 0.27, label = "Error due to \n parametrization", size = 3) +
  annotate("text", x = 0.5, y = 0.35, label = "Total error", size = 3)


## ---- fig.height=2.2, fig.width=5-------------------------------------------------

# FIGURE 2 ####################################################################

# Settings --------------------------------------
N <- 2^8
params <- paste("$x_", 1:2, "$", sep = "")
type <- c("R", "LHS", "QRN")

# Sample matrices -------------------------------
mat <- lapply(type, function(x) 
  data.table(sobol_matrices(matrices = "A", N = N, params = params, type = x)))
names(mat) <- type

# Plot ------------------------------------------
dt.plot <- rbindlist(mat, idcol = "type")
ggplot(dt.plot, aes(`$x_1$`, `$x_2$`)) +
  geom_point() +
  facet_wrap(~type) + 
  scale_x_continuous(breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  theme_AP()

