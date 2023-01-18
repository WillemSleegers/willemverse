
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)

# stat_vhistogram() -------------------------------------------------------

# Create some data
set.seed(1)

data <- tibble(
  condition = sample(c("control", "treatment"), 50, replace = TRUE),
  outcome = sample(1:7, 50, replace = TRUE)
)

data$outcome[1] <- NA

# Test out the stat
ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram()

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5)

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5, color = "black")

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5, color = "black", center = TRUE) +
  scale_y_continuous(breaks = 1:7)

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(center = TRUE, alpha = .25) +
  scale_y_continuous(breaks = 1:7) +
  stat_summary(geom = "errorbar", width = .1) +
  stat_summary(geom = "point", fun = "mean") +
  theme_minimal()
