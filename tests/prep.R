
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(viridis)
library(scales)

# Data --------------------------------------------------------------------

set.seed(1)

df <- tibble(
  condition = sample(c("control", "treatment"), 50, replace = TRUE),
  outcome = sample(1:7, 50, replace = TRUE)
)

# data$outcome[1] <- NA

# stat_vhistogram() -------------------------------------------------------

ggplot(df, aes(x = condition, y = outcome)) +
  stat_vhistogram()

ggplot(df, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5)

ggplot(df, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5, color = "black")

ggplot(df, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5, color = "black", center = TRUE) +
  scale_y_continuous(breaks = 1:7)

ggplot(df, aes(x = condition, y = outcome)) +
  stat_vhistogram(center = TRUE, alpha = .25) +
  scale_y_continuous(breaks = 1:7) +
  stat_summary(geom = "errorbar", width = .1) +
  stat_summary(geom = "point", fun = "mean") +
  theme_minimal()

# position_likert() -------------------------------------------------------

ggplot(df, aes(x = condition, fill = factor(outcome))) +
  geom_bar(position = "likert") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip()

counts <- count(df, condition, outcome) %>%
  group_by(condition) %>%
  mutate(pct = n / sum(n))

ggplot(counts, aes(x = condition, y = pct, fill = factor(outcome))) +
  geom_col(position = "likert", width = .5) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = likert(nudge_y = 0.3)
  ) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip()
