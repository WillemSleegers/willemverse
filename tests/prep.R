
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(viridis)
library(scales)
library(see)

# Data --------------------------------------------------------------------

set.seed(1)

df <- tibble(
  condition = sample(c("control", "treatment"), 50, replace = TRUE),
  outcome = sample(1:7, 50, replace = TRUE)
)

df$outcome_na <- df$outcome
df$outcome_na[1] <- NA
x <- filter(df, condition == "control")$outcome

# geom_vhistogram() --------------------------------------------------------

ggplot(df, aes(x = condition, y = outcome)) +
  geom_vhistogram()

ggplot(df, aes(x = condition, y = outcome)) +
  geom_vhistogram(bins = 7, center = TRUE)

ggplot(df, aes(x = condition, y = outcome)) +
  geom_vhistogram(bins = 7, center = TRUE, scale = .75)

ggplot(df, aes(x = condition, y = outcome)) +
  geom_vhistogram(center = TRUE, alpha = .25) +
  scale_y_continuous(breaks = 1:7) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1) +
  stat_summary(geom = "point", fun = "mean") +
  stat_summary(geom = "line", fun = "mean", linetype = "dashed", group = 1) +
  theme_minimal()

ggplot(df, aes(x = condition, y = outcome_na)) +
  geom_vhistogram(bins = 7, na.rm = TRUE)

# position_likert() -------------------------------------------------------

df <- read_csv("./inst/test.csv")

ggplot(df, aes(x = item, fill = factor(response))) +
  geom_bar(position = "likert") +
  coord_flip()

count(df, condition, outcome)

ggplot(df, aes(x = condition, fill = factor(outcome))) +
  geom_bar(position = "likert") +
  coord_flip()

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
    position = "likert"
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip()
