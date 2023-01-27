
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

set.seed(1)

n <- 50
n_items <- 5
n_options <- 5

df <- tibble(
  item = sample(str_c("item_", 1:n_items), n, replace = TRUE),
  response = sample(1:n_options, n, replace = TRUE)
)

ggplot(df, aes(x = item, fill = factor(response))) +
  geom_bar(position = "likert")

ggplot(df, aes(x = item, fill = factor(response))) +
  geom_bar(position = "likert") +
  coord_flip()

ggplot(df, aes(x = reorder(item, response), fill = factor(response))) +
  geom_bar(position = "likert") +
  coord_flip()

props <- df %>%
  count(item, response) %>%
  group_by(item) %>%
  mutate(pct = n / sum(n))

ggplot(props, aes(x = item, y = pct, fill = factor(response))) +
  geom_col(position = "likert") +
  coord_flip()
