# Load libraries
library(tidyverse)
library(lubridate)

# Load data
cmdty_df = read_csv(
  file = "./data/cmdty_open_int.txt"
)

# Dash locations
dash_loc = regexpr(
  "-",
  cmdty_df$`Market and Exchange Names`
)

# Wrangle data 
cmdty_df = cmdty_df %>%
  rename(
    product = `Market and Exchange Names`,
    date = `As of Date in Form YYYY-MM-DD`,
    open_int = `Open Interest (All)`
  ) %>%
  mutate(
    archive = year(date) * 100 + month(date),
    product = tolower(
      gsub(
        " ",
        "", 
        substr(
          product,
          1,
          (dash_loc-1)
        )
      )
    ),
    product = case_when(
      product %in% c("crudeoil,light'sweet'", "crudeoil,lightsweet") ~ "Crude Oil",
      product %in% c("soybeanoil", "soybeanmeal")                    ~ "Soybeans",
      product %in% c("gold")                                         ~ "Gold",
      TRUE                                                           ~ product
    )
  ) %>% 
  filter(
    product %in% c("Crude Oil", "Soybeans", "Gold"),
    archive >= 200001,
    archive <= 201012
  ) %>%
  select(
    product,
    archive,
    open_int
  )

# Open interest per month
cmdty_df = cmdty_df %>%
  group_by(
    product,
    archive
  ) %>%
  summarise(
    open_int = sum(open_int, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(
    product,
    archive
  ) %>%
  group_by(
    product
  ) %>% 
  mutate(
    t_index = row_number(),
    first_open_int = first(open_int)
  ) %>%
  ungroup() %>%
  mutate(
    scaled_open_int = open_int / first_open_int,
    Futures = product
  ) 

# Break points
break_points = cmdty_df %>%
  arrange(
    product,
    archive
  ) %>%
  mutate(
    year = floor(archive / 100)
  ) %>% 
  distinct(
    year,
    .keep_all = TRUE
  ) %>%
  select(
    t_index,
    year
  ) 

# Plot the time series
gg_cmdty = ggplot(
  data = cmdty_df
) + 
  geom_line(
    aes(
      x = t_index,
      y = scaled_open_int,
      col = Futures
    )
  ) + 
  scale_x_continuous(
    breaks = break_points$t_index,
    labels = break_points$year
  ) + 
  theme_bw() + 
  labs(
    y = "Scaled open interest",
    x = "Year",
    title = "Commodity futures: open interest by year"
  ) + 
  scale_size_manual(
    values = rep(1.5, 3)
  ) +
  theme(
    legend.position = "top"
  )
      
# Export the plot
cairo_pdf(
  filename = "./figures/lecture_1/cmdty.pdf",
  width = 9,
  height = 6
)
gg_cmdty
dev.off()


