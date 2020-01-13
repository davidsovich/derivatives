# ---- Description -------------------------------------------------------------------------------

# This program generates plots for Lecture 2. 

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Forward function
fwd_payoff = function(s, f) {
  s - f
}

# Plot dimensions
plot_width = 10
plot_height = 6

# ---- Practice problem 1 -------------------------------------------------------------------------

# Forward price
f1 = 2750

# Data
pp1_df = tibble(
  s = seq(
    from = 0,
    to = f1 * 2,
    by = 1
  ),
  long = fwd_payoff(s, f1),
  short = -1 * fwd_payoff(s, f1),
  net = long + short
) %>%
  gather(
   -s,
    key = "party",
    value = "payoff"
  )

# Plot long payoff
cairo_pdf(
  filename = "./figures/lecture_2/pp1a.pdf",
  width = 10,
  height = 6
)
ggplot(
  data = pp1_df %>%
    filter(
      party == "long"
    )
) + 
  geom_line(
    aes(
      x = s,
      y = payoff
    ),
    color = "blue"
  ) + 
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "black"
  ) + 
  geom_vline(
    xintercept = f1,
    linetype = "dashed",
    color = "darkgray"
  ) + 
  theme_bw()  +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 5000,
      by = 1000
    )
  ) + 
  labs(
    title = "Payoff to long forward",
    y = expression(Payoff),
    x = expression(S[T])
  ) + 
  annotate(
    "text",
    x = f1 + 50,
    y = f1,
    label = " = F"
  )
dev.off()

# Payoff to short forward
cairo_pdf(
  filename = "./figures/lecture_2/pp1b.pdf",
  width = 10,
  height = 6
)
ggplot(
  data = pp1_df %>%
    filter(
      party == "short"
    )
) + 
  geom_line(
    aes(
      x = s,
      y = payoff
    ),
    color = "red"
  ) + 
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "black"
  ) + 
  geom_vline(
    xintercept = f1,
    linetype = "dashed",
    color = "darkgray"
  ) + 
  theme_bw() +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 5000,
      by = 1000
    )
  ) + 
  labs(
    title = "Payoff to short forward",
    y = expression(Payoff),
    x = expression(S[T])
  ) + 
  annotate(
    "text",
    x = f1 + 50,
    y = f1,
    label = " = F"
  )
dev.off()

# Plot combined payoffs
cairo_pdf(
  filename = "./figures/lecture_2/pp1c.pdf",
  width = 10,
  height = 6
)
ggplot(
  data = pp1_df
) + 
  geom_line(
    aes(
      x = s,
      y = payoff,
      colour = party
    )
  ) + 
  geom_vline(
    xintercept = f1,
    linetype = "dashed",
    color = "darkgray"
  ) + 
  theme_bw() +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 5000,
      by = 1000
    )
  ) + 
  labs(
    title = "Payoff to forward",
    y = expression(Payoff),
    x = expression(S[T])
  ) + 
  annotate(
    "text",
    x = f1 + 50,
    y = f1,
    label = " = F"
  ) + 
  scale_colour_manual(
    values = c("blue", "darkgreen", "red")
  ) + 
  theme(legend.position="top")
dev.off()

# ---- Hedge for Hershey --------------------------------------------------------------------------

# ---- Hedge for cocoa farmer ---------------------------------------------------------------------




