
# model the system as linear equations

# replicates per treatment combination
n_rep <- 5

# simulate values for nitrogen
N <- rep(rep(seq(1, 10, length.out = 5), each = n_rep), 2)

# simulate values for the presence/absence of pathogens
P <- rep(c(0, 1), each = length(N)/2)

# simulate final biomass from this

# set the parameter values
alpha <- 1
beta_N <- 1.5
beta_P <- -0.1
beta_N_P <- -0.5

# calculate mu from the linear model
mu <- alpha + (beta_N*N) + (beta_P*P) + (beta_N_P*N*P)

# pass through a normal distribution
sigma_residual <- 0.25

# simulate the observed values
E1_Na <- rnorm(n = length(mu), mean = mu, sd = sigma_residual)

# plot the data

# pull into a data.frame
data <- dplyr::tibble(block_id = rep(seq_len(n_rep), 5*2),
                      N = N,
                      P = P,
                      E1_Na = E1_Na)
head(data)

View(data)

library(ggplot2)
ggplot(data = data,
       mapping = aes(x = N, y = E1_Na, colour = as.character(P))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(data = data,
       mapping = aes(x = P, y = E1_Na, colour = as.character(N))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# convert to wide-format
data_wide <-
  data |>
  tidyr::pivot_wider(id_cols = c("block_id", "N"),
                     names_from = "P",
                     values_from = "E1_Na")

# rename the variables
names(data_wide) <- c("block_id", "N", "E1_Na_sterile", "E1_Na_microbes")

# calculate plant-soil feedback
data_wide$psf <- with(data_wide, (E1_Na_microbes-E1_Na_sterile)/E1_Na_sterile)
head(data_wide)

# calculate the log plant-soil feedback
data_wide$psf_log <- with(data_wide, log(E1_Na_microbes/E1_Na_sterile))
head(data_wide)

# plot the data
ggplot(data = data_wide,
       mapping = aes(x = N, y = psf)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()


data_wide |>
  dplyr::group_by(N) |>
  dplyr::summarise(m_sterile = mean(E1_Na_sterile),
                   m_microbes = mean(E1_Na_microbes)) |>
  dplyr::mutate(psf = (m_microbes - m_sterile)/m_sterile,
                psf_log = log(m_microbes/m_sterile))


lm1 <- lm(log(E1_Na) ~ N + P + N:P, data = data)
summary(lm1)

x <- predict(lm1, dplyr::tibble(N = unique(data_wide$N),
                                P = 0))
y <- predict(lm1, dplyr::tibble(N = unique(data_wide$N),
                                P = 1))

y - x

data_wide |>
  dplyr::group_by(N) |>
  dplyr::summarise(m = mean(psf))


ggplot(data = data_wide,
       mapping = aes(x = N, y = psf_log)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()





