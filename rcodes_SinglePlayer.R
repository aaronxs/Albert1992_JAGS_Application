# Load package R ------------------------------
pacman::p_load(tidyverse, rjags, magrittr, highcharter)

# load function script ------------------------------
source('JAGS_model.R', echo=TRUE)

datt <- read.csv("Hitter Data.csv", stringsAsFactors = F)

hitter_list <- datt %>%
  group_by(Name) %>%
  summarise(ID = 1) %>%
  mutate(ID = 1:nrow(.))

datt <- datt %>%
  mutate(ID = hitter_list$ID[match(Name, hitter_list$Name)],
         # home run rate
         y = HR/AB) %>%
  # exclude season with 0 home run
  filter(ID == 1)

# at-bat
t <- datt$AB

# home run rate
y <- datt$y

# number of seasons
ns <- nrow(datt)
# dummy matrix for sampling from custom distribution function
zero <- rep(0, ns)

# parameters to monitor in NMA
params = c("lambda", "g", "phi", "beta1")

n_iter <- 10
n_burn <- 20

# data for mc
datt_mc <- list(
  t = t,
  y = y,
  zero = zero,
  nu = 10,
  ppi = pi,
  ns = ns
)

mc_seed <- 1234

# initial value + seed
set.seed(mc_seed)
inits <- list()
for(i in 1:4){
  inits[[i]] <- list(
    # lambda = sum(datt$HR)/sum(datt$AB),
    # lambda = runif(ns, 0, 1),
    g = rep(1, ns),
    beta = rnorm(3, mean = 0, sd = 1),
    beta_player = rnorm(1, mean = 0, sd = 1),
    .RNG.name="base::Super-Duper", .RNG.seed = mc_seed)
}

# * load JAGS code ------------------------------
tmpf <- tempfile()
tmps <- file(tmpf, "w")
# R2WinBUGS::write.model(jags_mod3, tmps)
R2WinBUGS::write.model(jags_mod4, tmps)
close(tmps)
rjags::load.module("glm")

res <- rjags::jags.model(tmpf, data = datt_mc, n.chains = 4, quiet = FALSE, inits = inits)

update(res, n.iter = n_burn) # burn-in

samples <- suppressWarnings(rjags::coda.samples(res,
                                                variable.names = params,
                                                n.iter = n_iter, thin = 1)) # number of samples of the MCMC

samp_jags_cp <- aperm(abind::abind(samples, along = 3), c(1, 3, 2))

# Summarize results ------------------------------------------------------------
mon <- rstan::monitor(samp_jags_cp, probs = c(0.025, 0.5, 0.975),
                      warmup = 0,
                      # print output
                      print = FALSE) %>%
  as.data.frame %>%
  tibble::rownames_to_column("param") %>%
  select(-Q50) %>%
  rename(Mean = mean,
         Q97.5 = `97.5%`,
         Q50 = `50%`,
         Q2.5 = `2.5%`)

# fp_input <- mon  %>%
#   filter(startsWith(param, "g[") | startsWith(param, "lambda[")) %>%
#   select("param", "Mean", "sd", "Q2.5", "Q50", "Q97.5") %>%
#   mutate(param1 = param) %>%
#   separate(col = param1, into = c("name", "year"), sep = "\\[") %>%
#   mutate(year = as.numeric(str_remove_all(year, "\\]"))) %>%
#   mutate(x_dummy = 1:nrow(.) - 1) %>%
#   mutate(lower = round(Mean - sd, 4),
#          estimate = round(Mean, 4),
#          upper = round(Mean + sd, 4),
#          val = paste0(format(round(Mean, 4), scientific = FALSE),
#                       " (", format(round(Mean - sd, 4), scientific = FALSE), " to ",
#                       format(round(Mean + sd, 4), scientific = FALSE), ")"))

# # Forest plot with ggplots ------------------------------------------------------------
# input_g <- mon %>%
#   filter(startsWith(param, "g[")) %>%
#   mutate(x_dummy = 1:nrow(.) - 1) %>%
#   mutate(lower = round(Mean - sd, 4),
#          estimate = round(Mean, 4),
#          upper = round(Mean + sd, 4),
#          val = paste0(format(round(Mean, 4), scientific = FALSE),
#                       " (", format(round(Mean - sd, 4), scientific = FALSE), " to ",
#                       format(round(Mean + sd, 4), scientific = FALSE), ")"))
# 
# # Forest plot ------------------------------------------------------------
# input_lambda <- mon %>%
#   filter(startsWith(param, "lambda[")) %>%
#   mutate(x_dummy = 1:nrow(.) - 1) %>%
#   mutate(lower = round(Mean - sd, 4),
#          estimate = round(Mean, 4),
#          upper = round(Mean + sd, 4),
#          val = paste0(format(round(Mean, 4), scientific = FALSE),
#                       " (", format(round(Mean - sd, 4), scientific = FALSE), " to ",
#                       format(round(Mean + sd, 4), scientific = FALSE), ")"))

# input_lambda <- mon  %>%
#   filter(startsWith(param, "g[") | startsWith(param, "lambda[")) %>%
#   select("param", "Mean", "sd", "Q2.5", "Q50", "Q97.5") %>%
#   mutate(param1 = param) %>%
#   separate(col = param1, into = c("name", "year"), sep = "\\[") %>%
#   mutate(year = as.numeric(str_remove_all(year, "\\]")),
#          name = factor(name, levels = c("g", "lambda"))) %>%
#   mutate(x_dummy = 1:nrow(.) - 1) %>%
#   mutate(lower = round(Mean - sd, 4),
#          estimate = round(Mean, 4),
#          upper = round(Mean + sd, 4),
#          val = paste0(format(round(Mean, 4), scientific = FALSE),
#                       " (", format(round(Mean - sd, 4), scientific = FALSE), " to ",
#                       format(round(Mean + sd, 4), scientific = FALSE), ")"))
# 
# fp <- ggplot(data = input_lambda,
#                     aes(x = year, y = estimate, ymin = lower, ymax = upper, colour = factor(year)), 
#                show.legend = FALSE) +
#   geom_errorbar(size = 0.5, width = 0.1) +
#   geom_point(data = input_lambda, aes(x = year, y = estimate), 
#              show.legend = FALSE, size = 2.5) +
#   # observed home run rate
#   # geom_point(data = datt, aes(x = 1:ns, y = HR/AB), colour = "black", shape = 17,  
#   #            show.legend = FALSE, size = 3, inherit.aes = FALSE) +
#   # geom_text(data = datt, aes(x = 1:ns, y = HR/AB, label = round(HR/AB, 4)), 
#   #           size = 3, colour = "black", fontface = "bold", inherit.aes = FALSE,
#   #           # prevent label from overlapping
#   #           position = position_jitter()) +
#   # # horizontal line at career mean home run rate
#   # geom_hline(aes(yintercept = mean(datt$HR/datt$AB))) + 
#   # geom_errorbar(data = input_g, 
#   #               aes(x = x_dummy + 1, y = estimate, ymin = lower, ymax = upper, colour = factor(x_dummy)), 
#   #               size = 0.5, width = 0.1, show.legend = FALSE, inherit.aes = FALSE) +
#   # geom_point(data = input_g, aes(x = x_dummy + 1, y = estimate, colour = factor(x_dummy)), 
#   #            show.legend = FALSE, size = 2.5, inherit.aes = FALSE) +
#   # geom_hline(aes(yintercept = 1)) + 
#   # # add treatment left as axis label
#   # scale_x_continuous(name = "Year",
#   #                    breaks = input_lambda$x_dummy + 1,
#   #                    labels = input_lambda$x_dummy + 1,
#   #                    # add treament effect median and 95% credible interval as axis label on the right
#   #                    sec.axis = sec_axis(trans = ~.,
#   #                                        name = "Median Rate (95% CrI)",
#   #                                        breaks = input_lambda$x_dummy + 1,
#   #                                        labels = input_lambda$val)) +
#   # add study name
#   facet_grid(name ~ ., scales = "free_y", switch = "y") +
#   # scale_y_continuous(trans = 'log10') +
#   # scale_y_continuous(breaks = seq(0, 1, 0.02), limits = c(0, 0.1), expand = c(0, 0)) +
#   # fllip coordinates
#   # coord_flip() +
#   # # title of plot
#   ggtitle(NULL) +
#   # # label for x axis and legend
#   xlab("Year") + ylab("Home Run Rate") +
#   # use a white background
#   theme_bw() + 
#   # axis label and plot formats
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour = "black", size = rel(0.9)),
#         axis.text.y = element_text(hjust = 0.5, vjust = 0.5, colour = "black", size = rel(0.9)),
#         axis.title.x = element_text(face="bold", colour = "black", size = rel(0.9)),
#         axis.title.y = element_text(face="bold", colour = "black", size = rel(0.9))) +
#   # title label formats
#   theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
#         plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
#   # treament name format
#   theme(panel.spacing.y = unit(1, "line"),
#         strip.background = element_rect(colour = "white", fill = "grey"),
#         strip.placement = "outside",
#         strip.text.y.left = element_text(vjust = 0.5, face = "bold", colour = "black", size = rel(0.9), angle = 0)
#   ) +
#   guides(colour = FALSE)
# fp