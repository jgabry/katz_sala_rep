
library(foreign)
library(survival)


com <- read.dta('katz_sala.dta')
com$censordum <- 1 - com$censordum

keep_vars <- c("censordum", "cumtenure", "seniority", 
               "pctreturned", "ch_pty_control", "dem",
               "privileged", "realign", "revolt", 
               "ausballot", "congno")

com_subset <- com[, keep_vars]
com_subset <- subset(com_subset, cumtenure <= 9 & congno < 71)


com_cox_all_congnos <- coxph(Surv(cumtenure, censordum) ~ seniority + pctreturned + ch_pty_control + dem + privileged + realign + revolt + ausballot, data=com_subset, ties = "breslow")

# library(pec)
# ndat1 <- with(com_subset, data.frame(seniority = mean(seniority), pctreturned = mean(pctreturned), ch_pty_control = 0, dem = 0, privileged = 0, realign = 0, revolt = 0, ausballot = 0))
# ndat2 <- with(com_subset, data.frame(seniority = mean(seniority), pctreturned = mean(pctreturned), ch_pty_control = 0, dem = 0, privileged = 0, realign = 0, revolt = 0, ausballot = 1))
# predictSurvProb(com_cox_all_congnos, newdata = ndat1, times = 1:9)
# predictSurvProb(com_cox_all_congnos, newdata = ndat2, times = 1:9)
# SF1 <- survfit(com_cox_all_congnos, newdata = ndat1)$surv
# SF2 <- survfit(com_cox_all_congnos, newdata = ndat2)$surv
# center <- function(x) {
#   scale(x, center = TRUE, scale = FALSE)
# }


# Run models by congress and predict probabilities
congnos <- sort(unique(com_subset$congno))

pr_t <- function(S, t) {
  # S values of survival function from survfit()
  
  if (t == 1) { # then S[t-1] = S(0) = 1
    p <- 1 - S[t]
  } else {
    p <- (S[t-1] - S[t]) / S[t-1]  
  }
  p
}

results <- list()
pp <- list()
haz <- matrix(NA, nr = length(congnos), nc = 9)
rownames(haz) <- congnos
colnames(haz) <- paste0("t", 1:9)
min_cong <- min(congnos)
for (C in congnos) {
  name <- paste0("cong_",C)
  print(name)
  fit <- coxph(Surv(cumtenure, censordum) ~ seniority + dem + privileged,
               data = subset(com_subset, congno == C), 
               ties = "breslow", # maybe use ties = exact?
               control = coxph.control(iter.max = 100)) 
  results[[name]] <- fit

  SF <- survfit(fit)$surv # default X values are means
  pred_p <- rep(0, length(SF))
  
  for (t in 1:length(pred_p)) {
    pred_p[t] <- pr_t(SF, t)
  }
  
  pp[[name]] <- pred_p
  
  base_haz <- -log(SF)
  new_data <- data.frame(t(fit$means)) 
  risk <- predict(fit, newdata = new_data, type = "risk")
  h <- base_haz*risk
  row <- C - min_cong + 1
  col <- length(h)
  haz[row, 1:col] <- h
}

haz_1_to_6 <- haz[, 1:6]
haz_1_to_6_ts <- ts(haz_1_to_6, start = min(congnos))

plot(haz_1_to_6_ts, plot.type = "single")

markers <- c("ausballot", "revolt", "realign")
marker_times <- with(com_subset, sapply(markers, FUN = function(x) {
    min(congno[get(x) == 1])
  }))

abline(v = marker_times, col = c("green", "purple", "blue"))


library(ggplot2)
library(reshape2)
pdat <- melt(haz_1_to_6, varnames = c("Congress","Duration"))
gg_haz <- ggplot(pdat, aes(x = Congress, y = value, color = Time))
gg_haz <- gg_haz + 
  theme_classic() %+replace% theme(axis.line = element_line(size = 3)) +
  ylab("Hazard (X values fixed at means)") +
  geom_vline(xintercept = marker_times, linetype = 2) + 
  geom_line()


