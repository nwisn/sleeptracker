
# a function to import and process the data
import_data <- function(file){
  require(readxl, quietly = T)
  xl <- read_excel(file)
  timezero <- as.POSIXlt.character("1899-12-31 00:00:00", tz = "UTC")

  columnclasses <- unlist(lapply(sapply(xl, class), function(x) x[1]))
  isPOSIX <- which(columnclasses %in% "POSIXct")

  for(thiscolname in names(columnclasses)[isPOSIX[-1]]){
    xl[[thiscolname]] <- as.numeric(difftime(xl[[thiscolname]], timezero, units = "min"))
  }
  return(xl)
}


# tls regression
TLSestimates <- function(yvec, xvec){
  r <- prcomp( ~ xvec + yvec )
  slope <- r$rotation[2,1] / r$rotation[1,1]
  intercept <- r$center[2] - slope*r$center[1]
  ests <- c(m = slope, b = as.numeric(intercept))
  return(ests)
}

# bootstrap tls regression
bootTLSestimates <- function(yvec, xvec, nboot){
  bootindices <- matrix(sample(1:length(xvec),
                               length(xvec)*nboot,
                               replace = T),
                        ncol = length(xvec))
  applied <- apply(bootindices, 1, function(ii){
    TLSestimates(xvec[ii], yvec[ii])
  })
  bootdf <- as.data.frame(t(applied))
  return(bootdf)
}

# compute bland altman stats and plot
blandaltman <- function(yvec, xvec, main = "Bland-Altman"){
  require(ggplot2, quietly = T)
  this_df <- data.frame(x = xvec, y = yvec)
  this_df$mean <- (this_df$y + this_df$x)/2
  this_df$difference <- this_df$y - this_df$x

  # limits of agreement
  s <- sd(this_df$difference)
  mu <- mean(this_df$difference)
  LOA <- c(mu-1.96*s, mu+1.96*s)

  gg.bland <- ggplot(this_df) +
    aes(x=mean, y=difference) +
    geom_point() +
    #geom_smooth(method = lm) +
    geom_hline(yintercept = 0, lty=1) +
    geom_hline(yintercept = c(mu, LOA), lty = c(1,2,2), color = "red") +
    ggtitle(main) +
    theme_bw()


  ret <- list(s = s,
              mu = mu,
              LOA = LOA,
              fig = gg.bland)
}


boxplot.paired <- function(yvec, xvec, ytitle, xtitle, main = "",
                           normalmean = NULL,
                           normalsd = NULL){
  require(ggplot2, quietly = T)
  require(reshape2, quietly = T)
  require(ggsci, quietly = T)
  require(ggpubr, quietly = T)

  this_df <- data.frame(x = xvec, y = yvec)
  this_df$i = 1:nrow(this_df)
  value_df <- melt(this_df[,c("y","x","i")], id.vars = "i")

  wilcoxtest <- wilcox.test(value~variable, data = value_df, paired = T)
  ttest <- t.test(value~variable, data = value_df, paired = T)

  fig <- ggplot(value_df) +
    aes(x=variable, y=value, fill=variable) +
    geom_boxplot(outlier.alpha = 0) +
    geom_point() +
    geom_line(aes(group = i), color = "grey") +
    scale_x_discrete(name = "device", labels = c(ytitle, xtitle)) +
    scale_y_continuous(name = "minutes") +
    scale_fill_jama() +
    stat_compare_means(method = "t.test", paired = TRUE, label.x = 1) +
    ggtitle(main) +
    guides(fill=F) +
    theme_bw() +
    geom_hline(yintercept = c(normalmean,
                              normalmean+1*normalsd,
                              normalmean-1*normalsd,
                              normalmean+1.96*normalsd,
                              normalmean-1.96*normalsd),
               lty = c(1,3,3,2,2),
               color = "red"
    ) +
    geom_ribbon(data = data.frame(x = c(0,1,2,3), ymin = normalmean-1.96*normalsd, ymax = normalmean+1.96*normalsd),
                                  aes(x = x, ymin =ymin, ymax = ymax),
                alpha = 0.1,
                inherit.aes = F) +
    geom_ribbon(data = data.frame(x = c(0,1,2,3), ymin = normalmean-1*normalsd, ymax = normalmean+1*normalsd),
                aes(x = x, ymin =ymin, ymax = ymax),
                alpha = 0.2,
                inherit.aes = F)

  ret <- list(wilcoxon = wilcoxtest,
           ttest = ttest,
           fig = fig)
  return(ret)
}


boxplot.triple <- function(zvec, yvec, xvec, ztitle, ytitle, xtitle, main = "",
                           normalmean = NULL,
                           normalsd = NULL){
  require(ggplot2, quietly = T)
  require(reshape2, quietly = T)
  require(ggsci, quietly = T)
  require(ggpubr, quietly = T)
  require(lmerTest)

  this_df <- data.frame(x = xvec, y = yvec, z = zvec)
  colnames(this_df) <- c(xtitle,ytitle,ztitle)
  this_df$i = 1:nrow(this_df)
  value_df <- melt(this_df[,c(ztitle,ytitle,xtitle,"i")], id.vars = "i")

  lmertest <- lmer(value ~ variable + (1|i), data = value_df)
  anova(lmertest)

  pairwise_comparisons <- list( c(xtitle, ytitle), c(xtitle, ztitle), c(ytitle, ztitle))

  fig <- ggplot(value_df) +
    aes(x=variable, y=value, fill=variable) +
    geom_boxplot(outlier.alpha = 0) +
    geom_point() +
    geom_line(aes(group = i), color = "grey", alpha = 0.7) +
    scale_x_discrete(name = "device", labels = c(ztitle, ytitle, xtitle)) +
    scale_y_continuous(name = "minutes") +
    scale_fill_jama() +
    # stat_compare_means(comparisons = pairwise_comparisons,
    #                    label = "p.signif", hide.ns = TRUE,
    #                    method = "t.test", paired = TRUE, label.x = 1) +
    ggtitle(main) +
    guides(fill=F) +
    theme_bw() +
    geom_hline(yintercept = c(normalmean,
                              normalmean+1*normalsd,
                              normalmean-1*normalsd,
                              normalmean+1.96*normalsd,
                              normalmean-1.96*normalsd),
               lty = c(1,3,3,2,2),
               color = "red"
    ) +
    geom_ribbon(data = data.frame(x = c(0.5,1,2,3, 3.5), ymin = normalmean-1.96*normalsd, ymax = normalmean+1.96*normalsd),
                aes(x = x, ymin =ymin, ymax = ymax),
                alpha = 0.1,
                inherit.aes = F) +
    geom_ribbon(data = data.frame(x = c(0.5,1,2,3,3.5), ymin = normalmean-1*normalsd, ymax = normalmean+1*normalsd),
                aes(x = x, ymin =ymin, ymax = ymax),
                alpha = 0.2,
                inherit.aes = F)

  ret <- list(lmer = lmertest,
              fig = fig)
  return(ret)
}




tlsregression <- function(yvec, xvec, ytitle, xtitle, main = "", nboot = 100, nlattice = 100){
  estimate <- TLSestimates(yvec, xvec)
  estimate.boot <- bootTLSestimates(yvec, xvec, nboot)
  estimate.ci <- sapply(estimate.boot, function(x) quantile(x, c(0.025, 0.975), type = 6))

  # confidence bands on tls regression
  linepredict <- function(x, m, b){ m * x + b }
  xlattice <- seq(min(xvec), max(xvec), length.out = nlattice)
  envsim <- apply(estimate.boot, 1, function(x) linepredict(xlattice ,x[1],x[2]))
  env.ci <- as.data.frame(t(apply(envsim, 1, function(x) quantile(x, c(0.025, 0.975), type = 6))))
  env.ci$x <- xlattice
  colnames(env.ci) <- c("lower", "upper", "x")

  plotdf <- data.frame(yvec, xvec)
  colnames(plotdf) <- c(ytitle, xtitle)

  gg.reg <- ggplot(plotdf) + aes_string(x=xtitle, y=ytitle) +
    geom_ribbon(data = env.ci, aes(x = x, ymin = lower, ymax = upper), inherit.aes = F, alpha = 0.2) +
    geom_point() +
    #stat_smooth(method = "lm") +
    geom_abline(slope=1, intercept=1, lty = 2) +
    geom_abline(slope=estimate[1], intercept=estimate[2], lty = 1, color = "red") +
    theme_bw() +
    coord_cartesian(ylim =c(min(c(xvec, yvec)), max(c(xvec, yvec))),
                    xlim =c(min(c(xvec, yvec)), max(c(xvec, yvec)))) +
    ggtitle(main)

  ret <- list(estimate = estimate,
              ci = estimate.ci,
              fig = gg.reg)
  return(ret)
}
