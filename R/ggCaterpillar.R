#' @import ggplot2


# from http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
# re = object of class ranef.mer
ggCaterpillar <- function(re, QQ=FALSE, likeDotplot=TRUE) {
  f <- function(x, nm = "ranef plot") {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))

    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
      scale <- 12-log(length(levels(pDf$ID)),2)
      p <- p + theme(axis.text.y = element_text(size=scale))
    }

    p <- p + theme(legend.position="none")
    p <- p + labs(title= nm)
    p <- p + geom_hline(yintercept=0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes())
    return(p)
  }

  #   lapply(re, f) # original
  lapply(seq_along(re), function(y, n, i) { f(y[[i]], n[[i]]) }, y=re, n=names(re)) # adds plot names
}
