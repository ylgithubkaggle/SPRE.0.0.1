##' stepwise regression
##'
##' generate a stepwise regression table from the data
##' @title stepwise regression
##' @param x a vector of times/sessions
##' @param y a vector of responses
##' @param skip_indices indices where analyses are not performed
##' @return an object of class 'SPRE_step_reg'
##' @author Deborah Weissman-Miller
##' @export
##' @importFrom stats pf
##' @importFrom stats lm
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
stepwise_regression <- function(x, y, skip_indices) {
    n <- length(x)
    
    lmfit <- lm(y ~ x)
    lmsum <- summary(lmfit)
    betahat1 <- lmsum$coefficients[2, "Estimate"]
    
    fDist <- matrix(NA, n, 8)
    colnames(fDist) <- c("No.", "x", "Rsquare", "Fstat", "FstatNum.", "FstatDen.", "Pvalue", "betahat")
    for (i in 1:n) {
        if (i %in% skip_indices) {
            fDist[i, ] <- c(i, x[i], rep(NA, 6))
        } else {
            lmsubfit <- lm(y[1:i] ~ x[1:i])
            lmsubsum <- summary(lmsubfit)
            betahat1 <- lmsubsum$coefficients[2, "Estimate"]
            Rsq <- as.numeric(lmsubsum$r.squared)
            fstat <- as.numeric(lmsubsum$fstatistic)  #lmsum$r.squared,
            pvalue <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
            fDist[i, ] <- c(i, x[i], Rsq, fstat, pvalue, betahat1)
        }
    }
    print(fDist, na.print = "--", digits = 3)
    maxfstatidx <- which.max(fDist[, "Fstat"])
    cat("Potential change point: tau =", fDist[maxfstatidx, "x"], "\n")
    cat("Largest Fstat: Fstat =", fDist[maxfstatidx, "Fstat"], "\n")
    cat("Index of potential change point: No. =", fDist[maxfstatidx, "No."], "\n")
    
    output_object <- list(fDist = fDist, lmfit = lmfit, x = x, y = y)
    class(output_object) <- "SPRE_step_reg"
    return(output_object)
}

##' print SPRE_step_reg
##'
##' print an SPRE_step_reg object
##' @title print SPRE
##' @param SPRE_step_reg an object of class 'SPRE_step_reg'
##' @return output some information to console
##' @author Deborah Weissman-Miller
##' @export
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
print.SPRE_step_reg <- function(SPRE_step_reg) {
    print(SPRE_step_reg$fDist, na.print = "--", digits = 3)
    maxfstatidx <- which.max(SPRE_step_reg$fDist[, "Fstat"])
    cat("Potential change point: tau =", SPRE_step_reg$fDist[maxfstatidx, "x"], "\n")
    cat("Largest Fstat: Fstat =", SPRE_step_reg$fDist[maxfstatidx, "Fstat"], "\n")
    cat("Index of potential change point: No. =", SPRE_step_reg$fDist[maxfstatidx, "No."], "\n")
}

##' SPRE
##'
##' Perform SPRE analysis given a stepwise regression table and indices of change points
##' @title SPRE
##' @param step_reg a table generated from stepwise regression analysis ,an object of class 'SPRE_step_reg'
##' @param chptidx index of change point
##' @param lastidx index of last point
##' @return an object of class 'SPRE'
##' @author Deborah Weissman-Miller
##' @export
##' @importFrom stats lm
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
##' SPRE_mod<-SPRE(step_reg,chptidx=11,lastidx=14)
SPRE <- function(step_reg, chptidx, lastidx = chptidx) {
    x <- step_reg$x
    y <- step_reg$y
    fDist <- step_reg$fDist
    chptFstat <- fDist[chptidx, "Fstat"]
    chptx <- fDist[chptidx, "x"]
    chptbetahat <- fDist[lastidx, "betahat"]
    kshape <- abs(log(abs(1 - chptbetahat * chptx)))
    lmfitformal <- lm(y[1:lastidx] ~ x[1:lastidx])
    
    cat("Change point: tau =", chptx, "\n")
    cat("Fstat at change point: Fstat =", chptFstat, "\n")
    cat("Index of change point: No. =", chptidx, "\n")
    cat("Betahat:", chptbetahat, "\n")
    cat("Shape parameter:", kshape, "\n")
    
    output_object <- list(step_reg = step_reg, chptidx = chptidx, lastidx = lastidx, chptFstat = chptFstat, chptbetahat = chptbetahat, 
        chptx = chptx, betahat = chptbetahat, kshape = kshape, lmfitformal = lmfitformal)
    class(output_object) <- "SPRE"
    return(output_object)
}

##' print SPRE
##'
##' print an object of class 'SPRE'
##' @title print SPRE
##' @param SPRE an object of class 'SPRE'
##' @return output some information to console
##' @author Deborah Weissman-Miller
##' @export
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
print.SPRE <- function(SPRE) {
    cat("Change point: tau =", SPRE$chptx, "\n")
    cat("Fstat at change point: Fstat =", SPRE$chptFstat, "\n")
    cat("Index of change point: No. =", SPRE$chptidx, "\n")
    cat("Betahat:", SPRE$chptbetahat, "\n")
    cat("Shape parameter:", SPRE$kshape, "\n")
}

##' plot residuals
##'
##' residuals plot from the linear model given in the first SPRE step
##' @title plot residuals
##' @param SPRE_object an object of class 'SPRE'
##' @return output some plots to device
##' @author Deborah Weissman-Miller
##' @export
##' @importFrom graphics par
##' @importFrom graphics abline
##' @importFrom graphics plot
##' @importFrom graphics title
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
##' SPRE_mod<-SPRE(step_reg,chptidx=11,lastidx=14)
##' plot_residuals(SPRE_mod)
plot_residuals <- function(SPRE_object) {
    opar <- par("mfrow", "mar")
    par(mfrow = c(2, 1), mar = c(4.7, 5.8, 2.5, 3.8))
    
    lmfitformal <- SPRE_object$lmfitformal
    plot(lmfitformal, 1, ann = FALSE, pch = 18, title("SPRE Residuals", col.main = "blue", xlab = "Fitted Values", ylab = "Residuals"))
    abline(0, 0)
    plot(lmfitformal, 2, ann = FALSE, pch = 18, title("Normal Q-Q", col.main = "blue", xlab = "Quantiles", ylab = "Standardized Residuals"))
    abline(0, 0)
    
    par(opar)
}

##' predict SPRE
##'
##' make predictions from SPRE model
##' @title predict SPRE
##' @param xpred times/sessions where predictions are made
##' @param kshape shape parameter (can be found from the output of SPRE function)
##' @param tau time/session of change point
##' @param theta1 prior value for response
##' @param predtype type of prediction, can be 'forward' (c.f. Weissman-Miller, D., 2013) or 'rolling' (c.f. Weissman-Miller, D., Miller, R. J., & Shotwell, M. P., 2017)
##' @return output some plots to device
##' @author Deborah Weissman-Miller
##' @export
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
##' SPRE_mod<-SPRE(step_reg,chptidx=11,lastidx=14)
##' kshape<-SPRE_mod$kshape
##' idxmaxFstat<-which.max(SPRE_mod$step_reg$fDist[,'Fstat'])
##' tau<-SPRE_mod$step_reg$fDist[idxmaxFstat,'x']
##' theta1<-HEAT_stat$FData[SPRE_mod$chptidx]
##' xpred<-7:46
##' SPRE_pred<-predict_SPRE(xpred=xpred,kshape=kshape,tau=tau,
##'                         theta1=theta1,predtype='rolling')
predict_SPRE <- function(xpred, kshape, tau, theta1, predtype = "forward") {
    tablepred <- matrix(NA, length(xpred) + 1, 5)
    colnames(tablepred) <- c("No.", "xpred", "tau", "R Ratio", "Theta_t")
    if (predtype == "forward") {
        for (i in 0:length(xpred)) {
            if (i == 0) {
                tt <- tau
                tablepred[i + 1, 1:2] <- c(i, tt)
            } else if (i == 1) {
                tt <- xpred[i]
                R_Ratio <- 1
                thetat <- R_Ratio * theta1
                tablepred[i + 1, ] <- c(i, tt, tau, R_Ratio, thetat)
            } else {
                tt_prior <- xpred[i - 1]
                tt_incre <- xpred[i]
                R_Ratio <- (1 - exp(-(tt_incre/tau)^kshape))/(1 - exp(-(tt_prior/tau)^kshape))
                thetat <- R_Ratio * tablepred[i, "Theta_t"]
                tablepred[i + 1, ] <- c(i, tt_incre, tau, R_Ratio, thetat)
            }
        }
    }
    if (predtype == "rolling") {
        for (i in 0:length(xpred)) {
            if (i == 0) {
                tt <- tau
                tablepred[i + 1, 1:2] <- c(i, tt)
            } else if (i == 1) {
                tt <- xpred[i]
                R_Ratio <- 1
                thetat <- R_Ratio * theta1
                tablepred[i + 1, ] <- c(i, tt, tau, R_Ratio, thetat)
            } else {
                tt_prior <- xpred[i - 1]
                tt_incre <- xpred[i]
                R_Ratio <- (1 - exp(-(tt_incre/tt_prior)^kshape))/(1 - exp(-(tt_prior/tt_prior)^kshape))
                thetat <- R_Ratio * tablepred[i, "Theta_t"]
                tablepred[i + 1, ] <- c(i, tt_incre, tau, R_Ratio, thetat)
            }
        }
    }
    print(tablepred)
    out_object <- list(xpred = xpred, tablepred = tablepred, kshape = kshape, tau = tau, theta1 = theta1, predtype = predtype)
    class(out_object) <- "SPRE_pred"
    return(out_object)
}

##' plot probability
##'
##' plot weibull distrubition of SPRE predictions
##' @title plot probability
##' @param SPRE_object SPRE analysis output, an object of class 'SPRE'
##' @param SPRE_pred SPRE prediction output, an object of class 'SPRE_pred'
##' @param xmin lower bound of plot range
##' @param xmax upper bound of plot range
##' @return output information to console and plots to device
##' @author Deborah Weissman-Miller
##' @export
##' @importFrom graphics plot
##' @importFrom graphics lines
##' @importFrom graphics abline
##' @importFrom graphics curve
##' @importFrom stats pweibull
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
##' SPRE_mod<-SPRE(step_reg,chptidx=11,lastidx=14)
##' kshape<-SPRE_mod$kshape
##' idxmaxFstat<-which.max(SPRE_mod$step_reg$fDist[,'Fstat'])
##' tau<-SPRE_mod$step_reg$fDist[idxmaxFstat,'x']
##' theta1<-HEAT_stat$FData[SPRE_mod$chptidx]
##' xpred<-7:46
##' SPRE_pred<-predict_SPRE(xpred=xpred,kshape=kshape,tau=tau,
##'                         theta1=theta1,predtype='rolling')
##' plot_probability(SPRE_mod,SPRE_pred)
plot_probability <- function(SPRE_object, SPRE_pred, xmin=0, xmax=max(SPRE_pred$xpred,na.rm=TRUE)) {
    betahat <- SPRE_object$betahat
    kshape <- SPRE_object$kshape
    xpred <- SPRE_pred$xpred
    x <- SPRE_object$step_reg$x
    plot(0, 0, type = "n", main = "Probability for SPRE Predictions with\neffective clinical stability (ECS)", ylab = "Probability", 
        xlim = c(xmin, xmax), ylim = c(0, 1))
    xline<-seq(xmin,xmax,length.out=200)
    yline<-pweibull(xline,scale = betahat, shape = kshape)
    lines(xline,yline,lwd=2)
    abline(v = 0, col = "blue", lty = 3)
}

##' plot weibull
##'
##' plot the weibull distribution from SPRE model
##' @title plot weibull
##' @param SPRE_object an object of class 'SPRE'
##' @param SPRE_pred an object of class 'SPRE_pred'
##' @return output plots to device
##' @author Deborah Weissman-Miller
##' @export
##' @importFrom graphics plot
##' @importFrom stats na.omit
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
##' SPRE_mod<-SPRE(step_reg,chptidx=11,lastidx=14)
##' kshape<-SPRE_mod$kshape
##' idxmaxFstat<-which.max(SPRE_mod$step_reg$fDist[,'Fstat'])
##' tau<-SPRE_mod$step_reg$fDist[idxmaxFstat,'x']
##' theta1<-HEAT_stat$FData[SPRE_mod$chptidx]
##' xpred<-7:46
##' SPRE_pred<-predict_SPRE(xpred=xpred,kshape=kshape,tau=tau,
##'                         theta1=theta1,predtype='rolling')
##' plot_weibull(SPRE_mod,SPRE_pred)
plot_weibull <- function(SPRE_object, SPRE_pred) {
    tablepred <- SPRE_pred$tablepred
    z <- as.numeric(na.omit(tablepred[, "Theta_t"]))
    fit.weibull <- fitdistrplus::fitdist(z, "weibull", lower = c(0, 0))
    plot(fit.weibull, cex.main = 0.8)
}

##' stability analysis
##'
##' stability analysis of SPRE predictions
##' @title stability analysis
##' @param SPRE_pred an object of class 'SPRE_pred'
##' @param digits an integer indicating the number of decimal places
##' @param npred number of predictions to be analysed
##' @return output information to console
##' @author Deborah Weissman-Miller
##' @export
##' @references 
##' Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012).  New single-subject and small-n design in occupational therapy:  Application to weight loss in obesity.  American Journal of Occupational Therapy, 66, 455-462. DOI: http://dx.doi.org/10.5014/ajot.2012.004788
##' Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184 DOI: http://dx.doi.org/10.1515/ijb-2012-0049
##' Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.
##' Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.
##' Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.
##' @examples
##' skip_indices<-1:2
##' step_reg<-stepwise_regression(HEAT_stat$Session,HEAT_stat$FData,skip_indices)
##' SPRE_mod<-SPRE(step_reg,chptidx=11,lastidx=14)
##' kshape<-SPRE_mod$kshape
##' idxmaxFstat<-which.max(SPRE_mod$step_reg$fDist[,'Fstat'])
##' tau<-SPRE_mod$step_reg$fDist[idxmaxFstat,'x']
##' theta1<-HEAT_stat$FData[SPRE_mod$chptidx]
##' xpred<-7:46
##' SPRE_pred<-predict_SPRE(xpred=xpred,kshape=kshape,tau=tau,
##'                         theta1=theta1,predtype='rolling')
##' prediction_stability(SPRE_pred)
prediction_stability <- function(SPRE_pred, digits = 2, npred = 20) {
    pred_round <- format(round(SPRE_pred$tablepred[1:npred, "Theta_t"], digits = digits))
    pred_dup <- duplicated(pred_round, fromLast = TRUE)
    output_table <- cbind(pred_round, ifelse(pred_dup, "**", "--"))
    colnames(output_table) <- c("Prediction", "Duplicate")
    
    cat("Predictions, significant digits = ", digits, ", rows = 1:", npred, "\n", sep = "")
    if (any(pred_dup)) {
        cat("First predicted duplicate value:", pred_round[pred_dup], "\n", sep = "")
    } else {
        cat("No duplicate found", "\n")
    }
    print(noquote(output_table))
}
