#' Bootstrapping for linear regression.
#' @param formula an object of class "formula" (or one that can be coerced to 
#'        that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible 
#'        by `as.data.frame` to a data frame) containing the variables 
#'        in the model.
#' @param R the number of bootstrap samples.
#' @param ... other parameters passed to [stats::lm()].
#' @return a list object.
#' @rdname lmboot
#' @examples
#' data(mtcars)
#' boot_out <- lm_boot(formula = mpg ~ cyl + disp + hp + wt, data = mtcars)
#' summary(boot_out)
lm_boot <- function(formula, data, R = 1000, ...) {
	results <- list()
	results$full_model <- lm(formula = formula, data = data, ...)
	results$coeficients <- data.frame(row.names = 1:R)
	for(i in names(results$full_model$coefficients)) {
		results$coeficients[,i] <- NA
	}
	results$fit <- data.frame(row.names = 1:R,
							  F_statistic = rep(NA, R),
							  df_numerator = rep(NA, R),
							  df_denominator = rep(NA, R),
							  R_squared = rep(NA, R),
							  Adjusted_R_squared = rep(NA, R))
	results$sample <- list()
	for(i in seq_len(R)) {
		results$sample <- sample(nrow(data), size = nrow(data), replace = TRUE)
		boot_data <- data[results$sample,]
		boot_lm_out <- lm(formula = formula, data = boot_data, ...)
		boot_lm_out_summary <- summary(boot_lm_out)
		results$coeficients[i,] <- boot_lm_out$coefficients
		results$fit[i,] <- c(boot_lm_out_summary$fstatistic, 
							 boot_lm_out_summary$r.squared, 
							 boot_lm_out_summary$adj.r.squared)
	}
	class(results) <- c('lmboot', 'list')
	return(results)
}

#' Plot function for bootstrapping linear regression.
#' @param x result of `lm_boot()`.
#' @param ... currently not used.
#' @rdname lmboot
plot.lmboot <- function(x, ...) {
	coef <- reshape2::melt(x$coeficients, id.vars = NULL)
	ggplot(coef, aes(x = value, y = variable)) + 
		ggdist::stat_halfeye(side = 'top') +
		scale_y_discrete(limits = rev) +
		xlab('Estimate')
}

#' Summary function for bootstrapping linear regression.
#' @param object result of `lm_boot()`.
#' @param ... currently not used.
#' @rdname lmboot
summary.lmboot <- function(object, ...) {
	ncols <- ncol(object$coeficients)
	summary_table <- data.frame(
		row.names = names(object$coeficients),
		boot_mean = rep(NA, ncols),
		boot_se = rep(NA, ncols),
		boot_median = rep(NA, ncols))
	for(i in row.names(summary_table)) {
		summary_table[i,] <- c(
			mean(object$coeficients[,i]),
			sd(object$coeficients[,i]),
			median(object$coeficients[,i])
		)
	}
	summary_table <- cbind(summary(object$full_model)$coefficients[,1:2], summary_table) |> as.data.frame()
	cat('Coefficients:\n')
	print(summary_table)
	cat('\nR-Squared:', mean(object$fit$R_squared), '(se =', sd(object$fit$R_squared), ')\n')
	cat('Adjusted R-Squared:', mean(object$fit$Adjusted_R_squared), '(se =', sd(object$fit$Adjusted_R_squared), ')')
	invisible(summary_table)
}

