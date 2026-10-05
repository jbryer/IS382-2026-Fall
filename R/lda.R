# Linear Discriminant Analysis
library(MASS)
library(ggplot2)

ggplot2::theme_set(ggplot2::theme_minimal())

data("iris")
data("penguins")

penguins <- penguins |> 
	tidyr::drop_na() |>
	dplyr::mutate(species = as.factor(as.character(species))) |>
	dplyr::mutate(dplyr::across(c('bill_len', 'bill_dep', 'flipper_len', 'body_mass'), 
								function(x) { c(scale(x)) }))

ggplot(penguins, aes(x = bill_len, y = flipper_len, color = species)) + 
	geom_point() + 
	coord_equal() +
	theme(legend.position = 'bottom')

penguins2 <- penguins |> dplyr::filter(species %in% c('Gentoo', 'Adelie'))
ggplot(penguins2, aes(x = bill_len, y = flipper_len, color = species)) + 
	geom_point()


lda2 <- MASS::lda(species ~ flipper_len, data = penguins2)
lda2
x <- seq(min(penguins2$flipper_len), max(penguins2$flipper_len), length.out = 300)
nd <- data.frame(
	flipper_len = x,
	species = predict(lda2, newdata = data.frame(flipper_len = x))$class
)
head(nd)
ggplot(nd, aes(x = flipper_len, color = species)) +
	geom_density(data = penguins2) +
	geom_point(y = 0)

thedata <- penguins
fit <- lda(species ~ bill_len + flipper_len, data = penguins)
# Extract scaling coefficients (the linear discriminant coefficients)
beta <- fit$scaling 
# Calculate the slope and intercept for the separating line in (var1, var2) space
slope <- -beta[1,1] / beta[2,1]
intercept <- mean(fit$means[,1] %*% beta[,1]) / beta[2,1]
ggplot(thedata, aes(x = bill_len, y = flipper_len, color = species)) + 
	geom_point() +
	# geom_abline(slope = -1 * (1 / slope), intercept = 0, color = 'blue') +
	geom_abline(slope = slope, intercept = intercept, color = 'black') +
	coord_equal()



groups <- unique(penguins2$species) |> as.character()
centers <- expand.grid(groups, c('bill_len', 'flipper_len'))
names(centers) <- c('group', 'variable')
centers$group <- as.character(centers$group)
centers$variable <- as.character(centers$variable)
centers$mean <- NA
for(i in 1:nrow(centers)) {
	centers[i,'mean'] <- mean(penguins2[penguins2$species == centers[i,'group'], centers[i,'variable']], na.rm = TRUE)
}
centers <- centers |> reshape2::dcast(group ~ variable, id = 'group', value.var = 'mean')
centers
ggplot(penguins2, aes(x = bill_len, y = flipper_len, color = species)) + 
	geom_point() +
	geom_point(data = centers, aes(color = group), size = 6) +
	geom_point(data = centers, color = 'black', size = 6, pch = 21)

lda_penguins2 <- MASS::lda(species ~ bill_len + flipper_len, data = penguins2)
lda_penguins2


lda_penguins <- MASS::lda(species ~ bill_len + flipper_len, data = penguins)
# lda_penguins <- MASS::lda(species ~ ., data = penguins)
lda_penguins
terms(lda_penguins$call, data = penguins)

predict(lda_penguins)

penguins_fit <- cbind(penguins, predict(lda_penguins))
ggplot(penguins_fit, aes(x = x.LD1, y = x.LD2, color = species)) +
	geom_point()


klaR::partimat(species ~ bill_len + flipper_len, 
			   data = penguins2, 
			   method = "lda", 
			   plot.matrix = TRUE)
data(penguins)
thedata <- penguins
lda_model <- MASS::lda(species ~ bill_len + flipper_len, data = thedata)

lda_model

library(klaR)
library(ggord) # remotes::install_github('fawda123/ggord')

p <- predict(lda_model, thedata)
ldahist(data = p$x[,1], g = thedata$species)
ggord(lda_model, thedata$species, ylim = c(-10, 10))
partimat(species ~ bill_len + flipper_len, data = thedata, method = "lda")

lda_partion_plot <- function(lda_model, thedata) {
	np <- 300
	
	response_var <- as.character(attr(lda_model$terms, 'predvars'))[2]
	predict_vars <- as.character(attr(lda_model$terms, 'predvars'))[-c(1:2)]
	var_pairs <- list()
	for(i in 1:(length(predict_vars)-1)) {
		for(j in (i+1):length(predict_vars)) {
			vars = c(predict_vars[i], predict_vars[j])
			x <- seq(from = min(thedata[,vars[1],drop=TRUE]),
					 to = max(thedata[,vars[1],drop=TRUE]),
					 length.out = np)
			y <- seq(from = min(thedata[,vars[2],drop=TRUE]),
					 to = max(thedata[,vars[2],drop=TRUE]),
					 length.out = np)
			var_pairs[[length(var_pairs) + 1]] <- vars
		}
	}
	
	nd <- data.frame(row.names = seq(1, np))
	for(i in predict_vars) {
		nd[,i] <- seq(from = min(thedata[,i,drop=TRUE]),
					  to = max(thedata[,i,drop=TRUE]),
					  length.out = np)
	}
	predictions <- predict(lda_model, newdata = nd)
	nd[,response_var] <- as.integer(predictions$class)
	
	plots <- list()
	for(i in seq_along(var_pairs)) {
		nd2 <- expand.grid(Var1 = nd[,var_pairs[[i]][1]],
						   Var2 = nd[,var_pairs[[i]][2]])
		names(nd2) <- var_pairs[[i]]
		nd2[,response_var] <- rep(nd[,response_var])
		
		plots[[i]] <- ggplot(thedata, aes(x = .data[[var_pairs[[i]][1]]], 
										  y = .data[[var_pairs[[i]][2]]])) +
			geom_contour(data = nd2, aes(z = .data[[response_var]])) +
			geom_point(aes(color = .data[[response_var]])) +
			theme(legend.position = 'none')
	}
	cowplot::plot_grid(plotlist = plots)
	
	ggplot() +
		geom_contour(data = nd2, aes(x = .data[[var_pairs[[i]][1]]], 
									 y = .data[[var_pairs[[i]][2]]],
									 z = .data[[response_var]])) +
		geom_point(data = thedata, aes(x = .data[[var_pairs[[i]][1]]], 
									   y = .data[[var_pairs[[i]][2]]],
									   color = .data[[response_var]])) +
		theme(legend.position = 'none')
	
	plots[[1]]
	plots[[2]]
	plots[[3]]
	
	# nd.x <- seq(from = min(thedata[,predict_vars[1], drop = TRUE]), 
	# 		    to = max(thedata[,predict_vars[1], drop = TRUE]),
	# 		    length.out = np)
	# nd.y <- seq(from = min(thedata[,predict_vars[2], drop = TRUE]), 
	# 		    to = max(thedata[,predict_vars[2], drop = TRUE]), 
	# 		    length.out = np)
	# nd <- expand.grid(Var1 = nd.x, Var2 = nd.y)
	# names(nd) <- predict_vars[1:2]
	# predictions <- predict(lda_model, newdata = nd)
	# nd[,response_var] <- as.integer(predictions$class)
	
	# df <- data.frame(
	# 	ld1 = seq(from = min(predictions$x[,1]), to = max(predictions$x[,1]), length.out = np), # LD1 scores
	# 	ld2 = seq(from = min(predictions$x[,2]), to = max(predictions$x[,2]), length.out = np) # LD2 scores
	# 	# class = predictions
	# )
	# df <- data.frame(
	# 	ld1 = seq(from = min(predictions$x[,1]), to = max(predictions$x[,1]), length.out = np), # LD1 scores
	# 	class = predictions
	# )
	ggplot(thedata, aes(x = bill_len, y = flipper_len, color = species)) +
		geom_point() + 
		geom_contour(data = nd, aes(z = species))
	
}


lda_penguins$means
lda_penguins$scaling

tmp <- cbind(species = penguins$species, as.data.frame(predict(lda_penguins)$x))
head(tmp)

ggplot(tmp, aes(x = LD1, y = LD2, color = species)) +
	geom_point()

tmp |> reshape2::melt(tmp, id.vars = 'species', measure.vars = c('LD1', 'LD2'), variable.name = 'LD') |> #head()
	ggplot(aes(x = value, color = species)) +
	geom_density() +
	facet_wrap(~ LD, ncol = 1)

ggplot(tmp, aes(x = LD1, y = LD2, color = species)) +
	geom_point()



ggplot(penguins, aes(x = bill_len, y = flipper_len, color = species)) + 
	geom_abline(intercept = lda_penguins$scaling['bill_len',1],
				slope = lda_penguins$scaling['flipper_len',2],
				color = 'black') +
	geom_point()


