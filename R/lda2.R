library(MASS)
library(ggplot2)
ggplot2::theme_set(ggplot2::theme_minimal())

data("penguins")
names(penguins)

df <- penguins |>
	# dplyr::select(species, bill_len, flipper_len) |>
	# dplyr::filter(species %in% c('Adelie', 'Gentoo')) |>
	tidyr::drop_na() |>
	dplyr::mutate(species = as.factor(as.character(species))) |>
	dplyr::mutate(dplyr::across(c('bill_len', 'bill_dep', 'flipper_len', 'body_mass'), 
								function(x) { c(scale(x)) }))

# Fit the LDA model
# For now, only support 2 predictors
# fit <- lda(species ~ bill_len + flipper_len + bill_dep, data = df)
fit <- lda(species ~ bill_len + flipper_len, data = df)

# Compute the inverse pooled covariance matrix (Sigma^-1)
# MASS::lda scaling matrix V satisfies V^T %*% Sigma %*% V = I, so Sigma^-1 = V %*% V^T
inv_cov <- fit$scaling %*% t(fit$scaling)


# Helper function to compute the decision boundary between two species
get_boundary <- function(sp1, sp2, fit) {
	# Calculate classification coefficients (W) and intercepts (b) for each species
	W <- fit$means %*% inv_cov
	b <- -0.5 * rowSums(fit$means * W) + log(fit$prior)
	
	vars = dimnames(W)[[2]]
	
	# Boundary equation: (W[sp1,] - W[sp2,]) %*% X + (b[sp1] - b[sp2]) = 0
	# bill_length is on x-axis (index 1), flipper_length is on y-axis (index 2)
	w_diff <- W[sp1, vars] - W[sp2, vars]
	b_diff <- b[sp1] - b[sp2]
	slope <- -w_diff[1] / w_diff[2]
	intercept <- -b_diff / w_diff[2]
	data.frame(
		Pair = paste(sp1, "vs", sp2), 
		Var1 = sp1,
		Var2 = sp2,
		Slope = slope, 
		Intercept = intercept,
		row.names = NULL
	)
}

boundaries <- data.frame()
groups <- unique(df$species) |> as.character()
for(i in seq(1, length(groups) - 1)) {
	for(j in seq(i + 1, length(groups))) {
		boundaries <- rbind(boundaries, get_boundary(groups[i], groups[j], fit))
	}
}
boundaries

pair <- 1

project_onto_line <- function(x, y, slope, intercept) {
	m <- slope
	c <- intercept
	# Orthogonal projection formulas derived by intersecting the target line 
	# with a perpendicular line passing through each individual point (x, y)
	proj_x <- (x + m * (y - c)) / (1 + m^2)
	proj_y <- m * proj_x + c
	return(data.frame(x = proj_x, y = proj_y, row.names = 1:length(proj_x)))
}

df2 <- project_onto_line(x = df$bill_len, 
						 y = df$flipper_len, 
						 slope = boundaries[pair,]$Slope,
						 intercept = boundaries[pair,]$Intercept)
boundaries[pair,]
df2 <- cbind(df, df2)
df2 <- df2 |> dplyr::filter(species %in% c(boundaries[pair,]$Var1, boundaries[pair,]$Var2))

#' Create the density distributions on a two dimensional plot.
#' 
#' This presumes that all the points, x and y, fall on a line. See the
#' [project_onto_line()] function.
#' 
#' @param x, y the variable to calculate the density for.
#' @param scale_factor scale factor to expand the height of the density curves.
#'        A value of 1 works well for standardized values. A negative scale
#'        factor will put the curve on the other side of the line.
#' @param the slope of the line. This function assumes all points fall exactly
#'        on a line. The default is to cacluate the slope from the first and 
#'        last values in `x` and `y`.
#' @param n the number of equally spaced points at which the density is to be 
#'        estimated. See [stats::density()] for more information.
#' @return a data.frame with two columns, `x` and `y`. Note that the data.frame
#'        will be sorted which means the values will not be in the same order
#'        as the `x` and `y` parameters.
#' @export
rotated_density <- function(
		x, y,
		slope = (y[1] - y[length(y)]) / (x[1] - x[length(y)]), 
		scale_factor = -1, 
		n = 512
) {
	dens <- stats::density(x, n = n)
	density_val <- stats::approx(dens$x, dens$y, xout = x)$y
	result <- data.frame(
		density_val = density_val,
		projected_x = x + (density_val * scale_factor) * slope / sqrt(1 + slope^2),
		projected_y = y - (density_val * scale_factor) * (1 / sqrt(1 + slope^2)),
		x = x,
		y = y
	)
	result <- result[order(x),]
	return(result)
}

rd <- rotated_density(x = df_var1$x, y = df_var1$y)
ggplot(df_var1, aes(x = x, y = y)) + geom_point() + geom_path(data = rd, aes(x = projected_x, y = projected_y))


df_var1 <- df2 |> dplyr::filter(species == boundaries[pair,]$Var1)
df_var2 <- df2 |> dplyr::filter(species == boundaries[pair,]$Var2)

dens1 <- stats::density(df_var1$x, n = 512)
dens2 <- stats::density(df_var2$x, n = 512)

df_var1$density_val <- approx(dens1$x, dens1$y, xout = df_var1$x)$y
df_var2$density_val <- approx(dens2$x, dens2$y, xout = df_var2$x)$y

df_var1$dens_offset_x <- df_var1$x + (df_var1$density_val * scale_factor) * (boundaries[pair,]$Slope / sqrt(1 + boundaries[pair,]$Slope^2))
df_var1$dens_offset_y <- df_var1$y - (df_var1$density_val * scale_factor) * (1 / sqrt(1 + boundaries[pair,]$Slope^2))
df_var2$dens_offset_x <- df_var2$x + (df_var2$density_val * 1*scale_factor) * (boundaries[pair,]$Slope / sqrt(1 + boundaries[pair,]$Slope^2))
df_var2$dens_offset_y <- df_var2$y - (df_var2$density_val * 1*scale_factor) * (1 / sqrt(1 + boundaries[pair,]$Slope^2))

df_density_curve1 <- df_var1[order(df_var1$x), ]
df_density_curve2 <- df_var2[order(df_var2$x), ]

nd2 <- data.frame(
	bill_len = seq(min(df$bill_len), max(df$bill_len), length.out = 150),
	flipper_len = seq(min(df$flipper_len), max(df$flipper_len), length.out = 150)
)
nd2 <- expand.grid(nd2)
nd2$species <- predict(fit, newdata = nd2)$class

df$predict_species <- predict(fit)$class
df$predict_good <- df$predict_species == df$species
table(df$predict_good)

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	geom_point(data = nd2, size = 10, alpha = 0.005, pch = 15) +
	# geom_segment(data = df2, aes(xend = x, yend = y), linetype = 2, alpha = 0.5) +
	geom_point(alpha = 0.5) +
	geom_point(data = df2, aes(x = x, y = y), alpha = 0.5) +
	geom_abline(data = boundaries, aes(slope = Slope, intercept = Intercept, color = Pair), size = 1.5) +
	geom_path(data = df_density_curve1, aes(x = dens_offset_x, y = dens_offset_y, color = species),
			  size = 1.2, alpha = 0.75) +
	geom_path(data = df_density_curve2, aes(x = dens_offset_x, y = dens_offset_y, color = species),
			  size = 1.2, alpha = 0.75) +
	geom_point(data = df[!df$predict_good,], color = 'black', pch = 1, size = 3) +
	scale_color_brewer(type = 'qual', palette = 2) +
	coord_equal()

##### Step through the process
df <- penguins |>
	# dplyr::select(species, bill_len, flipper_len) |>
	dplyr::filter(species %in% c('Adelie', 'Gentoo')) |>
	tidyr::drop_na() |>
	dplyr::mutate(species = as.factor(as.character(species))) |>
	dplyr::mutate(dplyr::across(c('bill_len', 'bill_dep', 'flipper_len', 'body_mass'), 
								function(x) { c(scale(x)) }))

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	geom_point(alpha = 0.5) +
	coord_equal() +
	theme(legend.position = 'bottom')

fit <- MASS::lda(species ~ bill_len + flipper_len, data = df)
centers <- fit$means |> as.data.frame()
centers$species <- row.names(centers)
middle <- c(min(centers[,1]) + diff(centers[,1]) / 2, min(centers[,2]) + diff(centers[,2] / 2))
names(middle) <- c('bill_len', 'flipper_len')

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	geom_point(alpha = 0.5) +
	geom_point(data = centers, size = 5) +
	geom_point(x = middle['bill_len'], y = middle['flipper_len'], color = 'black') +
	coord_equal() +
	theme(legend.position = 'bottom')

slope <- -1
intercept <- middle[2] - slope * middle[1]

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	geom_point(alpha = 0.5) +
	geom_point(data = centers, size = 5) +
	geom_point(x = middle['bill_len'], y = middle['flipper_len'], color = 'black') +
	geom_abline(intercept = intercept, slope = slope) +
	coord_equal() +
	theme(legend.position = 'bottom')

df_projected <- project_onto_line(x = df$bill_len, y = df$flipper_len, slope = slope, intercept = intercept)
df_projected <- cbind(df_projected, df[,c('bill_len', 'flipper_len', 'species')])

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	geom_segment(data = df_projected, aes(xend = x, yend = y), linetype = 2) +
	geom_point(alpha = 0.5) +
	geom_point(data = centers, size = 5) +
	geom_point(x = middle['bill_len'], y = middle['flipper_len'], color = 'black') +
	geom_abline(intercept = intercept, slope = slope) +
	coord_equal() +
	theme(legend.position = 'bottom')

density_curve1 <- rotated_density(x = df_projected[df_projected$species == 'Adelie',]$x,
								  y = df_projected[df_projected$species == 'Adelie',]$y)
density_curve1$species <- 'Adelie'
density_curve2 <- rotated_density(x = df_projected[df_projected$species == 'Gentoo',]$x,
								  y = df_projected[df_projected$species == 'Gentoo',]$y)
density_curve2$species <- 'Gentoo'

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	geom_segment(data = df_projected, aes(xend = x, yend = y), linetype = 2) +
	geom_point(data = df_projected, aes(x = x, y = y), size = 2, alpha = 0.5) +
	geom_path(data = density_curve1, aes(x = projected_x, y = projected_y, color = species), size = 1.2) +
	geom_path(data = density_curve2, aes(x = projected_x, y = projected_y, color = species), size = 1.2) +
	geom_point(alpha = 0.5) +
	geom_point(data = centers, size = 5) +
	geom_point(x = middle['bill_len'], y = middle['flipper_len'], color = 'black') +
	geom_abline(intercept = intercept, slope = slope) +
	coord_equal() +
	theme(legend.position = 'bottom')

# Look at the final solution
boundary <- get_boundary('Adelie', 'Gentoo', fit)

# df_projected <- project_onto_line(x = df$bill_len, y = df$flipper_len, 
# 								  slope = boundary$Slope, intercept = boundary$Intercept)
df_projected <- project_onto_line(x = df$bill_len, y = df$flipper_len, 
								  slope = perp_slope, intercept = perp_intercept)
df_projected <- cbind(df_projected, df[,c('bill_len', 'flipper_len', 'species')])

density_curve1 <- rotated_density(x = df_projected[df_projected$species == 'Adelie',]$x,
								  y = df_projected[df_projected$species == 'Adelie',]$y)
density_curve1$species <- 'Adelie'
density_curve2 <- rotated_density(x = df_projected[df_projected$species == 'Gentoo',]$x,
								  y = df_projected[df_projected$species == 'Gentoo',]$y)
density_curve2$species <- 'Gentoo'

ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	# geom_segment(data = df_projected, aes(xend = x, yend = y), linetype = 2) +
	geom_point(data = df_projected, aes(x = x, y = y), size = 2, alpha = 0.5) +
	geom_path(data = density_curve1, aes(x = projected_x, y = projected_y, color = species), size = 1.2) +
	geom_path(data = density_curve2, aes(x = projected_x, y = projected_y, color = species), size = 1.2) +
	geom_point(alpha = 0.5) +
	geom_point(data = centers, size = 5) +
	geom_point(x = middle['bill_len'], y = middle['flipper_len'], color = 'black') +
	geom_abline(intercept = boundary$Intercept, slope = boundary$Slope) +
	coord_equal() +
	theme(legend.position = 'bottom')

rotated_descriptives <- function(x, y, slope = (y[1] - y[length(y)]) / (x[1] - x[length(y)])) {
	theta <- atan(slope)
	# Rotate coordinates to make the line horizontal
	x_flat <- x * cos(theta) + y * sin(theta)
	# y_flat <- -x * sin(theta) + y * cos(theta) # In case I want this later
	return(c(mean = mean(x_flat), sd = sd(x_flat)))
}
(desc1 <- rotated_descriptives(density_curve1$x, density_curve1$y))
(desc2 <- rotated_descriptives(density_curve2$x, density_curve2$y))

unname((desc1['mean'] - desc2['mean'])^2 / (desc1['sd'] + desc2['sd'])) # What we are trying to maximize

perp_slope <- -1 / boundary$Slope
perp_intercept <- middle['flipper_len'] - perp_slope * middle['bill_len']


ggplot(df, aes(x = bill_len, y = flipper_len, color = species)) +
	# geom_segment(data = df_projected, aes(xend = x, yend = y), linetype = 2) +
	geom_path(data = density_curve1, aes(x = projected_x, y = projected_y, color = species), size = 1.2) +
	geom_path(data = density_curve2, aes(x = projected_x, y = projected_y, color = species), size = 1.2) +
	geom_point(alpha = 0.5) +
	geom_point(data = centers, size = 5) +
	geom_point(x = middle['bill_len'], y = middle['flipper_len'], color = 'black') +
	geom_abline(intercept = boundary$Intercept, slope = boundary$Slope) +
	geom_abline(intercept = perp_intercept, slope = perp_slope) +
	coord_equal() +
	theme(legend.position = 'bottom')

