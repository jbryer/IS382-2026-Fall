# ==============================================================================
# SVM 3D PROJECTION DEMONSTRATION (FOOLPROOF HYPERPLANE FIX)
# ==============================================================================
library(e1071)
library(plotly)

set.seed(42)
n_samples <- 150

# 1. Generate Simulated 2D Concentric Circles Data
r_inner <- runif(n_samples, 0, 1.5)
theta_inner <- runif(n_samples, 0, 2 * pi)
x1_inner <- r_inner * cos(theta_inner)
x2_inner <- r_inner * sin(theta_inner)

r_outer <- runif(n_samples, 2.5, 4.0)
theta_outer <- runif(n_samples, 0, 2 * pi)
x1_outer <- r_outer * cos(theta_outer)
x2_outer <- r_outer * sin(theta_outer)

dataset <- data.frame(
	X1 = c(x1_inner, x1_outer),
	X2 = c(x2_inner, x2_outer),
	Class = as.factor(c(rep("Inner", n_samples), rep("Outer", n_samples)))
)

# 2. Explicit 3D Projection
dataset$X3 <- dataset$X1^2 + dataset$X2^2

# 3. Train Linear SVM in 3D Space
svm_3d <- svm(Class ~ X1 + X2 + X3, 
			  data = dataset, 
			  kernel = "linear", 
			  cost = 10)

# 4. FOOLPROOF FIX: Probe the model's actual decision boundary
# We evaluate standard basis points to find the plane's true linear coefficients:
# Decision Value = beta0 + beta1*X1 + beta2*X2 + beta3*X3
probe_points <- data.frame(
	X1 = c(0, 1, 0, 0),
	X2 = c(0, 0, 1, 0),
	X3 = c(0, 0, 0, 1)
)

predictions <- predict(svm_3d, probe_points, decision.values = TRUE)
d_vals <- attr(predictions, "decision.values")

# Decode the linear system from the decision boundaries
beta0 <- d_vals[1]           # Intercept
beta1 <- d_vals[2] - beta0   # X1 coefficient
beta2 <- d_vals[3] - beta0   # X2 coefficient
beta3 <- d_vals[4] - beta0   # X3 coefficient

# 5. Create the Separating Hyperplane Grid
# The hyperplane exists exactly where the Decision Value == 0
# beta0 + beta1*X1 + beta2*X2 + beta3*X3 = 0  ->  X3 = (-beta0 - beta1*X1 - beta2*X2) / beta3
x1_grid <- seq(min(dataset$X1), max(dataset$X1), length.out = 30)
x2_grid <- seq(min(dataset$X2), max(dataset$X2), length.out = 30)
grid <- expand.grid(X1 = x1_grid, X2 = x2_grid)

grid$X3 <- (-beta0 - (beta1 * grid$X1) - (beta2 * grid$X2)) / beta3
z_matrix <- matrix(grid$X3, nrow = 30, ncol = 30)

# 6. Generate Plot
plot_3d <- plot_ly() %>%
	add_trace(
		data = dataset,
		x = ~X1, y = ~X2, z = ~X3,
		color = ~Class,
		colors = c('#FF4136', '#2ECC40'),
		type = 'scatter3d',
		mode = 'markers',
		marker = list(size = 4, opacity = 0.8),
		name = ~Class
	) %>%
	add_surface(
		x = x1_grid,
		y = x2_grid,
		z = z_matrix,
		opacity = 0.5,
		colorscale = 'Greys',
		showscale = FALSE,
		name = 'Separating Plane'
	) %>%
	layout(
		title = "Perfect SVM Separation via Probed Hyperplane",
		scene = list(
			xaxis = list(title = "X1"),
			yaxis = list(title = "X2"),
			zaxis = list(title = "X3 (X1² + X2²)")
		)
	)

plot_3d
