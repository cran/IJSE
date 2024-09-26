#' Calculate Infinite-Jackknife-Based Standard Errors for brms Models
#'
#' Computes infinite-jackknife-based standard errors for fixed effects parameters
#' from a `brmsfit` model object. The function handles both clustered and independent data.
#'
#' @param fit A `brmsfit` object resulting from fitting a model using the `brms` package.
#' @param cluster_var An optional vector indicating the cluster membership for each observation.
#'   If `NULL`, the function treats the data as independent.
#'
#' @return A named vector of standard errors for the fixed effects parameters.
#' @examples
#' \donttest{
#' # Load libraries
#'
#' library(brms)
#'
#' # Set a seed for reproducibility
#'
#' set.seed(42)
#'
#' ### Model 1: Linear Regression using brms
#'
#' # Simulate data
#'
#' n <- 300
#' age <- rnorm(n, mean = 40, sd = 10)
#' income <- rnorm(n, mean = 50000, sd = 10000)
#' education_years <- rnorm(n, mean = 12, sd = 2)
#'
#' # True coefficients
#'
#' beta_0 <- 50000    # Intercept
#' beta_age <- -1000  # Age effect
#' beta_income <- 0.5 # Income effect
#' beta_edu <- 2000   # Education effect
#' sigma <- 10000     # Residual standard deviation
#'
#' # Simulate house prices
#'
#' house_price <- beta_0 + beta_age * age + beta_income * income +
#'   beta_edu * education_years + rnorm(n, mean = 0, sd = sigma)
#'
#' # Create data frame
#'
#' data_linear <- data.frame(house_price, age, income, education_years)
#'
#' # Fit the model
#'
#' fit_linear <- brm(
#'   formula = house_price ~ age + income + education_years,
#'   data = data_linear,
#'   family = gaussian(),
#'   seed = 42
#' )
#'
#' # Summary
#'
#' summary(fit_linear)
#'
#' # Obtain IJ-based SE
#'
#' IJ_se(fit_linear)
#'
#' ### Model 2: Linear Regression for Clustered Data using brms
#'
#' # Simulate data
#'
#' n_schools <- 30
#' students_per_school <- 100
#' n <- n_schools * students_per_school
#'
#' # School IDs and types
#'
#' school_id <- rep(1:n_schools, each = students_per_school)
#' school_type <- rep(sample(c("Public", "Private"), n_schools, replace = TRUE),
#'                    each = students_per_school)
#' school_type_num <- ifelse(school_type == "Public", 0, 1)
#'
#' # Random intercepts for schools
#'
#' sigma_school <- 6
#' u_school <- rnorm(n_schools, mean = 0, sd = sigma_school)
#' u_school_long <- rep(u_school, each = students_per_school)
#'
#' # Student-level predictors
#'
#' student_age <- rnorm(n, mean = 15, sd = 1)
#' math_score <- rnorm(n, mean = 50, sd = 10)
#'
#' # True coefficients
#'
#' beta_0 <- 50             # Fixed intercept
#' beta_age <- 1.5          # Age effect
#' beta_math <- 1           # Math score effect
#' beta_school_type <- 5    # School type effect
#' sigma_student <- 3       # Residual standard deviation
#'
#' # Simulate reading scores
#'
#' reading_score <- beta_0 + beta_age * student_age + beta_math * math_score +
#'   beta_school_type * school_type_num + u_school_long +
#'   rnorm(n, mean = 0, sd = sigma_student)
#'
#' # Create data frame
#'
#' data_clustered <- data.frame(
#'   reading_score,
#'   student_age,
#'   math_score,
#'   school_id = factor(school_id),
#'   school_type,
#'   student_id = 1:n
#' )
#'
#' # Fit the model
#'
#' fit_clustered <- brm(
#'   formula = reading_score ~ student_age + math_score + school_type,
#'   data = data_clustered,
#'   family = gaussian(),
#'   seed = 42
#' )
#'
#' # Summary
#'
#' summary(fit_clustered)
#'
#' # Obtain IJ-based SE, taking the clustering into account
#' IJ_se(fit_clustered, cluster_var = data_clustered$school_id)
#'
#' ### Example 3: Quantile Regression using brms
#'
#' # Independent data for quantile regression
#' N <- 100
#' x <- runif(N)
#' eps <- 1 * x^2 + sin(rchisq(N, 8)) + sin(rnorm(x, 3))  # some random DGP
#' y <- 2 * x + runif(N) + eps^2
#'
#' # Create data frame
#' data_quantile <- data.frame(y, x)
#'
#' # Fit quantile regression model
#' fit_quantile <- brm(
#'   formula = bf(y ~ x, quantile = .3),  # Quantile regression with 30th percentile
#'   data = data_quantile,
#'   family = asym_laplace(link_quantile = "identity"),
#'   seed = 42
#' )
#'
#' # Summary of quantile regression model
#' summary(fit_quantile)
#'
#' # Obtain IJ-based SE
#' IJ_se(fit_quantile)
#'
#' ### Example 4: Quantile Regression for Clustered Data using brms
#'
#' # Clustered data for quantile regression
#' J <- 30  # Number of clusters
#' I <- 50  # Cluster size
#' subj <- rep(1:J, each = I)
#' rho <- 0.8
#'
#' # Random effect and error terms
#' U <- rnorm(J * I, sd = sqrt(1 / 3))
#' Z <- rep(rnorm(J, sd = 5), each = I)
#' E <- rnorm(J * I)
#'
#' # Covariates and response variable
#' X <- sqrt(rho) * Z + sqrt(1 - rho) * E
#' X2 <- X^2
#' Y <- 0.1 * U + X + X2 * U
#'
#' # Create data frame
#' data_cluster_quantile <- data.frame(Y, X, X2, subj = factor(subj))
#'
#' # Fit quantile regression model
#' fit_quantile_cluster <- brm(
#'   formula = bf(Y ~ X + X2, quantile = .33),  # Quantile regression with 33rd percentile
#'   data = data_cluster_quantile,
#'   family = asym_laplace(link_quantile = "identity"),
#'   seed = 42
#' )
#'
#' # Summary of quantile regression model
#' summary(fit_quantile_cluster)
#'
#' # Obtain IJ-based SE, taking clustering into account
#' IJ_se(fit_quantile_cluster, cluster_var = data_cluster_quantile$subj)
#' }
#'
#'
#' @importFrom as_draws_matrix posterior
#' @importFrom variables posterior
#' @importFrom log_lik brms
#' @importFrom cov stats
#'
#' @export
IJ_se <- function(fit, cluster_var = NULL) {
  # Check if 'fit' is a brmsfit object
  if (!inherits(fit, "brmsfit")) {
    stop("The 'fit' argument must be a brmsfit object.")
  }

  # Extract all posterior draws
  post_draws <- as_draws_matrix(fit)

  # Get the names of all variables in the draws object
  vars <- variables(post_draws)

  # Select variables that start with 'b_'
  b_vars <- vars[grepl('^b_', vars)]

  # Check if there are any fixed effects parameters
  if (length(b_vars) == 0) {
    stop("No fixed effects parameters ('b_') found in the model.")
  }

  # Subset the draws to include only the fixed effects
  post_draws <- post_draws[, b_vars, drop = FALSE]

  # Extract log-likelihood for all observations
  post_log <- tryCatch(
    {
      log_lik(fit)
    },
    error = function(e) {
      stop("Error extracting log-likelihood from the model. Ensure 'save_pars = save_pars(all = TRUE)' was used when fitting the model.")
    }
  )

  # Convert to matrix if necessary
  post_log <- as.matrix(post_log)

  # Validate cluster_var length if provided
  if (!is.null(cluster_var)) {
    if (length(cluster_var) != ncol(post_log)) {
      stop("Length of 'cluster_var' must match the number of observations in the model.")
    }
  }

  # Independent case
  if (is.null(cluster_var)) {
    N <- ncol(post_log)
    # Compute influence function
    inf <- N * cov(post_draws, post_log)
    # Compute standard errors
    se <- sqrt(diag(cov(t(inf))) / N)
  } else {
    # Clustered case
    clusters <- unique(cluster_var)
    J <- length(clusters)
    # Aggregate log-likelihood over clusters
    sum_log_by_cluster <- matrix(NA, nrow = nrow(post_log), ncol = J)
    for (j in seq_along(clusters)) {
      idx <- which(cluster_var == clusters[j])
      sum_log_by_cluster[, j] <- rowSums(post_log[, idx, drop = FALSE])
    }
    # Compute influence function
    inf <- J * cov(post_draws, sum_log_by_cluster)
    # Compute standard errors
    se <- sqrt(diag(cov(t(inf))) / J)
  }

  # Name the SEs with parameter names
  param_names <- colnames(post_draws)
  names(se) <- param_names

  return(se)
}
