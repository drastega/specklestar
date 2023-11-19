df <- expand.grid(x = 0:511, y = 0:511)
df <- dplyr::mutate(df, z = x + y, r = sqrt(x ^ 2 + y ^ 2))
base_matrix <- matrix(df$z, nrow = 512)
base_matrix <- base_matrix[, 512:1]
exp_matrix <- exp(-df$r / 3e2)
exp_matrix <- matrix(exp_matrix, nrow = 512)
exp_matrix <- exp_matrix[, 512:1]
noise <- rnorm(length(base_matrix), sd = 1)

filter_matrix <- matrix(df$r, nrow = 512)
filter_matrix <- filter_matrix[, 512:1]
filter_matrix <- ifelse(filter_matrix > 350, 0, 1)

result_matrix <- (14.6 + 5 * sin(0.02 * base_matrix)) * exp_matrix * filter_matrix + noise
result_matrix <- (14.6 + 5 * sin(0.02 * base_matrix)) * exp_matrix + noise

imageviewer::imageviewer(filter_matrix)
imageviewer::imageviewer(result_matrix)
