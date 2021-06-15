
# 前準備 ---------------------------------------------------------------------

packages <- c(
  "tidyverse",  # データの整形など
  "mvtnorm",    # 多次元正規分布の生成
  "GGally",     # データの相関関係の図表作成
  "broom",      # 線形回帰の結果を扱いやすくする
  "forester",   # 線形回帰の係数の図表作成
  "car"         # vif 計算のため
  )

for (package in packages) {
  library(package, character.only = TRUE)
}

set.seed(1111)


# パラメータの設定 ----------------------------------------------------------------

# 曝露モデルのパラメータ
a0 <- 3

a1 <- 3

a2 <- 3

# アウトカムモデルのパラメータ
b0 <- 5

b1 <- 5

b2 <- 5

b3 <- 5

# サンプルサイズ
n <- 100

# モデルの設定 ------------------------------------------------------------------

# 曝露モデル
exposure_model <- function(c1, c2){
  a0 + a1*c1 + a2*c2
}
  
# アウトカムモデル
outcome_model <- function(x1, c1, c2){
  b0 + b1*x1 + b2*c1 + b3*c2
}


# データの生成 ------------------------------------------------------------

# y(アウトカム), x1(曝露), c1, c2(交絡因子)を生成する。
# c1とc2は互いに関連しており、その相関係数はρであるとする。
# 相関係数が高い場合（ρ = 0.9）と低い場合(ρ = 0.1)の二つのデータセットを生成する。

# データ生成のための関数作成
simulate_data <- function(rho, n){
  # 2次元正規分布からc1とc2を生成
  mu_c12 <- c(0, 0)
  cov_c12_high <- rho # sigma_2 = sigma_3 = 1なので、cov=ρとなる。
  sigma_c1 <- 1
  sigma_c2 <- 1
  sigma_c12 <-  matrix(
    c(sigma_c1, cov_c12_high, cov_c12_high, sigma_c2), 
    ncol = 2
  )
  
  norm_2d <- rmvnorm(
    n = n, 
    mean = mu_c12, 
    sigma = sigma_c12
  )
  
  c1 <- norm_2d[, 1]
  c2 <- norm_2d[, 2]
  
  # 曝露モデルに従って、x1を生成
  mu_1 <- exposure_model(c1, c2)
  x1 <- rnorm(n = n, mean = mu_1, sd = 10)
  
  # アウトカムモデルに従って、yを生成
  mu_y <- outcome_model(x1, c1, c2)
  y <- rnorm(n = n, mean = mu_y, sd = 10)
  
  # 一つのデータフレームにまとめる
  data_high_cor <- tibble(y, x1, c1, c2)
}

# 交絡因子間の相関係数が高い場合(ρ = 0.9)
rho_high <- 0.9
data_high_corr <- simulate_data(rho = rho_high, n = n)

 ## チェック
GGally::ggpairs(
  data = data_high_corr, 
  lower = list(continuous = wrap("density")))

# 交絡因子間の相関係数が低い場合(ρ = 0.1)
rho_low <- 0.1
data_low_corr <- simulate_data(rho = rho_low, n = n)
 ## チェック
GGally::ggpairs(
  data = data_low_corr, 
  lower = list(continuous = wrap("density")))


# 分析 ----------------------------------------------------------------------

# forest plot のための関数作成
plot_result <- function(data){
  # 線形回帰
  fit <- data %>% 
    lm(y ~ x1 + c1 + c2, data = .) %>% 
    tidy(conf.int = TRUE)
  # 結果の図示
  forester(left_side_data = fit[1], 
           estimate = fit$estimate, 
           ci_low = fit$conf.low, 
           ci_high = fit$conf.high, 
           display = TRUE)
}

# 相関係数が高い場合(ρ = 0.9)
plot_result(data = data_high_corr) 
            
# 相関係数が低い場合(ρ = 0.1)
plot_result(data_low_corr)


# variance inflation factor の計算 -------------------------------------------

# 関数作成
compute_vif <- function(data){
  data %>% 
    lm(y ~ x1 + c1 + c2, data = .) %>% 
    vif()
}

# 高い場合
data_high_corr %>% compute_vif()

# 低い場合
data_low_corr %>% compute_vif()


# サンプルサイズを増やしたときのシミュレーション -------------------------------------------------

# nを増やす(元々の千倍に)
n_big <- n*1000

# データを再生成
data_high_corr_big_number <- simulate_data(rho = rho_high, n = n_big)

# 先ほどと同様の分析を実施
GGally::ggpairs(
  data = data_high_corr_big_number, 
  lower = list(continuous = wrap("density")))
plot_result(data_high_corr_big_number) # 信頼区間は小さくなり、estimateも真の値に近づいている

# vif の計算
data_high_corr_big_number %>% compute_vif() # しかし、vifは変わらないまま