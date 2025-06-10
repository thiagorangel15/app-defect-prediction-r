library(readxl)
library(psych)
library(ggplot2)
library(ggpubr)
library(PerformanceAnalytics)
library(nortest)
library(tidyr)

# carregando dados
dados <- read_excel("dataset_KC1_classlevel_numdefect.xlsx")

# visualizando dados
View(dados)
str(dados)
summary(dados)

# 2. Estatística Descritiva e Visualizações
# Tendência central e dispersão
describe(dados)

# Função para calcular moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Aplicar moda às colunas numéricas
sapply(dados, function(x) if(is.numeric(x)) moda(x) else NA)

# Histogramas com densidade
dados %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# Boxplots
dados %>%
  gather(variable, value) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "orange") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# Shapiro-Wilk
sapply(dados, function(x) if(is.numeric(x)) shapiro.test(x)$p.value else NA)

# 3. Análise de Correlação
# Matriz de correlação
chart.Correlation(dados[, sapply(dados, is.numeric)], histogram = TRUE, pch = 19)

# Variáveis mais correlacionadas com NUMDEFECTS
correlacoes <- cor(dados[, sapply(dados, is.numeric)])
correlacoes[,"NUMDEFECTS"] %>% sort(decreasing = TRUE)

# 4. Regressão Linear
# Scatterplots com regressão
ggscatter(dados, x = "sumLOC_TOTAL", y = "NUMDEFECTS", add = "reg.line", conf.int = TRUE)

# Regressão simples com NUMDEFECTS
modelo <- lm(NUMDEFECTS ~ sumLOC_TOTAL, data = dados)
summary(modelo)

# Diagnóstico
par(mfrow = c(2, 2))
plot(modelo)

