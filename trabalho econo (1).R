# INSTALAR PACOTES 
install.packages(c("readxl", "lmtest", "sandwich", "car", "stargazer"))

library(readxl); library(lmtest); library(sandwich); library(car); library(stargazer)

# LER A BASE
# tenta ir para a pasta onde o script está salvo (se não conseguir, segue assim mesmo)
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)

# procura a planilha na pasta; se não achar, abre uma janela para você escolher
arquivo <- "economatica.xlsx"
if (!file.exists(arquivo)) arquivo <- file.choose()

base <- read_excel(arquivo, skip = 3)

names(base) <- c("vazio","nome","classe","bolsa","tipo","status","cod",
                 "ativo","dl_ebitda","margem","naics","lucro","acoes",
                 "divida","pl","receita","setor")

# transforma em número as colunas usadas ("-" vira NA)
base$ativo   <- suppressWarnings(as.numeric(base$ativo))
base$lucro   <- suppressWarnings(as.numeric(base$lucro))
base$divida  <- suppressWarnings(as.numeric(base$divida))
base$pl      <- suppressWarnings(as.numeric(base$pl))
base$receita <- suppressWarnings(as.numeric(base$receita))

# FILTRAR A BASE
dados <- base

# tira bancos e outros
dados <- subset(dados, !is.na(setor) & setor != "Financeiro" & setor != "Outros")

# tira quem tem dado faltando
dados <- dados[complete.cases(dados[, c("ativo","lucro","divida","pl","receita")]), ]

# tira receita ou PL zero/negativo
dados <- subset(dados, receita > 0 & pl > 0)

# deixa uma linha por empresa (tira classes ON/PN repetidas)
dados <- dados[!duplicated(dados[, c("ativo","pl","receita","lucro")]), ]

nrow(dados)   # deve dar 255

# CRIAR VARIÁVEIS
# função que corta os extremos nos percentis 1% e 99% (winsorizar)
wins <- function(x) {
  q <- quantile(x, c(0.01, 0.99))
  pmin(pmax(x, q[1]), q[2])
}

# versões brutas (sem winsorizar)
dados$m_raw <- dados$lucro / dados$receita * 100   # margem líquida em %
dados$a_raw <- dados$divida / dados$pl             # alavancagem

# versões winsorizadas (as principais)
dados$margem_liq <- wins(dados$m_raw)
dados$alav       <- wins(dados$a_raw)

# tamanho e alavancagem ao quadrado
dados$lativo <- log(dados$ativo)
dados$alav2  <- dados$alav^2

# setor como categoria, com "Bens industriais" de base
dados$setor <- relevel(factor(dados$setor), ref = "Bens industriais")

# DESCRITIVAS
summary(dados[, c("margem_liq", "alav", "lativo")])
table(dados$setor)
cor(dados[, c("margem_liq", "alav", "lativo")])

plot(dados$alav, dados$margem_liq,
     xlab = "Alavancagem (Dívida/PL)", ylab = "Margem líquida (%)")
abline(lm(margem_liq ~ alav, data = dados), col = "red")

# 5 REGRESSÕES
m1 <- lm(margem_liq ~ alav, data = dados)
m2 <- lm(margem_liq ~ alav + lativo, data = dados)
m3 <- lm(margem_liq ~ alav + lativo + setor, data = dados)           # principal
m4 <- lm(m_raw ~ a_raw + lativo + setor, data = dados)               # sem winsorizar
m5 <- lm(margem_liq ~ alav + alav2 + lativo + setor, data = dados)   # não linear

modelos <- list(m1, m2, m3, m4, m5)

# RESULTADOS COM ERRO PADRÃO ROBUSTO
coeftest(m1, vcov. = vcovHC(m1, type = "HC1"))
coeftest(m2, vcov. = vcovHC(m2, type = "HC1"))
coeftest(m3, vcov. = vcovHC(m3, type = "HC1"))   # o mais importante
coeftest(m4, vcov. = vcovHC(m4, type = "HC1"))
coeftest(m5, vcov. = vcovHC(m5, type = "HC1"))

sapply(modelos, function(m) summary(m)$adj.r.squared)   # R² ajustado

# TESTES
# Breusch-Pagan nos 5 modelos (p pequeno = há heterocedasticidade)
for (m in modelos) print(bptest(m))

# F robusto nos 5 modelos (o modelo no conjunto é significativo?)
for (m in modelos) print(waldtest(m, vcov = vcovHC(m, type = "HC1")))

# VIF do modelo principal (colinearidade): olhe as linhas alav e lativo (deve ser < 5)
vif(m3)

# F conjunto dos setores (setor importa?)
nomes_setor <- names(coef(m3))[grepl("setor", names(coef(m3)))]
linearHypothesis(m3, nomes_setor, vcov. = vcovHC(m3, type = "HC1"))

# TABELA FINAL
ep <- lapply(modelos, function(m) sqrt(diag(vcovHC(m, type = "HC1"))))

stargazer(modelos, se = ep, type = "text", omit = "setor",
          omit.stat = c("f", "ser"))