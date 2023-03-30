#' shapiro_by_group - Teste de Shapiro-Wilk por grupo
#'
#' Esta função realiza o teste de Shapiro-Wilk em um conjunto de dados para cada grupo
#' definido por uma variável categórica especificada. A função retorna os resultados
#' de cada teste, incluindo o valor da estatística W, o valor-p e uma indicação se a
#' distribuição é normal ou não (com base em um nível de significância de 0,05).
#'
#' @param data Um objeto data.frame contendo as variáveis a serem testadas.
#' @param factor_var O nome da variável categórica que define os grupos.
#' @param num_var O nome da variável numérica a ser testada.
#'
#' @return Um objeto data.frame contendo os resultados do teste de Shapiro-Wilk para cada grupo.
#'
#' @examples
#' # criar um conjunto de dados simulado
#' set.seed(123)
#' notas_turma_a <- rnorm(30, mean = 7, sd = 1)
#' notas_turma_b <- rnorm(40, mean = 6.5, sd = 1.5)
#' turma <- rep(c("Turma A", "Turma B"), c(30, 40))
#' dados <- data.frame(Turma = turma, Notas = c(notas_turma_a, notas_turma_b))
#'
#' # chamar a função shapiro_by_group para testar a normalidade das notas de cada turma
#' resultados_shapiro <- shapiro_by_group(dados, "Turma", "Notas")
#' print(resultados_shapiro)
#'
#' @export

shapiro_by_group <- function(data, factor_var, num_var) {
  groups <- unique(data[, factor_var])
  results <- data.frame(Group = character(), W = numeric(), p.value = numeric(), Normal = character(), stringsAsFactors = FALSE)
  for (group in groups) {
    group_data <- data[data[, factor_var] == group, num_var]
    shapiro <- shapiro.test(group_data)
    is_normal <- ifelse(shapiro$p.value >= 0.05, "Sim", "Não")
    results <- rbind(results, data.frame(Group = group, W = shapiro$statistic, p.value = shapiro$p.value, Normal = is_normal))
  }
  return(results)
}

