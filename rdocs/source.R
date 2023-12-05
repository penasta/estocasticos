if (!require("pacman")) install.packages("pacman")
# Pacotes ----
pacman::p_load(tidyverse,data.table,scales,lubridate,geobr,
               abjutils,ggrepel,xlsx,readr,sf,car,lmtest,lubridate)

# Removendo notação científica ----
options(scipen = 999)

# Definindo fonte; Arial ----
windowsFonts(Arial=windowsFont("sans"))

# Definindo paleta de cores ----
cores <- c("#006eab", "#00a8e7","#ffffff", "#000000")

# Definindo função que retorna frequências relativas de um vetor ----
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com frequências relativas e absolutas de uma variável categórica ----
vector_frequencies <- function(vector) {
  frequency <- vector %>%
    table() %>%
    as_tibble() %>%
    mutate(
      rel = n %>%
        percent() %>%
        paste("%", sep = "")
    )
  colnames(frequency) <- c("groups", "absolute", "relative")
  return(frequency)
}

# Operador %notin% (faz o oposto do %in%)

`%notin%` <- Negate(`%in%`)

# seed (minha matrícula rs)

seed <- 150167636
