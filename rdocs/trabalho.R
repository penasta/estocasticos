# Pacotes, funções e afins ----
source("rdocs/source.R")

# Dados ----
decisoes <- readRDS("banco/decisoes.rds")
# Fonte dos dados: Corte aberta STF
# Link: https://transparencia.stf.jus.br/extensions/decisoes/decisoes.html
# Data de download: Sexta-feira 10 de novembro de 2023.

# 1.0 Análises ----

# 1.1.1 Boxplot de decisões por mês ----
decisoes %>%
#  filter(mes != "jan") %>%
#  filter(mes != "jul") %>%
  ggplot() +
  aes(
    x = mes,
    y = n
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# 1.1.2 Boxplot da quantidade por mês SIMULADO Poisson ----

decisoes$nsim <- rpois(length(decisoes$n), lambda = mean(decisoes$n))
decisoes %>%
#  filter(mes != "jan") %>%
#  filter(mes != "jul") %>%
  ggplot() +
  aes(
    x = mes,
    y = nsim
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# 1.2.1 Boxplot de decisões por ano ----
decisoes %>%
  ggplot() +
  aes(
    x = factor(ano),
    y = n
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# 1.2.2 Boxplot da quantidade por ano SIMULADO Poisson ----

decisoes %>%
  ggplot() +
  aes(
    x = factor(ano),
    y = nsim
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# 1.3.1 Boxplot da quantidade por juiz ----

decisoes %>%
  #  filter(nome != "Juiz 2") %>%
  #  filter(nome != "Juiz 4") %>%
  #  filter(nome != "Juiz 10") %>%
  #  filter(nome != "Juiz 11") %>%
  ggplot() +
  aes(
    x = nome,
    y = n
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# 1.3.2 Boxplot da quantidade por juiz SIMULADO Poisson ----

ggplot(decisoes) +
  aes(
    x = nome,
    y = nsim
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# 1.4 Analisando fontes de variação nos dados ----

summary(aov(n ~ nome + ano + mes + classe,data = decisoes))
# Nota-se que a maior fonte de variação é a classe, que não pertence ao escopo
# desta análise. Por conveniência, irei isolá-la

#  1.5 Boxplot da quantidade por classe ----

ggplot(decisoes) +
  aes(
    x = classe,
    y = n
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# Irei trabalhar apenas com a classe HC

HC <- decisoes |>
  filter(classe %in% c('HC'))

# 1.5.1 Boxplot da quantidade de HC ----

ggplot(HC) +
  aes(
    x = classe,
    y = n
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# Analisando se as fontes de variação desejadas continuam presentes

anova <- aov(n ~ nome * ano * mes,data = HC)
summary(anova) # Aparentemente, as fontes de variação postuladas no relatório se mantém.

# Pressupostos ----
# Independência ----
plot(anova$residuals)
# Normalidade ----
shapiro.test(anova$residuals)
qqnorm(anova$residuals)
qqline(anova$residuals)
# Homocedasticidade
leveneTest(n ~ factor(nome):factor(ano):factor(mes),data = HC)
dados <- data.frame(1:1068)
dados$residuos <- anova$residuals
dados$valorajustado <- anova$fitted.values
ggplot(data = dados, mapping = aes(x = valorajustado, y = residuos)) +
  geom_point(colour=cores[1], size=1) +
  scale_x_continuous() +
  labs(x="Valor Ajustado",
       y="Resíduos", title = "Homocedasticidade") +
  theme_minimal()
rm(anova,dados)

# Modelando a função de intensidade

datas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")
indices <- yday(datas)
dias_da_semana <- lubridate::wday(datas, label = T, abbr = T)
df <- data.frame(Data = datas, Indice = indices, Dia_da_Semana = dias_da_semana)
df <- df |>
  mutate(dia_util = ifelse(Dia_da_Semana %in% c("sáb","dom"),"não","sim"))
# Adicionando feriados manualmente pelo índice do dia ----
dias_feriados <- c(1:6,51:53,95:97,111,121,159,223,250,285,305:306,319,342,354:365)
df <- df |>
  mutate(dia_util = ifelse(Indice %in% dias_feriados,"não",dia_util))

# Exportando vetor de dias não úteis para colocar no relatório
# indices <- df %>%
#   filter(dia_util == "não") %>%
#   select(Indice) %>% pull()
#write.table(t(indices),"indices.txt",col.names = F, row.names = F,sep = ", ")

df$mes <- lubridate::month(df$Data, label = T, abbr = T)
df$ndias <- days_in_month(df$Data)

# 'Função de intensidade' aproximada por mês
# Para testes, iremos considerar simplesmente \Lambda_i = 0 para dia não útil, e = 1 para dia útil (t*1 e t*0 portanto).

v = df |>
  select(dia_util,mes,ndias) |>
  mutate(dia_util = ifelse(dia_util == 'não',0,1)) |>
  group_by(mes) |>
  summarise(sum(dia_util/ndias))

# Proposta para uma nova rodada: Calcular a média dos dias úteis e dos dias não úteis. Considerar a função de intensidade de dias não-uteis como 1 (t*1) e a dos dias úteis como (E[dias úteis]/E[dias não-úteis])

HC = left_join(HC,v,by='mes')
rm(v,df)
colnames(HC)[7] <- 'fi'

juiz = "Juiz 1"
HC %>%
  filter(ano == 2023,
         nome == juiz,
         mes != "nov") %>%
  ungroup() %>%
  select(n,fi) %>%
  mutate(lambda = n/fi) %>%
  select(lambda) %>%
  pull() %>%
  ks.test(., "ppois",lambda=mean(.))

HC %>%
  filter(ano == 2023,
         nome == juiz,
         mes != "nov") %>%
  ungroup() %>%
  select(n,fi) %>%
  mutate(lambda = n/fi) %>%
  select(lambda) %>%
  mutate(nsim = rpois(n=length(lambda),lambda=mean(lambda))) %>%
  reshape2::melt() %>%
  mutate(variable = ifelse(variable == "lambda","Valor real","Valor simulado")) %>%
  ggplot() +
  aes(
    x = variable,
    y = value
  ) +
  geom_boxplot(fill = c("#006eab"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#ffffff"
  ) +
  labs(x = "", y = "") +
  theme_minimal()

# Estimando total e comparando com o real

# Real

HC |>
  filter(ano == 2023,
         mes %notin% c('nov','dez')) |>
  ungroup() |> select(n) |> summary()

HC |>
  filter(ano == 2023,
         mes %notin% c('nov','dez')) |>
  ungroup() |> select(n) |> sum()

# Estimado

#juiz = "Juiz 1"
HC |>
  filter(ano == 2023,
#         nome == juiz,
         mes %notin% c('nov','dez')) |>
  select(!nsim) |>
  mutate(nsim = rpois(n=1,lambda=n*fi)) |>
  ungroup() |>
  select(nsim) |>
  summary()

HC |>
  filter(ano == 2023,mes %notin% c('nov','dez')) |>
  select(!nsim) |>
  ungroup() |>
  group_by(nome) |>
  mutate(lambdai = mean(n)) |>
  mutate(nsim = (rpois(n=1,lambda=lambdai))*fi) |>
  ungroup() |>
  select(nsim) |>
  sum()
