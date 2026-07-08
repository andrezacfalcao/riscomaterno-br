#fase 4 validação técnica (eda)
#dataset: dataset_final_brasil (saída da fase 3, 11.139 municípios-ano)
#pré-requisito: dataset_final_brasil já carregado no ambiente
#(ou rode: dataset_final_brasil <- readrds("dataset_final_brasil.rds"))
#o que esta fase faz:
#1. flag de instabilidade estatística da rmm (decisão: não destrutiva)
#2. estatísticas descritivas completas de todas as colunas
#3. checagem final de completude (na) na escala brasil
#4. exportação para .csv pronto para tableau
#limitação documentada (não corrigida nesta fase, ver nota abaixo):
#o piloto identificou peso mínimo de 100g em registros individuais do
#sinasc provável erro de digitação (ex: 1000g digitado como 100g).
#isso já está diluído dentro de pct_baixo_peso (proporção agregada por
#município-ano) e afeta poucos registros isoladamente. não reprocessamos
#a fase 2 para filtrar isso porque exigiria rodar as 27 ufs de novo por
#um ganho marginal registrar como limitação conhecida do dataset é
#prática aceita em data papers (seção "limitations").

library(dplyr)
library(ggplot2)

#torna este script autossuficiente (roda sozinho via rscript)
if (!exists("dataset_final_brasil")) {
  if (file.exists("dataset_final_BRASIL.rds")) {
    dataset_final_brasil <- readRDS("dataset_final_BRASIL.rds")
    cat("[OK] dataset_final_brasil carregado do disco.\n")
  } else {
    stop("dataset_final_BRASIL.rds não encontrado. Rode as fases anteriores primeiro.")
  }
}

#4.1 classificação de confiabilidade da rmm (3 níveis)
#substituímos a flag binária inicial por 3 níveis, porque a validação
#mostrou que o problema de instabilidade não tem corte limpo em 100
#nascidos vivos município com 118 nascidos vivos ainda mostrou rmm de
#2.542 (claramente ruído estatístico de amostra pequena, não risco real).
#limiares adotados:
#baixa : nascidos_vivos < 100 (rmm pode chegar a milhares, ruído extremo)
#média : 100-499 nascidos vivos (ainda instável, mas menos extremo)
#alta : >= 500 nascidos vivos (amostra grande o suficiente para
#refletir risco real, não apenas variação por acaso)

dataset_final_brasil <- dataset_final_brasil %>%
  mutate(
    CONFIABILIDADE_RMM = case_when(
      NASCIDOS_VIVOS < 100  ~ "Baixa",
      NASCIDOS_VIVOS < 500  ~ "Média",
      TRUE                  ~ "Alta"
    ),
    CONFIABILIDADE_RMM = factor(CONFIABILIDADE_RMM, levels = c("Baixa", "Média", "Alta"))
  )

#4.2 estatísticas descritivas completas

estatisticas_descritivas <- function(df) {
  colunas_numericas <- df %>% select(where(is.numeric)) %>% names()

  df %>%
    summarise(across(
      all_of(colunas_numericas),
      list(
        min    = ~min(.x, na.rm = TRUE),
        p25    = ~quantile(.x, 0.25, na.rm = TRUE),
        media  = ~mean(.x, na.rm = TRUE),
        mediana = ~median(.x, na.rm = TRUE),
        p75    = ~quantile(.x, 0.75, na.rm = TRUE),
        max    = ~max(.x, na.rm = TRUE),
        dp     = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )) %>%
    tidyr::pivot_longer(everything(), names_to = "campo", values_to = "valor") %>%
    tidyr::separate(campo, into = c("coluna", "estatistica"), sep = "__") %>%
    tidyr::pivot_wider(names_from = estatistica, values_from = valor)
}

resumo_estatistico <- estatisticas_descritivas(dataset_final_brasil)
print(resumo_estatistico, n = Inf)

#4.3 completude final (escala brasil)

completude_final <- dataset_final_brasil %>%
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
  tidyr::pivot_longer(everything(), names_to = "coluna", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\n== Completude final (dataset_final_brasil) ==\n")
print(completude_final, n = Inf)

#4.4 resumo da confiabilidade da rmm (para a seção de qualidade do artigo)

cat("\n== Distribuição por nível de confiabilidade ==\n")
resumo_confiabilidade <- dataset_final_brasil %>%
  group_by(CONFIABILIDADE_RMM) %>%
  summarise(
    n_municipios_ano = n(),
    pct_do_total = round(n() / nrow(dataset_final_brasil) * 100, 2),
    rmm_media = round(mean(RMM, na.rm = TRUE), 1),
    rmm_mediana = round(median(RMM, na.rm = TRUE), 1),
    rmm_maxima = round(max(RMM, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(resumo_confiabilidade)

#4.5 exportação para tableau (.csv)

write.csv(dataset_final_brasil, "dataset_final_BRASIL.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n[OK] Exportado: dataset_final_BRASIL.csv (", nrow(dataset_final_brasil), "linhas,",
    ncol(dataset_final_brasil), "colunas)\n")

saveRDS(dataset_final_brasil, "dataset_final_BRASIL.rds")
saveRDS(resumo_estatistico, "resumo_estatistico_BRASIL.rds")
saveRDS(completude_final, "completude_final_BRASIL.rds")

#4.6 visualização rmm por nível de confiabilidade
#a rmm tem muitos zeros (municípios sem óbito materno no período) e poucos
#valores extremos um histograma direto ficaria dominado pela barra de
#zero. por isso: (a) histograma excluindo rmm=0, pra ver a forma real da
#distribuição de quem teve óbito; (b) boxplot em escala log, pra comparar
#os 3 níveis lado a lado de forma justa.

library(ggplot2)

#(a) histograma por nível, excluindo zeros, eixo livre (escalas bem diferentes)
grafico_histograma <- dataset_final_brasil %>%
  filter(RMM > 0) %>%
  ggplot(aes(x = RMM, fill = CONFIABILIDADE_RMM)) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  facet_wrap(~CONFIABILIDADE_RMM, scales = "free", ncol = 1) +
  labs(
    title = "Distribuição da RMM por nível de confiabilidade (excluindo RMM = 0)",
    subtitle = "Note a escala do eixo X: 'Baixa' vai até milhares, 'Alta' fica abaixo de 1.200",
    x = "RMM (óbitos maternos por 100 mil nascidos vivos)",
    y = "Nº de municípios-ano"
  ) +
  theme_minimal()

print(grafico_histograma)
ggsave("rmm_histograma_por_confiabilidade.png", grafico_histograma, width = 8, height = 9, dpi = 150)

#(b) boxplot comparativo em escala log (permite ver os 3 lado a lado)
grafico_boxplot <- dataset_final_brasil %>%
  filter(RMM > 0) %>%
  ggplot(aes(x = CONFIABILIDADE_RMM, y = RMM, fill = CONFIABILIDADE_RMM)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  labs(
    title = "RMM por nível de confiabilidade (escala logarítmica)",
    x = "Nível de confiabilidade",
    y = "RMM (log10, óbitos maternos por 100 mil nascidos vivos)"
  ) +
  theme_minimal()

print(grafico_boxplot)
ggsave("rmm_boxplot_por_confiabilidade.png", grafico_boxplot, width = 7, height = 5, dpi = 150)

cat("\n[OK] Gráficos salvos: rmm_histograma_por_confiabilidade.png e rmm_boxplot_por_confiabilidade.png\n")
