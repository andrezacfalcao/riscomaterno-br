# =============================================================================
# PIPELINE COMPLETO E DEFINITIVO
# Dataset de Risco Socioespacial e Desfechos Materno-Infantis (SINASC + SIM)
# DSW/SBBD 2025
# =============================================================================
#
# ESTRUTURA DESTE ARQUIVO:
#   PARTE 0 - Setup
#   PARTE 1 - Funções reutilizáveis (Fase 2 + Fase 3)
#   PARTE 2 - Bloco Piloto PE           [HISTÓRICO/COMENTADO -- já validado]
#   PARTE 3 - Bloco Regional Nordeste   [HISTÓRICO/COMENTADO -- já validado]
#   PARTE 4 - Bloco Oficial Brasil (27 UFs)                    [ATIVO -- RODAR]
#   PARTE 5 - Fase 4: Validação Técnica (EDA)                  [ATIVO -- RODAR]
#   PARTE 6 - Fase 5: Carga e Publicação          [AINDA NÃO IMPLEMENTADA]
#
# COMO USAR AGORA:
#   1) Rode PARTE 0 e PARTE 1 (setup + funções) -- sempre primeiro.
#   2) NÃO precisa rodar PARTE 2 nem PARTE 3 -- estão comentadas, servem só
#      de registro histórico de como validamos o pipeline em etapas
#      (PE -> Nordeste -> Brasil) antes de rodar em escala nacional direta.
#   3) Apague os checkpoints antigos (a função de agregação mudou -- ver
#      instrução no início da PARTE 4) e rode a PARTE 4 (Brasil completo).
#   4) Rode a PARTE 5 (Fase 4: EDA) em seguida, usando o dataset_final_brasil
#      gerado na PARTE 4.
#
# =============================================================================
# HISTÓRICO DE ACHADOS DE VALIDAÇÃO (documentar no artigo, seção Qualidade)
# =============================================================================
#   1) CONSULTAS usa rótulos "1 a 3 vezes" / "4 a 6 vezes" / "7 ou mais vezes"
#      / "Nenhuma" (não "de 1 a 3 consultas", como assumido inicialmente).
#
#   2) OBITOPARTO NÃO é campo de óbito materno -- pertence à investigação de
#      óbito FETAL/INFANTIL (timing da morte do bebê em relação ao parto).
#      Incluí-lo por engano numa versão anterior inflou o resultado de forma
#      incorreta (150 -> 1496 óbitos maternos). Corrigido: só OBITOGRAV e
#      OBITOPUERP são válidos para a definição ampliada de óbito materno.
#        OBITOGRAV:  "Sim"/"Não" (binário normal)
#        OBITOPUERP: "De 0 a 42 dias"/"De 43 dias a 1 ano"/"Não"/NA
#                     (as duas primeiras categorias = afirmativo)
#
#   3) process_sinasc() zera 100% do campo PESO (bug do pacote microdatasus).
#      Corrigido restaurando o valor a partir do dado bruto (corrigir_peso()).
#
#   4) Códigos de município terminados em "0000" (ex: "260000" em PE)
#      representam "município ignorado", não uma cidade real -- removidos
#      via remover_municipio_placeholder().
#
#   5) Download de UFs grandes (SP) pode estourar o timeout padrão do R
#      (240s) por causa do tamanho do arquivo (~24MB). Corrigido com
#      options(timeout = 600) na Parte 0.
#
#   6) RMM (Razão de Mortalidade Materna) é estatisticamente instável em
#      municípios com poucos nascidos vivos -- um só óbito num município
#      pequeno gera RMM de milhares. Não existe corte limpo (município com
#      118 nascidos vivos ainda mostrou RMM de 2.542). Tratamento adotado:
#      classificação em 3 níveis de confiabilidade (não destrutiva -- ver
#      CONFIABILIDADE_RMM na Fase 4), em vez de uma flag binária ou de
#      remover/suprimir os dados.
#
#   7) PESO mínimo observado de 100g em registros individuais do SINASC --
#      abaixo do limite de viabilidade neonatal reconhecido (~245g é o
#      recorde mundial de sobrevivência), quase certamente erro de
#      digitação no preenchimento original da Declaração de Nascido Vivo.
#      NÃO CORRIGIMOS O VALOR (o dado original do SUS nunca é alterado) --
#      apenas sinalizamos com a flag TEM_PESO_IMPLAUSIVEL (< 300g), calculada
#      durante a agregação por município-ano.
# =============================================================================


# #############################################################################
# PARTE 0 - SETUP
# #############################################################################

library(microdatasus)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)

# Timeout maior para evitar falha de download em UFs grandes (achado 5)
options(timeout = 600)


# #############################################################################
# PARTE 1 - FUNÇÕES REUTILIZÁVEIS (FASE 2 + FASE 3)
# Nenhuma função aqui referencia UF nem ano específicos -- funcionam iguais
# para qualquer escala (piloto, regional ou nacional).
# #############################################################################

# ---- FASE 2: Transformação e Limpeza ---------------------------------------

# 1.1 Corrige o bug conhecido do PESO no process_sinasc() (achado 3)
corrigir_peso <- function(dados_sinasc_processado, dados_sinasc_brutos) {
  stopifnot(nrow(dados_sinasc_brutos) == nrow(dados_sinasc_processado))
  dados_sinasc_processado$PESO <- as.numeric(dados_sinasc_brutos$PESO)
  dados_sinasc_processado
}

# 1.2 Padronização de tipos e datas
padronizar_sinasc <- function(df) {
  df %>%
    mutate(
      CODMUNRES = as.character(CODMUNRES),
      COD_UF    = str_sub(CODMUNRES, 1, 2),
      DTNASC    = as.Date(DTNASC),
      ANO_NASC  = year(DTNASC),
      IDADEMAE  = suppressWarnings(as.numeric(as.character(IDADEMAE))),
      PESO      = suppressWarnings(as.numeric(as.character(PESO)))
    )
}

padronizar_sim <- function(df) {
  df %>%
    mutate(
      CODMUNRES = as.character(CODMUNRES),
      COD_UF    = str_sub(CODMUNRES, 1, 2),
      DTOBITO   = as.Date(DTOBITO),
      ANO_OBITO = year(DTOBITO),
      IDADE     = suppressWarnings(as.numeric(as.character(IDADE))),
      CAUSABAS  = as.character(CAUSABAS)
    )
}

# 1.3 Remoção de códigos de município placeholder "ignorado" (achado 4)
remover_municipio_placeholder <- function(df) {
  df %>% filter(!str_detect(CODMUNRES, "0000$"))
}

# 1.4 Tratamento de valores ignorados (9, 99, "Ignorado") -> NA
tratar_ignorados <- function(df, colunas) {
  valores_ignorados <- c("Ignorado", "Ignorada", "9", "99", "999", "Não Informado", "")
  df %>%
    mutate(across(
      all_of(colunas),
      ~ {
        x <- as.character(.x)
        x[x %in% valores_ignorados] <- NA
        x
      }
    ))
}

# 1.5 Identificação de óbito materno -- definição dupla: CID + ampliada
# (achado 2: OBITOPARTO excluído de propósito)
identificar_obito_materno <- function(df) {
  df %>%
    mutate(
      OBITO_MATERNO_CID = str_detect(CAUSABAS, "^O\\d{2}"),
      FLAG_DECLARADA = (
        OBITOGRAV == "Sim" |
        OBITOPUERP %in% c("De 0 a 42 dias", "De 43 dias a 1 ano")
      ),
      OBITO_MATERNO_AMPLIADO = if_else(
        SEXO == "Feminino" & (OBITO_MATERNO_CID | coalesce(FLAG_DECLARADA, FALSE)),
        TRUE, FALSE
      )
    )
}

# 1.6 Agregação por município-ano
# TEM_PESO_IMPLAUSIVEL (achado 7): sinaliza município-ano com pelo menos 1
# registro de peso ao nascer abaixo de 300g -- território de erro de
# digitação, não de bebê extremamente prematuro real (que tipicamente fica
# entre 500-999g). Não removemos nem corrigimos o valor original -- só
# sinalizamos, mesmo princípio não destrutivo usado na CONFIABILIDADE_RMM.
agregar_sinasc <- function(dados_sinasc) {
  dados_sinasc %>%
    group_by(COD_UF, CODMUNRES, ANO_NASC) %>%
    summarise(
      NASCIDOS_VIVOS        = n(),
      PCT_PARTO_CESAREO     = mean(PARTO == "Cesáreo", na.rm = TRUE) * 100,
      PCT_SEM_PRENATAL      = mean(CONSULTAS == "Nenhuma", na.rm = TRUE) * 100,
      PCT_MENOS_4_CONSULTAS = mean(CONSULTAS %in% c("Nenhuma", "1 a 3 vezes"), na.rm = TRUE) * 100,
      PCT_MAE_ADOLESCENTE   = mean(IDADEMAE < 20, na.rm = TRUE) * 100,
      PCT_BAIXO_PESO        = mean(PESO < 2500, na.rm = TRUE) * 100,
      IDADE_MEDIA_MAE       = mean(IDADEMAE, na.rm = TRUE),
      TEM_PESO_IMPLAUSIVEL  = any(PESO < 300, na.rm = TRUE),
      .groups = "drop"
    )
}

agregar_sim <- function(dados_sim) {
  dados_sim %>%
    group_by(COD_UF, CODMUNRES, ANO_OBITO) %>%
    summarise(
      OBITOS_MATERNOS_CID      = sum(OBITO_MATERNO_CID, na.rm = TRUE),
      OBITOS_MATERNOS_AMPLIADO = sum(OBITO_MATERNO_AMPLIADO, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(ANO_NASC = ANO_OBITO)
}

# 1.7 Relatório de completude (insumo direto pra seção "Qualidade")
relatorio_completude <- function(df, nome_base) {
  df %>%
    summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
    pivot_longer(everything(), names_to = "coluna", values_to = "pct_na") %>%
    mutate(base = nome_base) %>%
    arrange(desc(pct_na))
}

# 1.8 Checklist de validação automática -- Fase 2
validar_fase2 <- function(dados_sinasc, dados_sim, sinasc_agregado, sim_agregado) {
  cat("=====================================================\n")
  cat("        CHECKLIST DE VALIDAÇÃO - FASE 2\n")
  cat("=====================================================\n\n")

  cat("[1] Integridade de linhas\n")
  cat("    Registros SINASC (processado):", nrow(dados_sinasc), "\n")
  cat("    Registros SIM (processado):   ", nrow(dados_sim), "\n\n")

  cat("[2] PESO ao nascer (deve ter mediana ~2500-3500g, poucos NA)\n")
  print(summary(dados_sinasc$PESO))
  pct_na_peso <- round(mean(is.na(dados_sinasc$PESO)) * 100, 2)
  cat("    % NA em PESO:", pct_na_peso, "%",
      if (pct_na_peso > 5) " <-- ATENÇÃO: acima do esperado" else " -- OK", "\n\n")

  cat("[3] Níveis categóricos-chave (checar se bate com o esperado)\n")
  cat("    PARTO:\n"); print(table(dados_sinasc$PARTO, useNA = "always"))
  cat("    CONSULTAS:\n"); print(table(dados_sinasc$CONSULTAS, useNA = "always"))
  cat("\n")

  cat("[4] Agregação municipal\n")
  cat("    Municípios-ano no SINASC agregado:", nrow(sinasc_agregado), "\n")
  cat("    Municípios-ano no SIM agregado:   ", nrow(sim_agregado), "\n")
  n_placeholder <- sum(str_detect(sinasc_agregado$CODMUNRES, "0000$"))
  cat("    Códigos placeholder remanescentes (deveria ser 0):", n_placeholder, "\n\n")

  cat("[5] Óbitos maternos identificados\n")
  total_cid <- sum(sim_agregado$OBITOS_MATERNOS_CID)
  total_ampliado <- sum(sim_agregado$OBITOS_MATERNOS_AMPLIADO)
  cat("    Definição CID estrita: ", total_cid, "\n")
  cat("    Definição ampliada:    ", total_ampliado, "\n")
  cat("    Diferença (subnotificação capturada):", total_ampliado - total_cid, "\n")
  if (total_cid == 0) cat("    <-- ATENÇÃO: zero óbitos maternos é suspeito, investigar CAUSABAS\n")
  if (total_cid > 0 && total_ampliado > total_cid * 3) {
    cat("    <-- ATENÇÃO: definição ampliada é mais de 3x a estrita, investigar lógica.\n")
  }
  cat("\n")

  cat("[6] Faixas de valores (sanity check de outliers)\n")
  cat("    PESO mínimo:", min(dados_sinasc$PESO, na.rm = TRUE),
      "| máximo:", max(dados_sinasc$PESO, na.rm = TRUE), "\n")
  cat("    IDADEMAE mínima:", min(dados_sinasc$IDADEMAE, na.rm = TRUE),
      "| máxima:", max(dados_sinasc$IDADEMAE, na.rm = TRUE), "\n\n")

  cat("[7] Município-ano com peso implausível (< 300g em algum registro)\n")
  n_implausivel <- sum(sinasc_agregado$TEM_PESO_IMPLAUSIVEL)
  cat("    Total:", n_implausivel, "de", nrow(sinasc_agregado),
      sprintf("(%.2f%%)\n", 100 * n_implausivel / nrow(sinasc_agregado)))
  cat("    (não corrigido -- dado original do SUS preservado, apenas sinalizado)\n\n")

  cat("=====================================================\n")
  cat("Copie este bloco inteiro e cole na conversa para eu revisar.\n")
  cat("=====================================================\n")
}

# ---- FASE 3: Integração (Join) ---------------------------------------------

# 1.9 Join SINASC + SIM por município-ano e cálculo da RMM
integrar_sinasc_sim <- function(sinasc_agregado, sim_agregado) {
  sinasc_agregado %>%
    left_join(
      sim_agregado,
      by = c("COD_UF", "CODMUNRES", "ANO_NASC")
    ) %>%
    mutate(
      OBITOS_MATERNOS_CID      = coalesce(OBITOS_MATERNOS_CID, 0L),
      OBITOS_MATERNOS_AMPLIADO = coalesce(OBITOS_MATERNOS_AMPLIADO, 0L),
      RMM     = (OBITOS_MATERNOS_AMPLIADO / NASCIDOS_VIVOS) * 100000,
      RMM_CID = (OBITOS_MATERNOS_CID / NASCIDOS_VIVOS) * 100000
    ) %>%
    arrange(CODMUNRES, ANO_NASC)
}

# 1.10 Checklist de validação automática -- Fase 3
validar_fase3 <- function(dataset_final, sinasc_agregado) {
  cat("=====================================================\n")
  cat("        CHECKLIST DE VALIDAÇÃO - FASE 3\n")
  cat("=====================================================\n\n")

  cat("[1] Contagem de linhas\n")
  cat("    Linhas no SINASC agregado:", nrow(sinasc_agregado), "\n")
  cat("    Linhas no dataset final:  ", nrow(dataset_final), "\n")
  if (nrow(dataset_final) != nrow(sinasc_agregado)) {
    cat("    <-- ATENÇÃO: contagem mudou no join. Investigar duplicatas de chave.\n")
  } else {
    cat("    -- OK, contagem preservada (join 1:1 confirmado)\n")
  }
  cat("\n")

  cat("[2] Municípios sem nenhum óbito materno registrado no período\n")
  n_zero <- sum(dataset_final$OBITOS_MATERNOS_AMPLIADO == 0)
  cat("    Município-ano com zero óbitos maternos:", n_zero, "de", nrow(dataset_final),
      sprintf("(%.1f%%)\n", 100 * n_zero / nrow(dataset_final)))
  cat("\n")

  cat("[3] Distribuição da RMM (Razão de Mortalidade Materna por 100mil)\n")
  print(summary(dataset_final$RMM))
  n_inf <- sum(is.infinite(dataset_final$RMM))
  if (n_inf > 0) {
    cat("    <-- ATENÇÃO:", n_inf, "valores infinitos (município com 0 nascidos vivos).\n")
  }
  cat("\n")

  cat("[4] Municípios com RMM potencialmente instável (denominador pequeno)\n")
  suspeitos <- dataset_final %>%
    filter(NASCIDOS_VIVOS < 100, OBITOS_MATERNOS_AMPLIADO > 0) %>%
    arrange(desc(RMM))
  cat("    Município-ano com <100 nascidos vivos E ao menos 1 óbito materno:", nrow(suspeitos), "\n")
  cat("    (tratamento detalhado em 3 níveis -- ver Fase 4, CONFIABILIDADE_RMM)\n")
  if (nrow(suspeitos) > 0) {
    print(head(suspeitos %>% select(CODMUNRES, ANO_NASC, NASCIDOS_VIVOS, OBITOS_MATERNOS_AMPLIADO, RMM), 10))
  }
  cat("\n")

  cat("[5] Amostra do dataset final\n")
  print(head(dataset_final, 5))

  cat("\n=====================================================\n")
  cat("Copie este bloco inteiro e cole na conversa para eu revisar.\n")
  cat("=====================================================\n")
}


# #############################################################################
# PARTE 2 - BLOCO PILOTO PE (2022-2023)
# =============================================================================
# HISTÓRICO -- JÁ VALIDADO E SUPERADO. Mantido comentado só como registro de
# reprodutibilidade (mostra como o pipeline foi testado numa escala pequena
# antes de escalar). NÃO PRECISA RODAR -- os dados de PE já estão incluídos
# no bloco oficial Brasil (Parte 4).
# =============================================================================

# uf_piloto <- "PE"
# ano_inicio_piloto <- 2022
# ano_fim_piloto <- 2023
#
# sinasc_brutos_pe <- fetch_datasus(year_start = ano_inicio_piloto, year_end = ano_fim_piloto,
#                                    uf = uf_piloto, information_system = "SINASC")
# sim_brutos_pe <- fetch_datasus(year_start = ano_inicio_piloto, year_end = ano_fim_piloto,
#                                 uf = uf_piloto, information_system = "SIM-DO")
#
# sinasc_pe <- process_sinasc(sinasc_brutos_pe)
# sinasc_pe <- corrigir_peso(sinasc_pe, sinasc_brutos_pe)
# sinasc_pe <- padronizar_sinasc(sinasc_pe)
# sinasc_pe <- remover_municipio_placeholder(sinasc_pe)
# sinasc_pe <- tratar_ignorados(sinasc_pe, intersect(
#   c("PARTO", "CONSULTAS", "GESTACAO", "GRAVIDEZ", "ESCMAE", "RACACOR", "SEXO", "LOCNASC"),
#   names(sinasc_pe)
# ))
# sinasc_pe <- sinasc_pe %>% mutate(IDADEMAE = if_else(IDADEMAE < 9 | IDADEMAE > 60, NA_real_, IDADEMAE))
#
# sim_pe <- process_sim(sim_brutos_pe)
# sim_pe <- padronizar_sim(sim_pe)
# sim_pe <- remover_municipio_placeholder(sim_pe)
# sim_pe <- tratar_ignorados(sim_pe, intersect(
#   c("RACACOR", "ESC", "ASSISTMED", "CIRCOBITO", "SEXO"),
#   names(sim_pe)
# ))
# sim_pe <- identificar_obito_materno(sim_pe)
#
# sinasc_municipio_pe <- agregar_sinasc(sinasc_pe)
# sim_municipio_pe <- agregar_sim(sim_pe)
#
# validar_fase2(sinasc_pe, sim_pe, sinasc_municipio_pe, sim_municipio_pe)
#
# dataset_final_pe <- integrar_sinasc_sim(sinasc_municipio_pe, sim_municipio_pe)
# validar_fase3(dataset_final_pe, sinasc_municipio_pe)
#
# saveRDS(dataset_final_pe, "dataset_final_PE.rds")


# #############################################################################
# PARTE 3 - BLOCO REGIONAL NORDESTE (9 UFs)
# =============================================================================
# HISTÓRICO -- JÁ VALIDADO E SUPERADO. Foi a etapa intermediária entre o
# piloto PE e a rodada nacional completa (validou o pipeline em 9 estados
# diversos, com contagem de municípios batendo 100% com o IBGE). Mantido
# comentado só como registro. NÃO PRECISA RODAR -- os dados do Nordeste já
# estão incluídos no bloco oficial Brasil (Parte 4).
# =============================================================================

# ufs_nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
#
# dir.create("checkpoints_nordeste", showWarnings = FALSE)
#
# for (uf_atual in ufs_nordeste) {
#   arquivo_sinasc <- file.path("checkpoints_nordeste", paste0("sinasc_", uf_atual, ".rds"))
#   arquivo_sim    <- file.path("checkpoints_nordeste", paste0("sim_", uf_atual, ".rds"))
#
#   if (file.exists(arquivo_sinasc) && file.exists(arquivo_sim)) next
#
#   tryCatch({
#     sb <- fetch_datasus(year_start = 2022, year_end = 2023, uf = uf_atual, information_system = "SINASC")
#     s  <- process_sinasc(sb)
#     s  <- corrigir_peso(s, sb)
#     s  <- padronizar_sinasc(s)
#     s  <- remover_municipio_placeholder(s)
#     s  <- tratar_ignorados(s, intersect(
#            c("PARTO","CONSULTAS","GESTACAO","GRAVIDEZ","ESCMAE","RACACOR","SEXO","LOCNASC"),
#            names(s)))
#     s  <- s %>% mutate(IDADEMAE = if_else(IDADEMAE < 9 | IDADEMAE > 60, NA_real_, IDADEMAE))
#     s_agregado <- agregar_sinasc(s)
#
#     mb <- fetch_datasus(year_start = 2022, year_end = 2023, uf = uf_atual, information_system = "SIM-DO")
#     m  <- process_sim(mb)
#     m  <- padronizar_sim(m)
#     m  <- remover_municipio_placeholder(m)
#     m  <- tratar_ignorados(m, intersect(
#            c("RACACOR","ESC","ASSISTMED","CIRCOBITO","SEXO"), names(m)))
#     m  <- identificar_obito_materno(m)
#     m_agregado <- agregar_sim(m)
#
#     saveRDS(s_agregado, arquivo_sinasc)
#     saveRDS(m_agregado, arquivo_sim)
#   }, error = function(e) cat("ERRO na UF", uf_atual, ":", conditionMessage(e), "\n"))
# }
#
# arquivos_sinasc_ne <- list.files("checkpoints_nordeste", pattern = "^sinasc_.*\\.rds$", full.names = TRUE)
# arquivos_sim_ne    <- list.files("checkpoints_nordeste", pattern = "^sim_.*\\.rds$", full.names = TRUE)
# sinasc_municipio_nordeste <- bind_rows(lapply(arquivos_sinasc_ne, readRDS))
# sim_municipio_nordeste    <- bind_rows(lapply(arquivos_sim_ne, readRDS))
# dataset_final_nordeste <- integrar_sinasc_sim(sinasc_municipio_nordeste, sim_municipio_nordeste)
# validar_fase3(dataset_final_nordeste, sinasc_municipio_nordeste)
# saveRDS(dataset_final_nordeste, "dataset_final_NORDESTE.rds")


# #############################################################################
# PARTE 4 - BLOCO OFICIAL BRASIL (27 UFs) -- ATIVO, RODAR AGORA
# =============================================================================
# IMPORTANTE: a função agregar_sinasc() mudou (ganhou TEM_PESO_IMPLAUSIVEL),
# então checkpoints antigos estão desatualizados. Rode isto ANTES do loop:
#
#   unlink("checkpoints_nordeste", recursive = TRUE)
#   unlink("checkpoints_brasil", recursive = TRUE)
#
# Desenho: loop UF por UF com checkpoint individual em disco (retomável em
# caso de queda de conexão -- se cair no meio, rode o loop de novo e ele
# pula as UFs já processadas).
# #############################################################################

ufs_brasil <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS",
                "MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC",
                "SP","SE","TO")

dir.create("checkpoints_brasil", showWarnings = FALSE)

for (uf_atual in ufs_brasil) {
  arquivo_sinasc <- file.path("checkpoints_brasil", paste0("sinasc_", uf_atual, ".rds"))
  arquivo_sim    <- file.path("checkpoints_brasil", paste0("sim_", uf_atual, ".rds"))

  if (file.exists(arquivo_sinasc) && file.exists(arquivo_sim)) {
    cat("UF", uf_atual, "já processada, pulando.\n")
    next
  }

  resultado <- tryCatch({
    cat("=== Processando UF:", uf_atual, "===\n")

    cat("  Baixando SINASC...\n")
    sb <- fetch_datasus(year_start = 2022, year_end = 2023, uf = uf_atual, information_system = "SINASC")
    s  <- process_sinasc(sb)
    s  <- corrigir_peso(s, sb)
    s  <- padronizar_sinasc(s)
    s  <- remover_municipio_placeholder(s)
    s  <- tratar_ignorados(s, intersect(
           c("PARTO","CONSULTAS","GESTACAO","GRAVIDEZ","ESCMAE","RACACOR","SEXO","LOCNASC"),
           names(s)))
    s  <- s %>% mutate(IDADEMAE = if_else(IDADEMAE < 9 | IDADEMAE > 60, NA_real_, IDADEMAE))
    s_agregado <- agregar_sinasc(s)

    cat("  Baixando SIM...\n")
    mb <- fetch_datasus(year_start = 2022, year_end = 2023, uf = uf_atual, information_system = "SIM-DO")
    m  <- process_sim(mb)
    m  <- padronizar_sim(m)
    m  <- remover_municipio_placeholder(m)
    m  <- tratar_ignorados(m, intersect(
           c("RACACOR","ESC","ASSISTMED","CIRCOBITO","SEXO"), names(m)))
    m  <- identificar_obito_materno(m)
    m_agregado <- agregar_sim(m)

    saveRDS(s_agregado, arquivo_sinasc)
    saveRDS(m_agregado, arquivo_sim)
    cat("  OK -", uf_atual, "salva.\n")
    TRUE
  }, error = function(e) {
    cat("  ERRO na UF", uf_atual, ":", conditionMessage(e), "\n")
    cat("  (pode rodar o loop de novo depois -- ele retoma daqui)\n")
    FALSE
  })
}

arquivos_sinasc_br <- list.files("checkpoints_brasil", pattern = "^sinasc_.*\\.rds$", full.names = TRUE)
arquivos_sim_br    <- list.files("checkpoints_brasil", pattern = "^sim_.*\\.rds$", full.names = TRUE)

cat("\nUFs processadas com sucesso:", length(arquivos_sinasc_br), "de", length(ufs_brasil), "\n")

sinasc_municipio_brasil <- bind_rows(lapply(arquivos_sinasc_br, readRDS))
sim_municipio_brasil    <- bind_rows(lapply(arquivos_sim_br, readRDS))

dataset_final_brasil <- integrar_sinasc_sim(sinasc_municipio_brasil, sim_municipio_brasil)

# >>> RODAR E COLAR O OUTPUT NA CONVERSA <<<
validar_fase3(dataset_final_brasil, sinasc_municipio_brasil)

saveRDS(dataset_final_brasil, "dataset_final_BRASIL.rds")
cat("\n[OK] Total de municípios-ano no Brasil:", nrow(dataset_final_brasil), "\n")
cat("Municípios-ano por UF:\n")
print(table(dataset_final_brasil$COD_UF))


# #############################################################################
# PARTE 5 - FASE 4: VALIDAÇÃO TÉCNICA (EDA) -- ATIVO, RODAR EM SEGUIDA
# =============================================================================
# PRÉ-REQUISITO: dataset_final_brasil já carregado (saída da Parte 4, ou
# readRDS("dataset_final_BRASIL.rds")).
# =============================================================================

# ---- 5.1 Classificação de confiabilidade da RMM (3 níveis, achado 6) -------
# Baixa  : NASCIDOS_VIVOS < 100  (RMM pode chegar a milhares, ruído extremo)
# Média  : 100-499 nascidos vivos (ainda instável, mas menos extremo)
# Alta   : >= 500 nascidos vivos (amostra grande o suficiente pra refletir
#          risco real, não apenas variação por acaso)

dataset_final_brasil <- dataset_final_brasil %>%
  mutate(
    CONFIABILIDADE_RMM = case_when(
      NASCIDOS_VIVOS < 100  ~ "Baixa",
      NASCIDOS_VIVOS < 500  ~ "Média",
      TRUE                  ~ "Alta"
    ),
    CONFIABILIDADE_RMM = factor(CONFIABILIDADE_RMM, levels = c("Baixa", "Média", "Alta"))
  )

# ---- 5.2 Estatísticas descritivas completas --------------------------------

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

# ---- 5.3 Completude final (escala Brasil) ----------------------------------

completude_final <- dataset_final_brasil %>%
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
  tidyr::pivot_longer(everything(), names_to = "coluna", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\n== Completude final (dataset_final_brasil) ==\n")
print(completude_final, n = Inf)

# ---- 5.4 Resumo por nível de confiabilidade da RMM -------------------------

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

# ---- 5.5 Resumo de peso implausível (achado 7) -----------------------------

cat("\n== Município-ano com peso implausível (< 300g em algum registro) ==\n")
n_implausivel <- sum(dataset_final_brasil$TEM_PESO_IMPLAUSIVEL)
cat("Total:", n_implausivel, "de", nrow(dataset_final_brasil),
    sprintf("(%.2f%%)\n", 100 * n_implausivel / nrow(dataset_final_brasil)))
cat("(dado original do SUS preservado -- apenas sinalizado, não corrigido)\n")

# ---- 5.6 Visualização: RMM por nível de confiabilidade ---------------------
# A RMM tem muitos zeros e poucos valores extremos -- histograma direto
# ficaria dominado pela barra de zero. Por isso: (a) histograma excluindo
# zeros, pra ver a forma real da distribuição; (b) boxplot em escala log,
# pra comparar os 3 níveis lado a lado de forma justa.

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

# ---- 5.7 Exportação para Tableau (.csv) -------------------------------------

write.csv(dataset_final_brasil, "dataset_final_BRASIL.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n[OK] Exportado: dataset_final_BRASIL.csv (", nrow(dataset_final_brasil), "linhas,",
    ncol(dataset_final_brasil), "colunas)\n")

saveRDS(dataset_final_brasil, "dataset_final_BRASIL.rds")
saveRDS(resumo_estatistico, "resumo_estatistico_BRASIL.rds")
saveRDS(completude_final, "completude_final_BRASIL.rds")
saveRDS(resumo_confiabilidade, "resumo_confiabilidade_BRASIL.rds")


# #############################################################################
# PARTE 6 - FASE 5: CARGA E PUBLICAÇÃO -- AINDA NÃO IMPLEMENTADA
# =============================================================================
# Vai incluir:
#   - Exportação para .parquet (formato final, mais performático que .csv)
#   - Dicionário de dados formal (documento separado, já com rascunho pronto
#     no mapa_projeto_datapaper.md)
#   - Publicação no Zenodo com DOI
#   - Licenciamento CC-BY 4.0
#   - README.md do repositório (GitHub + Zenodo)
# #############################################################################
