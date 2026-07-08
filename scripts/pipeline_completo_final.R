#pipeline completo e definitivo
#dataset de risco socioespacial e desfechos materno-infantis (sinasc + sim)
#dsw/sbbd 2025
#estrutura deste arquivo:
#parte 0 setup
#parte 1 funções reutilizáveis (fase 2 + fase 3)
#parte 2 bloco piloto pe [histórico/comentado já validado]
#parte 3 bloco regional nordeste [histórico/comentado já validado]
#parte 4 bloco oficial brasil (27 ufs) [ativo rodar]
#parte 5 fase 4: validação técnica (eda) [ativo rodar]
#parte 6 fase 5: carga e publicação [ainda não implementada]
#como usar agora:
#1) rode parte 0 e parte 1 (setup + funções) sempre primeiro.
#2) não precisa rodar parte 2 nem parte 3 estão comentadas, servem só
#de registro histórico de como validamos o pipeline em etapas
#(pe -> nordeste -> brasil) antes de rodar em escala nacional direta.
#3) apague os checkpoints antigos (a função de agregação mudou ver
#instrução no início da parte 4) e rode a parte 4 (brasil completo).
#4) rode a parte 5 (fase 4: eda) em seguida, usando o dataset_final_brasil
#gerado na parte 4.
#histórico de achados de validação (documentar no artigo, seção qualidade)
#1) consultas usa rótulos "1 a 3 vezes" / "4 a 6 vezes" / "7 ou mais vezes"
#/ "nenhuma" (não "de 1 a 3 consultas", como assumido inicialmente).
#2) obitoparto não é campo de óbito materno pertence à investigação de
#óbito fetal/infantil (timing da morte do bebê em relação ao parto).
#incluí-lo por engano numa versão anterior inflou o resultado de forma
#incorreta (150 -> 1496 óbitos maternos). corrigido: só obitograv e
#obitopuerp são válidos para a definição ampliada de óbito materno.
#obitograv: "sim"/"não" (binário normal)
#obitopuerp: "de 0 a 42 dias"/"de 43 dias a 1 ano"/"não"/na
#(as duas primeiras categorias = afirmativo)
#3) process_sinasc() zera 100% do campo peso (bug do pacote microdatasus).
#corrigido restaurando o valor a partir do dado bruto (corrigir_peso()).
#4) códigos de município terminados em "0000" (ex: "260000" em pe)
#representam "município ignorado", não uma cidade real removidos
#via remover_municipio_placeholder().
#5) download de ufs grandes (sp) pode estourar o timeout padrão do r
#(240s) por causa do tamanho do arquivo (24mb). corrigido com
#options(timeout = 600) na parte 0.
#6) rmm (razão de mortalidade materna) é estatisticamente instável em
#municípios com poucos nascidos vivos um só óbito num município
#pequeno gera rmm de milhares. não existe corte limpo (município com
#118 nascidos vivos ainda mostrou rmm de 2.542). tratamento adotado:
#classificação em 3 níveis de confiabilidade (não destrutiva ver
#confiabilidade_rmm na fase 4), em vez de uma flag binária ou de
#remover/suprimir os dados.
#7) peso mínimo observado de 100g em registros individuais do sinasc
#abaixo do limite de viabilidade neonatal reconhecido (245g é o
#recorde mundial de sobrevivência), quase certamente erro de
#digitação no preenchimento original da declaração de nascido vivo.
#não corrigimos o valor (o dado original do sus nunca é alterado)
#apenas sinalizamos com a flag tem_peso_implausivel (< 300g), calculada
#durante a agregação por município-ano.


#parte 0 setup

library(microdatasus)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)

#timeout maior para evitar falha de download em ufs grandes (achado 5)
options(timeout = 600)


#parte 1 funções reutilizáveis (fase 2 + fase 3)
#nenhuma função aqui referencia uf nem ano específicos funcionam iguais
#para qualquer escala (piloto, regional ou nacional).

#fase 2: transformação e limpeza

#1.1 corrige o bug conhecido do peso no process_sinasc() (achado 3)
corrigir_peso <- function(dados_sinasc_processado, dados_sinasc_brutos) {
  stopifnot(nrow(dados_sinasc_brutos) == nrow(dados_sinasc_processado))
  dados_sinasc_processado$PESO <- as.numeric(dados_sinasc_brutos$PESO)
  dados_sinasc_processado
}

#1.2 padronização de tipos e datas
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

#1.3 remoção de códigos de município placeholder "ignorado" (achado 4)
remover_municipio_placeholder <- function(df) {
  df %>% filter(!str_detect(CODMUNRES, "0000$"))
}

#1.4 tratamento de valores ignorados (9, 99, "ignorado") -> na
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

#1.5 identificação de óbito materno definição dupla: cid + ampliada
#(achado 2: obitoparto excluído de propósito)
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

#1.6 agregação por município-ano
#tem_peso_implausivel (achado 7): sinaliza município-ano com pelo menos 1
#registro de peso ao nascer abaixo de 300g território de erro de
#digitação, não de bebê extremamente prematuro real (que tipicamente fica
#entre 500-999g). não removemos nem corrigimos o valor original só
#sinalizamos, mesmo princípio não destrutivo usado na confiabilidade_rmm.
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

#1.7 relatório de completude (insumo direto pra seção "qualidade")
relatorio_completude <- function(df, nome_base) {
  df %>%
    summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
    pivot_longer(everything(), names_to = "coluna", values_to = "pct_na") %>%
    mutate(base = nome_base) %>%
    arrange(desc(pct_na))
}

#1.8 checklist de validação automática fase 2
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

#fase 3: integração (join)

#1.9 join sinasc + sim por município-ano e cálculo da rmm
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

#1.10 checklist de validação automática fase 3
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


#parte 2 bloco piloto pe (2022-2023)
#histórico já validado e superado. mantido comentado só como registro de
#reprodutibilidade (mostra como o pipeline foi testado numa escala pequena
#antes de escalar). não precisa rodar os dados de pe já estão incluídos
#no bloco oficial brasil (parte 4).

#uf_piloto <- "pe"
#ano_inicio_piloto <- 2022
#ano_fim_piloto <- 2023
#sinasc_brutos_pe <- fetch_datasus(year_start = ano_inicio_piloto, year_end = ano_fim_piloto,
#uf = uf_piloto, information_system = "sinasc")
#sim_brutos_pe <- fetch_datasus(year_start = ano_inicio_piloto, year_end = ano_fim_piloto,
#uf = uf_piloto, information_system = "sim-do")
#sinasc_pe <- process_sinasc(sinasc_brutos_pe)
#sinasc_pe <- corrigir_peso(sinasc_pe, sinasc_brutos_pe)
#sinasc_pe <- padronizar_sinasc(sinasc_pe)
#sinasc_pe <- remover_municipio_placeholder(sinasc_pe)
#sinasc_pe <- tratar_ignorados(sinasc_pe, intersect(
#c("parto", "consultas", "gestacao", "gravidez", "escmae", "racacor", "sexo", "locnasc"),
#names(sinasc_pe)
#))
#sinasc_pe <- sinasc_pe %>% mutate(idademae = if_else(idademae < 9 | idademae > 60, na_real_, idademae))
#sim_pe <- process_sim(sim_brutos_pe)
#sim_pe <- padronizar_sim(sim_pe)
#sim_pe <- remover_municipio_placeholder(sim_pe)
#sim_pe <- tratar_ignorados(sim_pe, intersect(
#c("racacor", "esc", "assistmed", "circobito", "sexo"),
#names(sim_pe)
#))
#sim_pe <- identificar_obito_materno(sim_pe)
#sinasc_municipio_pe <- agregar_sinasc(sinasc_pe)
#sim_municipio_pe <- agregar_sim(sim_pe)
#validar_fase2(sinasc_pe, sim_pe, sinasc_municipio_pe, sim_municipio_pe)
#dataset_final_pe <- integrar_sinasc_sim(sinasc_municipio_pe, sim_municipio_pe)
#validar_fase3(dataset_final_pe, sinasc_municipio_pe)
#saverds(dataset_final_pe, "dataset_final_pe.rds")


#parte 3 bloco regional nordeste (9 ufs)
#histórico já validado e superado. foi a etapa intermediária entre o
#piloto pe e a rodada nacional completa (validou o pipeline em 9 estados
#diversos, com contagem de municípios batendo 100% com o ibge). mantido
#comentado só como registro. não precisa rodar os dados do nordeste já
#estão incluídos no bloco oficial brasil (parte 4).

#ufs_nordeste <- c("al", "ba", "ce", "ma", "pb", "pe", "pi", "rn", "se")
#dir.create("checkpoints_nordeste", showwarnings = false)
#for (uf_atual in ufs_nordeste) {
#arquivo_sinasc <- file.path("checkpoints_nordeste", paste0("sinasc_", uf_atual, ".rds"))
#arquivo_sim <- file.path("checkpoints_nordeste", paste0("sim_", uf_atual, ".rds"))
#if (file.exists(arquivo_sinasc) && file.exists(arquivo_sim)) next
#trycatch({
#sb <- fetch_datasus(year_start = 2022, year_end = 2023, uf = uf_atual, information_system = "sinasc")
#s <- process_sinasc(sb)
#s <- corrigir_peso(s, sb)
#s <- padronizar_sinasc(s)
#s <- remover_municipio_placeholder(s)
#s <- tratar_ignorados(s, intersect(
#c("parto","consultas","gestacao","gravidez","escmae","racacor","sexo","locnasc"),
#names(s)))
#s <- s %>% mutate(idademae = if_else(idademae < 9 | idademae > 60, na_real_, idademae))
#s_agregado <- agregar_sinasc(s)
#mb <- fetch_datasus(year_start = 2022, year_end = 2023, uf = uf_atual, information_system = "sim-do")
#m <- process_sim(mb)
#m <- padronizar_sim(m)
#m <- remover_municipio_placeholder(m)
#m <- tratar_ignorados(m, intersect(
#c("racacor","esc","assistmed","circobito","sexo"), names(m)))
#m <- identificar_obito_materno(m)
#m_agregado <- agregar_sim(m)
#saverds(s_agregado, arquivo_sinasc)
#saverds(m_agregado, arquivo_sim)
#}, error = function(e) cat("erro na uf", uf_atual, ":", conditionmessage(e), "\n"))
#}
#arquivos_sinasc_ne <- list.files("checkpoints_nordeste", pattern = "^sinasc_.*\\.rds$", full.names = true)
#arquivos_sim_ne <- list.files("checkpoints_nordeste", pattern = "^sim_.*\\.rds$", full.names = true)
#sinasc_municipio_nordeste <- bind_rows(lapply(arquivos_sinasc_ne, readrds))
#sim_municipio_nordeste <- bind_rows(lapply(arquivos_sim_ne, readrds))
#dataset_final_nordeste <- integrar_sinasc_sim(sinasc_municipio_nordeste, sim_municipio_nordeste)
#validar_fase3(dataset_final_nordeste, sinasc_municipio_nordeste)
#saverds(dataset_final_nordeste, "dataset_final_nordeste.rds")


#parte 4 bloco oficial brasil (27 ufs) ativo, rodar agora
#importante: a função agregar_sinasc() mudou (ganhou tem_peso_implausivel),
#então checkpoints antigos estão desatualizados. rode isto antes do loop:
#unlink("checkpoints_nordeste", recursive = true)
#unlink("checkpoints_brasil", recursive = true)
#desenho: loop uf por uf com checkpoint individual em disco (retomável em
#caso de queda de conexão se cair no meio, rode o loop de novo e ele
#pula as ufs já processadas).

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

#>>> rodar e colar o output na conversa <<<
validar_fase3(dataset_final_brasil, sinasc_municipio_brasil)

saveRDS(dataset_final_brasil, "dataset_final_BRASIL.rds")
cat("\n[OK] Total de municípios-ano no Brasil:", nrow(dataset_final_brasil), "\n")
cat("Municípios-ano por UF:\n")
print(table(dataset_final_brasil$COD_UF))


#parte 5 fase 4: validação técnica (eda) ativo, rodar em seguida
#pré-requisito: dataset_final_brasil já carregado (saída da parte 4, ou
#readrds("dataset_final_brasil.rds")).

#5.1 classificação de confiabilidade da rmm (3 níveis, achado 6)
#baixa : nascidos_vivos < 100 (rmm pode chegar a milhares, ruído extremo)
#média : 100-499 nascidos vivos (ainda instável, mas menos extremo)
#alta : >= 500 nascidos vivos (amostra grande o suficiente pra refletir
#risco real, não apenas variação por acaso)

dataset_final_brasil <- dataset_final_brasil %>%
  mutate(
    CONFIABILIDADE_RMM = case_when(
      NASCIDOS_VIVOS < 100  ~ "Baixa",
      NASCIDOS_VIVOS < 500  ~ "Média",
      TRUE                  ~ "Alta"
    ),
    CONFIABILIDADE_RMM = factor(CONFIABILIDADE_RMM, levels = c("Baixa", "Média", "Alta"))
  )

#5.2 estatísticas descritivas completas

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

#5.3 completude final (escala brasil)

completude_final <- dataset_final_brasil %>%
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
  tidyr::pivot_longer(everything(), names_to = "coluna", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\n== Completude final (dataset_final_brasil) ==\n")
print(completude_final, n = Inf)

#5.4 resumo por nível de confiabilidade da rmm

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

#5.5 resumo de peso implausível (achado 7)

cat("\n== Município-ano com peso implausível (< 300g em algum registro) ==\n")
n_implausivel <- sum(dataset_final_brasil$TEM_PESO_IMPLAUSIVEL)
cat("Total:", n_implausivel, "de", nrow(dataset_final_brasil),
    sprintf("(%.2f%%)\n", 100 * n_implausivel / nrow(dataset_final_brasil)))
cat("(dado original do SUS preservado -- apenas sinalizado, não corrigido)\n")

#5.6 visualização: rmm por nível de confiabilidade
#a rmm tem muitos zeros e poucos valores extremos histograma direto
#ficaria dominado pela barra de zero. por isso: (a) histograma excluindo
#zeros, pra ver a forma real da distribuição; (b) boxplot em escala log,
#pra comparar os 3 níveis lado a lado de forma justa.

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

#5.7 exportação para tableau (.csv)

write.csv(dataset_final_brasil, "dataset_final_BRASIL.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n[OK] Exportado: dataset_final_BRASIL.csv (", nrow(dataset_final_brasil), "linhas,",
    ncol(dataset_final_brasil), "colunas)\n")

saveRDS(dataset_final_brasil, "dataset_final_BRASIL.rds")
saveRDS(resumo_estatistico, "resumo_estatistico_BRASIL.rds")
saveRDS(completude_final, "completude_final_BRASIL.rds")
saveRDS(resumo_confiabilidade, "resumo_confiabilidade_BRASIL.rds")


#parte 6 fase 5: carga e publicação ainda não implementada
#vai incluir:
#exportação para .parquet (formato final, mais performático que .csv)
#dicionário de dados formal (documento separado, já com rascunho pronto
#no mapa_projeto_datapaper.md)
#publicação no zenodo com doi
#licenciamento cc-by 4.0
#readme.md do repositório (github + zenodo)
