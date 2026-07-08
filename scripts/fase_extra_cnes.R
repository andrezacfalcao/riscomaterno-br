#enriquecimento cnes (leitos obstétricos/neonatais)
#fonte adicional para medir infraestrutura de saúde por município
#pré-requisito: parte 0 e parte 1 do pipeline principal já carregadas
#(library(microdatasus), options(timeout = 600), funções de padronização)
#desenho: cnes é um retrato mensal (não anual como sinasc/sim). usamos
#dezembro de cada ano (2022, 2023) como mês de referência representa o
#estoque de infraestrutura ao final de cada ano, alinhado ao período do
#sinasc/sim.
#tipo_leito relevantes (cnes): leitos de obstetrícia e neonatologia/uti
#neonatal são os que mais diretamente conectam com risco materno-infantil.

library(dplyr)
library(stringr)
library(microdatasus)

options(timeout = 600)

ufs_brasil <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS",
                "MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC",
                "SP","SE","TO")

if (!exists("dataset_final_brasil")) {
  if (file.exists("dataset_final_BRASIL.rds")) {
    dataset_final_brasil <- readRDS("dataset_final_BRASIL.rds")
    cat("[OK] dataset_final_brasil carregado do disco.\n")
  } else {
    stop("dataset_final_BRASIL.rds não encontrado. Rode pipeline_completo_final.R primeiro.")
  }
}

#confirmado com dado real (teste piloto ac, dez/2022) e tabela oficial cnes:
#tp_leito é numérico sem zero à esquerda (1 a 7)
#codleito é texto com zero à esquerda ("01" a "95")
#códigos oficiais (fonte: cnes2.datasus.gov.br, tabela de domínio tipo de leito):
#tp_leito=4 (obstétrico): codleito "10" (cirúrgica) e "43" (clínica)
#tp_leito=2 (clínico): codleito "41" (neonatologia)
#tp_leito=3 (complementar): codleito "80","81","82" (uti neonatal i/ii/iii)
#e "92","93" (uci neonatal convencional/canguru)

#atualização: em vez de usar só dezembro como retrato do ano, agora
#calculamos a média (ou mediana, configurável) dos 12 meses do ano
#reduz o efeito de mudanças abruptas de infraestrutura perto do fim do ano,
#ao custo de números fracionários (que não existiram em nenhum mês
#específico) e de 12x mais downloads. documentar essa troca metodológica
#explicitamente no artigo.

extrair_leitos_obstetricos <- function(uf_atual, ano, estatistica = "media") {
  tryCatch({
    lt_brutos <- fetch_datasus(
      year_start = ano, month_start = 1,
      year_end = ano, month_end = 12,
      uf = uf_atual, information_system = "CNES-LT"
    )

    lt_brutos <- lt_brutos %>%
      mutate(
        CODMUNRES = as.character(CODUFMUN),
        COD_UF    = str_sub(CODMUNRES, 1, 2)
      )

    #achado de validação: sem isto, um mês sem nenhum leito obstétrico
    #simplesmente "desaparece" da agregação (a linha não existe, não vira
    #zero); isso infla artificialmente a média, pois ela seria calculada
    #só sobre os meses com leito > 0. a "grade completa" abaixo usa o
    #cadastro geral do cnes (todos os municípios/meses baixados, não só
    #os com leito obstétrico) para garantir que meses sem leito entrem
    #como 0 na conta, não fiquem ausentes.
    grade_completa <- lt_brutos %>%
      distinct(COD_UF, CODMUNRES, COMPETEN)

    leitos_por_mes <- lt_brutos %>%
      mutate(
        LEITO_OBSTETRICO_NEONATAL = (
          (TP_LEITO == 4 & CODLEITO %in% c("10", "43")) |
          (TP_LEITO == 2 & CODLEITO == "41") |
          (TP_LEITO == 3 & CODLEITO %in% c("80", "81", "82", "92", "93"))
        )
      ) %>%
      filter(LEITO_OBSTETRICO_NEONATAL) %>%
      group_by(COD_UF, CODMUNRES, COMPETEN) %>%
      summarise(leitos_no_mes = sum(as.numeric(QT_EXIST), na.rm = TRUE), .groups = "drop")

    #junta com a grade completa: mês sem leito obstétrico entra como 0
    #de verdade, não fica ausente
    leitos_completo <- grade_completa %>%
      left_join(leitos_por_mes, by = c("COD_UF", "CODMUNRES", "COMPETEN")) %>%
      mutate(leitos_no_mes = coalesce(leitos_no_mes, 0))

    leitos_completo %>%
      group_by(COD_UF, CODMUNRES) %>%
      summarise(
        N_LEITOS_OBSTETRICOS = if (estatistica == "media") {
          round(mean(leitos_no_mes, na.rm = TRUE), 1)
        } else {
          median(leitos_no_mes, na.rm = TRUE)
        },
        N_MESES_DISPONIVEIS = n(),
        .groups = "drop"
      ) %>%
      mutate(ANO_NASC = ano)
  }, error = function(e) {
    cat("  ERRO CNES-LT", uf_atual, ano, ":", conditionMessage(e), "\n")
    NULL
  })
}

#loop nacional (27 ufs x 2 anos) com checkpoint

dir.create("checkpoints_cnes", showWarnings = FALSE)

for (uf_atual in ufs_brasil) {
  for (ano in c(2022, 2023)) {
    arquivo <- file.path("checkpoints_cnes", paste0("cnes_", uf_atual, "_", ano, ".rds"))
    if (file.exists(arquivo)) { cat("CNES", uf_atual, ano, "já processado, pulando.\n"); next }

    cat("Processando CNES-LT:", uf_atual, ano, "\n")
    resultado <- extrair_leitos_obstetricos(uf_atual, ano)
    if (!is.null(resultado)) saveRDS(resultado, arquivo)
  }
}

#combina e confere

arquivos_cnes <- list.files("checkpoints_cnes", pattern = "^cnes_.*\\.rds$", full.names = TRUE)
cnes_municipio <- bind_rows(lapply(arquivos_cnes, readRDS))

cat("\n[OK] Município-ano com dado de CNES:", nrow(cnes_municipio), "\n")
print(summary(cnes_municipio$N_LEITOS_OBSTETRICOS))

saveRDS(cnes_municipio, "cnes_municipio_BRASIL.rds")

#join final: adiciona cnes ao dataset_final_brasil
#mesma lógica: left_join a partir do dataset_final_brasil (que já tem
#sinasc+sim), pela chave composta cod_uf+codmunres+ano_nasc. município-ano
#sem registro no cnes vira 0 significa "nenhum leito obstétrico/neonatal
#cadastrado naquele município naquele ano", que é informação real, não
#dado faltante.
#importante: remove primeiro qualquer coluna de leito de uma rodada
#anterior (ex: versão "só dezembro"), senão o left_join cria colunas
#duplicadas (.x/.y) e o coalesce() quebra por não achar o nome exato.

dataset_final_brasil <- dataset_final_brasil %>%
  select(-any_of(c("N_LEITOS_OBSTETRICOS", "TEM_LEITO", "N_MESES_DISPONIVEIS"))) %>%
  left_join(cnes_municipio, by = c("COD_UF", "CODMUNRES", "ANO_NASC")) %>%
  mutate(
    N_LEITOS_OBSTETRICOS = coalesce(N_LEITOS_OBSTETRICOS, 0),
    N_MESES_DISPONIVEIS = coalesce(N_MESES_DISPONIVEIS, 0L)
  )

cat("\n[OK] Dataset final agora com", ncol(dataset_final_brasil), "colunas\n")
cat("Município-ano SEM nenhum leito obstétrico/neonatal:",
    sum(dataset_final_brasil$N_LEITOS_OBSTETRICOS == 0), "de", nrow(dataset_final_brasil),
    sprintf("(%.1f%%)\n", 100 * mean(dataset_final_brasil$N_LEITOS_OBSTETRICOS == 0)))

saveRDS(dataset_final_brasil, "dataset_final_BRASIL.rds")
write.csv(dataset_final_brasil, "dataset_final_BRASIL.csv", row.names = FALSE, fileEncoding = "UTF-8")
