# =============================================================================
# FASE 5 - PREPARAÇÃO PARA PUBLICAÇÃO (ZENODO)
# Gera todos os arquivos que precisam ir no pacote de publicação
# =============================================================================
# PRÉ-REQUISITO: dataset_final_brasil já carregado (17 colunas)

library(dplyr)

# Torna este script autossuficiente (roda sozinho via Rscript)
if (!exists("dataset_final_brasil")) {
  if (file.exists("dataset_final_BRASIL.rds")) {
    dataset_final_brasil <- readRDS("dataset_final_BRASIL.rds")
    cat("[OK] dataset_final_brasil carregado do disco.\n")
  } else {
    stop("dataset_final_BRASIL.rds não encontrado. Rode as fases anteriores primeiro.")
  }
}

# Cria uma pasta só para os arquivos de publicação (organiza tudo junto)
dir.create("publicacao_zenodo", showWarnings = FALSE)

# =============================================================================
# 1. EXPORTAÇÃO DO DATASET EM MÚLTIPLOS FORMATOS
# =============================================================================
# .parquet -- formato recomendado (compacto, rápido, preserva tipos de dado)
# .csv     -- formato universal (qualquer pessoa abre, até no Excel)

if (!requireNamespace("arrow", quietly = TRUE)) {
  install.packages("arrow", repos = "https://cloud.r-project.org")
}
library(arrow)

write_parquet(dataset_final_brasil, "publicacao_zenodo/dataset_final_brasil.parquet")
write.csv(dataset_final_brasil, "publicacao_zenodo/dataset_final_brasil.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("[OK] Dataset exportado em .parquet e .csv\n")

# =============================================================================
# 2. DICIONÁRIO DE DADOS EM CSV (para acompanhar o dataset)
# =============================================================================

dicionario <- tibble::tribble(
  ~coluna, ~tipo, ~descricao,
  "COD_UF", "string", "Codigo da Unidade Federativa",
  "CODMUNRES", "string", "Codigo do municipio de residencia (IBGE/DATASUS)",
  "ANO_NASC", "inteiro", "Ano de referencia",
  "NASCIDOS_VIVOS", "inteiro", "Total de nascidos vivos no municipio-ano",
  "PCT_PARTO_CESAREO", "decimal", "Percentual de partos cesareos",
  "PCT_SEM_PRENATAL", "decimal", "Percentual de gestantes sem pre-natal",
  "PCT_MENOS_4_CONSULTAS", "decimal", "Percentual com menos de 4 consultas de pre-natal",
  "PCT_MAE_ADOLESCENTE", "decimal", "Percentual de maes com menos de 20 anos",
  "PCT_BAIXO_PESO", "decimal", "Percentual de recem-nascidos com menos de 2500g",
  "IDADE_MEDIA_MAE", "decimal", "Idade media das maes",
  "TEM_PESO_IMPLAUSIVEL", "booleano", "Municipio-ano com registro de peso menor que 300g",
  "OBITOS_MATERNOS_CID", "inteiro", "Obitos maternos, definicao CID-10 estrita (O00-O99)",
  "OBITOS_MATERNOS_AMPLIADO", "inteiro", "Obitos maternos, definicao ampliada (CID + flags SIM)",
  "RMM", "decimal", "Razao de Mortalidade Materna oficial, por 100 mil nascidos vivos",
  "RMM_CID", "decimal", "RMM conservadora, baseada na definicao CID estrita",
  "CONFIABILIDADE_RMM", "fator", "Confiabilidade estatistica da RMM: Baixa/Media/Alta",
  "N_LEITOS_OBSTETRICOS", "decimal", "Media de leitos obstetricos/neonatais ao longo dos 12 meses do ano (CNES-LT)",
  "N_MESES_DISPONIVEIS", "inteiro", "Quantos dos 12 meses do ano tinham dado geral do CNES disponivel para o municipio"
)

write.csv(dicionario, "publicacao_zenodo/dicionario_de_dados.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("[OK] Dicionário de dados exportado\n")

# =============================================================================
# 3. README.md PARA O REPOSITÓRIO
# =============================================================================

readme_texto <- '# RiscoMaterno-BR

Dataset municipal integrado de indicadores de pré-natal, mortalidade
materna e infraestrutura obstétrica no Brasil (2022-2023).

## Descrição

Este dataset integra três fontes públicas do Ministério da Saúde brasileiro
(SINASC, SIM e CNES) em nível de município-ano, cobrindo as 27 Unidades da
Federação. Calcula a Razão de Mortalidade Materna (RMM) municipal e uma
classificação de confiabilidade estatística inédita.

## Estrutura dos arquivos

- `dataset_final_brasil.parquet`: dataset completo, formato Parquet (recomendado)
- `dataset_final_brasil.csv`: dataset completo, formato CSV
- `dicionario_de_dados.csv`: descrição de cada coluna
- `pipeline_completo_final.R`: código-fonte da extração/limpeza/integração SINASC+SIM (reprodutibilidade)
- `fase_extra_cnes.R`: código-fonte do enriquecimento com infraestrutura (CNES)

## Volumetria

- 11.139 observações de município-ano
- 27 Unidades da Federação
- Período: 2022-2023
- 18 variáveis

## Como citar

[PREENCHER após publicação: citação no formato APA/ABNT com o DOI do Zenodo]

## Licença

CC-BY 4.0 -- uso livre com atribuição de crédito.

## Fontes originais

- SINASC/SIM/CNES: DATASUS, Ministério da Saúde do Brasil
- Extração via pacote R `microdatasus`
'

writeLines(readme_texto, "publicacao_zenodo/README.md")
cat("[OK] README.md gerado\n")

# =============================================================================
# 4. COPIA O CÓDIGO-FONTE PARA O PACOTE DE PUBLICAÇÃO (reprodutibilidade)
# =============================================================================
# Ajuste o caminho abaixo para onde está seu script principal
# file.copy("pipeline_completo_final.R", "publicacao_zenodo/pipeline_completo_final.R")
# file.copy("fase_extra_cnes.R", "publicacao_zenodo/fase_extra_cnes.R")

cat("\n[OK] Pasta 'publicacao_zenodo' pronta com:\n")
print(list.files("publicacao_zenodo"))
