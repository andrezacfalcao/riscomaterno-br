#orquestração com dagster riscomaterno-br
#importante: este arquivo não reimplementa nenhuma lógica de limpeza/
#transformação de dados. ele só chama os scripts r já validados, na ordem
#certa, usando o dagster como camada de orquestração (dependências, retry,
#monitoramento visual). a lógica de negócio continua 100% em r.
#instalação (rodar uma vez, no terminal):
#pip install dagster dagster-webserver
#como rodar (no terminal, na pasta onde este arquivo e os scripts .r estão):
#dagster dev -f pipeline_dagster.py
#depois abra http://localhost:3000 no navegador lá você vê o pipeline
#visualmente e clica em "materialize all" para rodar tudo.
#pré-requisito: r e rscript precisam estar instalados e acessíveis no path
#do sistema (o mesmo r que você já usa no rstudio funciona).

import subprocess
from dagster import op, job, In, Out, Nothing, get_dagster_logger


def rodar_script_r(nome_arquivo: str) -> str:
    """
    Executa um script R via Rscript e propaga erro se o script falhar.
    Isso é o equivalente, em Dagster, ao tryCatch que já usávamos nos loops
    R -- só que agora o Dagster também registra o resultado, retenta se
    configurado, e mostra tudo na interface visual.
    """
    logger = get_dagster_logger()
    logger.info(f"Executando: Rscript {nome_arquivo}")

    resultado = subprocess.run(
        ["Rscript", nome_arquivo],
        capture_output=True,
        text=True
    )

    logger.info(resultado.stdout)

    if resultado.returncode != 0:
        logger.error(resultado.stderr)
        raise Exception(f"Falha ao executar {nome_arquivo} (código {resultado.returncode})")

    return resultado.stdout


#cada @op abaixo corresponde a uma fase já existente do pipeline r.
#ajuste os nomes de arquivo se os seus scripts estiverem em outra pasta.

@op
def extrair_e_integrar_sinasc_sim() -> str:
    """
    Fases 1-3: extração das 27 UFs, limpeza (SINASC+SIM), agregação por
    município-ano e join com cálculo de RMM.
    Script: pipeline_completo_final.R
    """
    return rodar_script_r("pipeline_completo_final.R")


@op(ins={"depende_de": In(Nothing)})
def enriquecer_com_cnes() -> str:
    """
    Extração do CNES-LT (leitos obstétricos/neonatais, dez/2022 e dez/2023)
    e join com o dataset_final_brasil.
    Script: fase_extra_cnes.R
    """
    return rodar_script_r("fase_extra_cnes.R")


@op(ins={"depende_de": In(Nothing)})
def validar_e_explorar() -> str:
    """
    Fase 4: classificação de confiabilidade (CONFIABILIDADE_RMM),
    estatísticas descritivas, tabelas e gráficos exploratórios.
    Script: fase4_validacao_eda.R
    """
    return rodar_script_r("fase4_validacao_eda.R")


@op(ins={"depende_de": In(Nothing)})
def preparar_publicacao() -> str:
    """
    Fase 5: exportação final em .parquet/.csv, dicionário de dados,
    README.md -- arquivos prontos para upload no Zenodo.
    Script: fase5_publicacao.R
    """
    return rodar_script_r("fase5_publicacao.R")


#o job define a ordem de dependência entre as fases isso é literalmente
#o "dag" (directed acyclic graph) que a disciplina pede: cada fase só começa
#depois que a anterior terminar com sucesso.

@job
def pipeline_riscomaterno_br():
    etapa_1_3 = extrair_e_integrar_sinasc_sim()
    etapa_cnes = enriquecer_com_cnes(depende_de=etapa_1_3)
    etapa_eda = validar_e_explorar(depende_de=etapa_cnes)
    preparar_publicacao(depende_de=etapa_eda)
