# RiscoMaterno-BR: Dataset de Risco Socioespacial e Desfechos Materno-Infantis (SINASC + SIM + CNES)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21251929.svg)](https://doi.org/10.5281/zenodo.21251929)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![R Project](https://img.shields.io/badge/Language-R-%23276DC3.svg)](https://www.r-project.org/)
[![Python / Dagster](https://img.shields.io/badge/Orchestrator-Dagster-%23FC4349.svg)](https://dagster.io/)

---

## 1. Visão Geral e Objetivo

O **RiscoMaterno-BR** é um projeto de ciência de dados em saúde que visa resolver um dos grandes desafios no uso de dados abertos do SUS: a **fragmentação das bases de dados nacionais**. Devido à anonimização necessária para proteger a privacidade dos cidadãos, o cruzamento direto de dados individuais entre diferentes sistemas públicos é inviável sem procedimentos complexos de *linkage* probabilístico.

Este projeto propõe uma abordagem alternativa e altamente eficaz: **a criação de uma "zona comum" espacial e temporal de dados integrados a nível municipal (município-ano)**. Com isso, o projeto alcança os seguintes objetivos:

*   **Integração Multidimensional**: Reúne indicadores de qualidade do pré-natal (SINASC), desfechos de mortalidade materna (SIM) e infraestrutura de saúde obstétrica e neonatal (CNES).
*   **Métrica Inédita de RMM**: Consolida e calcula de forma automática a **Razão de Mortalidade Materna (RMM)** por município-ano, um indicador crítico que não está disponível diretamente nos sistemas oficiais do DATASUS.
*   **Reprodutibilidade Estrita**: O pipeline de dados é totalmente documentado e orquestrado por código aberto, permitindo que qualquer pesquisador reproduza o ativo de dados de ponta a ponta para qualquer Unidade da Federação (UF) e período de tempo.
*   **Reusabilidade para Pesquisa e ML**: O produto final do projeto é o próprio dataset — estruturado, validado e pronto para alimentar modelos epidemiológicos, algoritmos de Aprendizado de Máquina (Machine Learning) ou subsidiar decisões de gestores públicos de saúde.

---

## 2. Acesso ao Dataset

O dataset consolidado e enriquecido está publicado e permanentemente arquivado no **Zenodo**:

*   **Link de Acesso**: [https://doi.org/10.5281/zenodo.21251929](https://doi.org/10.5281/zenodo.21251929)
*   **Licença**: Creative Commons Attribution 4.0 International (CC-BY 4.0)

---

## 3. Arquitetura de Dados (Modelo Medallion)

O pipeline de engenharia de dados do projeto segue a arquitetura Medallion para garantir a rastreabilidade e integridade das informações extraídas:

| Camada | Descrição do Dado | Fontes / Processamento | Status |
| :--- | :--- | :--- | :--- |
| 🟤 **Bronze** *(Raw)* | Dados exatamente como saem do DATASUS (arquivos `.dbc` brutos) | Extração do SINASC, SIM e CNES via pacote R `microdatasus` (27 UFs) 
| ⚪ **Prata** *(Silver)* | Dados limpos, tipados e padronizados a nível de registro individual | Filtragem de inconsistências, tratamento de dados ignorados e padronização temporal 
| 🟡 **Ouro** *(Gold)* | Dados agregados, cruzados e enriquecidos a nível de **município-ano** | Agregação espacial (IBGE 6 d.), cálculo da RMM e join final de infraestrutura (CNES) 

---

## 4. Dicionário de Dados (Camada Ouro)

O dataset final (`dataset_final_brasil.csv` / `.parquet`) possui **18 colunas** e **11.139 observações**, abrangendo as 27 UFs no período de 2022-2023.

| Coluna | Tipo | Descrição | Origem |
| :--- | :--- | :--- | :--- |
| `COD_UF` | string | Código da UF de residência (2 primeiros dígitos do código do município) | IBGE |
| `CODMUNRES` | string | Código de 6 dígitos do município de residência (IBGE/DATASUS) | SINASC/SIM/CNES |
| `ANO_NASC` | inteiro | Ano de referência dos nascimentos e óbitos (2022-2023) | SINASC/SIM/CNES |
| `NASCIDOS_VIVOS` | inteiro | Número total de nascidos vivos registrados no município-ano | Agregado SINASC |
| `PCT_PARTO_CESAREO` | decimal | Percentual (%) de partos cesáreos no município-ano | SINASC (`PARTO`) |
| `PCT_SEM_PRENATAL` | decimal | Percentual (%) de mães que não realizaram consultas de pré-natal | SINASC (`CONSULTAS`) |
| `PCT_MENOS_4_CONSULTAS` | decimal | Percentual (%) de mães que realizaram menos de 4 consultas de pré-natal | SINASC (`CONSULTAS`) |
| `PCT_MAE_ADOLESCENTE` | decimal | Percentual (%) de mães com idade inferior a 20 anos | SINASC (`IDADEMAE`) |
| `PCT_BAIXO_PESO` | decimal | Percentual (%) de recém-nascidos com baixo peso ao nascer (< 2.500g) | SINASC (`PESO`) |
| `IDADE_MEDIA_MAE` | decimal | Idade média registrada das mães no município-ano | SINASC (`IDADEMAE`) |
| `TEM_PESO_IMPLAUSIVEL` | booleano | Indica se há ao menos 1 registro de peso < 300g no município-ano (mantido para preservar dados originais) | SINASC (`PESO`) |
| `OBITOS_MATERNOS_CID` | inteiro | Óbitos maternos sob a definição estrita da CID-10 (Códigos O00-O99) | SIM (`CAUSABAS`) |
| `OBITOS_MATERNOS_AMPLIADO` | inteiro | Óbitos maternos sob definição ampliada (inclui CID + variáveis de investigação `OBITOGRAV`/`OBITOPUERP`) | SIM |
| `RMM` | decimal | Razão de Mortalidade Materna oficial (baseada na definição ampliada) por 100 mil nascidos vivos | Calculado no join |
| `RMM_CID` | decimal | RMM alternativa e conservadora (baseada estritamente nos códigos CID-10) por 100 mil nascidos vivos | Calculado no join |
| `CONFIABILIDADE_RMM` | fator | Nível de confiabilidade estatística da RMM, baseado no volume de `NASCIDOS_VIVOS` (Baixa: <100, Média: 100-499, Alta: >=500) | Calculado na Fase 4 |
| `N_LEITOS_OBSTETRICOS` | decimal | Média do total de leitos obstétricos, neonatologia, e UTI/UCI neonatal disponíveis ao longo dos 12 meses do ano | CNES-LT |
| `N_MESES_DISPONIVEIS` | inteiro | Quantidade de meses (máximo 12) que possuíam dados do CNES disponíveis no ano (indicador de completude e auditoria) | CNES-LT |

---

## 5. Validação Técnica & Achados Metodológicos

Ao longo da validação técnica e do processo de engenharia reversa do DATASUS, foram feitas descobertas metodológicas críticas que foram devidamente corrigidas no pipeline do `RiscoMaterno-BR`:

1.  **Bug de Limpeza do Peso (`PESO`)**: O pacote `microdatasus` zerava erroneamente os dados de peso ao nascer durante algumas conversões. O pipeline corrige essa inconsistência restaurando a variável a partir do dado bruto.
2.  **Mortalidade Materna Ampliada vs. Estrita (CID)**: A métrica `OBITOS_MATERNOS_AMPLIADO` captura óbitos que as marcações puras do CID-10 perderiam, representando um aumento de aproximadamente ~53% na captura de dados reais em relação à classificação restrita.
3.  **Média de Leitos Obstétricos (CNES-LT v2)**: Em vez de capturar apenas uma variável de "estoque" pontual (retrato de dezembro), o pipeline calcula a **média real dos leitos ativos nos 12 meses do ano**.
4.  **Correção do Viés de Ausência de Leitos**: Um bug sutil comum em agregações de saúde ocorre quando meses sem leitos são omitidos da base original, o que inflaria artificialmente a média real dos municípios. O pipeline cria uma "grade completa" (município × mês) de forma que meses com zero leitos entrem no denominador com valor zero correto.

---

## 6. Estrutura do Repositório

```bash
riscomaterno-br/
├── scripts/
│   ├── pipeline_completo_final.R  # Fases 1 a 3 (extração, limpeza e join SINASC+SIM)
│   ├── fase_extra_cnes.R          # Enriquecimento com dados mensais de leitos do CNES-LT
│   ├── fase4_validacao_eda.R      # Fase 4 (Análise Exploratória de Dados, geração de tabelas e gráficos)
│   ├── fase5_publicacao.R         # Fase 5 (Preparação e exportação de metadados para o Zenodo)
│   └── pipeline_dagster.py        # Orquestrador visual do pipeline completo em Python
└── README.md                      # Instruções do projeto (este arquivo)
```

---

## 7. Orquestração e Como Executar o Pipeline

O pipeline de dados é orquestrado de forma visual usando o **Dagster** (Python), atuando como uma camada fina e segura de execução sobre os scripts analíticos originais em **R**.

```
extrair_e_integrar_sinasc_sim()  ➔  enriquecer_com_cnes()  ➔  validar_e_explorar()  ➔  preparar_publicacao()
```

### Pré-requisitos
*   **R** e **Rscript** instalados e acessíveis no PATH do sistema.
*   **Python 3.8+** instalado.

### Passo a Passo

1.  **Clone o repositório**:
    ```bash
    git clone https://github.com/seu-usuario/riscomaterno-br.git
    cd riscomaterno-br
    ```

2.  **Instale o Dagster e o Webserver**:
    ```bash
    pip install dagster dagster-webserver
    ```

3.  **Execute o orquestrador**:
    Na raiz do projeto, execute o comando abaixo para iniciar o painel administrativo:
    ```bash
    dagster dev -f scripts/pipeline_dagster.py
    ```

4.  **Acesse a interface gráfica**:
    *   Abra o seu navegador em [http://localhost:3000](http://localhost:3000).
    *   Navegue até a aba **Deployments** / **Jobs** e localize `pipeline_riscomaterno_br`.
    *   Clique em **Launch Run** ou **Materialize All** para executar o fluxo inteiro de ponta a ponta.

O Dagster lidará automaticamente com o encadeamento dos scripts, gerando logs claros de execução e salvando os arquivos intermediários nos respectivos caminhos.

---

## 8. Potencial de Reuso e Machine Learning

Este dataset consolidado foi projetado para habilitar novas pesquisas científicas e modelos preditivos em epidemiologia. Alguns exemplos de perguntas de pesquisa que podem ser estudadas a partir dele:

*   **RQ1**: Existe correlação entre a cobertura de pré-natal e a Razão de Mortalidade Materna municipal?
*   **RQ2**: Como essas relações variam regionalmente através das 5 grandes macrorregiões do Brasil?
*   **RQ3**: Qual o impacto direto do desabastecimento de leitos obstétricos (CNES) nos desfechos de mortalidade em municípios pequenos?

Como o dataset possui formato tabular clássico e já está normalizado e agregado no nível de município-ano, ele pode ser imediatamente carregado em bibliotecas como `scikit-learn` (Python) ou `caret`/`tidymodels` (R) para treinamento de modelos de regressão, classificação ou detecção de anomalias espaciais.

---

## 9. Como Citar

Se você utilizar este dataset ou código em sua pesquisa científica, cite da seguinte forma:

```text
Falcão, A. et al. (2026). RiscoMaterno-BR: Dataset de Risco Socioespacial e Desfechos Materno-Infantis Integrando SINASC, SIM e CNES (2022-2023). Zenodo. https://doi.org/10.5281/zenodo.21251929
```
