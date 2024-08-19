# Índice Grubel-Lloyd: Análise de Comércio Bilateral

Este projeto desenvolve uma análise do comércio bilateral entre o Brasil e outros países (China, Estados Unidos e Argentina), utilizando o Índice Grubel-Lloyd. O objetivo é calcular e visualizar o índice para diferentes produtos ao longo dos anos, permitindo insights sobre o grau de comércio intra-indústria entre o Brasil e alguns parceiros comerciais.

## Estrutura do Projeto

### Configurações Iniciais

Realiza-se a limpeza do ambiente RStudio e são importados os pacotes necessários, tais como `dplyr`, `ggplot2`, `tidyr`, e `plotly`. Além disso, são definidas as funções `read_data`, `calc_grubel_lloyd` e `plot_graph`.

Posteriormente, lê e trata a base de dados filtrando por um determinado produto, renomeando colunas, convertendo anos para o formato Date, e selecionando as colunas de interesse. Calcula-se o Índice Grubel-Lloyd para as exportações e importações entre o Brasil e o país especificado.

O índice é calculado como `GLi = 1 - ((|Xi - Mi|) / Xi + Mi)`.

Onde:
- GLi é o Índice de Grubel-Lloyd para o produto i;
- Xi representa o valor das exportações do produto i de um país para outro; 
- Mi representa o valor das importações do produto i pelo mesmo país; 
- |Xi − Mi| é o valor absoluto da diferença entre exportações e importações.

### Análises dos setores de café, fertilizantes e açúcar entre Brasil, China, EUA e Argentina

Calcula-se o Índice Grubel-Lloyd ao longo dos anos para cada produto, permitindo uma análise detalhada do comércio bilateral. Além disso, gera-se e gráficos interativos para visualizar melhor essas informações, permitindo uma análise visual do comércio intra-indústria entre os países.

### Análise de derivados de petróleo

Calcula-se o índice Grubel-Lloyd para cada código de produto entre o Brasil e os Estados Unidos. Os resultados são combinados em um único gráfico para comparação, mostrando a evolução do comércio intraindústria de derivados de petróleo entre 2013 e 2023.
