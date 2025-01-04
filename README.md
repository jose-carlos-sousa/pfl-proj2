# README: Blackstone

## 1. Identificação do Grupo

**Membros:**  
- **João Mendes** (up202208783) – Contribuição: 50%  
  - Tasks: []  
- **José Sousa** (up2022XXXXX) – Contribuição: 50%  
  - Tasks: [] 


## 2. Instalação e Execução

### Prerequisitos

- Instalar **SICStus Prolog 4.9** a partir do [website SICStus](https://sicstus.sics.se/).

### Linux

1. Descarregar o ficheiro zip do projeto.

2. Extrair o ficheiro zip.

3. Abrir o terminal na pasta do projeto.

4. Adicionar a variável de ambiente do SICStus Prolog:
    ```bash
    export PATH=$PATH:<path-de-instalação>/sicstus4.9.0/bin
    ```

5. Correr o SICStus Prolog:
    ```bash
    sicstus
    ```

6. Carregar o ficheiro do jogo:
    ```prolog
    ?- consult('game.pl').
    ```

7. Iniciar o jogo:
    ```prolog
    ?- play.
    ```

### Windows

1. Uhhhh


## 3. Descrição do Jogo

**Nome:** Blackstone

**Breve descrição:** Blackstone é um jogo de tabuleiro para dois jogadores, azul e vermelho, onde o objetivo é eliminar as peças do adversário. Cada movimento deixa para trás uma peça preta, que não pode ser movida ou eliminada. Quando uma peça de algum jogador fica sem movimentos possíveis, essa peça é eliminada. O jogo termina quando um ou ambos os jogadores ficarem sem peças.

**Fontes de regras e informações adicionais:**

- Website oficial: https://www.marksteeregames.com/

- Regras do jogo (PDF): https://www.marksteeregames.com/Blackstone_rules.pdf

## 4. Considerações de Implementação

**Tabuleiros de tamanho variável:** no jogo Blackstone, o tabuleiro pode ter um tamanho variável, sendo que o tamanho tem de ser um número par superior a 2.

**Regras opcionais:** Blackstone oferece duas variantes extra de jogo, Medium Churn e High Churn.
- **Medium Churn:** as peças pretas adjacentes a uma peça azul ou vermelha que seja eliminada são removidas do tabuleiro.
- **High Churn:** quando uma peça azul ou vermelha é eliminada, todas as peças pretas são removidas do tabuleiro.


## 5. Lógica do Jogo
### Representação da configuração do jogo

    [Descreve como a configuração do jogo é feita e como é usada em initial_state/2.]

### Representação do estado interno do jogo

    [Explicar como o estado interno é representado, incluindo o significado dos valores atómicos, e dar exemplos de um estado inicial, intermédio e final.]

### Representação de movimentos

    [Descrever como movimentos são representados, como coordenadas e outra informação usada, e como ele são usados em move/3.]

### Interação com o utilizador

    [Descrever brevemente o sistema de menu e como os inputs do utilizador são tratados, focando na sua validação.]

## 6. Conclusões

Sumário de Trabalho Realizado:

    [Descrição dos objetivos alcançados.]

Limitações:

    [Lista de limitações do projeto.]

Melhorias Futuras:

    [Lista de melhorias que podem ser feitas no futuro.]

### 7. Bibliografia

- [Regras de Blackstone](https://www.marksteeregames.com/Blackstone_rules.pdf)