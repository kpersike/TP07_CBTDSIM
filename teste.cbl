       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP07_CBTDSIM.
       AUTHOR. Kaik Persike.
       DATE-WRITTEN. 2026.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-FLAG-SAIR              PIC X(01) VALUE 'N'.
           88 WSSAIR                 VALUE 'S', 's'.

       01  WS-OPCAO                  PIC 9(01) VALUE 0.
       01  WS-IND                    PIC 9(02) VALUE 0.
       01  WS-ACHOU                  PIC X(01) VALUE 'N'.
       01  WS-BUSCA-COD              PIC X(05) VALUE SPACES.
       01  WS-CONFIRMA               PIC X(01) VALUE SPACES.

      * Tabela de Clientes
       01  WS-TAB-CLIENTES.
           05  WS-CLIENTE OCCURS 10 TIMES
                   ASCENDING KEY IS WS-CLI-CODIGO
                   INDEXED BY WS-IND-CLI.
               10  WS-CLI-CODIGO     PIC X(05).
               10  WS-CLI-NOME       PIC X(50).
               10  WS-CLI-CONTA      PIC X(08).
               10  WS-CLI-VALOR      PIC 9(07)V99.
               10  WS-CLI-PARCELAS   PIC 9(05).

       01  WS-CARGA-INICIAL.
           05  FILLER                PIC X(70) VALUE
               '00001Cliente Padrao 01                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00002Cliente Padrao 02                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00003Cliente Padrao 03                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00004Cliente Padrao 04                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00005Cliente Padrao 05                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00006Cliente Padrao 06                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00007Cliente Padrao 07                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00008Cliente Padrao 08                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00009Cliente Padrao 09                0000000100000012'.
           05  FILLER                PIC X(70) VALUE
               '00010Cliente Padrao 10                0000000100000012'.

       01  WS-TAB-REDEFINES REDEFINES WS-CARGA-INICIAL.
           05  WS-REG-INI OCCURS 10 TIMES.
               10  WS-INI-CODIGO     PIC X(05).
               10  WS-INI-NOME       PIC X(50).
               10  WS-INI-CONTA      PIC X(08).
               10  WS-INI-VALOR      PIC 9(07)V99.
               10  WS-INI-PARCELAS   PIC 9(05).

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PRINCIPAL UNTIL WSSAIR.
           STOP RUN.

       0100-INICIALIZAR.

           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 10
               MOVE WS-INI-CODIGO(WS-IND) TO WS-CLI-CODIGO(WS-IND)
               MOVE WS-INI-NOME(WS-IND)   TO WS-CLI-NOME(WS-IND)
               MOVE WS-INI-CONTA(WS-IND)  TO WS-CLI-CONTA(WS-IND)
               MOVE WS-INI-VALOR(WS-IND)  TO WS-CLI-VALOR(WS-IND)
               MOVE WS-INI-PARCELAS(WS-IND) TO WS-CLI-PARCELAS(WS-IND)
           END-PERFORM.

       0200-PRINCIPAL.
           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "      SISTEMA DE GESTAO - BANCO         ".
           DISPLAY "========================================".
           DISPLAY " 1 - Cadastrar os clientes              ".
           DISPLAY " 2 - Alterar emprestimo e parcelas      ".
           DISPLAY " 3 - Listar todos os clientes           ".
           DISPLAY " 4 - Consultar cliente específico       ".
           DISPLAY " 5 - Encerrar o programa                ".
           DISPLAY "========================================".
           DISPLAY "Digite sua opcao: " WITH NO ADVANCING.
           ACCEPT WS-OPCAO.

           EVALUATE WS-OPCAO
               WHEN 1
                   PERFORM 1000-CADASTRAR
               WHEN 2
                   PERFORM 2000-ALTERAR
               WHEN 3
                   PERFORM 3000-LISTAR
               WHEN 4
                   PERFORM 4000-CONSULTAR
               WHEN 5
                   MOVE 'S' TO WS-FLAG-SAIR
                   DISPLAY "Encerrando o programa..."
               WHEN OTHER
                   DISPLAY "Opcao invalida! Pressione ENTER."
                   ACCEPT WS-CONFIRMA
           END-EVALUATE.

      *-----------------------------------------------------------------
      * 1 - CADASTRAR OS CLIENTES
      *-----------------------------------------------------------------
       1000-CADASTRAR.
           DISPLAY "--- CADASTRO DE CLIENTES (1 a 10) ---".
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 10
               DISPLAY "-----------------------------------"
               DISPLAY "Cliente posicao: " WS-IND
               DISPLAY "Codigo atual (5 char): " WS-CLI-CODIGO(WS-IND)
               DISPLAY "Nome atual: " WS-CLI-NOME(WS-IND)

               DISPLAY "Deseja alterar este registro? (S/N): "
                   WITH NO ADVANCING
               ACCEPT WS-CONFIRMA

               IF WS-CONFIRMA = 'S' OR WS-CONFIRMA = 's'
                   DISPLAY "Novo Codigo (5 caracteres): "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-CODIGO(WS-IND)
                   DISPLAY "Novo Nome (ate 50 caracteres): "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-NOME(WS-IND)
                   DISPLAY "Nova Conta (8 caracteres): "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-CONTA(WS-IND)
                   DISPLAY "Novo Valor Emprestimo (ex: 0010000,00): "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-VALOR(WS-IND)
                   DISPLAY "Novo Num. Max. Parcelas (ex: 00012): "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-PARCELAS(WS-IND)
               END-IF
           END-PERFORM.
           DISPLAY "Cadastro finalizado. Pressione ENTER.".
           ACCEPT WS-CONFIRMA.

      *-----------------------------------------------------------------
      * 2 - ALTERAR EMPRÉSTIMO E PARCELAS USANDO SEARCH
      *-----------------------------------------------------------------
       2000-ALTERAR.
           DISPLAY "--- ALTERAR EMPRESTIMO / PARCELAS ---".
           DISPLAY "Digite o Codigo do Cliente (5 char): "
               WITH NO ADVANCING.
           ACCEPT WS-BUSCA-COD.

           MOVE 'N' TO WS-ACHOU.
           SET WS-IND-CLI TO 1.

           SEARCH WS-CLIENTE
               AT END
                   DISPLAY "Cliente nao encontrado na tabela!"
               WHEN WS-CLI-CODIGO(WS-IND-CLI) = WS-BUSCA-COD
                   MOVE 'S' TO WS-ACHOU
                   DISPLAY
                   "Cliente Encontrado: " WS-CLI-NOME(WS-IND-CLI)
                   DISPLAY
                   "Conta: " WS-CLI-CONTA(WS-IND-CLI)
                   DISPLAY
                   "Valor Atual: " WS-CLI-VALOR(WS-IND-CLI)
                   DISPLAY
                   "Parcelas Atuais: " WS-CLI-PARCELAS(WS-IND-CLI)

                   DISPLAY "Digite o NOVO Valor do Emprestimo: "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-VALOR(WS-IND-CLI)
                   DISPLAY "Digite o NOVO Numero de Parcelas: "
                       WITH NO ADVANCING
                   ACCEPT WS-CLI-PARCELAS(WS-IND-CLI)
                   DISPLAY "Dados alterados com sucesso!"
           END-SEARCH.

           DISPLAY "Pressione ENTER para continuar.".
           ACCEPT WS-CONFIRMA.

      *-----------------------------------------------------------------
      * 3 - LISTAR TODOS OS CLIENTES (1 A 1, PAUSANDO)
      *-----------------------------------------------------------------
       3000-LISTAR.
           DISPLAY "--- LISTAGEM DE CLIENTES ---".
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 10
               DISPLAY "----------------------------------------"
               DISPLAY "Posicao na Tabela : " WS-IND
               DISPLAY "Codigo do Cliente : " WS-CLI-CODIGO(WS-IND)
               DISPLAY "Nome do Cliente   : " WS-CLI-NOME(WS-IND)
               DISPLAY "Numero da Conta   : " WS-CLI-CONTA(WS-IND)
               DISPLAY "Valor Emprestimo  : " WS-CLI-VALOR(WS-IND)
               DISPLAY "Max. Parcelas     : " WS-CLI-PARCELAS(WS-IND)
               DISPLAY "----------------------------------------"
               DISPLAY "Pressione ENTER para o proximo cliente..."
               ACCEPT WS-CONFIRMA
           END-PERFORM.
           DISPLAY "Fim da listagem. Pressione ENTER.".
           ACCEPT WS-CONFIRMA.

      *-----------------------------------------------------------------
      * 4 - CONSULTAR DADOS DE DETERMINADO CLIENTE USANDO SEARCH
      *-----------------------------------------------------------------
       4000-CONSULTAR.
           DISPLAY "--- CONSULTAR CLIENTE ESPECIFICO ---".
           DISPLAY "Digite o Codigo do Cliente (5 char): "
               WITH NO ADVANCING.
           ACCEPT WS-BUSCA-COD.

           MOVE 'N' TO WS-ACHOU.
           SET WS-IND-CLI TO 1.

           SEARCH WS-CLIENTE
               AT END
                   DISPLAY "Cliente nao encontrado!"
               WHEN WS-CLI-CODIGO(WS-IND-CLI) = WS-BUSCA-COD
                   MOVE 'S' TO WS-ACHOU
                   DISPLAY "----------------------------------------"
                   DISPLAY "DADOS DO CLIENTE ENCONTRADO:"
                   DISPLAY
                   "Codigo            : " WS-CLI-CODIGO(WS-IND-CLI)
                   DISPLAY
                   "Nome              : " WS-CLI-NOME(WS-IND-CLI)
                   DISPLAY
                   "Conta             : " WS-CLI-CONTA(WS-IND-CLI)
                   DISPLAY
                   "Valor Emprestimo  : " WS-CLI-VALOR(WS-IND-CLI)
                   DISPLAY
                   "Max. Parcelas     : " WS-CLI-PARCELAS(WS-IND-CLI)
                   DISPLAY "----------------------------------------"
           END-SEARCH.

           DISPLAY "Pressione ENTER para continuar.".
           ACCEPT WS-CONFIRMA.
