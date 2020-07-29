      $set sourceformat"free"

      *>Divisão de identificação do programa
       identification division.
       program-id. "exercicio1".
       author. "Anderson Weber Junior".
       installation. "PC".
       date-written. 29/07/2020.
       date-compiled. 29/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.

           select arqTemp assign to "arqTempIndexed.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-dia
           file status is ws-fs-arqTemp.

       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqTemp.
       01 fd-temp.
          05 fd-dia                                pic  9(07).
          05 fd-temperatura                        pic s9(02)v99.

      *>----Variaveis de trabalho
       working-storage section.

       77  ws-fs-arqTemp                           pic  9(02).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).

       01 ws-temp.
          05 ws-dia                                pic  9(07).
          05 ws-temperatura                        pic s9(02)v99.

       77 ws-sair                                  pic  x(01).
          88  fechar-programa                      value "N" "n".
          88  voltar-tela                          value "V" "v".

       77  ws-menu                                 pic  x(02).

      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicialização
      *>------------------------------------------------------------------------

       inicializa section.
      *> open i-o abre o arquivo para leitura/escrita

           open i-o arqTemp

      *>conferindo a estabilidade do programa dps de executar a função de arquivo
           if ws-fs-arqTemp  <> 00
           and ws-fs-arqTemp <> 05 then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqTemp                    to ws-msn-erro-cod
               move "Erro ao abrir arq. arqTemp "    to ws-msn-erro-text
               perform finaliza-anormal
           end-if
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until fechar-programa

               move space to ws-sair
               display "'Ca' cadastrar"
               display "'Ci' consulta indexada"
               display "'Cs' consulta sequencial"
               display "'De' deletar"
               display "'Al' alterar"

               accept ws-menu

               evaluate ws-menu
                   when = "Ca"
                       perform cadastra-temp

                   when = "Ci"
                       perform consultar-temp

                   when = "Cs"
                       perform consultar-temp-sequencial-next

                   when = "De"
                       perform deletar-temp

                    when = "Al"
                       perform alterar-temp

                   when other
                       display "opcao invalida"
               end-evaluate


           end-perform


           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Rotina de consulta de temperatura  - lê o arquivo de forma indexada
      *>------------------------------------------------------------------------
       consultar-temp section.


      *> -------------  Ler dados do arquivo
               display "informe o dia a ser consultado (aaaaddd):"
               accept ws-dia

               move ws-dia to fd-dia
               read arqTemp

      *>conferindo a estabilidade do programa dps de executar a função de arquivo
               if  ws-fs-arqTemp <> 0
               and ws-fs-arqTemp <> 10 then
                   if ws-fs-arqTemp = 23 then
                       display "Data informada invalida!"
                   else
                       move 2                                   to ws-msn-erro-ofsset
                       move ws-fs-arqTemp                       to ws-msn-erro-cod
                       move "Erro ao ler arq. arqTemp "         to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

               move  fd-temp       to  ws-temp

      *>------------------------------------------------------------------------

               display "dia: "  ws-dia

               display "temperatura: "  ws-temperatura

           .
       consultar-temp-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Rotina de consulta de temperatura  - lê o arquivo de forma sequencial
      *>------------------------------------------------------------------------

       consultar-temp-sequencial-next section.

           perform consultar-temp

           perform until voltar-tela

               read arqTemp next
      *>conferindo a estabilidade do programa dps de executar a função de arquivo
               if  ws-fs-arqTemp <> 0  then
                  if ws-fs-arqTemp = 10 then
                      perform consultar-temp-sequencial-prev
                  else
                      move 3                                   to ws-msn-erro-ofsset
                      move ws-fs-arqTemp                       to ws-msn-erro-cod
                      move "Erro ao ler arq. arqTemp "         to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-temp       to  ws-temp

      *> ------------------------------------------------------------------------
               display "dia: "  ws-dia

               display "temperatura: "  ws-temperatura


               display "Deseja consultar mais um dia? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform


           .
       consultar-temp-seq-next-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Rotina de consulta de temperatura  - lê o arquivo de forma sequencial
      *>------------------------------------------------------------------------
       consultar-temp-sequencial-prev section.


           perform until voltar-tela

               read arqTemp previous
      *>conferindo a estabilidade do programa dps de executar a função de arquivo
               if  ws-fs-arqTemp <> 0  then
                  if ws-fs-arqTemp = 10 then
                      perform consultar-temp-sequencial-next
                  else
                      move 4                                   to ws-msn-erro-ofsset
                      move ws-fs-arqTemp                       to ws-msn-erro-cod
                      move "Erro ao ler arq. arqTemp "         to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-temp       to  ws-temp

      *> ------------------------------------------------------------------------
               display "dia: "  ws-dia

               display "temperatura: "  ws-temperatura

               display "Deseja consultar mais um dia? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform
           .
       consultar-temp-seq-prev-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Rotina de apagamento / Delete
      *>------------------------------------------------------------------------
       deletar-temp section.


      *> -------------  Apagar dados do registro do arquivo
               display "informe o dia a ser excluido (aaaaddd):"
               accept ws-dia

               move ws-dia to fd-dia
               delete arqTemp
      *>conferindo a estabilidade do programa dps de executar a função de arquivo
               if  ws-fs-arqTemp = 0 then
                   display "Temperatura do dia " ws-dia " apagada com sucesso!"
               else
                   if ws-fs-arqTemp = 23 then
                       display "Data informada invalida!"
                   else
                       move 5                                   to ws-msn-erro-ofsset
                       move ws-fs-arqTemp                       to ws-msn-erro-cod
                       move "Erro ao apagar arq. arqTemp "      to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if
           .
       deletar-temp-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Rotina de alteração de temperatura
      *>------------------------------------------------------------------------
       alterar-temp section.

               perform consultar-temp

      *> -------------  Alterar dados do registro do arquivo ------------------
               display "Informe uma nova temperatura:"
               accept ws-temperatura

               move ws-temperatura to fd-temperatura
               rewrite fd-temp
      *>conferindo a estabilidade do programa dps de executar a função de arquivo
               if  ws-fs-arqTemp = 0 then
                   display "Temperatura do dia " ws-dia " alterada com sucesso!"
               else
                   move 6                                   to ws-msn-erro-ofsset
                   move ws-fs-arqTemp                       to ws-msn-erro-cod
                   move "Erro ao alterar arq. arqTemp "     to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           .
       alterar-temp-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Rotina de cadastro de temperatura  - escreve no arquivo
      *>------------------------------------------------------------------------
       cadastra-temp section.

           perform until voltar-tela

               display "dia: "
               accept  ws-dia

               display "temperatura: "
               accept ws-temperatura

      *> -------------  Salvar dados no arquivo------------------------------------

               write fd-temp       from ws-temp
               if ws-fs-arqTemp <> 0 then
                   move 7                                   to ws-msn-erro-ofsset
                   move ws-fs-arqTemp                       to ws-msn-erro-cod
                   move "Erro ao escrever arq. arqTemp "    to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
      *> -------------------------------------------------------------------------

               display "Deseja cadastrar mais um dia? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform
           .
       cadastra-temp-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqTemp
           if ws-fs-arqTemp <> 0 then
               move 8                                to ws-msn-erro-ofsset
               move ws-fs-arqTemp                    to ws-msn-erro-cod
               move "Erro ao fechar arq. arqTemp "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           Stop run
           .
       finaliza-exit.
           exit.
