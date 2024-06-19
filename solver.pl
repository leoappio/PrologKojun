:- use_module(library(clpfd)).



/* Definição do tabuleiro inicial com grupos e posições vazias*/
/*
tabuleiro([[[1,2],[1,_],[2,_],[2,_],[2,1],[3,_]],
            [[4,_],[4,_],[4,_],[4 ,3],[4,_],[3,_]],
            [[5,_],[6,3],[6,_],[6,_],[4,5],[7,3]],
            [[5,_],[5,_],[5,_],[6,_],[7,_],[7,_]],
            [[8,_],[8,_],[10,3],[0,_],[0,4],[0,2]],
            [[9,_],[9,_],[10,_],[10,_],[0,_],[0,_]]]).
*/


tabuleiro([
    [[1,5], [2,_], [2,2], [2,_], [3,2], [3,_], [3,3], [3,1], [4,3], [4,1]],
    [[1,_], [1,4], [1,_], [2,1], [6,_], [6,5], [7,_], [7,5], [4,_], [7,4]],
    [[5,7], [5,5], [1,1], [6,7], [6,_], [8,_], [9,3], [7,1], [7,3], [7,_]],
    [[5,_], [5,4], [6,_], [6,_], [12,4],[8,_], [9,_], [9,_], [9,_], [10,3]],
    [[5,2], [5,_], [5,3], [6,4], [12,3],[12,2],[11,_],[10,_],[10,4],[10,_]],
    [[13,5],[13,6],[15,2],[15,_],[15,6],[12,_],[19,_],[19,_],[18,2],[18,_]],
    [[13,4],[13,1],[13,3],[15,4],[15,1],[16,3],[22,_],[20,4],[18,_],[18,3]],
    [[14,6],[14,7],[13,2],[15,3],[16,_],[16,1],[22,4],[20,_],[23,_],[23,1]],
    [[14,4],[14,2],[17,3],[17,5],[17,4],[17,_],[22,_],[20,2],[20,1],[21,3]],
    [[14,3],[14,1],[14,5],[17,2],[17,1],[17,6],[22,2],[22,1],[21,2],[21,1]]]).


grupo_quantidade(1,5).
grupo_quantidade(2,4).
grupo_quantidade(3,4).
grupo_quantidade(4,3).
grupo_quantidade(5,7).
grupo_quantidade(6,7).
grupo_quantidade(7,6).
grupo_quantidade(8,2).
grupo_quantidade(9,4).
grupo_quantidade(10,4).
grupo_quantidade(11,1).
grupo_quantidade(12,4).
grupo_quantidade(13,6).
grupo_quantidade(14,7).
grupo_quantidade(15,6).
grupo_quantidade(16,3).
grupo_quantidade(17,7).
grupo_quantidade(18,4).
grupo_quantidade(19,2).
grupo_quantidade(20,4).
grupo_quantidade(21,3).
grupo_quantidade(22,5).
grupo_quantidade(23,2).


/* Tamanho de cada grupo (grupo, tamanho) */
/*
grupo_quantidade(0,5).
grupo_quantidade(1,2).
grupo_quantidade(2,3).
grupo_quantidade(3,2).
grupo_quantidade(4,6).
grupo_quantidade(5,4).
grupo_quantidade(6,4).
grupo_quantidade(7,3).
grupo_quantidade(8,2).
grupo_quantidade(9,2).
grupo_quantidade(10,3).
*/


/* Função principal que resolve o Kojun. */
solver(Desafio) :-
    /* Concatena todas as listas do tabuleiro em uma única lista. */
    append(Desafio, Lista),
    /* Define o valor máximo para cada célula com base no grupo (valor definido na função grupo_quantidade). */
    maplist(valor_maximo_grupo, Lista),
    /* Verifica que os valores em cada linha são diferentes dos seus vizinhos. */
    maplist(vizinhos_diferentes, Desafio),
    /* Transpõe o tabuleiro para verificar as colunas. */
    transpose(Desafio, Columns),
    /* Verifica que os valores em cada coluna são diferentes dos seus vizinhos. */
    maplist(vizinhos_diferentes,Columns),
    /* Verifica que o valor superior é maior se estiver no mesmo grupo. */
    maplist(superior_maior, Columns),
    /* Agrupa os valores por grupo. */
    agrupa(0, Lista, Grupo0),
    agrupa(1, Lista, Grupo1),
    agrupa(2, Lista, Grupo2),
    agrupa(3, Lista, Grupo3),
    agrupa(4, Lista, Grupo4),
    agrupa(5, Lista, Grupo5),
    agrupa(6, Lista, Grupo6),
    agrupa(7, Lista, Grupo7),
    agrupa(8, Lista, Grupo8),
    agrupa(9, Lista, Grupo9),
    agrupa(10, Lista, Grupo10),
    /* Cria uma lista de grupos para verificação. */
    Grupos = [Grupo0,Grupo1,Grupo2,Grupo3,Grupo4,Grupo5,Grupo6,Grupo7,Grupo8,Grupo9,Grupo10], 
    /* Verifica que todos os valores em cada grupo são distintos. */
    todos_diferentes_grupo(Grupos), !.


/* Define o valor máximo que os valores de cada grupo podem assumir, com base no tamanho do grupo. */
valor_maximo_grupo([R,X]) :- grupo_quantidade(R,T), X in 1..T.


/* Verifica se o vizinho a direita eh diferente, de forma recursiva */
vizinhos_diferentes([[_,_]]).
vizinhos_diferentes([[_,X1],[R2,X2]|T]) :-
    X1 #\= X2, append([[R2,X2]],T,L), vizinhos_diferentes(L).


/* Verifica se o valor acima de outro eh maior, se fizerem parte do mesmo grupo */
superior_maior([[_,_]]).
superior_maior([[R1,X1],[R2,X2]|T]) :-
    ((R1 #\= R2);
    (X1 #> X2)), append([[R2,X2]],T,L), superior_maior(L).


/* Agrupa os elementos de mesmo grupo (recebe o grupo) */
agrupa(_, [], []).
agrupa(R, [[R1, X1] | T], [X1 | L]) :- R #= R1, agrupa(R, T, L).
agrupa(R, [[R1, _] | T], L) :- R #\= R1, agrupa(R, T, L).


/* Verifica se os membros de uma lista sao diferentes */
todos_diferentes_grupo([H]) :-
    all_distinct(H).
todos_diferentes_grupo([H|T]) :-
    all_distinct(H),
    todos_diferentes_grupo(T).


/* Extrai a solução do tabuleiro, preenchendo os valores não definidos. */
solucao(TabuleiroResposta) :-
    tabuleiro(TabuleiroProblema), 
    solver(TabuleiroProblema),
    extract_second_values(TabuleiroProblema, TabuleiroResposta).


/* Função auxiliar para extrair o segundo elemento de cada par em uma lista. */
extract_second([], []).
extract_second([[_, Second] | Rest], [Second | SecondValues]) :-
    extract_second(Rest, SecondValues).


/* Função auxiliar para extrair os valores de cada linha do tabuleiro. */
extract_second_values([], []).
extract_second_values([Sublist | Rest], [SecondValues | Result]) :-
    extract_second(Sublist, SecondValues),
    extract_second_values(Rest, Result).


% set_prolog_flag(answer_write_options, [max_depth(0)]), solucao(Result).