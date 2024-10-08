% Definindo os blocos
block(a).
block(b).
block(c).
block(d).

% Definindo o tamanho de cada bloco
size(a,1).
size(b,1).
size(c,2).
size(d,3).

% Definindo os lugares na mesa que podem estar livres ou podem ter algum bloco em cima
place(1).
place(2).
place(3).
place(4).
place(5).
place(6).   

% Posicionamento inicial dos blocos descrito para o trabalho
%              d d d
%    	 c c   a   b
%    	 -----------
% place  1 2 3 4 5 6

% Descrição do estado inicial
state1([on(c,p([1,2])),on(a,p([4,4])),on(b,p([6,6])),on(d,p([a,b])),clear(c),clear(d),clear(3),clear(5)]).
         	 
% Possíveis movimentações:
% 1. Bloco sobre a mesa que pode ser movido para cima de outro bloco
can(move(Block,p([Xi,Yi]),To),[clear(Block),clear(To),on(Block,p([Xi,Yi]))]):-
	block(Block),       % Verifica Bloco	
	block(To),	    
	place(Xi),	    % Verifica se é um lugar
	place(Yi),
	Block \== To,	     % Bloco e destino devem ser diferentes
    
	size(Block, S_b),
	size(To, S_t),
	SizeD is S_b - S_t,
	SizeD =< 2.	       % Garante que a diferença de tamanho seja menor ou igual a 2

can(move(Block, From, To), [clear(Block), on(Block, From)]):-
	block(Block),       
	block(To),        
	To \== Block,      	 
	block(From),       	
	From \== To,        	
	Block \== From,     	
    
	size(Block, S_b),
	size(To, S_t),
	count_blocks_on_top(From, Count), % Quantos blocos estão em cima da origem
	Count =< S_b,  % Quantidade de blocos acima da origem seja menor que o bloco
    
	SizeD is S_b - S_t,
	SizeD =< 2.

% 2. Bloco sobre outro bloco que será movimentado para cima de um bloco
can(move(Block,From,To),[clear(Block),clear(To),on(Block,From)]):-
	block(Block),       	
	block(To),         	
	To \== Block,       	
	block(From),       	
	From \== To,        	
	Block \== From,     	

    size(Block, S_b),
	size(To, S_t),
	SizeD is S_b - S_t,
	SizeD =< 2.

% 3. Bloco sobre um bloco que será movimentado para um local livre na mesa
can(move(Block,From,p([Xi,Yi])),[clear(Block),clear(Xi),clear(Yi),on(Block,From)]):-
	block(Block),       	
	block(From),
	place(Xi),
	place(Yi),
	Block \== From,
    
	size(Block, S_b),
	Sizep is (Yi - Xi)+1, % Calcula tamanho da mesa
	S_b =< Sizep.     % Garante que o tamanho do bloco seja menor ou igual ao espaço
    % disponível na mesa

% 4. Bloco sobre a mesa que será movimentado para um outro local livre na mesa
can(move(Block,p([Xi,Yi]),p([Xj,Yj])),[clear(Block),clear(Xj),clear(Yj),on(Block,p([Xi,Yi]))]):-
	block(Block),       	
	place(Xj), % Garante que todos os espaços sejam válidos
	place(Yj),
	place(Xi),
	place(Yi),
	Xi \== Xj,
	Yi \== Yj,
    
	size(Block, S_b),
	Sizep is (Yj - Xj)+1, % Calcula tamanho da mesa
	S_b =< Sizep. % Garante que o tamanho do bloco seja menor ou igual ao espaço
    			% disponível na mesa

% 5. Bloco maior sobre dois blocos que será movimentado para um espaço na mesa
can(move(Block,p([Xi,Yi]),p([Xj,Yj])),[clear(Block),clear(Xj),clear(Yj),on(Block,p([Xi,Yi]))]):-
	block(Block),       	
	place(Xj), % Garante que todos os espaços sejam válidos
	place(Yj),
	block(Xi),
	block(Yi),
	Xi \== Yi,
    
	Xi \== Xj, % As posições devem ser distintas
	Yi \== Yj,
    
	size(Block, S_b),
	Sizep is (Yj - Xj)+1, % Calcula tamanho da mesa
	S_b =< Sizep. % Garante que o tamanho do bloco seja menor ou igual ao espaço
    			% disponível na mesa

% 6. Bloco maior que está em cima de dois blocos e será movimentado para cima de outros % dois blocos
can(move(Block,p([Xi,Yi]),p([Xj,Yj])),[clear(Block),clear(Xj),clear(Yj),on(Block,p([Xi,Yi]))]):-
	block(Block),       	
	block(Xj), % Garante que todos os espaços sejam válidos
	block(Yj),
	block(Xi),
	block(Yi),


	% A posição inicial não pode ser igual a posição final
	Xi \== Yi,
	Xi \== Xj,
	Yi \== Yj,
	Xj \== Yj,

	% Um bloco não pode ser igual a uma das posições
	Block \== Xi,
	Block \== Yi,
	Block \== Xj,
	Block \== Yj.

% 7. Bloco sobre a mesa que será colocado em cima de outros dois blocos
can(move(Block,p([Xi,Yi]),p([Xj,Yj])),[clear(Block),clear(Xj),clear(Yj),on(Block,p([Xi,Yi]))]):-
	block(Block),       	
	block(Xj),
	block(Yj),
	place(Xi),
	place(Yi),

	% As posições devem ser distintas entre si
	Xi \== Yi,
	Xj \== Yj,

	% O bloco não pode ser igual a nenhuma das posições
	Block \== Xj,
	Block \== Yj.

% Predicado para contar o número de blocos sobre uma posição
count_blocks_on_top(Place, Count):-
	findall(Block, (state1(S), member(on(Block, Place), S)), BlocksOnTop),
	length(BlocksOnTop, Count).

% adds(Action, Relationships): “action” adiciona novas relações
adds(move(X,From,p([Xi,Yi])),[on(X,p([Xi,Yi])),clear(From)]).
adds(move(X,p([Xi,Yi]),To),[on(X,To),clear(Xi),clear(Yi)]).
adds(move(X,p([Xi,Yi]),p([Xj,Yj])),[on(X,p([Xj,Yj])),clear(Xi),clear(Yi)]).
adds(move(X,From,To),[on(X,To),clear(To),clear(From)]).
adds(move(X,From,To),[on(X,To),clear(From)]).
adds(move(X,p([Xi,Yi]),p([Xj,Yj])),[on(X,p([Xj,Yj])),clear(Xi),clear(Yi)]).

% deletes(Action, Relationships): “action” exclui relações
deletes(move(X,From,p([Xi,Yi])),[on(X,From),clear(Xi),clear(Yi)]).
deletes(move(X,p([Xi,Yi]),To),[on(X,p([Xi,Yi])),clear(To)]).
deletes(move(X,p([Xi,Yi]),p([Xj,Yj])),[on(X,p([Xi,Yi])),clear(Xj),clear(Yj)]).
deletes(move(X,From,To),[on(X,From),clear(To)]).
deletes(move(X,From,_),[on(X,From)]).

deletes(move(X,p([Xi,Yi]),p([Xj,Yj])),[on(X,p([Xi,Yi])),clear(Xj),clear(Yj)]).

object(X):-
	place(X)
	;
	block(X).

% Bloco em cima de si mesmo é impossível
impossible(on(X,X),_).

impossible(on(X,Y),Goals):-
	% Se Y não está livre, é impossível colocar X em cima de Y
	member(clear(Y),Goals)
	;
	member(on(X,Y1),Goals), Y1 \== Y   % Um bloco não pode estar em 2 lugares
	;
	member(on(X1,Y), Goals), X1 \== X. % 2 blocos não podem ocupar o mesmo lugar
impossible(clear(X),Goals):-
	% Se outro bloco está em cima de X é impossível ele estar livre
	member(on(_,X),Goals).

% Objetivos satisfeitos
plan( State, Goals, []):-
  satisfied( State, Goals).               	

% Planejamento do próximo passo utilizando recursão
plan( State, Goals, Plan):-

% Divisão do plano de ação e o restante
  append( PrePlan, [Action], Plan),       	
  select( State, Goals, Goal),         % Escolha do objetivo   	
  achieves( Action, Goal),
  can( Action, _),                % Obtém condições     	
  preserves(Action,Goals),                	
  regress( Goals, Action, RegressedGoals),	
  plan( State, RegressedGoals, PrePlan).

% Verifica se os objetivos foram satisfeitos
satisfied(_, []).
satisfied( State, Goals)  :-
  delete_all( Goals, State, []).          	
satisfied(State, [Goal|Goals]):-
  member(Goal, State),
  satisfied(State, Goals).

select(_, Goals, Goal)  :-           	
  member( Goal, Goals).                   

% Verifica se uma ação alcança um objetivo
achieves( Action, Goal)  :-
  adds( Action, Goals),
  member( Goal, Goals).

% Verifica se uma ação preserva os objetivos restantes
preserves( Action, Goals)  :-             	
  deletes( Action, Relations),
  \+  (member( Goal, Relations),          	
   	member( Goal, Goals) ).
preserves(Action, Goals):-
  deletes(Action, Relations),
  member(Goal, Relations),
  not_member(Goal,Goals).
  not_member(_, []) :- !.
% Realiza a regressão dos objetivos após execução
regress( Goals, Action, RegressedGoals)  :-   	
  adds( Action, NewRelations),
  delete_all( Goals, NewRelations, RestGoals),
  can( Action, Condition),
  addnew( Condition, RestGoals, RegressedGoals). 

% Adiciona novos objetivos à lista
addnew( [], L, L).

addnew( [Goal | _], Goals, _)  :-
  impossible( Goal, Goals),     	
  !,
  fail.                         	% Não foi possível adicionar

addnew( [X | L1], L2, L3)  :-
  member( X, L2),  !,           	
  addnew( L1, L2, L3).

addnew( [X | L1], L2, [X | L3])  :-
  addnew( L1, L2, L3).

% Deleta todos os elementos de uma lista de outra
delete_all( [], _, []).

delete_all( [X | L1], L2, Diff)  :-
  member( X, L2), !,
  delete_all( L1, L2, Diff).

delete_all( [X | L1], L2, [X | Diff])  :-
  delete_all( L1, L2, Diff).

% Verifica se um elemento é membro de uma lista
member(X,[X|_]).
member(X,[_|T]):-
 	member(X,T).

% Deleta um elemento de uma lista
delete(X,[X|Tail],Tail).
delete(X,[Y|Tail],[Y|Tail1]):-
   delete(X, Tail,Tail1).
