% *****************************
% Résolution de Sudoku assistée
% @lang: SWI Prolog
% @author: L.-O. Pochon
% @version: 14.07.2024
% ****************************** 
% :- debug.
% working_directory(O,"/Users/irpochon/Documents/SWI-Prolog/Sudoku").

:- use_module(library(apply)).
% :- use_module(library(clpfd)). % pour utiliser transpose
:- dynamic sudoku/2.
:- dynamic last_sudoku/1.  
:- dynamic posSud/6. % no sudoku, no d'ordre, ligne, colonne, case, valeur
:- dynamic matrix/4.

/* Content:
** 1. Exemples
** 2. Transformations
** 3. Controle
** 4. Placements et nettoyage
** 5. Par essais
** 6. Recherche de motifs
** 7. Utilitaires sur les grilles
** 8. Utilitaires sur des listes
** 9. Utilitaires sur les matrices
** 10. Interface
*/

% ***********
% 1. Exemples
% ***********
% 21 (***)
sudoku(1,[0, 0, 0, 0, 7, 0,  0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 9, 3, 0, 3, 6, 0, 0, 5, 0, 0, 0, 5, 0, 0,  0, 0, 3, 0, 7, 0, 0, 0, 3, 0, 8, 0, 9, 0, 0, 0, 7, 0, 4, 0, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 6, 5, 0, 2, 9, 0, 0, 0, 0, 8, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 0]).
% Solution de 1
sudoku(101,[8,5,4,3,7,9,1,6,2,7,2,1,8,4,6,5,9,3,9,3,6,2,1,5,7,4,8,
          5,8,2,6,9,3,4,7,1,6,4,3,1,8,7,9,2,5,1,7,9,4,5,2,3,8,6,
	  3,1,8,7,2,4,6,5,9,2,9,7,5,6,1,8,3,4,4,6,5,9,3,8,2,1,7]).
% 22 (***)
sudoku(2,[7,8,4,0,0,0,3,0,0,0,0,0,0,6,3,0,8,0,2,0,0,0,0,4,0,0,0,
          0,7,0,0,0,0,0,0,1,0,0,0,9,7,2,0,0,0,4,0,0,0,0,0,0,9,0,
	  0,0,0,5,0,0,0,0,8,0,6,0,3,4,0,0,0,0,0,0,2,0,0,0,5,4,9]).
% 13 (**)
sudoku(3,[8,0,7,0,0,9,5,0,0,6,0,0,2,7,0,0,0,0,0,2,9,0,8,0,0,0,0,
          0,4,2,0,0,0,0,6,0,0,3,0,0,5,0,0,1,0,0,8,0,0,0,0,2,9,0,
          0,0,0,0,6,0,7,2,0,0,0,0,0,9,8,0,0,1,0,0,5,7,0,0,4,0,9]).
% 20 minutes (Facile)
sudoku(4,[7,9,0,0,2,0,0,0,5,0,0,3,4,0,0,0,0,9,0,0,0,9,0,6,0,1,0,
          0,0,4,0,6,0,8,9,0,1,0,0,8,0,4,0,0,2,0,2,6,0,5,0,1,0,0,
          0,5,0,2,0,1,0,0,0,9,0,0,0,0,7,4,0,0,4,0,0,0,9,0,0,8,1]).
% 20 minutes (Difficile)
sudoku(5,[0,0,4,0,7,0,0,0,0,3,2,0,0,0,0,0,0,0,0,8,0,9,0,0,6,0,0,
          8,0,0,7,0,0,1,0,2,0,0,7,0,1,0,8,0,0,5,0,6,0,0,3,0,0,7,
          0,0,3,0,0,4,0,1,0,0,0,0,0,0,0,0,6,9,0,0,0,0,3,0,5,0,0]).
% solution: m 5 P* E E0 S* E0 P* P* P* P* E E0 S* E0 P* P* S* E0 6xP* V

% Sudoku très difficile (selon J-M. Bornoz) ??
sudoku(10,[1,0,0,0,0,7,0,9,0,0,3,0,0,2,0,0,0,8,0,0,9,6,0,0,5,0,0,
          0,0,5,3,0,0,9,0,0,0,1,0,0,8,0,0,0,2,6,0,0,0,0,4,0,0,0,
          3,0,0,0,0,0,0,1,0,0,4,0,0,0,0,0,0,7,0,0,7,0,0,0,3,0,0]).
% p 67 2 p 68 5 p 69 3    Puis D bug

% sudoku(101,S),afficheSudoku(S),transfMod(S,2,ST),nl,nl,afficheSudoku(ST).
  
% ******************
% 2. Transformations
% ******************  

% ** Modulo
transfMod(S,M,ST) :-
  transfMod0(S,M,ST).
  
transfMod0([],_,[]) :- !.
transfMod0([E|S],M,[ET|ST]) :- 
  ET is ((E+M-1) mod 9)+1,
  transfMod0(S,M,ST).

% ** permutation générale [1,2,3,4,5,6,7,8,9] -> PERM [a1,a2,... a9]
transfPerm(S,PERM,ST) :- 
  transfPerm0(S,PERM,ST). 

transfPerm0([],_,[]) :- !. 
transfPerm0([E|S],PERM,[E1|S1]) :-  
  nth1(E,PERM,E1),
  transfPerm0(S,PERM,S1).  

% ** Permutation des lignes (possible dans des paquets de 3)
transfPL(S,L1,L2,ST) :-
  lignes(S,L),
  nth1(L1,L,E,R),
  nth1(L2,PL,E,R),
  linToSudoku(PL,ST).

% ** Permutation des colonnes (possible dans des paquets de 3)
transfPC(S,L1,L2,ST) :-
  colonnes(S,L),
  nth1(L1,L,E,R),
  nth1(L2,LT,E,R),
  colToSudoku(LT,ST).

% ** Symetrie Verticale
transfSV(S,ST) :-
  colonnes(S,L),
  reverse(L,SL),
  colToSudoku(SL,ST).

% Symetrie Horizontale
transfSH(S,SH) :-
  lignes(S,L),
  reverse(L,SL),
  linToSudoku(SL,SH).

% Echange ligne colonne
transfLC(S,S1) :-
  lignes(S,L),
  colToSudoku(L,S1).
/*
transfLC(S,S1) :-
  transpose(S,S1).  
*/
% ***********
% 3. Controle
% ***********

% Sudoku OK
sudOK(S) :-
  colonnes(S,C),
  lignes(S,L),
  cases(S,Q),
  maplist(is_set,C),
  maplist(is_set,L),
  maplist(is_set,Q).

positionVide(N,LPOS) :-
  sudoku(N,S),
  findall(POS,nth1(POS,S,0),LPOS).

simpleSudoku([]) :- !. 
simpleSudoku([E|RE]) :- 
  not(is_list(E)),
  simpleSudoku(RE).  

% *************
% 4. Placements
% *************

% ** Tous les cas possibles 
% V valeur à placer, N nombre de placements
placementVal(V,S,S1,N) :- % (cmd PV)
  % Passer en revue les emplacements vides (0)
  % a partir de la position vérifier état ligne, colonne, case
  % passer par coordonnées puis valLigne, valColonne, valCase
  coord(LCOOR),
  seqSudoku(S,L,C,Q),
  placementVal0(LCOOR,S,L,C,Q,V,S1,0,N).
 
placementVal0([],_,_,_,_,_,[],N,N) :- !. % PV
placementVal0([COOR|RCOOR],[0|RS],L,C,Q,V,[V|RS1],NI,NO) :- 
  valLigne(COOR,L,VL),
  not(member(V,VL)),
  valColonne(COOR,C,CL),
  not(member(V,CL)),
  valCase(COOR,Q,QL),
  not(member(V,QL)),!,
  NI1 is NI+1,
  placementVal0(RCOOR,RS,L,C,Q,V,RS1,NI1,NO).
placementVal0([_|RCOOR],[E|RS],L,C,Q,V,[E|RS1],NI,NO) :- !,
  placementVal0(RCOOR,RS,L,C,Q,V,RS1,NI,NO).

%*** fin placementVal

% ** Placement dans une case si seule possible dans la case (cmd: Pv) 
% V valeur à placer; NCR nombre de placements 
placement(V,S,SC,NCR) :- % Valeur, Sudoku av, sudoku ap, nb cases modifées
  seqSudoku(S,L,C,Q),
  placement0(V,L,C,1,Q,QC,0,NCR),
  casToSudoku(QC,SC). 

placement0(_,_,_,_,[],[],NCR,NCR) :- !. % passage en revue des cases
placement0(V,L,C,N,[QN|RQ],[QN|RQC],NCR0,NCR) :- 
  member(V,QN),!,
  N1 is N+1,
  placement0(V,L,C,N1,RQ,RQC,NCR0,NCR).
placement0(V,L,C,N,[QN|RQ],[QN1|RQC],NCR0,NCR) :-
  remplacement(V,L,C,N,QN,QN1,NCR0,NCR1),  
  N1 is N+1,
  placement0(V,L,C,N1,RQ,RQC,NCR1,NCR).
  
% placement de V dans Nième case QN, si un seul on retourne la case complétée
remplacement(V,L,C,N,QN,QN1,NCR0,NCR) :-   
  remplacement0(V,L,C,N,1,0,NR,QN,QN0),   
  remplacement01(NR,QN,QN0,QN1,NCR0,NCR).   
  
remplacement01(1,_,QN0,QN0,NCR0,NCR) :- !,
  NCR is NCR0+1.   
remplacement01(_,QN,_,QN,NCR,NCR).   

remplacement0(_,_,_,_,_,NR,NR,[],[]) :- !.   
remplacement0(V,L,C,N,M,NR,NR1,[0|RQN],[V|RQN0]) :-    
  linColCasePos(N,M,Li,Col),
  nth1(Li,L,LL),
  not(member(V,LL)),
  nth1(Col,C,CC),
  not(member(V,CC)),!,
  M1 is M+1,
  NR0 is NR+1,
  remplacement0(V,L,C,N,M1,NR0,NR1,RQN,RQN0).    

remplacement0(V,L,C,N,M,NR,NR1,[E|RQN],[E|RQN0]) :-    
  M1 is M+1,
  remplacement0(V,L,C,N,M1,NR,NR1,RQN,RQN0).   

%*** fin placement

% ** Remplacement cas Simon

/*
% Placement d'une valeur puis examen des lignes et colonne avec 
% essai de résolution  
% Exemples avec V = 1    
% 2e case 3e ligne : un 1 s'y trouve forcément 
% 1e case 3e ligne 1e et 3e position à remettre 0 

% X X X  X X X   0 0 1
% 1 1 8  4 5 6   3 2 5
% 1 9 1  1 1 1   6 0 0  
*/

% ** Selon ligne puis colonne (ne semple pas efficace ??)
complSimon(S,SES1) :- % S sudoku à compléter (cmd S*)
  % pour chaque valeur V (dans l'ordre)
  % compléter S par V 
  % Si V est sur une seule ligne dans case òter V dans le reste de la ligne 
  % idem pour colonne 
  % accumuler dans un Sudoku multiple
  complSimon0(S,1,S,SES0),  % lignes
  % write("ligne ok"),nl,
  complSimon1(S,1,SES0,SES1). % colonnes

% ** Selon lignes
complSimonL(S,SES1) :- % S sudoku à compléter (cmd: Sl*)
  % pour chaque valeur V (dans l'ordre)
  % compléter S par V 
  % Si V est sur une seule ligne dans case òter V dans le reste de la ligne 
  % accumuler dans un Sudoku multiple
  complSimon0(S,1,S,SES1).  % lignes

% ** Selon colonnes
complSimonC(S,SES1) :- % S sudoku à compléter (cmd: Sc*)
  % pour chaque valeur V (dans l'ordre)
  % compléter S par V 
  % Si V est sur une seule colonne dans case òter V dans le reste de la col 
  % accumuler dans un Sudoku multiple
  complSimon1(S,1,S,SES1). % colonnes

complSimon0(_,10,SES,SES) :- !. 
complSimon0(S,V,SES,SES1) :-
  placementVal(V,S,S1,_),
  rempSimon(V,S1,S2),
  % write("Val "),write(V),write("ok"),nl,
  % afficheSudoku(S1),nl,
  % afficheSudoku(SES),nl,
  concatSudoku(S2,SES,SES0),
  % write("concat ok"),nl,
  V1 is V+1,
  complSimon0(S,V1,SES0,SES1).

complSimon1(_,10,SES,SES) :- !. 
complSimon1(S,V,SES,SES1) :-
  placementVal(V,S,S1,_),
%  rempSimon(V,S1,S2),
  rempSimonC(V,S1,S2),
  concatSudoku(S2,SES,SES0),
  % afficheSudoku(SES0),nl,
  V1 is V+1,
  complSimon1(S,V1,SES0,SES1).

% Valeurs retirées par ligne
rempSimon(VAL,S,S1) :- % Sudoku avec VAL multiple et 0 (Sl)
  seqSudoku(S,L,_,Q),
  caseSp(Q,VAL,QL),!,  
  retirerValL(VAL,L,QL,L1), 
  linToSudoku(L1,S1).

rempSimonC(VAL,S,S1) :- % Sudoku avec VAL multiple et 0 (Sc)
  seqSudoku(S,_,C,Q),
  caseSpC(Q,VAL,QL),!,   % recherche des cases avec VAL sur une seule colonne
  retirerValC(VAL,C,QL,C1), % retirer VAL de la colonne dans les 2 autres cases
  colToSudoku(C1,S1).    

retirerValL(_,L,[],L) :- !. % QL liste des cases concernées et la ligne 
retirerValL(VAL,L,[PQ|RQL],L1) :-  % L = S décomposé en lignes
  retirerVal(VAL,L,PQ,L0),!,
  retirerValL(VAL,L0,RQL,L1).

retirerValC(_,C,[],C) :- !. % QL liste des cases concernées et la colonne
retirerValC(VAL,C,[PQ|RQL],C1) :-  % C = S décomposé en colonnes
  retirerValc(VAL,C,PQ,C0),!,
  retirerValC(VAL,C0,RQL,C1).

% sudoku(5,S),afficheSudoku(S),tent3Sudoku(S,SS,NRT),etatSudoku(SS,SSS),nl,nett0Sudoku(SSS,NN),placementVal(4,NN,NNN,NP),afficheSudoku(NNN),rempSimon(4,NNN,SIM),nl,afficheSudoku(SIM).

% recherche des cases contenant VAL sur une seule ligne donne son numéro et
% le numéro de la ligne dans la case 
% (et donc il faudra exclure VAL sur la même ligne dans les autres cases)
% un peu redondant si pas d'autres VAL sur la ligne 

caseSp(Q,VAL,QL) :- % liste des cases, VAL, liste [no_case, no_ligne de la case]
  caseSp0(1,Q,VAL,QL).

caseSpC(Q,VAL,QL) :- % liste cases, VAL, liste [no_case, no_col de la case]
  caseSpC0(1,Q,VAL,QL).

caseSp0(10,_,_,[]) :- !.
caseSp0(N,[Q|RQ],VAL,[[N,NoL]|RQL]) :- 
  caseSp00(VAL,Q,NoL),!,
  N1 is N+1,
  caseSp0(N1,RQ,VAL,RQL). 
caseSp0(N,[_|RQ],VAL,RQL) :- 
  N1 is N+1,
  caseSp0(N1,RQ,VAL,RQL). 

caseSpC0(10,_,_,[]) :- !.
caseSpC0(N,[Q|RQ],VAL,[[N,NoC]|RQL]) :- % N no de la case
  caseSpC00(VAL,Q,NoC),!,
  N1 is N+1,
  caseSpC0(N1,RQ,VAL,RQL). 
caseSpC0(N,[_|RQ],VAL,RQL) :- 
  N1 is N+1,
  caseSpC0(N1,RQ,VAL,RQL). 

caseSp00(VAL,Q,NoL) :- 
  splitList(Q,3,LQ),
  selLinCase(VAL,LQ,1,NL),
  casSp01(NL,NoL). % echec si VAL sur plusieurs lignes

caseSpC00(VAL,Q,NoC) :- 
  splitListMod(Q,3,LQ), % colonnes de la case
  selColCase(VAL,LQ,1,NC), 
  casSpC01(NC,NoC). % echec si VAL sur plusieurs colonnes

casSp01([NoL],NoL).

casSpC01([NoC],NoC).
  
selLinCase(_,[],_,[]) :- !.
selLinCase(VAL,[SQ|RQ],NoL,[NoL|RNL]) :-
  member(VAL,SQ),!,  
  NoL1 is NoL+1,  
  selLinCase(VAL,RQ,NoL1,RNL).
selLinCase(VAL,[_|RQ],NoL,RNL) :-
  NoL1 is NoL+1,  
  selLinCase(VAL,RQ,NoL1,RNL).

selColCase(_,[],_,[]) :- !.
selColCase(VAL,[SQ|RQ],NoC,[NoC|RNC]) :-
  member(VAL,SQ),!,  
  NoC1 is NoC+1,  
  selColCase(VAL,RQ,NoC1,RNC).
selColCase(VAL,[_|RQ],NoC,RNC) :-
  NoC1 is NoC+1,  
  selColCase(VAL,RQ,NoC1,RNC).

% sudoku(2,S),placementVal(S,1,S1,N),afficheSudoku(S),nl,nl,afficheSudoku(S1),cases(S1,Q),caseSp(Q,1,QL).

% on ne pourrait que traiter la col concernée 
% retirer VAL sur ligne selon indication PQ = [NoC, NoL]
retirerVal(VAL,LS,PQ,LS1) :- % LS = S décomposé en ligne 
  noLigne(PQ,Li), % recherche de la ligne
  tranche(PQ,TR), % tranches de la ligne à modifier
  nth1(Li,LS,L),
  modifL(VAL,L,TR,L1), % nouvelle ligne
  ligneSudoku(LS,Li,L1,LS1). % nouvelle version sudoku en ligne
% linToSudoku(LS1,S1)

% retirer VAL sur colonne selon indication PQ = [NoQ (case), NoC (dans case)]
retirerValc(VAL,CS,PQ,CS1) :- % CS = S décomposé en colonne 
  noCol(PQ,Cj), % recherche de la colonne
  %write(Cj),write(" ; "),
  trancheC(PQ,TR), % tranches de la colonne à modifier
  %write(TR),nl,
  nth1(Cj,CS,C),   % la colonne
  %write(C),nl,
  modifC(VAL,C,TR,C1), % nouvelle col après modif de la tranche
  %write(C1),nl,
  colSudoku(CS,Cj,C1,CS1). % nouvelle version sudoku en colonne
		 
modifC(VAL,C,[P1,P2],C1) :-
   splitList(C,3,SC),
   nth1(P1,SC,SC1),
   replaceAll(SC1,VAL,0,SC11), 
   nth1(P2,SC,SC2),
   replaceAll(SC2,VAL,0,SC22), 
   replace1(P1,SC,SC11,SC0),
   replace1(P2,SC0,SC22,DC1),
   append(DC1,C1).

modifL(VAL,L,[P1,P2],L1) :-
   splitList(L,3,SL),
   nth1(P1,SL,SL1),
   replaceAll(SL1,VAL,0,SL11), 
   nth1(P2,SL,SL2),
   replaceAll(SL2,VAL,0,SL22), 
   replace1(P1,SL,SL11,SL0),
   replace1(P2,SL0,SL22,DL1),
   append(DL1,L1).
		 
ligneSudoku(LS,Li,L1,LS1) :-
   replace1(Li,LS,L1,LS1).

colSudoku(CS,Cj,C1,CS1) :-
   replace1(Cj,CS,C1,CS1).
		 
noLigne([NoC,NoL],NoL) :- 
  NoC<4,!.
noLigne([NoC,NoL],Li) :- 
  NoC<7,!,
  Li is NoL+3.
noLigne([_,NoL],Li) :- 
  Li is NoL+6.

noCol([NoQ,NoC],NoC) :- 
  member(NoQ,[1,4,7]),!.
noCol([NoQ,NoC],Cj) :- 
  member(NoQ,[2,5,8]),!,
  Cj is NoC+3.
noCol([_,NoC],Cj) :- 
  Cj is NoC+6.

tranche([Noc,_],[2,3]) :- 
  member(Noc,[1,4,7]),!.
tranche([Noc,_],[1,3]) :- 
  member(Noc,[2,5,8]),!.
tranche(_,[1,2]).
		 
trancheC([NoQ,_],[2,3]) :- 
  NoQ<4,!.
trancheC([NoQ,_],[1,3]) :- 
  NoQ<7,!.
trancheC(_,[1,2]).

% sudoku(2,S),placementVal(S,1,S1,N),afficheSudoku(S),nl,nl,afficheSudoku(S1),cases(S1,Q),caseSp(Q,1,QL).

% *** fin complSimon			  
  
% ** Recherche toutes les possibilités de chaque emplacement vide
etatSudoku(S,ES) :- 
  seqSudoku(S,L,C,Q),
  compSudoku(1,S,L,C,Q,ES).

% interface 
provSudoku(N) :-
  sudoku(N,S),
  etatSudoku(S,ES),
  NN is N+100,
  assertz(sudoku(NN,ES)).

% passage en revue des remplacements
compSudoku(_,[],_,_,_,[]) :- !.
compSudoku(N,[0|RS],L,C,Q,[ES|RES]) :- !, 
  %write(N),write(" "),
  valPossible(N,L,C,Q,ES),
  %write(N),write(" "),
  N1 is N+1,
  compSudoku(N1,RS,L,C,Q,RES).
compSudoku(N,[E|RS],L,C,Q,[E|RES]) :- 
  N1 is N+1,
  compSudoku(N1,RS,L,C,Q,RES). 

% valeurs possibles de l'emplacement N
valPossible(N,L,C,Q,ES) :- 
  coord(LCOOR),
  nth1(N,LCOOR,COOR),
  valLigne(COOR,L,VL),
  valColonne(COOR,C,VC),
  valCase(COOR,Q,VQ),
  append(VL,VC,IMP0),
  append(VQ,IMP0,IMP1),
  sort(IMP1,IMP2),
  epure(IMP2,IMP), % ote le premier 0 s'il en reste 1
  %write(IMP),write("-"),
  %diff([1,2,3,4,5,6,7,8,9],IMP,ES),
  subtract([1,2,3,4,5,6,7,8,9],IMP,ES),
  nouvelleV(N,ES).
  %append(IMP,ES,[1,2,3,4,5,6,7,8,9]). % ne fonctionne pas !

valLigne([CL,_],L,VL) :- 
  nth1(CL,L,VL).
valColonne([_,CC],C,VC) :- 
  nth1(CC,C,VC).
valCase(COOR,Q,VQ) :- 
  caseDe(COOR,CASE),
  nth1(CASE,Q,VQ).

% Nouvel emplacement rempli (outil pour contrôle sortie valPossible)
nouvelleV(N,[V]) :- !,
  write("Position: "),
  write(N),
  write("  "),
  write(V),
  nl.
nouvelleV(_,_).
  
%*** fin etatSudoku

% Nettoyage (sans remises à 0): remplacement des listes à valeur unique
nettSudoku([],[]) :- !. 
nettSudoku([[E]|R],[E|R1]) :- !, 
  nettSudoku(R,R1).
nettSudoku([E|R],[E|R1]) :- 
  not(is_list(E)),!, 
  nettSudoku(R,R1).
nettSudoku([L|R],[L|R1]) :- !, 
  nettSudoku(R,R1).
  
% ** Nettoyage (case à valeurs multiples remplacée par 0)
nett0Sudoku([],[]) :- !. 
nett0Sudoku([[E]|R],[E|R1]) :- !, 
  nett0Sudoku(R,R1).
nett0Sudoku([E|R],[E|R1]) :- 
  not(is_list(E)),!, 
  nett0Sudoku(R,R1).
nett0Sudoku([_|R],[0|R1]) :- !, 
  nett0Sudoku(R,R1).

%*** fin nettoyage

% pour analyser problème Simon 
% sudoku(5,S),afficheSudoku(S),tent3Sudoku(S,SS,NRT),etatSudoku(SS,SSS),nl,nett0Sudoku(SSS,NN),tent3Sudoku(NN,NNN,NR1),afficheSudoku(NNN).

% sudoku(5,S),afficheSudoku(S),tent3Sudoku(S,SS,NRT),etatSudoku(SS,SSS),nl,nett0Sudoku(SSS,NN),placementVal(4,NN,NNN,NP),afficheSudoku(NNN).
% à retirer 4, postions 73,74 because case 9

% *********
% 5. Essais
% *********

% ** Remplissage de la grille par backtrack à partir des valeurs possibles
% Impraticable comme moyen général (sauf Sudoku 4x4).
% utilisable sur cas réduit (2 possibilités p. ex) 			  
 
tentative(ES,SS) :- % ES avec cas multiples
  tentative0(ES,SS),
  afficheSudoku(SS),nl,
  sudOK(SS),!.

tentative0([],[]). 
tentative0([[E]|RES],[E|RSS]) :- !,
  tentative0(RES,RSS).
tentative0([E|RES],[E1|RSS]) :- 
  is_list(E),!,
  member(E1,E),  
  tentative0(RES,RSS).
tentative0([E|RES],[E|RSS]) :- 
  tentative0(RES,RSS).
  
% sudoku(1,S),etatSudoku(S,ES),tentative(ES,SS),afficheSudoku(SS).
% **** Fin tentative

% ** Placements répétés
% Essai de placement des valeur par ordre décroissant de leur fréquence

tent3Sudoku(S,SS,NRT) :- % cmd: P*
  statistique(S,1,Stat), % valeurs 
  sort_index(Stat,LVal),  % les valeurs par ordre décroissant
  placementElem(S,LVal,SS,0,NRT).

placementElem(S,[],S,NRT,NRT) :- !. 
placementElem(S,[V|RVal],SS,NRT0,NRT) :- 
  placement(V,S,SC,NR),
  NRT1 is NRT0+NR,
  suitePlacement(V,RVal,NR,SC,SS,NRT1,NRT).

suitePlacement(_,RVal,0,SC,SS,NRT0,NRT) :- !, 
  placementElem(SC,RVal,SS,NRT0,NRT).
suitePlacement(V,RVal,_,SC,SS,NRT0,NRT) :-  
  placement(V,SC,SC1,NR),
  NRT1 is NRT0+NR,
  suitePlacement(V,RVal,NR,SC1,SS,NRT1,NRT).

% *** fin tent3
			  
% Alternance et répétition de deux opérations: 
% pour chaque valeur rechercher les cases où elle a une unique possibilité, 
% puis recherche des cases où il n'y a pas d'autres valeurs possibles 
% (recherche de l'état puis nettoyage des cases où plusieurs sont possibles.)

% sudoku(1,S),afficheSudoku(S),tent3Sudoku(S,SS,NR1),nl,tent3Sudoku(SS,SS1,NR2),nl,afficheSudoku(SS1),etatSudoku(SS1,ES),nettSudoku(ES,ESS),tent3Sudoku(ESS,SSS,NR3),afficheSudoku(SSS).

% résout le sudoku 4 

% sudoku(4,S),afficheSudoku(S),tent3Sudoku(S,SS,NR1),nl,tent3Sudoku(SS,SS1,NR2),nl,afficheSudoku(SS1),etatSudoku(SS1,ES),nettSudoku(ES,ESS),tent3Sudoku(ESS,SSS,NR3),afficheSudoku(SSS),nett0Sudoku(SSS,ESSS),nl,tent3Sudoku(ESSS,SSSS,NR4),afficheSudoku(SSSS).

% **********************
% 6. Recherche de motifs
% **********************

% ** motif de 2
% Recherche des cases où deux emplacements sont les seuls qui peuvent 
% contenir deux valeurs. Elimination des autres valeurs 
% de ces cases et ces valeurs ailleurs
% La même procédure s'applique aux lignes et aux colonnes

% !! deux cas: 
% 1) [4,9] [4,9] à supprimer 4 et 9 dans les autres emplacements de la case
% 2) [1,6,9] [5,6,9] sans 6 ni 9 ailleurs, oter 1 et 5 dans ces 2 emplacements 

elimDoublet(ES,ESS,[NRQ,NRL,NRC]) :- % fonctionne à partir d'un état multiple
  elimDoubletCase(ES,ES1,NRQ),
  elimDoubletLigne(ES1,ES2,NRL),
  elimDoubletColonne(ES2,ESS,NRC).
 
elimDoubletCase(ES,ESS,NRQ) :-
  cases(ES,LQ),          
  purification(LQ,LQ1,0,NRQ), 
  casToSudoku(LQ1,ESS).

elimDoubletLigne(ES,ESS,NRL) :-
  lignes(ES,LQ),          
  purification(LQ,LQ1,0,NRL), 
  linToSudoku(LQ1,ESS).

elimDoubletColonne(ES,ESS,NRC) :-
  colonnes(ES,LQ),          
  purification(LQ,LQ1,0,NRC), 
  colToSudoku(LQ1,ESS).

purification([],[],NR,NR) :- !. 
purification([Q|RQ],[Q1|RQ1],NR0,NR) :- 
  purifCase(Q,Q1,N),!,
  NR1 is NR0+N,
  purification(RQ,RQ1,NR1,NR).
purification([Q|RQ],[Q|RQ1],NR0,NR) :- 
  purification(RQ,RQ1,NR0,NR).
  
purifCase(Q,Q1,NR) :- % Q= [[1,3],[4,7, ...]
  newMatrix("case",9,9), % position case x valeur mette 1 ou 0
  caseToMat("case",Q,1),  % LQ = [[1,0,1,0, ...], [0,0,0,1,0,0,1,0...]]
  traitementCase2("case",NR2), % cas 2 
  traitementCase1("case",NR1), % cas 1
  NR is NR1+NR2,
  matToCase("case",Q1,1),			    
  delMatrix("case").

% parcours des positions de la case 
% case comme matrice: chaque ligne est un emplacement
% les colonnes donne les valeurs de l'emplacement (0 ou 1) 
caseToMat(_,[],_) :- !. 
caseToMat(C,[E|RQ],N) :-  % E valeur ev liste
  caseToMat0(C,E,N), % remplissage de la Nième ligne
  N1 is N+1,			    
  caseToMat(C,RQ,N1).
			    
caseToMat0(C,E,N) :-
   is_list(E),!, % case multiple
   caseToMat01(C,E,N).
caseToMat0(C,E,N) :- % mettre 1 dans la position (N,E)
    putVal(C,N,E,1). 			    

caseToMat01(_,[],_):- !.
caseToMat01(C,[E|RE],N):- 
  putVal(C,N,E,1),
  caseToMat01(C,RE,N).

matToCase("case",[],10) :- !.		    
matToCase(C,[LJ1|RQ],N) :- % ligne N position N les colonnes J avec 1 
  findall(J,matrix(C,N,J,1),LJ),
  delist(LJ,LJ1),
  N1 is N+1,
  matToCase(C,RQ,N1).			    

delist([E],E) :- !.
delist(E,E).
/*
matToCase0(_,_,[],E0,E) :- 			    
  reverse(E0,E).
matToCase0(C,N,[J|RJ],E0,E) :-		    
   getVal(C,N,J,V),
   ajoute(V,E0,E1),
   matToCase0(C,N,RJ,E1,E). 			    
*/
ajoute(0,E0,E0) :- !.
ajoute(V,E0,[V|E0]).

% ** Cas 1: [4,9] [4,9] à supprimer 4 et 9 dans autres emplacements de la case
% particularité: sumLin des emplacements concerné vaut 2
% à vérifier les colonnes correpondent (même valeur)
% action ôter 4 et 9 dans autres emplacements (lignes)
traitementCase1(C,NR) :-
  sumLins(C,SL),
  SL = [_|SL1],			    
  examenLin(C,SL,SL1,1,2,0,NR). % examen par lignes d'abord 1 et 2  

examenLin(C,[2,_],[2],I1,I2,NR0,NR) :- % comparaison des 2 dernières lignes
  nettoiePosition1(C,I1,I2,NR1),!, % avec vérif même valeurs
  NR is NR0 + NR1.
examenLin(_,[_,_],[_],_,_,NR,NR) :- !. 
examenLin(C,[2|RSL],[2],I1,I2,NR0,NR) :- 
  nettoiePosition1(C,I1,I2,NR1),!, 
  RSL = [_|SL],
  NR01 is NR0+NR1,
  I10 is I1+1,
  I20 is I10+1,
  examenLin(C,RSL,SL,I10,I20,NR01,NR). 
examenLin(C,[_|RSL],[_],I1,_,NR0,NR) :- !,
  RSL = [_|SL],
  I10 is I1+1,
  I20 is I10+1,
  examenLin(C,RSL,SL,I10,I20,NR0,NR). 
examenLin(C,[2|RSL],[2|RSL1],I1,I2,NR0,NR) :- 
  nettoiePosition1(C,I1,I2,NR1),!, 
  NR01 is NR0+NR1,
  I20 is I2+1,
  examenLin(C,[2|RSL],RSL1,I1,I20,NR01,NR). 
examenLin(C,[2|RSL],[_|RSL1],I1,I2,NR0,NR) :- !,
  I20 is I2+1,
  examenLin(C,[2|RSL],RSL1,I1,I20,NR0,NR). 
examenLin(C,RSL,[_|RSL1],I1,I2,NR0,NR) :- !,
  I20 is I2+1,
  examenLin(C,RSL,RSL1,I1,I20,NR0,NR). 
examenLin(C,[_|RSL],_,I1,_,NR0,NR) :- 
  RSL = [_|SL],
  I10 is I1+1,
  I20 is I10+1,
  examenLin(C,RSL,SL,I10,I20,NR0,NR). 

nettoiePosition1(C,I1,I2,NR) :- % vérifie que les lignes I1 et I2
% ont 1 dans les mêmes colonnes puis met à 0 ces colonnes dans 
% les autres lignes  
  findall(J1,matrix(C,I1,J1,1),LJ1),
  findall(J2,matrix(C,I2,J2,1),LJ2),
  length(LJ1,2),
  length(LJ2,2),
  egale(LJ1,LJ2),
  LJ1= [C1,C2],
  %write(LJ1),nl,
  mise0Col(C,1,C1,C2,I1,I2,0,NR). % 0 dans les deux colonnes C1 et C2 sauf 
  % aux emplacements I1, I2 
  
egale([A,B],[A,B]) :- !.
egale([A,B],[B,A]).

mise0Col(_,10,_,_,_,_,NR,NR) :- !. 
mise0Col(CASE,I1,C1,C2,I1,I2,NR0,NR) :- !,
  I10 is I1+1,
  mise0Col(CASE,I10,C1,C2,I1,I2,NR0,NR).
mise0Col(CASE,I2,C1,C2,I1,I2,NR0,NR) :- !,
  I10 is I2+1,
  mise0Col(CASE,I10,C1,C2,I1,I2,NR0,NR).
mise0Col(CASE,I,C1,C2,I1,I2,NR0,NR) :-
  putVal0(CASE,I,C1,N1),
  putVal0(CASE,I,C2,N2),
  II is I+1,
  NR01 is NR0+N1+N2,
  mise0Col(CASE,II,C1,C2,I1,I2,NR01,NR).

% ** Cas 2: p. ex [1,6,9] [5,6,9] sans 6, 9 ailleurs, à ôter 1 et 5 
% particularité sumCol de 6 et 9 vaut 2
% à vérifier dans même emplacement (ligne)
% Action: oter 1 et 5 dans ces 2 emplacements  
	 			    
traitementCase2(C,NR) :-
  sumCols(C,SL),
  SL = [_|SL1],			    
  examenCol(C,SL,SL1,1,2,0,NR). % examen des colonnes à partir de 1,2

examenCol(C,[2,_],[2],C1,C2,NR0,NR) :-  
  nettoiePosition2(C,C1,C2,NR1),!, % vérifie que les couples sont 
% dans la même position (ligne)
  NR is NR0+NR1.
examenCol(_,[_,_],[_],_,_,NR,NR) :- !. 
examenCol(C,[2|RSL],[2],C1,C2,NR0,NR) :-  
  nettoiePosition2(C,C1,C2,NR1),!,
  NR01 is NR0+NR1,
  RSL = [_|RSL1],
  C10 is C1+1,
  C20 is C10+1,		    
  examenCol(C,RSL,RSL1,C10,C20,NR01,NR).   
examenCol(C,[_|RSL],[_],C1,_,NR0,NR) :-  
  RSL = [_|RSL1],
  C10 is C1+1,
  C20 is C10+1,		    
  examenCol(C,RSL,RSL1,C10,C20,NR0,NR).  
examenCol(C,[2|RSL],[2|RSL1],C1,C2,NR0,NR) :-  
  nettoiePosition2(C,C1,C2,NR1),!,
  NR01 is NR0+NR1,
  C20 is C2+1,		    
  examenCol(C,RSL,RSL1,C1,C20,NR01,NR).  
examenCol(C,[2|RSL],[_|RSL1],C1,C2,NR0,NR) :- !,
  C20 is C2+1,
  examenLin(C,[2|RSL],RSL1,C1,C20,NR0,NR). 
examenCol(C,[2|RSL],[_|RSL1],C1,C2,NR0,NR) :-  
  C20 is C2+1,
  examenCol(C,RSL,RSL1,C1,C20,NR0,NR).  
examenCol(C,[_|RSL],_,C1,_,NR0,NR) :-  
  RSL = [_|RSL1],
  C10 is C1+1,
  C20 is C10+1,		    
  examenCol(C,RSL,RSL1,C10,C20,NR0,NR).  

nettoiePosition2(C,C0,C1,NR) :- % vérifie que les colonnes C0 et C1
% ont 1 dans les mêmes lignes puis met à 0 ces lignes dans 
% les autres colonnes  
  findall(I1,matrix(C,I1,C0,1),LI1),
  findall(I2,matrix(C,I2,C1,1),LI2),
  length(LI1,2),
  length(LI2,2),
  egale(LI1,LI2),
  LI1= [I1,I2],
  %write(LI1), nl,
  mise0Lin(C,1,I1,I2,C0,C1,0,NR). % on met 0 dans les deux lignes I1 et I2 sauf 
  % aux emplacements C0 et C1 

mise0Lin(_,10,_,_,_,_,NR,NR) :- !. 
mise0Lin(CASE,C0,L1,L2,C0,C1,NR0,NR) :- !,
  C is C0+1,
  mise0Lin(CASE,C,L1,L2,C0,C1,NR0,NR).
mise0Lin(CASE,C1,L1,L2,C0,C1,NR0,NR) :- !,
  C is C1+1,
  mise0Lin(CASE,C,L1,L2,C0,C1,NR0,NR).
mise0Lin(CASE,C,L1,L2,C0,C1,NR0,NR) :- 
  putVal0(CASE,L1,C,N1),
  putVal0(CASE,L2,C,N2),
  CC is C+1,
  NR01 is NR0 + N1+N2,
  mise0Lin(CASE,CC,L1,L2, C0,C1,NR01,NR).

putVal0(CASE,L,C,0) :- 
  matrix(CASE,L,C,0),!.
putVal0(CASE,L,C,1) :- 
  putVal(CASE,L,C,0).

% sudoku(1,S),afficheSudoku(S),etatSudoku(S,ES),afficheSudoku(ES),elimDoublet(ES,ESS,NR), afficheSudoku(ESS)
   
% sudoku(1,S),afficheSudoku(S),tent3Sudoku(S,SS,NR1),nl,nl,tent3Sudoku(SS,SS1,NR2),etatSudoku(SS1,ES),nettSudoku(ES,ESS),tent3Sudoku(ESS,SSS,NR3),etatSudoku(SSS,ESSS),nl,nl,afficheSudoku(ESSS),elimDoublet(ESSS,ESSS1,NR4),nl,nl,afficheSudoku(ESSS1)

% m 1 A E E0 P* A E A 
% Dq  -> [4,5] 1X 4X 
% Dl [1,2] 2X
% E0 3xP*  E E0 2x P* V

% ********************************************
% 7. Utilitaires sur les grilles (position)
% ********************************************

% Coordonnées des cases gauche-droite haut-bas
coord([[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7],[1,8],[1,9],
       [2,1],[2,2],[2,3],[2,4],[2,5],[2,6],[2,7],[2,8],[2,9],
       [3,1],[3,2],[3,3],[3,4],[3,5],[3,6],[3,7],[3,8],[3,9],
       [4,1],[4,2],[4,3],[4,4],[4,5],[4,6],[4,7],[4,8],[4,9],
       [5,1],[5,2],[5,3],[5,4],[5,5],[5,6],[5,7],[5,8],[5,9],
       [6,1],[6,2],[6,3],[6,4],[6,5],[6,6],[6,7],[6,8],[6,9],
       [7,1],[7,2],[7,3],[7,4],[7,5],[7,6],[7,7],[7,8],[7,9],
       [8,1],[8,2],[8,3],[8,4],[8,5],[8,6],[8,7],[8,8],[8,9],
       [9,1],[9,2],[9,3],[9,4],[9,5],[9,6],[9,7],[9,8],[9,9]
      ]).

% Numéro des emplacement de gauche à droite de haut en bas
lpos([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
      21,22,23,24,25,26,27,28,29,30,41,42,43,44,45,46,47,48,49,50,
      51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,
      71,72,73,74,75,76,77,78,79,80,81]).

% Numéro emplacement selon coordonnée L,C
emmplacement(L,C,NO) :- 
  NO is (L-1) * 9 + C.

% Numéro des coins (haut-gauche) des cases
coin([1,4,7,28,31,34,55,58,61]).

% Ligne correspondant au Mième emplacement de la Nième case
linCasePos(N,M,L) :- 
  coin(CS),
  coord(Coord),
  nth1(N,CS,Coin),
  nth1(Coin,Coord,[LC,_]),
  L is LC + ((M - 1) div 3).

% Colonne correspondant au Mième emplacement de la Nième case
colCasePos(N,M,Col) :- 
  coin(CS),
  coord(Coord),
  nth1(N,CS,Coin),
  nth1(Coin,Coord,[_,CC]),
  Col is CC + ((M -1) mod 3).

% Ligne et colonne correspondant au Mième emplacement de la Nième case
linColCasePos(N,M,Li,Col) :- 
  coin(CS),
  coord(Coord),
  nth1(N,CS,Coin),
  nth1(Coin,Coord,[LC,CC]),
  Li is LC + ((M - 1) div 3),
  Col is CC + ((M -1) mod 3).
     
% Calcul des coordonnées des coins
coordCoin(LCC) :-
  coin(LC),
  coord(LCOOR),
  coordCoin0(LC,LCOOR,LCC).
  
coordCoin0([],_,[]) :- !.
coordCoin0([C|TC],LCOOR,[CC|RCC]) :-
  nth1(C,LCOOR,CC),
  coordCoin0(TC,LCOOR,RCC).

% L'emplacement de coordonnées (L,C) est-il dans la case N?
dansCase(N,[L,C]) :-
  coordCoin(LCC),
  nth1(N,LCC,[LC,CC]),
  L-LC >= 0,
  C-CC >= 0,
  L-LC < 3,
  C-CC < 3,!.

% (X,Y) dans quelle case?
caseDe(COOR,CASE) :-
  caseDe0(1,COOR,CASE).

caseDe0(N,COOR,N) :-
  dansCase(N,COOR),!.  
caseDe0(N,COOR,CASE) :-
  N1 is N+1,
  caseDe0(N1,COOR,CASE).

% Positions de la ligne N
ligne(N,LPOS) :-
  coord(COORD),
  ligne0(N,1,COORD,LPOS).

ligne0(N,P,[[N,_]|RC],[P|POS1]) :- !,
  P1 is P+1,
  ligne0(N,P1,RC,POS1).  
ligne0(N,P,[_|RC],POS) :- !,
  P1 is P+1,
  ligne0(N,P1,RC,POS).  
ligne0(_,_,[],[]).

% Positions de la colonne N
colonne(N,LPOS) :-
  coord(COORD),
  colonne0(N,1,COORD,LPOS).

colonne0(N,P,[[_,N]|RC],[P|POS1]) :- !,
  P1 is P+1,
  colonne0(N,P1,RC,POS1).  
colonne0(N,P,[_|RC],POS) :- !,
  P1 is P+1,
  colonne0(N,P1,RC,POS).  
colonne0(_,_,[],[]).

% Positions de la case N
case(N,LPOS) :-
  coord(COORD),
  case0(N,1,COORD,LPOS).

case0(N,P,[CO|RC],[P|POS1]) :- 
  dansCase(N,CO),!,
  P1 is P+1,
  case0(N,P1,RC,POS1).  
case0(N,P,[_|RC],POS) :- !,
  P1 is P+1,
  case0(N,P1,RC,POS).  
case0(_,_,[],[]).

% Position relative
% *****************

% case ajacente
casesAdj(NoC,Adj) :- 
  NoC < 4,!,
  subtract([1,2,3],[NoC],Adj).
casesAdj(NoC,Adj) :- 
  NoC < 7,!,
  subtract([4,5,6],[NoC],Adj).
casesAdj(NoC,Adj) :- 
  subtract([7,8,9],[NoC],Adj).
  
% Affichage
% *********

% Affichage de la grille
afficheSudoku(No,T) :- % T= 0  normal T = 1 brouillon
    sudoku(No,L),
    printSudoku(3,L,T).

afficheSudoku(S) :- 
    printSudoku(3,S,0).

printSudoku(0,_,_) :- !.
printSudoku(N,L,T) :-
  printSL(L,RL),
  printSL(RL,RL1),
  printSL(RL1,RL2),
  printSep(N,T),
  N1 is N-1,
  printSudoku(N1,RL2,T).
    
printSep(N,_) :-
   N < 2,!.  
printSep(_,0) :- !,
   write("-----|"),
   write("-----|"),
   write("-----"),
   nl.
printSep(_,1) :-
   write("***********************"),
   nl.

printSL(L,RL) :-
   printSL0(L,RL0),
   write("|"),
   printSL0(RL0,RL1),
   write("|"),
   printSL0(RL1,RL),
   nl.

printSL0([A,B,C|RL],RL) :-
   write(A),
   write(" "),write(B),
   write(" "),write(C).

% Découpages
% **********  

% Découpage des positions en lignes, colonne, cases
intFaceseqSudoku(No,L,C,Q) :- 
  sudoku(No,S),
  seqSudoku(S,L,C,Q).

seqSudoku(S,L,C,Q) :-
  lignes(S,L),
  colonnes(S,C),
  cases(S,Q).

% Ensemble des lignes d'un sudoku
lignes(S,L) :- 
  splitList(S,9,L).

% Ensemble des colonnes d'un sudoku
colonnes(S,L) :- 
  splitListMod(S,9,L).

% Ensemble des cases d'un sudoku
cases(S,L) :-
  cases0(S,1,L).

cases0(_,10,[]) :- !.
cases0(S,P,[LP|RP]) :-
  case(P,LPOS),
  nth1s(LPOS,S,LP),
  P1 is P+1,
  cases0(S,P1,RP).

% decomposition d'une cases (9 valeurs) par paquets de 3 lignes
ligneCase(Q,LQ) :- 
  splitList(Q,3,LQ).

% decomposition d'une cases (9 valeurs) par paquets de 3 colonnes
colCase(Q,CQ) :- 
  splitListMod(Q,3,CQ).

% Recomposition d'un Sudoku
% *************************
						       
% à partir de la liste des colonnes
colToSudoku(C,S) :-
  colToSudoku0(1,C,S).

colToSudoku0(10,_,[]) :- !.
colToSudoku0(N,C,S) :-  
  colToSudoku00(N,C,[],LN),
  N1 is N+1,
  colToSudoku0(N1,C,S0),
  append(LN,S0,S).  

colToSudoku00(_,[],S2,S) :- !,
  reverse(S2,S).
colToSudoku00(NL,[C|RC],RS2,S2) :- 
  nth1(NL,C,E),
  colToSudoku00(NL,RC,[E|RS2],S2). 

% à partir de la liste des lignes
linToSudoku(L,S) :-
  linToSudoku0(L,[],S).

linToSudoku0([],S,S) :- !.
linToSudoku0([L|RL],S1,S) :- 
  append(S1,L,S0),
  linToSudoku0(RL,S0,S).

% à partir de la liste des cases

casToSudoku(Q,S) :- 
  casToLin(1,Q,L),
  linToSudoku(L,S).

casToLin(4,_,[]) :- !.
casToLin(N,[C1,C2,C3|RC],[L1,L2,L3|RS]) :-
  casToLin0(C1,C2,C3,L1,L2,L3),
  N1 is N+1,
  casToLin(N1,RC,RS).

casToLin0(C1,C2,C3,L1,L2,L3) :- 
   C1=[C11,C12,C13,C21,C22,C23,C31,C32,C33],
   C2=[C14,C15,C16,C24,C25,C26,C34,C35,C36],
   C3=[C17,C18,C19,C27,C28,C29,C37,C38,C39],
   L1=[C11,C12,C13,C14,C15,C16,C17,C18,C19],
   L2=[C21,C22,C23,C24,C25,C26,C27,C28,C29],
   L3=[C31,C32,C33,C34,C35,C36,C37,C38,C39].

% sudoku(101,S),afficheSudoku(S),seqSudoku(S,_,_,Q),casToSudoku(Q,ST),nl,afficheSudoku(ST).
% !!!!! sudoku(101,S),afficheSudoku(S),seqSudoku(S,_,_,Q),linToSudoku(Q,ST),nl,afficheSudoku(ST).

% Concatenation de sudoku S1 simple S2 liste ou simple
% restriction ne remplace pas valeur non liste 
concatSudoku(S1,S2,ST) :- 
  concatSud0(S1,S2,ST).

concatSud0([],[],[]) :- !.

concatSud0([0|R1],[E|R2],[E|RT]) :- !, % position 0 non copiée
  concatSud0(R1,R2,RT).
concatSud0([E|R1],[0|R2],[[E]|RT]) :- !, % position 0 remplacée par liste 1 el
  concatSud0(R1,R2,RT).
concatSud0([_|R1],[E|R2],[E|RT]) :-  % emplacement simple non remplacé
  not(is_list(E)),!,
  concatSud0(R1,R2,RT).
concatSud0([E|R1],[F|R2],[F|RT]) :- % F forcément liste
  member(E,F),!,
  concatSud0(R1,R2,RT).
concatSud0([E|R1],[F|R2],[G|RT]) :-
  % write(E),write(";"),write(F),nl, 
  sort([E|F],G),
  % write(E),write(";"),write(F),write(";"),write(G),nl,
  concatSud0(R1,R2,RT).

% ************************
% 9. Utilitaires de listes
% ************************

% classe les index d'une liste par décroissance des valeurs
sort_index(L,SI) :- 
  length(L,LEN),
  sort_i0(LEN,L,1,SI).

sort_i0(LEN,L,N,[IMAX|RI]) :-
  N =< LEN,
  index_max(L,IMAX),
  replace1(IMAX,L,0,L1),!,
  N1 is N+1,
  sort_i0(LEN,L1,N1,RI).
sort_i0(_,_,_,[]).

% Index de la valeur maximum
index_max(L,IMAX) :-
  length(L,LEN),
  index_max0(LEN,L,1,1,IMAX,0,_).

index_max0(LEN,_,N,IMAX,IMAX,_,_) :-
  N > LEN,!.
index_max0(LEN,L,N,_,IMAX,MAX0,MAX) :-
  nth1(N,L,VN),
  VN > MAX0,!,
  N1 is N+1,
  index_max0(LEN,L,N1,N,IMAX,VN,MAX).
index_max0(LEN,L,N,I0,IMAX,MAX0,MAX) :-
  N1 is N+1,
  index_max0(LEN,L,N1,I0,IMAX,MAX0,MAX).

% Fréquence absolue de chaque valeur de la liste S
statistique(S,N,[Stat|RStats]) :- 
  N<10,!,
  count(S,N,Stat),
  N1 is N+1,
  statistique(S,N1,RStats).
statistique(_,_,[]).

% Comptage des éléments valant une valeur 
count(S,V,St) :-
  findall(V,member(V,S),L),
  length(L,St).

% Copie les valeurs des positions d'une 1e liste dans une 2e  
% nth1s(liste position, 1e liste, 2e liste)
nth1s([],_,[]) :- !.
nth1s([POS|RPOS],S,[V|RV]) :-
  nth1(POS,S,V),
  nth1s(RPOS,S,RV).

% ** Decoupage d'une liste en une liste de sous-listes de longueur N
splitList(LI,N,LL) :-
  splitList0(LI,N,LL).

splitList0(L,N,[]) :-
  length(L,LEN),
  LEN<N,!.
splitList0(LI,N,[LL|RLL]) :-
  cars(LI,N,LL,LI1),
  splitList0(LI1,N,RLL).		 

% ** Decoupage d'une liste L en une liste de sous-listes consituées 
% des classes de reste mod N des éléments pris de L 
splitListMod(LI,MOD,LL) :- 
  splitListMod0(LI,1,MOD,LL). 

splitListMod0(_,CL,MOD,[]) :- 
  CL>MOD,!. 
splitListMod0(LI,CL,MOD,[L|RLL]) :- 
  elemMod(LI,CL,MOD,L),
  CL1 is CL+1,
  splitListMod0(LI,CL1,MOD,RLL). 

elemMod(LI,CL,_,[]) :- 
  length(LI,LEN),
  CL>LEN,!.
elemMod(LI,CL,MOD,[E|RL]) :- 
  nth1(CL,LI,E),
  CL1 is CL+MOD,
  elemMod(LI,CL1,MOD,RL). 

% N premiers éléments de L
cars(L,N,LN,RL) :- 
  cars0(L,1,N,LN,RL).

cars0(L,N1,N,[],L) :- 
  N1 =:= (N+1),!.
cars0([E|RL],N0,N,[E|RLN],LF) :- 
  N1 is N0+1,
  cars0(RL,N1,N,RLN,LF).

%nth1s(LP,S,LV) :-
%  maplist(nth1,LP,S,LV).

% Remplacement
% la valeur en position ID de L par V 
replace1(ID,L,V,L1) :- 
  replace10(1,ID,L,V,L1).

replace10(_,_,[],_,[]) :- !.
replace10(N,ID,[_|R],V,[V|R1]) :-
  N=:=ID,!,
  N1 is N+1,  
  replace10(N1,ID,R,V,R1).
replace10(N,ID,[E|R],V,[E|R1]) :-
  N1 is N+1,
  replace10(N1,ID,R,V,R1).

% remplacement de toutes V par NV

replaceAll([],_,_,[]) :- !. 
replaceAll([V|RL],V,NV,[NV|RL1]) :- !, 
  replaceAll(RL,V,NV,RL1). 
replaceAll([E|RL],V,NV,[E|RL1]) :- !, 
  replaceAll(RL,V,NV,RL1). 


% Retirer 0 d'une liste (à simplifier si 0 en premier vu le sort)
epure([0|R],R) :- !.
epure(R,R) :- !.
  
epure(L,L0) :- 
  findall(A,memberNNul(A,L),L0).

memberNNul(A,L) :-
  member(A,L),
  A=\=0. 

% Difference d'ensemble 
% A tester la primitive subtract(S1,S2,S) S = S1-S2
diff([],_,[]) :- !.
diff([E|L1],L2,L) :- % L1 - L2
  member(E,L2),!,
  diff(L1,L2,L).
diff([E|L1],L2,[E|L]) :- 
  diff(L1,L2,L).

/* depredated -> splitList
% découpage d'une liste en paquets de 3, non det.
decoupe3(L,SL) :-
  member(N,[1,4,7]),
  mselect(L,N,3,SL0),
  reverse(SL0,SL).

mselect(_,_,0,[]) :- !.
mselect(L,N,Of,[E|RL]) :-
  I is N+Of-1,
  nth1(I,L,E),
  Of1 is Of-1,
  mselect(L,N,Of1,RL).
*/
% 
% **************
% NQ paquet de 3 cases (en ligne) 
% NC no de la case, NLC no de la ligne dans la case ne contenant 
% ni 0 ni VAL (non déterministe)
is_compact(NQ,VAL,NC,NLC) :-
  member(NC,[1,2,3]),   
  nth1(NC,NQ,CASE),
  decoupe3(CASE,NLC,SL),
  not(member(0,SL)),
  not(member(VAL,SL)).

% Mise sous forme d'une database
% ******************************

% posSud(No Sudoku,placement,ligne, colonne, case, valeur)

% liste -> db
sudoku2db(No,S) :-
  sudoku2db0(No,1,S).
sudoku2db0(_,_,[]) :- !.
sudoku2db0(No,N,[E|RS]) :- 
  NoL is ((N-1) div 9) +1, 
  NoC is ((N-1) mod 9) +1,
  caseDe([NoL,NoC],NoQ), 
  assertz(posSud(No,N,NoL,NoC,NoQ,E)),
  N1 is N+1,
  sudoku2db0(No,N1,RS).

% db -> liste
db2Sudoku(No,S) :-
  db2Sudoku0(No,1,S).

db2Sudoku0(_,82,[]) :- !.
db2Sudoku0(No,N,[E|RS]) :- 
  posSud(No,N,_,_,_,E),!,
  N1 is N+1,  
  db2Sudoku0(No,N1,RS). 

% *******************************
% 9. Utilitaires sur les matrices
% *******************************

newMatrix(NAME,NROW,NCOL) :-
  retractall(matrix(NAME,_,_,_)),		 
  makeRow(1,NAME,NROW,NCOL).

delMatrix(NAME) :-
  retractall(matrix(NAME,_,_,_)).

makeRow(N,_,NROW,_) :- 
  N>NROW,!.
makeRow(N,NAME,NROW,NCOL) :- 
  makeCol(N,1,NAME,NCOL),
  N1 is N+1,
  makeRow(N1,NAME,NROW,NCOL).

makeCol(_,M,_,NCOL) :- 
  M>NCOL,!.
makeCol(N,M,NAME,NCOL) :- 
  assertz(matrix(NAME,N,M,0)),
  M1 is M+1,
  makeCol(N,M1,NAME,NCOL). 

getVal(NAME,LI,CO,VAL) :- 
  matrix(NAME,LI,CO,VAL),!.

putVal(NAME,LI,CO,VAL) :- 
  retract(matrix(NAME,LI,CO,_)),
  assertz(matrix(NAME,LI,CO,VAL)),!.

afficheMatrix(NAME) :-
  findall(I,matrix(NAME,I,1,_),LI),
  sort(LI,SLI), 
  afficheRow(NAME,SLI).

afficheRow(_,[]) :- !.
afficheRow(NAME,[I|RI]) :- 
  getLin(NAME,I,LE),
  write(I),write(": "),
  write(LE),nl,
  afficheRow(NAME,RI). 
  
getLin(NAME,I,LE) :- 
  findall(J,matrix(NAME,1,J,_),LJ),
  sort(LJ,SLJ), 	 
  gL0(NAME,I,SLJ,LE).

gL0(_,_,[],[]) :- !.
gL0(NAME,I,[J|RJ],[E|RLE]) :-
  matrix(NAME,I,J,E),!,  		 
  gL0(NAME,I,RJ,RLE).

getCol(NAME,J,LE) :- 
  findall(I,matrix(NAME,I,1,_),LI),
  sort(LI,SLI), 	 
  gC0(NAME,J,SLI,LE).

gC0(_,_,[],[]) :- !.
gC0(NAME,J,[I|RI],[E|RLE]) :-
  matrix(NAME,I,J,E),!,  		 
  gC0(NAME,J,RI,RLE).

sumCol(NAME,J,S) :-
  getCol(NAME,J,LE), 
  sumlist(LE,S).

sumLin(NAME,I,S) :- 
  getLin(NAME,I,LE), 
  sumlist(LE,S).

sumCols(NAME,LS) :-
  findall(J,matrix(NAME,1,J,_),LJ),
  sort(LJ,SLJ),
  maplist(sumCol(NAME),SLJ,LS).

sumLins(NAME,LS) :-
  findall(I,matrix(NAME,I,1,_),LI),
  sort(LI,SLI),
  maplist(sumLin(NAME),SLI,LS).


% ********************
% 10. Interaction 
% ********************
			    
int_main :-
  read_line_to_string(user_input,E),
  execute(E).

execute("s") :- !.
execute(E) :-
  split_string(E," "," ",LE),
  execute0(LE),
  int_main.

verif_sudoku(S) :-
  length(S,81).		 

memo(SUD) :- 
  verif_sudoku(SUD),!,
  retractall(last_sudoku(_)),
  asserta(last_sudoku(SUD)).
memo(_) :- 
  write("Pas un sukoku"),
  nl.

last_sudoku("").
		 
recup_sudoku(_) :-
  last_sudoku(""),!,		 
  write("Pas de sudoku à récupéré"),
  nl,fail.
		 
recup_sudoku(SUD) :- 
  last_sudoku(SUD),!.

% ** Vide 
execute0([]) :- !.
		 
% ** Affichage			    
execute0(["A",SN|R]) :- % via index du sudoku
  number_string(N,SN),!,
  sudoku(N,S),
  afficheSudoku(S),nl,
  execute0(R).		  
execute0(["A"|R]) :-
  recup_sudoku(SUD),!,
  afficheSudoku(SUD),nl,
  execute0(R).		  
execute0(["A"|R]) :- !,
  write("Rien à afficher"),nl,
  execute0(R).		  

% ** Mémorisation			    
execute0(["M",SN|R]) :- %  mémorisation de SUD sous l'index N
  number_string(N,SN),!,
  recup_sudoku(SUD),
  assertz(sudoku(N,SUD)),
  execute0(R).		  

execute0(["M"|R]) :- !,
  write("Problème de mémorisation"),nl,
  execute0(R).		  

execute0(["m",SN | R]) :- %  mise du sudoku SN dans database
  number_string(N,SN),!,
  sudoku(N,S),
  memo(S),
  execute0(R).

% ** Transformation (modulo)   
execute0(["Tmod",SMOD, SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfMod(S,SMOD,R).
execute0(["Tmod",SMOD |R]) :- 
  recup_sudoku(S),!,
  i_transfMod(S,SMOD,R).

% ** Tranformation (permutation des valeurs)
execute0(["Tperm", SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfPerm(S,R).
execute0(["Tperm" |R]) :- 
  recup_sudoku(S),!,
  i_transfPerm(S,R).

% ** Permutation des lignes (possible dans des paquets de 3)
execute0(["Tl",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfPL(S,R).
execute0(["Tl" |R]) :- 
  recup_sudoku(S),!,
  i_transfPL(S,R).

% ** Permutation des colonnes (possible dans des paquets de 3)
execute0(["Tc",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfPC(S,R).
execute0(["Tc" |R]) :- 
  recup_sudoku(S),!,
  i_transfPC(S,R).

% ** Symetrie Verticale
execute0(["Tv",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfSV(S,R).
execute0(["Tv"|R]) :- 
  recup_sudoku(S),!,
  i_transfSV(S,R).

% ** Symetrie Horizontale
execute0(["Th",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfSH(S,R).
execute0(["T","h" |R]) :- 
  recup_sudoku(S),!,
  i_transfSH(S,R).

% Echange ligne colonne
execute0(["Td",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_transfLC(S,R).
execute0(["Td" |R]) :-
  recup_sudoku(S),!,
  i_transfLC(S,R).
 
execute0([Cmd|_]) :- 
  string_concat("T",_,Cmd),!,
  write("Problème de transformation"),nl.

% ** Placements de V là où seul possible
execute0(["Pv",SV,SN |R]) :- % SV valeur
  number_string(N,SN),!,
  sudoku(N,S),
  i_placement(SV,S,R).
execute0(["Pv",SV|R]) :- 
  recup_sudoku(S),!,
  i_placement(SV,S,R).

% ** Placement de V où partout possible
execute0(["PV",SV,SN |R]) :- % SV valeur
  number_string(N,SN),!,
  sudoku(N,S),
  i_placementVal(SV,S,R).
execute0(["PV",SV|R]) :- 
  recup_sudoku(S),!,
  i_placementVal(SV,S,R).

% Tentative de placement des valeurs triées par ordre décroissant
execute0(["P*",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_tent3(S,R).
execute0(["P*" |R]) :- 
  recup_sudoku(S),!,
  i_tent3(S,R).

execute0([Cmd|_]) :- 
  string_concat("P",_,Cmd),!,
  write("Problème de placement"),nl.

% placement de V à l'emplacement ID
execute0(["p", SID, SV|R]) :-
  number_string(ID,SID),
  number_string(V,SV),
  recup_sudoku(S),!,
  replace1(ID,S,V,S1), 
  memo(S1),
  execute0(R).		 

% ** Etat 
execute0(["E",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_etatSudoku(S,R).
execute0(["E" |R]) :- 
  recup_sudoku(S),!,
  i_etatSudoku(S,R).

% ** Nettoyage
execute0(["E1",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_nettSudoku(S,R). 
execute0(["E1" |R]) :- 
  recup_sudoku(S),!,
  i_nettSudoku(S,R). 

% ** Nettoyage et remose à 0
execute0(["E0",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_nett0Sudoku(S,R). 
execute0(["E0" |R]) :- 
  recup_sudoku(S),!,
  i_nett0Sudoku(S,R). 

% ** Oter valeur V impossible en ligne 
execute0(["Sl",SV,SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_rempSimon(SV,S,R).
execute0(["Sl",SV |R]) :- 
  recup_sudoku(S),!,
  i_rempSimon(SV,S,R).

% ** Oter valeur V impossible en colonne
execute0(["Sc",SV,SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_rempSimonC(SV,S,R).
execute0(["Sc",SV |R]) :- 
  recup_sudoku(S),!,
  i_rempSimonC(SV,S,R).

% ** Simon général 
execute0(["S*",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_complSimon(S,R).
execute0(["S*" |R]) :- 
  recup_sudoku(S),!,
  i_complSimon(S,R).

% ** Simon en colonnes (placements judicieux des valeurs)
execute0(["Sc*",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_complSimonC(S,R).
execute0(["Sc*" |R]) :- 
  recup_sudoku(S),!,
  i_complSimonC(S,R).

% ** Simon en lignes (placements judicieux des valeurs)
execute0(["Sl*",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_complSimonL(S,R).
execute0(["Sl*" |R]) :- 
  recup_sudoku(S),!,
  i_complSimonL(S,R).

% ** Doublet
execute0(["D*",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_elimDoublet(S,R).
execute0(["D*" |R]) :- 
  recup_sudoku(S),!,
  i_elimDoublet(S,R).

execute0(["Dq",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_elimDoubletCase(S,R).
execute0(["Dq" |R]) :- 
  recup_sudoku(S),!,
  i_elimDoubletCase(S,R).
    
execute0(["Dl",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_elimDoubletLigne(S,R).
execute0(["Dl" |R]) :- 
  recup_sudoku(S),!,
    i_elimDoubletLigne(S,R).
    
execute0(["Dc",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_elimDoubletColonne(S,R).
execute0(["Dc" |R]) :- 
  recup_sudoku(S),!,
  i_elimDoubletColonne(S,R).

% ** Vérification
execute0(["V",SN |R]) :- 
  number_string(N,SN),!,
  sudoku(N,S),
  i_sudOK(S,R).
execute0(["V" |R]) :- 
  recup_sudoku(S),!,
  i_sudOK(S,R). 

execute0(["H"]) :- 
  write("A, M, m"),nl,
  write("Tmod, Tperm, Tv, Th, Tl, Tc, Td"),nl,
  write("Pv, PV, P*, p"),nl,
  write("E, E1, E0"),nl,
  write("Sl, Sc, S*, Sl*, Sc*"),nl,
  write("Dq, Dl, Dc, D*"),nl,
  write("V, H, s"),nl.
		    
execute0([_,SN|_]) :- 
  number_string(N,SN),!,
  write("Sudoku "), write(N), write(" inexistant"),
  nl.			    
		    
execute0(_) :- 
  write("Commande non valide Sudoku"),
  nl.			    

i_transfMod(S,SMOD,R) :- 
  number_string(MOD,SMOD),
  transfMod(S,MOD,ST),
  memo(ST),
  execute0(R).

i_transfPerm(S,R) :- 
  write("Donner les 9 images séparées par des espaces"),nl,
  saisirPerm(PERM),
  transfPerm(S,PERM,ST), 
  memo(ST),
  execute0(R).

i_transfPL(S,R) :-
  write("Donner les deux lignes séparées par un espaces"),nl,
  saisirPerm(LI),
  LI = [L1,L2],
  transfPL(S,L1,L2,ST),
  memo(ST),
  execute0(R).

i_transfPC(S,R) :-
  write("Donner les deux colonnes séparées par un espaces"),nl,
  saisirPerm(LI),
  LI = [L1,L2],
  transfPC(S,L1,L2,ST),
  memo(ST),
  execute0(R).

i_transfLC(S,R) :- 
  transfLC(S,ST),
  memo(ST),
  execute0(R).

i_placement(SV,S,R) :- 
  number_string(V,SV),
  placement(V,S,SC,NCR), % Valeur, Sudoku av, sudoku ap, nb cases modifées
  memo(SC),
  write("Nb placements: "),write(NCR),nl,
  execute0(R).

i_placementVal(SV,S,R) :-
  number_string(V,SV),
  placementVal(V,S,ST,NR),
  memo(ST),
  write("Nb placements: "),write(NR),nl,
  execute0(R).

i_tent3(S,R) :- 
  tent3Sudoku(S,SS,NRT),
  memo(SS),
  write("Nb placements: "),write(NRT),nl,
  execute0(R).

i_etatSudoku(S,R) :-
  etatSudoku(S,SE),
  memo(SE),
  execute0(R).

i_nettSudoku(S,R) :- 
  nettSudoku(S,ST),
  memo(ST),
  execute0(R).

i_nett0Sudoku(S,R) :- 
  nett0Sudoku(S,ST),
  memo(ST),
  execute0(R).

i_rempSimon(SVAL,S,R) :-
  number_string(V,SVAL),
  rempSimon(V,S,S1), 
  memo(S1),
  execute0(R).

i_rempSimonC(SVAL,S,R) :-
  number_string(V,SVAL),
  rempSimonC(V,S,S1), 
  memo(S1),
  execute0(R).
		 
i_complSimon(S,R) :- 
  complSimon(S,SES),
  memo(SES),
  execute0(R).

i_complSimonC(S,R) :- 
  complSimonC(S,SES),
  memo(SES),
  execute0(R).

i_complSimonL(S,R) :- 
  complSimonL(S,SES),
  memo(SES),
  execute0(R).

i_elimDoublet(S,R) :- 
  elimDoublet(S,ST,NRT),
  write("Nb de modifications: "),write(NRT),nl,
  memo(ST),
  execute0(R).

i_elimDoubletCase(S,R) :- 
  elimDoubletCase(S,ST,NRT),
  write("Nb de modifications: "),write(NRT),nl,
  memo(ST),
  execute0(R).

i_elimDoubletLigne(S,R) :- 
  elimDoubletLigne(S,ST,NRT),
  write("Nb de modifications: "),write(NRT),nl,
  memo(ST),
  execute0(R).

 i_elimDoubletColonne(S,R) :- 
  elimDoubletColonne(S,ST,NRT),
  write("Nb de modifications: "),write(NRT),nl,
  memo(ST),
  execute0(R).

i_sudOK(S,R) :- 
  sudOK(S),!,
  write("Sudoku OK"),nl,
  execute0(R).

i_sudOK(_,R) :- 
  write("Erreur Sudoku"),nl,
  execute0(R).

saisirPerm(PERM) :-
  read_line_to_string(user_input,SPERM),
  split_string(SPERM," "," ",LS),
  maplist(string_number,LS,PERM).

string_number(S,N) :- 
  number_string(N,S).

% ********
% Test 
% ********

% ** en mode interactif
% ** m 5 P* E E0 S* D  M 51  ne semble pas retrouver les doublets
% P* S* E0 à prendre 
