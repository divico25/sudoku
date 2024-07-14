This repository is mainly intended to enter the world of github :)

Sudokus are saved in a database: sudoku(no, list values ​​from left to right, top to bottom)

Once loaded in the SWI Porolog environment, the int_main command start an interactive session whose main commands are:

m <sudoku number>: activates sudoku #number by default
A <sudoku number>: displays sudoku #number ; A: display default sudoku
M number: saves sudoku by default under sudoku #number
Tmod, Tperm, Tv, Th, Tl, Tc, Td [number]: perform permutation of values, symmetry, etc.
Pv #val, PV #val, P*, p #ind #val [number]: completes the sydoku #number or by default with a value
E, E1, E0: respectively completes the locations with all possible values ​​and performs the opposite operation
Dq, Dl, Dc, D*: from a complete sudoku reveals the pairs useful for the solution
V, H, s: verif, help, quit

Note: The constraint programming module for SWI Prolog provides a very elegant solution program.
