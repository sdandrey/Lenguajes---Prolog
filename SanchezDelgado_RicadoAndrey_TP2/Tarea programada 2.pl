
test_puzzle(Name,puzzle(Structure,Clues,Queries,Solution)):-
   structure(Name,Structure),
   clues(Name,Structure,Clues),
   queries(Name,Structure,Queries,Solution).

solve_puzzle(puzzle(_, Clues,Queries,Solution),Solution):-
   solve(Clues),solve(Queries).

solve([Clue|Clues]):-Clue,solve(Clues).
solve([]).

mostrar([]).
mostrar([C|Cs]) :- writeln(C),mostrar(Cs).

resolver(Acertijo, Structure, Solucion) :-
         test_puzzle(Acertijo,Puzzle),    % Primero define el cuádruple usando el nombre.
         Puzzle=puzzle(Structure,_,_,_),  % Se extrae la estructura del cuádruple para poder ver y depurar.
         solve_puzzle(Puzzle,Solucion),   % Aplica las pistas y consultas y obtiene la solución.
         mostrar(Structure).              % Muestra estructura en forma legible.


structure(matrimonio,[matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_),
matrimonio(_,_,_,_,_,_,_,_)]).


structure(barcos,[barco(1,_,_,_,_,_),
          barco(2,_,_,_,_,_),
          barco(3,_,_,_,_,_),
          barco(4,_,_,_,_,_),
          barco(5,_,_,_,_,_)]
   ).



clues(
      matrimonio, % identifica las pistas como del acertijo "matrimonio"
      Amigos, % la estructura del acertijo va atando todo
      
   [ 
     %Cuarta pista: 4.  A Stan Horrocks y a su esposa Hannah les gusta el color blanco.
     (
       matrimonio(matrimonio(hannah,stan,horrocks,_,_,blanco,_,_),Amigos)
      
      ),
     %Setima pista: 7.  A Mateo y a su esposa les gusta el color rosa; además prestaron el libro "Mulatka Gabriela"
     (
       matrimonio(matrimonio(_,mateo,_,_,_,rosa,mulakta,_),Amigos)
     ),

     %pista: 1. Daniella Black y su marido trabajan como asistentes de compras
     (
       matrimonio(matrimonio(daniella,_,black,asistentescompras,_,_,_,_),Amigos) 
     ),
     %Segunda pista: 2. El libro "El perro marino" fue prestado por la pareja que conduce un Fiat y que aman el color rojo.
     (
       matrimonio(matrimonio(_,_,_,_,fiat,rojo,elperromarino,_),Amigos)

     ),
     %Tercer pista: 3. A Owen y a su esposa Victoria les gusta  el color marrón
     (
       matrimonio(matrimonio(victoria,owen,_,_,_,marron,_,_),Amigos)
     ),
     %Quinta pista: 5.  Jenny Smith y su marido trabajan como gerentes de almacén y conducen un Wartburg
     (
       matrimonio(matrimonio(jenny,_,smith,gerentesdealmacen,wartburg,_,_,_),Amigos)
       
     ),%Sexta pista: 6.  Mónica y su marido Alexander pidieron prestado el libro "El abuelo José".
     (
       matrimonio(matrimonio(monica,alexander,_,_,_,_,_,elabuelojose),Amigos) 
     ),
     %Octava pista: 8.  Irene y su marido Oto trabajan como contadores
     (
       matrimonio(matrimonio(irene,oto,_,contadores,_,_,_,_),Amigos)
     ),
     %Novena pista: 9.  El libro "Éramos cinco" lo pidió prestado la pareja que conduce un Trabant
     (
       matrimonio(matrimonio(_,_,_,_,trabant,_,_,eramoscinco),Amigos)
     ),
     %Decima pista: 10. Los Cermak son cobradores de entradas; ellos prestaron el libro "El armiño del techo".
     (
       matrimonio(matrimonio(_,_,cermak,cobradoresdeentradas,_,_,elarminodeltecho,_),Amigos)
     ),
     %pista 11. El señor y la señora Kuril son ambos médicos; ellos pidieron prestado el libro "El juez Slovacko".
     (
       matrimonio(matrimonio(_,_,kuril,medicos,_,_,_,eljuezslovacko),Amigos)
     ),
     %pista 12. A Paul y a su esposa les gusta el color verde
     (
       matrimonio(matrimonio(_,paul,_,_,_,verde,_,_),Amigos)
     ),
     %pista 13. A Verónica Dvorak y a su marido les gusta el color azul
     (
       matrimonio(matrimonio(veronica,_,dvorak,_,_,azul,_,_),Amigos)
     ),
     %pista 14. Rick y su esposa trajeron para prestar el libro "El juez Slovacko" y conducen un Zhiguli.
     (
       matrimonio(matrimonio(_,rick,_,_,zhiguli,_,eljuezslovacko,_),Amigos)
     ),
     %pista 16. La pareja que conduce un Dacia, ama el color violeta
     (
       matrimonio(matrimonio(_,_,_,_,dacia,violeta,_,_),Amigos)
     ),
     %pista 17. La pareja que trabaja como maestros pidió prestado el libro "La dama comisaria"
     (
       matrimonio(matrimonio(_,_,_,maestros,_,_,_,ladamacomisaria),Amigos)
     ),
     %pista 18. La pareja de agricultores conduce un Moskvich.
     (
       matrimonio(matrimonio(_,_,_,agricultores,moskvich,_,_,_),Amigos)
     ),
     %pista 19. Pamela y su esposo conducen un Renault y trajeron el libro "El abuelo José"
     (
       matrimonio(matrimonio(pamela,_,_,_,renault,_,elabuelojose,_),Amigos)
     ),
     %pista 20. Pamela y su esposo pidieron prestado el libro que trajeron el señor y la señora Zajac 
     (
       matrimonio(matrimonio(pamela,_,_,_,_,_,_,Libro),Amigos),
       
       matrimonio(matrimonio(_,_,zajac,_,_,_,Libro,_),Amigos)
     ),

     %pista 21. A Roberto y a su esposa les gusta el color amarillo; ellos pidieron en préstamo el libro "La comedia moderna".
     (
       matrimonio(matrimonio(_,roberto,_,_,_,amarillo,_,lacomediamoderna),Amigos)
     ),
     %pista 22. El señor y la señora Swain trabajan como "compradores".
     (
       matrimonio(matrimonio(_,_,swain,compradores,_,_,_,elperromarino),Amigos)
     ),
     %pista 23. "La comedia moderna" fue traído por la pareja que conduce un Skoda.
     (
       matrimonio(matrimonio(_,_,_,_,skoda,_,lacomediamoderna,_),Amigos)
     ),
     %pista 15. Una pareja trajo el libro "La dama comisaria" y pidió prestado el libro "Mulatka Gabriela"
     (
       matrimonio(matrimonio(_,_,_,_,_,_,ladamacomisaria,mulatkagabriela),Amigos)
     )
   ]).


clues(
      barcos, % identifica las pistas como del acertijo "barcos"
      Amigos, % la estructura del acertijo va atando todo

   [
      %Primera pista: El barco griego sale a las seis y lleva cafe
     ( 
       barcos(barco(_, griego,seis,cafe,_,_),Amigos)%%
     ),
     %Segunda pista:El barco en el medio tiene una chimenea negra
     (
       barcos(barco(3,_,_,_, negra,_),Amigos)%%
     ),
     %Tercera pista: El barco ingles sale a las 9
     (
       barcos(barco(_,ingles,nueve,_,_,_),Amigos)
     ),
     %Cuarta pista: El barco frances tiene una chimenea azul
     %esta a la izquierda de un barco que transporta cafe
     (
       barcos(barco(N4,frances,_,_, azul,_),Amigos),%%
       barcos(barco(N5,_,_,cafe, _,_),Amigos),%%
       N4 is N5-1
     ),
     %Quinta pista: A la derecha del barco que lleva cacao esta un barco de marsella
     (
       barcos(barco(N6,_,_,cacao, _,_),Amigos),
       barcos(barco(N7,_,_,_,_,marsella),Amigos),
       N7 is N6+1

     ),
     %Sexta pista: La nave brasileña se encamina a manila
     (
       barcos(barco(_,brasilena,_,_, _,manila),Amigos)
     ),
     %Septima pista: Al lado del barco que lleva arroz hay un barco con una chimenea verde
     (
       barcos(barco(N9,_,_,arroz,_,_),Amigos),
       barcos(barco(N10,_,_,_, verde,_),Amigos),
       (N10 is N9+1 ; N10 is N9-1)

     ),
     %Octava pista: El barco que va a genova sale a las 5
     (
       barcos(barco(_,_,cinco,_, _,genova),Amigos)
     ),
     %Novena pista: El barco español sale a las 7 y esta a la derecha del barco de marsella
     (
       barcos(barco(N12,espanol,siete,_, _,_),Amigos),
       barcos(barco(N13,_,_,_, _,marsella),Amigos),
       N12 is N13+1

     ),
     %Decima pista: El barco con la chimenea roja va a Hamburgo
     (
       barcos(barco(_,_,_,_, roja,hamburgo),Amigos)
     ),
     %Undecima pista: Al lado del barco que sale a las siete hay un barco con chimenea blanca
     (
       barcos(barco(N15,_,siete,_, _,_),Amigos),
       barcos(barco(N16,_,_,_, blanca,_),Amigos),
       (N16 is N15+1 ; N16 is N15-1)
     ),
     %Duodecima pista: El barco de mas a la derecha lleva maiz
     (
       barcos(barco(5,_,_,maiz, _,_),Amigos)
     ),
     %Pista trece: El barco con una chimenea negra sale a las 8
     (
       barcos(barco(_,_,ocho,_, negra,_),Amigos)
     ),
     %Pista catorce: El barco que lleva maiz esta anclado al lado del barco que lleva arroz
     (
       barcos(barco(N18,_,_,maiz, _,_),Amigos),
       barcos(barco(N19,_,_,arroz, _,_),Amigos), (N18 is N19+1 ; N18 is N19-1)
     ),
     %Pista quince: El barco con destino a hamburgo sale a las seis
     (
       barcos(barco(_,_,seis,_, _,hamburgo),Amigos)
     )
                 ]).

queries(
        matrimonio, 
        Amigos, 
        
        [ 

        ],
        
        % Respuestas pedidas. Usa los valores determinados en la lista anterior.
        [
          
        ]).


% Esta relación hace consultas a la estructura común y luego
% prepara las respuestas del enunciado.
queries(
        barcos, % identifica las pistas como del acertijo "barcos"
        Amigos, % la estructura del acertijo va atando todo

        % Preguntas a la estructura
        [ 
          barcos(barco(_,NombreBarco1,_,te,_,_),Amigos),
          barcos(barco(_,NombreBarco2,_,_,_,portSide),Amigos)
        ],



        % Respuestas pedidas. Usa los valores determinados en la lista anterior.
        [
          ['El barco que transporta te es: ', NombreBarco1],
          ['El barco que va a portSide es: ', NombreBarco2]
        ]).

% Estas relaciones son usadas para generar alternativas.
% Como no se puede comparar directamente X \= Y si alguno no está
% instanciada, se debe plantear un caso específico y continuar.

% Escoge un elemento de la estructura
matrimonio(Am,Structure) :- member(Am,Structure).

% Escoge un elemento de la estructura
barcos(Am,Structure) :- member(Am,Structure).

% Escoge uno de los posibles nombres
nombre(N) :- member(N,[michael,simon,richard]).

% Escoge una de las posibles nacionalidades
nacionalidad(Nac) :- member(Nac, [americano,israeli,australiano]).

% Escoge uno de los posibles deportes
deporte(D) :- member(D,[basquetball,tenis,cricket]).





