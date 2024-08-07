% El parcial Vocaloid.

%canta(Cantante, Cancion).
%cancion(NombreCancion, Duracion):

% megurineLuka sabe cantar la canción nightFever cuya duración es de 4 min y también canta la canción foreverYoung que dura 5 minutos.	
% hatsuneMiku sabe cantar la canción tellYourWorld que dura 4 minutos.
% gumi sabe cantar foreverYoung que dura 4 min y tellYourWorld que dura 5 min
% seeU sabe cantar novemberRain con una duración de 6 min y nightFever con una duración de 5 min.
% kaito no sabe cantar ninguna canción.

% Tener en cuenta que puede haber canciones con el mismo nombre pero con diferentes duraciones

canta(megurineLuka, cancion(nightFever,4)).
canta(megurineLuka, cancion(foreverYoung, 5)).
canta(hatsuneMiku, cancion(tellYourWorld, 4)).
canta(gumi, cancion(foreverYoung, 4)).
canta(gumi, cancion(tellYourWorld, 5)).
canta(seeU, cancion(novemberRain, 6)).
canta(seeU, cancion(nightFever, 5)).
% kaito NO sabe cantar ninguna cancion --> entonces no lo agrego a la base de conocimientos!!

% 1) Para comenzar el concierto, es preferible introducir primero 
% a los cantantes más novedosos, por lo que necesitamos un predicado 
% para saber si un vocaloid es novedoso cuando saben al menos 2 
% canciones y el tiempo total que duran todas las canciones debería
% ser menor a 15.

novedoso(Vocaloid) :-
    cantaAlMenosDosCanciones(Vocaloid),
    tiempoTotalCanciones(Vocaloid, Tiempo),
    Tiempo < 15.

cantaAlMenosDosCanciones(Vocaloid) :-
    canta(Vocaloid, Cancion1),
    canta(Vocaloid, Cancion2),
    Cancion1 \= Cancion2.

tiempoTotalCanciones(Cantante, TiempoTotal) :-
    findall(Tiempo, tiempoCancion(Cantante, Tiempo), ListaDeTiempos),
    sum_list(ListaDeTiempos, TiempoTotal).

tiempoCancion(Cantante, Tiempo) :-
    canta(Cantante, Cancion),
    tiempo(Cancion, Tiempo).    

tiempo(cancion(_, Tiempo), Tiempo).   

% 2) Hay algunos vocaloids que simplemente no quieren cantar canciones
%  largas porque no les gusta, es por eso que se pide saber si un 
% cantante es acelerado, condición que se da cuando todas sus
% canciones duran 4 minutos o menos. Resolver sin usar forall/2

vocaloid(Cantante) :- canta(Cantante, _).

acelerado(Cantante) :-
    vocaloid(Cantante),
    not((canta(Cantante, Cancion), tiempo(Cancion, Tiempo), Tiempo > 4 )).
    % NO tiene canciones que tengan una duracion mayor a 4 minutos

% Si nos permitirian usar el forall seria:
aceleradoFORALL(Cantante) :-
    vocaloid(Cantante),
    forall((canta(Cantante, Cancion), tiempo(Cancion, Tiempo)), Tiempo =< 4).

% ”No canta una canción que dure más de 4 minutos” (not/1) es lo mismo que “Todas sus canciones duran 4 o menos minutos” (forall/2).

% --- CONCIERTOS ----
%concierto(NombreConcierto, PaisDondeSeHace, CantidadDeFama, TipoDeConcierto)

% Tipos de concierto:
% - gigante(cantMininaDeCanciones, DuracionTotal)
% - mediano(DuracionTotal)
% - pequeño(CantidadDada)

% 1) Modelar los conciertos y agregar en la base de conocimiento todo lo necesario

concierto(mikuExpo, eeuu, 2000, gigante(2, 6)).
concierto(magicalMirai, japon, 3000, gigante(3, 10)).
concierto(vocalektVisions, eeuu, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequenio(4)).

% 2) Se requiere saber si un vocaloid puede participar en un concierto,
% esto se da cuando cumple los requisitos del tipo de concierto. 
% También sabemos que Hatsune Miku puede participar en cualquier 
% concierto.

puedeParticipar(Cantante, Concierto) :-
    vocaloid(Cantante),
    Cantante \= hatsuneMiku,
    concierto(Concierto, _, _, TipoDeConcierto),
    cumpleConLosRequisitos(Cantante, TipoDeConcierto).

puedeParticipar(hatsuneMiku, Concierto) :- concierto(Concierto,_,_,_).

cumpleConLosRequisitos(Cantante, gigante(CantidadMinima, DuracionMinima)) :-
    cantidadDeCanciones(Cantante, Cantidad), 
    CantidadMinima =< Cantidad,
    tiempoTotalCanciones(Cantante, DuracionTotal),
    DuracionMinima =< DuracionTotal.

cumpleConLosRequisitos(Cantante, mediano(DuracionMaxima)) :-
    tiempoTotalCanciones(Cantante, DuracionTotal),
    DuracionTotal < DuracionMaxima.

cumpleConLosRequisitos(Cantante, pequenio(DuracionMinima)) :-
    canta(Cantante, Cancion),
    tiempo(Cancion, DuracionCancion),
    DuracionMinima < DuracionCancion.

cantidadDeCanciones(Cantante, Cantidad) :-
    findall(Cancion, canta(Cantante, Cancion), Canciones),
    length(Canciones, Cantidad).

% 3) Conocer el vocaloid más famoso, es decir con mayor nivel de fama.
% El nivel de fama de un vocaloid se calcula como la fama total que 
% le dan los conciertos en los cuales puede participar multiplicado 
% por la cantidad de canciones que sabe cantar

masFamoso(Cantante) :-
    vocaloid(Cantante),
    nivelDeFama(Cantante, NivelFamoso),
    forall((vocaloid(OtroCantante), OtroCantante \= Cantante), (nivelDeFama(OtroCantante, OtroNivel), NivelFamoso >= OtroNivel)).

masFamosoV1(Cantante) :-
    vocaloid(Cantante),
    forall((vocaloid(OtroCantante), OtroCantante \= Cantante), tieneMasNivelDeFamaQue(Cantante, OtroCantante)).

tieneMasNivelDeFamaQue(Cantante, OtroCantante) :-           % CONSULTAR PORQUE NO PUEDO USAR ESTE!!
    nivelDeFama(Cantante, Nivel),
    nivelDeFama(OtroCantante, NivelOtro),
    Nivel >= NivelOtro.

masFamosoV2(Cantante) :-
    nivelDeFama(Cantante, NivelFamoso),
    forall(nivelDeFama(_, OtroNivel), NivelFamoso >= OtroNivel).    % para todos los Otros Niveles Existentes, son menores al Nivel del mas famoso!!

nivelDeFama(Cantante, NivelFama) :-
    famaTotal(Cantante, FamaTotal),
    cantidadDeCanciones(Cantante, Cantidad),
    NivelFama is FamaTotal * Cantidad.

famaTotal(Cantante, FamaTotal) :-         % la fama total que me da la suma de todas las famas de los conciertos que puedo participar
    vocaloid(Cantante),
    findall(Fama, famaConcierto(Cantante, Fama), Famas),
    sum_list(Famas, FamaTotal).
    
famaConcierto(Cantante, FamaConcierto) :- % la fama que me da un concierto que puedo participar
    puedeParticipar(Cantante, Concierto),
    concierto(Concierto,_,FamaConcierto,_).

    














