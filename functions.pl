% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $
%Srijitha Somangili
%ssomangi

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon],
   write(List).

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

not( X ) :- X, !, fail.
not( _ ).

link(L,L).
link(A,B) :- flight(A,B,_).


make_radians(Deg, Min, Rad):-
   Degmin is Min / 60,
   Rd is Deg + Degmin,
   Rad is Rd * (pi/180). 

%getting the distance in miles
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

%distance between two flights
distance(Start,End,Length):-
   airport(Start,_,degmin(Latdeg1, Latmin1),degmin(Londeg1, Lonmin1)),
   airport(End,_,degmin(Latdeg2, Latmin2),degmin(Londeg2, Lonmin2)),
   make_radians(Latdeg1, Latmin1, Lat1),
   make_radians(Londeg1,Lonmin1,Lon1),
   make_radians(Latdeg2,Latmin2,Lat2),
   make_radians(Londeg2, Lonmin2, Lon2),
   haversine_radians(Lat1,Lon1,Lat2,Lon2,Length).
   
%time of flight
time_t(Distance,Time):-
    Time is (Distance/500)*60.

%find arrival time
is_flight(Depart,Arrive, time(Hr,Min), arrive(H,M)):-
   Begin is (Hr * 60) +Min,
   distance(Depart,Arrive, Dist),
   time_t(Dist, Tm),
   Finaltime is round(Begin + Tm),
   Final is Finaltime,
   H is floor(Finaltime / 60),
   M is mod(Final,60).

%see if two flights are connected
connected(flight(D1, A1, time(H1,M1)),
           flight(D2,_,time(H2,M2))):-
   A1 = D2,
   is_flight(D1,A1,time(H1,M1),arrive(Hr1,Mr1)),
   T1 is (Hr1 *60) +Mr1,
   T1 =< 1465,
   T2 is (H2 *60) + M2,
   T2 >= (T1 + 30).

%write hours correctly
write_hours(H) :-
   (H<10 -> write(' '),write(H);write(H)).
%write min correctly
write_min(M) :-
   (M<10 -> write('0'),write(M);write(M)).

%write in uppercase
toUp([]).

toUp([H|T]):-
   lower_upper(H,U),
   write(U),
   toUp(T).

printUpper(Str):-
   atom_chars(Str,List),
   toUp(List).

is_connected(Start,Start,Start,time(_,_), new(H,M)) :-
   flight(Start,_, time(H,M)).

is_connected(Start,Start,End,time(_,_), new(H,M)) :-
   flight(Start,End,time(X,Y)),
   H is X,
   M is Y.

is_connected(Start,Stop,End,time(H1,M1), new(H2,M2)):-
   flight(Start, Stop, time(H1,M1)),
   flight(Stop,End, time(X,Y)),
   connected(flight(Start,Stop,time(H1,M1)),
              flight(Stop,End,time(X,Y))),
    H2 is X, M2 is Y.

no_airport(A):-
   not(airport(A,_,degmin(_,_),degmin(_,_))),
   write('No such airport: '), write(A), nl, !,fail.

%main function for airplane plan
fly(Start,Start):-
   write('Invalid Path: unable to fly.'),nl,!,fail.

fly(Start,End):-
   no_airport(End);
   no_airport(Start);
   writeallpaths(Start,End).            

%function to call listpath and writepath
writeallpaths(Node, Node) :-
   write(Node), write(' is '), write(Node),nl.

writeallpaths(Node,Next) :-
   flight(Node,_, time(H,M)),
   listpath(Node, Node, Next, time(H,M), [Node], Path),
   writeout( Node, Path, H,M),!.

%gets out the first three elements in path
writeout(Node, [Head|Tail],H,M):-
   writepath(Node,Head,Tail,time(H,M)).

%write out flight of one stage
write_connection(Start,Start,End,time(_,_),new(X,Y)) :-
   flight(Start,End,time(H,M)),
   airport(Start,Str1, degmin(_,_), degmin(_,_)),
   airport(End,Str2,degmin(_,_), degmin(_,_)),
   is_flight( Start,End, time(H,M), arrive(Hr,Mr)),
   X is H, Y is M,
   write('depart  '),printUpper(Start), write('  '),write(Str1),
        write(' '), write_hours(H), write(':'), write_min(M),nl,
   write('arrive  '),printUpper(End), write('  '), write(Str2),
        write(' '), write_hours(Hr), write(':'),write_min(Mr),nl.

write_connection(Start, Stop,End, time(H1,M1), new(X,Y)) :-
   flight(Stop,End,time(H2,M2)),
   connected( flight(Start,Stop,time(H1,M1)),
              flight(Stop,End,time(H2,M2))),
   X is H2, Y is M2,
   is_flight(Stop,End, time(H2,M2), arrive(Hr2, Mr2)),
   airport(Stop,Str1,degmin(_,_), degmin(_,_)),
   airport(End,Str2,degmin(_,_), degmin(_,_)),
   write('depart  '),printUpper(Stop), write('  '),write(Str1),
         write(' '), write_hours(H2), write(':'), write_min(M2),nl,
    write('arrive  '),printUpper(End), write('  '), write(Str2),
        write(' '), write_hours(Hr2), write(':'),write_min(Mr2),nl.

%iterates through path to print full trip
writepath(_,_,[],time(_,_)):-nl.
writepath(First,Second,[Third|Tail], time(H,M)) :-
   write_connection(First,Second,Third, time(H,M), new(X,Y)),
   writepath(Second,Third,Tail, time(X,Y)).

%creates the list of each airline visited
listpath(Node,End,Outlist):-
   listpath( Node,_,End,time(_,_),[Node],Outlist).

listpath(Node,_,Node,time(_,_),_,[Node]).

listpath(Node,Prev,End,time(H,M),Tried,[Node|Path]):-
   link(Node,Next),
   is_connected(Prev,Node,Next, time(H,M), new(H2,M2)),
   not(member(Next,Tried)),
   listpath(Next,Node,End,time(H2,M2),[Next|Tried],Path).


