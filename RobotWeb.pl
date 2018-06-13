:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

% we need this module from the HTTP client library for http_read_data
:- use_module(library(http/http_client)).
:- http_handler('/', web_form, []).
:- http_handler('/cinematica', landing_pad, []).
:- http_handler('/directa', cDirecta, []).
:- http_handler('/recta', tRecta, []).
:- consult(matDH).
:- consult(icm).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    setPeriod('servo1'),
    setPeriod('servo2'),
    setPeriod('servo3'),
    setPolarity('servo1'),
    setPolarity('servo2'),
    setPolarity('servo3'),
    dCinematica(0, 90, 90, _,_,_).
    
    
    web_form(_Request) :-
	reply_html_page(
	    title('Robotica'),
	    [
	     h1('Cinematica Directa de un Brazo Robotico de tres grados de libertad y estimacion de una trayectoria recta del extremo final de robot'),
	     form([action='/cinematica', method='POST'], [
		      p([], [
			    input([name=escolha, type=radio, value='1']), 'Cinematica directa',br([]),
			    input([name=escolha, type=radio, value='4']), 'Recta', br([])
			]),
		      p([], input([name=submit, type=submit, value='Enter'], []))
		  ]),
	     br([]),
	     pre('Prolog')
	    ]).
    
landing_pad(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [escolha=E, _], []),
    % format('Content-type: text/html~n~n', []),
    % format('<p>',[]),
    atom_number(E, Opcao),
    calculos(Opcao).
    % portray_clause(Opcao).


calculos(Opcao):-
    Opcao = 1,
    reply_html_page(
	    title('Robotica'),
	    [
	     form([action='/directa', method='POST'], [
		p([], [
		  label([for=teta1],'Teta1:'),
		  input([name=teta1, type=textarea])
		      ]),
		p([], [
		  label([for=teta2],'Teta2:'),
		  input([name=teta2, type=textarea])
		      ]),
		p([], [
		  label([for=teta3],'Teta3:'),
		  input([name=teta3, type=textarea])
		      ]),
		
		p([], input([name=submit, type=submit, value='Enter'], []))
	      ])]).


calculos(Opcao):-
    Opcao = 4,
    reply_html_page(
	    title('Robotica'),
	    [
	     form([action='/recta', method='POST'], [
		p([], [
		  label([for=x],'X: '),
		  input([name=x, type=textarea])
		      ]),
		p([], [
		  label([for=y],'Y: '),
		  input([name=y, type=textarea])
		      ]),
		p([], [
		  label([for=z],'Z: '),
		  input([name=z, type=textarea])
		      ]),
		
		p([], input([name=submit, type=submit, value='Enter'], []))
	      ])]).


cDirecta(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [teta1=Teta11, teta2=Teta21, teta3=Teta31, _], []),
    format('Content-type: text/html~n~n', []),
    format('<p>',[]),
    portray_clause('Calculando a cinematica directa'),
    atom_number(Teta11, Teta1),
    atom_number(Teta21, Teta2),
    atom_number(Teta31, Teta3),
    dCinematica(Teta1, Teta2, Teta3, X, Y, Z),
    format('</p><p>========<br></br>X = ', []),
    portray_clause(X),
    format('</p><p>Y = ', []),
    portray_clause(Y),
    format('</p><p>Z = ', []),
    portray_clause(Z),
    format('</p><br></br>'),
    format('<form action="/", method="POST">
  <input type="submit" value="Regresar">
</form> ').

tRecta(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [x=X1, y=Y1, z=Z1, _], []),
    format('Content-type: text/html~n~n', []),
    format('<style>
table {
    width:100%;
}
table, th, td {
    border: 1px solid black;
    border-collapse: collapse;
}
th, td {
    padding: 5px;
    text-align: left;
}
table#t01 tr:nth-child(even) {
    background-color: #eee;
}
table#t01 tr:nth-child(odd) {
   background-color:#fff;
}
table#t01 th	{
    background-color: black;
    color: white;
}
</style>', []),
    format('<p>',[]),
    portray_clause('Trajetoria Recta'),
    atom_number(X1, X),
    atom_number(Y1, Y),
    atom_number(Z1, Z),
    write('<table id="t01">
  <tr>
    <th>X</th>
    <th>Y</th>		
    <th>Z</th>
    <th>Teta1</th>
    <th>Teta2</th>
    <th>Teta3</th>
  </tr>'),
    trajetoriaRecta(X, Y, Z),
    write('</table>'),
    format('</p><p>========<br></br> Y = ', []),
    portray_clause(Y),
    format('</p><p>Z = ', []),
    portray_clause(Z),
    format('</p><br></br>'),
    format('<form action="/", method="POST">
  <input type="submit" value="Regresar">
</form> ').

dCinematica(Teta1, Teta2, Teta3, X, Y, Z):-
    Teta11 is 90 + Teta1,
    Teta22 is - Teta2,
    Teta33 is Teta3,

    eslavonServo('servo1', _, Min1, Max1, _, _),
    Teta11 =< Max1 + 1,
    Teta11 >= Min1 - 1,

    eslavonServo('servo2', _, Min2, Max2, _, _),
    Teta2 =< Max2 + 1,
    Teta2 >= Min2 - 1,

    eslavonServo('servo3', _, Min3, Max3, _, _),
    Teta3 =< Max3 + 1,
    Teta3 >= Min3 - 1,
    
    tFinal(Teta11, Teta22, Teta33, T),
    pegarPosicao(T, X, Y, Z),
    Y >= 0,
    Z >= 0,
    Teta is 90 - Teta1,
    %setDuty('servo1', Teta11),
    %setDuty('servo2', Teta2),
    %setDuty('servo3', Teta3), 
    vaiPara(Teta, Teta2, Teta3),
!.

dCinematica(_, _, _, X, Y, Z):-
    X='Sem Solucao',
    Y='Sem Solucao',
    Z='Sem Solucao'.

    
trajetoriaRecta(X, Y, Z):-
         
    iCinematicaIr(TetaM, Teta2, Teta3, -X, Y, Z),
    conferir(-X, Y,Z, TetaM, Teta2, Teta3),
    sleep(4),
    trajetoriaRecta2(-X, Y, Z, -13).

trajetoriaRecta2(X, _, _, XF):-
    X < XF, !.

trajetoriaRecta2(X, Y, Z, XF):-

    iCinematica(TetaM, Teta2, Teta3, X, Y, Z),
    
    conferir(X, Y, Z, TetaM, Teta2, Teta3),
    sleep(0.1),
    X1 is X - 0.5,
    trajetoriaRecta2(X1, Y, Z, XF).

modulo(X,Y,Xo,Yo,M):-
{
    M * M = (X - Xo) * (X -Xo) + (Y - Yo) * (Y - Yo)
}.

vetor(X, Y, Xo, Yo,Alfa,Beta):-
    modulo(X, Y, Xo, Yo, Modulo),
    Alfa is (X - Xo) / Modulo,
    Beta is (Y - Yo) / Modulo.

trajRecta(X, Y, Xo, Yo):-
    vetor(X,Y,Xo,Yo,Alfa,Beta),
    iCinematicaIr(_, _, _, Xo, Yo, 10),
    sleep(4),
    trajRecta2(X,Y,Xo,Yo,Alfa,Beta,0).

trajRecta2(X,_,Xo,_,_,_,_):-
    X>=Xo, !. 

trajRecta2(X,Y,Xo,Yo,Alfa,Beta,T):-
    X2 is Xo + Alfa * T,
    Y2 is Yo + Beta * T,
    T1 is T + 0.01,
     
    iCinematica(TetaM, Teta2, Teta3, X2, Y2, 10),
    conferir(X2, Y2, 10, TetaM, Teta2, Teta3),
    sleep(0.05),
    trajRecta2(X,Y,X2,Y2,Alfa,Beta,T1).


conferir(X, Y, Z, TetaM, Teta2, Teta3):-
    write('<tr>'),
    write('<td>'),
    write(X),
    write('</td><td>'),
    write(Y),
    write('</td><td>'),
    write(Z),
    write('</td><td>'),
    write(TetaM),
    write('</td><td>'),
    write(Teta2),
    write('</td><td>'),
    write(Teta3),
    write('</td></tr>\n').
    
