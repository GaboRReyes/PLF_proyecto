:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_server_files)).
:- use_module(library(pengines)).

% Base de conocimiento: Enfermedades y sus s�ntomas
enfermedad(gripe, [
    fiebre, tos, dolor_de_cabeza, dolor_muscular, fatiga
]).

enfermedad(resfriado_comun, [
    congestion_nasal, estornudos, dolor_de_garganta, tos_leve
]).

enfermedad(covid19, [
    fiebre, tos_persistente, perdida_de_olfato, perdida_de_gusto, dificultad_respiratoria
]).

enfermedad(alergia_estacional, [
    estornudos, picazon_ojos, congestion_nasal, ojos_lagrimosos
]).

enfermedad(gastroenteritis, [
    nauseas, vomitos, diarrea, dolor_abdominal, fiebre_leve
]).

% Interfaz web
:- http_handler(root(.), home, []).
:- http_handler(root(diagnosticar), diagnostico, []).
:- http_handler(root(resultado), mostrar_resultado, []).

% Guardar el ID del servidor
:- dynamic servidor_id/1.

%Encendido del servidor local
start_server(Port) :-
    http_server(http_dispatch, [port(Port), workers(1)]),
    assertz(servidor_id(Port)).

%Apaga el servidor
stop_server :-
    servidor_id(Port),
    http_stop_server(Port, []),
    retract(servidor_id(Port)),
    format('Servidor detenido en el puerto ~w.~n', [Port]).

% P�gina de inicio
home(_Request) :-
    reply_html_page(
        title('Sistema Experto de Diagnostico Medico'),
        [
            div([class='container'],
                [
                    h1('Sistema Experto de Diagnostico Medico'),
                    img([src='/imagenes/doctor.png', alt='Doctor', width='300']),
                    p('Este sistema ayuda a diagnosticar posibles enfermedades de las vias respiratorias basandonos en los sintomas.'),
                    p('Haga clic en el boton para comenzar el diagnostico:'),
                    form([action='/diagnosticar', method='POST'],
                        [input([type=submit, value='Comenzar Diagnostico', class='btn'])])
                ])
        ]).

% Proceso de diagn�stico
diagnostico(Request) :-
    member(method(post), Request),
    findall(Enfermedad, enfermedad(Enfermedad, _), Enfermedades),
    reply_html_page(
        title('Diagnostico - Preguntas'),
        [
            div([class='container'],
                [
                    h1('Diagnostico de Enfermedades'),
                    img([src='/imagenes/sintomas.png', alt='Sintomas', width='300']),
                    p('Por favor responda las siguientes preguntas:'),
                    form([action='/resultado', method='POST'],
                        [
                            ul([], maplist(crear_pregunta, Enfermedades)),
                            input([type=submit, value='Obtener Diagnostico', class='btn'])
                        ])
                ])
        ]).

crear_pregunta(Enfermedad) :-
    enfermedad(Enfermedad, Sintomas),
    li([
        h2(Enfermedad),
        ul([], maplist(crear_opcion(Enfermedad), Sintomas))
    ]).

crear_opcion(Enfermedad, Sintoma) :-
    li([input([type=checkbox, name=Sintoma, value='true']), ' ', label([for=Sintoma], Sintoma)]).

% Mostrar resultados
mostrar_resultado(Request) :-
    member(method(post), Request),
    findall(Sintoma, (member(Sintoma=_, Request), atom(Sintoma)), SintomasPresentes),
    findall(Enfermedad, (
        enfermedad(Enfermedad, Sintomas),
        subset(Sintomas, SintomasPresentes)
    ), EnfermedadesPosibles),
    reply_html_page(
        title('Resultados del Diagnostico'),
        [
            div([class='container'],
                [
                    h1('Resultados del Diagnostico'),
                    img([src='/imagenes/resultados.png', alt='Resultados', width='300']),
                    h2('Enfermedades posibles:'),
                    ul([], maplist(li, EnfermedadesPosibles)),
                    p('NOTA: Este es solo un sistema experto educativo. Consulte a un medico para un diagnostico real.'),
                    a([href='/'], 'Volver al inicio')
                ])
        ]).

:- absolute_file_name(imagenes, ImgDir, [file_type(directory), access(read)]),
   http_handler(root(imagenes), http_reply_from_files(ImgDir, []), [prefix]).

