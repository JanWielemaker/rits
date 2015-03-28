/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   HTTP server.

   Example usage to spawn a server on port 5050:

       $ swipl -f server.pl -g "server(5050)"

   then direct your browser to http://localhost:5050
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(server,
	  [ server/1			% +Port
	  ]).

:- use_module(library(pengines)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).

:- pengine_application(rits_web).
:- use_module(rits_web:library(pengines)).
:- use_module(rits_web:library(pengines_io)).
:- use_module(rits_web:'../rits').
pengines:prepare_module(Module, rits_web, _Options) :-
        pengines_io:pengine_bind_io_to_html(Module).

:- http_handler(/, http_reply_from_files(., []), [prefix]).


server(Port) :-
	http_server(http_dispatch, [port(Port)]).
