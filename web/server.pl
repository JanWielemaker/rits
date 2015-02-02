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
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).

:- pengine_application(rits).
:- use_module(rits:library(pengines_io)).
pengines:prepare_module(Module, rits, _Options) :-
        pengines_io:pengine_bind_io_to_html(Module).

:- http_handler(/, http_reply_from_files(., []), [prefix]).

:- use_module('../rits').

server(Port) :-
	http_server(http_dispatch, [port(Port)]).
