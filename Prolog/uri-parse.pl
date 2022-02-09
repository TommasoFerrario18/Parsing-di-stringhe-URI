%%% -*- Mode: Prolog -*-
%%% Tommaso Ferrario 869005
%%% They are used to indicate where to assert uri_parse/2 and

%%% read_authority/3 predicates.
:- dynamic uri_parse/2.
:- dynamic read_authority/3.

%%% uri_parser/2
%%%
%%% uri_parser(URIString, URI)is true if URIString can be split up
%%% into the compound term:
%%% URI = uri(Scheme, Userinfo, Host, Port, Query, Fragment)
uri_parse(URIString,
	  uri(DScheme, User, Host, Port, Path, Query, Fragment)) :-
    string_chars(URIString, ListChars),
    chars_to_atom(ListChars, AtomList),
    scheme(AtomList, Scheme, RestUriChars),
    downcase_atom(Scheme, DScheme),
    aux_uri_parse(DScheme, RestUriChars,
		  uri_no_scheme(User, Host, Port, Path, Query,
				Fragment)),
    asserta((uri_parse(URIString,
		       uri(Scheme, User, Host, Port, Path, Query,
			   Fragment)) :- !)).

%%% uri_display/1
%%%
%%% Print the various parts of the uri to the default output stream.
uri_display(URI) :- uri_display(URI, user_output).

%%% uri_display/2
%%%
%%% Print the various parts of the uri on a file.
uri_display(URI, Stream) :-
    is_stream(Stream), print_uri(Stream, URI), close(Stream).

%%% print_uri/2
%%%
%%%
print_uri(Out, uri(Scheme, User, Host, Port, Path, Query, Fragment)) :-
    writeln(Out, "Display URI:"),
    write(Out, "\tScheme: "), writeln(Out, Scheme),
    write(Out, "\tUserinfo: "), writeln(Out, User),
    write(Out, "\tHost: "), writeln(Out, Host),
    write(Out, "\tPort: "), writeln(Out, Port),
    write(Out, "\tPath: "), writeln(Out, Path),
    write(Out, "\tQuery: "), writeln(Out, Query),
    write(Out, "\tFragment: "), writeln(Out, Fragment).

%%% aux_uri_parse/3
%%%
%%% This predicate allows me to recognize the remaining parts of
%%% the uri. It takes the choice of what to do according to the
%%% scheme or the characters present immediately after the ":".
aux_uri_parse([], _, _) :- !, fail.
aux_uri_parse(_, [], uri_no_scheme([], [], 80, [], [], [])) :- !.
%%% Scheme "mailto"
aux_uri_parse(mailto, Chars,
	      uri_no_scheme(User, Host, 80, [], [], [])) :- !,
    read_authority(Chars, Authority_chars, _RestURIChars),
    Authority_chars \= [],
    user_parse(Authority_chars, UserList, OtherChars),
    UserList \= [],
    host_parse(OtherChars, host(HostList, _)),
    list_to_atom(HostList, Host),
    list_to_atom(UserList, User).
%%% Scheme "news"
aux_uri_parse(news, Chars, uri_no_scheme([], Host, 80, [], [], [])) :- !,
    host_parse(Chars, host(HostList, _)),
    check_host(HostList, _),
    list_to_atom(HostList, Host).
%%% Scheme "tel"
aux_uri_parse(tel, Chars, uri_no_scheme(User, [], 80, [], [], [])) :- !,
    user_parse(Chars, UserList, _),
    list_to_atom(UserList, User).
%%% Scheme "fax"
aux_uri_parse(fax, Chars, uri_no_scheme(User, [], 80, [], [], [])) :- !,
    user_parse(Chars, UserList, _),
    list_to_atom(UserList, User).
%%% Scheme "zos"
aux_uri_parse(zos, [(/), (/)], _) :- !, fail.
aux_uri_parse(zos, [(/), (/) | T], _) :-
    authority_parse(T, _, []), last(T, (/)), !, fail.
aux_uri_parse(zos, [(/), (/) | T],
	      uri_no_scheme(User, Host, Port, [], [], [])) :-
    authority_parse(T, authority_part(User, Host, Port), Out),
    Out = [], !.
aux_uri_parse(zos, [(/), (/) | T],
	      uri_no_scheme(User, Host, Port, Path, Query, Fragment)) :-
    authority_parse(T, authority_part(User, Host, Port), Other),
    path_parse(Other, optional(PathC, QueryC, FragmentC)),
    !, path_zos(PathC, 0),
    list_to_atom(PathC, Path),
    list_to_atom(QueryC, Query),
    list_to_atom(FragmentC, Fragment).
aux_uri_parse(zos, [(/)| T],
	      uri_no_scheme([], [], 80, Path, Query, Fragment)) :- !,
    path_parse(T, optional(PathC, QueryC, FragmentC)),
    !, path_zos(PathC, 0),
    list_to_atom(PathC, Path),
    list_to_atom(QueryC, Query),
    list_to_atom(FragmentC, Fragment).
aux_uri_parse(_,[(/), (/)], _) :- !, fail.
aux_uri_parse(_,[(/), (/) | T],
	      uri_no_scheme(User, Host, Port, Path, Query, Fragment)) :-
    !, authority_parse(T, authority_part(User, Host, Port),
		       OptionalChar),
    path_parse(OptionalChar, optional(PathC, QueryC, FragmentC)),
    check_path(PathC, _), !,
    list_to_atom(PathC, Path),
    list_to_atom(QueryC, Query),
    list_to_atom(FragmentC, Fragment).
aux_uri_parse(_, [(/) | T],
	      uri_no_scheme([], [],  80, Path, Query, Fragment)) :- !,
    path_parse(T, optional(PathC, QueryC, FragmentC)),
    check_path(PathC, _), !,
    list_to_atom(PathC, Path),
    list_to_atom(QueryC, Query),
    list_to_atom(FragmentC, Fragment).

%%% scheme/3
%%%
%%% It receives a list of characters in input and returns the scheme
%%% recognized as an atom and the characters remaining after
%%% recognition.
scheme(ListChar, Scheme, Rest) :-
    aux_scheme(ListChar, ListOut, Rest),
    list_to_atom(ListOut, Scheme).

%%% aux_scheme/3
%%%
%%% It input a list of atoms. On this list it recognizes the scheme,
%%% it returns it as a list. In addition to this returns the list
%%% with unused atoms.
aux_scheme([], [], []) :- !, fail.
aux_scheme([(:)| T], [], T) :- !.
aux_scheme([H | T], [H | Rest], OtherChar) :-
    H \= (/), H \= (?), H \= (#), H \= (:), !,
    aux_scheme(T, Rest, OtherChar).

%%% authority_parse/3
%%%
%%% It input a list of characters and returns the scheme and the
%%% remaining characters.
%%% Passes the list to a predicate that recognizes the scheme.
%%% Then checks the validity and converts the list to an atom.
authority_parse([], authority_part([], [], []), []) :- !.
authority_parse(Chars, authority_part(User, Host, Port),
		RestURIChars) :-
    read_authority(Chars, Authority_chars, RestURIChars),
    authority(Authority_chars, author(UserList, HostList, PortList)),
    !, check_host(HostList, _),
    list_to_atom(UserList, User),
    list_to_atom(HostList, Host),
    list_to_atom(PortList, PortA),
    atom_number(PortA, Port),
    between(0, 65536, Port).

%%% read_authority/3
%%%
%%% It picks up a list of atoms and recognizes the entire authority
%%% block. Returns the authority block and the rest of the list.
read_authority([], [], []) :- !.
read_authority([(/) | T], [], T) :- !.
read_authority([H | T], [H | Rest], Other) :-
    read_authority(T, Rest, Other).

%%% authority/2
%%%
%%%
authority([(@) | _], _) :- !, fail.
authority(List, author([], User, Port)) :-
    aux_authority(List, autho(User, [], Port)), !.
authority(List, author(User, Host, Port)) :-
    aux_authority(List, autho(User, Host, Port)), !.

%%% aux_authority/2
%%%
%%%
aux_authority([], autho([], [], ['8', '0'])) :- !.
aux_authority([(@), (:) | _], _) :- !, fail.
aux_authority([(@)], _) :- !, fail.
aux_authority([(@) | T], autho([], Host, Port)) :- !,
    host_parse(T, host(Host, Port)).
aux_authority([(:)], autho([], [], [])) :- !, fail.
aux_authority([(:) | T], autho([], [], Port)) :- !,
    port_parse(T, Port).
aux_authority([H | T], autho([H | Rest], Host, Port)) :-
    H \= (/), H \= (?), H \= (#), H \= (:), !,
    aux_authority(T, autho(Rest, Host, Port)).

%%% user_parse/3
%%%
%%% It input a list of atoms. On this list it recognizes userinfo,
%%% returns them as list of atoms. In addition to this returns the
%%% list with unused atoms.
user_parse([], [], []) :- !.
user_parse([(@)], _, _) :- !, fail.
user_parse([(@) | T], [], T) :- !.
user_parse([H | T], [H | UserChars], RestAuto) :-
    H \= (/), H \= (?), H \= (#), H \= (:), !,
    user_parse(T, UserChars, RestAuto).

%%% host_parse/3
%%%
%%% It input a list of atoms. On this list it recognizes the host,
%%% it returns it as a list of atoms. In addition to this returns
%%% the list with unused atoms.
host_parse([], host([], ['8', '0'])) :- !.
host_parse([(:)], host([], _)) :- !, fail.
host_parse([(:) | T], host([], Port)) :- !,
    port_parse(T, Port).
host_parse([H | T], host([H | Rest], Port)) :-
    H \= (/), H \= (?), H \= (#), H \= (:), H \= (@), !,
    host_parse(T, host(Rest, Port)).

%%% port_parse/2
%%%
%%% It input a list of atoms. On this list it recognizes the port
%%% and returns it as a list of atoms. Besides that, it controls
%%% that each atom has a number..
port_parse([], []) :- !.
port_parse([(/) | _], []) :- !.
port_parse([H | T], [H | Rest]) :-
    char_type(H, digit),
    port_parse(T, Rest).

%%% path_parse/3
%%%
%%% It input a list of atoms. On this list it recognizes the path
%%% and returns it as a list of atoms that belongs to a structure
%%% called optional. This structure contains in addition to the
%%% path also the query and fragment if present.
path_parse([], optional([], [], [])) :- !.
path_parse([(#)], _) :- !, fail.
path_parse([(?)], _) :- !, fail.
path_parse([(#) | T], optional([], [], Frag)) :- !,
    fragment_parse(T, Frag).
path_parse([(?) | T], optional([], Query, Frag)) :- !,
    query_parse(T, query_part(Query, Frag)).
path_parse([H | T], optional([H | Rest], Query, Fragment)) :-
    H \= (@), H \= (?), H \= (#), H \= (:), !,
    path_parse(T, optional(Rest, Query, Fragment)).

%%% query_parse/3
%%%
%%% It input a list of atoms. In this list it recognizes the query
%%% and returns it as a list of atoms belonging to a structure called
%%% query_part. This structure contains in addition to the query also
%%% the fragment if present.
query_parse([], query_part([], [])) :- !.
query_parse([(#)], _) :- !, fail.
query_parse([(#) | T], query_part([], Frag)) :- !,
    fragment_parse(T, Frag).
query_parse([H | T], query_part([H | Rest], Frag)) :-
    H \= (#), !,
    query_parse(T, query_part(Rest, Frag)).

%%% fragment_parse/2
%%%
%%% It input a list of atoms. On this list it recognizes the
%%% fragment, it returns it as list of atoms.
fragment_parse([], []) :- !.
fragment_parse([H | T], [H | Rest]) :-
    fragment_parse(T, Rest).

%%% chars_to_atom/2
%%%
%%% It take in input a list of characters and returns a list of atoms.
chars_to_atom([], []) :- !.
chars_to_atom([H | T], [A | Rest]) :-
    chars_to_atom(T, Rest),
    atom_codes(A, [H]).

%%% list_to_atom/2
%%%
%%% It take in input a list and returns an atom.
list_to_atom([], []) :- !.
list_to_atom(List, Atom) :-
    atomic_list_concat(List, Atom).

%%% check_host/2
%%%
%%%
check_host(List, []) :-
    check_num_dots(List),
    length(List, 15),
    is_ip(List), !.
check_host(List, _) :-
    check_host_aux(List, _).

%%% check_host_aux/2
%%%
%%%
check_host_aux([], []) :- !.
check_host_aux(['.'], _) :- !, fail.
check_host_aux(['.' | T], []) :- !,
    check_host_aux(T, _).
check_host_aux([H | T], [H | Rest]) :- !,
    check_host_aux(T, Rest).

%%% check_path/2
%%%
%%%
check_path([], []) :- !.
check_path(['/'], _) :- !, fail.
check_path(['/' | T], []) :- !,
    check_path(T, _).
check_path([H | T], [H | Rest]) :- !,
    check_path(T, Rest).

%%% is_ip/1
%%%
%%% It takes a list in input and checks if it is an ip and if the
%%% value of each NNN block is between 0 and 255.
is_ip(List) :-
    ip(List, Uno, Due, Tre, Quattro),
    between(0, 255, Uno),
    between(0, 255, Due),
    between(0, 255, Tre),
    between(0, 255, Quattro), !.
is_ip(_) :- fail.

%%% chek_num_dots/1
%%%
%%% Check if the list passed to the input contains only numbers
%%% and "." points.
check_num_dots([]) :- !.
check_num_dots([H | T]) :-
    char_type(H, digit), !,
    check_num_dots(T).
check_num_dots(['.']) :- !, fail.
check_num_dots(['.' | T]) :- !,
    check_num_dots(T).
%%% ip/5
%%%
%%% It reads the various parts of the ip and converts them into
%%% numbers.
ip(List, P1, P2, P3, P4) :-
    read_part(List, LP1, RestP1), list_number(LP1, P1),
    read_part(RestP1, LP2, RestP2), list_number(LP2, P2),
    read_part(RestP2, LP3, RestP3), list_number(LP3, P3),
    read_part(RestP3, LP4, _RestP4), list_number(LP4, P4).

%%% read_part/3
%%%
%%% Predicate used to read the various parts of the ip.
read_part([], [], []) :- !.
read_part(['.' | T], [], T) :- !.
read_part([H | T], [H | Xs], Rest) :-
    read_part(T, Xs, Rest).

%%% list_number/2
%%%
%%% Preached to convert a list into numbers.
list_number(List, Num) :-
    atomic_list_concat(List, Atom),
    atom_number(Atom, Num).

%%% path_zos/3
%%%
%%% Predicate to read the path in case the scheme and zos,
%%% with their respective controls.
path_zos([], 0) :- !, fail.
path_zos([], Acc) :- !, Acc =< 44.
path_zos(['.'], _) :- !, fail.
path_zos(['(' | T], Acc) :-
    Acc =< 44, !, id8(T, 0).
path_zos([H], Acc) :-
    Acc < 44, !, char_type(H, alnum).
path_zos([(/) | T], 0) :- !,
    AccUp is 0 + 1,
    path_zos(T, AccUp).
path_zos(['.' | T], Acc) :- !,
    AccUp is Acc + 1,
    path_zos(T, AccUp).
path_zos([H | T], 0) :- !,
    char_type(H, alpha), AccUp is 0 + 1,
    path_zos(T, AccUp).
path_zos([H | T], Acc) :- !,
    char_type(H, alnum), AccUp is Acc + 1,
    path_zos(T, AccUp).

%%% id8/4
%%%
%%% Predicate to read the id8 opional part of the path in the case
%%% of scheme zos.
id8([')'], Acc) :- !, Acc =< 8.
id8([')' | _], _) :- !, fail.
id8([H | T], 0) :- !,
    AccUp is 0 + 1, char_type(H, alpha),
    id8(T, AccUp).
id8([H | T], Acc) :-
    AccUp is Acc + 1, char_type(H, alnum),
    id8(T, AccUp).
