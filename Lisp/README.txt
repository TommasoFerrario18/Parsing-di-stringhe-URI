COMPOSIZIONE DEL GRUPPO

- Ferrario Tommaso 869005

DESCRIZIONE
Il file uri-parse.lisp implementa un semplice parse per gli URI (Uniform
Resource Identifier) sviluppato in Common Lisp.
Riconosce gli URI nelle forme:
- URI1 ::= scheme ':' [authority] ['/' [path] ['?' query] ['#' fragment]]
- URI2 ::= scheme ':' scheme-syntax

FUNZIONI
- uri-parse:	string -> uri-structure
- uri-scheme: 	uri-structure -> string
- uri-userinfo:	uri-structure -> string
- uri-host: 	uri-structure -> string
- uri-port: 	uri-structure -> integer
- uri-path: 	uri-structure -> string
- uri-query: 	uri-structure -> string
- uri-fragment:	uri-structure -> string
- uri-display: 	uri-structure &optional stream -> T

FUNZIONI PER IL PARSING
- scheme:		list -> list
  Questa funzione prende in input una lista e restituisce una
  lista composta da liste di caratteri che rappresentano lo scheme
  e il resto dell'URI.
  
- aux-uri-parse:     	list string -> uri-structure senza 'URI
  Questa funzione prende in input una lista di caratteri e una
  stringa che rappresenta lo scheme.
  In base allo scheme procede con il parsing.
  Restituisce una lista di stringhe e numeri che rappresentano le
  parti dell'URI.
  
- authority:		list -> authority-structure optional-list
  Questa funzione prende in input una lista ricava l'authority e controlla
  la presenza delle userinfo.
  
- aux-authority:	list -> authority-structure
  Questa funzione si occupa di fare il parsing del blocco authority
  dell'URI.
  
- user: 		list &optional scheme -> list(user host port)
  Questa funzione prende in input una lista e restituisce una
  lista composta da liste di caratteri che rappresentano le userinfo,
  l'host e la port.
  
- host:      		list -> list(host port)
  Questa funzione prende in input una lista e restituisce una
  lista composta da liste di caratteri che rappresentano l'host
  e la port.
  
- port:      		list -> list(port)
  Questa funzione prende in input una lista e restituisce una
  lista di caratteri che rappresenta la port.
  
- path:	     		list -> list(path query fragment)
  Questa funzione prende in input una lista e restituisce una
  lista composta da liste di caratteri che rappresentano il path,
  la query e il fragment.
  
- query:     		list -> list(query fragment)
  Questa funzione prende in input una lista e restituisce una
  lista composta da liste di caratteri che rappresentano la query
  e il fragment.
  
- fragment:   		list -> list(fragment)
  Questa funzione prende in input una lista e restituisce una
  lista di caratteri che rappresenta il fragment.

FUNZIONI DI AIUTO
- print-uri:		list stream -> T
  Questa funzione prende in input una lista e uno stream e stampa
  tutti gli elemnti della lista sullo stream.
  
- str-to-num:		list -> list
  Questa funzione prende in input una lista e fa il parsing del primo
  elemento da stringa a intero.
  
- concat: 		list char -> list
  Queste due funzioni sono usate per concatenare un valore in
  una lista di liste.
- add:			list char -> list

- between:	    	integer integer integer -> T | NIL
  Questa funzione prende in input tre numeri interi e restituisce
  T se il terzo argomento è compreso tra i primi due, NIL altrimenti.
  
- list-to-string:	list(list-chars) -> list(string)
  Questa funzione prende in input una lista composta da liste di
  caratteri. Restituisce una lista di stringhe formate partendo
  dalle liste di caratteri.
  
- read-authority:	list -> list
  Questa funzione prende in input una lista. Restituisce una lista
  composta da due liste che rappresentano l'authority e la parte
  di URI ancora da parsare.

FUNZIONI DI CONTROLLO
- uri-p:	     uri-structure -> T | NIL
  Questa funzione verifica se il paraetro passato in input è
  un uri-structure.
  
- check-host:	     list -> T
  Questa funzione prende in input una lista, verifica che tale lista
  rappresenta un IPv4 o meno.
  
- aux-check-host:    list -> T
  Questa funzione prende in input una lista, verifica che tale lista
  rispetti la grammatica che rappresenta l'host.
  
- check-path:	     list -> T
  Questa funzione prende in input una lista, verifica che tale lista
  rispetti la grammatica che rappresenta il path.
  
- ip-p		     list -> T
  Questa funzione prende in input una lista, verifica che tale lista
  corrisponde a un indirizzo IPv4 valido.
  
- ip-part: 	     list -> list
  Questa funzione prende in input una lista di caratteri. Restituisce
  una sotto-lista dell'input composta dai caratteri fino al carattere
  punto.
  
- only-digit-dots:   list -> T
  Questa funzione prende in input una lista di caratteri. Controlla
  se tale lista è composta solo da caratteri che rappresentano numeri
  o punti.
  
- is-ip-p: 	     list -> T
  Questa funzione prende in input una lista di interi, per ognuno di
  essi verifica se è compreso tra 0 e 255.
  
- path-zos-p:	     list -> T
  Questa funzione accetta in input una lista di caratteri, e verifica
  se rispetta la grammatica relativa allo schema zos per la parte id44
  del path.
  
- id8-p:	     list -> T
  Questa funzione accetta in input una lista di caratteri, e verifica
  se rispetta la grammatica relativa allo schema zos per la parte id8
  del path.
