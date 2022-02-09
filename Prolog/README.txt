COMPOSIZIONE DEL GRUPPO

- Ferrario Tommaso 869005

DESCRIZIONE
Il file uri-parse.pl implementa un semplice parse per gli URI (Uniform
Resource Identifier) sviluppato in Prolog.
Riconosce gli URI nelle forme:
- URI1 ::= scheme ':' [authority] ['/' [path] ['?' query] ['#' fragment]]
- URI2 ::= scheme ':' scheme-syntax

PREDICATI PER IL PARSING

  - uri_parser/2
  - aux_uri_parse/3
  - scheme/3
  - aux_scheme/3
  - authority_parse/3
  - read_authority/3
  - user_parse/3
  - host_parse/3
  - port_parse/2
  - path_parse/3
  - query_parse/3
  - fragment_parse/2

PREDICATI PER LA STAMPA

 - uri_display/1
 - uri_display/2
 - print_uri/2

PREDICATI PER IL CONTROLLO

 - check_host/2
 - check_host_aux/2
 - check_path/2
 - is_ip/1
 - chek_num_dots/1
 - ip/5
 - read_part/3
 - list_number/2
 - path_zos/3
 - id8/4

PREDICATI UTILI

 - dynamic uri_parse/2.
 - dynamic read_authority/3.
 - chars_to_atom/2
 - list_to_atom/2

