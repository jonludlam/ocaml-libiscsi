(* Ctypes interface to libiscsi *)

open Ctypes
open Foreign

type context = unit ptr
let context : context typ = ptr void

let create_context =
  foreign "iscsi_create_context" (string @-> (returning context))

let destroy_context = 
  foreign "iscsi_destroy_context" (context @-> (returning int))

let get_fd ctx = 
  let get_fd_int =
    foreign "iscsi_get_fd" (context @-> (returning int)) in
  let fd : Unix.file_descr = Obj.magic (get_fd_int ctx) in
  fd

let which_events =
  foreign "iscsi_which_events" (context @-> returning int) 

type pollfd
let pollfd : pollfd structure typ = structure "pollfd"
let fd = pollfd *:* int
let events = pollfd *:* short
let revents = pollfd *:* short
let () = seal pollfd

let poll_one (myfd : Unix.file_descr) myevents =
  let myfd : int = Obj.magic myfd in
  let fds = make pollfd in
  setf fds fd myfd;
  setf fds events myevents;
  setf fds revents 0;
  let poll_int = 
    foreign "poll" (ptr pollfd @-> int @-> int @-> (* returning_checking_errno *) returning int) in
  let result = poll_int (addr fds) 1 (-1) in
  (* Result should never be zero (timeout) *)
  getf fds revents

let service =
  foreign "iscsi_service" (context @-> int @-> (returning int))

let queue_length =
  foreign "iscsi_queue_length" (context @-> (returning int))

(*let set_timeout =
  foreign "iscsi_set_timeout" (context @-> int @-> (returning int))*)

let set_tcp_keepalive =
  foreign "iscsi_set_tcp_keepalive" (context @-> int @-> int @-> int @-> (returning int))

type url
let url : url structure typ = structure "url"
let url_portal = url *:* string
let url_target = url *:* string
let url_user = url *:* string
let url_passwd = url *:* string
let url_lun = url *:* int
let url_context = url *:* context
let () = seal url

let parse_full_url ctx myurl =
  let parse_full_url_int = 
    foreign "iscsi_parse_full_url" (context @-> string @-> returning (ptr url)) in
  let ptr = parse_full_url_int ctx myurl in
  if (to_voidp ptr)=null then failwith "Failed to parse url" else !@ ptr

let destroy_url ctx url =
  (* Need to check on lifetimes of c-allocated objects *)
  failwith "Unimplemented"

module Immediate_data = struct
  type t = No | Yes

  let int_of_t t = match t with | No -> 0 | Yes -> 1
  let t_of_int i = match i with | 0 -> No | 1 -> Yes | _ -> failwith (Printf.sprintf "Unknown Immediate_data value %d" i)
end

let set_immediate_data ctx immediate_data =
  let i = Immediate_data.int_of_t immediate_data in
  let set_immediate_data_int = 
    foreign "iscsi_set_immediate_data" (context @-> int @-> returning int)
  in set_immediate_data_int ctx i

module Initial_r2t = struct
  type t = No | Yes

  let int_of_t t = match t with | No -> 0 | Yes -> 1
  let t_of_int i = match i with | 0 -> No | 1 -> Yes | _ -> failwith (Printf.sprintf "Unknown Initial_r2t value %d" i)
end

let set_initial_r2t ctx initial_r2t =
  let i = Initial_r2t.int_of_t initial_r2t in
  let set_initial_r2t_int = 
    foreign "iscsi_set_initial_r2t" (context @-> int @-> returning int)
  in set_initial_r2t_int ctx i

let get_error =
  foreign "iscsi_get_error" (context @-> returning string)
	
let command_cb = context @-> int @-> ptr void @-> ptr void @-> returning void

let connect_async =
  foreign "iscsi_connect_async" (context @-> string @-> funptr command_cb @-> ptr void @-> returning int)

let connect_sync =
  foreign "iscsi_connect_sync" (context @-> string @-> returning int)

module Session_type = struct
  type t = Discovery | Normal

  let int_of_t t = match t with | Discovery -> 1 | Normal -> 2
  let t_of_int i = match i with | 1 -> Discovery | 2 -> Normal | _ -> failwith (Printf.sprintf "Unknown Session_type %d" i)
end

let set_session_type ctx session_type =
  let i = Session_type.int_of_t session_type in
  let set_session_type_int = 
    foreign "iscsi_set_session_type" (context @-> int @-> returning int) 
  in set_session_type_int ctx i

let login_sync =
  foreign "iscsi_login_sync" (context @-> returning int)

let login_async =
  foreign "iscsi_login_async" (context @-> funptr command_cb @-> ptr void @-> returning int)

let logout_async =
  foreign "iscsi_logout_async" (context @-> funptr command_cb @-> ptr void @-> returning int)

let discovery_async =
  foreign "iscsi_discovery_async" (context @-> funptr command_cb @-> ptr void @-> returning int)

let set_targetname = 
  foreign "iscsi_set_targetname" (context @-> string @-> returning int)

type discovery_address
let discovery_address : discovery_address structure typ = structure "discovery_address"
let discovery_address_next = discovery_address *:* ptr discovery_address
let discovery_address_target_name = discovery_address *:* string
let discovery_address_target_address = discovery_address *:* string
let () = seal discovery_address

						    

