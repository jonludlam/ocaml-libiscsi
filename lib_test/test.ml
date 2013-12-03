open Ctypes

let discovery_cb ctx status command_data private_data =
  if status <> 0 then failwith (Iscsilib.get_error ctx);
  let rec inner addr =
    if to_voidp addr = null 
    then () 
    else begin
      let target = getf !@addr Iscsilib.discovery_address_target_name in
      let address = getf !@addr Iscsilib.discovery_address_target_address in
      let next = getf !@addr Iscsilib.discovery_address_next in
      Printf.printf "address=%s\ntarget=%s\n\n%!" address target;
    end
  in inner (from_voidp Iscsilib.discovery_address command_data);
  Iscsilib.logout_async ctx (fun ctx status command_data private_data -> ()) null;
  ()
    
let discovery_login_cb ctx status command_data private_data =
  let result = Iscsilib.discovery_async ctx discovery_cb private_data in
  Printf.printf "discovery_async: result=%d\n%!" result;
  ()

let discovery_connect_cb ctx status command_data private_data =
  let result = Iscsilib.set_session_type ctx Iscsilib.Session_type.Discovery in
  let result = Iscsilib.set_targetname ctx "none" in
  Printf.printf "set_session_type: result=%d\n%!" result;
  let result = Iscsilib.login_async ctx discovery_login_cb private_data in
  Printf.printf "login_async: result=%d\n%!" result;
  ()

let _ =
  let ctx = Iscsilib.create_context "iqn.2013-12.org.xen:client" in
  let target = "10.80.239.117:3260" in
  let res = Iscsilib.connect_async ctx target discovery_connect_cb null in
  
  let rec inner () =
    let fd = Iscsilib.get_fd ctx in
    let events = Iscsilib.which_events ctx in
    let revents = Iscsilib.poll_one fd events in
    Printf.printf "revents=%d\n%!" revents;
    let result = Iscsilib.service ctx revents in
    if result < 0 then failwith (Iscsilib.get_error ctx) else inner ()
  in inner ()
  
