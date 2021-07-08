open! Core
open! Async

let random_wait_time () = Random.float_range 0.25 0.75

let try_get_uri uri =
  try_with (fun () -> Cohttp_async.Client.get uri)
  |> Deferred.Or_error.of_exn_result

let get_redirect_uri response =
  let open Cohttp in
  Response.headers response |> Header.get_location

(* Can we break this up into smaller chunks? *)
(* TODO sometimes you get a redirect to a relative URL...could convert these to
   elpais URLs and try those. *)
let get_follow_redirects (uri : Uri.t) ~(max_redirects : int) :
    (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.Or_error.t =
  let rec loop ~(uri : Uri.t) ~(max_redirects : int) :
      (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.Or_error.t =
    let open Cohttp in
    let%bind () = after (sec (random_wait_time ())) in
    let%bind result = try_get_uri uri in
    match result with
    | Error err ->
        let msg = sprintf "Couldn't get uri '%s'" (Uri.to_string uri) in
        let x = Or_error.error msg err Error.sexp_of_t in
        return x
    | Ok (response, body) as res -> (
        (* Is it a redirect? *)
        let code = Code.code_of_status @@ Response.status response in
        match Code.is_redirection code with
        | false ->
            let x = res in
            return x
        | true -> (
            match get_redirect_uri response with
            | None ->
                let x =
                  Or_error.errorf "No redirect URI for '%s'" (Uri.to_string uri)
                in
                return x
            | Some new_uri ->
                eprintf "LOG -- in redirect: new_uri: %s\n"
                  (Uri.to_string new_uri);
                if max_redirects > 0 then
                  (* Drain response body so we don't leak connections. *)
                  let x =
                    Cohttp_async.Body.drain body >>= fun _body ->
                    loop ~max_redirects:(max_redirects - 1) ~uri:new_uri
                  in
                  x
                else
                  let x =
                    Or_error.errorf "Too many redirects for uri '%s'"
                      (Uri.to_string uri)
                  in
                  return x))
  in
  loop ~uri ~max_redirects

let get_soup uri ~max_redirects =
  let%bind res = get_follow_redirects uri ~max_redirects in
  match res with
  | Error err ->
      let msg =
        sprintf "Failed to get soup because we failed to get uri '%s'"
          (Uri.to_string uri)
      in
      Deferred.Or_error.error msg err Error.sexp_of_t
  | Ok (response, body) -> (
      match Cohttp.Response.status response with
      | `OK ->
          let%bind body_text = Cohttp_async.Body.to_string body in
          let%bind stream = return @@ Markup.string body_text in
          let%bind soup =
            stream |> Markup.parse_html |> Markup.signals |> Soup.from_signals
            |> return
          in
          Deferred.Or_error.return soup
      | status ->
          (* TODO probably need to drain the body here. *)
          let x =
            Cohttp_async.Body.drain body >>= fun _body ->
            Deferred.Or_error.errorf "ERROR -- Non-Ok response '%s' for %s"
              (Cohttp.Code.string_of_status status)
              (Uri.to_string uri)
          in
          x)
