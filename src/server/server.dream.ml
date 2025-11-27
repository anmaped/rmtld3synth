let () =
  let app =
    Dream.router
      [ Dream.get "/favicon.ico" (Dream.from_filesystem "static" "favicon.ico")
      ; Dream.get "/" (Dream.from_filesystem "static" "index.html")
      ; Dream.get "/style.css" (Dream.from_filesystem "static" "style.css")
      ; Dream.get "/logo.png" (Dream.from_filesystem "static" "logo.png")
      ; Dream.get "/rmtld3synth.js" (Dream.from_filesystem "static" "rmtld3synth.js")
      ; Dream.get "/bindings.js" (Dream.from_filesystem "static" "bindings.js")
      ; Dream.get "/examples/:file" (fun request ->
            let path = Dream.param request "file" in
            Dream.from_filesystem "unittest" path request ) ]
    |> Dream.logger
  in
  Dream.run ~interface:"0.0.0.0" ~port:8001 app
