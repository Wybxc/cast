open Core
module Pretty = Cast.Pretty.Make (PPrint.ToChannel)

let () =
  let json = Yojson.Basic.from_channel In_channel.stdin in
  let ast = Cast.Clang.from_json json in
  Pretty.render ast stdout;
  Out_channel.newline stdout
