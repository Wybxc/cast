open Core

let colored_doc color s =
  PPrint.fancystring (Ansi.with_fg color s) (String.length s)

let comment = colored_doc Ansi.BrightBlack

let lit = colored_doc Ansi.Cyan

let str = colored_doc Ansi.Yellow

let id = colored_doc Ansi.White

let macro = colored_doc Ansi.Red

let var = colored_doc Ansi.Blue

let cls = colored_doc Ansi.Green

let op = colored_doc Ansi.White

let sym = colored_doc Ansi.Red

let typ = colored_doc Ansi.Green

let kwd = colored_doc Ansi.Blue

let sp = colored_doc Ansi.Magenta
