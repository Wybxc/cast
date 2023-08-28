open Core

type color4 =
  | Black
  | Red
  | Green
  | Blue
  | Yellow
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightBlue
  | BrightYellow
  | BrightMagenta
  | BrightCyan
  | BrightWhite

let open_color4 = function
  | Black -> "\027[30m"
  | Red -> "\027[31m"
  | Green -> "\027[32m"
  | Yellow -> "\027[33m"
  | Blue -> "\027[34m"
  | Magenta -> "\027[35m"
  | Cyan -> "\027[36m"
  | White -> "\027[37m"
  | BrightBlack -> "\027[90m"
  | BrightRed -> "\027[91m"
  | BrightGreen -> "\027[92m"
  | BrightYellow -> "\027[93m"
  | BrightBlue -> "\027[94m"
  | BrightMagenta -> "\027[95m"
  | BrightCyan -> "\027[96m"
  | BrightWhite -> "\027[97m"

let close_color4 = "\027[0m"

let enable_ansi = ref true

let with_fg fg s =
  if !enable_ansi then open_color4 fg ^ s ^ close_color4 else s
