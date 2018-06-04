let version = "0.29"

let path =
  try (Sys.getenv "FLITTER_HOME")
  with Not_found->"./"

let std_xxx = ref (Filename.concat path "xxx.yyy")

let logger =
  try Some (Sys.getenv "FLITTER_LOGGER")
  with Not_found-> None
