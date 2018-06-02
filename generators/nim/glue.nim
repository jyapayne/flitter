proc preDec*[T: Ordinal | uint | uint64](x: var T; y = 1): T {.discardable.} =
  dec x, y
  result = x

proc postDec*[T: Ordinal | uint | uint64](x: var T; y = 1): T {.discardable.} =
  result = x
  dec x, y

proc postInc*[T: Ordinal | uint | uint64](x: var T; y = 1): T {.discardable.} =
  result = x
  inc x, y

proc preInc*[T: Ordinal | uint | uint64](x: var T; y = 1): T {.discardable.} =
  inc x, y
  result = x
