import macros

proc getChildren(node: NimNode, nodeKind: NimNodeKind): seq[NimNode] =
  var stack: seq[NimNode] = @[node]
  result = @[]

  while stack.len() > 0:
    let newNode = stack.pop()
    for i in 0 ..< newNode.len():
      let child = newNode[i]
      if child.kind == nodeKind:
        result.add(child)
      else:
        stack.add(child)

macro `@`*(objName: typed, fields: untyped): untyped =

  let objImpl = objName.getImpl()
  let identDefs = objImpl.getChildren(nnkIdentDefs)

  result = newNimNode(nnkObjConstr).add(objImpl[0])

  for i in 0..<len(fields):
    let childLit = fields[i]
    let idDef = identDefs[i]

    let colonExpr = newColonExpr(idDef[0], childLit)
    result.add(colonExpr)

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
