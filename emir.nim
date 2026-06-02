import std/[os, strutils, osproc, tables, json, sets, strformat, sequtils, httpclient, logging]
import checksums/sha1
import cligen

type
  TokenType = enum
    tkNone, tkPipeline, tkStage, tkDep, tkExec, tkCd, tkFetch, tkLog,
    tkPlusDir, tkMinusDir, tkPlusFile, tkMinusFile, tkHash, tkIdent, 
    tkString, tkArrow, tkAssign, tkNewline, tkEOF

  Token = object
    kind: TokenType
    valStr: string
    line: int
    col: int

  NodeKind = enum
    nkProgram, nkVariableAssign, nkStageDecl,
    nkExec, nkCd, nkFetch, nkLog, nkPlusDir, nkMinusDir, nkPlusFile, nkMinusFile

  Node = ref object
    line, col: int
    case kind: NodeKind
    of nkProgram:
      statements: seq[Node]
    of nkVariableAssign:
      varName: string
      varValue: string
    of nkStageDecl:
      stageName: string
      dependencies: seq[string]
      trackedFile: string
      body: seq[Node]
    of nkExec:
      cmd: string
    of nkCd:
      dir: string
    of nkFetch:
      url, dest: string
    of nkLog:
      message: string
    of nkPlusDir, nkMinusDir, nkPlusFile, nkMinusFile:
      path: string

  EmirError = object of CatchableError

  Config = object
    dryRun: bool
    verbose: bool
    cacheFile: string
    globalVars: Table[string, string]
    stages: Table[string, Node]

  # structured dag representation
  DependencyGraph = object
    adjList: Table[string, seq[string]]

# error helper
proc raiseError(line, col: int, msg: string) {.noreturn.} =
  raise newException(EmirError, &"error [{line}:{col}]: {msg}")

# lexer
proc tokenize(source: string): seq[Token] =
  var 
    cursor = 0
    line = 1
    col = 1
  
  template advance(n: int = 1) =
    for _ in 0..<n:
      if cursor < source.len:
        if source[cursor] == '\n':
          line += 1
          col = 1
        else:
          col += 1
        cursor += 1

  while cursor < source.len:
    let ch = source[cursor]

    case ch
    of ' ', '\t':
      advance()
    of '#': # skip comments
      while cursor < source.len and source[cursor] notin {'\n', '\r'}:
        advance()
    of '\n', '\r':
      let startL = line
      let startC = col
      if ch == '\r' and cursor + 1 < source.len and source[cursor+1] == '\n':
        advance(2)
      else:
        advance()
      result.add(Token(kind: tkNewline, line: startL, col: startC))
    of '=':
      result.add(Token(kind: tkAssign, line: line, col: col))
      advance()
    of '-':
      if cursor + 1 < source.len and source[cursor+1] == '>':
        result.add(Token(kind: tkArrow, line: line, col: col))
        advance(2)
      elif cursor + 4 < source.len and source[cursor..cursor+4] == "-file":
        result.add(Token(kind: tkMinusFile, line: line, col: col))
        advance(5)
      elif cursor + 3 < source.len and source[cursor..cursor+3] == "-dir":
        result.add(Token(kind: tkMinusDir, line: line, col: col))
        advance(4)
      else:
        result.add(Token(kind: tkIdent, valStr: "-", line: line, col: col))
        advance()
    of '+':
      if cursor + 4 < source.len and source[cursor..cursor+4] == "+file":
        result.add(Token(kind: tkPlusFile, line: line, col: col))
        advance(5)
      elif cursor + 3 < source.len and source[cursor..cursor+3] == "+dir":
        result.add(Token(kind: tkPlusDir, line: line, col: col))
        advance(4)
      else:
        result.add(Token(kind: tkIdent, valStr: "+", line: line, col: col))
        advance()
    of '>':
      if cursor + 1 < source.len and source[cursor+1] == '>':
        result.add(Token(kind: tkLog, line: line, col: col))
        advance(2)
      else:
        raiseError(line, col, "missing second '>' for log command")
    of '"':
      let startL = line
      let startC = col
      advance()
      var s = ""
      while cursor < source.len and source[cursor] != '"':
        s.add(source[cursor])
        advance()
      if cursor >= source.len:
        raiseError(startL, startC, "unclosed string literal")
      advance()
      result.add(Token(kind: tkString, valStr: s, line: startL, col: startC))
    of 'a'..'z', 'A'..'Z', '.', '/', '_', '0'..'9':
      let startL = line
      let startC = col
      var id = ""
      while cursor < source.len and source[cursor] in {'a'..'z', 'A'..'Z', '0'..'9', '_', '.', '/', '-'}:
        id.add(source[cursor])
        advance()
      
      let kind = case id
        of "pipeline": tkPipeline
        of "stage": tkStage
        of "needs": tkDep
        of "exec": tkExec
        of "cd": tkCd
        of "fetch": tkFetch
        of "hash": tkHash
        else: tkIdent
      
      result.add(Token(kind: kind, valStr: id, line: startL, col: startC))
    else:
      raiseError(line, col, &"unknown character: '{ch}'")
      
  result.add(Token(kind: tkEOF, line: line, col: col))

# parser
type Parser = object
  tokens: seq[Token]
  pos: int

proc peek(p: Parser, offset = 0): Token =
  if p.pos + offset < p.tokens.len: p.tokens[p.pos + offset]
  else: p.tokens[^1]

proc eat(p: var Parser): Token =
  result = p.peek()
  if result.kind != tkEOF: p.pos += 1

proc expect(p: var Parser, kind: TokenType, errorMsg: string): Token =
  let t = p.peek()
  if t.kind != kind:
    raiseError(t.line, t.col, errorMsg & &" (got: {t.kind})")
  return p.eat()

proc skipNewlines(p: var Parser) =
  while p.peek().kind == tkNewline:
    discard p.eat()

proc parseAction(p: var Parser): Node =
  let t = p.peek()
  case t.kind
  of tkExec:
    discard p.eat()
    let cmd = p.expect(tkString, "expected string after 'exec'")
    return Node(kind: nkExec, line: t.line, col: t.col, cmd: cmd.valStr)
  of tkCd:
    discard p.eat()
    let dir = p.expect(tkString, "expected string after 'cd'")
    return Node(kind: nkCd, line: t.line, col: t.col, dir: dir.valStr)
  of tkLog:
    discard p.eat()
    let msg = p.expect(tkString, "expected string after '>>'")
    return Node(kind: nkLog, line: t.line, col: t.col, message: msg.valStr)
  of tkPlusDir:
    discard p.eat()
    let path = p.expect(tkString, "expected string after '+dir'")
    return Node(kind: nkPlusDir, line: t.line, col: t.col, path: path.valStr)
  of tkMinusDir:
    discard p.eat()
    let path = p.expect(tkString, "expected string after '-dir'")
    return Node(kind: nkMinusDir, line: t.line, col: t.col, path: path.valStr)
  of tkPlusFile:
    discard p.eat()
    let path = p.expect(tkString, "expected string after '+file'")
    return Node(kind: nkPlusFile, line: t.line, col: t.col, path: path.valStr)
  of tkMinusFile:
    discard p.eat()
    let path = p.expect(tkString, "expected string after '-file'")
    return Node(kind: nkMinusFile, line: t.line, col: t.col, path: path.valStr)
  of tkFetch:
    discard p.eat()
    let url = p.expect(tkString, "expected url string after 'fetch'")
    discard p.expect(tkArrow, "expected '->' delimiter")
    let dest = p.expect(tkString, "expected destination path string")
    return Node(kind: nkFetch, line: t.line, col: t.col, url: url.valStr, dest: dest.valStr)
  else:
    raiseError(t.line, t.col, &"unexpected action keyword: '{t.valStr}'")

proc parseStatement(p: var Parser): Node =
  p.skipNewlines()
  let t = p.peek()
  
  if t.kind == tkEOF: return nil
  
  if t.kind == tkIdent and p.peek(1).kind == tkAssign:
    let name = p.eat().valStr
    discard p.eat()
    let val = p.expect(tkString, "expected string assignment value")
    if p.peek().kind != tkEOF: discard p.expect(tkNewline, "expected newline after variable assignment")
    return Node(kind: nkVariableAssign, line: t.line, col: t.col, varName: name, varValue: val.valStr)
  
  elif t.kind == tkPipeline:
    discard p.eat()
    discard p.expect(tkIdent, "expected pipeline identifier name")
    return nil

  elif t.kind == tkStage:
    discard p.eat()
    let name = p.expect(tkIdent, "expected stage identifier name").valStr
    var deps: seq[string] = @[]
    var trackingHash = ""
    
    if p.peek().kind == tkDep:
      discard p.eat()
      while p.peek().kind == tkIdent:
        deps.add(p.eat().valStr)
        
    if p.peek().kind == tkHash:
      discard p.eat()
      trackingHash = p.expect(tkString, "expected target file path string after 'hash'").valStr
    
    if p.peek().kind != tkEOF: discard p.expect(tkNewline, "expected newline after stage declaration header")
    p.skipNewlines()
    
    var body: seq[Node] = @[]
    while p.peek().kind notin {tkStage, tkPipeline, tkEOF}:
      body.add(p.parseAction())
      if p.peek().kind != tkEOF:
        discard p.expect(tkNewline, "expected newline after action step")
      p.skipNewlines()
      
    return Node(kind: nkStageDecl, line: t.line, col: t.col, stageName: name, dependencies: deps, trackedFile: trackingHash, body: body)
  
  else:
    raiseError(t.line, t.col, &"invalid top-level syntax token statement: '{t.valStr}'")

proc parse(source: string): Node =
  var p = Parser(tokens: tokenize(source), pos: 0)
  var program = Node(kind: nkProgram, line: 1, col: 1, statements: @[])
  while p.peek().kind != tkEOF:
    let stmt = p.parseStatement()
    if stmt != nil:
      program.statements.add(stmt)
  return program

# variables, caching, and native networking
proc interpolate(str: string, vars: Table[string, string]): string =
  result = str
  for k, v in vars:
    result = result.replace("{" & k & "}", v)

proc getFileHash(path: string): string =
  if not fileExists(path): return ""
  return $secureHashFile(path)

proc checkCache(cfg: Config, stage, file: string): bool =
  if not fileExists(cfg.cacheFile): return false
  try:
    let data = parseFile(cfg.cacheFile)
    if data.hasKey(stage):
      let node = data[stage]
      return node["file"].getStr() == file and node["hash"].getStr() == getFileHash(file)
  except CatchableError:
    discard
  return false

proc writeCache(cfg: Config, stage, file: string) =
  var data = if fileExists(cfg.cacheFile): 
               try: parseFile(cfg.cacheFile) except CatchableError: newJObject()
             else: newJObject()
  let stageNode = newJObject()
  stageNode["file"] = %file
  stageNode["hash"] = %getFileHash(file)
  data[stage] = stageNode
  writeFile(cfg.cacheFile, data.pretty())

proc performNativeFetch(url: string, dest: string) =
  let client = newHttpClient()
  try:
    client.downloadFile(url, dest)
  except CatchableError as e:
    raise newException(EmirError, &"download failed: {e.msg}")
  finally:
    client.close()

# native execution shell wrapper to bypass fragile manual token splitting
proc runNativeShell(cmd: string): int =
  let (shell, args) = if defined(windows):
    ("cmd.exe", ["/c", cmd])
  else:
    ("/bin/sh", ["-c", cmd])
  
  try:
    let p = startProcess(shell, args = args, options = {poParentStreams})
    result = p.waitForExit()
    p.close()
  except OSError as e:
    result = -1

# semantic validation pass
proc validateAst(cfg: Config) =
  proc checkVars(str: string, line, col: int) =
    var i = 0
    while i < str.len:
      if str[i] == '{':
        let start = i + 1
        var endIdx = start
        while endIdx < str.len and str[endIdx] != '}': endIdx.inc
        if endIdx < str.len:
          let v = str[start..<endIdx]
          if not cfg.globalVars.hasKey(v):
            raiseError(line, col, &"undefined variable reference '{v}' prior to runtime")
        i = endIdx
      i.inc

  for name, node in cfg.stages:
    if node.trackedFile != "":
      checkVars(node.trackedFile, node.line, node.col)
    for action in node.body:
      case action.kind
      of nkLog: checkVars(action.message, action.line, action.col)
      of nkCd: checkVars(action.dir, action.line, action.col)
      of nkExec: checkVars(action.cmd, action.line, action.col)
      of nkPlusDir, nkMinusDir, nkPlusFile, nkMinusFile: checkVars(action.path, action.line, action.col)
      of nkFetch:
        checkVars(action.url, action.line, action.col)
        checkVars(action.dest, action.line, action.col)
        if not action.url.contains("://") and not action.url.contains("{"):
          raiseError(action.line, action.col, &"fetch url invalid format: '{action.url}'")
      else: discard

# structured dag implementation
proc initDependencyGraph(cfg: Config): DependencyGraph =
  result = DependencyGraph(adjList: initTable[string, seq[string]]())
  for name, node in cfg.stages:
    result.adjList[name] = node.dependencies

proc topologicalSort(g: DependencyGraph, target: string): seq[string] =
  var 
    resolvedOrder: seq[string] = @[]
    visiting = initHashSet[string]()
    visited = initHashSet[string]()

  proc dfs(nodeName: string) =
    if nodeName in visiting:
      raise newException(EmirError, &"circular dependency loop detected at stage: '{nodeName}'")
    if nodeName notin visited:
      visiting.incl(nodeName)
      if not g.adjList.hasKey(nodeName):
        raise newException(EmirError, &"unresolved stage reference dependency mapping missing: '{nodeName}'")
      
      for dep in g.adjList[nodeName]:
        dfs(dep)
        
      visiting.excl(nodeName)
      visited.incl(nodeName)
      resolvedOrder.add(nodeName)

  dfs(target)
  return resolvedOrder

proc resolveAndExecute(cfg: var Config, target: string) =
  validateAst(cfg)
  let graph = initDependencyGraph(cfg)
  let resolvedOrder = graph.topologicalSort(target)

  if cfg.verbose:
    info &"execution graph plan order: {resolvedOrder.join(\" -> \")}"

  for stage in resolvedOrder:
    let node = cfg.stages[stage]
    
    if node.trackedFile != "":
      let resolvedPath = node.trackedFile.interpolate(cfg.globalVars)
      if cfg.checkCache(stage, resolvedPath):
        if cfg.verbose: info &"stage '{stage}' cached (no shifts in tracking target: {resolvedPath}). skipping."
        continue

    if cfg.verbose or cfg.dryRun:
      info &"stage task run: {stage}"

    for action in node.body:
      case action.kind
      of nkLog:
        info action.message.interpolate(cfg.globalVars)
      of nkCd:
        let path = action.dir.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"cd {path}"
        if not cfg.dryRun: setCurrentDir(path)
      of nkExec:
        let cmd = action.cmd.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"exec {cmd}"
        if not cfg.dryRun:
          let exitCode = runNativeShell(cmd)
          if exitCode != 0:
            raiseError(action.line, action.col, &"command execution exited with non-zero code: {exitCode}")
      of nkPlusDir:
        let path = action.path.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"dir+ {path}"
        if not cfg.dryRun and not dirExists(path): createDir(path)
      of nkMinusDir:
        let path = action.path.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"dir- {path}"
        if not cfg.dryRun and dirExists(path): removeDir(path)
      of nkPlusFile:
        let path = action.path.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"file+ {path}"
        if not cfg.dryRun and not fileExists(path): writeFile(path, "")
      of nkMinusFile:
        let path = action.path.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"file- {path}"
        if not cfg.dryRun and fileExists(path): removeFile(path)
      of nkFetch:
        let url = action.url.interpolate(cfg.globalVars)
        let dest = action.dest.interpolate(cfg.globalVars)
        if cfg.verbose: debug &"fetch {url} -> {dest}"
        if not cfg.dryRun:
          try:
            performNativeFetch(url, dest)
          except EmirError as e:
            raiseError(action.line, action.col, e.msg)
      else: discard

    if node.trackedFile != "" and not cfg.dryRun:
      cfg.writeCache(stage, node.trackedFile.interpolate(cfg.globalVars))

# integrated test framework
proc runTests() =
  echo "--- running internal validation tests ---"
  
  # test 1: lexer and validation checks
  try:
    let source = "pipeline test\nstage step\n  >> \"hello\""
    let tokens = tokenize(source)
    assert tokens.anyIt(it.kind == tkPipeline), "pipeline token missing"
    assert tokens.anyIt(it.kind == tkStage), "stage token missing"
    echo "[pass] lexer tokens match specifications"
  except CatchableError as e:
    echo "[fail] lexer specs broken: ", e.msg

  # test 2: dag cycle interception
  try:
    var mockGraph = DependencyGraph(adjList: initTable[string, seq[string]]())
    mockGraph.adjList["A"] = @["B"]
    mockGraph.adjList["B"] = @["A"] # circular loop
    discard mockGraph.topologicalSort("A")
    echo "[fail] dag validation missed an active circular reference loop"
  except EmirError:
    echo "[pass] dag correctly intercepts circular reference loops"

  echo "--- testing suite complete ---"

# standard execution entrance called by cligen
proc emir(script = "", stage = "", dryRun = false, verbose = false, runSuite = false) =
  var consoleLog = newConsoleLogger(fmtStr="[$time] $levelname: ")
  addHandler(consoleLog)

  if runSuite:
    runTests()
    quit(0)

  if script == "" or stage == "":
    echo "error: arguments 'script' and 'stage' are mandatory."
    echo "usage: emir --script=<file.emir> --stage=<target_stage> [--dryRun] [--verbose]"
    quit(1)

  if not fileExists(script):
    quit(&"file tracking target missing context structure layout path: '{script}'")

  var cfg = Config(dryRun: dryRun, verbose: verbose, cacheFile: ".emir_cache.json", globalVars: initTable[string, string](), stages: initTable[string, Node]())

  try:
    let ast = parse(readFile(script))
    for stmt in ast.statements:
      if stmt.kind == nkVariableAssign:
        cfg.globalVars[stmt.varName] = stmt.varValue
      elif stmt.kind == nkStageDecl:
        cfg.stages[stmt.stageName] = stmt

    cfg.resolveAndExecute(stage)
    
  except EmirError as e:
    fatal e.msg
    quit(1)
  except CatchableError as e:
    fatal "fatal execution error context break conditions: " & e.msg
    quit(1)

when isMainModule:
  # cligen automatically builds standard help layouts and flag parameters
  dispatch(emir, help = {
    "script": "target pipeline configuration file blueprint path",
    "stage": "designated stage block element to execute",
    "dryRun": "toggle action validation pass without modifying the file system state",
    "verbose": "expose granular tracing outputs and system path status switches",
    "runSuite": "trigger internal framework validation unit tests"
  })