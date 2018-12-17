actor CoordinatorInterpreter
  let env: Env
  var token_count: USize = 0
  let token_list: Array[String] = Array[String]

  new create(env': Env, file: String) =>
    env = env'
    let interpreter = SemanticInterpreterPass(this, env.out)
    let parser = SyntaxParserPass(
      this, {(syntax: SyntaxEvent) =>
        interpreter(consume syntax)} val)
    let categorizer = TokenCategorizerPass(
      this, {(token: TokenEvent) =>
        parser(consume token)} val)
    let filter = CharacterFilterPass(
      this, {(char: CharacterEvent iso) =>
        categorizer(consume char)} val)
    let reader = FileReaderPass(
      this, {(line: FileEvent) =>
        filter(consume line)} val)
    try reader(file, env.root as AmbientAuth) end

  be pass_error(pass: Pass, err: String = "Unknown error") =>
    let pass_name = match pass
    | let p': FileReaderPass =>
      "File reader"
    | let p': CharacterFilterPass =>
      "Character filter"
    | let p': TokenCategorizerPass =>
      "Token categorizer"
    | let p': SyntaxParserPass =>
      "Syntax parser"
    | let p': SemanticInterpreterPass =>
      "Semantic interpreter"
    // else
    //   "Unknown pass"
    end
    let error_string: String iso = recover String(
      pass_name.size() + 8 + err.size()) end
    error_string .> append(pass_name) .> append (" error: ") .> append(err)
    env.out.print(consume error_string)
    env.exitcode(2)
