actor TestSyntaxCoordinator
  let env: Env

  new create(env': Env, file: String) =>
    env = env'
    let parser = SyntaxParserPass(
      this, {(token: SyntaxEvent)(coordinator: TestSyntaxCoordinator = this) =>
        coordinator(consume token)} val)
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

  be apply(token: SyntaxEvent) =>
    match consume token
    | SyntaxEOF =>
      env.out.print("Syntax is ok")
    | let label: SyntaxLabel iso =>
      env.out.print("Label: " + (consume label).label.string())
    | let remark: SyntaxRemark iso =>
      env.out.print("Remark: " + (consume remark).remark)
    // else // TODO
    end

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
    // else
    //   "Unknown pass"
    end
    let error_string: String iso = recover String(
      pass_name.size() + 8 + err.size()) end
    error_string .> append(pass_name) .> append (" error: ") .> append(err)
    env.out.print(consume error_string)
    env.exitcode(2)
