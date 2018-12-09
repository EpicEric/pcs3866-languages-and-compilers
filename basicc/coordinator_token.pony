use "format"

actor TestTokenCoordinator
  let env: Env
  var token_count: USize = 0
  let token_list: Array[String] = Array[String]

  new create(env': Env, file: String) =>
    env = env'
    let categorizer = TokenCategorizerPass(
      this, {(token: TokenEvent)(coordinator: TestTokenCoordinator = this) =>
        coordinator(consume token)} val)
    let filter = CharacterFilterPass(
      this, {(char: CharacterEvent iso) =>
        categorizer(consume char)} val)
    let reader = FileReaderPass(
      this, {(line: FileEvent) =>
        filter(consume line)} val)
    try reader(file, env.root as AmbientAuth) end

  be apply(token: TokenEvent) =>
    match (consume token)
    | TokenEOF =>
      env.out.print("Read " + token_count.string() + " token(s).")
      for token' in token_list.values() do
        env.out.print(token')
      end
    | let token': TokenEventWord iso =>
      token_count = token_count + 1
      token_list.push(_format_token(consume token'))
    end

  fun _format_token(token: TokenEventWord iso): String =>
    let line = token.line
    let column = token.column
    let category =
      match token.category
        | TokenIdentifier => "ID"
        | TokenNumber => "Number"
        | TokenSpecial => "Special"
      end
    "#"
      + Format(token_count.string() where width = 4)
      + " ("
      + Format(line.string() where width = 3, align = AlignRight)
      + ", "
      + Format(column.string() where width = 3, align = AlignRight)
      + ") "
      + Format(category where width = 7, align = AlignRight)
      + ": "
      + (consume token).data

  be pass_error(pass: Pass, err: String = "Unknown error") =>
    let pass_name = match pass
    | let p': FileReaderPass =>
      "File reader"
    | let p': CharacterFilterPass =>
      "Character filter"
    | let p': TokenCategorizerPass =>
      "Token categorizer"
    else
      "Unknown pass"
    end
    let error_string: String iso = recover String(
      pass_name.size() + 8 + err.size()) end
    error_string .> append(pass_name) .> append (" error: ") .> append(err)
    env.out.print(consume error_string)
    env.exitcode(2)
