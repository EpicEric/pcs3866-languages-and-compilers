use "format"

actor TestTokenCoordinator
  let env: Env
  var token_count: USize = 0

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
    | TokenEOF => None
    | let token': TokenEventWord iso =>
      token_count = token_count + 1
      env.out.print(_format_token(consume token'))
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
    let token_data = String.from_iso_array((consume token).data)
    "#"
      + Format(token_count.string() where width = 4)
      + " ("
      + Format(line.string() where width = 3, align = AlignRight)
      + ", "
      + Format(column.string() where width = 3, align = AlignRight)
      + ") "
      + Format(category where width = 7, align = AlignRight)
      + ": "
      + (consume token_data)

  be pass_error(pass: Pass, err: String = "") =>
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
