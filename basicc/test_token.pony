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
      match token'.category
      | TokenIdentifier =>
        let line = token'.line
        let column = token'.column
        env.out.print(
          "(" + line.string() + ", " + column.string() +
          ")      ID: " + String.from_iso_array((consume token').data))
      | TokenNumber =>
        let line = token'.line
        let column = token'.column
        env.out.print(
          "(" + line.string() + ", " + column.string() +
          ")  Number: " + String.from_iso_array((consume token').data))
      | TokenSpecial =>
        let line = token'.line
        let column = token'.column
        env.out.print(
          "(" + line.string() + ", " + column.string() +
          ") Special: " + String.from_iso_array((consume token').data))
      end
    end

  be pass_error(pass: Pass, err: String = "") =>
    env.out.print("Error: " + err)
    env.exitcode(2)
