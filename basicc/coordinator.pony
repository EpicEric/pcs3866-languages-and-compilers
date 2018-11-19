type Pass is
  ( FileReaderPass
  | CharacterFilterPass
  | TokenCategorizerPass
  | SyntaxParserPass )

interface tag Coordinator
  be pass_error(pass: Pass, err: String = "")
