type Pass is
  ( FileReaderPass
  | CharacterFilterPass
  | TokenCategorizerPass
  | SyntaxParserPass
  | SemanticInterpreterPass )

interface tag Coordinator
  be pass_error(pass: Pass, err: String = "Unknown error")
