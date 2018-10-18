type Pass is
  ( FileReaderPass
  | CharacterFilterPass
  | TokenCategorizerPass )

interface tag Coordinator
  be pass_error(pass: Pass, err: String = "")
