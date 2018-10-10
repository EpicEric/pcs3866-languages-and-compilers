type Pass is
  ( FileReadPass
  | FileLineExtractPass
  | CharacterFilterPass
  | TokenCategorizerPass )

interface tag Coordinator
  be pass_error(pass: Pass, err: String = "") => None
