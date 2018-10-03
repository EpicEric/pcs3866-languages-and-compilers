
type Pass is
  ( FileReadPass
  | FileLineExtractPass
  | CharacterFilterPass
  )

interface tag Coordinator
  be pass_error(pass: Pass, err: String = "") => None
