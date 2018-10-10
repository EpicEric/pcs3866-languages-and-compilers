primitive TokenIdentifier
primitive TokenNumber
primitive TokenSpecial
primitive TokenEOF
type TokenCategory is 
  ( TokenIdentifier
  | TokenNumber
  | TokenSpecial
  | TokenEOF )

class TokenEvent
  let data: Array[U8]
  let category: TokenCategory

  new create(data': Array[U8], category': TokenCategory) =>
    data = data'
    category = category'

actor TokenCategorizerPass
  let coordinator: Coordinator
  let callback: {(TokenEvent iso)} val
  var data: Array[U8] iso = recover Array[U8] end
  var category: (TokenCategory | None) = None

  new create(
    coordinator': Coordinator,
    callback': {(TokenEvent iso)} val)
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(character: CharacterEvent iso) =>
    let char_type: CharacterType = character.char_type
    let value: U8 = character.character
    match char_type
    | CharacterTypeLetter =>
      match category
      | TokenIdentifier => None
      else commit_token() end
      data.push(value)
      category = TokenIdentifier
    | CharacterTypeDigit =>
      match category
      | TokenIdentifier => None
      | TokenNumber => None
      else commit_token() end
      data.push(value)
      category = TokenNumber
    | CharacterTypeSpecial =>
      commit_token()
      data.push(value)
      category = TokenSpecial
    | CharacterTypeEOF =>
      commit_token()
      category = TokenEOF
      commit_token()
    else commit_token() end

  fun ref commit_token() =>
    match category
    | let category': TokenCategory =>
      let data' = (data = recover Array[U8] end)
      callback(
        recover TokenEvent(consume data', category') end)
      category = None
    end
