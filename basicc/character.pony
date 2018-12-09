primitive CharacterTypeLetter
primitive CharacterTypeDigit
primitive CharacterTypeSpecial
primitive CharacterTypeDelimiter
primitive CharacterTypeControl
primitive CharacterTypeEOF
type CharacterType is
  ( CharacterTypeLetter
  | CharacterTypeDigit
  | CharacterTypeSpecial
  | CharacterTypeDelimiter
  | CharacterTypeControl
  | CharacterTypeEOF )

type CharacterTypeUseful is
  ( CharacterTypeLetter
  | CharacterTypeDigit
  | CharacterTypeSpecial )
type CharacterTypeDisposable is CharacterTypeDelimiter

class CharacterEvent
  let character: U8
  let line: USize
  let column: USize
  let char_type: CharacterType

  new create(
    character': U8, line': USize, column': USize, char_type': CharacterType) =>
    character = character'
    line = line'
    column = column'
    char_type = char_type'

primitive CharacterClassifier
  fun apply(character: U8): CharacterType ? =>
    if
      ((character >= 0x61) and (character <= 0x7A)) or
      ((character >= 0x41) and (character <= 0x5A))
    then
      CharacterTypeLetter
    elseif (character >= 0x30) and (character <= 0x39) then
      CharacterTypeDigit
    elseif (character >= 0x21) and (character <= 0x7E) then
      CharacterTypeSpecial
    elseif
      (character == 0x9) or (character == 0xA) or (character == 0xD)
      or (character == 0x20)
    then
      CharacterTypeDelimiter
    else
      error
    end

actor CharacterFilterPass
  let coordinator: Coordinator
  let callback: {(CharacterEvent iso)} val
  var pass_error: Bool = false
  var line_number: USize = 0

  new create(
    coordinator': Coordinator,
    callback': {(CharacterEvent iso)} val)
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(line: FileEvent) =>
    if pass_error then return end
    var count: USize = 0
    try
      match (consume line)
      | let l: FileEventLine iso =>
        line_number = l.number
        let array: Array[U8] iso = (consume l).string.iso_array()
        for c in (consume array).values() do
          count = count + 1
          callback(this.classify(c, line_number, count)?)
        end
        callback(recover CharacterEvent(
          0xA, line_number, count, CharacterTypeControl) end)
      | FileEventEOF =>
        callback(recover CharacterEvent(
          0x0, line_number + 1, 1, CharacterTypeEOF) end)
      end
    else
      pass_error = true
      coordinator.pass_error(
        this, "Invalid character in position " + count.string() +
              " on line " + line_number.string())
    end

  fun tag classify(
    character: U8, line: USize, column: USize): CharacterEvent iso^ ?
  =>
    let char_type: CharacterType = CharacterClassifier(character)?
    recover CharacterEvent(character, line, column, char_type) end
