use "files"

actor FileReadPass
  let coordinator: Coordinator
  let callback: {(File iso)} val

  new create(coordinator': Coordinator, callback': {(File iso)} val) =>
    coordinator = coordinator'
    callback = callback'

  be apply(filename: String, auth: AmbientAuth) =>
    let caps = recover val FileCaps .> set(FileRead) end
    try
      let path: FilePath = FilePath(auth, filename, caps)?
      callback(recover OpenFile(path) as File end)
    else
      coordinator.pass_error(
        this,
        "Couldn't open file '" + filename + "'.")
    end

primitive FileEventEOF

class FileEventLine
  let number: USize
  let string: String

  new create(number': USize, string': String) =>
    number = number'
    string = string'

type FileEvent is (FileEventEOF | FileEventLine iso)

actor FileLineExtractPass
  let coordinator: Coordinator
  let callback: {(FileEvent)} val

  new create(coordinator': Coordinator, callback': {(FileEvent)} val) =>
    coordinator = coordinator'
    callback = callback'

  be apply(file: File iso) =>
    var line_number: USize = 1
    for line in FileLines(consume file) do
      callback(recover FileEventLine((line_number = line_number + 1), line) end)
    end
    callback(FileEventEOF)
