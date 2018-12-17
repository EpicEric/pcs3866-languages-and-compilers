use "buffered"

actor SemanticInterpreterPass
  let coordinator: Coordinator
  let stream: OutStream
  var pass_error: Bool = false
  var finished_parse: Bool = false
  var finished_run: Bool = false

  let output: Writer = Writer

  var pointer: USize = 0
  let program: Array[SyntaxEvent val] = program.create()

  new create(
    coordinator': Coordinator,
    stream': OutStream
  ) =>
    coordinator = coordinator'
    stream = stream'
  
  be apply(syntax: SyntaxEvent) =>
    if pass_error then return end
    if finished_parse then
      coordinator.pass_error(this, "Cannot read syntactic structures after EOF")
      pass_error = true
      return
    end
    let syntax': SyntaxEvent val = consume syntax
    match syntax'
    | SyntaxEOF =>
      finished_parse = true
      run()
    else
      program.push(syntax')
    end
  
  be run() =>
    if pass_error then return end
    if finished_run then
      coordinator.pass_error(this, "Cannot run commands after end of program")
      pass_error = true
      return
    end
    if pointer == program.size() then _finish() end
    //TODO

  fun ref _finish() =>
    output.u8('\n')
    stream.writev(output.done())
    finished_run = true
