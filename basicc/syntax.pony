use "collections"


actor SyntaxParserPass
  let coordinator: Coordinator
  let callback: {(TokenEvent)} val //FIXME
  var pass_error: Bool = false
  var finished: Bool = false

  // READ / DATA
  let read_list: Array[String] = Array[String]
  let data_list: Array[F32] = Array[F32]

  // DIM ({identifier: [count_dim1, count_dim2, ...]})
  let dim_map: Map[String, Array[U32]] = Map[String, Array[U32]]

  // FOR / NEXT ({var: (return_point, positive_step, max_exp, step_exp)})
  let for_map: Map[String, (U32, Bool)] = Map[String, (U32, Bool)] //FIXME

  // List of labels for each statement
  let label_list: Array[U32] = Array[U32]

  new create(
    coordinator': Coordinator,
    callback': {(TokenEvent)} val) //FIXME
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(character: TokenEvent) =>
    if pass_error then return end
    coordinator.pass_error(this, "SyntaxParserPass is unimplemented")
    pass_error = true
