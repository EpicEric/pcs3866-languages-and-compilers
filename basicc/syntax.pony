use "collections"

/* Statement labels */

class SyntaxLabel
  let label: U32

  new create(label': U32) =>
    label = label'

/* Compiler labels */

class SyntaxCompilerLabel
  let label: String

  new create(label': String) =>
    label = label'

/* Expressions */

primitive SyntaxAdd
primitive SyntaxSubtract
primitive SyntaxMultiply
primitive SyntaxDivide
primitive SyntaxPower
type SyntaxBinaryOperator is
  ( SyntaxAdd
  | SyntaxSubtract
  | SyntaxMultiply
  | SyntaxDivide
  | SyntaxPower )

primitive SyntaxNegation
primitive SyntaxSine
primitive SyntaxCosine
primitive SyntaxTangent
primitive SyntaxArctangent
primitive SyntaxExponential
primitive SyntaxAbsolute
primitive SyntaxLogarithm
primitive SyntaxSquareRoot
primitive SyntaxInteger
primitive SyntaxRandom

class SyntaxUserDefinedFunctionCall // DEF FNx
  let name: String

  new create(name': String) =>
    name = name'

type SyntaxUnaryOperator is
  ( SyntaxNegation
  | SyntaxSine
  | SyntaxCosine
  | SyntaxTangent
  | SyntaxArctangent
  | SyntaxExponential
  | SyntaxAbsolute
  | SyntaxLogarithm
  | SyntaxSquareRoot
  | SyntaxInteger
  | SyntaxRandom
  | SyntaxUserDefinedFunctionCall )

class SyntaxExpressionNumber
  let value: F32

  new create(value': F32) =>
    value = value'

class SyntaxExpressionVariable
  let name: String
  let index: (None | Array[SyntaxExpression])

  new create(name': String, index': (None | Array[SyntaxExpression]) = None) =>
    name = name'
    index = index'

class SyntaxExpressionUnary
  let operand: SyntaxExpression
  let operator: SyntaxUnaryOperator

  new create(operand': SyntaxExpression, operator': SyntaxUnaryOperator) =>
    operand = operand'
    operator = operator'

class SyntaxExpressionBinary
  let left_operand: SyntaxExpression
  let right_operand: SyntaxExpression
  let operator: SyntaxBinaryOperator

  new create(
    left_operand': SyntaxExpression,
    right_operand': SyntaxExpression,
    operator': SyntaxBinaryOperator
  ) =>
    left_operand = left_operand'
    right_operand = right_operand'
    operator = operator'

type SyntaxExpression is
  ( SyntaxExpressionNumber
  | SyntaxExpressionVariable
  | SyntaxExpressionUnary
  | SyntaxExpressionBinary )

/* LET / READ+DATA attribution */

class SyntaxAttribution
  let variable: SyntaxExpressionVariable
  let expression: SyntaxExpression

  new create(
    variable': SyntaxExpressionVariable,
    expression': SyntaxExpression
  ) =>
    variable = variable'
    expression = expression'

/* PRINT */

primitive SyntaxPrintLineBreak

class SyntaxPrintString
  let text: String

  new create(text': String) =>
    text = text'

class SyntaxPrintExpression
  let expression: SyntaxExpression

  new create(expression': SyntaxExpression) =>
    expression = expression'

type SyntaxPrint is
  ( SyntaxPrintLineBreak
  | SyntaxPrintString
  | SyntaxPrintExpression )

/* GOTO */

class SyntaxGoto
  let label: U32

  new create(label': U32) =>
    label = label'

/* Compiler GOTO */

class SyntaxCompilerGoto
  let label: String

  new create(label': String) =>
    label = label'

/* IF / FOR */

primitive SyntaxEqualTo
primitive SyntaxDifferent
primitive SyntaxGreaterThan
primitive SyntaxLesserThan
primitive SyntaxGreaterThanOrEqualTo
primitive SyntaxLesserThanOrEqualTo
type SyntaxComparator is
  ( SyntaxEqualTo
  | SyntaxDifferent
  | SyntaxGreaterThan
  | SyntaxLesserThan
  | SyntaxGreaterThanOrEqualTo
  | SyntaxLesserThanOrEqualTo )

class SyntaxIf
  let left_expression: SyntaxExpression
  let right_expression: SyntaxExpression
  let comparator: SyntaxComparator
  let destination_label: U32

  new create(
    left_expression': SyntaxExpression,
    right_expression': SyntaxExpression,
    comparator': SyntaxComparator,
    destination_label': U32
  ) =>
    left_expression = left_expression'
    right_expression = right_expression'
    comparator = comparator'
    destination_label = destination_label'

/* NEXT - Compiler-defined label */

class SyntaxNext
  let left_expression: SyntaxExpression
  let right_expression: SyntaxExpression
  let comparator: SyntaxComparator
  let destination_label: String

  new create(
    left_expression': SyntaxExpression,
    right_expression': SyntaxExpression,
    comparator': SyntaxComparator,
    destination_label': String
  ) =>
    left_expression = left_expression'
    right_expression = right_expression'
    comparator = comparator'
    destination_label = destination_label'

/* DEF FNx */

class SyntaxUserDefinedFunctionDeclaration
  let argument: String
  let expression: SyntaxExpression

  new create(argument': String, expression': SyntaxExpression) =>
    argument = argument'
    expression = expression'

/* GOSUB / RETURN */

class SyntaxSubroutine
  let subroutine: U32

  new create(subroutine': U32) =>
    subroutine = subroutine'

primitive SyntaxReturn

/* REM */

class SyntaxRemark
  let remark: String

  new create(remark': String) =>
    remark = remark'

/* Syntax blocks for semantic */

type SyntaxEvent is
  ( SyntaxLabel iso
  | SyntaxCompilerLabel iso
  | SyntaxAttribution iso
  | SyntaxPrint iso
  | SyntaxGoto iso
  | SyntaxCompilerGoto iso
  | SyntaxIf iso
  | SyntaxNext iso
  | SyntaxUserDefinedFunctionDeclaration iso
  | SyntaxSubroutine iso
  | SyntaxReturn
  | SyntaxRemark iso )

actor SyntaxParserPass
  let coordinator: Coordinator
  let callback: {(SyntaxEvent)} val
  var pass_error: Bool = false
  var finished: Bool = false

  // READ / DATA
  let read_list: Array[SyntaxExpressionVariable iso] = read_list.create()
  let data_list: Array[SyntaxExpressionNumber iso] = data_list.create()

  // DIM ({identifier: [count_dim1, count_dim2, ...]})
  let dim_map: Map[String, Array[U32]] = dim_map.create()

  // FOR / NEXT ({var: (return_point, max_exp, step_exp)})
  let for_map: Map[String, (U32, SyntaxExpression, SyntaxExpression)] =
    for_map.create()

  // List of labels for each statement
  let label_list: Array[U32] = Array[U32]

  new create(
    coordinator': Coordinator,
    callback': {(SyntaxEvent)} val)
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(character: TokenEvent) =>
    if pass_error then return end
    coordinator.pass_error(this, "SyntaxParserPass is unimplemented")
    pass_error = true

  fun ref syntax_read(variable: SyntaxExpressionVariable) ? =>
    """
    Add variable to read_list and pop attributions if necessary.
    If variable is dimensioned, add multiple variables.
    """
    if dim_map.contains(variable.name) then
      error //FIXME Implementar read para variáveis dimensionadas
    //TODO Reaproveitar objeto
    else read_list.push(recover SyntaxExpressionVariable(variable.name) end) end
    _pop_read_data()?

  fun ref syntax_data(value: F32) ? =>
    """
    Add value to data_list and pop attributions if necessary.
    """
    data_list.push(recover iso SyntaxExpressionNumber(value) end)
    _pop_read_data()?

  fun ref _pop_read_data() ? =>
    """
    Convert a READ / DATA block into a LET block.
    """
    while (read_list.size() > 0) and (data_list.size() > 0) do
      let variable: SyntaxExpressionVariable iso = read_list.shift()?
      let value: SyntaxExpressionNumber iso = data_list.shift()?
      let event: SyntaxEvent = recover iso SyntaxAttribution(
        consume variable,
        consume value) end
      callback(consume event)
    end

  fun ref syntax_for(
    variable: String,
    label: U32,
    max_exp: SyntaxExpression,
    step_exp: SyntaxExpression
  ) ? =>
    """
    Save FOR data in map
    """
    if for_map.contains(variable) then error end
    for_map(variable) = (label, max_exp, step_exp)

  fun ref syntax_next(variable: String) ? =>
    """
    Convert a FOR / NEXT block into a LET / IF block:

    ForLabel:
      ...
      LET STEP = StepExp
      LET MAX = MaxExp
      LET Variable = Variable + STEP
      IF STEP > 0 THEN DescLoop
      IF Variable <= MAX THEN ReturnLoop
      GOTO EscapeLoop
    DescLoop:
      IF Variable >= MAX THEN ReturnLoop
    EscapeLoop:
      ...
    """
    let for_data = for_map(variable)?
    let return_label: U32 = for_data._1
    let max_exp: SyntaxExpression iso = recover iso for_data._2 end
    let step_exp: SyntaxExpression iso = recover iso for_data._3 end
    let desc_loop_label: String = "DESC_" + variable
    let loop_escape_label: String = "END_LOOP_" + variable

    let step_attribution_event: SyntaxEvent = recover iso SyntaxAttribution(
      SyntaxExpressionVariable("STEP"),
      consume step_exp) end
    let max_attribution_event: SyntaxEvent = recover iso SyntaxAttribution(
      SyntaxExpressionVariable("MAX"),
      consume max_exp) end
    let step_addition_event: SyntaxEvent = recover iso SyntaxAttribution(
      SyntaxExpressionVariable(variable),
      SyntaxExpressionBinary(
        SyntaxExpressionVariable(variable),
        SyntaxExpressionVariable("STEP"),
        SyntaxAdd)) end
    let step_comparation_event: SyntaxEvent = recover iso SyntaxNext(
      SyntaxExpressionVariable("STEP"),
      SyntaxExpressionNumber(0),
      SyntaxGreaterThan,
      desc_loop_label) end
    let asc_loop_event: SyntaxEvent = recover iso SyntaxIf(
      SyntaxExpressionVariable(variable),
      SyntaxExpressionVariable("MAX"),
      SyntaxLesserThanOrEqualTo,
      return_label) end
    let escape_loop_event: SyntaxEvent = recover iso SyntaxCompilerGoto(
      loop_escape_label) end
    let desc_label_event: SyntaxEvent = recover iso SyntaxCompilerLabel(
      desc_loop_label) end
    let desc_loop_event: SyntaxEvent = recover iso SyntaxIf(
      SyntaxExpressionVariable(variable),
      SyntaxExpressionVariable("MAX"),
      SyntaxGreaterThanOrEqualTo,
      return_label) end
    let escape_label_event: SyntaxEvent = recover iso SyntaxCompilerLabel(
      loop_escape_label) end

    callback(consume step_attribution_event)
    callback(consume max_attribution_event)
    callback(consume step_addition_event)
    callback(consume step_comparation_event)
    callback(consume asc_loop_event)
    callback(consume escape_loop_event)
    callback(consume desc_label_event)
    callback(consume desc_loop_event)
    callback(consume escape_label_event)
