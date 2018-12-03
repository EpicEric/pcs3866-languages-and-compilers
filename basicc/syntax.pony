use "collections"

/* Command labels */

class SyntaxLabel
  let label: U32

  new create(label': U32) =>
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
  ( SyntaxSine
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
  let value: String ref

  new create(value': String ref) =>
    value = value'

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
  let variable: String
  let expression: SyntaxExpression

  new create(variable': String, expression': SyntaxExpression) =>
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

  new create(
    left_expression': SyntaxExpression,
    right_expression': SyntaxExpression,
    comparator': SyntaxComparator
  ) =>
    left_expression = left_expression'
    right_expression = right_expression'
    comparator = comparator'

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
  | SyntaxAttribution iso
  | SyntaxPrint iso
  | SyntaxGoto iso
  | SyntaxIf iso
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
  let read_list: Array[String] = Array[String]
  let data_list: Array[F32] = Array[F32]

  // DIM ({identifier: [count_dim1, count_dim2, ...]})
  let dim_map: Map[String, Array[U32]] = Map[String, Array[U32]]

  // FOR / NEXT ({var: (return_point, positive_step, max_exp, step_exp)})
  let for_map: Map[String, (U32, Bool, SyntaxExpression, SyntaxExpression)] = Map[String, (U32, Bool, SyntaxExpression, SyntaxExpression)]

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
