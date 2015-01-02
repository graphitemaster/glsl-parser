#include "ast.h"

namespace glsl {

const char *astStatement::name() const {
    switch (type) {
    case kCompound:     return "compound";
    case kDeclaration:  return "declaration";
    case kExpression:   return "expression";
    case kIf:           return "if";
    case kSwitch:       return "switch";
    case kCaseLabel:    return "case label";
    case kWhile:        return "while";
    case kDo:           return "do";
    case kFor:          return "for";
    case kContinue:     return "continue";
    case kBreak:        return "break";
    case kReturn:       return "return";
    case kDiscard:      return "discard";
    }
    return "(unknown)";
}

astTU::astTU(int type)
    : type(type)
{
}

astType::astType(bool builtin)
    : builtin(builtin)
{
}

astStruct::astStruct()
    : astType(false)
    , name(0)
{
}

astBuiltin::astBuiltin(int type)
    : astType(true)
    , type(type)
{
}

astVariable::astVariable(int type)
    : name(0)
    , baseType(0)
    , isArray(false)
    , isPrecise(false)
    , type(type)
{
}

astFunctionVariable::astFunctionVariable()
    : astVariable(astVariable::kFunction)
    , isConst(false)
    , initialValue(0)
{
}

astFunctionParameter::astFunctionParameter()
    : astVariable(astVariable::kParameter)
    , storage(-1)
    , auxiliary(-1)
    , memory(0)
    , precision(-1)
{
}

astGlobalVariable::astGlobalVariable()
    : astVariable(astVariable::kGlobal)
    , storage(-1)
    , auxiliary(-1)
    , memory(0)
    , precision(-1)
    , interpolation(-1)
    , isInvariant(false)
    , initialValue(0)
{
}

astSimpleStatement::astSimpleStatement(int type)
    : astStatement(type)
{
}

astCompoundStatement::astCompoundStatement()
    : astStatement(astStatement::kCompound)
{
}

astEmptyStatement::astEmptyStatement()
    : astSimpleStatement(astStatement::kEmpty)
{
}

astDeclarationStatement::astDeclarationStatement()
    : astSimpleStatement(astStatement::kDeclaration)
{
}

astLayoutQualifier::astLayoutQualifier()
    : name(0)
    , initialValue(0)
{
}

astFunction::astFunction()
    : returnType(0)
    , name(0)
    , isPrototype(false)
{
}

astDeclaration::astDeclaration()
    : variable(0)
{
}

astStatement::astStatement(int type)
    : type(type)
{
}

astExpressionStatement::astExpressionStatement(astExpression *expression)
    : astSimpleStatement(astStatement::kExpression)
    , expression(expression)
{
}

astIfStatement::astIfStatement()
    : astSimpleStatement(astStatement::kIf)
    , condition(0)
    , thenStatement(0)
    , elseStatement(0)
{
}

astSwitchStatement::astSwitchStatement()
    : astSimpleStatement(astStatement::kSwitch)
    , expression(0)
{
}

astCaseLabelStatement::astCaseLabelStatement()
    : astSimpleStatement(astStatement::kCaseLabel)
    , condition(0)
    , isDefault(false)
{
}

astIterationStatement::astIterationStatement(int type)
    : astSimpleStatement(type)
{
}

astWhileStatement::astWhileStatement()
    : astIterationStatement(astStatement::kWhile)
    , condition(0)
    , body(0)
{
}

astDoStatement::astDoStatement()
    : astIterationStatement(astStatement::kDo)
    , body(0)
    , condition(0)
{
}

astForStatement::astForStatement()
    : astIterationStatement(astStatement::kFor)
    , init(0)
    , condition(0)
    , loop(0)
    , body(0)
{
}

astJumpStatement::astJumpStatement(int type)
    : astStatement(type)
{
}

astContinueStatement::astContinueStatement()
    : astJumpStatement(astStatement::kContinue)
{
}

astBreakStatement::astBreakStatement()
    : astJumpStatement(astStatement::kBreak)
{
}

astReturnStatement::astReturnStatement()
    : astJumpStatement(astStatement::kReturn)
    , expression(0)
{
}

astDiscardStatement::astDiscardStatement()
    : astJumpStatement(astStatement::kDiscard)
{
}

astExpression::astExpression(int type)
    : type(type)
{
}

astIntConstant::astIntConstant(int value)
    : astExpression(astExpression::kIntConstant)
    , value(value)
{
}

astUIntConstant::astUIntConstant(unsigned int value)
    : astExpression(astExpression::kUIntConstant)
    , value(value)
{
}

astFloatConstant::astFloatConstant(float value)
    : astExpression(astExpression::kFloatConstant)
    , value(value)
{
}

astDoubleConstant::astDoubleConstant(double value)
    : astExpression(astExpression::kDoubleConstant)
    , value(value)
{
}

astBoolConstant::astBoolConstant(bool value)
    : astExpression(astExpression::kBoolConstant)
    , value(value)
{
}

astVariableIdentifier::astVariableIdentifier(astVariable *variable)
    : astExpression(astExpression::kVariableIdentifier)
    , variable(variable)
{
}

astFieldOrSwizzle::astFieldOrSwizzle()
    : astExpression(kFieldOrSwizzle)
    , operand(0)
    , name(0)
{
}

astArraySubscript::astArraySubscript()
    : astExpression(kArraySubscript)
    , operand(0)
    , index(0)
{
}

astFunctionCall::astFunctionCall()
    : astExpression(astExpression::kFunctionCall)
    , name(0)
{
}

astConstructorCall::astConstructorCall()
    : astExpression(astExpression::kConstructorCall)
    , type(0)
{
}

astUnaryExpression::astUnaryExpression(int type, astExpression *operand)
    : astExpression(type)
    , operand(operand)
{
}

astBinaryExpression::astBinaryExpression(int type)
    : astExpression(type)
    , operand1(0)
    , operand2(0)
{
}

astPostIncrementExpression::astPostIncrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPostIncrement, operand)
{
}

astPostDecrementExpression::astPostDecrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPostDecrement, operand)
{
}

astUnaryPlusExpression::astUnaryPlusExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kUnaryPlus, operand)
{
}

astUnaryMinusExpression::astUnaryMinusExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kUnaryMinus, operand)
{
}

astUnaryBitNotExpression::astUnaryBitNotExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kBitNot, operand)
{
}

astUnaryLogicalNotExpression::astUnaryLogicalNotExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kLogicalNot, operand)
{
}

astPrefixIncrementExpression::astPrefixIncrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPrefixIncrement, operand)
{
}

astPrefixDecrementExpression::astPrefixDecrementExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kPrefixDecrement, operand)
{
}

astSequenceExpression::astSequenceExpression()
    : astBinaryExpression(astExpression::kSequence)
{
}

astAssignmentExpression::astAssignmentExpression(int assignment)
    : astBinaryExpression(astExpression::kAssign)
    , assignment(assignment)
{
}

astOperationExpression::astOperationExpression(int operation)
    : astBinaryExpression(astExpression::kOperation)
    , operation(operation)
{
}

}
