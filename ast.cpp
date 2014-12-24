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

astExpressionStatement::astExpressionStatement(astExpression *expression)
    : astSimpleStatement(astStatement::kExpression)
    , expression(expression)
{
}

astIfStatement::astIfStatement()
    : astSimpleStatement(astStatement::kIf)
{
}

astSwitchStatement::astSwitchStatement()
    : astSimpleStatement(astStatement::kSwitch)
{
}

astCaseLabelStatement::astCaseLabelStatement()
    : astSimpleStatement(astStatement::kCaseLabel)
{
}

astIterationStatement::astIterationStatement(int type)
    : astSimpleStatement(type)
{
}

astWhileStatement::astWhileStatement()
    : astIterationStatement(astStatement::kWhile)
{
}

astDoStatement::astDoStatement()
    : astIterationStatement(astStatement::kDo)
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
{
}

astDiscardStatement::astDiscardStatement()
    : astJumpStatement(astStatement::kDiscard)
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
{
}

astArraySubscript::astArraySubscript()
    : astExpression(kArraySubscript)
{
}

astFunctionCall::astFunctionCall()
    : astExpression(astExpression::kFunctionCall)
{
}

astConstructorCall::astConstructorCall()
    : astExpression(astExpression::kConstructorCall)
{
}

astUnaryExpression::astUnaryExpression(int type, astExpression *operand)
    : astExpression(type)
    , operand(operand)
{
}

astBinaryExpression::astBinaryExpression(int type)
    : astExpression(type)
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
    : astUnaryExpression(astExpression::kPlus, operand)
{
}

astUnaryMinusExpression::astUnaryMinusExpression(astExpression *operand)
    : astUnaryExpression(astExpression::kMinus, operand)
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

}
