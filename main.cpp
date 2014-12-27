#include <stdio.h>

#include "parser.h"

using namespace glsl;

#undef KEYWORD
#define KEYWORD(X) #X,
static const char *kTypes[] = {
    #include "lexemes.h"
};
#undef KEYWORD
#define KEYWORD(...)

#undef OPERATOR
#define OPERATOR(N, S, P) S,
static const char *kOperators[] = {
    #include "lexemes.h"
};
#undef OPERATOR
#define OPERATOR(...)

#define print(...) \
    do { \
        printf(__VA_ARGS__); \
    } while (0)

static void printExpression(astExpression *expression);
static void printStatement(astStatement *statement);

static void printBuiltin(astBuiltin *builtin) {
    print("%s", kTypes[builtin->type]);
}

static void printType(astType *type) {
    if (type->builtin)
        return printBuiltin((astBuiltin*)type);
}

static void printIntConstant(astIntConstant *expression) {
    print("%d", expression->value);
}

static void printUIntConstant(astUIntConstant *expression) {
    print("%du", expression->value);
}

static void printFloatConstant(astFloatConstant *expression) {
    print("%g", expression->value);
}

static void printDoubleConstant(astDoubleConstant *expression) {
    print("%g", expression->value);
}

static void printBoolConstant(astBoolConstant *expression) {
    print("%s", expression->value ? "true" : "false");
}

static void printArraySize(const std::vector<astConstantExpression*> &arraySizes) {
    for (size_t i = 0; i < arraySizes.size(); i++) {
        print("[");
        printExpression(arraySizes[i]);
        print("]");
    }
}

static void printVariable(astVariable *variable, bool nameOnly = false) {
    if (nameOnly) {
        print("%s", variable->name.c_str());
        return;
    }

    printType(variable->baseType);
    print(" %s", variable->name.c_str());

    if (nameOnly)
        return;

    if (variable->isArray)
        printArraySize(variable->arraySizes);
}

static void printStorage(int storage) {
    switch (storage) {
        case kConst:
            print("const ");
            break;
        case kIn:
            print("in ");
            break;
        case kOut:
            print("out ");
            break;
        case kAttribute:
            print("attribute ");
            break;
        case kUniform:
            print("uniform ");
            break;
        case kVarying:
            print("varying ");
            break;
        case kBuffer:
            print("buffer ");
            break;
        case kShared:
            print("shared ");
            break;
    }
}

static void printAuxiliary(int auxiliary) {
    switch (auxiliary) {
        case kCentroid:
            print("centroid ");
            break;
        case kSample:
            print("sample ");
            break;
        case kPatch:
            print("patch ");
            break;
    }
}

static void printMemory(int memory) {
    if (memory & kCoherent) print("coherent ");
    if (memory & kVolatile) print("volatile ");
    if (memory & kRestrict) print("restrict ");
    if (memory & kReadOnly) print("readonly ");
    if (memory & kWriteOnly) print("writeonly ");
}

static void printPrecision(int precision) {
    switch (precision) {
        case kLowp:
            printf("lowp ");
            break;
        case kMediump:
            printf("mediump ");
            break;
        case kHighp:
            printf("highp ");
            break;
    }
}

static void printGlobalVariable(astGlobalVariable *variable) {
    printStorage(variable->storage);
    printAuxiliary(variable->auxiliary);
    printMemory(variable->memory);
    printPrecision(variable->precision);

    switch (variable->interpolation) {
        case kSmooth:
            print("smooth ");
            break;
        case kFlat:
            print("flat ");
            break;
        case kNoPerspective:
            print("noperspective ");
            break;
    }

    printVariable((astVariable*)variable);

    if (variable->initialValue) {
        print(" = ");
        printExpression(variable->initialValue);
    }

    print(";\n");
}

static void printVariableIdentifier(astVariableIdentifier *expression) {
    printVariable(expression->variable, true);
}

static void printFieldOrSwizzle(astFieldOrSwizzle *expression) {
    printExpression(expression->operand);
    print(".%s", expression->name.c_str());
}

static void printArraySubscript(astArraySubscript *expression) {
    printExpression(expression->operand);
    print("[");
    printExpression(expression->index);
    print("]");
}

static void printFunctionCall(astFunctionCall *expression) {
    print("%s(", expression->name.c_str());
    for (size_t i = 0; i < expression->parameters.size(); i++) {
        printExpression(expression->parameters[i]);
        if (i != expression->parameters.size() - 1)
            print(", ");
    }
    print(")");
}

static void printConstructorCall(astConstructorCall *expression) {
    printType(expression->type);
    print("(");
    for (size_t i = 0; i < expression->parameters.size(); i++) {
        printExpression(expression->parameters[i]);
        if (i != expression->parameters.size() - 1)
            print(", ");
    }
    print(")");
}

static void printFunctionVariable(astFunctionVariable *variable) {
    if (variable->isConst)
        print("const ");
    printVariable((astVariable*)variable);
    if (variable->initialValue) {
        print(" = ");
        printExpression(variable->initialValue);
    }
    print(";\n");
}

static void printPostIncrement(astPostIncrementExpression *expression) {
    printExpression(expression->operand);
    print("++");
}

static void printPostDecrement(astPostDecrementExpression *expression) {
    printExpression(expression->operand);
    print("--");
}

static void printUnaryMinus(astUnaryMinusExpression *expression) {
    print("-");
    printExpression(expression->operand);
}

static void printUnaryPlus(astUnaryPlusExpression *expression) {
    print("+");
    printExpression(expression->operand);
}

static void printUnaryBitNot(astUnaryBitNotExpression *expression) {
    print("~");
    printExpression(expression->operand);
}

static void printUnaryLogicalNot(astUnaryLogicalNotExpression *expression) {
    print("!");
    printExpression(expression->operand);
}

static void printPrefixIncrement(astPrefixIncrementExpression *expression) {
    print("++");
    printExpression(expression->operand);
}

static void printPrefixDecrement(astPrefixDecrementExpression *expression) {
    print("--");
    printExpression(expression->operand);
}

static void printAssign(astAssignmentExpression *expression) {
    printExpression(expression->operand1);
    print(" %s ", kOperators[expression->assignment]);
    printExpression(expression->operand2);
}

static void printSequence(astSequenceExpression *expression) {
    printExpression(expression->operand1);
    printf(", ");
    printExpression(expression->operand2);
}

static void printOperation(astOperationExpression *expression) {
    printExpression(expression->operand1);
    printf(" %s ", kOperators[expression->operation]);
    printExpression(expression->operand2);
}

static void printExpression(astExpression *expression) {
    switch (expression->type) {
        case astExpression::kIntConstant:
            return printIntConstant((astIntConstant*)expression);
        case astExpression::kUIntConstant:
            return printUIntConstant((astUIntConstant*)expression);
        case astExpression::kFloatConstant:
            return printFloatConstant((astFloatConstant*)expression);
        case astExpression::kDoubleConstant:
            return printDoubleConstant((astDoubleConstant*)expression);
        case astExpression::kBoolConstant:
            return printBoolConstant((astBoolConstant*)expression);
        case astExpression::kVariableIdentifier:
            return printVariableIdentifier((astVariableIdentifier*)expression);
        case astExpression::kFieldOrSwizzle:
            return printFieldOrSwizzle((astFieldOrSwizzle*)expression);
        case astExpression::kArraySubscript:
            return printArraySubscript((astArraySubscript*)expression);
        case astExpression::kFunctionCall:
            return printFunctionCall((astFunctionCall*)expression);
        case astExpression::kConstructorCall:
            return printConstructorCall((astConstructorCall*)expression);
        case astExpression::kPostIncrement:
            return printPostIncrement((astPostIncrementExpression*)expression);
        case astExpression::kPostDecrement:
            return printPostDecrement((astPostDecrementExpression*)expression);
        case astExpression::kUnaryMinus:
            return printUnaryMinus((astUnaryMinusExpression*)expression);
        case astExpression::kUnaryPlus:
            return printUnaryPlus((astUnaryPlusExpression*)expression);
        case astExpression::kBitNot:
            return printUnaryBitNot((astUnaryBitNotExpression*)expression);
        case astExpression::kLogicalNot:
            return printUnaryLogicalNot((astUnaryLogicalNotExpression*)expression);
        case astExpression::kPrefixIncrement:
            return printPrefixIncrement((astPrefixIncrementExpression*)expression);
        case astExpression::kPrefixDecrement:
            return printPrefixDecrement((astPrefixDecrementExpression*)expression);
        case astExpression::kAssign:
            return printAssign((astAssignmentExpression*)expression);
        case astExpression::kSequence:
            return printSequence((astSequenceExpression*)expression);
        case astExpression::kOperation:
            return printOperation((astOperationExpression*)expression);
    }
}

static void printCompoundStatement(astCompoundStatement *statement) {
    print(" {\n");
    for (size_t i = 0; i < statement->statements.size(); i++)
        printStatement(statement->statements[i]);
    print("}\n");
}

static void printEmptyStatement(astEmptyStatement *statement) {
    print(";");
}

static void printDeclarationStatement(astDeclarationStatement *statement) {
    for (size_t i = 0; i < statement->variables.size(); i++)
        printFunctionVariable(statement->variables[i]);
}

static void printExpressionStatement(astExpressionStatement *statement) {
    printExpression(statement->expression);
    print(";\n");
}

static void printIfStetement(astIfStatement *statement) {
    print("if(");
    printExpression(statement->condition);
    print(")");
    printStatement(statement->thenStatement);
    if (statement->elseStatement) {
        print("else");
        if (statement->elseStatement->type == astStatement::kIf)
            print(" ");
        printStatement(statement->elseStatement);
    }
}

static void printSwitchStatement(astSwitchStatement *statement) {
    print("switch(");
    printExpression(statement->expression);
    print(") {\n");
    for (size_t i = 0; i < statement->statements.size(); i++)
        printStatement(statement->statements[i]);
    print("}\n");
}

static void printCaseLabelStatement(astCaseLabelStatement *statement) {
    if (statement->isDefault)
        print("default");
    else {
        print("case ");
        printExpression(statement->condition);
    }
    print(":\n");
}

static void printWhileStatement(astWhileStatement *statement) {
    print("while(");
    printExpression((astExpression*)statement->condition);
    print(")");
    printStatement(statement->body);
}

static void printDoStatement(astDoStatement *statement) {
    print("do");
    printStatement(statement->body);
    print("while(");
    printExpression(statement->condition);
    print(");\n");
}

static void printForStatement(astForStatement *statement) {
    print("for(");
    if (statement->init)
        printExpression((astExpression*)statement->init);
    print("; ");
    if (statement->condition)
        printExpression(statement->condition);
    print("; ");
    if (statement->loop)
        printExpression(statement->loop);
    print(")");
    printStatement(statement->body);
}

static void printContinueStatement(astContinueStatement *statement) {
    print("continue;\n");
}

static void printBreakStatement(astBreakStatement *statement) {
    print("break;\n");
}

static void printReturnStatement(astReturnStatement *statement) {
    if (statement->expression) {
        print("return ");
        printExpression(statement->expression);
        print(";\n");
    } else {
        print("return;\n");
    }
}

static void printDiscardStatement(astDiscardStatement *statement) {
    print("discard;\n");
}

static void printStatement(astStatement *statement) {
    switch (statement->type) {
        case astStatement::kCompound:
            return printCompoundStatement((astCompoundStatement*)statement);
        case astStatement::kEmpty:
            return printEmptyStatement((astEmptyStatement*)statement);
        case astStatement::kDeclaration:
            return printDeclarationStatement((astDeclarationStatement*)statement);
        case astStatement::kExpression:
            return printExpressionStatement((astExpressionStatement*)statement);
        case astStatement::kIf:
            return printIfStetement((astIfStatement*)statement);
        case astStatement::kSwitch:
            return printSwitchStatement((astSwitchStatement*)statement);
        case astStatement::kCaseLabel:
            return printCaseLabelStatement((astCaseLabelStatement*)statement);
        case astStatement::kWhile:
            return printWhileStatement((astWhileStatement*)statement);
        case astStatement::kDo:
            return printDoStatement((astDoStatement*)statement);
        case astStatement::kFor:
            return printForStatement((astForStatement*)statement);
        case astStatement::kContinue:
            return printContinueStatement((astContinueStatement*)statement);
        case astStatement::kBreak:
            return printBreakStatement((astBreakStatement*)statement);
        case astStatement::kReturn:
            return printReturnStatement((astReturnStatement*)statement);
        case astStatement::kDiscard:
            return printDiscardStatement((astDiscardStatement*)statement);
    }
    print("\n");
}

static void printFunctionParameter(astFunctionParameter *parameter) {
    printStorage(parameter->storage);
    printAuxiliary(parameter->auxiliary);
    printMemory(parameter->memory);
    printPrecision(parameter->precision);
    printType(parameter->baseType);
    if (parameter->name.size())
        print(" %s", parameter->name.c_str());
    if (parameter->isArray)
        printArraySize(parameter->arraySizes);
}

static void printFunction(astFunction *function) {
    printType(function->returnType);
    print(" %s(", function->name.c_str());
    for (size_t i = 0; i < function->parameters.size(); i++)
        printFunctionParameter(function->parameters[i]);
    print(")");
    if (function->isPrototype) {
        print(";\n");
        return;
    }
    print(" {\n");
    for (size_t i = 0; i < function->statements.size(); i++)
        printStatement(function->statements[i]);
    print("}\n");
}

static void printTU(astTU *tu) {
    for (size_t i = 0; i < tu->globals.size(); i++)
        printGlobalVariable(tu->globals[i]);
    for (size_t i = 0; i < tu->functions.size(); i++)
        printFunction(tu->functions[i]);
}

int main(int argc, char **argv) {
    argc--;
    argv++;
    if (!argc) {
        fprintf(stderr, "expected source file\n");
        return 1;
    }

    FILE *fp = fopen(argv[0], "r");
    if (!fp) {
        fprintf(stderr, "failed to open `%s'\n", argv[0]);
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    std::string source;
    source.resize(length);
    if (fread(&source[0], length, 1, fp) != 1) {
        fprintf(stderr, "failed to read source\n");
        fclose(fp);
        return 1;
    }
    fclose(fp);

    parser p(source);
    astTU *tu = p.parse(astTU::kFragment);
    if (tu)
        printTU(tu);
    else
        fprintf(stderr, "%s\n", p.error());

    return 0;
}
