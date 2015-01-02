#include <stdio.h>  // fread, fclose, fprintf, stderr
#include <string.h> // strcmp, memcpy

#include <vector>

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
    if (variable->isPrecise)
        print("precise ");

    if (nameOnly) {
        print("%s", variable->name);
        return;
    }

    printType(variable->baseType);
    print(" %s", variable->name);

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
    std::vector<astLayoutQualifier*> &qualifiers = variable->layoutQualifiers;
    if (variable->layoutQualifiers.size()) {
        print("layout (");
        for (size_t i = 0; i < qualifiers.size(); i++) {
            astLayoutQualifier *qualifier = qualifiers[i];
            print("%s", qualifier->name);
            if (qualifier->initialValue) {
                print(" = ");
                printExpression(qualifier->initialValue);
            }
            if (i != qualifiers.size() - 1)
                print(", ");
        }
        print(") ");
    }

    printStorage(variable->storage);
    printAuxiliary(variable->auxiliary);
    printMemory(variable->memory);
    printPrecision(variable->precision);

    if (variable->isInvariant)
        print("invariant ");

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
    print(".%s", expression->name);
}

static void printArraySubscript(astArraySubscript *expression) {
    printExpression(expression->operand);
    print("[");
    printExpression(expression->index);
    print("]");
}

static void printFunctionCall(astFunctionCall *expression) {
    print("%s(", expression->name);
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

static void printEmptyStatement() {
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

static void printContinueStatement() {
    print("continue;\n");
}

static void printBreakStatement() {
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

static void printDiscardStatement() {
    print("discard;\n");
}

static void printStatement(astStatement *statement) {
    switch (statement->type) {
    case astStatement::kCompound:
        return printCompoundStatement((astCompoundStatement*)statement);
    case astStatement::kEmpty:
        return printEmptyStatement();
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
        return printContinueStatement();
    case astStatement::kBreak:
        return printBreakStatement();
    case astStatement::kReturn:
        return printReturnStatement((astReturnStatement*)statement);
    case astStatement::kDiscard:
        return printDiscardStatement();
    }
    print("\n");
}

static void printFunctionParameter(astFunctionParameter *parameter) {
    printStorage(parameter->storage);
    printAuxiliary(parameter->auxiliary);
    printMemory(parameter->memory);
    printPrecision(parameter->precision);
    printType(parameter->baseType);
    if (parameter->name)
        print(" %s", parameter->name);
    if (parameter->isArray)
        printArraySize(parameter->arraySizes);
}

static void printFunction(astFunction *function) {
    printType(function->returnType);
    print(" %s(", function->name);
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

struct sourceFile {
    const char *fileName;
    FILE *file;
    int shaderType;
};

int main(int argc, char **argv) {
    int shaderType = -1;
    std::vector<sourceFile> sources;
    while (argc > 1) {
        ++argv;
        --argc;
        if (argv[0][0] == '-' && argv[0][1]) {
            const char *what = argv[0] + 1;
            if (!strcmp(what, "c"))
                shaderType = astTU::kCompute;
            else if (!strcmp(what, "v"))
                shaderType = astTU::kVertex;
            else if (!strcmp(what, "tc"))
                shaderType = astTU::kTessControl;
            else if (!strcmp(what, "te"))
                shaderType = astTU::kTessEvaluation;
            else if (!strcmp(what, "g"))
                shaderType = astTU::kGeometry;
            else if (!strcmp(what, "f"))
                shaderType = astTU::kFragment;
            else {
                fprintf(stderr, "unknown option: `%s'\n", argv[0]);
                return 1;
            }
        } else {
            // Treat as fragment shader by default
            if (shaderType == -1)
                shaderType = astTU::kFragment;
            sourceFile source;
            if (!strcmp(argv[0], "-")) {
                source.fileName = "<stdin>";
                source.file = stdin;
                source.shaderType = shaderType;
                sources.push_back(source);
            } else {
                source.fileName = argv[0];
                if ((source.file = fopen(argv[0], "r"))) {
                    source.shaderType = shaderType;
                    sources.push_back(source);
                } else {
                    fprintf(stderr, "failed to read shader file: `%s' (ignoring)\n", argv[0]);
                }
            }
        }
    }

    for (size_t i = 0; i < sources.size(); i++) {
        std::vector<char> contents;
        // Read contents of file
        if (sources[i].file != stdin) {
            fseek(sources[i].file, 0, SEEK_END);
            contents.resize(ftell(sources[i].file));
            fseek(sources[i].file, 0, SEEK_SET);
            fread(&contents[0], 1, contents.size(), sources[i].file);
            fclose(sources[i].file);
        } else {
            char buffer[1024];
            int c;
            while ((c = fread(buffer, 1, sizeof(buffer), stdin))) {
                contents.reserve(contents.size() + c);
                contents.insert(contents.end(), buffer, buffer + c);
            }
        }
        contents.push_back('\0');
        parser p(&contents[0], sources[i].fileName);
        astTU *tu = p.parse(sources[i].shaderType);
        if (tu) {
            printTU(tu);
        } else {
            fprintf(stderr, "%s\n", p.error());
        }
    }
    return 0;
}
