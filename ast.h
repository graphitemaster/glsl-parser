#ifndef AST_HDR
#define AST_HDR
#include <vector>    // std::vector
#include <stdlib.h>  // malloc, free

namespace glsl {

// Type-erasure
template <typename T>
static inline void astDestroy(void *self) {
    ((T*)self)->~T();
    free(self);
}

struct astMemory {
    astMemory() : data(0), dtor(0) { }
    template <typename T>
    astMemory(T *data) : data((void*)data), dtor(&astDestroy<T>) { }
    void *data;
    void (*dtor)(void*);
    void destroy() {
        dtor(data);
    }
};

// Nodes are to inherit from astNode or astCollector
template <typename T>
struct astNode {
    void *operator new(size_t size, std::vector<astMemory> *collector) throw() {
        void *data = malloc(size);
        if (data)
            collector->push_back(astMemory((T*)data));
        return data;
    }
private:
    void *operator new(size_t);
    void operator delete(void *);
};


struct astFunction;
struct astType;
struct astGlobalVariable;
struct astExpression;
struct astLayoutQualifier;
struct astStatement;

struct astTU {
    astTU(int type);

    enum {
        kCompute,
        kVertex,
        kTessControl,
        kTessEvaluation,
        kGeometry,
        kFragment
    };

    std::vector<astFunction*> functions;
    std::vector<astType*> types;
    std::vector<astGlobalVariable*> globals;

    int type;

private:
    astTU(const astTU&);
    astTU &operator=(const astTU&);
};

struct astType : astNode<astType> {
    astType(bool builtin);
    bool builtin;
};

struct astStruct : astType {
    astStruct();
    char *name;
};

struct astBuiltin : astType {
    astBuiltin(int type);
    int type; // kKeyword_*
};

typedef astExpression astConstantExpression;

enum {
    kHighp,
    kMediump,
    kLowp
};

struct astVariable : astNode<astVariable> {
    enum {
        kFunction,
        kParameter,
        kGlobal
    };
    astVariable(int type);
    char *name;
    astType *baseType;
    bool isArray;
    bool isPrecise;
    int type;
    std::vector<astConstantExpression *> arraySizes;
};

struct astFunctionVariable : astVariable {
    astFunctionVariable();
    bool isConst;
    astExpression *initialValue;
};

// Storage qualifiers
enum {
    kConst,
    kIn,
    kOut,
    kInOut,
    kAttribute,
    kUniform,
    kVarying,
    kBuffer,
    kShared
};

// Auxiliary storage qualifiers
enum {
    kCentroid,
    kSample,
    kPatch,
};

// Memory qualifiers
enum {
    kCoherent = 1 << 0,
    kVolatile = 1 << 1,
    kRestrict = 1 << 2,
    kReadOnly = 1 << 3,
    kWriteOnly = 1 << 4
};

struct astFunctionParameter : astVariable {
    astFunctionParameter();
    int storage; // in or out only
    int auxiliary;
    int memory;
    int precision;
};

enum {
    kSmooth,
    kFlat,
    kNoPerspective
};

struct astGlobalVariable : astVariable {
    astGlobalVariable();
    int storage;
    int auxiliary;
    int memory;
    int precision;
    int interpolation;
    bool isInvariant;
    astConstantExpression *initialValue;
    std::vector<astLayoutQualifier*> layoutQualifiers;
};

struct astLayoutQualifier : astNode<astLayoutQualifier> {
    astLayoutQualifier();
    char *name;
    astConstantExpression *initialValue;
};

struct astFunction : astNode<astFunction> {
    astFunction();
    astType *returnType;
    char *name;
    std::vector<astFunctionParameter*> parameters;
    std::vector<astStatement*> statements;
    bool isPrototype;
};

struct astDeclaration : astNode<astDeclaration> {
    astDeclaration();
    astVariable *variable;
};

struct astStatement : astNode<astStatement> {
    astStatement(int type);
    enum {
        kCompound,
        kEmpty,
        kDeclaration,
        kExpression,
        kIf,
        kSwitch,
        kCaseLabel,
        kWhile,
        kDo,
        kFor,
        kContinue,
        kBreak,
        kReturn,
        kDiscard
    };
    int type;
    const char *name() const;
};

struct astSimpleStatement : astStatement {
    astSimpleStatement(int type);
};

struct astCompoundStatement : astStatement {
    astCompoundStatement();
    std::vector<astStatement*> statements;
};

struct astEmptyStatement : astSimpleStatement {
    astEmptyStatement();
};

struct astDeclarationStatement : astSimpleStatement {
    astDeclarationStatement();
    std::vector<astFunctionVariable*> variables;
};

struct astExpressionStatement : astSimpleStatement {
    astExpressionStatement(astExpression *expression);
    astExpression *expression;
};

struct astIfStatement : astSimpleStatement {
    astIfStatement();
    astExpression *condition;
    astStatement *thenStatement;
    astStatement *elseStatement;
};

struct astSwitchStatement : astSimpleStatement {
    astSwitchStatement();
    astExpression *expression;
    std::vector<astStatement*> statements;
};

struct astCaseLabelStatement : astSimpleStatement {
    astCaseLabelStatement();
    astConstantExpression *condition;
    bool isDefault;
};

struct astIterationStatement : astSimpleStatement {
    astIterationStatement(int type);
};

struct astWhileStatement : astIterationStatement {
    astWhileStatement();
    astSimpleStatement *condition; // astExpressionStatement or astDeclarationStatement only
    astStatement *body;
};

struct astDoStatement : astIterationStatement {
    astDoStatement();
    astStatement *body;
    astExpression *condition;
};

struct astForStatement : astIterationStatement {
    astForStatement();
    astSimpleStatement *init; // astExpressionStatement or astDeclarationStatement only
    astExpression *condition;
    astExpression *loop;
    astStatement *body;
};

struct astJumpStatement : astStatement {
    astJumpStatement(int type);
};

struct astContinueStatement : astJumpStatement {
    astContinueStatement();
};

struct astBreakStatement : astJumpStatement {
    astBreakStatement();
};

struct astReturnStatement : astJumpStatement {
    astReturnStatement();
    astExpression *expression;
};

struct astDiscardStatement : astJumpStatement {
    astDiscardStatement();
};

struct astExpression : astNode<astExpression> {
    astExpression(int type);
    // Base class
    enum {
        kIntConstant,
        kUIntConstant,
        kFloatConstant,
        kDoubleConstant,
        kBoolConstant,
        kVariableIdentifier,
        kFieldOrSwizzle,
        kArraySubscript,
        kFunctionCall,
        kConstructorCall,
        kPostIncrement,
        kPostDecrement,
        kUnaryMinus,
        kUnaryPlus,
        kBitNot,
        kLogicalNot,
        kPrefixIncrement,
        kPrefixDecrement,
        kSequence,
        kAssign,
        kOperation
    };
    int type;
};

struct astIntConstant : astExpression {
    astIntConstant(int value);
    int value;
};

struct astUIntConstant : astExpression {
    astUIntConstant(unsigned int value);
    unsigned int value;
};

struct astFloatConstant : astExpression {
    astFloatConstant(float value);
    float value;
};

struct astDoubleConstant : astExpression {
    astDoubleConstant(double value);
    double value;
};

struct astBoolConstant : astExpression {
    astBoolConstant(bool value);
    bool value;
};

struct astVariableIdentifier : astExpression {
    astVariableIdentifier(astVariable *variable);
    astVariable *variable;
};

struct astFieldOrSwizzle : astExpression {
    astFieldOrSwizzle();
    astExpression *operand;
    char *name;
};

struct astArraySubscript : astExpression {
    astArraySubscript();
    astExpression *operand;
    astExpression *index;
};

struct astFunctionCall : astExpression {
    astFunctionCall();
    char *name;
    std::vector<astExpression*> parameters;
};

struct astConstructorCall : astExpression {
    astConstructorCall();
    astType *type;
    std::vector<astExpression*> parameters;
};

struct astUnaryExpression : astExpression {
    // Base class
    astUnaryExpression(int type, astExpression *operand);
    astExpression *operand;
};

struct astBinaryExpression : astExpression {
    // Base class
    astBinaryExpression(int type);
    astExpression *operand1;
    astExpression *operand2;
};

struct astPostIncrementExpression : astUnaryExpression {
    astPostIncrementExpression(astExpression *operand);
};

struct astPostDecrementExpression : astUnaryExpression {
    astPostDecrementExpression(astExpression *operand);
};

struct astUnaryPlusExpression : astUnaryExpression {
    astUnaryPlusExpression(astExpression *operand);
};

struct astUnaryMinusExpression : astUnaryExpression {
    astUnaryMinusExpression(astExpression *operand);
};

struct astUnaryBitNotExpression : astUnaryExpression {
    astUnaryBitNotExpression(astExpression *operand);
};

struct astUnaryLogicalNotExpression : astUnaryExpression {
    astUnaryLogicalNotExpression(astExpression *operand);
};

struct astPrefixIncrementExpression : astUnaryExpression {
    astPrefixIncrementExpression(astExpression *operand);
};

struct astPrefixDecrementExpression : astUnaryExpression {
    astPrefixDecrementExpression(astExpression *operand);
};

struct astSequenceExpression : astBinaryExpression {
    astSequenceExpression();
};

struct astAssignmentExpression : astBinaryExpression {
    astAssignmentExpression(int assignment);
    int assignment;
};

struct astOperationExpression : astBinaryExpression {
    astOperationExpression(int operation);
    int operation;
};

}

#endif
