#ifndef AST_HDR
#define AST_HDR
#include <vector>    // std::vector
#include <string>    // std::string
#include <algorithm> // std::find

#include <stdlib.h>  // malloc, free

namespace glsl {

// Nodes are to inherit from astNode or astCollector
template <typename T>
struct astCollect;

template <typename T>
struct astNode {
    ~astNode() {
        ((T*)this)->~T();
    }

    void *operator new(size_t size, astCollect<T> *collector);
    void operator delete(void *ptr, astCollect<T> *collector);

private:
    void *operator new(size_t);
    void operator delete(void *);
};

template <typename T>
inline void *astNode<T>::operator new(size_t size, astCollect<T> *collector) {
    void *data = malloc(size);
    collector->m_nodes.push_back((astNode<T>*)data);
    return data;
}

template <typename T>
inline void astNode<T>::operator delete(void *data, astCollect<T> *collector) {
    typename std::vector<astNode<T>*>::iterator it = std::find(collector->m_nodes.begin(),
                                                               collector->m_nodes.end(),
                                                               data);
    if (it != collector->m_nodes.end())
        collector->m_nodes.erase(it);
    free(data);
}

template <typename T>
struct astCollect {
    ~astCollect();
    std::vector<astNode<T>*> m_nodes;
    friend struct astNode<T>;
};

template <typename T>
inline astCollect<T>::~astCollect() {
    for (size_t i = 0; i < m_nodes.size(); i++) {
        if (m_nodes[i])
            m_nodes[i]->~astNode<T>();
        free(m_nodes[i]);
        m_nodes.pop_back();
    }
}

struct astFunction;
struct astType;
struct astGlobalVariable;
struct astExpression;
struct astLayoutQualifier;
struct astStatement;

struct astTU : astCollect<astTU> {
    astTU() { }

    std::vector<astFunction*> functions;
    std::vector<astType*> types;
    std::vector<astGlobalVariable*> globals;

private:
    astTU(const astTU&);
    astTU &operator=(const astTU&);
};

struct astType : astNode<astType> {
    astType(bool builtin);
    bool builtin;
};

struct astStruct : astType {
    std::string name;
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
    astVariable();
    std::string name;
    astType *type;
    bool isArray;
    astConstantExpression *arraySize;
};

struct astFunctionVariable : astVariable {
    astFunctionVariable();
    bool isConst;
    astExpression *initialValue;
};

enum {
    kConst = 1 << 0,
    kIn = 1 << 1,
    kOut = 1 << 2,
    kCentroid = 1 << 3,
    kPatch = 1 << 4,
    kSample = 1 << 5,
    kUniform = 1 << 6,
    kInvariant = 1 << 7
};

struct astFunctionParameter : astVariable {
    astFunctionParameter();
    int flags; // only kIn and kOut are valid here
    // TODO: memory qualifier
    int precision;
};

enum {
    kSmooth,
    kFlat,
    kNoPerspective
};

struct astGlobalVariable : astVariable {
    astGlobalVariable();
    int flags;
    int interpolation;
    int precision;
    std::vector<astLayoutQualifier*> layoutQualifiers;
};

struct astLayoutQualifier : astNode<astLayoutQualifier> {
    astLayoutQualifier();
    // TODO: implement layout qualifiers
};

struct astFunction : astNode<astFunction> {
    astType *returnType;
    std::string name;
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
    astExpression *condition;
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
        kPlus,
        kMinus
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
    std::string name;
};

struct astArraySubscript : astExpression {
    astArraySubscript();
    astExpression *operand;
    astExpression *index;
};

struct astFunctionCall : astExpression {
    astFunctionCall();
    std::string name;
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
    astAssignmentExpression(int assignment, int type);
    int assignment;
};

struct astPlusExpression : astBinaryExpression {
    astPlusExpression();
};

struct astMinusExpression : astBinaryExpression {
    astMinusExpression();
};

}

#endif
