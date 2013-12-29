#include <iostream>
#include "clang/Parse/Parser.h"
#include "ParsePragma.h"
#include "RAIIObjectsForParser.h"
#include "clang/Basic/RogerConstr.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "clang/Frontend/Utils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Program.h"
using namespace clang;

namespace clang {

struct RogerNonType;
struct RogerDeclaration;
struct RogerClassDecl;
struct RogerNamespace;

struct RogerDeclList {
  SmallVector<RogerNonType*, 4> NonType;
  SmallVector<RogerDeclaration*, 4> NameDeclaration;
  SmallVector<RogerClassDecl*, 4> ClassDecl;
};

struct RogerNamespaceDeclList : RogerDeclList {
  SmallVector<RogerNamespace*, 4> Namespace;
};

struct RogerRange {
  int begin;
  int end;
  int size() {
    return end - begin;
  }
};



struct RogerNonType {
  RogerNonType(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  RogerRange range;
};

struct RogerDeclaration {
  RogerDeclaration(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  enum NameKind {
    NAME_PLAIN, NAME_OPERATOR, NAME_ARRAY_OPERATOR, NAME_CONVERSION, NAME_CONSTRUCTOR, NAME_DESTRUCTOR, _NAME_ERROR
  };
  NameKind nameKind;
  int nameToken;
  bool isType;
  RogerRange declaration;
  //RogerRange declarator;
  //int declaratorNumber;
};

struct RogerClassDecl {
  RogerClassDecl(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  int nameToken;
  int tagToken;
  bool isTemplate;
  RogerRange declaration;
  RogerRange classTokens;
  RogerDeclList inner;
  SmallVector<RogerNonType*, 1> Using;
};

struct RogerNamespace {
  RogerNamespace(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int nameToken;
  RogerNamespaceDeclList inner;
};

struct RogerTopLevelDecls {
  SmallVector<Parser::DeclGroupPtrTy, 4> List;
};

class RogerOverviewFileParser {
private:
  const unsigned char *m_current;
  const unsigned char *m_end;
  CachedTokens &Toks;
public:
  RogerOverviewFileParser(llvm::MemoryBuffer& buffer, CachedTokens &T) : Toks(T) {
    m_current = reinterpret_cast<const unsigned char *>(buffer.getBufferStart());
    m_end = reinterpret_cast<const unsigned char *>(buffer.getBufferEnd());
  }
  RogerNamespaceDeclList* parse() {
    char mainCode = takeByte();
    if (mainCode == 0) {
      return parseData();
    } else if (mainCode == 1) {
      parseError();
      return NULL;
    } else {
      assert(false && "Unknown code");
      return NULL;
    }
  }

private:
  RogerNamespaceDeclList* parseData() {
    RogerNamespaceDeclList* list = new RogerNamespaceDeclList;
    while (m_current < m_end) {
      parseRegion(list, list, 0);
    }
    return list;
  }
  void parseRegion(RogerDeclList* list, RogerNamespaceDeclList* nslist, RogerClassDecl* classDecl) {
    char regionCode = takeByte();
    switch (regionCode) {
    case 1:
      parseNonType(list);
      break;
    case 2:
      parseDeclaration(list);
      break;
    case 3:
      parseClassDecl(list);
      break;
    case 4:
      parseFunction(list);
      break;
    case 5:
      assert(nslist);
      parseNamespace(nslist);
      break;
    case 6:
      assert(classDecl && "Using must be in class");
      parseUsing(classDecl);
      break;
    default:
      assert(false && "Unknown code");
    }
  }
  void parseNonType(RogerDeclList* list) {
    RogerNonType *nonType = new RogerNonType(Toks);
    nonType->visibility = takeByte();
    parseRange(nonType->range);
    list->NonType.push_back(nonType);
  }
  void parseDeclaration(RogerDeclList* list) {
    RogerDeclaration *declaration = new RogerDeclaration(Toks);
    declaration->visibility = takeByte();
    declaration->isType = takeByte();
    declaration->nameToken = takeInt16();
    declaration->nameKind = RogerDeclaration::NAME_PLAIN;
    parseRange(declaration->declaration);
    RogerRange declaratorRangeIgnore;
    parseRange(declaratorRangeIgnore);
    int declaratorNumber = takeInt16();
    assert(declaratorNumber == 0);
    list->NameDeclaration.push_back(declaration);
  }
  void parseClassDecl(RogerDeclList* list) {
    RogerClassDecl* classDecl = new RogerClassDecl(Toks);
    classDecl->visibility = takeByte();
    classDecl->nameToken = takeInt16();
    parseRange(classDecl->declaration);
    parseRange(classDecl->classTokens);
    while (true) {
      assert(m_current < m_end);
      if (*m_current == 0) {
        ++m_current;
        break;
      }
      parseRegion(&classDecl->inner, 0, classDecl);
    }
    list->ClassDecl.push_back(classDecl);
  }
  void parseFunction(RogerDeclList* list) {
    RogerDeclaration *declaration = new RogerDeclaration(Toks);
    declaration->isType = false;
    declaration->visibility = takeByte();
    int nameKind = takeByte();
    switch (nameKind) {
    case 0:
      declaration->nameKind = RogerDeclaration::NAME_PLAIN;
      declaration->nameToken = takeInt16();
      break;
    case 1:
      declaration->nameKind = RogerDeclaration::NAME_OPERATOR;
      declaration->nameToken = takeInt16();
      break;
    case 2:
      declaration->nameKind = RogerDeclaration::NAME_ARRAY_OPERATOR;
      declaration->nameToken = takeInt16();
      break;
    case 3:
      declaration->nameKind = RogerDeclaration::NAME_CONVERSION;
      declaration->nameToken = 0;
      break;
    case 4:
      declaration->nameKind = RogerDeclaration::NAME_CONSTRUCTOR;
      declaration->nameToken = 0;
      break;
    case 5:
      declaration->nameKind = RogerDeclaration::NAME_DESTRUCTOR;
      declaration->nameToken = 0;
      break;
    default:
      assert(false && "Unknown name code");
    }
    parseRange(declaration->declaration);
    list->NameDeclaration.push_back(declaration);
  }
  void parseNamespace(RogerNamespaceDeclList* list) {
    RogerNamespace* ns = new RogerNamespace(Toks);
    ns->nameToken = takeInt16();
    while (true) {
      assert(m_current < m_end);
      if (*m_current == 0) {
        ++m_current;
        break;
      }
      parseRegion(&ns->inner, &ns->inner, 0);
    }
    list->Namespace.push_back(ns);
  }

  void parseUsing(RogerClassDecl* classDecl) {
    RogerNonType *nonType = new RogerNonType(Toks);
    nonType->visibility = takeByte();
    parseRange(nonType->range);
    classDecl->Using.push_back(nonType);
  }

  void parseRange(RogerRange &range) {
    range.begin = takeInt16();
    range.end = takeInt16();
  }

  void parseError() {
    std::cout << "Error";
    int len = takeInt16();
    assert(m_current + len == m_end);
    StringRef message(reinterpret_cast<const char *>(m_current), len);
    std::cout << message.str() << '\n';
  }

  char takeByte() {
    assert(m_current < m_end);
    return *(m_current++);
  }
  int takeInt16() {
    assert(m_current + 1 < m_end);
    int result = m_current[0] + m_current[1] * 256;
    m_current += 2;
    return result;
  }
};

}


class RogerCallbackGuard {
  bool first;
public:
  RogerCallbackGuard() : first(true) {}
  void check() {
    assert(first);
    first = false;
  }
};

class RogerCallbackInUseScope {
  bool &b;
public:
  RogerCallbackInUseScope(bool *bb) : b(*bb) {
    b = true;
  }
  ~RogerCallbackInUseScope() {
    b = false;
  }
};

RogerNamespaceDeclList* Parser::ParseRogerPartOverview(CachedTokens &Toks) {
  const char* tokensFileName = "tokens";
  const char* overviewFileName = "overview";

  std::string Error;
  OwningPtr<llvm::raw_fd_ostream> OS;
  OS.reset(new llvm::raw_fd_ostream(
      tokensFileName, Error,
      llvm::sys::fs::F_Binary));
  assert(Error.empty());

  CacheTokensRoger(Toks, OS.get());
  OS->close();

  // Call overview parser.
  {
    const char* executableName = "/home/peter/clang-roger/cdt-hack/parse_overview";
    StringRef Executable(executableName);
    SmallVector<const char*, 128> Argv;
    Argv.push_back(executableName);
    Argv.push_back(tokensFileName);
    Argv.push_back(overviewFileName);
    Argv.push_back(0);

    int res = llvm::sys::ExecuteAndWait(Executable, Argv.data());
    assert(res == 0 && "Result code is not null");
  }

  // Parse overview.
  {
    OwningPtr<llvm::MemoryBuffer> File;

    bool res = llvm::MemoryBuffer::getFile(overviewFileName, File);
    assert(!res && "Cannot read file");

    RogerOverviewFileParser parser(*File.get(), Toks);

    return parser.parse();
  }
}

struct Parser::RogerParsingQueue::Item {
  DeclContext *DC;
  LateParsedDeclaration *lateDecl;
};

Parser::RogerParsingQueue::Item *Parser::RogerParsingQueue::pop() {
  if (List.empty()) {
    return 0;
  }
  Item *result = List.back();
  List.pop_back();
  return result;
}

void Parser::RogerParsingQueue::addAndWrap(LateParsedDeclaration *lateDecl, DeclContext *dc) {
  Item *item = new Item;
  item->DC = dc;
  item->lateDecl = lateDecl;
  List.push_back(item);
}

class clang::RogerParseScope {
  Parser *P;
  Sema &Actions;
  RogerConstr<Parser::TemplateParameterDepthRAII> CurTemplateDepthTracker;
  Scope *SavedScope;
  RogerConstr<Sema::ContextRAII> GlobalSavedContext;
  SmallVector<Parser::ParseScope*, 4> TemplateParamScopeStack;

  bool closed;
public:
  RogerParseScope(Parser *PP, DeclContext *context)
      : P(PP), Actions(PP->Actions), closed(false)
  {
    // Track template parameter depth.
    new (CurTemplateDepthTracker.getBuffer()) Parser::TemplateParameterDepthRAII(P->TemplateParameterDepth);


    SmallVector<DeclContext*, 4> DeclContextsToReenter;
    DeclContext *DD = context;
    while (!DD->isTranslationUnit()) {
      assert(DD);
      DeclContextsToReenter.push_back(DD);
      DD = DD->getLexicalParent();
    }

    SavedScope = P->getCurScope();
    P->EnterScope(Scope::DeclScope);
    P->getCurScope()->Init(0, Scope::DeclScope);
    P->getCurScope()->setEntity(DD);

    // To restore the context after late parsing.
    new (GlobalSavedContext.getBuffer()) Sema::ContextRAII(Actions, DD);

    // Reenter template scopes from outermost to innermost.
    SmallVectorImpl<DeclContext *>::reverse_iterator II =
        DeclContextsToReenter.rbegin();
    for (; II != DeclContextsToReenter.rend(); ++II) {
      if (ClassTemplatePartialSpecializationDecl *MD =
              dyn_cast_or_null<ClassTemplatePartialSpecializationDecl>(*II)) {
        TemplateParamScopeStack.push_back(
            new Parser::ParseScope(P, Scope::TemplateParamScope));
        Actions.ActOnReenterTemplateScope(P->getCurScope(), MD);
        ++CurTemplateDepthTracker.get();
      } else if (CXXRecordDecl *MD = dyn_cast_or_null<CXXRecordDecl>(*II)) {
        bool IsClassTemplate = MD->getDescribedClassTemplate() != 0;
        TemplateParamScopeStack.push_back(
            new Parser::ParseScope(P, Scope::TemplateParamScope,
                          /*ManageScope*/IsClassTemplate));
        Actions.ActOnReenterTemplateScope(P->getCurScope(),
                                          MD->getDescribedClassTemplate());
        if (IsClassTemplate)
          ++CurTemplateDepthTracker.get();
      }
      TemplateParamScopeStack.push_back(new Parser::ParseScope(P, Scope::DeclScope));
      Actions.PushDeclContext(Actions.getCurScope(), *II);
    }
  }

  void close() {
    // Exit scopes.
    SmallVectorImpl<Parser::ParseScope *>::reverse_iterator I =
      TemplateParamScopeStack.rbegin();
    for (; I != TemplateParamScopeStack.rend(); ++I)
      delete *I;


    GlobalSavedContext.del();

    P->getCurScope()->TweakParent(SavedScope);
    P->ExitScope();

    CurTemplateDepthTracker.del();

    closed = true;
  }
  ~RogerParseScope() {
    if (!closed) {
      close();
    }
  }
};

class clang::RogerRecordParseScope {
  Parser *P;
  Sema &Actions;
  RogerConstr<RogerParseScope> rogerParseScope;
  Decl *TagOrTemplate;
  Sema::ParsingClassState SemaParsingState;
  bool closed;
public:
  RogerRecordParseScope(Parser *PP, Decl *TagOrTemplate, Parser::ParsingClass *parsingClass)
      : P(PP), Actions(PP->Actions), TagOrTemplate(TagOrTemplate), closed(false) {
    new (rogerParseScope.getBuffer()) RogerParseScope(PP, dyn_cast<DeclContext>(TagOrTemplate));

    Actions.ActOnTagResumeDefinitionRoger(P->getCurScope(), TagOrTemplate);

    SemaParsingState = P->PushParsingClassRoger(parsingClass);
  }
  void close() {
    Parser::ParsingClass *ignoreParsingClass;
    P->PopParsingClass(SemaParsingState, &ignoreParsingClass);

    Actions.ActOnTagPauseDefinitionRoger(TagOrTemplate);
    rogerParseScope.del();
    closed = true;
  }
  ~RogerRecordParseScope() {
    if (!closed) {
      close();
    }
  }
};

class clang::RogerNamespaceParseScope {
  //Parser *P;
  RogerConstr<RogerParseScope> rogerParseScope;
  bool closed;

public:
  RogerNamespaceParseScope(Parser *PP, DeclContext *DC, Parser::RogerParsingNamespace *parsingNs)
      : //P(PP),
        closed(false) {
    new (rogerParseScope.getBuffer()) RogerParseScope(PP, DC);
    //P->RogerNamespaceStack.push(parsingNs);
  }
  void close() {
    //P->RogerNamespaceStack.pop();
    rogerParseScope.del();
    closed = true;
  }
  ~RogerNamespaceParseScope() {
    if (!closed) {
      close();
    }
  }
};


void Parser::ParseRogerPartOpt(ASTConsumer *Consumer) {
  if (!Tok.is(tok::kw_capybara)) {
    return;
  }

  ConsumeToken();

  CachedTokens DefinitionToks;
  CachedTokens DeclarationToks;

  ConsumeAndStoreUntil(tok::eof, tok::kw_capybara, DefinitionToks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);

  RogerNamespaceDeclList *declarationPart = 0;
  if (Tok.is(tok::kw_capybara)) {
    ConsumeAnyToken();

    ConsumeAndStoreUntil(tok::eof, DeclarationToks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
    declarationPart = ParseRogerPartOverview(DeclarationToks);
  }

  RogerNamespaceDeclList *definitionPart = ParseRogerPartOverview(DefinitionToks);

  struct MainCallback : Sema::RogerOnDemandParserInt {
    Parser *parser;
    RogerCallbackGuard guard;
    void ParseFunction(LateParsedTemplate &LPT) {
      guard.check();
      parser->ParseLateTemplatedFuncDef(LPT);
    }
  };
  MainCallback semaCallback;
  semaCallback.parser = this;

  RogerTopLevelDecls topLevelDecls;
  RogerTopLevelDecls secondaryTopLevelDecls;

  RogerParsingQueue parsingQueue;
  rogerParsingQueue = &parsingQueue;

  Actions.ActOnRogerModeStart(&semaCallback);

  RogerParsingNamespace *topParsingNs = new RogerParsingNamespace;

  FillRogerNamespaceWithNames(definitionPart, Actions.CurContext, topParsingNs, &topLevelDecls);
  if (declarationPart) {
    FillRogerNamespaceWithNames(declarationPart, Actions.CurContext, topParsingNs, &secondaryTopLevelDecls);
  }

  // Only finish topLevelDecls.
  SmallVector<DeclGroupRef, 4> UnwappedTopLevelList(topLevelDecls.List.size());
  for (DeclGroupPtrTy *it = topLevelDecls.List.begin(); it != topLevelDecls.List.end(); ++it) {
    UnwappedTopLevelList.push_back(it->get());
  }
  SmallVector<DeclGroupRef, 4> SecondUnwappedTopLevelList(topLevelDecls.List.size());
  for (DeclGroupPtrTy *it = secondaryTopLevelDecls.List.begin(); it != secondaryTopLevelDecls.List.end(); ++it) {
    SecondUnwappedTopLevelList.push_back(it->get());
  }

  Actions.ActOnNamespaceFinishRoger(getCurScope()->getEntity(), &UnwappedTopLevelList);
  Actions.ActOnNamespaceFinishRoger(getCurScope()->getEntity(), &SecondUnwappedTopLevelList);

  while (RogerParsingQueue::Item *item = rogerParsingQueue->pop()) {
    DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
    ParenBraceBracketBalancer BalancerRAIIObj(*this);

    RogerParseScope parseScope(this, item->DC);
    item->lateDecl->ParseLexedMethodDeclarations();
    item->lateDecl->ParseLexedMethodDefs();
    item->lateDecl->ParseRogerLexedStaticInitializers();
    delete item->lateDecl;

    delete item;
  }

  for (size_t i = 0; i < topLevelDecls.List.size(); ++i) {
    Consumer->HandleTopLevelDecl(topLevelDecls.List[i].get());
  }

  Actions.ActOnRogerModeFinish();
  rogerParsingQueue = 0;
}

static AccessSpecifier calculateASRaw(int explicitVisibility, int defaultVisibility) {
  int vis = explicitVisibility ? explicitVisibility : defaultVisibility;
  switch (vis) {
  default: assert(false && "Unknown visibility");
  case 0: return AS_none;
  case 1: return AS_public;
  case 2: return AS_protected;
  case 3: return AS_private;
  }
}

static AccessSpecifier calculateAS(int explicitVisibility, DeclContext *DC) {
  int contextVisibility = 0;
  if (RecordDecl *Rec = dyn_cast<RecordDecl>(DC)) {
    if (Rec->getTagKind() == TTK_Class) {
      contextVisibility = 3;
    } else {
      contextVisibility = 1;
    }
  }
  return calculateASRaw(explicitVisibility, contextVisibility);
}

template<typename T, T V>
struct RogerLiteral {
  static const T v;
};
template<typename T, T V>
const T RogerLiteral<T, V>::v(V);

class clang::RogerNestedTokensState {
  Parser *P;
  const Token *begin;
  int size;
  bool isClosed;
  Token sentinel;
  Token saved;
  Preprocessor::RogerStreamState *ticket;
  Token PrevTok;
  unsigned short PrevParenCount, PrevBracketCount, PrevBraceCount;
public:
  RogerNestedTokensState(Parser *P, const Token *begin, int size) : P(P), begin(begin), size(size), isClosed(false) {
    saved = P->Tok;

    PrevParenCount = P->ParenCount;
    PrevBracketCount = P->BracketCount;
    PrevBraceCount = P->BraceCount;
    P->ParenCount = 0;
    P->BracketCount = 0;
    P->BraceCount = 0;

    ticket = P->PP.EnterRogerTokenStream(begin, size, false);
    P->ConsumeAnyToken();
  }
  ~RogerNestedTokensState() {
    if (!isClosed) {
      close();
    }
  }
  void skipRest() {
    while (P->Tok.isNot(tok::eof)) {
      P->ConsumeAnyToken();
    }
  }
  unsigned getPos() {
    SourceLocation Loc = P->Tok.getLocation();
    for (int pos = 0; pos < size; pos++) {
      if (begin[pos].getLocation() == Loc) {
        return pos;
      }
    }
    return -1;
  }

  void close() {
    if (P->Tok.isNot(tok::eof)) {
      P->Diag(P->Tok, diag::err_roger_invalid_token_before_region_end)
        << getTokenSimpleSpelling(P->Tok.getKind());
      while (P->Tok.isNot(tok::eof)) {
        P->ConsumeAnyToken();
      }
    }

    P->PP.ExitRogerTokenStream(ticket);
    P->Tok = saved;

    P->ParenCount = PrevParenCount;
    P->BracketCount = PrevBracketCount;
    P->BraceCount = PrevBraceCount;
  }
};

void Parser::FillRogerNamespaceWithNames(RogerNamespaceDeclList *rogerNsDeclList, DeclContext *DC, RogerParsingNamespace *parsingNs, RogerTopLevelDecls *topLevelDecs) {
  Sema::RogerLogScope logScope("FillRogerNamespaceWithNames");
  if (NamedDecl *nd = dyn_cast<NamedDecl>(DC)) {
    nd->printName(logScope.outs_nl());
    logScope.outs() << '\n';
  }

  struct TypesForRec {
    typedef RogerNamespaceDeclList DeclList;
    typedef DeclContext DeclContext;
    static SmallVector<RogerNamespace*, 4> *getNsDeclList(RogerNamespaceDeclList *declList) {
      return &declList->Namespace;
    }
    typedef RogerParsingNamespace ParsingState;
    typedef RogerLiteral<Parser::DeclGroupPtrTy (Parser::*)(RogerDeclaration *, DeclContext *, RogerParsingNamespace *), &Parser::ParseRogerDeclarationRegion> ParseNamedDeclaration;
  };
  RogerNamespaceParseScope scope(this, DC, parsingNs);
  FillRogerDeclContextWithNames<TypesForRec>(rogerNsDeclList, DC, parsingNs, topLevelDecs);

  struct CompleteParsingCb : RogerItemizedLateParseCallback {
    Parser *P;
    RogerParsingNamespace *parsingNs;
    DeclContext *DC;
    RogerCallbackGuard guard;
    bool isInUse;
    CompleteParsingCb() : isInUse(false) {}
    bool isBusy() {
      return isInUse;
    }
    void parseDeferred() {
      RogerCallbackInUseScope inUse(&isInUse);
      guard.check();

      //P->RogerCompleteNamespaceParsing(DC, parsingNs);
    }
  };
  CompleteParsingCb *cb = new CompleteParsingCb;
  cb->P = this;
  cb->parsingNs = parsingNs;
  cb->DC = DC;

  DC->RogerCompleteParsingCallback = cb;
}

void Parser::FillRogerRecordWithNames(RogerClassDecl *rogerClassDecl, RecordDecl *RD, ParsingClass *parsingClass) {
  Sema::RogerLogScope logScope("FillRogerRecordWithNames");
  RD->printName(logScope.outs_nl());
  logScope.outs() << '\n';


  struct TypesForNs {
    typedef RogerDeclList DeclList;
    typedef RecordDecl DeclContext;
    static SmallVector<RogerNamespace*, 4> *getNsDeclList(RogerDeclList *declList) {
      return 0;
    }
    typedef ParsingClass ParsingState;
    typedef RogerLiteral<DeclGroupPtrTy (Parser::*)(RogerDeclaration*, Decl*, ParsingClass *), &Parser::ParseRogerMemberRegion> ParseNamedDeclaration;
  };

  RogerRecordParseScope scope(this, RD, parsingClass);
  {
    FillRogerDeclContextWithNames<TypesForNs>(&rogerClassDecl->inner, RD, parsingClass, 0);
  }

  for (size_t i = 0; i < rogerClassDecl->Using.size(); ++i) {
    RogerNonType *rogerUsing = rogerClassDecl->Using[i];

    RogerNestedTokensState parseNestedTokens(this, &rogerUsing->Toks[rogerUsing->range.begin], rogerUsing->range.size());

    DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
    ParenBraceBracketBalancer BalancerRAIIObj(*this);

    AccessSpecifier AS = calculateAS(rogerUsing->visibility, RD);

    ParsedAttributesWithRange attrs(AttrFactory);
    ParseCXXClassMemberDeclaration(AS, attrs.getList());
  }

  struct CompleteParsingCb : RogerItemizedLateParseCallback {
    Parser *P;
    RecordDecl *RD;
    ParsingClass *parsingClass;
    RogerCallbackGuard guard;
    bool isInUse;
    CompleteParsingCb() : isInUse(false) {}
    bool isBusy() {
      return isInUse;
    }
    void parseDeferred() {
      RogerCallbackInUseScope inUse(&isInUse);
      guard.check();

      P->RogerCompleteCXXMemberSpecificationParsing(RD, parsingClass);
    }
  };
  CompleteParsingCb *cb = new CompleteParsingCb;
  cb->P = this;
  cb->RD = RD;
  cb->parsingClass = parsingClass;

  RD->RogerCompleteParsingCallback = cb;
}

template<typename Types>
void Parser::FillRogerDeclContextWithNames(typename Types::DeclList *rogerDeclList, typename Types::DeclContext *DC, typename Types::ParsingState *parsingObj, RogerTopLevelDecls *topLevelDecs) {

  for (size_t i = 0; i < rogerDeclList->NameDeclaration.size(); ++i) {
    RogerDeclaration* decl = rogerDeclList->NameDeclaration[i];

    struct CallbackImpl : public RogerItemizedLateParseCallback {
      Parser* parser;
      RogerDeclaration* decl;
      typename Types::DeclContext *DC;
      RogerTopLevelDecls *topLevelDecs;
      RogerCallbackGuard guard;
      bool isInUse;
      typename Types::ParsingState *parsingObj;
      CallbackImpl() : isInUse(false) {}
      bool isBusy() {
        return isInUse;
      }
      void parseDeferred() {
        RogerCallbackInUseScope inUse(&isInUse);
        guard.check();
        DeclGroupPtrTy Result = (parser->*(Types::ParseNamedDeclaration::v))(decl, DC, parsingObj);
        if (topLevelDecs && Result) {
          topLevelDecs->List.push_back(Result);
        }
      }
    };

    CallbackImpl *cb = new CallbackImpl();
    cb->parser = this;
    cb->decl = decl;
    cb->DC = DC;
    cb->parsingObj = parsingObj;
    cb->topLevelDecs = topLevelDecs;
    bool isArray = false;
    switch (decl->nameKind) {
    case RogerDeclaration::NAME_PLAIN: {
      Token &nameToken = decl->Toks[decl->nameToken];
      DeclarationName Name(nameToken.getIdentifierInfo());
      Actions.ActOnNamedDeclarationRoger(Name, cb);
      break;
    }
    case RogerDeclaration::NAME_ARRAY_OPERATOR:
      isArray = true;
      // Fall-through.
    case RogerDeclaration::NAME_OPERATOR: {
      Token &nameToken = decl->Toks[decl->nameToken];
      OverloadedOperatorKind operatorKind;
      switch (nameToken.getKind()) {
      case tok::kw_new:
        operatorKind = isArray ? OO_Array_New : OO_New;
        break;
      case tok::kw_delete:
        operatorKind = isArray ? OO_Array_Delete : OO_Delete;
        break;
      case tok::l_paren:
        operatorKind = OO_Call;
        break;
      case tok::l_square:
        operatorKind = OO_Subscript;
        break;
#define OVERLOADED_OPERATOR_MULTI(Name,Spelling,Unary,Binary,MemberOnly)
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
      case tok::Token: \
        operatorKind = OO_##Name; \
        break;
#include "clang/Basic/OperatorKinds.def"
      default:
        assert(false && "Unknown operator");
      }
      DeclarationName Name = Actions.Context.DeclarationNames.getCXXOperatorName(operatorKind);
      Actions.ActOnNamedDeclarationRoger(Name, cb);
      break;
    }
    case RogerDeclaration::NAME_CONVERSION:
      Actions.ActOnConversionDeclarationRoger(cb);
      break;
    case RogerDeclaration::NAME_CONSTRUCTOR:
      Actions.ActOnConstructorDeclarationRoger(cb);
      break;
    case RogerDeclaration::NAME_DESTRUCTOR:
      Actions.ActOnDestructorDeclarationRoger(cb);
      break;
    default:
      assert(false);
    }
  }
  for (size_t i = 0; i < rogerDeclList->ClassDecl.size(); ++i) {
    RogerClassDecl* decl = rogerDeclList->ClassDecl[i];
    Token &nameToken = decl->Toks[decl->nameToken];

    struct CallbackImpl : public RogerItemizedLateParseCallback {
      Parser* parser;
      RogerClassDecl* decl;
      DeclContext *DC;
      RogerTopLevelDecls *topLevelDecs;
      RogerCallbackGuard guard;
      bool isInUse;
      CallbackImpl() : isInUse(false) {}
      bool isBusy() {
        return isInUse;
      }
      void parseDeferred() {
        RogerCallbackInUseScope inUse(&isInUse);
        guard.check();
        Parser::DeclGroupPtrTy result = parser->ParseRogerTemplatableClassDecl(decl, DC);
        if (topLevelDecs && result) {
          topLevelDecs->List.push_back(result);
        }
      }
    };

    CallbackImpl *cb = new CallbackImpl();
    cb->parser = this;
    cb->decl = decl;
    cb->DC = Actions.CurContext;
    cb->topLevelDecs = topLevelDecs;
    Actions.ActOnNamedDeclarationRoger(nameToken.getIdentifierInfo(), cb);
  }
  for (size_t i = 0; i < rogerDeclList->NonType.size(); ++i) {
    RogerNonType *nonType = rogerDeclList->NonType[i];
    Token &T = nonType->Toks[nonType->range.begin];
    Diag(T, diag::err_roger_invalid_non_type_region)
      << getTokenSimpleSpelling(T.getKind());
  }
  if (SmallVector<RogerNamespace*, 4> *nsDeclList = Types::getNsDeclList(rogerDeclList)) {
    for (size_t i = 0; i < nsDeclList->size(); ++i) {
      RogerNamespace *ns = (*nsDeclList)[i];
      Token &nameTok = ns->Toks[ns->nameToken];
      IdentifierInfo *II = nameTok.getIdentifierInfo();
      ParsedAttributesWithRange attrs(AttrFactory);
      struct Callback : RogerItemizedLateParseCallback {
        Parser *parser;
        RogerNamespace *ns;
        DeclContext *dc;
        RogerTopLevelDecls *topLevelDecs;
        RogerCallbackGuard guard;
        bool isInUse;
        Callback() : isInUse(false) {}
        bool isBusy() {
          return isInUse;
        }
        void parseDeferred() {
          RogerCallbackInUseScope inUse(&isInUse);
          parser->FillRogerNamespaceWithNames(&ns->inner, dc, new RogerParsingNamespace, 0);
        }
      };
      Callback *cb = new Callback();
      cb->parser = this;
      cb->ns = ns;
      cb->topLevelDecs = topLevelDecs;
      NamespaceDecl *nsDc = Actions.ActOnRogerNamespaceDef(getCurScope(), II, nameTok.getLocation(), cb, attrs.getList());
      cb->dc = nsDc;

      if (topLevelDecs) {
        topLevelDecs->List.push_back(Actions.ConvertDeclToDeclGroup(nsDc));
      }
    }
  }
}

Parser::DeclGroupPtrTy Parser::ParseRogerDeclarationRegion(RogerDeclaration *decl, DeclContext *DC, RogerParsingNamespace *parsingNs) {
  RogerNestedTokensState parseNestedTokens(this, &decl->Toks[decl->declaration.begin], decl->declaration.size());
  RogerNamespaceParseScope scope(this, DC, parsingNs);

  ParsedAttributesWithRange attrs(AttrFactory);
  return ParseExternalDeclaration(attrs);
}

Parser::DeclGroupPtrTy Parser::ParseRogerMemberRegion(RogerDeclaration *decl, Decl *RD, ParsingClass *parsingClass) {
  RogerNestedTokensState parseNestedTokens(this, &decl->Toks[decl->declaration.begin], decl->declaration.size());
  RogerRecordParseScope scope(this, RD, parsingClass);

  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  AccessSpecifier AS = calculateAS(decl->visibility, dyn_cast<DeclContext>(RD));

  ParsedAttributesWithRange attrs(AttrFactory);
  ParseCXXClassMemberDeclaration(AS, attrs.getList());
  return DeclGroupPtrTy();
}

class RogerRecordPreparser : public RogerItemizedLateParseCallback {
  Parser& parser;
  CXXRecordDecl *recDecl;
  RogerClassDecl *classDecl;
  int offset;
  RogerCallbackGuard guard;
  bool isInUse;
public:
  RogerRecordPreparser(Parser& p, CXXRecordDecl *rd, RogerClassDecl *cd, int offset) : parser(p), recDecl(rd), classDecl(cd), offset(offset), isInUse(false) {}
  bool isBusy() {
    return isInUse;
  }
  void parseDeferred() {
    RogerCallbackInUseScope inUse(&isInUse);
    guard.check();
    parser.PreparseRogerClassBody(recDecl, classDecl, offset);
  }
};

Parser::DeclGroupPtrTy Parser::ParseRogerTemplatableClassDecl(RogerClassDecl *classDecl, DeclContext *DC) {
  RogerNestedTokensState parseNestedTokens(this, &classDecl->Toks[classDecl->declaration.begin], classDecl->declaration.size());
  RogerParseScope scope(this, DC);

  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  AccessSpecifier AS = calculateAS(classDecl->visibility, DC);

  Decl *resultDecl;

  if (Tok.is(tok::kw_template)) {
    assert((Tok.is(tok::kw_export) || Tok.is(tok::kw_template)) &&
           "Token does not start a template declaration.");

    // Enter template-parameter scope.
    ParseScope TemplateParmScope(this, Scope::TemplateParamScope);

    // Tell the action that names should be checked in the context of
    // the declaration to come.
    ParsingDeclRAIIObject
      ParsingTemplateParams(*this, ParsingDeclRAIIObject::NoParent);

    // Parse multiple levels of template headers within this template
    // parameter scope, e.g.,
    //
    //   template<typename T>
    //     template<typename U>
    //       class A<T>::B { ... };
    //
    // We parse multiple levels non-recursively so that we can build a
    // single data structure containing all of the template parameter
    // lists to easily differentiate between the case above and:
    //
    //   template<typename T>
    //   class A {
    //     template<typename U> class B;
    //   };
    //
    // In the first case, the action for declaring A<T>::B receives
    // both template parameter lists. In the second case, the action for
    // defining A<T>::B receives just the inner template parameter list
    // (and retrieves the outer template parameter list from its
    // context).
    bool isSpecialization = true;
    bool LastParamListWasEmpty = false;
    TemplateParameterLists ParamLists;
    TemplateParameterDepthRAII CurTemplateDepthTracker(TemplateParameterDepth);

    do {
      // Consume the 'export', if any.
      SourceLocation ExportLoc;
      if (Tok.is(tok::kw_export)) {
        ExportLoc = ConsumeToken();
      }

      // Consume the 'template', which should be here.
      SourceLocation TemplateLoc;
      if (Tok.is(tok::kw_template)) {
        TemplateLoc = ConsumeToken();
      } else {
        Diag(Tok.getLocation(), diag::err_expected_template);
        return Parser::DeclGroupPtrTy();
      }

      // Parse the '<' template-parameter-list '>'
      SourceLocation LAngleLoc, RAngleLoc;
      SmallVector<Decl*, 4> TemplateParams;
      if (ParseTemplateParameters(CurTemplateDepthTracker.getDepth(),
                                  TemplateParams, LAngleLoc, RAngleLoc)) {
        // Skip until the semi-colon or a }.
        SkipUntil(tok::r_brace, true, true);
        if (Tok.is(tok::semi))
          ConsumeToken();
        return Parser::DeclGroupPtrTy();
      }

      ParamLists.push_back(
        Actions.ActOnTemplateParameterList(CurTemplateDepthTracker.getDepth(),
                                           ExportLoc,
                                           TemplateLoc, LAngleLoc,
                                           TemplateParams.data(),
                                           TemplateParams.size(), RAngleLoc));

      if (!TemplateParams.empty()) {
        isSpecialization = false;
        ++CurTemplateDepthTracker;
      } else {
        LastParamListWasEmpty = true;
      }
    } while (Tok.is(tok::kw_export) || Tok.is(tok::kw_template));

    ParsingDeclSpec DS(*this, &ParsingTemplateParams);

    resultDecl = ParseRogerClassForwardDecl(classDecl,
        ParsedTemplateInfo(&ParamLists, isSpecialization, LastParamListWasEmpty),
                    AS, parseNestedTokens);
  } else {
    ParsingDeclSpec DS(*this);
    resultDecl = ParseRogerClassForwardDecl(classDecl, ParsedTemplateInfo(), AS, parseNestedTokens);
  }

  // Keep position for the future parsing.
  parseNestedTokens.skipRest();

  return Actions.ConvertDeclToDeclGroup(resultDecl);
}

Decl *Parser::ParseRogerClassForwardDecl(RogerClassDecl *classDecl,
    const ParsedTemplateInfo &TemplateInfo,
    AccessSpecifier AS, RogerNestedTokensState &parseState) {
  // Assumption.
  tok::TokenKind TagTokKind = Tok.getKind();
  DeclSpec::TST TagType;
  if (TagTokKind == tok::kw_struct)
    TagType = DeclSpec::TST_struct;
  else if (TagTokKind == tok::kw___interface)
    TagType = DeclSpec::TST_interface;
  else if (TagTokKind == tok::kw_class)
    TagType = DeclSpec::TST_class;
  else {
    assert(TagTokKind == tok::kw_union && "Not a class specifier");
    TagType = DeclSpec::TST_union;
  }
  ConsumeToken();

  assert(Tok.is(tok::identifier));
  IdentifierInfo *Name = Tok.getIdentifierInfo();
  SourceLocation NameLoc = ConsumeToken();

  CXXScopeSpec emptyScopeSpec;
  ParsedAttributesWithRange attrs(AttrFactory);

  MultiTemplateParamsArg TParams;
  if (TemplateInfo.TemplateParams)
    TParams =
      MultiTemplateParamsArg(&(*TemplateInfo.TemplateParams)[0], TemplateInfo.TemplateParams->size());


  bool OwnedDecl;
  bool IsDependent = false;
  Decl *TagOrTempResult = Actions.ActOnTag(getCurScope(), TagType, Sema::TUK_Definition, SourceLocation(),
      emptyScopeSpec, Name, NameLoc, attrs.getList(), AS,
                                     /*ModulePrivateSpecLoc=*/ SourceLocation(),
                                     TParams, OwnedDecl, IsDependent,
                                     SourceLocation(), false,
                                     clang::TypeResult());
  assert(!IsDependent && "Don't understand");
  if (!TagOrTempResult) {
    return 0;
  }

  Decl *ResultDecl = TagOrTempResult;
  Actions.AdjustDeclIfTemplate(ResultDecl);
  CXXRecordDecl *recDecl = cast<CXXRecordDecl>(ResultDecl);

  recDecl->rogerState = new RecordDecl::RogerState;

  recDecl->pauseDefinitionRoger();

  RogerRecordPreparser *lateParser = new RogerRecordPreparser(*this, recDecl, classDecl, parseState.getPos());
  recDecl->rogerState->parseHeader = lateParser;

  return TagOrTempResult;
}


void Parser::PreparseRogerClassBody(CXXRecordDecl *recDecl, RogerClassDecl *classDecl, int tokenOffset) {
  RogerNestedTokensState parseNestedTokens(this, &classDecl->Toks[classDecl->declaration.begin] + tokenOffset, classDecl->declaration.size() - tokenOffset);
  RogerParseScope scope(this, recDecl->getDeclContext());


  // Quick hack.
  SourceLocation StartLoc = Tok.getLocation();
  SourceLocation AttrFixitLoc = StartLoc;
  ParsedAttributesWithRange attrs(AttrFactory);

  DeclSpec::TST TagType;
  switch (recDecl->getTagKind()) {
  case TTK_Struct:
    TagType = DeclSpec::TST_struct;
    break;
  case TTK_Interface:
    TagType = DeclSpec::TST_interface;
    break;
  case TTK_Union:
    TagType = DeclSpec::TST_union;
    break;
  case TTK_Enum:
    assert(false);
    break;
  case TTK_Class:
    TagType = DeclSpec::TST_class;
    break;
  }

  recDecl->startDefinition();

  ParsingClass *parsingClass;
  ParseCXXMemberSpecification(StartLoc, AttrFixitLoc, attrs, TagType,
                              recDecl, true, &parsingClass);

  parseNestedTokens.skipRest();

  struct FillNamesCallBack : RogerItemizedLateParseCallback {
    Parser *P;
    CXXRecordDecl *recDecl;
    RogerClassDecl *classDecl;
    ParsingClass *parsingClass;
    RogerCallbackGuard guard;
    bool isInUse;
    FillNamesCallBack() : isInUse(false) {}
    bool isBusy() {
      return isInUse;
    }
    void parseDeferred() {
      RogerCallbackInUseScope inUse(&isInUse);
      guard.check();
      P->FillRogerRecordWithNames(classDecl, recDecl, parsingClass);
    }
  };
  FillNamesCallBack *cb = new FillNamesCallBack;
  cb->P = this;
  cb->recDecl = recDecl;
  cb->classDecl = classDecl;
  cb->parsingClass = parsingClass;
  recDecl->RogerNameFillCallback = cb;
}

void Parser::RogerCompleteCXXMemberSpecificationParsing(RecordDecl *RD, ParsingClass *parsingClass) {
  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  {
    RogerRecordParseScope scope(this, RD, parsingClass);

    ParseLexedAttributes(getCurrentClass());
    ParseLexedMethodDeclarations(getCurrentClass());

    // We've finished with all pending member declarations.
    Actions.ActOnFinishCXXMemberDecls();

    ParseLexedMemberInitializers(getCurrentClass());
    ParseLexedMethodDefs(getCurrentClass());
  }
  DeallocateParsedClasses(parsingClass);
}

//void Parser::RogerCompleteNamespaceParsing(DeclContext *DC, RogerParsingNamespace *parsingNs) {
//  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
//  ParenBraceBracketBalancer BalancerRAIIObj(*this);
//
//  RogerParseScope scope(this, DC);
//
//  for (size_t i = 0; i < parsingNs->LateParsedDeclarations.size(); ++i) {
//    LateParsedDeclaration *lateDecl = parsingNs->LateParsedDeclarations[i];
//    lateDecl->ParseLexedMethodDeclarations();
//    lateDecl->ParseLexedMethodDefs();
//    lateDecl->ParseRogerLexedStaticInitializers();
//    delete lateDecl;
//  }
//
//  delete parsingNs;
//}

struct Parser::LateParsedStaticVarInitializer::Callback : RogerItemizedLateParseCallback {
  CachedTokens Toks;

  /// CachedTokens - The sequence of tokens that comprises the initializer,
  /// including any leading '='.
  Decl *Field;
  Parser *Self;

  RogerCallbackGuard guard;
  bool isInUse;
  Callback() : isInUse(false) {}
  bool isBusy() {
    return isInUse;
  }
  void parseDeferred() {
    RogerCallbackInUseScope inUse(&isInUse);
    guard.check();

    Self->ParseLexedRogerStaticVarInitializer(this);
  }
};

Parser::LateParsedStaticVarInitializer::LateParsedStaticVarInitializer(Parser *P, VarDecl *FD)
    : Self(P), Field(FD) {
  Callback *cb = new Callback;
  callback = cb;
  cb->Field = FD;
  cb->Self = P;
  Field->rogerParseInitializerCallback = cb;
}

void Parser::LateParsedStaticVarInitializer::ParseRogerLexedStaticInitializers() {
  if (!Field || !Field->rogerParseInitializerCallback) {
    return;
  }
  Field->rogerParseInitializerCallback->parseDeferred();
  delete Field->rogerParseInitializerCallback;
  Field->rogerParseInitializerCallback = 0;
}


CachedTokens &Parser::LateParsedStaticVarInitializer::getToks() {
  return callback->Toks;
}

void Parser::ParseLexedRogerStaticVarInitializer(LateParsedStaticVarInitializer::Callback *cb) {
  if (!cb->Field || cb->Field->isInvalidDecl())
    return;

  Decl *ThisDecl = cb->Field;

  DeclContext *parent = ThisDecl->getDeclContext();
  RogerParseScope parseScope(this, parent);

  RogerNestedTokensState parseNestedTokens(this, cb->Toks.begin(), cb->Toks.size());

  // Hack.
  bool TypeContainsAuto = false;

  if (Tok.is(tok::equal)) {
    ConsumeToken();
    ExprResult Init(ParseInitializer());
    if (Init.isInvalid()) {
      SkipUntil(tok::comma, true, true);
      Actions.ActOnInitializerError(ThisDecl);
    } else {
      Actions.AddInitializerToDecl(ThisDecl, Init.take(),
                                   /*DirectInit=*/false, TypeContainsAuto);
    }
  } else if (Tok.is(tok::l_paren)) {
    // Parse C++ direct initializer: '(' expression-list ')'
    BalancedDelimiterTracker T(*this, tok::l_paren);
    T.consumeOpen();

    ExprVector Exprs;
    CommaLocsTy CommaLocs;

    if (ParseExpressionList(Exprs, CommaLocs)) {
      Actions.ActOnInitializerError(ThisDecl);
      SkipUntil(tok::r_paren);
    } else {
      // Match the ')'.
      T.consumeClose();

      assert(!Exprs.empty() && Exprs.size()-1 == CommaLocs.size() &&
             "Unexpected number of commas!");

      ExprResult Initializer = Actions.ActOnParenListExpr(T.getOpenLocation(),
                                                          T.getCloseLocation(),
                                                          Exprs);
      Actions.AddInitializerToDecl(ThisDecl, Initializer.take(),
                                   /*DirectInit=*/true, TypeContainsAuto);
    }
  } else {
    assert(false);
  }
  assert(Tok.is(tok::eof));
  ConsumeAnyToken();
}
