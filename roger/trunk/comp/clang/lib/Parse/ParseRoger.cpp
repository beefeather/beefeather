#include <iostream>
#include "clang/Parse/Parser.h"
#include "ParsePragma.h"
#include "RAIIObjectsForParser.h"
#include "clang/Basic/RogerConstr.h"
#include "clang/AST/ASTConsumer.h"
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
};

struct RogerNamespace {
  RogerNamespace(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int nameToken;
  RogerNamespaceDeclList inner;
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
      parseRegion(list, list);
    }
    return list;
  }
  void parseRegion(RogerDeclList* list, RogerNamespaceDeclList* nslist) {
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
      parseRegion(&classDecl->inner, 0);
    }
    list->ClassDecl.push_back(classDecl);
  }
  void parseFunction(RogerDeclList* list) {
    RogerDeclaration *declaration = new RogerDeclaration(Toks);
    declaration->isType = false;
    declaration->visibility = takeByte();
    declaration->nameToken = takeInt16();
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
      parseRegion(&ns->inner, &ns->inner);
    }
    list->Namespace.push_back(ns);
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


void Parser::ParseRogerPartOpt() {
  if (!Tok.is(tok::kw_capybara)) {
    return;
  }

  InRogerMode = true;

  ConsumeToken();

  CachedTokens Toks;

  ConsumeAndStoreUntil(tok::eof, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);

  const char* tokensFileName = "tokens";
  const char* overviewFileName = "overview";

  std::string Error;
  OwningPtr<llvm::raw_fd_ostream> OS;
  OS.reset(new llvm::raw_fd_ostream(
      tokensFileName, Error,
      llvm::sys::fs::F_Binary));
  if (!Error.empty())
    return;

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
  RogerNamespaceDeclList *rogerDeclList;
  {
    OwningPtr<llvm::MemoryBuffer> File;

    bool res = llvm::MemoryBuffer::getFile(overviewFileName, File);
    assert(!res && "Cannot read file");

    RogerOverviewFileParser parser(*File.get(), Toks);

    rogerDeclList = parser.parse();
  }


  struct MainCallback : Sema::RogerOnDemandParserInt {
    Parser *parser;
    void ParseFunction(LateParsedTemplate &LPT) {
      parser->ParseLateTemplatedFuncDef(LPT);
    }
  };
  MainCallback semaCallback;
  semaCallback.parser = this;

  Actions.ActOnRogerModeStart(&semaCallback);

  FillRogerNamespaceWithNames(rogerDeclList, Actions.CurContext);

  Actions.ActOnNamespaceFinishRoger(getCurScope()->getEntity());

  Actions.ActOnRogerModeFinish();
}


static AccessSpecifier calculateAS(int explicitVisibility, int defaultVisibility) {
  int vis = explicitVisibility && defaultVisibility;
  switch (vis) {
  default: assert(false && "Unknown visibility");
  case 0: return AS_none;
  case 1: return AS_public;
  case 2: return AS_protected;
  case 3: return AS_private;
  }
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

void Parser::FillRogerNamespaceWithNames(RogerNamespaceDeclList *rogerDeclList, DeclContext *DC) {
  RogerParseScope scope(this, DC);

  int namespaceVisibility = 0;

  for (size_t i = 0; i < rogerDeclList->NameDeclaration.size(); ++i) {
    RogerDeclaration* decl = rogerDeclList->NameDeclaration[i];
    Token &nameToken = decl->Toks[decl->nameToken];

    struct CallbackImpl : public RogerItemizedLateParseCallback {
      Parser* parser;
      RogerDeclaration* decl;
      DeclContext *DC;
      void parseDeferred() {
        parser->ParseRogerDeclarationRegion(decl, 0, DC);
      }
    };

    CallbackImpl *cb = new CallbackImpl();
    cb->parser = this;
    cb->decl = decl;
    cb->DC = Actions.CurContext;
    Actions.ActOnNamedDeclarationRoger(nameToken.getIdentifierInfo(), cb);
    //ParseRogerClassDecl(rogerDeclList->NameDeclaration[i], namespaceVisibility);
  }
  for (size_t i = 0; i < rogerDeclList->ClassDecl.size(); ++i) {
    ParseRogerClassDecl(rogerDeclList->ClassDecl[i], namespaceVisibility);
  }
  for (size_t i = 0; i < rogerDeclList->NonType.size(); ++i) {
    //ParseRogerNonTypeRegion(rogerDeclList->NonType[i], namespaceVisibility);
    assert(false);
  }
  for (size_t i = 0; i < rogerDeclList->Namespace.size(); ++i) {
    RogerNamespace *ns = rogerDeclList->Namespace[i];
    Token &nameTok = ns->Toks[ns->nameToken];
    IdentifierInfo *II = nameTok.getIdentifierInfo();
    ParsedAttributesWithRange attrs(AttrFactory);
    struct Callback : RogerItemizedLateParseCallback {
      Parser *parser;
      RogerNamespace *ns;
      DeclContext *dc;
      void parseDeferred() {
        parser->FillRogerNamespaceWithNames(&ns->inner, dc);
      }
    };
    Callback *cb = new Callback();
    cb->parser = this;
    cb->ns = ns;
    DeclContext *nsDc = Actions.ActOnRogerNamespaceDef(getCurScope(), II, nameTok.getLocation(), cb, attrs.getList());
    cb->dc = nsDc;
  }
}

//void Parser::ParseRogerNonTypeRegion(RogerNonType *nonType, int defaultVisibility, DeclContext *DC) {
//  // Sample.
//  // (void)Parser::ParseLexedMethodDeclaration;
//
//  // Sema::ActOnStartDelayedMemberDeclarations
//  // ParseScope PrototypeScope();
//
//  Token sentinel;
//  sentinel.startToken();
//  sentinel.setKind(tok::eof);
//  PP.EnterTokenStream(&sentinel, 1, true, false);
//
//  PP.EnterTokenStream(&nonType->Toks[nonType->range.begin], nonType->range.size(), true, false);
//  ConsumeAnyToken();
//
//  RogerParseScope scope(this, DC);
//
//  while (Tok.isNot(tok::eof)) {
//    ParsedAttributesWithRange attrs(AttrFactory);
//    ParseExternalDeclaration(attrs);
//  }
//}

class clang::RogerNestedTokensState {
  Parser *P;
  bool isClosed;
  Token sentinel;
  Token saved;
  Preprocessor::RogerStreamState *ticket;
  Token PrevTok;
  unsigned short PrevParenCount, PrevBracketCount, PrevBraceCount;
public:
  RogerNestedTokensState(Parser *P, const Token *begin, int size) : P(P), isClosed(false) {
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

void Parser::ParseRogerDeclarationRegion(RogerDeclaration *decl, int defaultVisibility, DeclContext *DC) {
  RogerNestedTokensState parseNestedTokens(this, &decl->Toks[decl->declaration.begin], decl->declaration.size());
//  Token sentinel;
//  sentinel.startToken();
//  sentinel.setKind(tok::eof);
//  PP.EnterTokenStream(&sentinel, 1, true, false);
//
//  PP.EnterTokenStream(&decl->Toks[decl->declaration.begin], decl->declaration.size(), true, false);
//  ConsumeAnyToken();

  RogerParseScope scope(this, DC);

  ParsedAttributesWithRange attrs(AttrFactory);
  ParseExternalDeclaration(attrs);
}

class RogerRecordLateParser : public RogerItemizedLateParseCallback {
  Parser& parser;
  CXXRecordDecl *recDecl;
  RogerClassDecl *classDecl;
public:
  RogerRecordLateParser(Parser& p, CXXRecordDecl *rd, RogerClassDecl *cd) : parser(p), recDecl(rd), classDecl(cd) {}
  virtual void parseDeferred() {
    parser.ParseRogerClassBody(recDecl, classDecl);
  }
};

void Parser::ParseRogerClassDecl(RogerClassDecl *classDecl, int defaultVisibility) {
  Token &nameTok = classDecl->Toks[classDecl->nameToken];

  // Assumption.
  Token &tagToken = classDecl->Toks[classDecl->nameToken - 1];
  tok::TokenKind TagTokKind = tagToken.getKind();
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

  CXXScopeSpec emptyScopeSpec;
  ParsedAttributesWithRange attrs(AttrFactory);

  AccessSpecifier AS = calculateAS(classDecl->visibility, defaultVisibility);
  MultiTemplateParamsArg TParams;
  bool OwnedDecl;
  bool IsDependent = false;
  Decl *TagOrTempResult = Actions.ActOnTag(getCurScope(), TagType, Sema::TUK_Declaration, classDecl->Toks[classDecl->declaration.begin].getLocation(),
      emptyScopeSpec, nameTok.getIdentifierInfo(), nameTok.getLocation(), attrs.getList(), AS,
                                     /*ModulePrivateSpecLoc=*/ SourceLocation(),
                                     TParams, OwnedDecl, IsDependent,
                                     SourceLocation(), false,
                                     clang::TypeResult());
  assert(!IsDependent && "Don't understand");

  CXXRecordDecl *recDecl = cast<CXXRecordDecl>(TagOrTempResult);
  RogerRecordLateParser *lateParser = new RogerRecordLateParser(*this, recDecl, classDecl);
  recDecl->rogerCompleteTypeCallback = lateParser;
}

void Parser::ParseRogerClassBody(CXXRecordDecl *recDecl, RogerClassDecl *classDecl) {
  RogerNestedTokensState parseNestedTokens(this, &classDecl->Toks[classDecl->classTokens.begin], classDecl->classTokens.size());

//  Token sentinel;
//  sentinel.startToken();
//  sentinel.setKind(tok::eof);
//  PP.EnterTokenStream(&sentinel, 1, true, false);
//
//  PP.EnterTokenStream(&classDecl->Toks[classDecl->classTokens.begin], classDecl->classTokens.size(), true, false);
//  ConsumeAnyToken();

  DeclContext *DC = recDecl->getDeclContext();
  RogerParseScope scope(this, DC);

  SourceLocation StartLoc;
  //= classDecl->Toks[classDecl->classTokens.begin + 1].getLocation();
  // Don't understand.
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
    TagType = DeclSpec::TST_enum;
    break;
  case TTK_Class:
    TagType = DeclSpec::TST_class;
    break;
  }

  ParseCXXMemberSpecification(StartLoc, AttrFixitLoc, attrs, TagType,
                              recDecl, true);
}
