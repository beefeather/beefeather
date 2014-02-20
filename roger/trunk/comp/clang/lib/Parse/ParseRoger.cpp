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
struct RogerEnumDecl;
struct RogerNamespace;
struct RogerFile;

struct RogerDeclList {
  SmallVector<RogerNonType*, 4> NonType;
  SmallVector<RogerDeclaration*, 4> NameDeclaration;
  SmallVector<RogerClassDecl*, 4> ClassDecl;
  SmallVector<RogerEnumDecl*, 0> EnumDecl;
  SmallVector<RogerDeclaration*, 4> Using;
  SmallVector<RogerNonType*, 4> UsingDir;
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


struct RogerFile {
  CachedTokens &Toks;
  DeclContext *nsContext;
  RogerRange range;
  SourceLocation startLoc;
  RogerNamespaceDeclList inner;
  NamespaceDecl *FileScopeNs;
  std::string FileName;
  RogerFile(CachedTokens &Toks) : Toks(Toks), FileScopeNs(0) {}
};


struct RogerBlindParsable {
  virtual Token &rangeBegin() = 0;
  virtual int rangeSize() = 0;
  virtual int getVisibility() = 0;
};

struct RogerNonType : RogerBlindParsable{
  RogerNonType(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  RogerRange range;

  Token &rangeBegin() { return Toks[range.begin]; }
  int rangeSize() { return range.size(); }
  int getVisibility() { return visibility; }
};

struct RogerDeclaration : RogerBlindParsable {
  RogerDeclaration(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  enum NameKind {
    NAME_PLAIN, NAME_OPERATOR, NAME_ARRAY_OPERATOR, NAME_CONVERSION, NAME_CONSTRUCTOR, NAME_DESTRUCTOR, NAME_INSTANTIATION, _NAME_ERROR
  };
  NameKind nameKind;
  int nameToken;
  RogerRange range;
  bool isTemplateSecondary;
  Token &rangeBegin() { return Toks[range.begin]; }
  int rangeSize() { return range.size(); }
  int getVisibility() { return visibility; }
};

struct RogerClassDecl {
  RogerClassDecl(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  int nameToken;
  int tagToken;
  RogerRange declaration;
  RogerRange classTokens;
  bool isTemplateSecondary;
  RogerDeclList inner;
};

struct RogerNamespace {
  RogerNamespace(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int nameToken;
  RogerNamespaceDeclList inner;
};

struct RogerEnumDecl : RogerBlindParsable {
  RogerEnumDecl(CachedTokens &T) : Toks(T) {}
  CachedTokens &Toks;
  int visibility;
  int nameToken;
  RogerRange range;
  SmallVector<int, 4> inner;
  Token &rangeBegin() { return Toks[range.begin]; }
  int rangeSize() { return range.size(); }
  int getVisibility() { return visibility; }
};

struct RogerTopLevelDecls {
  SmallVector<Parser::DeclGroupPtrTy, 4> List;
};

class RogerOverviewFileParser {
private:
  const unsigned char *m_current;
  const unsigned char *m_end;
  Parser *P;
  CachedTokens &Toks;
public:
  RogerOverviewFileParser(llvm::MemoryBuffer& buffer, CachedTokens &T, Parser *P) : P(P), Toks(T) {
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
      assert(false && "Unknown file content code");
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
      assert(nslist && "Parse region must be invoked within namespace context");
      parseNamespace(nslist);
      break;
    case 6:
      parseUsing(list);
      break;
    case 7:
      parseErrorRegion();
      break;
    case 8:
      parseEnumRegion(list);
      break;
    default:
      assert(false && "Unknown region code");
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
    declaration->nameToken = takeInt16();
    if (declaration->nameToken == 0) {
      declaration->nameKind = RogerDeclaration::NAME_INSTANTIATION;
    } else {
      declaration->nameKind = RogerDeclaration::NAME_PLAIN;
    }
    parseRange(declaration->range);
    declaration->isTemplateSecondary = takeByte();
    list->NameDeclaration.push_back(declaration);
  }
  void parseClassDecl(RogerDeclList* list) {
    RogerClassDecl* classDecl = new RogerClassDecl(Toks);
    classDecl->visibility = takeByte();
    classDecl->nameToken = takeInt16();
    parseRange(classDecl->declaration);
    parseRange(classDecl->classTokens);
    classDecl->isTemplateSecondary = takeByte();
    while (true) {
      assert(m_current < m_end && "Unexpected end of file while reading list of regions in class");
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
    declaration->visibility = takeByte();
    parseName(declaration->nameKind, declaration->nameToken);
    parseRange(declaration->range);
    declaration->isTemplateSecondary = takeByte();
    list->NameDeclaration.push_back(declaration);
  }
  void parseNamespace(RogerNamespaceDeclList* list) {
    RogerNamespace* ns = new RogerNamespace(Toks);
    ns->nameToken = takeInt16();
    while (true) {
      assert(m_current < m_end && "Unexpected end of file while reading list of regions in namespace");
      if (*m_current == 0) {
        ++m_current;
        break;
      }
      parseRegion(&ns->inner, &ns->inner, 0);
    }
    list->Namespace.push_back(ns);
  }

  void parseUsing(RogerDeclList* list) {
    int visibility = takeByte();
    int hasName = takeByte();
    if (hasName) {
      RogerDeclaration *rogerDecl = new RogerDeclaration(Toks);
      rogerDecl->visibility = visibility;
      parseName(rogerDecl->nameKind, rogerDecl->nameToken);
      parseRange(rogerDecl->range);
      list->Using.push_back(rogerDecl);
    } else {
      RogerNonType *rogerDecl = new RogerNonType(Toks);
      rogerDecl->visibility = visibility;
      parseRange(rogerDecl->range);
      list->UsingDir.push_back(rogerDecl);
    }
  }

  void parseErrorRegion() {
    RogerRange range;
    parseRange(range);
    StringRef message = takeString();
    P->Diag(Toks[range.begin], diag::err_roger_invalid_region) << message;
  }

  void parseEnumRegion(RogerDeclList* list) {
    RogerEnumDecl *declaration = new RogerEnumDecl(Toks);
    declaration->visibility = takeByte();
    declaration->nameToken = takeInt16();
    while (true) {
      int i = takeInt16();
      if (i == 0) {
        break;
      }
      declaration->inner.push_back(i);
    }
    parseRange(declaration->range);
    list->EnumDecl.push_back(declaration);
  }

  void parseName(RogerDeclaration::NameKind &nameKind, int &nameToken) {
    int nameKindCode = takeByte();
    switch (nameKindCode) {
    case 0:
      nameKind = RogerDeclaration::NAME_PLAIN;
      nameToken = takeInt16();
      break;
    case 1:
      nameKind = RogerDeclaration::NAME_OPERATOR;
      nameToken = takeInt16();
      break;
    case 2:
      nameKind = RogerDeclaration::NAME_ARRAY_OPERATOR;
      nameToken = takeInt16();
      break;
    case 3:
      nameKind = RogerDeclaration::NAME_CONVERSION;
      nameToken = 0;
      break;
    case 4:
      nameKind = RogerDeclaration::NAME_CONSTRUCTOR;
      nameToken = 0;
      break;
    case 5:
      nameKind = RogerDeclaration::NAME_DESTRUCTOR;
      nameToken = 0;
      break;
    default:
      assert(false && "Unknown declaration name code");
    }
  }

  void parseRange(RogerRange &range) {
    range.begin = takeInt16();
    range.end = takeInt16();
  }

  void parseError() {
    std::cout << "Error";
    StringRef message = takeString();
    std::cout << message.str() << '\n';
  }

  char takeByte() {
    assert(m_current < m_end && "End of file");
    return *(m_current++);
  }
  int takeInt16() {
    assert(m_current + 1 < m_end && "End of file");
    int result = m_current[0] + m_current[1] * 256;
    m_current += 2;
    return result;
  }
  StringRef takeString() {
    int len = takeInt16();
    assert(m_current + len <= m_end && "End of file while reading a string");
    StringRef result(reinterpret_cast<const char *>(m_current), len);
    m_current += len;
    return result;
  }
};

}


class RogerCallbackGuard {
  bool first;
public:
  RogerCallbackGuard() : first(true) {}
  void check() {
    assert(first && "Callback guard violation");
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

  assert(rogerFrontendUtils && "Roger frontend utils are not available");

  {
    const char* tokensFileName = "tokens";

    OwningPtr<llvm::raw_fd_ostream> OsDebug;
    std::string Error;
    OsDebug.reset(new llvm::raw_fd_ostream(tokensFileName, Error, llvm::sys::fs::F_Binary));
    assert(Error.empty() && "Failed to create debug tokens file");

    rogerFrontendUtils->CacheTokensRoger(Toks, OsDebug.get());
    OsDebug->close();
  }


  SmallString<128> TokensTempPath;
  {
    OwningPtr<llvm::raw_fd_ostream> OS;

    TokensTempPath += "tokens-%%%%%%%%";
    bool problem = llvm::sys::fs::createUniqueFile(TokensTempPath.str(), TokensTempPath);
    assert(!problem && "Failed to create tokens unique file");

    std::string Error;
    OS.reset(new llvm::raw_fd_ostream(TokensTempPath.c_str(), Error, llvm::sys::fs::F_Binary));
    assert(Error.empty() && "Failed to create main tokens file");

    rogerFrontendUtils->CacheTokensRoger(Toks, OS.get());
    OS->close();
  }


  SmallString<128> OverviewTempPath;
  {
    OverviewTempPath += "overview-%%%%%%%%";
    bool problem = llvm::sys::fs::createUniqueFile(OverviewTempPath.str(), OverviewTempPath);
    assert(!problem  && "Failed to create overview unique file");
  }

  // Call overview parser.
  {
    StringRef Executable;
    if (getLangOpts().RogerPathToOverviewParser.empty()) {
      Executable = "./roger_parse_overview";
    } else {
      assert(getLangOpts().RogerPathToOverviewParser[0] == '=' && "Path to overview parser must be specified with '='");
      Executable = getLangOpts().RogerPathToOverviewParser;
      Executable = Executable.substr(1);
    }
    SmallVector<const char*, 5> Argv;
    Argv.push_back(Executable.data());
    Argv.push_back(TokensTempPath.c_str());
    Argv.push_back(OverviewTempPath.c_str());
    if (getLangOpts().RogerVerbose) {
      Argv.push_back("-v");
    }
    Argv.push_back(0);

    int res = llvm::sys::ExecuteAndWait(Executable, Argv.data());
    assert(res == 0 && "Failed to execute overview parser, result code is not null");
  }

  // Parse overview.
  OwningPtr<llvm::MemoryBuffer> FileBytes;
  bool res = llvm::MemoryBuffer::getFile(OverviewTempPath.c_str(), FileBytes);
  assert(!res && "Cannot read overview file");

  llvm::sys::fs::remove(OverviewTempPath.str());
  llvm::sys::fs::remove(TokensTempPath.str());

  RogerOverviewFileParser parser(*FileBytes.get(), Toks, this);
  return parser.parse();
}

struct Parser::RogerParsingQueue::Item {
  DeclContext *DC;
  LateParsedDeclaration *lateDecl;
  RogerFile *file;
};

Parser::RogerParsingQueue::Item *Parser::RogerParsingQueue::pop() {
  if (List.empty()) {
    return 0;
  }
  Item *result = List.back();
  List.pop_back();
  return result;
}

void Parser::RogerParsingQueue::addAndWrap(LateParsedDeclaration *lateDecl, DeclContext *dc, RogerFile *file) {
  Item *item = new Item;
  item->DC = dc;
  item->lateDecl = lateDecl;
  assert(file);
  item->file = file;
  List.push_back(item);
}

class clang::RogerParseScope {
  Parser *P;
  Sema &Actions;
  RogerConstr<Parser::TemplateParameterDepthRAII> CurTemplateDepthTracker;
  Scope *SavedScope;
  RogerFile *SavedFile;
  NamespaceDecl *SavedFileScopeNs;
  DeclContext *SavedContext;
  RogerConstr<Sema::ContextRAII> GlobalSavedContext;
  SmallVector<Parser::ParseScope*, 4> TemplateParamScopeStack;

  bool closed;
public:
  RogerParseScope(Parser *PP, RogerFile *file, DeclContext *context, CXXRecordDecl *AdditionalTemplateHolder = 0)
      : P(PP), Actions(PP->Actions), closed(false)
  {
    // Track template parameter depth.
    new (CurTemplateDepthTracker.getBuffer()) Parser::TemplateParameterDepthRAII(P->TemplateParameterDepth);

    SavedContext = P->Actions.CurContext;

    SmallVector<DeclContext*, 4> DeclContextsToReenter;
    DeclContext *DD = context;
    while (!DD->isTranslationUnit()) {
      DeclContextsToReenter.push_back(DD);
      DD = DD->getLexicalParent();
    }

    SavedScope = P->getCurScope();
    SavedFile = P->rogerParsingFile;
    P->rogerParsingFile = file;
    P->EnterScope(Scope::DeclScope);
    P->getCurScope()->Init(0, Scope::DeclScope);
    P->getCurScope()->setEntity(DD);

    P->Actions.CurContext = DD;

    SavedFileScopeNs = Actions.CurRogerImportNamespace;
    Actions.CurRogerImportNamespace = P->GetOrParseRogerFileScope(file);

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
    if (AdditionalTemplateHolder) {
      bool IsClassTemplate = AdditionalTemplateHolder->getDescribedClassTemplate() != 0;
      TemplateParamScopeStack.push_back(
          new Parser::ParseScope(P, Scope::TemplateParamScope,
                        /*ManageScope*/IsClassTemplate));
      Actions.ActOnReenterTemplateScope(P->getCurScope(),
          AdditionalTemplateHolder->getDescribedClassTemplate());
      if (IsClassTemplate)
        ++CurTemplateDepthTracker.get();
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
    Actions.CurRogerImportNamespace = SavedFileScopeNs;
    P->rogerParsingFile = SavedFile;

    CurTemplateDepthTracker.del();
    P->Actions.CurContext = SavedContext;

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
  RogerRecordParseScope(Parser *PP, RogerFile *file, Decl *TagOrTemplate, Parser::ParsingClass *parsingClass)
      : P(PP), Actions(PP->Actions), TagOrTemplate(TagOrTemplate), closed(false) {
    new (rogerParseScope.getBuffer()) RogerParseScope(PP, file, dyn_cast<DeclContext>(TagOrTemplate));

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
  RogerNamespaceParseScope(Parser *PP, RogerFile *file, DeclContext *DC, Parser::RogerParsingNamespace *parsingNs)
      : //P(PP),
        closed(false) {
    new (rogerParseScope.getBuffer()) RogerParseScope(PP, file, DC);
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

typedef std::vector<RogerFile*> RogerFileVector;

template<typename T, int S>
static void TossRogerDeclsToFiles(RogerNamespaceDeclList *input, RogerFileVector &output,
    SmallVector<T*, S> RogerNamespaceDeclList::* list_field, RogerRange T::* rangeField, CachedTokens &Toks, Parser *P) {
  SmallVector<T*, S> &inputPart = input->*list_field;
  RogerFileVector::iterator outputIt = output.begin();
  for (size_t i = 0; i < inputPart.size(); ++i) {
    T* item = inputPart[i];
    while ((item->*rangeField).begin >= (*outputIt)->range.end) {
      ++outputIt;
      assert(outputIt != output.end());
    }
    if ((item->*rangeField).begin < (*outputIt)->range.begin || (item->*rangeField).end > (*outputIt)->range.end) {
      P->Diag(Toks[(item->*rangeField).begin], diag::err_roger_declaration_accross_files);
    }
    ((*outputIt)->inner.*list_field).push_back(item);
  }
}


void Parser::ParseRogerPartOpt(ASTConsumer *Consumer) {
  if (!Tok.is(tok::kw_capybara)) {
    return;
  }

  SourceLocation capybaraLoc = Tok.getLocation();

  PP.RogerDisableDirectives = true;

  //ConsumeToken();

  // Do we need it?
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

  Actions.ActOnRogerModeStart(&semaCallback);

  CachedTokens Toks;

  RogerFileVector files;

  ConsumeToken();

  // Rest of source.
  while (true) {
    RogerFile* file = new RogerFile(Toks);

    if (Tok.isNot(tok::eof)) {
      file->startLoc = Tok.getLocation();
    }

    const char *nofilePath = 0;
    DeclContext *nsContext = ParseRogerNamespaceHeader(nofilePath);

    file->nsContext = nsContext;
    file->range.begin = Toks.size();
    ConsumeAndStoreUntil(tok::eof, tok::kw_capybara, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
    file->range.end = Toks.size();
    files.push_back(file);

    if (Tok.is(tok::kw_capybara)) {
      ConsumeToken();
    } else {
      break;
    }
  }

  // Files in directory.
  {
    FileID mainFileId = PP.getSourceManager().getMainFileID();
    const FileEntry *mainFileEn = PP.getSourceManager().getFileEntryForID(mainFileId);
    assert(mainFileEn);
    llvm::error_code EC;
    typedef llvm::sys::fs::recursive_directory_iterator
      recursive_directory_iterator;
    for (recursive_directory_iterator Entry(mainFileEn->getDir()->getName(), EC), End;
         Entry != End && !EC; Entry.increment(EC)) {

      if (Entry->path() == mainFileEn->getName()) {
        continue;
      }

      using llvm::StringSwitch;
      // Check whether this entry has an extension typically associated with
      // headers.
      if (!StringSwitch<bool>(llvm::sys::path::extension(Entry->path()))
             .Cases(".cc", ".cpp", true)
             .Default(false))
        continue;

      std::string relativePath = Entry->path().substr(strlen(mainFileEn->getDir()->getName()));

      llvm::outs() << "File: " << Entry->path() << '\n';

      const FileEntry *SourceFile = PP.getFileManager().getFile(Entry->path());
      FileID FileID = PP.getSourceManager().createFileID(SourceFile, capybaraLoc, SrcMgr::C_User);
      PP.EnterSourceFile(FileID, 0, capybaraLoc);

      RogerFile* file = new RogerFile(Toks);

      ConsumeToken();

      const char *filePathPos = relativePath.data();
      SourceLocation FileStartLocation = Tok.getLocation();
      if (Tok.isNot(tok::eof)) {
        file->startLoc = Tok.getLocation();
      }
      DeclContext *nsContext = ParseRogerNamespaceHeader(filePathPos);

      if (filePathPos == 0 || !llvm::sys::path::is_separator(*filePathPos)) {
        Diag(FileStartLocation, diag::err_roger_namespace_doesnt_match_path);
      } else {
        ++filePathPos;
        file->FileName = std::string(filePathPos);
        if (file->FileName != llvm::sys::path::filename(relativePath)) {
          Diag(FileStartLocation, diag::err_roger_namespace_doesnt_match_path);
        }
      }

      file->nsContext = nsContext;
      file->range.begin = Toks.size();
      ConsumeAndStoreUntil(tok::eof, tok::kw_capybara, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
      file->range.end = Toks.size();
      files.push_back(file);
    }
  }

  RogerNamespaceDeclList *definitionPart = ParseRogerPartOverview(Toks);

  RogerTopLevelDecls topLevelDecls;
  RogerTopLevelDecls secondaryTopLevelDecls;

  RogerParsingQueue parsingQueue;
  rogerParsingQueue = &parsingQueue;

  RogerParsingNamespace *topParsingNs = new RogerParsingNamespace;

  TossRogerDeclsToFiles<RogerDeclaration, 4>(definitionPart, files, &RogerNamespaceDeclList::NameDeclaration, &RogerDeclaration::range, Toks, this);
  TossRogerDeclsToFiles<RogerClassDecl, 4>(definitionPart, files, &RogerNamespaceDeclList::ClassDecl, &RogerClassDecl::declaration, Toks, this);
  TossRogerDeclsToFiles<RogerEnumDecl, 0>(definitionPart, files, &RogerNamespaceDeclList::EnumDecl, &RogerEnumDecl::range, Toks, this);
  TossRogerDeclsToFiles<RogerNonType, 4>(definitionPart, files, &RogerNamespaceDeclList::NonType, &RogerNonType::range, Toks, this);
  TossRogerDeclsToFiles<RogerDeclaration, 4>(definitionPart, files, &RogerNamespaceDeclList::Using, &RogerDeclaration::range, Toks, this);
  TossRogerDeclsToFiles<RogerNonType, 4>(definitionPart, files, &RogerNamespaceDeclList::UsingDir, &RogerNonType::range, Toks, this);
  assert(definitionPart->Namespace.empty());
  //TossRogerDeclsToFiles<RogerNamespace>(definitionPart, files, &RogerNamespaceDeclList::Namespace, &RogerNamespace::range);

  for (size_t i = 0; i < files.size(); ++i) {
    RogerFile *file = files[i];
    FillRogerSourceFileWithNames(&file->inner, file->nsContext, topParsingNs, file, &topLevelDecls);
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

  for (size_t i = 0; i < files.size(); ++i) {
    RogerFile *file = files[i];
    NamespaceDecl *FileScopeNs = GetOrParseRogerFileScope(file);
    Actions.ActOnNamespaceFinishRoger(FileScopeNs, 0);
  }

  while (RogerParsingQueue::Item *item = rogerParsingQueue->pop()) {
    DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
    ParenBraceBracketBalancer BalancerRAIIObj(*this);

    RogerParseScope parseScope(this, item->file, item->DC);
    item->lateDecl->ParseLexedMethodDeclarations();
    item->lateDecl->ParseLexedMethodDefs();
    item->lateDecl->ParseRogerLexedStaticInitializers();
    delete item->lateDecl;

    delete item;
  }

  for (size_t i = 0; i < topLevelDecls.List.size(); ++i) {
    Consumer->HandleTopLevelDecl(topLevelDecls.List[i].get());
  }

  {
    // Late template parsing can begin.
    if (getLangOpts().DelayedTemplateParsing)
      Actions.SetLateTemplateParser(LateTemplateParserCallback, this);
    if (!PP.isIncrementalProcessingEnabled())
      Actions.ActOnEndOfTranslationUnit();
    //else don't tell Sema that we ended parsing: more input might come.
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
    while (!isAtOurEof()) {
      assert(P->Tok.isNot(tok::eof));
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
    if (!isAtOurEof()) {
      P->Diag(P->Tok, diag::err_roger_invalid_token_before_region_end)
        << getTokenSimpleSpelling(P->Tok.getKind());
      while (!isAtOurEof()) {
        assert(P->Tok.isNot(tok::eof));
        P->ConsumeAnyToken();
      }
    }

    P->PP.ExitRogerTokenStream(ticket);
    P->Tok = saved;

    P->ParenCount = PrevParenCount;
    P->BracketCount = PrevBracketCount;
    P->BraceCount = PrevBraceCount;
  }

private:
  bool isAtOurEof() {
    return P->Tok.is(tok::eof) && P->Tok.getLocation() == begin->getLocation();
  }
};


class RogerGuardableCallback : public RogerItemizedLateParseCallback {
  RogerCallbackGuard guard;
  bool isInUse;
public:
  bool isBusy() {
    return isInUse;
  }
  void parseDeferred() {
    RogerCallbackInUseScope inUse(&isInUse);
    guard.check();
    parseDeferredImpl();
  }
protected:
  RogerGuardableCallback() : isInUse(false) {}
  virtual void parseDeferredImpl() = 0;
};


static std::string getExpectedClassName(std::string fileName) {
  const StringRef shortName = llvm::sys::path::stem(fileName);
  if (shortName.find('-') != StringRef::npos) {
    return std::string();
  }
  return std::string(shortName);
}

void Parser::FillRogerSourceFileWithNames(RogerNamespaceDeclList *rogerNsDeclList, DeclContext *DC, RogerParsingNamespace *parsingNs, RogerFile *file, RogerTopLevelDecls *topLevelDecs) {

  std::string expectedClassName = getExpectedClassName(file->FileName);

  if (!expectedClassName.empty()) {
    SourceLocation problemLoc;
    if (!rogerNsDeclList->NonType.empty()) {
      RogerNonType* nonType = rogerNsDeclList->NonType[0];
      problemLoc = nonType->Toks[nonType->range.begin].getLocation();
    } else if (!rogerNsDeclList->NameDeclaration.empty()) {
      RogerDeclaration* decl = rogerNsDeclList->NameDeclaration[0];
      problemLoc = decl->Toks[decl->range.begin].getLocation();
    } else if (!rogerNsDeclList->EnumDecl.empty()) {
      RogerEnumDecl* decl = rogerNsDeclList->EnumDecl[0];
      problemLoc = decl->Toks[decl->range.begin].getLocation();
    } else if (rogerNsDeclList->ClassDecl.size() == 0) {
      problemLoc = file->startLoc;
    } else if (rogerNsDeclList->ClassDecl.size() >= 1) {
      RogerClassDecl *singleClass = rogerNsDeclList->ClassDecl[0];
      IdentifierInfo *singleClassII = singleClass->Toks[singleClass->nameToken].getIdentifierInfo();
      if (expectedClassName != singleClassII->getNameStart()) {
        problemLoc = singleClass->Toks[singleClass->nameToken].getLocation();
      } else if (rogerNsDeclList->ClassDecl.size() > 1) {
        RogerClassDecl* decl = rogerNsDeclList->ClassDecl[1];
        problemLoc = decl->Toks[decl->nameToken].getLocation();
      }
    }

    if (problemLoc.isValid()) {
      Diag(problemLoc, diag::err_roger_unexpected_in_class_file) << expectedClassName;
    }
  }

  Sema::RogerLogScope logScope("FillRogerSourceFileWithNames", !getLangOpts().RogerVerbose);
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
    typedef RogerLiteral<Parser::DeclGroupPtrTy (Parser::*)(RogerBlindParsable *, DeclContext *, RogerFile *, RogerParsingNamespace *), &Parser::ParseRogerDeclarationRegion> ParseNamedDeclaration;
  };
  FillRogerDeclContextWithNames<TypesForRec>(rogerNsDeclList, DC, parsingNs, topLevelDecs, file);

  struct CompleteParsingCb : RogerGuardableCallback {
    Parser *P;
    RogerParsingNamespace *parsingNs;
    DeclContext *DC;
    void parseDeferredImpl() {
      //P->RogerCompleteNamespaceParsing(DC, parsingNs);
    }
  };
  CompleteParsingCb *cb = new CompleteParsingCb;
  cb->P = this;
  cb->parsingNs = parsingNs;
  cb->DC = DC;

  DC->RogerCompleteParsingCallback = cb;
}

void Parser::FillRogerRecordWithNames(RogerClassDecl *rogerClassDecl, RecordDecl *RD, RogerFile *file, ParsingClass *parsingClass) {
  Sema::RogerLogScope logScope("FillRogerRecordWithNames", !getLangOpts().RogerVerbose);
  RD->printName(logScope.outs_nl());
  logScope.outs() << '\n';


  struct TypesForNs {
    typedef RogerDeclList DeclList;
    typedef RecordDecl DeclContext;
    static SmallVector<RogerNamespace*, 4> *getNsDeclList(RogerDeclList *declList) {
      return 0;
    }
    typedef ParsingClass ParsingState;
    typedef RogerLiteral<DeclGroupPtrTy (Parser::*)(RogerBlindParsable*, Decl*, RogerFile *, ParsingClass *), &Parser::ParseRogerMemberRegion> ParseNamedDeclaration;
  };

  {
    FillRogerDeclContextWithNames<TypesForNs>(&rogerClassDecl->inner, RD, parsingClass, 0, file);
  }

  if (rogerClassDecl->inner.Using.size() || rogerClassDecl->inner.UsingDir.size()) {
    RogerRecordParseScope scope(this, file, RD, parsingClass);
    for (size_t i = 0; i < rogerClassDecl->inner.Using.size(); ++i) {
      RogerDeclaration *rogerUsing = rogerClassDecl->inner.Using[i];

      RogerNestedTokensState parseNestedTokens(this, &rogerUsing->Toks[rogerUsing->range.begin], rogerUsing->range.size());

      DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
      ParenBraceBracketBalancer BalancerRAIIObj(*this);

      AccessSpecifier AS = calculateAS(rogerUsing->visibility, RD);

      ParsedAttributesWithRange attrs(AttrFactory);
      ParseCXXClassMemberDeclaration(AS, attrs.getList());
    }
    for (size_t i = 0; i < rogerClassDecl->inner.UsingDir.size(); ++i) {
      RogerNonType *rogerUsing = rogerClassDecl->inner.UsingDir[i];

      RogerNestedTokensState parseNestedTokens(this, &rogerUsing->Toks[rogerUsing->range.begin], rogerUsing->range.size());

      DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
      ParenBraceBracketBalancer BalancerRAIIObj(*this);

      AccessSpecifier AS = calculateAS(rogerUsing->visibility, RD);

      ParsedAttributesWithRange attrs(AttrFactory);
      ParseCXXClassMemberDeclaration(AS, attrs.getList());
    }
  }

  struct CompleteParsingCb : RogerGuardableCallback {
    Parser *P;
    RecordDecl *RD;
    ParsingClass *parsingClass;
    RogerFile *file;
    void parseDeferredImpl() {
      P->RogerCompleteCXXMemberSpecificationParsing(RD, file, parsingClass);
    }
  };
  CompleteParsingCb *cb = new CompleteParsingCb;
  cb->P = this;
  cb->RD = RD;
  cb->file = file;
  cb->parsingClass = parsingClass;

  RD->RogerCompleteParsingCallback = cb;

  // Handle using.
}

template<typename CB_TYPES>
class RogerParseCallback : public RogerGuardableCallback {
  Parser* parser;
  typename CB_TYPES::ROGER_DECL* decl;
  typename CB_TYPES::DECL_CONTEXT *DC;
  RogerTopLevelDecls *topLevelDecs;
  RogerFile *file;
  typename CB_TYPES::ParsingState *parsingObj;
public:
  RogerParseCallback(Parser *parser, typename CB_TYPES::ROGER_DECL *decl,
      typename CB_TYPES::DECL_CONTEXT *DC, RogerTopLevelDecls *topLevelDecs, RogerFile *file, typename CB_TYPES::ParsingState *parsingObj)
      : parser(parser), decl(decl), DC(DC), topLevelDecs(topLevelDecs), file(file), parsingObj(parsingObj) {}
  void parseDeferredImpl() {
    Parser::DeclGroupPtrTy result = (parser->*(CB_TYPES::ParseMethod::v))(decl, DC, file, parsingObj);
    if (topLevelDecs && result) {
      topLevelDecs->List.push_back(result);
    }
  }
};


template<typename Types>
void Parser::FillRogerDeclContextWithNames(typename Types::DeclList *rogerDeclList, typename Types::DeclContext *DC, typename Types::ParsingState *parsingObj, RogerTopLevelDecls *topLevelDecs, RogerFile *file) {

  FillRogerDeclContextWithNamedDecls<Types>(rogerDeclList->NameDeclaration, DC, parsingObj, topLevelDecs, file);

  for (size_t i = 0; i < rogerDeclList->ClassDecl.size(); ++i) {
    RogerClassDecl* decl = rogerDeclList->ClassDecl[i];
    Token &nameToken = decl->Toks[decl->nameToken];

    struct CbTypesForClass {
      typedef DeclContext DECL_CONTEXT;
      typedef RogerClassDecl ROGER_DECL;
      typedef void ParsingState;
      typedef RogerLiteral<Parser::DeclGroupPtrTy (Parser::*)(RogerClassDecl *, DeclContext *, RogerFile *, void *), &Parser::ParseRogerTemplatableClassDecl> ParseMethod;
    };

    RogerItemizedLateParseCallback *cb = new RogerParseCallback<CbTypesForClass>(this, decl, DC, topLevelDecs, file, 0);
    Actions.ActOnNamedDeclarationRoger(DC, nameToken.getIdentifierInfo(), decl->isTemplateSecondary, cb);
  }
  for (size_t i = 0; i < rogerDeclList->NonType.size(); ++i) {
    RogerNonType *nonType = rogerDeclList->NonType[i];
//    Token &T = nonType->Toks[nonType->range.begin];
//    Diag(T, diag::err_roger_invalid_non_type_region)
//      << getTokenSimpleSpelling(T.getKind());

    struct CbTypesForNonType {
      typedef RogerNonType ROGER_DECL;
      typedef typename Types::DeclContext DECL_CONTEXT;
      typedef typename Types::ParsingState ParsingState;
      typedef typename Types::ParseNamedDeclaration ParseMethod;
    };

    RogerItemizedLateParseCallback *cb = new RogerParseCallback<CbTypesForNonType>(this, nonType, DC, topLevelDecs, file, parsingObj);

    Actions.ActOnNamedDeclarationRoger(DC, DeclarationName(), false, cb);
  }

  for (size_t i = 0; i < rogerDeclList->EnumDecl.size(); ++i) {
    RogerEnumDecl *enumDecl = rogerDeclList->EnumDecl[i];

    struct CbTypesForEnum {
      typedef RogerEnumDecl ROGER_DECL;
      typedef typename Types::DeclContext DECL_CONTEXT;
      typedef typename Types::ParsingState ParsingState;
      typedef typename Types::ParseNamedDeclaration ParseMethod;
    };

    RogerItemizedLateParseCallback *cb = new RogerParseCallback<CbTypesForEnum>(this, enumDecl, DC, topLevelDecs, file, parsingObj);

    SmallVector<DeclarationName, 10> names;
    names.push_back(DeclarationName(enumDecl->Toks[enumDecl->nameToken].getIdentifierInfo()));
    for (size_t i = 0; i < enumDecl->inner.size(); ++i) {
      names.push_back(DeclarationName(enumDecl->Toks[enumDecl->inner[i]].getIdentifierInfo()));
    }
    Actions.ActOnMultiNameDeclarationRoger(DC, ArrayRef<DeclarationName>(names), cb);
  }

  if (SmallVector<RogerNamespace*, 4> *nsDeclList = Types::getNsDeclList(rogerDeclList)) {
    for (size_t i = 0; i < nsDeclList->size(); ++i) {

      // Old code.
      assert(false);
    }
  }
}

template<typename Types>
void Parser::FillRogerDeclContextWithNamedDecls(SmallVector<RogerDeclaration*, 4> &NameDeclarationList, typename Types::DeclContext *DC, typename Types::ParsingState *parsingObj, RogerTopLevelDecls *topLevelDecs, RogerFile *file) {
  for (size_t i = 0; i < NameDeclarationList.size(); ++i) {
    RogerDeclaration* decl = NameDeclarationList[i];

    struct CbTypesForNamed {
      typedef RogerDeclaration ROGER_DECL;
      typedef typename Types::DeclContext DECL_CONTEXT;
      typedef typename Types::ParsingState ParsingState;
      typedef typename Types::ParseNamedDeclaration ParseMethod;
    };

    RogerItemizedLateParseCallback *cb = new RogerParseCallback<CbTypesForNamed>(this, decl, DC, topLevelDecs, file, parsingObj);

    bool isArray = false;
    switch (decl->nameKind) {
    case RogerDeclaration::NAME_PLAIN: {
      Token &nameToken = decl->Toks[decl->nameToken];
      DeclarationName Name(nameToken.getIdentifierInfo());
      Actions.ActOnNamedDeclarationRoger(DC, Name, decl->isTemplateSecondary, cb);
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
      Actions.ActOnNamedDeclarationRoger(DC, Name, decl->isTemplateSecondary, cb);
      break;
    }
    case RogerDeclaration::NAME_CONVERSION:
      Actions.ActOnConversionDeclarationRoger(DC, decl->isTemplateSecondary, cb);
      break;
    case RogerDeclaration::NAME_CONSTRUCTOR:
      Actions.ActOnConstructorDeclarationRoger(DC, decl->isTemplateSecondary, cb);
      break;
    case RogerDeclaration::NAME_DESTRUCTOR:
      Actions.ActOnDestructorDeclarationRoger(DC, cb);
      break;
    case RogerDeclaration::NAME_INSTANTIATION: {
      DeclarationName Name;
      Actions.ActOnNamedDeclarationRoger(DC, Name, decl->isTemplateSecondary, cb);
      break;
    }
    default:
      assert(false);
    }
  }
}


DeclContext *Parser::ParseRogerNamespaceHeader(const char * &filePathPos) {
  DeclContext *result = Actions.CurContext;
  if (Tok.isNot(tok::kw_namespace)) {
    return result;
  }
  ConsumeToken();
  ParsedAttributesWithRange attrs(AttrFactory);
  while (true) {
    if (Tok.isNot(tok::identifier)) {
      break;
    }
    IdentifierInfo *II = Tok.getIdentifierInfo();

    if (filePathPos) {
      if (!llvm::sys::path::is_separator(*filePathPos)) {
        filePathPos = 0;
      } else {
        ++filePathPos;
        if (strncmp(II->getNameStart(), filePathPos, II->getLength()) != 0) {
          filePathPos = 0;
        } else {
          filePathPos += II->getLength();
        }
      }
    }
    DeclContext *parent = result;
    result = Actions.ActOnRogerNamespaceHeaderPart(parent, Tok.getIdentifierInfo(), Tok.getLocation(), attrs.getList());
    ConsumeAnyToken();
    if (Tok.isNot(tok::coloncolon)) {
      break;
    }
    ConsumeToken();
  }
  ExpectAndConsume(tok::semi, diag::err_expected_semi_after,
                   "namespace name",
                   tok::semi);
  return result;
}


Parser::DeclGroupPtrTy Parser::ParseRogerDeclarationRegion(RogerBlindParsable *decl, DeclContext *DC, RogerFile *file, RogerParsingNamespace *parsingNs) {
  RogerNestedTokensState parseNestedTokens(this, &decl->rangeBegin(), decl->rangeSize());
  RogerNamespaceParseScope scope(this, file, DC, parsingNs);

  ParsedAttributesWithRange attrs(AttrFactory);
  return ParseExternalDeclaration(attrs);
}

Parser::DeclGroupPtrTy Parser::ParseRogerMemberRegion(RogerBlindParsable *decl, Decl *RD, RogerFile *file, ParsingClass *parsingClass) {
  RogerNestedTokensState parseNestedTokens(this, &decl->rangeBegin(), decl->rangeSize());
  RogerRecordParseScope scope(this, file, RD, parsingClass);

  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  AccessSpecifier AS = calculateAS(decl->getVisibility(), dyn_cast<DeclContext>(RD));

  ParsedAttributesWithRange attrs(AttrFactory);
  ParseCXXClassMemberDeclaration(AS, attrs.getList());
  return DeclGroupPtrTy();
}

class RogerRecordPreparser : public RogerGuardableCallback {
  Parser& parser;
  CXXRecordDecl *recDecl;
  RogerClassDecl *classDecl;
  RogerFile *file;
  int offset;
public:
  RogerRecordPreparser(Parser& p, CXXRecordDecl *rd, RogerClassDecl *cd, RogerFile *file, int offset) : parser(p), recDecl(rd), classDecl(cd), file(file), offset(offset) {}
  void parseDeferredImpl() {
    parser.PreparseRogerClassBody(recDecl, classDecl, file, offset);
  }
};

Parser::DeclGroupPtrTy Parser::ParseRogerTemplatableClassDecl(RogerClassDecl *classDecl, DeclContext *DC, RogerFile *file, void *parsingState) {
  RogerNestedTokensState parseNestedTokens(this, &classDecl->Toks[classDecl->declaration.begin], classDecl->declaration.size());
  RogerParseScope scope(this, file, DC);

  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  AccessSpecifier AS = calculateAS(classDecl->visibility, DC);

  Decl *resultDecl;
  bool typeSpecError = false;

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
        SkipUntil(tok::r_brace, StopAtSemi | StopBeforeMatch);
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

    resultDecl = ParseRogerClassForwardDecl(classDecl, file,
        ParsedTemplateInfo(&ParamLists, isSpecialization, LastParamListWasEmpty),
                    AS, parseNestedTokens, typeSpecError);
  } else {
    ParsingDeclSpec DS(*this);
    resultDecl = ParseRogerClassForwardDecl(classDecl, file, ParsedTemplateInfo(), AS, parseNestedTokens, typeSpecError);
  }

  // Should we handle typeSpecError?

  // Keep position for the future parsing.
  parseNestedTokens.skipRest();

  return Actions.ConvertDeclToDeclGroup(resultDecl);
}

Decl *Parser::ParseRogerClassForwardDecl(RogerClassDecl *classDecl, RogerFile *file,
    const ParsedTemplateInfo &TemplateInfo,
    AccessSpecifier AS, RogerNestedTokensState &parseState, bool &typeSpecError) {
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
  SourceLocation KwLoc = Tok.getLocation();

  ConsumeToken();



  // What is that?
  bool EnteringContext = true; //(DSContext == DSC_class || DSContext == DSC_top_level);

  // Parse the (optional) nested-name-specifier.
  //CXXScopeSpec &SS = DS.getTypeSpecScope();
  CXXScopeSpec SS;
  if (getLangOpts().CPlusPlus) {
    // "FOO : BAR" is not a potential typo for "FOO::BAR".
    ColonProtectionRAIIObject X(*this);

    if (ParseOptionalCXXScopeSpecifier(SS, ParsedType(), EnteringContext)) {
      // DS.SetTypeSpecError();
      typeSpecError = true;
    }
    if (SS.isSet())
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::annot_template_id))
        Diag(Tok, diag::err_expected_ident);
  }

  TemplateParameterLists *TemplateParams = TemplateInfo.TemplateParams;

  // Parse the (optional) class name or simple-template-id.
  IdentifierInfo *Name = 0;
  SourceLocation NameLoc;
  TemplateIdAnnotation *TemplateId = 0;
  if (Tok.is(tok::identifier)) {
    Name = Tok.getIdentifierInfo();
    NameLoc = ConsumeToken();

    if (Tok.is(tok::less) && getLangOpts().CPlusPlus) {
      // The name was supposed to refer to a template, but didn't.
      // Eat the template argument list and try to continue parsing this as
      // a class (or template thereof).
      TemplateArgList TemplateArgs;
      SourceLocation LAngleLoc, RAngleLoc;
      if (ParseTemplateIdAfterTemplateName(TemplateTy(), NameLoc, SS,
                                           true, LAngleLoc,
                                           TemplateArgs, RAngleLoc)) {
        // We couldn't parse the template argument list at all, so don't
        // try to give any location information for the list.
        LAngleLoc = RAngleLoc = SourceLocation();
      }

      Diag(NameLoc, diag::err_explicit_spec_non_template)
        << (TemplateInfo.Kind == ParsedTemplateInfo::ExplicitInstantiation)
        << (TagType == DeclSpec::TST_class? 0
            : TagType == DeclSpec::TST_struct? 1
            : TagType == DeclSpec::TST_interface? 2
            : 3)
        << Name
        << SourceRange(LAngleLoc, RAngleLoc);

      // Strip off the last template parameter list if it was empty, since
      // we've removed its template argument list.
      if (TemplateParams && TemplateInfo.LastParameterListWasEmpty) {
        if (TemplateParams && TemplateParams->size() > 1) {
          TemplateParams->pop_back();
        } else {
          TemplateParams = 0;
          const_cast<ParsedTemplateInfo&>(TemplateInfo).Kind
            = ParsedTemplateInfo::NonTemplate;
        }
      } else if (TemplateInfo.Kind
                                == ParsedTemplateInfo::ExplicitInstantiation) {
        // Pretend this is just a forward declaration.
        TemplateParams = 0;
        const_cast<ParsedTemplateInfo&>(TemplateInfo).Kind
          = ParsedTemplateInfo::NonTemplate;
        const_cast<ParsedTemplateInfo&>(TemplateInfo).TemplateLoc
          = SourceLocation();
        const_cast<ParsedTemplateInfo&>(TemplateInfo).ExternLoc
          = SourceLocation();
      }
    }
  } else if (Tok.is(tok::annot_template_id)) {
    TemplateId = takeTemplateIdAnnotation(Tok);
    NameLoc = ConsumeToken();

    if (TemplateId->Kind != TNK_Type_template &&
        TemplateId->Kind != TNK_Dependent_template_name) {
      // The template-name in the simple-template-id refers to
      // something other than a class template. Give an appropriate
      // error message and skip to the ';'.
      SourceRange Range(NameLoc);
      if (SS.isNotEmpty())
        Range.setBegin(SS.getBeginLoc());

      Diag(TemplateId->LAngleLoc, diag::err_template_spec_syntax_non_template)
        << TemplateId->Name << static_cast<int>(TemplateId->Kind) << Range;

      //DS.SetTypeSpecError();
      typeSpecError = true;
      SkipUntil(tok::semi, StopBeforeMatch);
      return 0;
    }
  }


  // Create the tag portion of the class or class template.
  DeclResult TagOrTempResult = true; // invalid
  //TypeResult TypeResult = true; // invalid

  bool Owned = false;

  ParsedAttributesWithRange attrs(AttrFactory);


  if (TemplateId) {
    ASTTemplateArgsPtr TemplateArgsPtr(TemplateId->getTemplateArgs(),
                                       TemplateId->NumArgs);
    // Explicit specialization, class template partial specialization,
    // or explicit instantiation.
    {
      // This is an explicit specialization or a class template
      // partial specialization.
      TemplateParameterLists FakedParamLists;
      if (TemplateInfo.Kind == ParsedTemplateInfo::ExplicitInstantiation) {
        // This looks like an explicit instantiation, because we have
        // something like
        //
        //   template class Foo<X>
        //
        // but it actually has a definition. Most likely, this was
        // meant to be an explicit specialization, but the user forgot
        // the '<>' after 'template'.
        // It this is friend declaration however, since it cannot have a
        // template header, it is most likely that the user meant to
        // remove the 'template' keyword.
//        assert((TUK == Sema::TUK_Definition || TUK == Sema::TUK_Friend) &&
//               "Expected a definition here");

        {
          SourceLocation LAngleLoc
            = PP.getLocForEndOfToken(TemplateInfo.TemplateLoc);
          Diag(TemplateId->TemplateNameLoc,
               diag::err_explicit_instantiation_with_definition)
            << SourceRange(TemplateInfo.TemplateLoc)
            << FixItHint::CreateInsertion(LAngleLoc, "<>");

          // Create a fake template parameter list that contains only
          // "template<>", so that we treat this construct as a class
          // template specialization.
          FakedParamLists.push_back(
            Actions.ActOnTemplateParameterList(0, SourceLocation(),
                                               TemplateInfo.TemplateLoc,
                                               LAngleLoc,
                                               0, 0,
                                               LAngleLoc));
          TemplateParams = &FakedParamLists;
        }
      }

      // Build the class template specialization.
      TagOrTempResult
        = Actions.ActOnClassTemplateSpecialization(getCurScope(), TagType, Sema::TUK_Definition,
            KwLoc, SourceLocation(), SS,
                       TemplateId->Template,
                       TemplateId->TemplateNameLoc,
                       TemplateId->LAngleLoc,
                       TemplateArgsPtr,
                       TemplateId->RAngleLoc,
                       attrs.getList(),
                       MultiTemplateParamsArg(
                                    TemplateParams? &(*TemplateParams)[0] : 0,
                                 TemplateParams? TemplateParams->size() : 0));
    }
  } else {
    if (TemplateInfo.Kind == ParsedTemplateInfo::ExplicitInstantiation) {
      // If the declarator-id is not a template-id, issue a diagnostic and
      // recover by ignoring the 'template' keyword.
      Diag(Tok, diag::err_template_defn_explicit_instantiation)
        << 1 << FixItHint::CreateRemoval(TemplateInfo.TemplateLoc);
      TemplateParams = 0;
    }

    bool IsDependent = false;

    // Don't pass down template parameter lists if this is just a tag
    // reference.  For example, we don't need the template parameters here:
    //   template <class T> class A *makeA(T t);
    MultiTemplateParamsArg TParams;
    if (TemplateParams)
      TParams =
        MultiTemplateParamsArg(&(*TemplateParams)[0], TemplateParams->size());

    // Declaration or definition of a class type
    TagOrTempResult = Actions.ActOnTag(getCurScope(), TagType, Sema::TUK_Definition, KwLoc,
        SS, Name, NameLoc, attrs.getList(), AS,
                                       /*ModulePrivateSpecLoc=*/ SourceLocation(),
                                       TParams, Owned, IsDependent,
                                       SourceLocation(), false,
                                       clang::TypeResult());


    // If ActOnTag said the type was dependent, try again with the
    // less common call.
    if (IsDependent) {
      assert(false);
    }
  }

  if (TagOrTempResult.isInvalid()) {
    return 0;
  }

  Decl *ResultDecl = TagOrTempResult.get();
  Actions.AdjustDeclIfTemplate(ResultDecl);
  CXXRecordDecl *recDecl = cast<CXXRecordDecl>(ResultDecl);

  recDecl->rogerState = new RecordDecl::RogerState;

  recDecl->pauseDefinitionRoger();

  RogerRecordPreparser *lateParser = new RogerRecordPreparser(*this, recDecl, classDecl, file, parseState.getPos());
  recDecl->rogerState->parseHeader = lateParser;

  return TagOrTempResult.get();
}


void Parser::PreparseRogerClassBody(CXXRecordDecl *recDecl, RogerClassDecl *classDecl, RogerFile *file, int tokenOffset) {
  RogerNestedTokensState parseNestedTokens(this, &classDecl->Toks[classDecl->declaration.begin] + tokenOffset, classDecl->declaration.size() - tokenOffset);
  RogerParseScope scope(this, file, recDecl->getDeclContext(), recDecl);

  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);


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

  struct FillNamesCallBack : RogerGuardableCallback {
    Parser *P;
    CXXRecordDecl *recDecl;
    RogerClassDecl *classDecl;
    ParsingClass *parsingClass;
    RogerFile *file;
    void parseDeferredImpl() {
      P->FillRogerRecordWithNames(classDecl, recDecl, file, parsingClass);
    }
  };
  FillNamesCallBack *cb = new FillNamesCallBack;
  cb->P = this;
  cb->recDecl = recDecl;
  cb->classDecl = classDecl;
  cb->parsingClass = parsingClass;
  cb->file = file;
  recDecl->RogerNameFillCallback = cb;
}

void Parser::RogerCompleteCXXMemberSpecificationParsing(RecordDecl *RD, RogerFile *file, ParsingClass *parsingClass) {
  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  {
    RogerRecordParseScope scope(this, file, RD, parsingClass);

    ParseLexedAttributes(getCurrentClass());
    ParseLexedMethodDeclarations(getCurrentClass());

    // We've finished with all pending member declarations.
    Actions.ActOnFinishCXXMemberDecls();

    ParseLexedMemberInitializers(getCurrentClass());
    ParseLexedMethodDefs(getCurrentClass());
  }
  DeallocateParsedClasses(parsingClass);
}

NamespaceDecl *Parser::GetOrParseRogerFileScope(RogerFile *file) {
  if (!file) {
    return 0;
  }
  if (file->FileScopeNs) {
    return file->FileScopeNs;
  }

  // Enter a scope for the namespace.
  ParseScope NamespaceScope(this, Scope::DeclScope);

  NamespaceDecl *NsD = Actions.ActOnEnterRogerFileScopeNamespace(getCurScope());

  struct TypesForFileScopeNs {
    typedef DeclContext DeclContext;
    typedef RogerParsingNamespace ParsingState;
    typedef RogerLiteral<Parser::DeclGroupPtrTy (Parser::*)(RogerBlindParsable *, DeclContext *, RogerFile *, RogerParsingNamespace *), &Parser::ParseRogerDeclarationRegion> ParseNamedDeclaration;
  };

  FillRogerDeclContextWithNamedDecls<TypesForFileScopeNs>(file->inner.Using, Actions.CurContext, 0, 0, 0);

  for (size_t i = 0; i < file->inner.UsingDir.size(); ++i) {
    RogerNonType *rogerUsing = file->inner.UsingDir[i];
    RogerNestedTokensState parseNestedTokens(this, &rogerUsing->Toks[rogerUsing->range.begin], rogerUsing->range.size());
    DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(TemplateIds, TemplateIdsBeingCovered);
    ParenBraceBracketBalancer BalancerRAIIObj(*this);
    ParsedAttributesWithRange attrs(AttrFactory);
    ParseExternalDeclaration(attrs);
  }

  Actions.ActOnExitRogerFileScopeNamespace();

  file->FileScopeNs = NsD;

  return NsD;
}

struct Parser::LateParsedStaticVarInitializer::Callback : RogerGuardableCallback {
  CachedTokens Toks;

  /// CachedTokens - The sequence of tokens that comprises the initializer,
  /// including any leading '='.
  Decl *Field;
  Parser *Self;
  RogerFile *file;
  void parseDeferredImpl() {
    Self->ParseLexedRogerStaticVarInitializer(this);
  }
};

Parser::LateParsedStaticVarInitializer::LateParsedStaticVarInitializer(Parser *P, VarDecl *FD, RogerFile *file)
    : Self(P), Field(FD) {
  Callback *cb = new Callback;
  callback = cb;
  cb->Field = FD;
  cb->Self = P;
  cb->file = file;
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
  RogerParseScope parseScope(this, cb->file, parent);

  if (cb->Toks.empty()) {
    // Roger hack.
    bool containsPlaceholderType = false;
    Actions.ActOnUninitializedDecl(ThisDecl, containsPlaceholderType);
    return;
  }

  RogerNestedTokensState parseNestedTokens(this, cb->Toks.begin(), cb->Toks.size());

  // Hack.
  bool TypeContainsAuto = false;

  if (Tok.is(tok::equal)) {
    ConsumeToken();
    ExprResult Init(ParseInitializer());
    if (Init.isInvalid()) {
      SkipUntil(tok::comma, StopAtSemi | StopBeforeMatch);
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

template<void (Parser::LateParsedDeclaration::*parseMethod)(), RogerItemizedLateParseCallback * FunctionDecl::*callbackField>
Parser::LateParsedDeclaration *Parser::CreateOnDemandLateParsedDeclaration(LateParsedDeclaration *LM, DeclContext *DC,
    Decl *ND, RogerFile *file) {
  FunctionDecl *FD;
  if (FunctionTemplateDecl *FTD = dyn_cast<FunctionTemplateDecl>(ND))
    FD = FTD->getTemplatedDecl();
  else
    FD = cast<FunctionDecl>(ND);

  struct CB : RogerGuardableCallback {
    Parser *P;
    DeclContext *DC;
    LateParsedDeclaration *LM;
    FunctionDecl *FD;
    RogerFile *file;
    void parseDeferredImpl() {
      DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(P->TemplateIds, P->TemplateIdsBeingCovered);
      ParenBraceBracketBalancer BalancerRAIIObj(*P);

      RogerParseScope parseScope(P, file, DC);
      (LM->*parseMethod)();
      delete LM;
    }
  };
  CB *cb = new CB;
  cb->P = this;
  cb->DC = DC;
  cb->LM = LM;
  cb->FD = FD;
  cb->file = file;

  FD->*callbackField = cb;

  struct LP : LateParsedDeclaration {
    Parser *P;
    FunctionDecl *FD;
    LateParsedDeclaration *LM;

    void ParseLexedMethodDefs() {
      callParseMethod(&LateParsedDeclaration::ParseLexedMethodDefs);
    }

    void ParseLexedMethodDeclarations() {
      callParseMethod(&LateParsedDeclaration::ParseLexedMethodDeclarations);
    }

    void callParseMethod(void (Parser::LateParsedDeclaration::*invokedMethod)()) {
      if (invokedMethod != parseMethod) {
        return;
      }
      if (!(FD->*callbackField)) {
        return;
      }
      delete (FD->*callbackField);
      FD->*callbackField = 0;
      LM->ParseLexedMethodDefs();
      delete LM;
    }

  };
  LP *lp = new LP();
  lp->P = this;
  lp->LM = LM;
  lp->FD = FD;
  return lp;
}

Parser::LateParsedDeclaration *Parser::CreateOnDemandLexedMethod(LexedMethod *LM, DeclContext *DC, RogerFile *file) {
  Decl *ND = LM->D;
  return CreateOnDemandLateParsedDeclaration<&LateParsedDeclaration::ParseLexedMethodDefs, &FunctionDecl::rogerDeferredBodyParse>(LM, DC, ND, file);
}

template
Parser::LateParsedDeclaration *Parser::CreateOnDemandLateParsedDeclaration
    <&Parser::LateParsedDeclaration::ParseLexedMethodDeclarations, &FunctionDecl::rogerDeferredDeclarationParse>
    (LateParsedDeclaration *LM, DeclContext *DC, Decl *ND, RogerFile *file);
