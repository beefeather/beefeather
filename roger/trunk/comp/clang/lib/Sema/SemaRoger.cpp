#include "clang/Sema/SemaInternal.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTLambda.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeLoc.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/CXXFieldCollector.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include <map>
#include <set>

using namespace clang;

void Sema::ActOnNamedDeclarationRoger(IdentifierInfo* Name, RogerItemizedLateParseCallback *callback) {
  DeclContext::UnparsedNamedDecl *node = new DeclContext::UnparsedNamedDecl;
  node->name = Name;
  node->callback = callback;
  CurContext->unparsedDecls.push_back(node);
}

void Sema::ActOnNamespaceFinishRoger(DeclContext* ns) {
  while (!ns->unparsedDecls.empty()) {
    IdentifierInfo *II = ns->unparsedDecls.begin()->name;

    llvm::ilist<DeclContext::UnparsedNamedDecl> todo;

    for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = ns->unparsedDecls.begin(); it != ns->unparsedDecls.end();) {
      DeclContext::UnparsedNamedDecl &node = *it;
      if (node.name == II) {
        assert(!node.beingCompiled && "compile error with cyclic ref");
        todo.push_back(node);
        ns->unparsedDecls.remove(it);
      } else {
        ++it;
      }
    }

    for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = todo.begin(); it != todo.end(); ++it) {
      it->callback->parseDeferred();
      delete it->callback;
    }
  }

  for (DeclContext::decl_iterator it = ns->decls_begin(); it != ns->decls_end(); ++it) {
    if (FunctionDecl *FnD = dyn_cast<FunctionDecl>(*it)) {
      if (FnD->RogerPlannedForLateParsing) {
        InstantiateFunctionDefinitionRoger(FnD);
        FnD->RogerPlannedForLateParsing = false;
      }
    } else if (NamespaceDecl *NsD = dyn_cast<NamespaceDecl>(*it)) {
      if (NsD->RogerNameFillCallback) {
        NsD->RogerNameFillCallback->parseDeferred();
        delete NsD->RogerNameFillCallback;
        NsD->RogerNameFillCallback = 0;
      }
      ActOnNamespaceFinishRoger(NsD);
    }
  }
}

void Sema::InstantiateFunctionDefinitionRoger(FunctionDecl *Function) {
  if (Function->isInvalidDecl())
    return;

  if (Function->getTemplateSpecializationKind() == TSK_ExplicitSpecialization &&
      !Function->getClassScopeSpecializationPattern())
    return;

  assert(RogerOnDemandParser && "RogerOnDemandParser problemo");
  LateParsedTemplate *LPT = LateParsedTemplateMap.lookup(Function);
  assert(LPT && "missing LateParsedTemplate");
  RogerOnDemandParser->ParseFunction(*LPT);

}



void Sema::MaterializeRogerNames(DeclarationName Name, DeclContext* dc) {
  // TODO: set correct scope and state.

  if (!dc) {
    return;
  }

  if (dc->RogerNameFillCallback) {
    DeclContext *savedContext = CurContext;
    Scope *savedScope = CurScope;
    dc->RogerNameFillCallback->parseDeferred();
    delete dc->RogerNameFillCallback;
    dc->RogerNameFillCallback = 0;
    assert(CurContext == savedContext);
    assert(CurScope == savedScope);
  }

  IdentifierInfo *II = Name.getAsIdentifierInfo();
  if (!II) {
    return;
  }
  llvm::ilist<DeclContext::UnparsedNamedDecl> todo;

  for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = dc->unparsedDecls.begin(); it != dc->unparsedDecls.end();) {
    DeclContext::UnparsedNamedDecl &node = *it;
    if (node.name == II) {
      assert(!node.beingCompiled && "compile error with cyclic ref");
      todo.push_back(node);
      dc->unparsedDecls.remove(it);
    } else {
      ++it;
    }
  }
  for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = todo.begin(); it != todo.end(); ++it) {
    DeclContext *savedContext = CurContext;
    CurContext = dc;
    it->callback->parseDeferred();
    delete it->callback;
    CurContext = savedContext;
  }
  // TODO: restore current scope and state.
}

NamespaceDecl *Sema::ActOnRogerNamespaceDef(Scope *NamespcScopeIgnore,
                                   IdentifierInfo *II,
                                   SourceLocation NameLoc,
                                   RogerItemizedLateParseCallback *rogerCallback,
                                   AttributeList *AttrList) {

  // set CurContext

  SourceLocation NamespaceLoc;
  SourceLocation IdentLoc;
  SourceLocation InlineLoc;
  SourceLocation LBrace;
  SourceLocation StartLoc = InlineLoc.isValid() ? InlineLoc : NamespaceLoc;

  // For anonymous namespace, take the location of the left brace.
  SourceLocation Loc = II ? IdentLoc : LBrace;
  bool IsInline = false;
  bool IsInvalid = false;
  bool IsStd = false;
  bool AddToKnown = false;
  //Scope *DeclRegionScope = NamespcScope->getParent();

  NamespaceDecl *PrevNS = 0;
  // C++ [namespace.def]p2:
  //   The identifier in an original-namespace-definition shall not
  //   have been previously defined in the declarative region in
  //   which the original-namespace-definition appears. The
  //   identifier in an original-namespace-definition is the name of
  //   the namespace. Subsequently in that declarative region, it is
  //   treated as an original-namespace-name.
  //
  // Since namespace names are unique in their scope, and we don't
  // look through using directives, just look for any ordinary names.

  const unsigned IDNS = Decl::IDNS_Ordinary | Decl::IDNS_Member |
  Decl::IDNS_Type | Decl::IDNS_Using | Decl::IDNS_Tag |
  Decl::IDNS_Namespace;
  NamedDecl *PrevDecl = 0;
  DeclContext::lookup_result R = CurContext->getRedeclContext()->lookup(II);
  for (DeclContext::lookup_iterator I = R.begin(), E = R.end(); I != E;
       ++I) {
    if ((*I)->getIdentifierNamespace() & IDNS) {
      PrevDecl = *I;
      break;
    }
  }

  PrevNS = dyn_cast_or_null<NamespaceDecl>(PrevDecl);

  if (PrevNS) {
    // This is an extended namespace definition.
    if (IsInline != PrevNS->isInline()) {
//      DiagnoseNamespaceInlineMismatch(*this, NamespaceLoc, Loc, II,
//                                      &IsInline, PrevNS);
      assert(false && "need to implement this");
    }
    if (PrevNS->IsRogerNamespace) {
      Diag(NameLoc, diag::err_namespace_redefinition_roger);
    }
  } else if (PrevDecl) {
    // This is an invalid name redefinition.
    Diag(Loc, diag::err_redefinition_different_kind)
      << II;
    Diag(PrevDecl->getLocation(), diag::note_previous_definition);
    IsInvalid = true;
    // Continue on to push Namespc as current DeclContext and return it.
  } else if (II->isStr("std") &&
             CurContext->getRedeclContext()->isTranslationUnit()) {
    // This is the first "real" definition of the namespace "std", so update
    // our cache of the "std" namespace to point at this definition.
    PrevNS = getStdNamespace();
    IsStd = true;
    AddToKnown = !IsInline;
  } else {
    // We've seen this namespace for the first time.
    AddToKnown = !IsInline;
  }

  NamespaceDecl *Namespc = NamespaceDecl::Create(Context, CurContext, IsInline,
                                                 StartLoc, Loc, II, PrevNS);
  Namespc->IsRogerNamespace = true;
  Namespc->RogerNameFillCallback = rogerCallback;

  if (IsInvalid)
    Namespc->setInvalidDecl();

  //ProcessDeclAttributeList(DeclRegionScope, Namespc, AttrList);

  // FIXME: Should we be merging attributes?
  if (const VisibilityAttr *Attr = Namespc->getAttr<VisibilityAttr>())
    PushNamespaceVisibilityAttr(Attr, Loc);

  if (IsStd)
    StdNamespace = Namespc;
  if (AddToKnown)
    KnownNamespaces[Namespc] = false;

  CurContext->addDecl(Namespc);

  return Namespc;
}



void Sema::ActOnRogerModeStart(RogerOnDemandParserInt* RogerOnDemandParser) {
  assert(RogerOnDemandParser);
  this->RogerOnDemandParser = RogerOnDemandParser;
}

void Sema::ActOnRogerModeFinish() {
  this->RogerOnDemandParser = 0;
}
