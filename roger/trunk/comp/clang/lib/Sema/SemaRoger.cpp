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

void Sema::ActOnNamedDeclarationRoger(DeclarationName Name, RogerItemizedLateParseCallback *callback) {
  DeclContext::UnparsedNamedDecl *node = new DeclContext::UnparsedNamedDecl;
  node->name = Name;
  node->callback = callback;
  CurContext->unparsedDecls.push_back(node);
}

static void AddRogerDeclarationToCurrentRecord(RogerItemizedLateParseCallback *callback, DeclContext *dc, llvm::ilist<DeclContext::UnparsedNamedDecl> RecordDecl::RogerState::* field) {
  RecordDecl *rd = cast<RecordDecl>(dc);
  DeclContext::UnparsedNamedDecl *node = new DeclContext::UnparsedNamedDecl;
  node->name = DeclarationName();
  node->callback = callback;
  (rd->rogerState->*field).push_back(node);
}

void Sema::ActOnConversionDeclarationRoger(RogerItemizedLateParseCallback *callback) {
  AddRogerDeclarationToCurrentRecord(callback, CurContext, &RecordDecl::RogerState::rogerConversionDecls);
}
void Sema::ActOnConstructorDeclarationRoger(RogerItemizedLateParseCallback *callback) {
  AddRogerDeclarationToCurrentRecord(callback, CurContext, &RecordDecl::RogerState::rogerConstructorDecls);
}
void Sema::ActOnDestructorDeclarationRoger(RogerItemizedLateParseCallback *callback) {
  AddRogerDeclarationToCurrentRecord(callback, CurContext, &RecordDecl::RogerState::rogerDestructorDecls);
}


void Sema::ActOnNamespaceFinishRoger(DeclContext* ns, SmallVector<DeclGroupRef, 4> *TopLevelList) {
  CompleteDeclContextRoger(ns, TopLevelList);
}

void Sema::CompleteDeclContextRoger(DeclContext* ns, SmallVector<DeclGroupRef, 4> *TopLevelList) {
  while (!ns->unparsedDecls.empty()) {
    DeclarationName Name = ns->unparsedDecls.begin()->name;

    llvm::ilist<DeclContext::UnparsedNamedDecl> todo;

    for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = ns->unparsedDecls.begin(); it != ns->unparsedDecls.end();) {
      DeclContext::UnparsedNamedDecl &node = *it;
      if (node.name == Name) {
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
  if (RecordDecl *rec = dyn_cast<RecordDecl>(ns)) {
    MaterializeRogerContructors(rec);
    MaterializeRogerDestructors(rec);
    MaterializeRogerConversionOperators(rec);
  }

  if (false &&            TopLevelList) {
    for (DeclGroupRef *it = TopLevelList->begin(); it != TopLevelList->end(); ++it) {
      DeclGroupRef &DG = *it;
      for (DeclGroupRef::iterator I = DG.begin(), E = DG.end(); I != E; ++I) {
        CompleteDeclRoger(*I);
      }
    }
  } else {
    for (DeclContext::decl_iterator it = ns->decls_begin(); it != ns->decls_end(); ++it) {
      CompleteDeclRoger(*it);
    }
  }

  if (ns->RogerCompleteParsingCallback) {
    assert(!ns->RogerCompleteParsingCallback->isBusy());
    ns->RogerCompleteParsingCallback->parseDeferred();
    delete ns->RogerCompleteParsingCallback;
    ns->RogerCompleteParsingCallback = 0;
  }
}

void Sema::CompleteDeclRoger(Decl* D) {
  if (NamespaceDecl *NsD = dyn_cast<NamespaceDecl>(D)) {
    if (NsD->RogerNameFillCallback) {
      assert(!NsD->RogerNameFillCallback->isBusy());
      NsD->RogerNameFillCallback->parseDeferred();
      delete NsD->RogerNameFillCallback;
      NsD->RogerNameFillCallback = 0;
    }
    ActOnNamespaceFinishRoger(NsD, 0);
  } else if (RecordDecl *RecD = dyn_cast<RecordDecl>(D)) {
    if (!RecD->isImplicit()) {
      RequireCompleteRecordRoger(RecD, RRCR_FULL);
    }
  }
}

//void Sema::InstantiateFunctionDefinitionRoger(FunctionDecl *Function) {
//  if (Function->isInvalidDecl())
//    return;
//
//  if (Function->getTemplateSpecializationKind() == TSK_ExplicitSpecialization &&
//      !Function->getClassScopeSpecializationPattern())
//    return;
//
//  assert(RogerOnDemandParser && "RogerOnDemandParser problemo");
//  LateParsedTemplate *LPT = LateParsedTemplateMap.lookup(Function);
//  assert(LPT && "missing LateParsedTemplate");
//  RogerOnDemandParser->ParseFunction(*LPT);
//}

static void MaterializeRogerList(Sema &S, llvm::ilist<DeclContext::UnparsedNamedDecl> &list, DeclContext* dc) {
  if (dc->RogerNameFillCallback) {
    dc->RogerNameFillCallback->parseDeferred();
    delete dc->RogerNameFillCallback;
    dc->RogerNameFillCallback = 0;
  }

  llvm::ilist<DeclContext::UnparsedNamedDecl> todo;
  for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = list.begin(); it != list.end(); ++it) {
    todo.push_back(*it);
  }
  list.clear();

  DeclContext *savedContext = S.CurContext;
  S.CurContext = dc;
  for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = todo.begin(); it != todo.end(); ++it) {
    it->callback->parseDeferred();
    delete it->callback;
  }
  S.CurContext = savedContext;
}

void Sema::MaterializeRogerContructors(RecordDecl *Class) {
  if (Class->rogerState) {
    MaterializeRogerList(*this, Class->rogerState->rogerConstructorDecls, Class);
  }
}
void Sema::MaterializeRogerDestructors(RecordDecl *Class) {
  if (Class->rogerState) {
    MaterializeRogerList(*this, Class->rogerState->rogerDestructorDecls, Class);
  }
}
void Sema::MaterializeRogerConversionOperators(RecordDecl *Class) {
  if (Class->rogerState) {
    MaterializeRogerList(*this, Class->rogerState->rogerConversionDecls, Class);
  }
}


void Sema::MaterializeRogerNames(DeclarationName Name, DeclContext* dc, bool Redecl) {
  if (!dc) {
    return;
  }

  if (dc->RogerNameFillCallback) {
    RogerLogScope logScope("MaterializeRogerNames 1: fill names");
    logScope.outs_nl() << Name;
    if (NamedDecl *nd = dyn_cast<NamedDecl>(dc)) {
      logScope.outs() << " in ";
      nd->printName(logScope.outs());
    }
    if (Redecl) {
      logScope.outs() << " for redeclaration";
    }
    logScope.outs() << " item(s)\n";
    if (dc->RogerNameFillCallback->isBusy()) {
      logScope.outs_nl() << "busy\n";
      return;
    }
    dc->RogerNameFillCallback->parseDeferred();
    delete dc->RogerNameFillCallback;
    dc->RogerNameFillCallback = 0;
  }

  llvm::ilist<DeclContext::UnparsedNamedDecl> todo;

  for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = dc->unparsedDecls.begin(); it != dc->unparsedDecls.end();) {
    DeclContext::UnparsedNamedDecl &node = *it;
    if (node.name == Name) {
      assert(!node.beingCompiled && "compile error with cyclic ref");
      todo.push_back(node);
      dc->unparsedDecls.remove(it);
    } else {
      ++it;
    }
  }
  if (!todo.size()) {
    return;
  }
  {
    RogerLogScope logScope("MaterializeRogerNames 2: go over items");
    logScope.outs_nl() << Name;
    if (NamedDecl *nd = dyn_cast<NamedDecl>(dc)) {
      logScope.outs() << " in ";
      nd->printName(logScope.outs());
    }
    if (Redecl) {
      logScope.outs() << " for redeclaration";
    }
    logScope.outs() << todo.size() << " item(s)\n";

    for (llvm::ilist<DeclContext::UnparsedNamedDecl>::iterator it = todo.begin(); it != todo.end(); ++it) {
      DeclContext *savedContext = CurContext;
      CurContext = dc;
      it->callback->parseDeferred();
      delete it->callback;
      CurContext = savedContext;
  }
  }
}

bool Sema::RequireCompleteTypeRoger(QualType T, RogerRequireCompleteReason RogerOnlyInheritance) {
  if (!T->isIncompleteType()) {
    return false;
  }
  if (T.getCanonicalType()->getTypeClass() != Type::Record) {
    return false;
  }
  RecordDecl *Rec = cast<RecordType>(T.getCanonicalType())->getDecl();
  return RequireCompleteRecordRoger(Rec, RogerOnlyInheritance);
}

bool Sema::RequireCompleteRecordRoger(RecordDecl *Rec, RogerRequireCompleteReason RogerOnlyInheritance) {
  RecordDecl::RogerState *state = Rec->rogerState;
  if (!state) {
    return false;
  }
  RogerLogScope logScope("RequireCompleteRecordRoger");
  Rec->printName(logScope.outs_nl());
  logScope.outs() << "\n";
  if (state->currentStep == RecordDecl::RogerState::PREPARSING) {
    logScope.outs_nl() << "Required while preparsing\n";
    return false;
  }
  if (state->currentStep == RecordDecl::RogerState::FORWARD) {
    RogerLogScope logScope("parsing header as required");
    assert(state->parseHeader);
    state->currentStep = RecordDecl::RogerState::PREPARSING;
    state->parseHeader->parseDeferred();
    delete state->parseHeader;
    state->parseHeader = 0;
    state->currentStep = RecordDecl::RogerState::PREPARSE_DONE;
  }
  if (RogerOnlyInheritance == RRCR_INHERITANCE) {
    return true;
  }
  if (state->currentStep == RecordDecl::RogerState::FILLING_NAMES) {
    logScope.outs_nl() << "Required while filling names\n";
    return false;
  }
  if (state->currentStep == RecordDecl::RogerState::PREPARSE_DONE) {
    RogerLogScope logScope("filling names as required");
    assert(Rec->RogerNameFillCallback);
    state->currentStep = RecordDecl::RogerState::FILLING_NAMES;
    Rec->RogerNameFillCallback->parseDeferred();
    delete Rec->RogerNameFillCallback;
    Rec->RogerNameFillCallback = 0;
    state->currentStep = RecordDecl::RogerState::FILL_NAMES_DONE;
  }
  if (RogerOnlyInheritance == RRCR_NESTED) {
    return true;
  }
  if (state->currentStep == RecordDecl::RogerState::COMPLETING) {
    logScope.outs_nl() << "Required while completing\n";
    return false;
  }
  if (state->currentStep == RecordDecl::RogerState::FILL_NAMES_DONE) {
    RogerLogScope logScope("completing record as required");
    state->currentStep = RecordDecl::RogerState::COMPLETING;

    assert(Rec->isBeingDefinedRoger());

    CompleteDeclContextRoger(Rec, 0);

    ActOnFinishCXXMemberSpecificationRoger(Rec);
    state->currentStep = RecordDecl::RogerState::COMPLETE;
  }
  assert(state->currentStep == RecordDecl::RogerState::COMPLETE);
  return false;
}

void Sema::ActOnFinishCXXMemberSpecificationRoger(RecordDecl *TagDecl) {
  Scope* S = 0;
  SourceLocation RLoc;
  SourceLocation LBrac;
  SourceLocation RBrac;
  AttributeList *AttrList = 0;

  if (!TagDecl)
    return;

  // Make sure we work with templates!
  //AdjustDeclIfTemplate(TagDecl);

//  for (const AttributeList* l = AttrList; l; l = l->getNext()) {
//    if (l->getKind() != AttributeList::AT_Visibility)
//      continue;
//    l->setInvalid();
//    Diag(l->getLoc(), diag::warn_attribute_after_definition_ignored) <<
//      l->getName();
//  }



  // Must reorder fields and methods!
  // Add private: and public: decls

  Decl** fieldsStart = 0;
  int fieldsSize = 0;
  RecordDecl::RogerState *rogerState = TagDecl->rogerState;
  if (rogerState->rogerCollectedFields) {
    fieldsStart = reinterpret_cast<Decl**>(rogerState->rogerCollectedFields->data());
    fieldsSize = rogerState->rogerCollectedFields->size();
  }

  ActOnFields(S, RLoc, TagDecl, llvm::makeArrayRef(
              // strict aliasing violation!
      fieldsStart, fieldsSize), LBrac, RBrac, AttrList);

  if (rogerState->rogerCollectedFields) {
    delete rogerState->rogerCollectedFields;
    rogerState->rogerCollectedFields = 0;
  }

  CheckCompletedCXXClass(
                        dyn_cast_or_null<CXXRecordDecl>(TagDecl));
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

void Sema::ActOnTagPauseDefinitionRoger(Decl *TagD) {
  if (RecordDecl *rd = dyn_cast<RecordDecl>(TagD)) {
    RecordDecl::RogerState *rogerState = rd->rogerState;
    if (FieldCollector->getCurNumFields()) {
      if (!rogerState->rogerCollectedFields) {
        rogerState->rogerCollectedFields = new std::vector<FieldDecl*>;
      }
      FieldDecl **pos = FieldCollector->getCurFields();
      for (size_t i = 0; i < FieldCollector->getCurNumFields(); ++i) {
        rogerState->rogerCollectedFields->push_back(pos[i]);
      }
    }
  }
  FieldCollector->FinishClass();
//
//  // Exit this scope of this tag's definition.
//  PopDeclContext();

  if (RecordDecl *rd = dyn_cast<RecordDecl>(TagD)) {
    rd->pauseDefinitionRoger();
  }
}

void Sema::ActOnTagResumeDefinitionRoger(Scope *S, Decl *TagD) {
  //TagDecl *Tag = cast<TagDecl>(TagD);
  RecordDecl *rd = dyn_cast<RecordDecl>(TagD);
  if (rd) {
    rd->resumeDefinitionRoger();
  }

  // Do we need it?
  /*
  if (rd) {
    CXXRecordDecl *injectedDecl = 0;
    for (DeclContext::decl_iterator it = rd->decls_begin(); it != rd->decls_end(); ++it) {
      if (CXXRecordDecl *t = dyn_cast<CXXRecordDecl>(*it)) {
        if (rd->getDeclName() == t->getDeclName()) {
          injectedDecl = t;
          break;
        }
      }
    }
    assert(injectedDecl);
    PushOnScopeChains(injectedDecl, S);
  }
  */

  FieldCollector->StartClass();
}





void Sema::RogerDefineRecord(CXXRecordDecl *decl) {
  if (decl->isRogerRec()) {
    RequireCompleteRecordRoger(decl, RRCR_INHERITANCE);
  }
}

//void Sema::ActOnPauseCXXMemberDeclarationsRoger(Scope *S, Decl *TagD) {
//  AdjustDeclIfTemplate(TagD);
//  CXXRecordDecl *Record = cast<CXXRecordDecl>(TagD);
//
//  FieldCollector->StartClass();
//
//  if (!Record->getIdentifier())
//    return;
//
//  if (FinalLoc.isValid())
//    Record->addAttr(new (Context) FinalAttr(FinalLoc, Context));
//
//  // C++ [class]p2:
//  //   [...] The class-name is also inserted into the scope of the
//  //   class itself; this is known as the injected-class-name. For
//  //   purposes of access checking, the injected-class-name is treated
//  //   as if it were a public member name.
//  CXXRecordDecl *InjectedClassName
//    = CXXRecordDecl::Create(Context, Record->getTagKind(), CurContext,
//                            Record->getLocStart(), Record->getLocation(),
//                            Record->getIdentifier(),
//                            /*PrevDecl=*/0,
//                            /*DelayTypeCreation=*/true);
//  Context.getTypeDeclType(InjectedClassName, Record);
//  InjectedClassName->setImplicit();
//  InjectedClassName->setAccess(AS_public);
//  if (ClassTemplateDecl *Template = Record->getDescribedClassTemplate())
//      InjectedClassName->setDescribedClassTemplate(Template);
//  PushOnScopeChains(InjectedClassName, S);
//  assert(InjectedClassName->isInjectedClassName() &&
//         "Broken injected-class-name");
//}
//
//void Sema::ActOnResumeCXXMemberDeclarationsRoger(Scope *S, Decl *TagD) {
//  AdjustDeclIfTemplate(TagD);
//  CXXRecordDecl *Record = cast<CXXRecordDecl>(TagD);
//
//  FieldCollector->StartClass();
//
//  if (!Record->getIdentifier())
//    return;
//
//  if (FinalLoc.isValid())
//    Record->addAttr(new (Context) FinalAttr(FinalLoc, Context));
//
//  // C++ [class]p2:
//  //   [...] The class-name is also inserted into the scope of the
//  //   class itself; this is known as the injected-class-name. For
//  //   purposes of access checking, the injected-class-name is treated
//  //   as if it were a public member name.
//  CXXRecordDecl *InjectedClassName
//    = CXXRecordDecl::Create(Context, Record->getTagKind(), CurContext,
//                            Record->getLocStart(), Record->getLocation(),
//                            Record->getIdentifier(),
//                            /*PrevDecl=*/0,
//                            /*DelayTypeCreation=*/true);
//  Context.getTypeDeclType(InjectedClassName, Record);
//  InjectedClassName->setImplicit();
//  InjectedClassName->setAccess(AS_public);
//  if (ClassTemplateDecl *Template = Record->getDescribedClassTemplate())
//      InjectedClassName->setDescribedClassTemplate(Template);
//  PushOnScopeChains(InjectedClassName, S);
//  assert(InjectedClassName->isInjectedClassName() &&
//         "Broken injected-class-name");
//}



void Sema::ActOnRogerModeStart(RogerOnDemandParserInt* RogerOnDemandParser) {
  assert(RogerOnDemandParser);
  this->RogerOnDemandParser = RogerOnDemandParser;
}

void Sema::ActOnRogerModeFinish() {
  this->RogerOnDemandParser = 0;
}
bool Sema::IsInRogerMode() {
  return this->RogerOnDemandParser;
}

const bool Sema::RogerLogScope::mute = false;
