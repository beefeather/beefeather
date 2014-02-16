package ru.spb.rybin.clangroger.overviewparser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTDeclarationListOwner;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTNodeLocation;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTConversionName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTExplicitTemplateInstantiation;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTOperatorName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTQualifiedName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateId;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDirective;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVisibilityLabel;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.internal.core.dom.parser.IASTInternalEnumerationSpecifier;

import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.ClassRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DataFile;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationName;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.EnumRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.ErrorFile;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.ErrorRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.File;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.FunctionRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.NamespaceRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.NonTypeRegion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.Region;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.TokenRange;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.UsingRegion;

class AstConverter {
	private static final boolean SUPPORT_NAMESPACES = false;
	
	static File createOutputMode(IASTTranslationUnit unit, int textLength, Printer printer) {
		final List<? extends Region> regions = dumpDeclarations(unit, printer);
		return new DataFile() {
			@Override public List<? extends Region> regions() {
				return regions;
			}
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitData(this);
			}
		};
	}
	
	static File createErrorFile(final String message) {
		return new ErrorFile() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitError(this);
			}
			@Override public String message() {
				return message;
			}
		};
	}

	private static List<? extends Region> dumpDeclarations(IASTDeclarationListOwner declarationListOwner, Printer printer) {
		final List<Region> regions = new ArrayList<Region>();
		
		IASTDeclaration[] declarations = declarationListOwner.getDeclarations(true);
		if (declarations.length != 0) {
			int lastUnreportedPos = declarations[0].getNodeLocations()[0].getNodeOffset();
			
			Integer currentVisibility = null;
			
			for (IASTDeclaration d : declarations) {
				Integer newVisibility = currentVisibility;
				boolean other;
				if (d instanceof ICPPASTNamespaceDefinition) {
					ICPPASTNamespaceDefinition namespaceDefinition = (ICPPASTNamespaceDefinition) d;
					if (SUPPORT_NAMESPACES) {
						NamespaceRegion namespaceRegion = dumpNamespace(namespaceDefinition, printer);
						regions.add(namespaceRegion);
					} else {
						ErrorRegion region = createErrorRegion(namespaceDefinition, "namespace block", printer);
						regions.add(region);
					}
					other = false;
				} else if (d instanceof ICPPASTVisibilityLabel) {
					ICPPASTVisibilityLabel visibilityLabel = (ICPPASTVisibilityLabel) d;
					newVisibility = visibilityLabel.getVisibility();
					other = false;
				} else if (d instanceof IASTFunctionDefinition) {
					IASTFunctionDefinition functionDef = (IASTFunctionDefinition) d;
					if (functionDef.getDeclarator().getName() instanceof ICPPASTQualifiedName) {
						regions.add(dumpDefinitionRegion(currentVisibility, functionDef, printer));
					} else {
						FunctionRegion region = dumpFunctionDefinition(functionDef, functionDef, currentVisibility, false, false, printer);
						regions.add(region);
					}
					other = false;
				} else if (d instanceof ICPPASTUsingDeclaration) {
					ICPPASTUsingDeclaration usingDecl = (ICPPASTUsingDeclaration) d;
					UsingRegion region = dumpUsingDeclaration(usingDecl, currentVisibility, printer);
					regions.add(region);
					other = false;
				} else if (d instanceof ICPPASTUsingDirective) {
					ICPPASTUsingDirective usingDir = (ICPPASTUsingDirective) d;
					UsingRegion region = dumpUsingDirective(usingDir, currentVisibility, printer);
					regions.add(region);
					other = false;
				} else {
					Collection<? extends Region> typeRegions = dumpTypeItem(d, currentVisibility, printer);
					if (typeRegions == null) {
						other = true;
					} else {
						regions.addAll(typeRegions);
						other = false;
					}
				}
				if (!other) {
					IASTNodeLocation loc = d.getNodeLocations()[0];
					int unreportedEnd = loc.getNodeOffset();
					NonTypeRegion nonTypeRegion = dumpOther(currentVisibility, lastUnreportedPos, unreportedEnd, printer);
					if (nonTypeRegion != null) {
					    regions.add(nonTypeRegion);
					}
					lastUnreportedPos = loc.getNodeOffset() + loc.getNodeLength();
				}
				currentVisibility = newVisibility;
			}
			{
				if (declarations.length > 0) {
					IASTNodeLocation loc = declarations[declarations.length - 1].getNodeLocations()[0];
					int endPos = loc.getNodeOffset() + loc.getNodeLength();
					NonTypeRegion nonTypeRegion = dumpOther(currentVisibility, lastUnreportedPos, endPos, printer);
					if (nonTypeRegion != null) {
					    regions.add(nonTypeRegion);
					}
				}
			}
		}
		return regions;
	}
	
	private static Region dumpDefinitionRegion(Integer visibility, IASTDeclaration decl, Printer printer) {
		//return createErrorRegion(decl, "definitions are not supported", printer);
		return dumpNonType(visibility, createRange(decl), printer);
	}
	
	private static FunctionRegion dumpFunctionDefinition(final IASTFunctionDefinition functionDef, final IASTDeclaration fullDecl, final Integer currentVisibility,
			boolean isTemplate, final boolean isTemplateSpecialization, Printer printer) {
		IASTFunctionDeclarator declarator = functionDef.getDeclarator();
		IASTName name = declarator.getName();
		if (isTemplateSpecialization) {
  		  ICPPASTTemplateId templateId = (ICPPASTTemplateId) name;
  		  name = templateId.getTemplateName();
		}
		if (name.isQualified()) {
			throw new RuntimeException("Unexpected qualified name");
		}
		
		final DeclarationName declarationName = createName(name, functionDef);
		
		printer.println("<function name=" + name.toString() + " isTemplate=" + isTemplate + " isSpecialization=" + isTemplateSpecialization +
				" visibility=" + currentVisibility + " nameLoc=" + getLocationString(name) + " fullDeclaration=" + getLocationString(fullDecl));
		return new FunctionRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitFunction(this);
			}
			@Override public Integer visibility() {
				return currentVisibility;
			}
			@Override public DeclarationName name() {
				return declarationName;
			}
			@Override public TokenRange declaration() {
				return createRange(fullDecl);
			}
			@Override public boolean isTemplateSecondary() {
				return isTemplateSpecialization;
			}
		};
	}
	
	private static DeclarationName createName(final IASTName name, IASTFunctionDefinition functionDef) {
		DeclarationName declarationName;
        if (name instanceof ICPPASTOperatorName) {
			final ICPPASTOperatorName operatorName = (ICPPASTOperatorName) name;
			final boolean isArray = operatorName.getNodeLocations()[0].getNodeLength() == 4 && operatorName.getRawToken(2).getType() == IToken.tLBRACE;
			declarationName = new DeclarationName.Operator() {
				@Override public <R> R accept(Visitor<R> visitor) {
					return visitor.visitOpeartor(this);
				}
				@Override public int codeToken() {
					return operatorName.getNodeLocations()[0].getNodeOffset() + 1;
				}
				@Override public boolean isArray() {
					return isArray;
				}
			};
		} else if (name instanceof ICPPASTConversionName) {
			declarationName = new DeclarationName.Conversion() {
				@Override public <R> R accept(Visitor<R> visitor) {
					return visitor.visitConversion(this);
				}
			};
		} else if (name.getNodeLocations()[0].getNodeLength() == 2 && name.getRawToken(0).getType() == IToken.tBITCOMPLEMENT) {
			declarationName = new DeclarationName.Destructor() {
				@Override public <R> R accept(Visitor<R> visitor) {
					return visitor.visitDestructor(this);
				}
			};
		} else {
			declarationNameDone: {
				if (functionDef != null) {
					IASTDeclSpecifier declSpec = functionDef.getDeclSpecifier();
					if (name.getNodeLocations()[0].getNodeLength() == 1) {
						if (declSpec instanceof IASTSimpleDeclSpecifier) {
							IASTSimpleDeclSpecifier simpleDeclSpecifier = (IASTSimpleDeclSpecifier) declSpec;
							if (simpleDeclSpecifier.getType() == IASTSimpleDeclSpecifier.sc_unspecified) {
								if (functionDef.getParent() instanceof IASTCompositeTypeSpecifier) {
									IASTCompositeTypeSpecifier compositeTypeSpecifier = (IASTCompositeTypeSpecifier) functionDef.getParent();
									String className = compositeTypeSpecifier.getName().getRawSignature();
									String funName = name.getRawSignature();
									if (funName.equals(className)) {
										declarationName = new DeclarationName.Constructor() {
											@Override public <R> R accept(Visitor<R> visitor) {
												return visitor.visitConstructor(this);
											}
										};
										break declarationNameDone;
									}
								}
							}
			    		}
					}
				}
				declarationName = new DeclarationName.Plain() {
					@Override public <R> R accept(Visitor<R> visitor) {
						return visitor.visitPlain(this);
					}
					@Override
					public int token() {
						return createSingleTokenPosition(name);
					}
				};
			}
		}
		return declarationName;
	}
	
	private static UsingRegion dumpUsingDeclaration(final ICPPASTUsingDeclaration usingDecl, final Integer currentVisibility, Printer printer) {
		printer.println("<using declaration visibility=" + currentVisibility + " name=" + usingDecl.getName() + " declaration=" + getLocationString(usingDecl));
		IASTName astName = usingDecl.getName();
		if (astName instanceof ICPPASTQualifiedName) {
			ICPPASTQualifiedName qualifiedName = (ICPPASTQualifiedName) astName;
			astName = qualifiedName.getLastName();
		}
		final DeclarationName name = createName(astName, null);
		return new UsingRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitUsing(this);
			}
			@Override public Integer visibility() {
				return currentVisibility;
			}
			@Override public DeclarationName name() {
				return name;
			}
			@Override public TokenRange declaration() {
				return createRange(usingDecl);
			}
		};
	}
	
	private static UsingRegion dumpUsingDirective(final ICPPASTUsingDirective usingDir, final Integer currentVisibility, Printer printer) {
		printer.println("<using directive visibility=" + currentVisibility + " declaration=" + getLocationString(usingDir));
		return new UsingRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitUsing(this);
			}
			@Override public Integer visibility() {
				return currentVisibility;
			}
			@Override public DeclarationName name() {
				return null;
			}
			@Override public TokenRange declaration() {
				return createRange(usingDir);
			}
		};
	}
	
	private static NamespaceRegion dumpNamespace(ICPPASTNamespaceDefinition ns, Printer printer) {
		final IASTName name = ns.getName();
		printer.println("<namespace name=" + name.getRawSignature() + " nameLoc=" + getLocationString(name));
		final List<? extends Region> inner = dumpDeclarations(ns, printer.getInner());
		printer.println(">");
		return new NamespaceRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitNamespace(this);
			}
			@Override public int nameToken() {
				return createSingleTokenPosition(name);
			}
			@Override public List<? extends Region> innerRegions() {
				return inner;
			}
		};
	}

	private static Collection<? extends Region> dumpTypeItem(final IASTDeclaration d, final Integer currentVisibility, Printer printer) {
		if (d instanceof IASTSimpleDeclaration) {
			IASTSimpleDeclaration simpleDeclaration = (IASTSimpleDeclaration) d;
			IASTDeclSpecifier sp = simpleDeclaration.getDeclSpecifier();
			if (sp instanceof ICPPASTCompositeTypeSpecifier) {
				final ICPPASTCompositeTypeSpecifier composite = (ICPPASTCompositeTypeSpecifier) sp;
				if (!composite.getName().toString().isEmpty()) {
					printer.println("<class name=" + composite.getName().getRawSignature() + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(composite.getName()) + " declaration=" + getLocationString(d) + " classLoc=" + getLocationString(composite));
					if (simpleDeclaration.getDeclarators().length > 0) {
						throw new RuntimeException("Unexpected declaration");
					}
					ClassRegion classRegion = dumpAndCreateClassRegion(currentVisibility, d, composite, false, false, printer);
					printer.println(">");
					return Collections.singletonList(classRegion);
				}
			} else if (sp instanceof IASTInternalEnumerationSpecifier) {
				IASTEnumerationSpecifier enumerationSpecifier = (IASTEnumerationSpecifier) sp;
				IASTName name = enumerationSpecifier.getName();
				if (!name.toString().isEmpty()) {
					final int nameToken = createSingleTokenPosition(name);
					final List<Integer> elements = new ArrayList<Integer>();
					printer.println("<enum name=" + enumerationSpecifier.getName().getRawSignature() + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(enumerationSpecifier.getName()) + " declaration=" + getLocationString(d));
	                Printer inner = printer.getInner();
					for (IASTEnumerator el : enumerationSpecifier.getEnumerators()) {
						inner.println("<enumElement name=" + el.getName().getRawSignature() + " nameLoc=" + getLocationString(el.getName()));
						elements.add(createSingleTokenPosition(el.getName()));
					}
					printer.println(">");
					
					if (simpleDeclaration.getDeclarators().length > 0) {
						throw new RuntimeException("Unexpected declaration");
					}
					EnumRegion enumRegion = new EnumRegion() {
						@Override public <T> T accept(Visitor<T> visitor) {
							return visitor.visitEnum(this);
						}
						@Override public Integer visibility() {
							return currentVisibility;
						}
						@Override public int nameToken() {
							return nameToken;
						}
						@Override public List<? extends Integer> elementNames() {
							return elements;
						}
						@Override public TokenRange declaration() {
							return createRange(d);
						}
					};
					return Collections.singletonList(enumRegion);
				}
			}
			IASTDeclarator[] decls = simpleDeclaration.getDeclarators();
			List<Region> regionList = new ArrayList<Region>(decls.length);
			if (decls.length != 1) {
				return createErrorRegions(d, "Multiple declarators (" + decls.length + ")", printer);
			}
			final IASTDeclarator dd = decls[0];
			printer.println("<declaration name=" + dd.getName().getRawSignature() + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(dd.getName()) + ", declaration=" + getLocationString(d) + " declarator=" + getLocationString(dd) + ">");
			DeclarationRegion typedefRegion = new DeclarationRegion() {
				@Override public <T> T accept(Visitor<T> visitor) {
					return visitor.visitDeclaration(this);
				}
				@Override public Integer visibility() {
					return currentVisibility;
				}
				@Override public Integer nameToken() {
					return createSingleTokenPosition(dd.getName());
				}
				@Override public TokenRange declaration() {
					return createRange(d);
				}
				@Override public boolean isTemplateSecondary() {
					return false;
				}
			};
			regionList.add(typedefRegion);
			return regionList;
		} else if (d instanceof ICPPASTTemplateDeclaration) {
			ICPPASTTemplateDeclaration templateDeclaration = (ICPPASTTemplateDeclaration) d;
			boolean isSpecialization = templateDeclaration instanceof ICPPASTTemplateSpecialization;
			IASTDeclaration internalDeclaration = templateDeclaration.getDeclaration();
			if (internalDeclaration instanceof IASTFunctionDefinition) {
				IASTFunctionDefinition functionDef = (IASTFunctionDefinition) internalDeclaration; 
				FunctionRegion region = dumpFunctionDefinition(functionDef, d, currentVisibility, true, isSpecialization, printer);
				return Collections.singletonList(region);
			} else if (internalDeclaration instanceof IASTSimpleDeclaration) {
				IASTSimpleDeclaration simpleDeclaration = (IASTSimpleDeclaration) internalDeclaration;
				IASTDeclSpecifier sp = simpleDeclaration.getDeclSpecifier();
				if (simpleDeclaration.getDeclarators().length != 0) {
					return createErrorRegions(internalDeclaration, "Unexpected declarators", printer);
				}
				ICPPASTCompositeTypeSpecifier composite = (ICPPASTCompositeTypeSpecifier) sp;
				printer.println("<templateClass name=" + composite.getName().getRawSignature() + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(composite.getName()) + " declaration=" + getLocationString(d) + " classLoc=" + getLocationString(composite) + " isSpecialization=" + isSpecialization);
				ClassRegion classRegion = dumpAndCreateClassRegion(currentVisibility, d, composite, true, isSpecialization, printer);
				printer.println(">");
				return Collections.singletonList(classRegion);
			} else {
				return createErrorRegions(d, "Unrecognized template", printer);
			}
		} else if (d instanceof ICPPASTExplicitTemplateInstantiation) {
			printer.println("<templateInstantiation visibility=" + currentVisibility + " declaration=" + getLocationString(d) + ">");
			DeclarationRegion declarationRegion = new DeclarationRegion() {
				@Override public <T> T accept(Visitor<T> visitor) {
					return visitor.visitDeclaration(this);
				}
				@Override public Integer visibility() {
					return currentVisibility;
				}
				@Override public Integer nameToken() {
					return null;
				}
				@Override public TokenRange declaration() {
					return createRange(d);
				}
				@Override public boolean isTemplateSecondary() {
					return true;
				}
			};
			return Collections.singletonList(declarationRegion);
		}
		return null;
	}
	
	private static ErrorRegion createErrorRegion(IASTNode node, String message, Printer printer) {
		ErrorRegion region = new ErrorRegion.Impl(message, createRange(node));
		printer.println("<errorRegion range=" + getLocationString(node) + " message='" + message + "'>");
		return region;
	}
	
	private static Collection<? extends ErrorRegion> createErrorRegions(IASTNode node, String message, Printer printer) {
		return Collections.singletonList(createErrorRegion(node, message, printer));
	}
	
	private static NonTypeRegion dumpOther(Integer visibility, final int begin, final int end, Printer printer) {
		if (begin == end) {
			return null;
		}
		TokenRange range = new TokenRange() {
			@Override public int start() {
				return begin;
			}
			@Override public int end() {
				return end;
			}
		};
		return dumpNonType(visibility, range, printer);
	}
	
	private static NonTypeRegion dumpNonType(final Integer visibility, final TokenRange range, Printer printer) {
		printer.println("<otherDeclarations begin=" + range.start() + "+" + (range.end()-range.start()) + " visibility="+ visibility + ">");
		return new NonTypeRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitNonType(this);
			}
			@Override public Integer visibility() {
				return visibility;
			}
			@Override public TokenRange range() {
				return range;
			}
		};
	}
	
	private static List<? extends Region> dumpClassDefiniton(ICPPASTCompositeTypeSpecifier composite, Printer printer) {
		return dumpDeclarations(composite, printer);
	}
	
	private static ClassRegion dumpAndCreateClassRegion(final Integer currentVisibility, final IASTDeclaration d, final ICPPASTCompositeTypeSpecifier composite,
			final boolean isTemplate, final boolean isTemplateSpecialization, Printer printer) {
		final List<? extends Region> innerRegions = dumpClassDefiniton(composite, printer.getInner());
		IASTName name = composite.getName();
		if (isTemplateSpecialization) {
  		  ICPPASTTemplateId templateId = (ICPPASTTemplateId) name;
  		  name = templateId.getTemplateName();
		}
		final IASTName nameFinal = name;
			
		return new ClassRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitClass(this);
			}
			@Override public Integer visibility() {
				return currentVisibility;
			}
			@Override public int nameToken() {
				return createSingleTokenPosition(nameFinal);
			}
			@Override public boolean isTemplate() {
				return isTemplate;
			}
			@Override public List<? extends Region> innerRegions() {
				return innerRegions;
			}
			@Override public TokenRange declaration() {
				return createRange(d);
			}
			@Override public TokenRange classTokens() {
				return createRange(composite);
			}
			@Override public boolean isTemplateSecondary() {
				return isTemplateSpecialization;
			}
		};
	}
	
	private static String getLocationString(IASTNode node) {
		IASTNodeLocation[] nodeLocations = node.getNodeLocations();
		IASTNodeLocation loc = nodeLocations[0];
		return loc.getNodeOffset() + "+" + loc.getNodeLength();
	}
	
	private static TokenRange createRange(IASTNode node) {
		IASTNodeLocation[] nodeLocations = node.getNodeLocations();
		final IASTNodeLocation loc = nodeLocations[0];
		return new TokenRange() {
			@Override public int start() {
				return loc.getNodeOffset();
			}
			@Override public int end() {
				return loc.getNodeOffset() + loc.getNodeLength();
			}
		};
	}
	private static int createSingleTokenPosition(IASTNode node) {
		IASTNodeLocation[] nodeLocations = node.getNodeLocations();
		IASTNodeLocation loc = nodeLocations[0];
		if (loc.getNodeLength() != 1) {
			throw new RuntimeException("Length should be 1");
		}
		return loc.getNodeOffset();
	}
	
	static class Printer {
		private final String offset;
		private final boolean isMute;
		Printer(String offset, boolean isMute) {
			this.offset = offset;
			this.isMute = isMute;
		}
		void println(String text) {
			if (!isMute) {
				System.out.println(offset + text);
			}
		}
		Printer getInner() {
			if (isMute) {
				return this;
			} else {
				return new Printer(offset + "  ", false);
			}
		}
	}
}
