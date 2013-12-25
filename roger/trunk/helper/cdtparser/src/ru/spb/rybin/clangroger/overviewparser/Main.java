package ru.spb.rybin.clangroger.overviewparser;

import java.io.CharArrayWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.cdt.core.dom.ast.ExpansionOverlapsBoundaryException;
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTDeclarationListOwner;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTOperatorName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVisibilityLabel;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.model.ILexedContent;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.parser.EmptyFilesProvider;

import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.*;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.Region.Visitor;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.TokenRange;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.UsingRegion;
import ru.spb.rybin.eclipsereplacement.CoreException;

public class Main {
	
	public static void main(String[] args) {
		String inputFileName = args[0];
		String outputFileName = args[1];
		boolean oldCode = false;
		try {
			go(inputFileName, oldCode, outputFileName);
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	private static void go(String inputFileName, boolean oldCode, String outputFileName) throws IOException, CoreException {
		GPPLanguage language = GPPLanguage.getDefault();
		
		IIndex index = null;
		int options = GPPLanguage.OPTION_SKIP_FUNCTION_BODIES;
		IParserLogService log = new IParserLogService() {
			@Override public void traceLog(String message) {
				System.out.println("Trace: " + message);
			}
			@Override public boolean isTracing() {
				return true;
			}
		};

		IASTTranslationUnit unit;
		int textLength;
		if (oldCode) {
			FileContent reader;
			{
				CharArrayWriter charArrayWriter = new CharArrayWriter();
				FileInputStream stream = new FileInputStream(inputFileName);
				stream.close();
				
				FileReader inputFile = new FileReader(inputFileName);
				
				char[] buffer = new char[1024];
				while (true) {
					int res = inputFile.read(buffer);
					if (res == -1) {
						break;
					}
					charArrayWriter.write(buffer, 0, res);
				}
				inputFile.close();
				char[] charArray = charArrayWriter.toCharArray();
				textLength = charArray.length;
				reader = FileContent.create(inputFileName, charArray);
			}
			
			IScannerInfo scannerInfo = new ScannerInfo();
			unit = language.getASTTranslationUnit(reader, scannerInfo, EmptyFilesProvider.getInstance(), index, options, log);
		} else {
			FileInputStream stream = new FileInputStream(inputFileName);
			TokenReader tokenReader = new TokenReader(stream);
			ILexedContent lexedContent = tokenReader.read();
			stream.close();
			
			unit = language.getASTTranslationUnit(lexedContent, index, options, log);
			textLength = lexedContent.size(); 
		}
		
		File fileData = createOutputMode(unit, textLength, new Printer(""));
		FileOutputStream outputStream = new FileOutputStream(outputFileName);
		OverviewWriter.write(fileData, outputStream);
		outputStream.close();
	}
	
	private static File createOutputMode(IASTTranslationUnit unit, int textLength, Printer printer) {
		if (false) {
			return createErrorFile("Hello from CDT");
		}
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
	
	private static File createErrorFile(final String message) {
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
					NamespaceRegion namespaceRegion = dumpNamespace(namespaceDefinition, printer);
					regions.add(namespaceRegion);
					other = false;
				} else if (d instanceof ICPPASTVisibilityLabel) {
					ICPPASTVisibilityLabel visibilityLabel = (ICPPASTVisibilityLabel) d;
					newVisibility = visibilityLabel.getVisibility();
					other = false;
				} else if (d instanceof IASTFunctionDefinition) {
					IASTFunctionDefinition functionDef = (IASTFunctionDefinition) d;
					FunctionRegion region = dumpFunctionDefinition(functionDef, currentVisibility, printer);
					regions.add(region);
					other = false;
				} else if (d instanceof ICPPASTUsingDeclaration) {
					ICPPASTUsingDeclaration usingDecl = (ICPPASTUsingDeclaration) d;
					UsingRegion region = dumpUsingDeclaration(usingDecl, currentVisibility, printer);
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
	
	private static FunctionRegion dumpFunctionDefinition(final IASTFunctionDefinition functionDef, final Integer currentVisibility, Printer printer) {
		IASTFunctionDeclarator declarator = functionDef.getDeclarator();
		final IASTName name = declarator.getName();
		if (name.isQualified()) {
			throw new RuntimeException("Unexpected qualified name");
		}
		
		final DeclarationName declarationName;
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
		
		printer.println("<function name=" + name.toString() + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(name) + " declaration=" + getLocationString(functionDef));
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
				return createRange(functionDef);
			}
		};
	}
	
	private static UsingRegion dumpUsingDeclaration(final ICPPASTUsingDeclaration usingDecl, final Integer currentVisibility, Printer printer) {
		printer.println("<using visibility=" + currentVisibility + " declaration=" + getLocationString(usingDecl));
		return new UsingRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitUsing(this);
			}
			@Override public Integer visibility() {
				return currentVisibility;
			}
			@Override public TokenRange declaration() {
				return createRange(usingDecl);
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
					ClassRegion classRegion = dumpAndCreateClassRegion(currentVisibility, d, composite, false, printer);
					printer.println(">");
					return Collections.singletonList(classRegion);
				}
			}
			final boolean isTypeFinal = (sp.getStorageClass() & IASTDeclSpecifier.sc_typedef) != 0;  
			IASTDeclarator[] decls = simpleDeclaration.getDeclarators();
			List<Region> regionList = new ArrayList<Region>(decls.length);
			for (int i = 0; i < decls.length; i++) {
				final IASTDeclarator dd = decls[i];
				final int pos = i;
				printer.println("<declaration name=" + dd.getName().getRawSignature() + " isType=" + isTypeFinal + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(dd.getName()) + ", declaratorIndex=" + i + ", declaration=" + getLocationString(d) + " declarator=" + getLocationString(dd) + ">");
				DeclarationRegion typedefRegion = new DeclarationRegion() {
					@Override public <T> T accept(Visitor<T> visitor) {
						return visitor.visitDeclaration(this);
					}
					@Override public Integer visibility() {
						return currentVisibility;
					}
					@Override public int nameToken() {
						return createSingleTokenPosition(dd.getName());
					}
					@Override public boolean isType() {
						return isTypeFinal;
					}
					@Override public int declaratorNumber() {
						return pos;
					}
					@Override public TokenRange declarator() {
						return createRange(dd);
					}
					@Override public TokenRange declaration() {
						return createRange(d);
					}
				};
				regionList.add(typedefRegion);
			}
			return regionList;
		} else if (d instanceof ICPPASTTemplateDeclaration) {
			ICPPASTTemplateDeclaration templateDeclaration = (ICPPASTTemplateDeclaration) d;
			IASTDeclaration internalDeclaration = templateDeclaration.getDeclaration();
			if (internalDeclaration instanceof IASTSimpleDeclaration) {
				IASTSimpleDeclaration simpleDeclaration = (IASTSimpleDeclaration) internalDeclaration;
				IASTDeclSpecifier sp = simpleDeclaration.getDeclSpecifier();
				if (sp instanceof ICPPASTCompositeTypeSpecifier) {
					ICPPASTCompositeTypeSpecifier composite = (ICPPASTCompositeTypeSpecifier) sp;
					printer.println("<templateClass name=" + composite.getName().getRawSignature() + " visibility=" + currentVisibility + " nameLoc=" + getLocationString(composite.getName()) + " declaration=" + getLocationString(d) + " classLoc=" + getLocationString(composite));
					if (simpleDeclaration.getDeclarators().length > 0) {
						throw new RuntimeException("Unexpected declaration");
					}
					ClassRegion classRegion = dumpAndCreateClassRegion(currentVisibility, d, composite, true, printer);
					printer.println(">");
					return Collections.singletonList(classRegion);
				}
			}
		}
		return null;
	}
	
	private static NonTypeRegion dumpOther(final Integer visibility, final int begin, final int end, Printer printer) {
		if (begin == end) {
			return null;
		}
		printer.println("<otherDeclarations begin=" + begin + "+" + (end-begin) + " visibility="+ visibility + ">");
		return new NonTypeRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitNonType(this);
			}
			@Override public Integer visibility() {
				return visibility;
			}
			@Override public TokenRange range() {
				return new TokenRange() {
					@Override public int start() {
						return begin;
					}
					@Override public int end() {
						return end;
					}
				};
			}
		};
	}
	
	private static List<? extends Region> dumpClassDefiniton(ICPPASTCompositeTypeSpecifier composite, Printer printer) {
		return dumpDeclarations(composite, printer);
	}
	
	private static ClassRegion dumpAndCreateClassRegion(final Integer currentVisibility, final IASTDeclaration d, final ICPPASTCompositeTypeSpecifier composite, final boolean isTemplate, Printer printer) {
		final List<? extends Region> innerRegions = dumpClassDefiniton(composite, printer.getInner());
		return new ClassRegion() {
			@Override public <T> T accept(Visitor<T> visitor) {
				return visitor.visitClass(this);
			}
			@Override public Integer visibility() {
				return currentVisibility;
			}
			@Override public int nameToken() {
				return createSingleTokenPosition(composite.getName());
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
		Printer(String offset) {
			this.offset = offset;
		}
		void println(String text) {
			System.out.println(offset + text);
		}
		Printer getInner() {
			return new Printer(offset + "  ");
		}
	}
}