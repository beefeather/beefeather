package org.eclipse.cdt.internal.core.parser.scanner;

import java.nio.CharBuffer;
import java.util.Collections;
import java.util.Map;

import org.eclipse.cdt.core.dom.ast.IASTComment;
import org.eclipse.cdt.core.dom.ast.IASTFileLocation;
import org.eclipse.cdt.core.dom.ast.IASTImageLocation;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNodeLocation;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorIncludeStatement;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorMacroDefinition;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorMacroExpansion;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorStatement;
import org.eclipse.cdt.core.dom.ast.IASTProblem;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.IMacroBinding;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit.IDependencyTree;
import org.eclipse.cdt.core.model.ILexedContent;
import org.eclipse.cdt.core.parser.EndOfFileException;
import org.eclipse.cdt.core.parser.IScanner;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.core.parser.IncludeExportPatterns;
import org.eclipse.cdt.core.parser.OffsetLimitReachedException;
import org.eclipse.cdt.internal.core.dom.parser.ASTNodeSpecification;
import org.eclipse.cdt.internal.core.parser.scanner.Lexer.LexerOptions;

public class LexedContentReader implements IScanner {
	private final Object sourceObject = new Object(){};
	private int pos = 0;
	private final Token[] tokens;
	public LexedContentReader(ILexedContent lexedContent) {
		Token[] array = new Token[lexedContent.size()];
		for (int i = 0; i < array.length; i++) {
			String s = lexedContent.getText(i);
			Token t;
			if (s == null) {
				t = new Token(lexedContent.getType(i), sourceObject, i, i + 1);
			} else {
				t = new TokenWithImage(lexedContent.getType(i), sourceObject, i, i + 1, s.toCharArray());
			}
			array[i] = t;
		}
		tokens = array;
	}

	@Override
	public Map<String, IMacroBinding> getMacroDefinitions() {
		return Collections.emptyMap();
	}

	@Override
	public IToken nextToken() throws EndOfFileException {
		if (pos < tokens.length - 1) {
    		return tokens[pos++];
		} else {
			throw new EndOfFileException(pos);
		}
	}

	@Override
	public boolean isOnTopContext() {
		return true;
	}

	@Override
	public void cancel() {
	}

	@Override
	public ILocationResolver getLocationResolver() {
		return new ILocationResolver() {
			@Override
			public void setRootNode(IASTTranslationUnit tu) {
			}
			
			@Override
			public IASTFileLocation getMappedFileLocation(final int offset, final int length) {
				return new LocationImpl(offset, length); 
			}
			
			@Override
			public String getContainingFilePath(int sequenceNumber) {
				return "Prelexed content path";
			}
			
			@Override
			public char[] getUnpreprocessedSignature(IASTFileLocation loc) {
				if (loc.getNodeLength() == 1) {
					return tokens[loc.getNodeOffset()].getCharImage();
				} else {
					StringBuilder builder = new StringBuilder();
					for (int i = 0; i < loc.getNodeLength(); i++) {
						if (i != 0) {
							builder.append(' ');
						}
						builder.append(tokens[loc.getNodeOffset() + i].getCharImage());
					}
					return builder.toString().toCharArray();
				}
			}
			
			@Override
			public IToken getUnpreprocessedToken(IASTFileLocation loc, int offset) {
				return tokens[loc.getNodeOffset() + offset];
			}
			
			@Override
			public IASTNodeLocation[] getLocations(int sequenceNumber,
					int length) {
			    return new IASTNodeLocation[] { new LocationImpl(sequenceNumber, length) };
			}
		};
		
	}
	static class LocationImpl implements IASTFileLocation {
		private final int offset;
		private final int length;
		
		LocationImpl(int offset, int length) {
			this.offset = offset;
			this.length = length;
		}

		@Override
		public int getNodeOffset() {
			return offset;
		}
		
		@Override
		public int getNodeLength() {
			return length;
		}
		@Override
		public String getFileName() {
			return "prelexed content";
		}
		
		@Override
		public int getStartingLineNumber() {
			return 0;
		}
		
		@Override
		public int getEndingLineNumber() {
			return 0;
		}
	}

	@Override
	public void setContentAssistMode(int offset) {
	}

	@Override
	public void setSplitShiftROperator(boolean val) {
	}

	@Override
	public void setComputeImageLocations(boolean val) {
	}

	@Override
	public void setTrackIncludeExport(IncludeExportPatterns patterns) {
	}

	@Override
	public void setProcessInactiveCode(boolean val) {
	}

	@Override
	public void skipInactiveCode() throws OffsetLimitReachedException {
	}

	@Override
	public int getCodeBranchNesting() {
		return 0;
	}

	@Override
	public void setScanComments(boolean val) {
	}

}
