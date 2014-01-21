package ru.spb.rybin.clangroger.overviewparser;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationName.Constructor;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationName.Conversion;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationName.Destructor;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationName.Operator;
import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.DeclarationName.Plain;

class OverviewWriter {
  interface File {
	  <T> T accept(Visitor<T> visitor);
	  interface Visitor<T> {
		  T visitData(DataFile dataFile);
		  T visitError(ErrorFile errorFile);
	  }
  }
  
  interface DataFile extends File {
	  List<? extends Region> regions();
  }
  
  interface ErrorFile extends File {
	  String message();
  }
  
  interface Region {
	  <T> T accept(Visitor<T> visitor);
	  interface Visitor<T> {
		  T visitNonType(NonTypeRegion nonTypeRegion);
		  T visitDeclaration(DeclarationRegion declarationRegion);
		  T visitClass(ClassRegion classRegion);
          T visitFunction(FunctionRegion functionRegion);
          T visitNamespace(NamespaceRegion namespaceRegion);
          T visitUsing(UsingRegion usingRegion);
          T visitEnum(EnumRegion enumRegion);
          T visitError(ErrorRegion errorRegion);
	  }
  }
  
  interface NonTypeRegion extends Region {
	  Integer visibility();
	  TokenRange range();
  }
  
  interface DeclarationRegion extends Region {
	  Integer visibility();
	  // null means unnamed, means template instantiation.
	  Integer nameToken();
	  TokenRange declaration();
	  boolean isTemplateSecondary();
  }
  
  interface FunctionRegion extends Region {
	  Integer visibility();
	  DeclarationName name();
	  TokenRange declaration();
	  boolean isTemplateSecondary();
  }

  interface ClassRegion extends Region {
	  Integer visibility();
	  int nameToken();
	  boolean isTemplate();
	  TokenRange declaration();
	  TokenRange classTokens();
	  List<? extends Region> innerRegions();
	  boolean isTemplateSecondary();
  }
  
  interface NamespaceRegion extends Region {
	  int nameToken();
	  List<? extends Region> innerRegions();
  }
  
  interface UsingRegion extends Region {
	  Integer visibility();
	  DeclarationName name();
	  TokenRange declaration();
  }
  interface EnumRegion extends Region {
	  int nameToken();
	  Integer visibility();
	  List<? extends Integer> elementNames(); 
	  TokenRange declaration();
  }
  
  interface ErrorRegion extends Region {
	  TokenRange declaration();
	  String errorMessage();
	  
	  class Impl implements ErrorRegion {
		private final String message;
		private final TokenRange range;
		
		public Impl(String message, TokenRange range) {
			this.message = message;
			this.range = range;
		}
		
		@Override public <T> T accept(Visitor<T> visitor) {
			return visitor.visitError(this);
		}
		@Override public TokenRange declaration() {
			return range;
		}
		@Override public String errorMessage() {
			return message;
		}
	  }
  }

  interface DeclarationName {
	  interface Plain extends DeclarationName {
		  int token();
	  }
	  interface Operator extends DeclarationName {
		  int codeToken();
		  boolean isArray();
	  }
	  interface Conversion extends DeclarationName {
	  }
	  interface Constructor extends DeclarationName {
	  }
	  interface Destructor extends DeclarationName {
	  }
	  
	  interface Visitor<R> {
		  R visitPlain(Plain plain);
		  R visitOpeartor(Operator operator);
		  R visitConversion(Conversion conversion);
		  R visitConstructor(Constructor constructor);
		  R visitDestructor(Destructor destructor);
	  }
	  <R> R accept(Visitor<R> visitor);
  }
  
  interface TokenRange {
	  int start();
	  int end();
  }

  static void write(File data, final OutputStream output) {
	class Recursion {
		void go(File file) {
			file.accept(new File.Visitor<Void>() {
				@Override public Void visitData(DataFile dataFile) {
					writeDataFile(dataFile);
					return null;
				}
				@Override public Void visitError(ErrorFile errorFile) {
					writeErrorFile(errorFile);
					return null;
				}
			});
		}
		private void writeDataFile(DataFile data) {
			writeByte((byte) 0); 
			for (Region r : data.regions()) {
				writeRegion(r);
			}
		}
		private void writeErrorFile(ErrorFile errorFile) {
			writeByte((byte) 1); 
			String message = errorFile.message();
			writeString(message);
		}
		private void writeString(String s) {
			byte[] bb = s.getBytes();
			writeInt16(bb.length);
			writeBytes(bb);
		}
		private void writeRegion(Region r) {
			r.accept(new Region.Visitor<Void>() {
				@Override
				public Void visitNonType(NonTypeRegion nonTypeRegion) {
					writeNonType(nonTypeRegion);
					return null;
				}

				@Override
				public Void visitDeclaration(DeclarationRegion typedefRegion) {
					writeDeclaration(typedefRegion);
					return null;
				}
				
				@Override
				public Void visitFunction(FunctionRegion functionRegion) {
					writeFunction(functionRegion);
					return null;
				}

				@Override
				public Void visitClass(ClassRegion classRegion) {
					writeClass(classRegion);
					return null;
				}
				
				@Override
				public Void visitNamespace(NamespaceRegion namespaceRegion) {
					writeNamespace(namespaceRegion);
					return null;
				}
				
				@Override
				public Void visitUsing(UsingRegion usingRegion) {
					writeUsing(usingRegion);
					return null;
				}
				@Override
				public Void visitEnum(EnumRegion enumRegion) {
					writeEnum(enumRegion);
					return null;
				}
				@Override
				public Void visitError(ErrorRegion errorRegion) {
					writeError(errorRegion);
					return null;
				}
			});
		}
		private void writeNonType(NonTypeRegion nonTypeRegion) {
			writeByte((byte) 1);
			writeVisibility(nonTypeRegion.visibility());
			writeRange(nonTypeRegion.range());
		}
		private void writeDeclaration(DeclarationRegion declarationRegion) {
			writeByte((byte) 2);
			writeVisibility(declarationRegion.visibility());
			Integer namePos = declarationRegion.nameToken();
			int codedNamePos;
			if (namePos == null) {
				codedNamePos = 0;
			} else {
				codedNamePos = namePos;
				if (codedNamePos == 0) {
					throw new RuntimeException("Unsupported code for name position");
				}
			}
			writeInt16(codedNamePos);
			writeRange(declarationRegion.declaration());
			writeByte((byte)(declarationRegion.isTemplateSecondary() ? 1 : 0));
		}
		private void writeClass(ClassRegion classRegion) {
			writeByte((byte) 3);
			writeVisibility(classRegion.visibility());
			writeInt16(classRegion.nameToken());
			writeRange(classRegion.declaration());
			writeRange(classRegion.classTokens());
			writeBool(classRegion.isTemplateSecondary());
			
			for (Region r : classRegion.innerRegions()) {
				writeRegion(r);
			}
			writeByte((byte) 0);
		}
		private void writeFunction(FunctionRegion functionRegion) {
			writeByte((byte) 4);
			writeVisibility(functionRegion.visibility());
			writeDeclarationName(functionRegion.name());
			writeRange(functionRegion.declaration());
			writeByte((byte)(functionRegion.isTemplateSecondary() ? 1 : 0));
		}
		private void writeNamespace(NamespaceRegion namespaceRegion) {
			writeByte((byte) 5);
			writeInt16(namespaceRegion.nameToken());
			for (Region r : namespaceRegion.innerRegions()) {
				writeRegion(r);
			}
			writeByte((byte) 0);
		}
		
		private void writeUsing(UsingRegion usingRegion) {
			writeByte((byte) 6);
			writeVisibility(usingRegion.visibility());
			if (usingRegion.name() == null) {
				writeBool(false);
			} else {
				writeBool(true);
				writeDeclarationName(usingRegion.name());
			}
			writeRange(usingRegion.declaration());
		}

		private void writeError(ErrorRegion errorRegion) {
			writeByte((byte) 7);
			writeRange(errorRegion.declaration());
			writeString(errorRegion.errorMessage());
		}

		private void writeEnum(EnumRegion enumRegion) {
			writeByte((byte) 8);
			writeVisibility(enumRegion.visibility());
			writeInt16(enumRegion.nameToken());
			for (Integer elName : enumRegion.elementNames()) {
				if (elName == 0) {
					throw new RuntimeException("Null enum token");
				}
				writeInt16(elName);
			}
			writeInt16(0);
			writeRange(enumRegion.declaration());
		}
		
		private void writeDeclarationName(DeclarationName declarationName) {
			declarationName.accept(new DeclarationName.Visitor<Void>() {
				@Override
				public Void visitPlain(Plain plain) {
					writeByte((byte) 0);
					writeInt16(plain.token());
					return null;
				}

				@Override
				public Void visitOpeartor(Operator operator) {
					int code = operator.isArray() ? 2 : 1;
					writeByte((byte) code);
					writeInt16(operator.codeToken());
					return null;
				}

				@Override
				public Void visitConversion(Conversion conversion) {
					writeByte((byte) 3);
					return null;
				}

				@Override
				public Void visitConstructor(Constructor constructor) {
					writeByte((byte) 4);
					return null;
				}

				@Override
				public Void visitDestructor(Destructor destructor) {
					writeByte((byte) 5);
					return null;
				}
			});
		}
		
		private void writeVisibility(Integer visibility) {
			int code = visibility == null ? 0 : visibility.intValue();
			writeByte((byte) code);
		}
		private void writeRange(TokenRange range) {
			writeInt16(range.start());
			writeInt16(range.end());
		}
		
		private void writeInt16(int n) {
			if (n < 0) {
				throw new RuntimeException("Too small: " + n);
			}
			if (n > 0xFFFF) {
				throw new RuntimeException("Too big: " + n);
			}
			byte b1 = (byte) (n & 0xFF);
			byte b2 = (byte) ((n>>8) & 0xFF);
			writeByte(b1);
			writeByte(b2);
		}
		private void writeByte(byte b) {
			try {
				output.write(b);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		private void writeBool(boolean b) {
			writeByte((byte)(b ? 1 : 0));
		}
		private void writeBytes(byte[] bytes) {
			try {
				output.write(bytes);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}
	new Recursion().go(data);
  }
}
