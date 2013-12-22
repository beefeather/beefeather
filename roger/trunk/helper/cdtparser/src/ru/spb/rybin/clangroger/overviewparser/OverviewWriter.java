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
			writeInt16(message.length());
			writeBytes(message.getBytes());
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
			writeByte((byte) (declarationRegion.isType() ? 1 : 0));
			writeInt16(declarationRegion.nameToken());
			writeRange(declarationRegion.declaration());
			writeRange(declarationRegion.declarator());
			writeInt16(declarationRegion.declaratorNumber());
		}
		private void writeClass(ClassRegion classRegion) {
			writeByte((byte) 3);
			writeVisibility(classRegion.visibility());
			writeInt16(classRegion.nameToken());
			writeRange(classRegion.declaration());
			writeRange(classRegion.classTokens());
			
			for (Region r : classRegion.innerRegions()) {
				writeRegion(r);
			}
			writeByte((byte) 0);
		}
		private void writeFunction(FunctionRegion functionRegion) {
			writeByte((byte) 4);
			writeVisibility(functionRegion.visibility());
			functionRegion.name().accept(new DeclarationName.Visitor<Void>() {
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
			writeRange(functionRegion.declaration());
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
			writeRange(usingRegion.declaration());
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
	  }
  }
  
  interface NonTypeRegion extends Region {
	  Integer visibility();
	  TokenRange range();
  }
  
  interface DeclarationRegion extends Region {
	  Integer visibility();
	  boolean isType();
	  int nameToken();
	  TokenRange declaration();
	  TokenRange declarator();
	  int declaratorNumber();
  }
  
  interface FunctionRegion extends Region {
	  Integer visibility();
	  DeclarationName name();
	  TokenRange declaration();
  }

  interface ClassRegion extends Region {
	  Integer visibility();
	  int nameToken();
	  boolean isTemplate();
	  TokenRange declaration();
	  TokenRange classTokens();
	  List<? extends Region> innerRegions();
  }
  
  interface NamespaceRegion extends Region {
	  int nameToken();
	  List<? extends Region> innerRegions();
  }
  
  interface UsingRegion extends Region {
	  Integer visibility();
	  TokenRange declaration();
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
}
