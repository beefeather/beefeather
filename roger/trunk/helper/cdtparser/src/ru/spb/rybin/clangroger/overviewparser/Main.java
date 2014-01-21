package ru.spb.rybin.clangroger.overviewparser;

import java.io.ByteArrayOutputStream;
import java.io.CharArrayWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;

import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.model.ILexedContent;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.parser.EmptyFilesProvider;

import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.File;
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
		
		File fileData;
		try {
			fileData = AstConverter.createOutputMode(unit, textLength, new AstConverter.Printer(""));
		} catch (Exception e) {
			ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
			e.printStackTrace(new PrintStream(byteStream));
			fileData = AstConverter.createErrorFile(new String(byteStream.toByteArray()));
		}
		FileOutputStream outputStream = new FileOutputStream(outputFileName);
		OverviewWriter.write(fileData, outputStream);
		outputStream.close();
	}
}
