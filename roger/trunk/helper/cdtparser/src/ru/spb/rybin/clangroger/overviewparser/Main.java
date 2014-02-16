package ru.spb.rybin.clangroger.overviewparser;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.model.ILexedContent;
import org.eclipse.cdt.core.parser.IParserLogService;

import ru.spb.rybin.clangroger.overviewparser.OverviewWriter.File;
import ru.spb.rybin.eclipsereplacement.CoreException;

public class Main {
	public static void main(String[] args) {
		CommandLine commandLine = parseCommandLine(args);
		try {
			go(commandLine);
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	private interface CommandLine {
		String input();
		String output();
		boolean isVerbose();
	}
	
	private static void go(CommandLine commandLine) throws IOException, CoreException {
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
		
		FileInputStream stream = new FileInputStream(commandLine.input());
		TokenReader tokenReader = new TokenReader(stream);
		ILexedContent lexedContent = tokenReader.read();
		stream.close();
		
		IASTTranslationUnit unit = language.getASTTranslationUnit(lexedContent, index, options, log);
		int textLength = lexedContent.size(); 
		
		File fileData;
		try {
			fileData = AstConverter.createOutputMode(unit, textLength, new AstConverter.Printer("", !commandLine.isVerbose()));
		} catch (Exception e) {
			ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
			e.printStackTrace(new PrintStream(byteStream));
			fileData = AstConverter.createErrorFile(new String(byteStream.toByteArray()));
		}
		FileOutputStream outputStream = new FileOutputStream(commandLine.output());
		OverviewWriter.write(fileData, outputStream);
		outputStream.close();
	}
	
	private static CommandLine parseCommandLine(String[] args) {
		class Data implements CommandLine {
			List<String> args = new ArrayList<String>(2);
			boolean verbose = false;
			@Override public String input() {
				return args.get(0);
			}
			@Override public String output() {
				return args.get(1);
			}
			@Override public boolean isVerbose() {
				return verbose;
			}
		}
		Data data = new Data();
		int pos = 0;
		while (pos < args.length) {
			String s = args[pos];
			pos++;
			if (s.startsWith("-")) {
				if ("-v".equals(s) || "--verbose".equals(s)) {
					data.verbose = true;
				} else {
					throw new IllegalArgumentException("Unknown option " + s);
				}
			} else {
				data.args.add(s);
			}
		}
		if (data.args.size() < 2) {
			throw new IllegalArgumentException("Too few arguments, must be 2");
		}
		if (data.args.size() > 2) {
			throw new IllegalArgumentException("Too many arguments, must be 2");
		}
		return data;
	}
}
