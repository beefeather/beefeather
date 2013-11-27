package ru.spb.rybin.clangroger.overviewparser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class ClangToken {
	static final ClangToken INSTANCE = new ClangToken();
	
	public static final String IDENTIFIER_KIND_ID = "identifier";

	static abstract class Kind {
		abstract String getId();
		
		boolean isLiteral() {
			return LiteralsKind.LITERAL_ID_SET.contains(getId()); 
		}
		public String toString() {
			return getId();
		}
	}
	
	Kind get(int pos) {
		return tokenInfoList.get(pos);
	}
	
	private static ArrayList<Kind> buildList() throws IOException {
		InputStream stream = ClangToken.class.getClassLoader().getResourceAsStream("ru/spb/rybin/clangroger/overviewparser/TokenKinds.def");
		Reader reader = new InputStreamReader(stream);
		BufferedReader bufferedReader = new BufferedReader(reader);
		ArrayList<Kind> list = new ArrayList<Kind>();
		TokenLineMatcher[] matchers = createMatchers();
		while (true) {
			String line = bufferedReader.readLine();
			if (line == null) {
				break;
			}
			for (TokenLineMatcher m : matchers) {
				boolean res = m.matchLine(line, list);
				if (res) {
					break;
				}
			}
		}
		Set<String> literalIds = new HashSet<String>(LiteralsKind.LITERAL_ID_LIST);
		for (Kind k : list) {
			literalIds.remove(k.getId());
		}
		if (!literalIds.isEmpty()) {
			throw new RuntimeException("Unknows ids: " + literalIds);
		}
		
		return list;
	}
	
	private ClangToken() {
		try {
			this.tokenInfoList = buildList();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	private List<Kind> tokenInfoList;
	
	private static abstract class TokenLineMatcher {
		abstract boolean matchLine(String line, Collection<? super Kind> output);
	}
	
	private static abstract class MatcherBase extends TokenLineMatcher {
		private final String keyword;
		private final Pattern pattern;
		public MatcherBase(String keyword, String patternText) {
			this.keyword = keyword;
			this.pattern = Pattern.compile("^" + keyword + patternText);
		}
		boolean matchLine(String line, Collection<? super Kind> output) {
			if (!line.startsWith(keyword)) {
				return false;
			}
			Matcher matcher = pattern.matcher(line);
			boolean res = matcher.find();
			if (!res) {
				throw new RuntimeException("Match is expected");
			}
			Kind info = createInfo(matcher);
			output.add(info);
			return true;
		}
		abstract Kind createInfo(Matcher matcher);
	}
	
	private static TokenLineMatcher[] createMatchers() {
		return new TokenLineMatcher[] {
				new MatcherBase("TOK", "\\((\\w+)\\)") {
					@Override
					Kind createInfo(Matcher matcher) {
						final String id = matcher.group(1);
						return new Kind() {
							@Override String getId() {
								return id;
							}
						};
					}
				},
				new MatcherBase("PUNCTUATOR", "\\((\\w+)\\s*,\\s*\\\"(.+)\\\"\\)") {
					@Override
					Kind createInfo(Matcher matcher) {
						final String id = matcher.group(1);
						final String visible = matcher.group(2);
						return new Kind() {
							@Override String getId() {
								return id;
							}
						};
					}
				},
				new MatcherBase("KEYWORD", "\\((\\w+)\\s*,\\s*([\\w| ]+)\\)") {
					@Override
					Kind createInfo(Matcher matcher) {
						final String id = matcher.group(1);
						final String flags = matcher.group(2);
						return new Kind() {
							@Override String getId() {
								return id;
							}
						};
					}
				},
				new MatcherBase("ANNOTATION", "\\((\\w+)\\)") {
					@Override
					Kind createInfo(Matcher matcher) {
						final String id = matcher.group(1);
						return new Kind() {
							@Override String getId() {
								return id;
							}
						};
					}
				},
		};
	}
	
	private static class LiteralsKind {
		static final List<String> LITERAL_ID_LIST = Arrays.asList(new String[] {
				"numeric_constant",
				"char_constant",
				"wide_char_constant",
				"utf16_char_constant",
				"utf32_char_constant",
				"angle_string_literal",
				"string_literal",
				"wide_string_literal",
				"utf8_string_literal",
				"utf16_string_literal",
	            "utf32_string_literal"
		}); 
		static final Set<String> LITERAL_ID_SET = new HashSet<String>(LITERAL_ID_LIST);
	}
}
