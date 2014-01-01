package ru.spb.rybin.clangroger.overviewparser;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.cdt.core.model.ILexedContent;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.internal.core.parser.scanner.TokenUtil;

class TokenReader {
	private final InputStream in;

	TokenReader(InputStream in) {
		this.in = in;
	}
	
	private static class TokenAdapter {
		int cdtKind;
		String text;

		TokenAdapter(int cdtKind, String text) {
			this.cdtKind = cdtKind;
			this.text = text;
		}
		
		@Override
		public String toString() {
			if (text == null) {
				return new String(TokenUtil.getImage(cdtKind));
			} else {
				return text;
			}
		}
	}
	
	ILexedContent read() throws IOException {
		final List<TokenAdapter> result = new ArrayList<TokenAdapter>();
		while (true) {
			int kindCode = in.read();
			if (kindCode == -1) {
				break;
			}
			ClangToken.Kind kind = ClangToken.INSTANCE.get(kindCode);
			//System.out.println(kind);
			int flags = in.read();
			//System.out.println(flags);
			{
				int b1 = in.read();
				int b2 = in.read();
				int length = b2 * 256 + b1;
				//System.out.println(length);
			}
			
			String s;
			int cdtKind;
			if (kind.isLiteral() || kind.getId().equals(ClangToken.IDENTIFIER_KIND_ID)) {
				int b1 = in.read();
				int b2 = in.read();
				int length = b2 * 256 + b1;
				//System.out.println(length);
				byte[] bytes = new byte[length];
				int res = in.read(bytes);
				if (res != bytes.length) {
					throw new RuntimeException("Need more elaborate reader");
				}
				s = new String(bytes);
				//System.out.println("Literal: " + s);
				
				if (kind.getId().equals("numeric_constant")) {
					if (s.indexOf('.') == -1) {
						cdtKind = IToken.tINTEGER;
					} else {
						cdtKind = IToken.tFLOATINGPT;
					}
				} else if (kind.getId().equals(ClangToken.IDENTIFIER_KIND_ID)) {
					cdtKind = IToken.tIDENTIFIER;
				} else {
					Integer kindObj = MAPPING_TABLE.textual.get(kind.getId());
					if (kindObj == null) {
						throw new RuntimeException("Unknown id " + kind.getId());
					}
					cdtKind = kindObj;
				}
			} else if (kind.getId().equals("__null")) {
				cdtKind = IToken.tIDENTIFIER;
				s = "NULL";
			} else if (MAPPING_TABLE.simpleToImage.containsKey(kind.getId())) {
				s = MAPPING_TABLE.simpleToImage.get(kind.getId());
				cdtKind = IToken.tIDENTIFIER;
			} else {
				s = null;
				Integer kindObj = MAPPING_TABLE.simple.get(kind.getId());
				if (kindObj == null) {
					throw new RuntimeException("Unknown id " + kind.getId());
				}
				cdtKind = kindObj;
			}
			TokenAdapter tokenAdapter = new TokenAdapter(cdtKind, s);
			//System.out.print(tokenAdapter + " ");
			result.add(tokenAdapter);
		}
		result.add(new TokenAdapter(IToken.tEND_OF_INPUT, null));
		
		return new ILexedContent() {
			@Override public int size() {
				return result.size();
			}
			@Override public int getType(int pos) {
				return result.get(pos).cdtKind;
			}
			@Override public String getText(int pos) {
				return result.get(pos).text;
			}
		};
	}
	
	private static final MappingTable MAPPING_TABLE;
	static {
		MAPPING_TABLE = new MappingTable();
		MAPPING_TABLE.fill();
	}
	
	private static class MappingTable {
		
		Map<String, Integer> simple = new HashMap<String, Integer>();
		Map<String, String> simpleToImage = new HashMap<String, String>();
		Map<String, Integer> textual = new HashMap<String, Integer>();
		
		void add(String clangId, int cdtId) {
			simple.put(clangId, cdtId);
		}
		
		void addToImage(String clangId, String image) {
			simpleToImage.put(clangId, image);
		}
		
		void addTextual(String clangId, int cdtId) {
			textual.put(clangId, cdtId);
		}
		
		void fill() {
			//add("unknown", IToken.tUNKNOWN_CHAR);
			add("eof", IToken.tEND_OF_INPUT);
			//add("eod", IToken.t);
			//add("code_completion", IToken.t);
			//add("cxx_defaultarg_end", IToken.t);
			//add("comment", IToken.t);
			add("identifier", IToken.tIDENTIFIER);
			//add("raw_identifier", IToken.t);
			
			//addTextual("numeric_constant", IToken.t);
			addTextual("char_constant", IToken.tCHAR);
			//addTextual("wide_char_constant", IToken.t);
			addTextual("utf16_char_constant", IToken.tUTF16CHAR);
			addTextual("utf32_char_constant", IToken.tUTF32CHAR);
			addTextual("string_literal", IToken.tSTRING);
			//addTextual("wide_string_literal", IToken.t);
			//addTextual("angle_string_literal", IToken.t);
			//addTextual("utf8_string_literal", IToken.t);
			addTextual("utf16_string_literal", IToken.tUTF16STRING);
			addTextual("utf32_string_literal", IToken.tUTF32STRING);
			
			add("l_square", IToken.tLBRACKET);
			add("r_square", IToken.tRBRACKET);
			add("l_paren", IToken.tLPAREN);
			add("r_paren", IToken.tRPAREN);
			add("l_brace", IToken.tLBRACE);
			add("r_brace", IToken.tRBRACE);
			add("period", IToken.tDOT);
			add("ellipsis", IToken.tELLIPSIS);
			add("amp", IToken.tAMPER);
			add("ampamp", IToken.tAND);
			add("ampequal", IToken.tAMPERASSIGN);
			add("star", IToken.tSTAR);
			add("starequal", IToken.tSTARASSIGN);
			add("plus", IToken.tPLUS);
			add("plusplus", IToken.tINCR);
			add("plusequal", IToken.tPLUSASSIGN);
			add("minus", IToken.tMINUS);
			add("arrow", IToken.tARROW);
			add("minusminus", IToken.tDECR);
			add("minusequal", IToken.tMINUSASSIGN);
			add("tilde", IToken.tBITCOMPLEMENT);
			add("exclaim", IToken.tNOT);
			add("exclaimequal", IToken.tNOTEQUAL);
			add("slash", IToken.tDIV);
			add("slashequal", IToken.tDIVASSIGN);
			add("percent", IToken.tMOD);
			add("percentequal", IToken.tMODASSIGN);
			add("less", IToken.tLT);
			add("lessless", IToken.tSHIFTL);
			add("lessequal", IToken.tLTEQUAL);
			add("lesslessequal", IToken.tSHIFTLASSIGN);
			add("greater", IToken.tGT);
			add("greatergreater", IToken.tSHIFTR);
			add("greaterequal", IToken.tGTEQUAL);
			add("greatergreaterequal", IToken.tSHIFTRASSIGN);
			add("caret", IToken.tXOR);
			add("caretequal", IToken.tXORASSIGN);
			add("pipe", IToken.tBITOR);
			add("pipepipe", IToken.tOR);
			add("pipeequal", IToken.tBITORASSIGN);
			add("question", IToken.tQUESTION);
			add("colon", IToken.tCOLON);
			add("semi", IToken.tSEMI);
			add("equal", IToken.tASSIGN);
			add("equalequal", IToken.tEQUAL);
			add("comma", IToken.tCOMMA);
			//add("hash", IToken.t);
			//add("hashhash", IToken.t);
			//add("hashat", IToken.t);
			//add("periodstar", IToken.t);
			add("arrowstar", IToken.tARROWSTAR);
			add("coloncolon", IToken.tCOLONCOLON);
			//add("at", IToken.t);
			//add("lesslessless", );
			//add("greatergreatergreater", IToken.t);
			add("auto", IToken.t_auto);
			add("break", IToken.t_break);
			add("case", IToken.t_case);
			add("char", IToken.t_char);
			add("const", IToken.t_const);
			add("continue", IToken.t_continue);
			add("default", IToken.t_default);
			add("do", IToken.t_do);
			add("double", IToken.t_double);
			add("else", IToken.t_else);
			add("enum", IToken.t_enum);
			add("extern", IToken.t_extern);
			add("float", IToken.t_float);
			add("for", IToken.t_for);
			add("goto", IToken.t_goto);
			add("if", IToken.t_if);
			add("inline", IToken.t_inline);
			add("int", IToken.t_int);
			add("long", IToken.t_long);
			add("register", IToken.t_register);
			add("restrict", IToken.t_restrict);
			add("return", IToken.t_return);
			add("short", IToken.t_short);
			add("signed", IToken.t_signed);
			add("sizeof", IToken.t_sizeof);
			add("static", IToken.t_static);
			add("struct", IToken.t_struct);
			add("switch", IToken.t_switch);
			add("typedef", IToken.t_typedef);
			add("union", IToken.t_union);
			add("unsigned", IToken.t_unsigned);
			add("void", IToken.t_void);
			add("volatile", IToken.t_volatile);
			add("while", IToken.t_while);
//			add("_Alignas", IToken.t);
//			add("_Alignof", IToken.t);
//			add("_Atomic", IToken.t);
//			add("_Bool", IToken.t);
//			add("_Complex", IToken.t);
//			add("_Generic", IToken.t);
//			add("_Imaginary", IToken.t);
//			add("_Noreturn", IToken.t);
//			add("_Static_assert", IToken.t);
//			add("_Thread_local", IToken.t);
//			add("__func__", IToken.t);
//			add("__objc_yes", IToken.t);
//			add("__objc_no", IToken.t);
			add("asm", IToken.t_asm);
			add("bool", IToken.t_bool);
			add("catch", IToken.t_catch);
			add("class", IToken.t_class);
			add("const_cast", IToken.t_const_cast);
			add("delete", IToken.t_delete);
			add("dynamic_cast", IToken.t_dynamic_cast);
			add("explicit", IToken.t_explicit);
			add("export", IToken.t_export);
			add("false", IToken.t_false);
			add("friend", IToken.t_friend);
			add("mutable", IToken.t_mutable);
			add("namespace", IToken.t_namespace);
			add("new", IToken.t_new);
			add("operator", IToken.t_operator);
			add("private", IToken.t_private);
			add("protected", IToken.t_protected);
			add("public", IToken.t_public);
			add("reinterpret_cast", IToken.t_reinterpret_cast);
			add("static_cast", IToken.t_static_cast);
			add("template", IToken.t_template);
			add("this", IToken.t_this);
			add("throw", IToken.t_throw);
			add("true", IToken.t_true);
			add("try", IToken.t_try);
			add("typename", IToken.t_typename);
			add("typeid", IToken.t_typeid);
			add("using", IToken.t_using);
			add("virtual", IToken.t_virtual);
			add("wchar_t", IToken.t_wchar_t);
//			add("alignas", IToken.t_a);
//			add("alignof", IToken.t);
			add("char16_t", IToken.t_char16_t);
			add("char32_t", IToken.t_char32_t);
//			add("constexpr", IToken.t);
//			add("decltype", IToken.t);
//			add("noexcept", IToken.t);
//			add("nullptr", IToken.t);
//			add("static_assert", IToken.t);
//			add("thread_local", IToken.t);
//			add("capybara", IToken.t);
//			add("_Decimal32", IToken.t);
//			add("_Decimal64", IToken.t);
//			add("_Decimal128", IToken.t);
			addToImage("__null", "NULL");
//			add("__alignof", IToken.t);
			addToImage("__attribute", "__attribute__");
//			add("__builtin_choose_expr", IToken.t);
//			add("__builtin_offsetof", IToken.t);
//			add("__builtin_types_compatible_p", IToken.t);
//			add("__builtin_va_arg", IToken.t);
//			add("__extension__", IToken.t);
//			add("__imag", IToken.t);
//			add("__int128", IToken.t);
//			add("__label__", IToken.t);
//			add("__real", IToken.t);
//			add("__thread", IToken.t);
//			add("__FUNCTION__", IToken.t);
//			add("__PRETTY_FUNCTION__", IToken.t);
//			add("typeof", IToken.t);
//			add("L__FUNCTION__", IToken.t);
//			add("__has_nothrow_assign", IToken.t);
//			add("__has_nothrow_move_assign", IToken.t);
//			add("__has_nothrow_copy", IToken.t);
//			add("__has_nothrow_constructor", IToken.t);
//			add("__has_trivial_assign", IToken.t);
//			add("__has_trivial_move_assign", IToken.t);
//			add("__has_trivial_copy", IToken.t);
//			add("__has_trivial_constructor", IToken.t);
//			add("__has_trivial_move_constructor", IToken.t);
//			add("__has_trivial_destructor", IToken.t);
//			add("__has_virtual_destructor", IToken.t);
//			add("__is_abstract", IToken.t);
//			add("__is_base_of", IToken.t);
//			add("__is_class", IToken.t);
//			add("__is_convertible_to", IToken.t);
//			add("__is_empty", IToken.t);
//			add("__is_enum", IToken.t);
//			add("__is_final", IToken.t);
//			add("__is_interface_class", IToken.t);
//			add("__is_literal", IToken.t);
//			add("__is_literal_type", IToken.t);
//			add("__is_pod", IToken.t);
//			add("__is_polymorphic", IToken.t);
//			add("__is_trivial", IToken.t);
//			add("__is_union", IToken.t);
//			add("__is_trivially_constructible", IToken.t);
//			add("__is_trivially_copyable", IToken.t);
//			add("__is_trivially_assignable", IToken.t);
//			add("__underlying_type", IToken.t);
//			add("__is_lvalue_expr", IToken.t);
//			add("__is_rvalue_expr", IToken.t);
//			add("__is_arithmetic", IToken.t);
//			add("__is_floating_point", IToken.t);
//			add("__is_integral", IToken.t);
//			add("__is_complete_type", IToken.t);
//			add("__is_void", IToken.t);
//			add("__is_array", IToken.t);
//			add("__is_function", IToken.t);
//			add("__is_reference", IToken.t);
//			add("__is_lvalue_reference", IToken.t);
//			add("__is_rvalue_reference", IToken.t);
//			add("__is_fundamental", IToken.t);
//			add("__is_object", IToken.t);
//			add("__is_scalar", IToken.t);
//			add("__is_compound", IToken.t);
//			add("__is_pointer", IToken.t);
//			add("__is_member_object_pointer", IToken.t);
//			add("__is_member_function_pointer", IToken.t);
//			add("__is_member_pointer", IToken.t);
//			add("__is_const", IToken.t);
//			add("__is_volatile", IToken.t);
//			add("__is_standard_layout", IToken.t);
//			add("__is_signed", IToken.t);
//			add("__is_unsigned", IToken.t);
//			add("__is_same", IToken.t);
//			add("__is_convertible", IToken.t);
//			add("__array_rank", IToken.t);
//			add("__array_extent", IToken.t);
//			add("__private_extern__", IToken.t);
//			add("__module_private__", IToken.t);
//			add("__declspec", IToken.t);
//			add("__cdecl", IToken.t);
//			add("__stdcall", IToken.t);
//			add("__fastcall", IToken.t);
//			add("__thiscall", IToken.t);
//			add("__forceinline", IToken.t);
//			add("__unaligned", IToken.t);
//			add("__kernel", IToken.t);
//			add("vec_step", IToken.t);
//			add("__private", IToken.t);
//			add("__global", IToken.t);
//			add("__local", IToken.t);
//			add("__constant", IToken.t);
//			add("__read_only", IToken.t);
//			add("__write_only", IToken.t);
//			add("__read_write", IToken.t);
//			add("__builtin_astype", IToken.t);
//			add("image1d_t", IToken.t);
//			add("image1d_array_t", IToken.t);
//			add("image1d_buffer_t", IToken.t);
//			add("image2d_t", IToken.t);
//			add("image2d_array_t", IToken.t);
//			add("image3d_t", IToken.t);
//			add("sampler_t", IToken.t);
//			add("event_t", IToken.t);
//			add("__pascal", IToken.t);
//			add("__vector", IToken.t);
//			add("__pixel", IToken.t);
//			add("half", IToken.t);
//			add("__bridge", IToken.t);
//			add("__bridge_transfer", IToken.t);
//			add("__bridge_retained", IToken.t);
//			add("__bridge_retain", IToken.t);
//			add("__ptr64", IToken.t);
//			add("__ptr32", IToken.t);
//			add("__sptr", IToken.t);
//			add("__uptr", IToken.t);
//			add("__w64", IToken.t);
//			add("__uuidof", IToken.t);
//			add("__try", IToken.t);
//			add("__finally", IToken.t);
//			add("__leave", IToken.t);
//			add("__int64", IToken.t);
//			add("__if_exists", IToken.t);
//			add("__if_not_exists", IToken.t);
//			add("__single_inheritance", IToken.t);
//			add("__multiple_inheritance", IToken.t);
//			add("__virtual_inheritance", IToken.t);
//			add("__interface", IToken.t);
//			add("__builtin_convertvector", IToken.t);
//			add("cxxscope", IToken.t);
//			add("typename", IToken.t);
//			add("template_id", IToken.t);
//			add("primary_expr", IToken.t);
//			add("decltype", IToken.t);
//			add("pragma_unused", IToken.t);
//			add("pragma_vis", IToken.t);
//			add("pragma_pack", IToken.t);
//			add("pragma_parser_crash", IToken.t);
//			add("pragma_captured", IToken.t);
//			add("pragma_msstruct", IToken.t);
//			add("pragma_align", IToken.t);
//			add("pragma_weak", IToken.t);
//			add("pragma_weakalias", IToken.t);
//			add("pragma_redefine_extname", IToken.t);
//			add("pragma_fp_contract", IToken.t);
//			add("pragma_opencl_extension", IToken.t);
//			add("pragma_openmp", IToken.t);
//			add("pragma_openmp_end", IToken.t);
		}
	}
}
