package ru.spb.rybin.eclipsereplacement;

public class Assert {

	public static void isNotNull(Object o) {
		if (o == null) {
		    throw new RuntimeException();
		}
	}

	public static void isTrue(boolean b) {
		if (!b) {
		    throw new RuntimeException();
		}
	}

	public static boolean isLegal(boolean b, String message) {
		if (b) {
			return true;
		} else { 
			throw new IllegalArgumentException(message);
		}
	}

	public static boolean isLegal(boolean b) {
		return isLegal(b, null);
	}

}
