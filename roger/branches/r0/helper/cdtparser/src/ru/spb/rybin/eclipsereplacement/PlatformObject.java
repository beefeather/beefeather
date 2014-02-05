package ru.spb.rybin.eclipsereplacement;

public class PlatformObject implements IAdaptable {
	@Override
	public Object getAdapter(Class adapter) {
		if (adapter.isInstance(this)) {
			return this;
		}
		return null;
	}

}
