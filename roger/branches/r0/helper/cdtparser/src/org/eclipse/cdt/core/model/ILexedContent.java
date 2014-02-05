package org.eclipse.cdt.core.model;


public interface ILexedContent {
  int size();
  int getType(int pos);
  String getText(int pos);
}
