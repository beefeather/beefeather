#ifndef H_COMMENTSCACHESAVER
#define H_COMMENTSCACHESAVER

#include <iostream>
#include <fstream>
#include <fstream>
#include <string>

#include "comment.h"

using namespace std;

/**
* Сохраняет комментарии в кэш на локальный диск
*/
struct CommentsCacheSaver
{
public:	
	//сохраняет комментарии
	//path - путь, куда сохраняет
	//rootComment - корень дерева комментариев.
	void SaveComments(string path, LjPost* rootComment);	
private:	
	void SaveSubcomment(ofstream& file, Comment* rootComment, int includeLevel);
    void SaveContainer(ofstream& file, Container* rootComment, int includeLevel);

	string filePath_; 
	LjPost* rootComment_;	
};

#endif
