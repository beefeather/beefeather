#ifndef H_COMMENTSCACHELOADER
#define H_COMMENTSCACHELOADER

#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <fstream>
#include <string>

#include "comment.h"

using namespace std;


/**
* Загружает комментарии из кэша
*/
struct CommentsCacheLoader
{
public:
	//загружает комментарии с локального диска.
	//url - url поста
	//path - путь к файлу с комментариями
	//rootComment - LjPost, корень дерева комментариев
	void LoadCommentsFromCache(string url, string path, LjPost* rootComment);
private:	
	string addCommentFromCache(ifstream& file, Container* parent, int level);

	string url_; 	
	string filePath_; 	
	LjPost* rootComment_;			
};


#endif
