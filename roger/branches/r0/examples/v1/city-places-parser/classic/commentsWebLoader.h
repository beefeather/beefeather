#ifndef H_COMMENTSWEBLOADER
#define H_COMMENTSWEBLOADER

#include <iostream>
#include <sstream> //to use stringstream for intToStr function
#include <fstream>
#include <stdlib.h> 
#include <fstream>
#include <string>
#include "comment.h"
#include "network.h"

using namespace std;

/**
* Загружает комментарии из сети
*/
struct CommentsWebLoader
{
public:	
	//url - url поста
	//rootComment - корень дерева комментариев
	//filePathPrefix - префикс в названиях файлов кэша
	void LoadComments(string url, LjPost* rootComment, string filePathPrefix);
private:	
	void LoadPagesFromWeb();
	void LoadCommentsFromWeb();
	void addCommentFromWeb(Container* parent, string thread);

	string intToStr(int number);

	string filePath_; 
	string url_;
	string filePathPrefix_;
	LjPost* rootComment_;
	string postNum_;
	int pages_;
};

#endif
