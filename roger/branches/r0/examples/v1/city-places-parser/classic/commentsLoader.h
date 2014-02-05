#ifndef H_COMMENTSLOADER
#define H_COMMENTSLOADER

#include <iostream>
#include <fstream>
#include <string>

#include "comment.h"
#include "commentsCacheLoader.h"
#include "commentsCacheSaver.h"
#include "commentsWebLoader.h"

using namespace std;

/**
* Класс, отвечающий за всё внешнее вразимодействие с комментариями.
* Сам определяет, есть ли этот пост и комментарии в кэше, 
* Скачивает их из сети и добавляет в кэш в обратном случае.
*/
struct CommentsLoader
{
public:
	CommentsLoader(); 
	//загружает дерево комментариев
	//url - url поста
	//rootComment - корень дерева комментариев. Считается, что он уже проинициализирован.
	void LoadComments(string url, LjPost* rootComment); 
private:
	string filePath_; 
	string url_;
	string filePathPrefix_; //префикс преед всеми файлами кэша
	LjPost* rootComment_; 	
	string postNum_; //номер поста
};

#endif
