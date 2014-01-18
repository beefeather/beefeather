#ifndef H_COMMENTSPROCESSOR
#define H_COMMENTSPROCESSOR

#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <fstream>
#include <string>

#include "comment.h"
#include "place.h"
#include "jsEngine.h"
#include "geocoder.h"

using namespace std;

/**
*  Разбирает комментарии, находит в них места и выставляет оценки.
*/
struct CommentsProcessor
{
public:
	//scriptPath - путь к js файлу (локальному)
	//rootComment - корень дерева комментариев. Должен быть проинициализирован
	//places - список найденных мест. Должен быть проинициализирован
	void run(string scriptPath, LjPost* rootComment, vector<Place>* places);
private:
	void processSubComments(Comment* comment, int* out_posRank, int* out_negRank);

	string filePath_; 	
	string jsCode_;
	LjPost* rootComment_;	
	vector<Place>* places_;
	JsEngine engine_;
	Geocoder geocoder_;	
};


#endif
