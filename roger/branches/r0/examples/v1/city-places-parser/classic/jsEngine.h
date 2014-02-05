#ifndef H_JSENGINE
#define H_JSENGINE

#include <stdlib.h>
#include <iostream>
#include <string>
#include "geocoder.h"
#include "place.h"
#include "comment.h"

using namespace std;

/**
* Класс, работающий с javascript движком Google V8 и предназначенный для разбора комментариев ЖЖ
* see also:
* http://habrahabr.ru/blogs/development/72474/
* http://habrahabr.ru/blogs/development/72592/
* http://habrahabr.ru/blogs/development/72765/
*/

struct JsEngine
{
public:	
	~JsEngine();	
	//Инициализация движка. Необходимо вызвать перед первым запуском
	//code - строка с js кодом внутри
	//places - список мест, в который будут добавляться найденные места. Должен быть проинициализирован
	void init(string code, vector<Place>* places);

	//Устанавливает геокодер. Необходимо вызвать до первого запуска для возможности геокодировать места.
	void setGeocoder(Geocoder* newGeocoder);

	//Выполнить js код над комментарием
	//comment - комментарий, над которым работаем
	//posRank / negRank - оценки, которые будут выставлены местам из этого комментария (на основе подкомментариев, см. jsProcessor.cpp)
	//out_posRank / out_negRank - оценки, полученные в этом комментарии. Выходные параметры.
	void runJS(Comment* comment, int posRank, int negRank, int* out_posRank, int* out_negRank);
private: 
	string code_;
	vector<Place>* places_;	
};



#endif
