#ifndef H_HTMLWRITER
#define H_HTMLWRITER

#include <string>
#include <vector>
#include <fstream>
#include "place.h"

using namespace std;

/**
* сохраняет результат в виде html-таблички
*/
struct HTMLWriter
{
public:
	HTMLWriter(vector<Place>& places); 
	~HTMLWriter();
	//сохраняет результат по указанному пути
	void Save(string path);
private:
	vector<Place>& places_;
};

#endif
