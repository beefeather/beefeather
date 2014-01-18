#ifndef H_KMLWRITER
#define H_KMLWRITER

#include <string>
#include <vector>
#include <fstream>
#include "place.h"

using namespace std;

/**
* сохраняет результат в kml-файл, пригодный к просмотру в google maps / google earth
*/
struct KMLWriter
{
public:
	KMLWriter(vector<Place>& places); 
	~KMLWriter();
	//сохраняет по указанному пути
	void Save(string path);
private:
	vector<Place>& places_;
};

#endif
