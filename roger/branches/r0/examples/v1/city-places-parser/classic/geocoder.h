#ifndef H_GEOCODER
#define H_GEOCODER

#include <string>
#include <vector>
#include <algorithm>
#include <stdlib.h>
#include "place.h"
#include "network.h"

using namespace std;

/**
* Класс, работающий с геокодером Google maps
*/

struct Geocoder
{
public:
	Geocoder();
	//реализация, принимающая список мест и подставляющая туда координаты
	void GetCoordinates(vector<Place>& places);

	//отправляет запрос и возвращает string с ответом google maps
	//placeName - просто имя места. 
	string SendRequest(string placeName);

	//установить город. к запросам будет добавляться город для более точного геокодирования, отсеиваться результаты вне этого города.
	void setCityName(string city);
private: 
	void calculateCityBounds();
	string checkCoordinates(string response, string placeName);

	string city_;
	string cityLon_;
	string cityLat_;
	string cityBounds_;
};

#endif
