#include "geocoder.h"
#include <cmath>

string requestType = "http://maps.google.com/maps/geo?q=";
string mapsAPIKey = "ABQIAAAArHr2R681bOacRTA7NiR7KRTIKY0J4v1euTkM_44KO8g9LlpXpRQNl7-CwQWCn5E2wBhD4HlBksrcwg";
string requestKeys = "&output=json&oe=utf8&oe=utf8&sensor=false&hl=ru&gl=ru&key=" + mapsAPIKey;	

Geocoder::Geocoder() : city_(""), cityLon_(""), cityLat_(""), cityBounds_("0.2") { }

void Geocoder::GetCoordinates(vector<Place>& places){
	Network network;
	
	for(std::vector<Place>::iterator i = places.begin(); i != places.end(); ++i){
		string name = (*i).getName();
		
		replace(name.begin(), name.end(), ' ', '+');
		
		string request = requestType + city_ + ",+" + name + requestKeys;
		if(cityLon_ != ""){
			request += "&ll="+cityLat_+","+cityLon_+"&spn="+cityBounds_;
		}
		string response = network.sendGET(request);
		
		//checking status
		//example:    "code": 200,
		if (response.find("\"code\": 200,") == -1 ){
			continue;  //nothing found on google maps. 
		}

		//getting adress
		//example:   "address": "Невский просп., 30, город Санкт-Петербург, Россия, 191011",
		string addressBeginTag = "address\": \"";
		int addressBeginPos = response.find(addressBeginTag);
		if(addressBeginPos == -1) continue;
		addressBeginPos += addressBeginTag.length();
		int addressEndPos = response.find("\n", addressBeginPos) - 2;
		string address = response.substr(addressBeginPos, addressEndPos - addressBeginPos);

		//getting coordinates
		//example:      "coordinates": [ 30.3270367, 59.9356409, 0 ]
		string coordinatesBeginTag = "\"coordinates\": [ ";
		int lonBeginPos = response.find(coordinatesBeginTag) + coordinatesBeginTag.length();
		int lonEndPos = response.find(",", lonBeginPos);
		char* pEnd;
		double lon = strtod(response.substr(lonBeginPos, lonEndPos - lonBeginPos).c_str(), &pEnd);

		int latBeginPos = lonEndPos + 2;
		int latEndPos = response.find(",", latBeginPos);
		double lat = strtod(response.substr(latBeginPos, latEndPos - latBeginPos).c_str(), &pEnd);
		
		//setting new options
		(*i).setAddress(address);
		(*i).setCoordinates(lat, lon);
	}//for
}

string Geocoder::SendRequest(string placeName){
	Network network;
	replace(placeName.begin(), placeName.end(), ' ', '+');
	string request = requestType + city_ + ",+" + placeName + requestKeys;
	//если установлены границы координат для города, то добавляем их в запрос	
	if(cityLon_ != ""){			
			request += "&ll="+cityLat_+","+cityLon_+"&spn="+cityBounds_+","+cityBounds_;
	}	

	string response = network.sendGET(request);	
	//опыт показал, что даже это не спасает от поиска вне города. Нужна проверка результата
	if(cityLon_ != ""){	
		response = checkCoordinates(response, placeName);
	}
	return response;
}

//хак: если нашлось чтото вне города, или просто сам город, то вручную меняем код возврата на "не найдено"
string Geocoder::checkCoordinates(string response, string placeName){
	if (response.find("\"code\": 200,") == -1 ){
			return response;  //итак ничего не найдено
	}

	//вручную смотрим что вернулось, и если координаты нам не подходят, то меняем код возврата на 200
	//getting coordinates
	//example:      "coordinates": [ 30.3270367, 59.9356409, 0 ]
	string coordinatesBeginTag = "\"coordinates\": [ ";
	int lonBeginPos = response.find(coordinatesBeginTag) + coordinatesBeginTag.length();
	int lonEndPos = response.find(",", lonBeginPos);
	char* pEnd;
	double lon = strtod(response.substr(lonBeginPos, lonEndPos - lonBeginPos).c_str(), &pEnd);

	int latBeginPos = lonEndPos + 2;
	int latEndPos = response.find(",", latBeginPos);
	double lat = strtod(response.substr(latBeginPos, latEndPos - latBeginPos).c_str(), &pEnd);
		
	double cityLon = strtod(cityLon_.c_str(), &pEnd);
	double cityLat = strtod(cityLat_.c_str(), &pEnd);
	double cityBounds = strtod(cityBounds_.c_str(), &pEnd);
	
	bool isCorrectionNeeded = false;	
	if(abs(lon - cityLon) > cityBounds || 
	   abs(lat - cityLat) > cityBounds ) {
		isCorrectionNeeded = true;
	}

	//проверяем, не искали ли мы сам город 
	//запрос включает в себя название города и не сильно длиннее его.
	if(placeName.find(city_) != -1 && placeName.length() - city_.length() < 5) {
		isCorrectionNeeded = true;
	}	

	if(isCorrectionNeeded) {	
		//нашлось что-то левое вне города
		//заменяем поле code на 200
		int codePos = response.find("\"code\": "); //нашли просто code и не важно что было за ним
		//length of:  "code": 200 == 11
		response.replace(codePos, 11, "\"code\": 602");	//подставили туда 602 (не найдено)				
	}

	return response;
}

void Geocoder::setCityName(string city){
	city_ = city;
	calculateCityBounds();
}

//вычисляет координаты города
void Geocoder::calculateCityBounds(){
	Network network;

	string request = requestType + city_ + requestKeys;
	string response = network.sendGET(request);
		
	//checking status	
	if (response.find("\"code\": 200,") == -1 ){
		cout<<"Error. City not found\n";
		return;  //nothing found on google maps.
	}

	//getting coordinates
	//example:      "coordinates": [ 30.3270367, 59.9356409, 0 ]
	string coordinatesBeginTag = "\"coordinates\": [ ";
	int lonBeginPos = response.find(coordinatesBeginTag) + coordinatesBeginTag.length();
	int lonEndPos = response.find(",", lonBeginPos);
	char* pEnd;
	cityLon_ = response.substr(lonBeginPos, lonEndPos - lonBeginPos).c_str();

	int latBeginPos = lonEndPos + 2;
	int latEndPos = response.find(",", latBeginPos);
	cityLat_  = response.substr(latBeginPos, latEndPos - latBeginPos);

}

