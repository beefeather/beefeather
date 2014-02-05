#ifndef H_NETWORK
#define H_NETWORK

#include <stdlib.h>
#include <iostream>
#include <sys/stat.h> 
#include <string>
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>

using namespace cURLpp;
using namespace Options;
using namespace types;
using namespace std;

/**
* Класс, позволяющий работать с сетью. Использует библиотеку сURLpp
*/

struct Network
{
public:
	//посылает GET-запрос и возвращает ответ
	string sendGET(string requestStr);
};

//callback curlpp functor
size_t WriteMemoryCallback(char* ptr, size_t size, size_t nmemb);















#endif
