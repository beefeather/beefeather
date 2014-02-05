#include "network.h"

string RequestAnswer;

string Network::sendGET(string requestStr){
	curlpp::Cleanup cleanup;	
	curlpp::Easy request;

	request.setOpt(Url(requestStr.c_str()));
	  
	WriteFunctionFunctor functor(WriteMemoryCallback);
	WriteFunction* cb = new curlpp::options::WriteFunction(functor);
	request.setOpt(cb);
	RequestAnswer.clear();
	request.perform();		
	return RequestAnswer;
}

//callback, который пишет ответ в string. Без функцтора - в stdout
size_t WriteMemoryCallback(char* ptr, size_t size, size_t nmemb){		
	RequestAnswer += ptr;
	int FullSize = size * nmemb;
	return FullSize;
}

