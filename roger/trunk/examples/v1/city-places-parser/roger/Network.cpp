namespace;

using std::ofstream;
using std::string;
using cURLpp::types::WriteFunctionFunctor;
using cURLpp::options::WriteFunction;
using curlpp::options::Url;

/**
 * Класс, позволяющий работать с сетью. Использует библиотеку сURLpp
 */

struct Network {
public:
  //посылает GET-запрос и возвращает ответ
  string sendGET(string requestStr) {
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


  //callback curlpp functor
  //callback, который пишет ответ в string. Без функцтора - в stdout
  static size_t WriteMemoryCallback(char* ptr, size_t size, size_t nmemb) {
    RequestAnswer += ptr;
    int FullSize = size * nmemb;
    return FullSize;
  }

  static string RequestAnswer;
};
