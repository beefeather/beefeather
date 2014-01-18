namespace js;

using std::cout;
using std::string;
using std::vector;

/**
 * Класс, работающий с javascript движком Google V8 и предназначенный для разбора комментариев ЖЖ
 * see also:
 * http://habrahabr.ru/blogs/development/72474/
 * http://habrahabr.ru/blogs/development/72592/
 * http://habrahabr.ru/blogs/development/72765/
 */

struct JsEngine {
public:
  ~JsEngine() {
    //финальная чистка всего v8. Не нужна, ибо v8 живёт до конца работы приложения.
    //v8::V8::Dispose();
  }
  //Инициализация движка. Необходимо вызвать перед первым запуском
  //code - строка с js кодом внутри
  //places - список мест, в который будут добавляться найденные места. Должен быть проинициализирован
  void init(string code, vector<Place>* places) {
    code_ = code;
    places_ = places;

    // Создаёт handle scope (локальный scope) в стеке v8
    //Это пул хранения временных хэндлов
    v8::HandleScope handle_scope;

    // Создаёт глобальную среду выполнения кода V8
    v8::Handle<v8::ObjectTemplate> global = v8::ObjectTemplate::New();

    //Устанавливает в эту среду глобальные функции с++ callback'ов
    global->Set(v8::String::New("print"), v8::FunctionTemplate::New(Print));
    global->Set(v8::String::New("GoogleMapsRequest"),
        v8::FunctionTemplate::New(GoogleMapsRequest));

    //сохраняем наш глобальный контекст
    context_ = v8::Context::New(NULL, global);
  }

  //Устанавливает геокодер. Необходимо вызвать до первого запуска для возможности геокодировать места.
  void setGeocoder(Geocoder* newGeocoder) {
    geocoder = newGeocoder;
  }

  //Выполнить js код над комментарием
  //comment - комментарий, над которым работаем
  //posRank / negRank - оценки, которые будут выставлены местам из этого комментария (на основе подкомментариев, см. jsProcessor.cpp)
  //out_posRank / out_negRank - оценки, полученные в этом комментарии. Выходные параметры.
  void runJS(lj::Comment* comment, int posRank, int negRank, int* out_posRank,
      int* out_negRank) {
    // Создаёт handle scope (локальный scope) в стеке v8
    //Это пул хранения временных хэндлов
    v8::HandleScope handle_scope;

    // Входит в созданный в init глобальный контекст
    v8::Context::Scope context_scope(context_);

    // переводит строку с++ с кодом js в строку v8
    v8::Handle<v8::String> source = v8::String::New(code_.c_str());

    //компилируем скрипт
    v8::Handle<v8::Script> script;
    {
      //Стековый объект для отлова javascript-исключений
      v8::TryCatch try_catch;
      //Компилируем код
      script = v8::Script::Compile(source);
      if (script.IsEmpty()) {
        //Если скрипт не скомпилировался то сообщаем об ошибке
        ReportException(&try_catch);
      }
    }

    //выполняем скрипт
    {
      v8::TryCatch try_catch;
      script->Run();
      if (try_catch.HasCaught()) {
        ReportException(&try_catch);
      }
    }

    //Создаём v8 строку с названием функции
    v8::Handle<v8::String> fun_name = v8::String::New("ProcessLjComment");
    //Ищем значение этой строки в глобальном контексте
    v8::Handle<v8::Value> process_val = context_->Global()->Get(fun_name);

    //если найденное значение - не функция, то выходим.
    if (!process_val->IsFunction()) {
      return;
    }

    //кастуем это значение к функции v8
    v8::Handle<v8::Function> process_fun = v8::Handle<v8::Function>::Cast(
        process_val);

    //Преобразовываем текст комментария к строке v8
    v8::Handle<v8::String> input_line = v8::String::New(
        comment->getText().c_str());
    if (input_line == v8::Undefined()) {
      return;
    }

    //оформляем этот текст комментария в виде параметров функции
    const int argc = 1;
    v8::Handle<v8::Value> argv[argc] = { input_line };

    v8::Handle<v8::Value> jsresult; //результат выполнения функции
    {
      v8::TryCatch try_catch;
      //вызываем функцию js, передаём туда текст комментария и получаем результат
      jsresult = process_fun->Call(v8::Context::GetCurrent()->Global(), argc,
          argv);
      //если было словлено какое-то исключение, то сообщаем об ошибке
      if (try_catch.HasCaught()) {
        ReportException(&try_catch);
        return;
      }
    }

    //по соглашению, результат - массив
    // 0-й элемент - положительная оценка комментария
    // 1-й элемент - негативная оценка комментария
    // далее - объекты javascript, описывающие места

    //кастуем результат в массив
    if (!jsresult->IsArray()) {
      return;
    }
    // It is an array; cast it to a Array
    v8::Handle<v8::Array> jsResultPlaces = v8::Handle<v8::Array>::Cast(
        jsresult);

    //первые 2 значения в массиве по соглашению - положительные и негативные оценки.
    v8::Local<v8::Object> jsVal_positiveRank = v8::Local<v8::Object>::Cast(
        jsResultPlaces->Get(v8::Integer::New(0)));
    v8::String::Utf8Value val_pos_rank(jsVal_positiveRank);
    std::string str_pos_rank(ToCString(val_pos_rank));
    *out_posRank = atoi(str_pos_rank.c_str());

    v8::Local<v8::Object> jsVal_negativeRank = v8::Local<v8::Object>::Cast(
        jsResultPlaces->Get(v8::Integer::New(1)));
    v8::String::Utf8Value val_neg_rank(jsVal_negativeRank);
    std::string str_neg_rank(ToCString(val_neg_rank));
    *out_negRank = atoi(str_neg_rank.c_str());

    //далее объекты json с описанием найденных мест
    for (unsigned i = 2; i != jsResultPlaces->Length(); ++i) {
      //кастуем элемент массива в объект
      v8::Local<v8::Object> jsPlace = v8::Local<v8::Object>::Cast(
          jsResultPlaces->Get(v8::Integer::New(i)));

      //поля объекта
      string name;
      string address;
      string latitude;
      string longitude;

      //по названию выдираем поля из полученного объекта
      v8::Local<v8::Value> jsVal_name = jsPlace->Get(v8::String::New("name"));
      v8::String::Utf8Value val_name(jsVal_name);
      std::string property_name(ToCString(val_name));
      name = property_name;

      v8::Local<v8::Value> jsVal_address = jsPlace->Get(
          v8::String::New("address"));
      v8::String::Utf8Value val_address(jsVal_address);
      std::string property_address(ToCString(val_address));
      address = property_address;

      v8::Local<v8::Value> jsVal_latitude = jsPlace->Get(
          v8::String::New("latitude"));
      v8::String::Utf8Value val_latitude(jsVal_latitude);
      std::string property_latitude(ToCString(val_latitude));
      latitude = property_latitude;

      v8::Local<v8::Value> jsVal_longitude = jsPlace->Get(
          v8::String::New("longitude"));
      v8::String::Utf8Value val_longitude(jsVal_longitude);
      std::string property_longitude(ToCString(val_longitude));
      longitude = property_longitude;

      //генерируем место и добавляем его в список найденных мест
      Place place(name, address, comment); //name, address
      place.setCoordinates(strtod(latitude.c_str(), NULL),  //lat, lon
      strtod(longitude.c_str(), NULL));
      place.setRank(posRank, negRank);

      cout << "+ " << place.getName() << " " << place.getAddress() << " "
          << place.getLat() << " " << place.getLon() << "\n";
      places_->push_back(place);

    }
    return;
  }
private:
  string code_;
  vector<Place>* places_;
};

// создаёт c-строку из строки v8
const char* ToCString(const v8::String::Utf8Value& value) {
  return *value ? *value : "<string conversion failed>";
}

//функция сообщает об ошибке в процессе выполнения v8 кода
//стянуто отсюа:
//http://code.google.com/p/v8/source/browse/trunk/samples/lineprocessor.cc
void ReportException(v8::TryCatch* try_catch) {
  v8::HandleScope handle_scope;
  v8::String::Utf8Value exception(try_catch->Exception());
  const char* exception_string = ToCString(exception);
  v8::Handle<v8::Message> message = try_catch->Message();
  if (message.IsEmpty()) {
    // V8 didn't provide any extra information about this error; just
    // print the exception.
    printf("%s\n", exception_string);
  } else {
    // Print (filename):(line number): (message).
    v8::String::Utf8Value filename(message->GetScriptResourceName());
    const char* filename_string = ToCString(filename);
    int linenum = message->GetLineNumber();
    printf("%s:%i: %s\n", filename_string, linenum, exception_string);
    // Print line of source code.
    v8::String::Utf8Value sourceline(message->GetSourceLine());
    const char* sourceline_string = ToCString(sourceline);
    printf("%s\n", sourceline_string);
    // Print wavy underline (GetUnderline is deprecated).
    int start = message->GetStartColumn();
    for (int i = 0; i < start; i++) {
      printf(" ");
    }
    int end = message->GetEndColumn();
    for (int i = start; i < end; i++) {
      printf("^");
    }
    printf("\n");
  }
}

//функции с++, вызываемые из js. описание функций ниже.
//проще всего сделать их глобальными.

//функция - callback.
//Вызывается, когда в js встречается функция print()
//Выводит в stdout переданные аргументы
v8::Handle<v8::Value> Print(const v8::Arguments& args) {
  bool first = true;
  for (int i = 0; i < args.Length(); i++) {
    v8::HandleScope handle_scope;
    if (first) {
      first = false;
    } else {
      printf(" ");
    }
    v8::String::Utf8Value str(args[i]);
    const char* cstr = ToCString(str);
    printf("%s", cstr);
  }
  printf("\n");
  fflush(stdout);
  return v8::Undefined();
}

//функция - callback.
//Вызывается, когда в js встречается функция GoogleMapsRequest()
//отправляет запрос геокодеру и возвращает в js ответ геокодера
v8::Handle<v8::Value> GoogleMapsRequest(const v8::Arguments& args) {
  v8::HandleScope handle_scope;
  v8::String::Utf8Value str(args[0]);
  string input(ToCString(str));

  string output = "";
  if (geocoder != NULL) {
    output = geocoder->SendRequest(input);
  }

  return v8::String::New(output.c_str());
}
Geocoder * geocoder; //обращаться к методам геокодера тоже проще, если он глобальный.

//постоянная ссылка на контекст.
v8::Persistent<v8::Context> context_;
