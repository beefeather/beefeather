namespace;

using std::ifstream;
using std::cout;
using std::string;

/**
 * Класс, отвечающий за всё внешнее вразимодействие с комментариями.
 * Сам определяет, есть ли этот пост и комментарии в кэше,
 * Скачивает их из сети и добавляет в кэш в обратном случае.
 */
struct CommentsLoader {
public:
  CommentsLoader() {
    filePathPrefix_ = "saved_"; //устанавливаем префикс перед всеми файлами кэша
  }
  //загружает дерево комментариев
  //url - url поста
  //rootComment - корень дерева комментариев. Считается, что он уже проинициализирован.
  void LoadComments(string url, lj::LjPost* rootComment) {
    url_ = url;
    rootComment_ = rootComment;

    //Выдираем номер поста из url
    //example url: http://tema.livejournal.com/704158.html?page=1comments
    int postNumBeginPos = url.find(".com/");
    if (postNumBeginPos == -1)
      return;
    postNumBeginPos += 5;
    int postNumEndPos = url.find(".html", postNumBeginPos);
    postNum_ = url.substr(postNumBeginPos, postNumEndPos - postNumBeginPos);
    filePath_ = filePathPrefix_ + postNum_;

    //пытаемся открыть кэш, в зависимости от успеха/неудачи грузим либо из кэша, либо из сети.
    ifstream input_file;
    input_file.open(filePath_.c_str());
    if (input_file.fail()) {
      cout << "No cached found. Loading from web...\n";

      lj::CommentsWebLoader loader;
      loader.LoadComments(url_, rootComment_, filePathPrefix_);

      filecache::CommentsCacheSaver saver;
      saver.SaveComments(filePath_, rootComment_);
    } else {
      cout << "Loading from cache...\n";
      input_file.close();

      filecache::CommentsCacheLoader loader;
      loader.LoadCommentsFromCache(url_, filePath_, rootComment_);
    }
  }
private:
  string filePath_;
  string url_;
  string filePathPrefix_; //префикс преед всеми файлами кэша
  lj::LjPost* rootComment_;
  string postNum_; //номер поста
};
