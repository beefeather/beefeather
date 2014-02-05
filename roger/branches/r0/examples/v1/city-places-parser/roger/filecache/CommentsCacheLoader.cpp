namespace filecache;

using std::ifstream;
using std::string;

/**
 * Загружает комментарии из кэша
 */
struct CommentsCacheLoader {
public:
  //загружает комментарии с локального диска.
  //url - url поста
  //path - путь к файлу с комментариями
  //rootComment - LjPost, корень дерева комментариев
  void LoadCommentsFromCache(string url, string path, lj::LjPost* rootComment) {
    url_ = url;
    filePath_ = path;
    rootComment_ = rootComment;

    ifstream input_file;
    input_file.open(filePath_.c_str());

    string firstLine;
    getline(input_file, firstLine);

    addCommentFromCache(input_file, rootComment_, 0);
    input_file.close();
  }
private:
  //рекурсивно добавляет комментарий.
  //возвращает прочитанную строку, чтобы она не потерялась при выходе из рекурсии наверх
  // и добавилась нужному предку
  string addCommentFromCache(ifstream& file, lj::Container* parent, int level) {
    string nextLine;
    bool isReadNextFromFile = true;
    do {
      //считали следующую строчку
      if (isReadNextFromFile)
        if (!getline(file, nextLine))
          break; //EOF;

      //определили её уровень вложенности.
      int nextLevel = nextLine.find("|");
      int textBeginPos = nextLine.find("|", nextLevel + 1);
      string str_subCommentID = nextLine.substr(nextLevel + 1,
          textBeginPos - nextLevel - 1);
      int subCommentID = atoi(str_subCommentID.c_str());
      string subCommentText = nextLine.substr(textBeginPos + 1);

      //если меньше, то вышли из этого уровня рекурсии
      if (nextLevel <= level) {
        return nextLine;
      }

      lj::Comment* subComment = new lj::Comment(subCommentText, subCommentID);
      subComment->setURL(url_ + "?thread=" + str_subCommentID);
      parent->addSubComment(subComment);

      //если он больше, чем у текущего, то добавить входной коммент как подкомментарий и вызваться из него
      if (nextLevel > level) {
        nextLine = addCommentFromCache(file, subComment, level + 1);
        isReadNextFromFile = false;
      }
    } while (true);
    return string(); // Was missing!
  }

  string url_;
  string filePath_;
  lj::LjPost* rootComment_;
};
