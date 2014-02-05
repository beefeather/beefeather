namespace filecache;

using std::ofstream;
using std::string;
using std::vector;

/**
 * Сохраняет комментарии в кэш на локальный диск
 */
struct CommentsCacheSaver {
public:
  //сохраняет комментарии
  //path - путь, куда сохраняет
  //rootComment - корень дерева комментариев.
  void SaveComments(string path, lj::LjPost* rootComment) {
    filePath_ = path;
    rootComment_ = rootComment;
    ofstream fout;
    fout.open(filePath_.c_str());
    fout << "|-1|\n"; //корень дерева комментариев
    SaveContainer(fout, rootComment_, 0);
    fout.close();
  }
private:
  //сохраняет себя
  void SaveSubcomment(ofstream& file, lj::Comment* rootComment,
      int includeLevel) {
    for (int i = 0; i != includeLevel; ++i)
      file << '-';

    file << "|" << rootComment->getID() << "|" << rootComment->getText()
        << "\n";
    SaveContainer(file, rootComment, includeLevel);
  }
  //сохраняет все свои подкомментарии
  void SaveContainer(ofstream& file, lj::Container* rootComment,
      int includeLevel) {
    vector<lj::Comment*> subComments = rootComment->getSubComments();
    for (vector<lj::Comment*>::iterator it = subComments.begin();
        it != subComments.end(); ++it)
      SaveSubcomment(file, *it, includeLevel + 1);
  }

  string filePath_;
  lj::LjPost* rootComment_;
};
