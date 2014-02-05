namespace lj;

using std::cout;
using std::endl;
using std::ifstream;
using std::istreambuf_iterator;
using std::ofstream;
using std::string;
using std::stringstream;

/**
 * Загружает комментарии из сети
 */
struct CommentsWebLoader {
public:
  //url - url поста
  //rootComment - корень дерева комментариев
  //filePathPrefix - префикс в названиях файлов кэша
  void LoadComments(string url, LjPost* rootComment, string filePathPrefix) {
    url_ = url;
    rootComment_ = rootComment;
    filePathPrefix_ = filePathPrefix;
    //example url: http://tema.livejournal.com/704158.html?page=1comments

    int postNumBeginPos = url.find(".com/");
    if (postNumBeginPos == -1)
      return;
    postNumBeginPos += 5;
    int postNumEndPos = url.find(".html", postNumBeginPos);
    postNum_ = url.substr(postNumBeginPos, postNumEndPos - postNumBeginPos);
    filePath_ = filePathPrefix_ + postNum_;

    LoadPagesFromWeb();
    LoadCommentsFromWeb();
  }
private:
  //скачивает и сохраняет в кэш все страницы поста
  void LoadPagesFromWeb() {
    //скачиваем первую страницу.
    string page;
    Network network;
    page = network.sendGET(url_);

    //ищем на ней, сколько всего страниц в посте.
    int countEndPos = 0;
    int count = 1;
    do {
      int countBeginPos = page.find("#comments'>[", countEndPos);
      if (countBeginPos == -1)
        break;

      countBeginPos += 12;
      countEndPos = page.find("]", countBeginPos);
      if (countEndPos == -1)
        break;

      count = atoi(
          page.substr(countBeginPos, countEndPos - countBeginPos).c_str());
    } while (true);
    pages_ = count;
    cout << "pages: " << count << "\n";

    //скачиваем все страницы поста.
    for (int i = 1; i <= count; ++i) {
      //generating page URL & downloading
      string currentUrl = url_ + "?page=" + intToStr(i);
      cout << "downloading & saving page: " << currentUrl << "\n";

      page = network.sendGET(currentUrl);

      //сохраняем скаченную страницу в кэш
      string currFilePath = filePath_ + "_" + intToStr(i);
      ofstream fout;
      fout.open(currFilePath.c_str());
      fout << page << endl;
      fout.close();
    }
  }
  //загружаем комментарии из страницы.
  //в ЖЖ вложенные комментарии необходимо скачивать отдельными запросами.
  //этот метод находит все невложенные комментарии в посте и для каждого из них
  //скачивает его тред, где находятся все остальные комментарии.
  void LoadCommentsFromWeb() {
    for (int i = 1; i <= pages_; ++i) {
      string tmpFilePath = filePath_ + "_" + intToStr(i);
      ifstream input_file;
      input_file.open(tmpFilePath.c_str());
      if (input_file.fail()) {   //no other pages
        break;
      }

      cout << "Page " << i << ":\n";
      string page((istreambuf_iterator<char>(input_file)),
          (istreambuf_iterator<char>()));
      input_file.close();

      bool isNextCommentExists = true;
      int commentIdEndPos = 0;
      do {
        //находим комментарий в посте
        int commentIdBeginPos = page.find("<div id='ljcmt", commentIdEndPos);
        if (commentIdBeginPos == -1) { //no other comments
          break;
        }
        commentIdBeginPos += 14;
        commentIdEndPos = page.find("'", commentIdBeginPos);

        string commentID = page.substr(commentIdBeginPos,
            commentIdEndPos - commentIdBeginPos);

        //игнорируем вложенные комментарии на данном этапе.
        //вложенность определяется аргументов width (в ЖЖ стоят пиксельные распорки)
        //usign <img src='http://l-stat.livejournal.com/img/dot.gif' height='1' width='0'>
        string commentWidthBeginTag = "height='1' width='";
        int commentWidthBeginPos = page.find(commentWidthBeginTag,
            commentIdEndPos) + commentWidthBeginTag.length();
        int commentWidthEndPos = page.find("'", commentWidthBeginPos);
        int commentWidth = atoi(
            page.substr(commentWidthBeginPos,
                commentWidthEndPos - commentWidthBeginPos).c_str());

        if (commentWidth != 0) {
          continue;  //если комментарий был вложенным, то пропускаем его сейчас.
        }

        //скачиваем тред этого комментария и запускаем парсер комментариев из него
        cout << "getting thread from comment " << commentID << '\n';
        string thread;
        string threadRequest =
            "http://tema.livejournal.com/tema/__rpc_get_thread?journal=tema&itemid="
                + postNum_ + "&thread=" + commentID;
        Network network;
        thread = network.sendGET(threadRequest);
        addCommentFromWeb(rootComment_, thread);
      } while (true);
    }   //for
  }

  //парсит комментарии из треда.
  void addCommentFromWeb(Container* parent, string thread) {
    Container* currentParent = parent;
    int parentIDEndPos = 0;
    do {
      //current comment id
      string commentIDBeginTag = "<div id='cmtbar";
      int commentIdBeginPos = thread.find(commentIDBeginTag, parentIDEndPos);
      if (commentIdBeginPos == -1)
        return; //no comments
      commentIdBeginPos += commentIDBeginTag.length();
      int commentIdEndPos = thread.find("'", commentIdBeginPos);
      string str_currCommentID = thread.substr(commentIdBeginPos,
          commentIdEndPos - commentIdBeginPos);
      int currCommentID = atoi(str_currCommentID.c_str());

      //current comment text
      string commentBeginTag = "<div class='talk-comment-box'>";
      string commentEndTag = "<p style='margin: 0.7em 0 0.2em 0'>";
      int commentBeginPos = thread.find(commentBeginTag, commentIdEndPos)
          + commentBeginTag.length();
      int commentEndPos = thread.find(commentEndTag, commentBeginPos);
      string currCommentText = thread.substr(commentBeginPos,
          commentEndPos - commentBeginPos);

      //current comment parent id
      string replyBeginTag = "?replyto";
      int replyTagBeginPos = thread.find(replyBeginTag, commentEndPos)
          + replyBeginTag.length();
      int parentIDBeginPos = thread.find("#t", replyTagBeginPos) + 2;
      parentIDEndPos = thread.find("'", parentIDBeginPos);
      int parentID =
          atoi(
              thread.substr(parentIDBeginPos, parentIDEndPos - parentIDBeginPos).c_str());

      Comment* currentComment = new Comment(currCommentText, currCommentID);
      currentComment->setURL(url_ + "?thread=" + str_currCommentID);

      //в этот момент предком может быть либо текущий комментарий, либо кто-то из его предков. находим его
      while (currentParent->getParent() != NULL) {
        if (currentParent->getID() == parentID)
          break;
        currentParent = currentParent->getParent();
      }

      currentParent->addSubComment(currentComment);
      currentParent = currentComment;
    } while (true);
  }

  string intToStr(int number) {
    stringstream ss; //create a stringstream
    ss << number; //add number to the stream
    return ss.str(); //return a string with the contents of the stream
  }

  string filePath_;
  string url_;
  string filePathPrefix_;
  LjPost* rootComment_;
  string postNum_;
  int pages_;
};
