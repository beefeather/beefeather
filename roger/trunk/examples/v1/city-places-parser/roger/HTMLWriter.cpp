namespace;

using std::ofstream;
using std::string;
using std::vector;

/**
 * сохраняет результат в виде html-таблички
 */
struct HTMLWriter {
public:
  HTMLWriter(vector<Place>& places) :
      places_(places) {
  }
  ~HTMLWriter() {
  }
  //сохраняет результат по указанному пути
  void Save(string path) {
    string TITLE = "Title";

    ofstream fout;
    fout.open(path.c_str());

    fout
        << "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n";
    fout << "<html>\n";
    fout << "<head>\n";
    fout
        << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>\n";
    fout << "\t<title>" << TITLE.c_str() << "</title>\n";
    fout << "</head>\n";
    fout << "<body>\n";

    fout << "\t<table border=\"1\" cellspacing=\"0\" cellpadding=\"0\">\n";

    fout << "\t\t<tr>\n";
    fout << "\t\t\t<td>" << "Название" << "</td>\n";
    fout << "\t\t\t<td>" << "Адрес" << "</td>\n";
    fout << "\t\t\t<td>" << "url" << "</td>\n";
    fout << "\t\t\t<td>" << "Рейтинг" << "</td>\n";
    fout << "\t\t</tr>\n";

    //все места из одного комментария будут иметь одинаковый номер в названии.
    //у мест из одного комментария всегда одинаковый url комментария
    int commentNum = 0;
    string previousURL = "";

    for (std::vector<Place>::iterator i = places_.begin(); i != places_.end();
        ++i) {
      if (previousURL != (*i).getComment()->getURL()) {
        ++commentNum;
      }
      fout << "\t\t<tr>\n";
      fout << "\t\t\t<td>" << (*i).getName() << " (" << commentNum << ")"
          << "</td>\n";
      fout << "\t\t\t<td>" << (*i).getAddress() << "</td>\n";
      fout << "\t\t\t<td> <a href=\"" << (*i).getComment()->getURL() << "#t"
          << (*i).getComment()->getID() << "\">Комментарий</a></td>\n";
      fout << "\t\t\t<td><span style=\"color:green\">+"
          << (*i).getPositiveRank() << "</span> <span style=\"color:red\">-"
          << (*i).getNegativeRank() << "</td>\n";
      fout << "\t\t</tr>\n";
      previousURL = (*i).getComment()->getURL();
    }

    fout << "\t</table>\n";

    fout << "</body>\n";
    fout << "</html>\n";

    fout.close();
  }

private:
  vector<Place>& places_;
};
