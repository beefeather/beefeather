namespace;

using std::ofstream;
using std::string;
using std::vector;

/**
 * сохраняет результат в kml-файл, пригодный к просмотру в google maps / google earth
 */
struct KMLWriter {
public:
  KMLWriter(vector<Place>& places) :
      places_(places) {
  }
  ~KMLWriter() {
  }
  //сохраняет по указанному пути
  void Save(string path) {
    ofstream fout;
    fout.open(path.c_str());

    fout << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    fout << "<kml>\n";
    fout << "<Document>\n";

    //все места из одного комментария будут иметь одинаковый номер в названии.
    //у мест из одного комментария всегда одинаковый url комментария
    int commentNum = 0;
    string previousURL = "";

    for (std::vector<Place>::iterator i = places_.begin(); i != places_.end();
        ++i) {
      if (previousURL != (*i).getComment()->getURL()) {
        ++commentNum;
      }
      fout << "\t<Placemark>\n";
      fout << "\t\t<address>\n";
      fout << "\t\t\t" << (*i).getAddress() << "\n";
      fout << "\t\t</address>\n";
      fout << "\t\t<name>" << (*i).getName() << " (" << commentNum
          << ")</name>\n";

      fout << "\t\t<description>\n";
      fout << "\t\t<![CDATA[\n";
      fout << "\t\t\t" << (*i).getAddress() << "\n";
      fout << "\t\t\t</br>Оценка: <span style=\"color:green\">+"
          << (*i).getPositiveRank() << "</span> <span style=\"color:red\">-"
          << (*i).getNegativeRank() << "</span> \n";

      fout << "\t\t\t</br><a href=\"" << (*i).getComment()->getURL() << "#t"
          << (*i).getComment()->getID() << "\">Ссылка на комментарий</a>\n";
      fout << "\t\t\t</br>" << (*i).getComment()->getText() << "\n";
      fout << "\t\t]]>\n";
      fout << "\t\t</description>\n";

      fout << "\t\t<Point>\n";
      fout << "\t\t\t<coordinates>" << (*i).getLon() << "," << (*i).getLat()
          << "</coordinates>\n";
      fout << "\t\t</Point>\n";

      fout << "\t</Placemark>\n";
      previousURL = (*i).getComment()->getURL();
    }

    fout << "</Document>\n";
    fout << "</kml>\n";

    fout.close();
  }
private:
  vector<Place>& places_;
};
