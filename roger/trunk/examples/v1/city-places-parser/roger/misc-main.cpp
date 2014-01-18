namespace;

using std::cerr;
using std::cout;
using std::string;
using std::vector;

int main(int argc, char *argv[]) {
  if (argc != 3) {
    cerr << " Usage: cityPlacesParser lj_URL CityName";
    return 1;
  }

  //examples:
  //string url = "http://tema.livejournal.com/704158.html";
  //string cityName = "Ереван";

  //string url = "http://tema.livejournal.com/685479.html";
  //string cityName = "London";

  //string url = "http://tema.livejournal.com/570773.html";
  //string cityName = "Санкт-Петербург";

  string url(argv[1]);
  string cityName(argv[2]);

  cout << url << "\n";
  cout << cityName << "\n";

  //download post
  CommentsLoader commentsLoader;
  lj::LjPost rootComment;
  rootComment.setCity(cityName);

  commentsLoader.LoadComments(url, &rootComment);
  printAllSubcomments(&rootComment, 1);

  //find places
  vector<Place> places;

  CommentsProcessor ljProcessor;
  ljProcessor.run("processLjComment.js", &rootComment, &places);

  //save result
  HTMLWriter htmlWriter(places);
  htmlWriter.Save("result.html");

  KMLWriter kmlWriter(places);
  kmlWriter.Save("result.kml");

  return 0;
}

//выведет на экран красиво отформатированный список всех комментариев
void printAllSubcomments(lj::Container* container, int includeLevel) {
  vector<lj::Comment*> subComments = container->getSubComments();
  for (vector<lj::Comment*>::iterator it = subComments.begin();
      it != subComments.end(); ++it) {
    lj::Comment* rootComment = *it;
    for (int i = 0; i != includeLevel; ++i)
      cout << '-';

    cout << rootComment->getText() << "\n";
    printAllSubcomments(rootComment, includeLevel + 1);
  }
}
