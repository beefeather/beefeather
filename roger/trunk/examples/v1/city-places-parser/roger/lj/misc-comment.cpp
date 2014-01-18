namespace lj;

using std::string;
using std::vector;

/**
 * Базовый класс - контейнер, содержащий подэлементы
 */
struct Container {
  vector<Comment*> getSubComments() {
    return subComments_;
  }
  void addSubComment(Comment* subComment) {
    subComment->setParent(this);
    subComments_.push_back(subComment);
  }
  virtual int getID() = 0;
  virtual Container* getParent() = 0;
  ~Container() {
    for (vector<Comment*>::iterator i = subComments_.begin();
        i != subComments_.end(); ++i) {
      delete *i;
    }
  }
private:
  vector<Comment*> subComments_;
};

/**
 * Класс поста - используется в качестве rootComment, т.е. корень всех-всех комментариев поста. Также содержит название города.
 */
class LjPost: public Container {
public:
  int getID() {
    return -1;
  }
  string getCity() {
    return city_;
  }
  void setCity(string city) {
    city_ = city;
  }
  Container* getParent() {
    return NULL;
  }
private:
  string city_;
};

/**
 * Класс комментария - содержит исследуемый текст, url комментария.
 */
struct Comment: public Container {
public:
  Comment(string text = "", int id = 0) :
      text_(text), id_(id), parent_(NULL) {
  }
  string getURL() {
    return url_;
  }
  void setURL(string url) {
    url_ = url;
  }
  string getText() {
    return text_;
  }
  int getID() {
    return id_;
  }
  Container* getParent() {
    return parent_;
  }
  void setParent(Container* parent) {
    parent_ = parent;
  }
private:
  string text_;
  string url_;
  int id_;
  Container* parent_;

public:
};
