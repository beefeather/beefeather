namespace;

using std::string;

/**
 * Место, элемент результата программы
 */
struct Place {
public:
  Place(string name, string address, lj::Comment* comment) :
      name_(name), address_(address), comment_(comment), pos_rank_(0), neg_rank_(
          0), lat_(0), lon_(0) {
  }

  //название места
  void setName(string name) {
    name_ = name;
  }
  string getName() {
    return name_;
  }

  //его адресс
  void setAddress(string address) {
    address_ = address;
  }
  string getAddress() {
    return address_;
  }

  //положительная/негативная оценки (независимы друг от друга)
  void setRank(int positive, int negative) {
    pos_rank_ = positive;
    neg_rank_ = negative;
  }
  int getPositiveRank() {
    return pos_rank_;
  }
  int getNegativeRank() {
    return neg_rank_;
  }

  //комментарий, в котором было найдено это место
  lj::Comment* getComment() {
    return comment_;
  }

  //координаты
  void setCoordinates(double lat, double lon) {
    lat_ = lat;
    lon_ = lon;
  }
  double getLat() {
    return lat_;
  }
  double getLon() {
    return lon_;
  }

private:
  string name_;
  string address_;
  lj::Comment* comment_;
  int pos_rank_;
  int neg_rank_;
  double lon_;
  double lat_;
};
