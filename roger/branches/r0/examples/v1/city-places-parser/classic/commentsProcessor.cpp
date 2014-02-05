#include "commentsProcessor.h"

void CommentsProcessor::run(string scriptPath, LjPost* rootComment, vector<Place>* places){
	filePath_ = scriptPath;
	rootComment_ = rootComment;
	places_ = places;

	//reading js code
	ifstream input_file;
   	input_file.open(filePath_.c_str());
	string jsCode ( (istreambuf_iterator<char>(input_file)),
                    (istreambuf_iterator<char>()) );
	input_file.close();
	jsCode_ = jsCode;

	//Устанавливаем имя города в геокодер
	geocoder_.setCityName(rootComment->getCity());	
	engine_.setGeocoder(&geocoder_);	
	//инициализируем работу с js
	engine_.init(jsCode_, places);

	//looking all comments
	vector<Comment*> subComments = rootComment->getSubComments();
	int posRank = 0;
	int negRank = 0;
	for(vector<Comment*>::iterator it = subComments.begin(); it != subComments.end(); ++it){	
		processSubComments((*it), &posRank, &negRank);
	}	
}

void CommentsProcessor::processSubComments(Comment* comment, int* out_posRank, int* out_negRank){
	vector<Comment*> subComments = comment->getSubComments();

	//для получение значения из jsEngine
	int curr_posRank = 0; 
	int curr_negRank = 0;

	//сумма оценок подкомментариев
	int subCommentsPosRank = 0;	
	int	subCommentsNegRank = 0;

	//сперва смотрим всё глубже
	for(vector<Comment*>::iterator it = subComments.begin(); it != subComments.end(); ++it){
		processSubComments((*it), out_posRank, out_negRank);
		subCommentsPosRank += *out_posRank;
		subCommentsNegRank += *out_negRank;		
	}

	//выставляем всем местам рейтинг = сумме рейтингов всех подкомментариев
	engine_.runJS(comment, subCommentsPosRank, subCommentsNegRank, &curr_posRank, &curr_negRank);		
	//а заодно выдираем наружу эту сумму + то, что было в только что обработанном комментарии
	*out_posRank = curr_posRank + subCommentsPosRank;
	*out_negRank = curr_negRank + subCommentsNegRank;	

}
