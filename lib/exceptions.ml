exception UnknownVariable of string;;
(*Should never be called once we have typechecking*)
exception WrongType;;
exception PatternUnmatched;;
exception LetRecWronglyFormed;;
(*Should go away*)
exception UnimplementedError;;