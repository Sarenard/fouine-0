exception UnknownVariable of string;;
(*Should never be called once we have typechecking*)
exception WrongType;;
exception Unreachable;;
exception PatternUnmatched;;
exception LetRecWronglyFormed;;
(*Should go away*)
exception UnimplementedError;;
exception Not_unifyable;;
exception Not_inferable;;
exception FreePolyVar;;
