%{
%}
%require "3.2"
%language "c++"



%code
{
// Print a list of strings.
	auto operator<< (std::ostream& o, const std::vector<std::string>& ss)
		-> std::ostream&
	{
		o << '{';
		const char *sep = "";
		for (const auto& s: ss)
		{
			o << sep << s;
			sep = ", ";
		}
		return o << '}';
	}

	  namespace yy
	  {
		  // Return the next token.
		  auto yylex () -> parser::symbol_type
		  {
			  static int count = 0;
			  switch (int stage = count++)
			  {

			  case 0:
				  return parser::make_TEXT ("I have three numbers for you.");

			  case 1: case 2: case 3:
				  return parser::make_NUMBER (stage);

			  case 4:
				  return parser::make_TEXT ("And that's all!");

			  default:
				  return parser::make_YYEOF ();

			  }
		  }
	  }

}


%define api.value.type variant
%define api.token.constructor

%nterm <std::vector<std::string>> list;
%nterm <std::string> item;
%token <std::string> TEXT;
%token <int> NUMBER;

%%
result:
	list  { std::cout << $1 << '\n'; }
	;
	

list:
	 %empty     { /* Generates an empty string list */ }
      |	 list item  { $$ = $1; $$.push_back ($2); }
      ;

item:
  TEXT
  | NUMBER  { $$ = std::to_string ($1); }
;

%%

namespace yy
{
  // Report an error to the user.
    auto parser::error (const std::string& msg) -> void
      {
          std::cerr << msg << '\n';
      }
}

int main ()
{
  yy::parser parse;
    return parse ();
    }
