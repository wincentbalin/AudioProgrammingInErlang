-module(input).
-author('Wincent Balin').
-export([sum_of_numbers/0]).

sum_of_numbers() ->
	First_number_input = io:get_line("Please enter a number: "),
	{First_number, _} = string:to_integer(First_number_input),
	Second_number_input = io:get_line("Please enter a second number: "),
	{Second_number, _} = string:to_integer(Second_number_input),
	Sum_of_numbers = First_number + Second_number,
	io:format("~b + ~b = ~b~n", [First_number, Second_number, Sum_of_numbers]).
	