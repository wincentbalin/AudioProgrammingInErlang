-module(serial_music).
-author('Wincent Balin').
-export([main/0,play/1]).

% Amount of pitches in an octave
-define(OCTAVE, 12).

% Enforce pitch boundaires within an octave
octave_boundaries(Pitch) when Pitch > 11 ->
	octave_boundaries(Pitch - ?OCTAVE);
octave_boundaries(Pitch) when Pitch < 0 ->
	octave_boundaries(Pitch + ?OCTAVE);
octave_boundaries(Pitch) ->
	Pitch.

% Translate pitch class to note name
note_name(0) -> "C";
note_name(1) -> "C#";
note_name(2) -> "D";
note_name(3) -> "D#";
note_name(4) -> "E";
note_name(5) -> "F";
note_name(6) -> "F#";
note_name(7) -> "G";
note_name(8) -> "G#";
note_name(9) -> "A";
note_name(10) -> "A#";
note_name(11) -> "B".


% Tuple based implementation
%
%% Create a rectangular matrix
%matrix_create(Width, Height, Contents) ->
%	{matrix, {width, Width}, {height, Height}, {contents, Contents}}.
%
%matrix_create(Width, Height) ->
%	Contents = erlang:make_tuple(Width * Height, 0),
%	matrix_create(Width, Height, Contents).
%
%% Calculate index in contents of the matrix
%matrix_index(I, J, Width) -> J * Width + I + 1.
%
%% Get value from the rectangular matrix
%matrix_get(Matrix, I, J) ->
%	{matrix, {width, Width}, {height, _}, {contents, Contents}} = Matrix,
%	Index = matrix_index(I, J, Width),
%	element(Index, Contents).
%
%% Set value in the rectangular matrix
%matrix_set(Matrix, I, J, Value) ->
%	{matrix, {width, Width}, {height, Height}, {contents, Contents}} = Matrix,
%	if
%		I >= Width ->
%			bounds_error;
%		J >= Height ->
%			bounds_error;
%		true ->
%			Index = matrix_index(I, J, Width),
%			Changed_contents = setelement(Index, Contents, Value),
%			matrix_create(Width, Height, Changed_contents)
%	end.

% Create a rectangular matrix
matrix_create(Width, Height, Contents) ->
	{matrix, {width, Width}, {height, Height}, {contents, Contents}}.

matrix_create(Width, Height) ->
	Size = Width * Height,
	Contents = array:new([{size, Size}, {default, 0}, {fixed, true}]),
	matrix_create(Width, Height, Contents).

% Calculate index in contents of the matrix
matrix_index(I, J, Width) -> J * Width + I.

% Get value from the rectangular matrix
matrix_get(Matrix, I, J) ->
	{matrix, {width, Width}, {height, _}, {contents, Contents}} = Matrix,
	Index = matrix_index(I, J, Width),
	array:get(Index, Contents).

% Set value in the rectangular matrix
matrix_set(Matrix, I, J, Value) ->
	{matrix, {width, Width}, {height, Height}, {contents, Contents}} = Matrix,
	Index = matrix_index(I, J, Width),
	Changed_contents = array:set(Index, Value, Contents),
	matrix_create(Width, Height, Changed_contents).

%% Print matrix
%matrix_print(Matrix) -> 
%	{matrix, {width, Width}, {height, Height}, {contents, _}} = Matrix,
%	matrix_print(Matrix, 0, Width, 0, Height).
%
%matrix_print(_, _, _, J, Height) when J == Height ->
%	ok;
%matrix_print(Matrix, I, Width, J, Height) when I == Width ->
%	io:format("~n"),
%	matrix_print(Matrix, 0, Width, J + 1, Height);
%matrix_print(Matrix, I, Width, J, Height) ->
%	io:format("~b ", [matrix_get(Matrix, I, J)]),
%	matrix_print(Matrix, I + 1, Width, J, Height).

% Create note matrix from the given list of notes
series_matrix_create() ->
	matrix_create(?OCTAVE, ?OCTAVE).

% Enter series of notes into the series matrix
series_matrix_init(Series, Matrix) ->
	series_matrix_init(Series, Matrix, 0).

series_matrix_init([], Matrix, _) ->
	Matrix;
series_matrix_init(Series, Matrix, Index) ->
	Note = octave_boundaries(hd(Series)),
	Updated_matrix = matrix_set(Matrix, 0, Index, Note),
	series_matrix_init(tl(Series), Updated_matrix, Index + 1).

% Perform musical(!!) inversion of notes in the left column
series_matrix_invert(Matrix) ->
	{matrix, {width, Width}, {height, _}, {contents, _}} = Matrix,
	series_matrix_invert(Matrix, 1, Width).

series_matrix_invert(Matrix, Index, Width) when Index == Width ->
	Matrix;
series_matrix_invert(Matrix, Index, Width) ->
	Value1 = matrix_get(Matrix, Index - 1, 0),
	Value2 = matrix_get(Matrix, 0, Index - 1),
	Value3 = matrix_get(Matrix, 0, Index),
	Value = octave_boundaries(Value1 + Value2 - Value3),
	Updated_matrix = matrix_set(Matrix, Index, 0, Value),
	series_matrix_invert(Updated_matrix, Index + 1, Width).

% Perform musical(!!) transpositions
series_matrix_transpose(Matrix) ->
	{matrix, {width, Width}, {height, Height}, {contents, _}} = Matrix,
	Value00 = matrix_get(Matrix, 0, 0),
	series_matrix_transpose(Matrix, 1, Width, 1, Height, Value00).

series_matrix_transpose(Matrix, _, _, J, Height, _) when J == Height ->
	Matrix;
series_matrix_transpose(Matrix, I, Width, J, Height, Value00) when I == Width ->
	series_matrix_transpose(Matrix, 1, Width, J + 1, Height, Value00);
series_matrix_transpose(Matrix, I, Width, J, Height, Value00) ->
	Value1 = matrix_get(Matrix, 0, J),
	Value2 = matrix_get(Matrix, I, 0),
	Value = octave_boundaries(Value1 + Value2 - Value00),
	Updated_matrix = matrix_set(Matrix, I, J, Value),
	series_matrix_transpose(Updated_matrix, I + 1, Width, J, Height, Value00).

% Print series matrix as notes
series_matrix_print(Matrix) ->
	{matrix, {width, Width}, {height, Height}, {contents, _}} = Matrix,
	series_matrix_print(Matrix, 0, Width, 0, Height).

series_matrix_print(_, _, _, J, Height) when J == Height ->
	ok;
series_matrix_print(Matrix, I, Width, J, Height) when I == Width ->
	io:format("~n"),
	series_matrix_print(Matrix, 0, Width, J + 1, Height);
series_matrix_print(Matrix, I, Width, J, Height) ->
	Value = matrix_get(Matrix, I, J),
	Note = note_name(Value),
	io:format("~-3s", [Note]),
	series_matrix_print(Matrix, I + 1, Width, J, Height).

% Create initial matrix, an inversion and all possible transpositions, then print them
play(Initial_notes) ->
	Series_matrix = series_matrix_create(),
	Initialized_matrix = series_matrix_init(Initial_notes, Series_matrix),
	Inverted_matrix = series_matrix_invert(Initialized_matrix),
	Transposed_matrix = series_matrix_transpose(Inverted_matrix),
	series_matrix_print(Transposed_matrix).

main() ->
	io:format("Please use serial_music:play([List_of_notes])~n").

% Not needed
%
%% Print series, series after series
%print_series([]) ->
%	ok;
%print_series(Series) ->
%	print_subseries(hd(Series)),
%	print_series(tl(Series)).
%
%% Print subseries, note after note
%print_subseries([]) ->
%	io:format("~n");
%print_subseries(Subseries) ->
%	io:format(" ~s ", [note_name(hd(Subseries))]),
%	print_subseries(tl(Subseries)).
