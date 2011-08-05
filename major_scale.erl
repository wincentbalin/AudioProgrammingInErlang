-module(major_scale).
-author('Wincent Balin').
-export([main/0]).

% Translate pitch class to note name
note_name(0) -> "C";
note_name(1) -> "Db";
note_name(2) -> "D";
note_name(3) -> "Eb";
note_name(4) -> "E";
note_name(5) -> "F";
note_name(6) -> "Gb";
note_name(7) -> "G";
note_name(8) -> "Ab";
note_name(9) -> "A";
note_name(10) -> "Bb";
note_name(11) -> "B".

% Enforce pitch boundaires
enforce_boundaries(Pitch) when Pitch > 11 ->
	enforce_boundaries(Pitch - 12);
enforce_boundaries(Pitch) when Pitch < 0 ->
	enforce_boundaries(Pitch + 12);
enforce_boundaries(Pitch) ->
	Pitch.

% Build scale
build_scale(Key) ->
	build_scale(Key, "", 0).

build_scale(Key, Scale, Index) when Index < 7 ->
	Pitch_step =
		case Index of
			2 -> 1;
			_ -> 2
		end,
	Next_key = (Key + Pitch_step) rem 12,
	Next_scale = lists:concat([Scale, note_name(Key), " "]),
	build_scale(Next_key, Next_scale, Index + 1);
build_scale(_, Scale, _) ->
	Scale.

main() ->
	Note_input = io:get_line("Please enter the key (in pitch-class number, 0-11): "),
	{Note_raw, _} = string:to_integer(Note_input),
	% Make sure start note is within boundaries
	Note = enforce_boundaries(Note_raw),
	io:format("~s~n", [build_scale(Note)]).
	