-module(transpose).
-author('Wincent Balin').
-export([main/0]).

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

% Translate note name to pitch class
note_pitch("C") -> 0;
note_pitch("C#") -> 1;
note_pitch("D") -> 2;
note_pitch("D#") -> 3;
note_pitch("E") -> 4;
note_pitch("F") -> 5;
note_pitch("F#") -> 6;
note_pitch("G") -> 7;
note_pitch("G#") -> 8;
note_pitch("A") -> 9;
note_pitch("A#") -> 10;
note_pitch("B") -> 11.

% Enforce pitch boundaires
enforce_boundaries(Pitch) when Pitch > 11 ->
	enforce_boundaries(Pitch - 12);
enforce_boundaries(Pitch) when Pitch < 0 ->
	enforce_boundaries(Pitch + 12);
enforce_boundaries(Pitch) ->
	Pitch.

main() ->
	Note_raw = io:get_line("Enter base note (capitals, use # for sharps, eg. A#): "),
	Note = lists:filter(fun(X) -> lists:member(X, "CDEFGHAB#") end, Note_raw),
	Interval_raw = io:get_line("Enter interval in semitones: "),
	{Interval, _} = string:to_integer(Interval_raw),
	% Find base note
	Base_note = note_pitch(Note),
	% Perform transposition
	Transposed_note = enforce_boundaries(Base_note + enforce_boundaries(Interval)),
	% Print results
	io:format("~s transposed by ~b semitones is ~s~n", [Note, Interval, note_name(Transposed_note)]).
	