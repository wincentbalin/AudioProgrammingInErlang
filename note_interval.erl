-module(note_interval).
-author('Wincent Balin').
-export([main/0]).

% Translate from note to pitch class
note_to_pitch($C) -> 0;
note_to_pitch($c) -> 0;
note_to_pitch($D) -> 2;
note_to_pitch($d) -> 2;
note_to_pitch($E) -> 4;
note_to_pitch($e) -> 4;
note_to_pitch($F) -> 5;
note_to_pitch($f) -> 5;
note_to_pitch($G) -> 7;
note_to_pitch($g) -> 7;
note_to_pitch($A) -> 9;
note_to_pitch($a) -> 9;
note_to_pitch($B) -> 11;
note_to_pitch($b) -> 11;
note_to_pitch(Wrong_note) ->	% Defensive porogramming is bad! (Usually...)
	io:format("Error: ~p is not a natural note!~n", [[Wrong_note]]),
	false.

% Translate amount of semitones to interval description
interval_description(0) -> "Unison";
interval_description(1) -> "Minor 2nd up or major 7th down";
interval_description(2) -> "Major 2nd up or minor 7th down";
interval_description(3) -> "Minor 3rd up or major 6th down";
interval_description(4) -> "Major 3rd up or minor 6th down";
interval_description(5) -> "Perfect 4th up or perfect 5th down";
interval_description(6) -> "Augmented 4th";
interval_description(7) -> "Perfect 5th up or perfect 4th down";
interval_description(8) -> "Minor 6th up or major 3rd down";
interval_description(9) -> "Major 6th up or minor 3rd down";
interval_description(10) -> "Minor 7th up or major 2nd down";
interval_description(11) -> "Major 7th up or minor 2nd down".

% Ensure boundaries of the interval
correct_interval(Interval) when Interval < 0 ->
	correct_interval(Interval + 12);
correct_interval(Interval) when Interval > 11 ->
	correct_interval(Interval - 12);
correct_interval(Interval) ->
	Interval.

main() ->
	% Input notes
	io:format("Please enter two natural notes:~n"),
	First_note_input = io:get_line("First note: "),
	First_note = hd(First_note_input),
	Second_note_input = io:get_line("Second note: "),
	Second_note = hd(Second_note_input),
	% Translate from note to pitch class
	First_pitch = note_to_pitch(First_note),
	Second_pitch = note_to_pitch(Second_note),
	% Calculate interval
	Simple_interval = Second_pitch - First_pitch,
	Interval = correct_interval(Simple_interval),
	% Print amount of semitones
	Semitones_up = Interval,
	Semitones_down =
		case Interval of
			0 -> 0;
			Other -> 12 - Other
		end,
	io:format("~b semitones up or ~b semitones down~n", [Semitones_up, Semitones_down]),
	% Print interval name
	io:format("~s~n", [interval_description(Interval)]).
	