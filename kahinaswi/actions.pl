/*  This file contains excerpts from trace.pl, which is part of
    XPCE -- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(actions,[map_action/3,fix_action/3]).

:- use_module(library(debug)).

%%	map_action(+GuiAction, +Frame, -Action) is det.
%
%	Map the abstract action of the gui-tracer into actions for the
%	low-level tracer.  Runs in the debugged thread.
%
%	@tbd	The argument frame is not used.  Delete?

map_action(creep, _, continue) :-
	traceall.
map_action(skip, _, continue) :-
	get_tracer(selected_frame, Frame),
	prolog_frame_attribute(Frame, level, Level),
	Skip is Level + 1,
	trace,
	prolog_skip_level(_, Skip).
map_action(into, _, continue) :-
	visible(+unify),
	traceall.
map_action(leap, _, continue) :-
	prolog_skip_level(_, very_deep),
	notrace.
map_action(retry, _, retry(Frame)) :-
	traceall,
	get_tracer(selected_frame, Frame).
map_action(fail, _, fail) :-
	traceall.
map_action(nodebug, _, nodebug).
map_action(leap, _, continue) :-
	notrace.
map_action(abort, _, abort).
map_action(halt, _, continue) :-
	halt.
map_action(finish, _, continue) :-
	get_tracer(selected_frame, Frame),
	prolog_frame_attribute(Frame, level, Level),
	trace,
	prolog_skip_level(_, Level).

%%	traceall is det.
%
%	Go into non-skipping trace mode.

traceall :-
	prolog_skip_level(_, very_deep),
	trace.

fix_action(fail, skip,   creep) :- !.
fix_action(exit, skip,   creep) :- !.
fix_action(_,    Action, Action).
