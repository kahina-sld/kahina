:- multifile '*>'/2.

% Some Agreement Principles of Chapter 2

% Principles relating individuation mode and number

(individuation_mode_rel,
 inst:num:sg) *> non_aggregate_rel.

(individuation_mode_rel,
 inst:num:pl) *> aggregate_rel.

non_aggregate_rel *> inst:num:sg.

aggregate_rel *> inst:num:pl.


% English as a natural gender language:
% Principles relating sex/humanness/(in-)animacy and gender

male_rel *> inst:gen:masc.

female_rel *> inst:gen:fem.

non_human_rel *> inst:gen:neut.

inanimate_rel *> inst:gen:neut.

(human_rel,
 inst:gen:masc) *> male_rel.

(human_rel,
 inst:gen:fem) *> female_rel.