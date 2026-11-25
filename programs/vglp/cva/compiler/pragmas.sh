#!/bin/bash
# The pragmas of a contract, as the term vGLP's compiler reads them (README.md beside this file, "The pragmas"; vGLP, Section 5.4, "The contract's words"; GSC, Section 2, its first paragraph): the lines after the line "Pragmas" of the contract's NAME.txt and of the NAME.txt of every contract it extends, by its "% Extends:" line and theirs, the contracts extended first.  A contract whose own NAME.txt has no line "Pragmas" has none, no_pragmas, which the compiler refuses: "the compiler prints a platform only from a contract with pragmas, and refuses one with none" (vGLP ffcb093, Section 5.4, "The contract's words").  A contract "has their tabs with its own, a tab of an inherited tab's name replacing it" (vGLP 14d9064, Section 5.4, "The contract's words", its last sentence): the tab it writes stands in the place of the inherited one, which is gone.  A button is of the acts it names, and a button replaces a button (vGLP 4236349, Section 5.4, "The contract's words", its last sentence): "an inherited button, with its fields and its questions, stands while the contract has an act it names, an act the contract replaces being gone and its button with it; and where the contract writes a button of an inherited button's name, its button, with its fields and its questions, replaces the inherited one, the other buttons of the inherited line standing".  A line's fields name, in the schemas' order, the variables of its button's acts that the atoms each requires at the proposer's role do not bind, and a line whose fields are not these is refused (ibid.).  The fields after a button, up to the next button, are that button's, and two lines that stand writing one button are refused, one name being one button with one line (ibid.: "for each act they propose, its button, in brackets, bearing the act's name, and its fields, in angle brackets"; "A button is of the acts it names").
#
#   bash programs/vglp/cva/compiler/pragmas.sh <NAME.txt> [<contract term> | @<file>]
#
# The contract term, as GSC's as_read/2 prints it, is the second argument, or, written @<file>, is read from that file: a term over 128 KB cannot be passed as an argument on Linux.
#
# prints pragmas(Lists, Acts) or no_pragmas on one line, each string as its GLP source text inside a quoted name, since the compiler writes a word as it is, the reader reading the double-quoted string back as the word.  The lines after "Pragmas" up to the first that names a clause are lists, one for each tab, "Name: what it lists", list(Name, Words); every other line is a clause's, in GSC's form, its proposer, its buttons and fields, and its indented lines (README.md beside this file, 1), each act act(Schema, Button, Fields, Sentence, Tab, Proposer), Sentence said(Questions) or none, a question's words text(S) or person(I), I the rank among the schema's persons of the role the name bears, by the names the schema term gives its roles (GSC 25b6b3c, Section 2), Proposer by([G]).  A contract extended is found beside the contract, or else under programs/ by its NAME.txt.  Exits 1, naming the line, on a pragma it cannot read or a clause it cannot find, and on every line it refuses, each named with the variable its fields leave unnamed or the field beyond them; 2 on a bad argument.  With PRAGMAS_LINES set it prints on the standard error each button line and the variables its fields name by the rule, or why it is refused, replaced or gone.

# A NAME.txt may carry two kinds of pragmas under their headings, "Interface pragmas" and "Implementation pragmas", "Pragmas" alone meaning the interface ones (GSC, sections/02-contracts.tex:3; GSA, sections/s3-super-app.tex:69): the interface pragmas are read as above, and the implementation pragmas are skipped, no paper yet saying what form they take in the app's code.

# With --from, prints instead the NAME.txt of each contract the contract is written from, on its line saying so (relation_names below) or its line % Extends:, and of each that one is written from, and so on, one to a line, each before the contracts written from it, the contract itself last: what cascade.sh composes contracts over (README.md beside this file, "Composition").
#
#   bash programs/vglp/cva/compiler/pragmas.sh --from <NAME.txt>
set -u
FROM=""
if [ "${1:-}" = "--from" ]; then FROM=1; shift; fi
[ $# -ge 1 ] && [ $# -le 2 ] && [ -f "$1" ] || { echo "usage: pragmas.sh [--from] <NAME.txt> [<contract term> | @<file holding it>]" >&2; exit 2; }
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
exec perl -CSD -Mutf8 -e '
use strict; use warnings; use Cwd ();
my ($top, $programs, $term, $from) = @ARGV;
# The names of the contracts, read from the table of calculus/NAMES.txt, "name on main | name | file name | row and atom", or "name on main | name | file name | row and atom | row and atom on main", its fifth column "the row and atom on main today, before the renaming" (calculus/NAMES.txt, Calculus-3 d15a5308), a line of four columns or five read alike, the one source of the names: "How a reader knows a name: a name is one of the names of this table and nothing else.  Match the known names longest first, so that \"Community Bank restricts Bank and extends Coins.\" names Community Bank, Bank and Coins, and \"Trading Company and Bank\" two contracts" (calculus/NAMES.txt).  A name of the first column, the name a contract has on main, is the name of its file, as Friendship.txt; a name of the second column is the name of the file of its third, "the name with a hyphen for each space and no apostrophe", as Community-Bank.txt (ibid.).  $file_of{N} is the file name of the contract named N, $name_of_file{F} the name of the contract whose file name is F, and $known every name, the longest first.  The table is calculus/NAMES.txt beside programs/, or the file CONTRACT_NAMES names, a table of the same form, as the suite gives its fixtures theirs.
my (%file_of, %name_of_file);
{
    my $tf = $ENV{CONTRACT_NAMES} // "$programs/../calculus/NAMES.txt";
    open(my $nh, "<:encoding(UTF-8)", $tf) or die "pragmas.sh: $tf: the table of the names of the contracts cannot be read\n";
    my $in = 0;
    while (my $l = <$nh>) {
        chomp $l;
        my @c = map { s/^\s+|\s+$//gr } split /\|/, $l, -1;
        next unless @c == 4 || @c == 5;
        if (!$in) { $in = 1 if $c[0] eq "name on main"; next }
        next if grep { $_ eq "" } @c;
        $file_of{$c[0]} //= $c[0]; $name_of_file{$c[0]} //= $c[0];
        $file_of{$c[1]} //= $c[2]; $name_of_file{$c[2]} //= $c[1];
    }
    close $nh;
    %file_of or die "pragmas.sh: $tf: no table of the names of the contracts in it\n";
}
my $known_alts = join("|", map { quotemeta } sort { length($b) <=> length($a) || $a cmp $b } keys %file_of);
my $known = qr/(?<![A-Za-z0-9-])(?:$known_alts)(?![A-Za-z0-9-])/;
# A contract named by its file, as the lines this prints show it: the name the table gives that file, or else the file name.
sub shown_name { my ($f) = @_; (my $b = $f) =~ s{.*/}{}; $b =~ s/\.txt$//; return $name_of_file{$b} // $b }
# The term may come in a file, its second argument then @<file>: a term over 128 KB cannot be one argument of a command on Linux (MAX_ARG_STRLEN), as WC\x27s and SC\x27s are (Calculus #6 and Helper #8, vGLP_inbox, 2026-11-15 22:48 UTC).
if ($term =~ /^\@(.+)$/s) { my $tf = $1; open(my $th, "<:encoding(UTF-8)", $tf) or die "pragmas.sh: $tf: the file of the term cannot be read\n"; local $/; $term = <$th>; close $th; $term =~ s/\s+\z//; }
# The schemas of the contract term, where it is given, each with the variables of its head after its roles that the request names, in order: a button names the schemas bearing its name or its name with a suffix after an underscore, as swap_cc of [Swap] (GSC, Code_inbox.md, 2026-10-24 08:06 UTC).  A variable local to a role, agent, speech-act or numeric, one that occurs only in atoms the schema requires or deletes at that one role, and in the constraint, is not named by the request, and no field names it (vGLP 8a3ae79, Section 5.4, "Speech-act schemas as local specifications": "A schema-binding does not bind a local variable: the machine of the person in that role finds a value for it in its own state.  Every other variable is bound by the schema-binding, and a request names it"), as t, the date of the issuer, in the redeem_bc of GB, and d, the person Bob points to with the label, in the lpoint_friend of GDO-L.
# The arguments of the term at the top level of the text $s, split at its commas outside brackets.
sub top_args {
    my ($s) = @_;
    my (@a, $d, $cur); $d = 0; $cur = "";
    for my $ch (split //, $s) {
        if ($ch eq "," && $d == 0) { push @a, $cur; $cur = ""; next }
        $d++ if $ch eq "(" || $ch eq "[";
        $d-- if $ch eq ")" || $ch eq "]";
        $cur .= $ch;
    }
    push @a, $cur if $cur =~ /\S/;
    s/^\s+|\s+$//g for @a;
    return @a;
}
# The text inside the brackets that open at position $i of $s, and the position after them.
sub inside {
    my ($s, $i) = @_;
    my $d = 0;
    for (my $j = $i; $j < length $s; $j++) {
        my $ch = substr($s, $j, 1);
        $d++ if $ch eq "(" || $ch eq "[";
        $d-- if $ch eq ")" || $ch eq "]";
        return (substr($s, $i + 1, $j - $i - 1), $j + 1) if $d == 0;
    }
    return ("", length $s);
}
# The variables local to a role of the schema whose arguments are @args: "A variable that occurs only in atoms required or deleted at one role, and in the constraint and the safe formulas, is local to it; and a variable is also local to a role, and read there, where atoms the schema requires at that role hold it, no atom it deletes, forbids or adds at that role and no atom it requires, deletes or forbids at another role holds it, and atoms it adds at other roles do; a variable of an atom deleted at one role and added at another is not local, and the request names it" (vGLP 6abbed9, Section 5.4, "Speech-act schemas as local specifications"; local.glp, locals_of), an agent, speech-act or numeric variable alike, inside a name term or a signed term included; each named by its name, as the head writes it: kept or deleted at one role and nowhere else, forbidden nowhere and listed by no listed role, and added nowhere, or deleted nowhere and added at other roles only.
sub var_tokens {
    my ($t) = @_;
    $t =~ s/\batom\(\w+,/atom(/g;
    return map { (my $x = $_) =~ s/^\x27|\x27$//g; $x } ($t =~ /(\x27(?:[^\x27\\]|\\.)*\x27|\b[A-Za-z_]\w*\b)(?!\s*\()/g);
}
sub local_vars {
    my ($roles) = @_;
    $roles =~ s/^\[//; $roles =~ s/\]$//;
    my (%at, %elsewhere, %del, %add, %delat, $lb);
    my $r = 0;
    for my $spec (top_args($roles)) {
        $r++;
        my ($f) = $spec =~ /^(c?rs)\(/ or next;
        my ($body) = inside($spec, length $f);
        my @p = top_args($body);
        if ($f eq "crs") { my $kind = shift @p; $elsewhere{$_} = 1 for var_tokens($kind); $lb //= $1 if $kind =~ /^q?listed\((\d+)/ }
        # The variables among the arguments of the atoms the role deletes, a cleared atom, with the wildcard, and a multiplicity apart (vGLP, Section 5.4: a variable of an atom the schema deletes at its base is no local variable).
        if (defined $p[2]) {
            (my $dl = $p[2]) =~ s/^\[//; $dl =~ s/\]$//;
            for my $m (top_args($dl)) {
                next if $m =~ /\bany\b|\bext_any\(/;
                1 while $m =~ s/^\s*(?:times|rest)\((.*)\)\s*$/ (top_args($1))[-1] /e;
                $delat{$_}{$r} = 1 for var_tokens($m);
            }
        }
        for my $k (1 .. $#p) {
            # A cleared atom, its wildcard among its arguments, requires nothing, so names no variable local to the role (GSC, Definition "Speech-act schema"; Calculus #11, vGLP_inbox.md, 2026-11-24 18:42 UTC: Commercial Bank\x27s Loan clears every vote_lend(Orga, _, Bob, Sum, Day) "and it requires none; so it binds neither Sum nor Day").
            my $pk = $p[$k];
            if ($k == 2) { (my $dl = $pk) =~ s/^\[//; $dl =~ s/\]$//; $pk = join(", ", grep { !/\bany\b|\bext_any\(/ } top_args($dl)) }
            for my $x (var_tokens($pk)) {
                if ($k == 2 || $k == 3) { $at{$x}{$r} = 1 }
                if ($k == 2) { $del{$x} = 1 }
                if ($k == 1) { $add{$x}{$r} = 1 }
                if ($k == 4) { $elsewhere{$x} = 1 }
            }
        }
    }
    # Each local variable with 0, or, where it is read at its role (vGLP 6abbed9, Section 5.4), that role; and the base of the schema\x27s listed roles, where it has one.
    my @out;
    for my $x (keys %at) {
        next if $elsewhere{$x} || keys %{ $at{$x} } != 1;
        my ($r0) = keys %{ $at{$x} };
        if (!$add{$x}) { push @out, [$x, 0, $delat{$x}{$r0} ? $r0 : 0] }
        elsif (!$del{$x} && !$add{$x}{$r0}) { push @out, [$x, $r0, 0] }
    }
    return (\@out, $lb);
}
my (@snames, %svars, %preds, %locals);
# A variable read at a role is local only where that role is not the base of the schema (vGLP 7892aeb, Section 5.4: "A variable is also local to a role other than the base of the schema ... and read there"; "the request names it, as it names a variable that only the base\x27s atoms hold and other roles record, the person who proposes choosing it, as Alice chooses whom she introduces"): the base the base of its listed roles or the role its line names before its button, the variable read at the base put back among the request\x27s variables once that line is read.
my (%readat, %lbase, %hvars, %based, %delbase);
sub base_read {
    my ($S, $pl) = @_;
    return if $based{$S}++;
    my $b = $lbase{$S} // $pl;
    my $back = 0;
    for my $v (keys %{ $readat{$S} || {} }) { next unless $readat{$S}{$v} == $b && $locals{$S}{$v}; delete $locals{$S}{$v}; $back = 1 }
    # A variable of an atom the schema deletes at its base is no local variable: "the person who proposes chooses which of their own records the act removes, so the schema-binding binds that variable and the request names it" (vGLP, Section 5.4, "Speech-act schemas as local specifications").
    for my $v (keys %{ $delbase{$S} || {} }) { next unless $delbase{$S}{$v} == $b && $locals{$S}{$v}; delete $locals{$S}{$v}; $back = 1 }
    $svars{$S} = [ grep { !$locals{$S}{$_} } @{ $hvars{$S} } ] if $back;
}
$preds{$1} = 1 while defined $term && $term =~ /\batom\((\w+),/g;
if (defined $term && $term ne "") {
    while ($term =~ /\b([ncq]?schema)\(/g) {
        my $kw = $1;
        my $open = pos($term) - 1;
        my ($body, $end) = inside($term, $open);
        pos($term) = $open + 1;
        my @args = top_args($body);
        # A schema whose head names its roles, schema(Name, Names, Vars, Roles), cschema(Name, Names, Vars, Reaches, Roles) or nschema(Name, Names, Vars, Constraint, Roles) (programs/gsc/README.md), carries its variables third.
        # A schema with quantifiers, qschema(Name, Names, Vars, Bound, Constraint, Roles, Conjuncts, Forms), or with no Names (programs/gsc/self.glp), carries its roles sixth, or fifth, and not last.
        my $named = ($kw eq "schema" && @args == 4) || ($kw eq "qschema" && @args == 8) || ($kw ne "schema" && $kw ne "qschema" && @args == 5);
        my ($n, $a) = @args[0, $named ? 2 : 1];
        $n =~ s/^\x27(.*)\x27$/$1/;    # a quoted name, as \x27no-show\x27 of [No-show], is the name without its quotes
        next if !defined $a || exists $svars{$n};
        push @snames, $n;
        my $rarg = $kw eq "qschema" ? $args[$named ? 5 : 4] : $args[-1];
        my ($lv, $lb) = local_vars($rarg);
        my %local = map { $_->[0] => 1 } @$lv;
        $readat{$n} = { map { $_->[0] => $_->[1] } grep { $_->[1] } @$lv };
        # A variable of an atom deleted at the base that the constraint also names is as it was, local (local.glp, locals_of).
        my $carg = $kw eq "nschema" ? $args[$named ? 3 : 2] : $kw eq "qschema" ? $args[$named ? 4 : 3] : "";
        my %incon = map { $_ => 1 } var_tokens($carg // "");
        $delbase{$n} = { map { $_->[0] => $_->[2] } grep { $_->[2] && !$incon{$_->[0]} } @$lv };
        $lbase{$n} = $lb if defined $lb;
        $a =~ s/^\[//; $a =~ s/\]$//;
        my @hv = map { (my $x = $_) =~ s/^\x27|\x27$//g; $x } ($a =~ /\w+\((\x27(?:[^\x27\\]|\\.)*\x27|\w+)\)/g);
        $locals{$n} = { map { $_ => 1 } grep { $local{$_} } @hv };
        $svars{$n} = [ grep { !$local{$_} } @hv ];
        $hvars{$n} = [ @hv ];
    }
}
# The contract term read whole, for the roles of each schema: tparse gives a term as [functor, args] for a compound, ["[]", items] for a list, and [name] for a constant.
my $tsrc = "";
sub tparse {
    $tsrc =~ /\G\s*/gc;
    if ($tsrc =~ /\G\[/gc) {
        my @items;
        $tsrc =~ /\G\s*/gc;
        return ["[]", []] if $tsrc =~ /\G\]/gc;
        while (1) {
            push @items, tparse();
            $tsrc =~ /\G\s*/gc;
            next if $tsrc =~ /\G,/gc;
            if ($tsrc =~ /\G\|/gc) { push @items, tparse(); $tsrc =~ /\G\s*\]/gc; last }
            $tsrc =~ /\G\]/gc or die "pragmas.sh: the contract term is not read at " . pos($tsrc) . "\n";
            last;
        }
        return ["[]", \@items];
    }
    $tsrc =~ /\G(\x27(?:[^\x27\\]|\\.)*\x27|"(?:[^"\\]|\\.)*"|-?\d+|[A-Za-z_]\w*)/gc
        or die "pragmas.sh: the contract term is not read at " . (pos($tsrc) // 0) . "\n";
    my $n = $1;
    return [$n] unless $tsrc =~ /\G\(/gc;
    my @args;
    while (1) {
        push @args, tparse();
        $tsrc =~ /\G\s*/gc;
        next if $tsrc =~ /\G,/gc;
        $tsrc =~ /\G\)/gc or die "pragmas.sh: the contract term is not read at " . pos($tsrc) . "\n";
        last;
    }
    return [$n, \@args];
}
sub tstr {   # a term written back, to compare arguments
    my ($t) = @_;
    return $t->[0] if @$t == 1;
    return "[" . join(",", map { tstr($_) } @{ $t->[1] }) . "]" if $t->[0] eq "[]";
    return $t->[0] . "(" . join(",", map { tstr($_) } @{ $t->[1] }) . ")";
}
# Each schema, its roles, $roles{S}, each { guarded, listed, comps }, comps its four lists of atoms, each [P, [args]], in the order of rs/5 (added, deleted, required, forbidden); its head, $head{S}, the variables after its roles; and the names of its roles, $rnames{S}, where its head names them (GSC 25b6b3c, Section 2: "the schemas naming their roles as the clauses do").
my (%roles, %head, %rnames, %ones, %vmarks, %tabpred, @pending, %covered, @lines_out, @faults, %cons);
sub find_schemas {
    my ($t) = @_;
    return if @$t == 1;
    if ($t->[0] =~ /^[ncq]?schema$/ && @{ $t->[1][0] } == 1) {
        (my $n = $t->[1][0][0]) =~ s/^\x27(.*)\x27$/$1/;
        return if exists $roles{$n};
        my $q = $t->[0] eq "qschema";
        my $named = ($t->[0] eq "schema" && @{ $t->[1] } == 4) || ($q && @{ $t->[1] } == 8) || (!$q && $t->[0] ne "schema" && @{ $t->[1] } == 5);
        # The constraint of an nschema or a qschema, which a field does not name a variable it determines (field_keys3).
        $cons{$n} = $t->[1][$named ? 4 : 3] if $q;
        $cons{$n} = $t->[1][$named ? 3 : 2] if $t->[0] eq "nschema";
        $head{$n} = $t->[1][$named ? 2 : 1][1];
        # A name is the role name, or one(Name, Other), the role Name possibly the person of the role Other (GSC 316d875, Section 2), read as Name.
        if ($named) { $rnames{$n} = [ map { my $e = $_->[0] eq "one" ? $_->[1][0] : $_; (my $x = $e->[0]) =~ s/^\x27|\x27$//g; $x } @{ $t->[1][1][1] } ] }
        # The roles the head marks possibly one person, one(Name, Other): $ones{S}, each [role, role of Other].
        if ($named) {
            my $i = 0;
            for my $e (@{ $t->[1][1][1] }) {
                $i++;
                next unless $e->[0] eq "one";
                # Its others, one or a list (the Calculus prints one(Name, Others), GSC #19 and Calculus #8, 2026-11-17), each read as above.
                for my $o (map { (my $x = $_) =~ s/^\x27|\x27$//g; $x } ($e->[1][1][0] eq "[]" ? map { $_->[0] } @{ $e->[1][1][1] } : ($e->[1][1][0]))) {
                    my ($j) = grep { $rnames{$n}[$_ - 1] eq $o } 1 .. scalar @{ $rnames{$n} };
                    push @{ $ones{$n} }, [$i, $j] if defined $j;
                }
            }
        }
        my @rs;
        my $rterm = $q ? $t->[1][$named ? 5 : 4] : $t->[1][-1];
        for my $r (@{ $rterm->[1] }) {
            my ($kind, $g, @c) = $r->[0] eq "crs" ? @{ $r->[1] } : (undef, @{ $r->[1] });
            push @rs, { guarded => ($g->[0] eq "unguarded" ? 0 : 1),
                        listed => ((defined $kind && ($kind->[0] eq "listed" || $kind->[0] eq "qlisted")) ? 1 : 0),
                        comps => [ map { [ map { [ $_->[1][0][0], $_->[1][1][1] ] } @{ $_->[1] } ] } @c ],
                        # The same lists as parsed terms, an atom with a multiplicity, times(K, A), read as its atom A, its multiplicity K not an argument of it (GSC, Appendix "Numbers": "a multiplicity k.phi is k copies of phi").
                        atoms => [ map { [ map { $_->[0] eq "times" ? $_->[1][1] : $_ } @{ $_->[1] } ] } @c ],
                        # The multiplicities, which name their variables at the role (det, below).
                        mults => [ map { $_->[1][0] } grep { $_->[0] eq "times" } map { @{ $_->[1] } } @c ] };
        }
        $roles{$n} = \@rs;
        # The names after those of the roles are the marks of the schema\x27s agent variables that are no roles, one(Name, Other) (programs/gsc/self.glp, as_read, GLP GSC-marks f3314f4e): no role is named by one, and a role\x27s or a variable\x27s mark whose Other is no role, a variable or \x27_\x27, is $vmarks{S}, each [key, key], a key "r<i>" of the role i, "v<Name>" of the variable or "_" (vGLP, Section 5.4: "and persons of different names to different persons but for two the clause names as possibly one person").
        if ($named) {
            my $k = scalar @rs;
            my @all = @{ $t->[1][1][1] };
            splice @{ $rnames{$n} }, $k if @{ $rnames{$n} } > $k;
            $ones{$n} = [ grep { $_->[0] <= $k && $_->[1] <= $k } @{ $ones{$n} // [] } ];
            my %isrole = map { $rnames{$n}[$_ - 1] => $_ } 1 .. $k;
            my $i = 0;
            for my $e (@all) {
                $i++;
                next unless $e->[0] eq "one";
                (my $nm = $e->[1][0][0]) =~ s/^\x27|\x27$//g;
                my $me = $i <= $k ? "r$i" : "v$nm";
                for my $o (map { (my $x = $_) =~ s/^\x27|\x27$//g; $x } ($e->[1][1][0] eq "[]" ? map { $_->[0] } @{ $e->[1][1][1] } : ($e->[1][1][0]))) {
                    my $ot = $o eq "_" ? "_" : exists $isrole{$o} ? "r$isrole{$o}" : "v$o";
                    next if $me =~ /^r/ && $ot =~ /^r/;
                    push @{ $vmarks{$n} }, [$me, $ot];
                }
            }
        } else {
            # A schema that declares no names, as the compiler\x27s fixtures write it, has no names to keep apart: each agent variable possibly any of the others.
            for my $h (@{ $head{$n} // [] }) { next unless ref $h && $h->[0] eq "pvar"; (my $vn = $h->[1][0][0]) =~ s/^\x27|\x27$//g; push @{ $vmarks{$n} }, ["v$vn", "_"] }
        }
        return;
    }
    find_schemas($_) for @{ $t->[1] };
}
if (defined $term && $term ne "") { $tsrc = $term; find_schemas(tparse()) }
# A variable the constraint determines from the variables local to one role and those the schema-binding binds, that only that role’s atoms name, is local to that role, and is no place of the request: "so is a variable the constraint determines from the variables local to that role and the variables the schema-binding binds, as the count i_1 a treasurer’s state holds after minting k coins with i held before, i_1 = i + k" (vGLP 9ee6f87, Section 5.4, "Speech-act schemas as local specifications"), as local.glp’s det_locals decides it: an equality conjunct of the constraint whose one variable not local, of a coefficient not 0, is of the head.
sub con_conjs { my ($c) = @_; return () if !ref $c || @$c == 1; return (con_conjs($c->[1][0]), con_conjs($c->[1][1])) if $c->[0] eq "and"; return ($c) }
for my $n (keys %cons) {
    next unless $roles{$n};
    my (%role_of, %named_at);
    my $r = 0;
    for my $rs (@{ $roles{$n} }) {
        $r++;
        for my $k (0 .. 3) { for my $a (@{ $rs->{atoms}[$k] }) { next if $k == 1 && cleared($a); $named_at{$_}{$r} = 1 for atom_keys($a) } }
        # The variable of a multiplicity is named at its role, as k of cmint (local.glp, det_pick).
        for my $m (@{ $rs->{mults} || [] }) { $named_at{$_}{$r} = 1 for atom_keys($m) }
    }
    for my $x (keys %{ $locals{$n} || {} }) { my @rr = keys %{ $named_at{"v$x"} || {} }; $role_of{"v$x"} = $rr[0] if @rr == 1 }
    # The variables of the atoms the schema deletes, at any role, which the constraint never determines (local.glp, dels_name; Grassroots-Audit, cvac.glp.md, Finding 2).
    my %deleted;
    for my $rs (@{ $roles{$n} }) { for my $a (@{ $rs->{atoms}[1] }) { next if cleared($a); $deleted{$_} = 1 for atom_keys($a) } }
    # The variables of a cleared atom, which the request names, never local and never determined (OSPM, act-schemas-local, VEUI vglp33-determined: "a cleared atom being neither"), as Sum of Central Bank\x27s Lend, so that Interest = Sum * Rate is determined at Orga, Rate local there (local.glp, occ_roles).
    my %clr;
    for my $rs (@{ $roles{$n} }) { for my $a (@{ $rs->{atoms}[1] }) { next unless cleared($a); $clr{$_} = 1 for atom_keys($a) } }
    my $more = 1;
    while ($more) { $more = 0;
    for my $c (con_conjs($cons{$n})) {
        next unless $c->[0] eq "eq";
        my %vs = map { $_ => 1 } (expr_nvars($c->[1][0]), expr_nvars($c->[1][1]));
        my @nl = grep { !$role_of{$_} } keys %vs;
        my %lr = map { $role_of{$_} => 1 } grep { $role_of{$_} } keys %vs;
        # Of two variables or more local to no role, the one no atom names, the others bound by the schema-binding (local.glp, det_pick).
        @nl = grep { !%{ $named_at{$_} || {} } && !$clr{$_} } @nl if @nl > 1;
        next unless @nl == 1 && keys %lr == 1;
        my ($v) = @nl; my ($i) = keys %lr;
        next unless lin_coef(["minus", [$c->[1][0], $c->[1][1]]], $v) != 0;
        next if $deleted{$v};
        next if $clr{$v};
        # Named by the atoms of that role, or by none, as Rides\x27 Top = S + Total (local.glp, det_ok; vGLP #32, a5ea6b25).
        my @at = keys %{ $named_at{$v} || {} };
        next unless @at == 0 || (@at == 1 && $at[0] == $i);
        (my $vn = $v) =~ s/^v//;
        next unless grep { $_ eq $vn } @{ $svars{$n} || [] };
        next if $locals{$n}{$vn};
        $locals{$n}{$vn} = 1;
        $role_of{$v} = $i;
        $svars{$n} = [ grep { $_ ne $vn } @{ $svars{$n} } ];
        $more = 1;
    }
    }
}
# The schemas a button realises: those bearing its name, the button in lower case, or its name with a suffix after an underscore (GSC #7, GSC docs/design-decisions.md, 2026-10-24: "the schemas realising it are those bearing its name or its name with a suffix after an underscore"), as [Enter] realises enter and enter_2 and [Swap] swap_cc, but for a schema another button of the contract names by its own name, as [Post own] names post_own and [Post] does not; the button alone where the term is not given or none is found.
my %seen; my @order; my %extends;
# The words of each instantiation, by the contract that writes it and the contract it instantiates ("an organisation for a ward"), and the name the line gives each contract a file extends, by file (relation_names and visit, below), from which an inherited line\x27s names are put in the instance\x27s own (own_names, below).
my (%inst_words, %parent_name);
my %named_by_button;
# The schema a button names (README.md, 2): its text in lower case, each run of spaces an underscore, and every character that is no letter, digit, underscore or hyphen left out, as the apostrophe of [Redeem member\x27s coin], the schema redeem_members_coin (Calculus #6, vGLP_inbox.md, 2026-11-15 16:04 UTC).
sub act_name { my ($b) = @_; (my $a = lc $b) =~ s/ +/_/g; $a =~ s/[^a-z0-9_-]//g; return $a }
# The guarding roles of a schema whose persons must consent to its act and are not asked (below).
my %consent_of;
sub realising {
    my ($act) = @_;
    return ($act) if !@snames;
    my @r = grep { $_ eq $act || (index($_, "${act}_") == 0 && !$named_by_button{$_}) } @snames;
    return @r ? @r : ($act);
}
# The places of a schema’s persons, in the order of its variables, its roles then its head: its roles that are not listed roles, then its agent variables (cvac.glp, person_places).
sub person_places {
    my ($s) = @_;
    my @pp; my $i = 0;
    for my $r (@{ $roles{$s} }) { $i++; push @pp, $i unless $r->{listed} }
    for my $v (@{ $head{$s} }) { $i++; push @pp, $i if $v->[0] eq "pvar" }
    return @pp;
}
sub guarding { my ($s) = @_; my $i = 0; my @g; for my $r (@{ $roles{$s} }) { $i++; push @g, $i if $r->{guarded} } return @g }
# The NAME.txt of the contract named $name, by the file name the table gives it (calculus/NAMES.txt: "The file name is the name with a hyphen for each space and no apostrophe: NAME.txt"; a name on main the name of its file, as today): beside the contract, or else under programs/.
sub find_txt {
    my ($name, $beside) = @_;
    my $b = $file_of{$name};
    return undef unless defined $b;
    my $f = "$beside/$b.txt";
    return $f if -f $f;
    for my $g (glob("$programs/*/$b.txt"), glob("$programs/*/*/$b.txt")) { return $g }
    return undef;
}
# The headings of the pragmas (GSC, Section 2, its first paragraph, sections/02-contracts.tex:3; GSA, sections/s3-super-app.tex:69; GSC #13, Code_inbox.md, 2026-11-07 19:29 UTC, 2): a NAME.txt may carry "Interface pragmas" and "Implementation pragmas", "Pragmas" alone meaning the interface ones.  The interface pragmas are read as the pragmas were; the implementation pragmas are skipped, no paper yet saying what form they take in the app\x27s code.
sub iface_head { my ($x) = @_; return $x eq "Pragmas" || $x eq "Interface pragmas" }
sub impl_head { my ($x) = @_; return $x eq "Implementation pragmas" }
sub any_head { my ($x) = @_; return iface_head($x) || impl_head($x) }
# An adopted contract carries its Logic pragmas after its clauses, under the line "Logic pragmas", and its Interface pragmas after them (calculus/README.md, "Adopting a contract"): the contract ends at that line, and the lines from it to the next heading of pragmas are skipped, the clauses they repeat being no clauses of the contract.
sub logic_head { my ($x) = @_; return $x eq "Logic pragmas" }
sub contract_end { my ($x) = @_; return any_head($x) || logic_head($x) }
# A line a contract inherits from its base is read with the names the contract gives the persons of the acts it names, wherever they differ from the base\x27s: "the inherited line is read with Orga for Wendy wherever the instance renames the role, and every other name of the base\x27s line as the instance names it" (GSC #19, vGLP_inbox.md, 2026-11-17 12:33 UTC; GSC, Section 2: "A name in a pragma is the schema\x27s role of that name"; the Calculus\x27s Definition Operations, instantiation).  A name maps to the name at the same place of the same act\x27s schema, among its roles and among its agent variables, in the base\x27s term, its NAME.glp beside its NAME.txt, and in the contract\x27s term.
my %base_names;
# The buttons a contract replaces, by the line of its base (below).
my %replaced;
sub top_split {
    my ($s) = @_;
    my (@out, $d, $cur); $d = 0; $cur = "";
    for my $ch (split //, $s) {
        if ($ch eq "," && $d == 0) { push @out, $cur; $cur = ""; next }
        $d++ if $ch eq "(" || $ch eq "["; $d-- if $ch eq ")" || $ch eq "]";
        $cur .= $ch;
    }
    push @out, $cur if $cur =~ /\S/;
    return @out;
}
sub read_base_names {
    my ($f) = @_;
    return $base_names{$f} if exists $base_names{$f};
    my %bn;
    (my $g = $f) =~ s/\.txt$/.glp/;
    if (-f $g && open my $h, "<", $g) {
        my $t = do { local $/; <$h> };
        close $h;
        $t =~ s/^\s*%.*$//mg;
        while ($t =~ /\bsas\(\s*(\w+)\s*,\s*(?=\[)/g) {
            my $sn = $1; my $at = pos($t);
            my ($ns, $next) = inside($t, $at);
            my $rest = substr($t, $next);
            $rest =~ /^\s*,\s*\[/ or next;
            my ($vs) = inside($rest, index($rest, "["));
            my @names = map { /^\s*(?:one\(\s*)?\x27([^\x27]*)\x27/ ? $1 : () } top_split($ns);
            my @vars = map { /^\s*\x27([^\x27]*)\x27/ ? $1 : () } top_split($vs);
            my %isv = map { $_ => 1 } @vars;
            my %isn = map { $_ => 1 } @names;
            $bn{$sn} = { roles => [ grep { !$isv{$_} } @names ], vars => [ grep { $isn{$_} } @vars ] };
        }
    }
    return $base_names{$f} = \%bn;
}
# A name is renamed only where the instance renames that role, and a role both share keeps its name (GSC #19, vGLP_inbox.md, 2026-11-17 12:33 UTC): a name of the base\x27s schema maps to the name at its place in the instance\x27s schema only where the base\x27s schema bears no role or variable of the instance\x27s name and the instance\x27s bears none of the base\x27s, so that Commercial Bank\x27s expel, adding DeciderB at the place of Organisation\x27s Codecider, renames nothing.  A button the contract replaces with a line of its own is not read from the base\x27s line (vGLP, Section 5.4, "The contract\x27s words": "its button ... replaces the inherited one"), so that Joint Accounts\x27 own [Deposit] maps no name of Coins\x27 Escrow line.  The names of a button line, and of the question lines after it, which belong to it, are renamed by the schemas of that line\x27s buttons.
# The kinds the names declaration of a contract gives its names, in their order: "Alice and Bob are persons, Orga an organisation." gives Alice and Bob the kind person and Orga organisation (the Calculus, Section 1, "The names line of a contract declares the names of its own clauses"; calculus/tools/gcsas.py, kinds_line).
my %decl_kinds;
sub decl_kinds {
    my ($f) = @_;
    return $decl_kinds{$f} if exists $decl_kinds{$f};
    my @out;
    my $N = qr/[A-Z][A-Za-z]*/;
    for my $x (read_txt($f)) {
        next if $x =~ /^%/;
        last if $x =~ /^\d+\./ || contract_end($x);
        for my $l (split /\.\s{2,}/, $x) {
            next if $l =~ /\*/ or $l !~ /^$N(?:, $N)*(?: and $N)? (?:is|are) /;
            my $seg = (split /; |\. /, $l)[0]; $seg =~ s/\.\s*$//;
            while ($seg =~ /((?:$N, )*$N(?: and $N)?) (?:(?:is|are) )?((?:an? )?[a-z][A-Za-z\x27 -]*?)(?=, | and [A-Z]|$)/g) {
                my ($ns, $k) = ($1, $2); $k =~ s/^an? //;
                next if $k =~ /^possibly/;
                my @ns = ($ns =~ /($N)/g);
                $k = $k eq "children" ? "child" : $k =~ /ies$/ ? $k =~ s/ies$/y/r : $k =~ /s$/ ? $k =~ s/s$//r : $k if @ns > 1;
                push @out, [ $_, $k ] for @ns;
            }
        }
        last if @out;
    }
    return $decl_kinds{$f} = \@out;
}
# The kinds of the names of a contract and of every contract it is written from, its own first, in order.
sub kinds_from {
    my ($f, $seen) = @_;
    $seen //= {};
    return () if $seen->{$f}++;
    return (@{ decl_kinds($f) }, map { kinds_from($_, $seen) } @{ $extends{$f} // [] });
}
# The names a contract puts for the names of a contract it instantiates, its own: where it instantiates it with a kind for a kind, "an organisation for a ward", the first name the instantiated contract and those it is written from give the kind replaced, Guardians\x27 Wendy, is the last name the contract\x27s own declaration gives the kind put that they do not give, Organisation\x27s Orga: "Its conditions, persons and effects are written with the names of its own contract: Organisation:4 writes Orga, the organisation, where Guardians:2 writes Wendy, the ward" (the Calculus, Section 1, citations; calculus/tools/gcsas.py, own_for; Udi, 2026-11-17, to Calculus #8).
sub own_for {
    my ($c, $p) = @_;
    my %mp;
    my $pn = $parent_name{$c}{$p} // return \%mp;
    my @items = map { split /\s*,\s*(?:and\s+)?|\s+and\s+/ } @{ $inst_words{$c}{$pn} // [] };
    my @pk = kinds_from($p);
    my %inp = map { $_->[0] => 1 } @pk;
    for my $it (@items) {
        next unless $it =~ /^\s*(?:(?:an?|the)\s+)?(.+?)\s+for\s+(?:(?:an?|the)\s+)?(.+?)\s*$/;
        my ($new, $old) = ($1, $2);
        my ($o) = grep { !exists $mp{$_} } map { $_->[1] eq $old ? $_->[0] : () } @pk;
        my @mine = map { $_->[1] eq $new && !$inp{ $_->[0] } ? $_->[0] : () } @{ decl_kinds($c) };
        $mp{$o} = $mine[-1] if defined $o && @mine;
    }
    return \%mp;
}
# The names the contract read, $top, puts for the names of a contract $f it is written from, through every instantiation between them: the map of each step from $f up to $top applied in turn.
my %own_names;
sub own_names {
    my ($f) = @_;
    return $own_names{$f} if exists $own_names{$f};
    my @path;
    my %vis;
    my $find; $find = sub {
        my ($g) = @_;
        return 1 if $g eq $f;
        return 0 if $vis{$g}++;
        for my $h (@{ $extends{$g} // [] }) { if ($find->($h)) { unshift @path, [ $g, $h ]; return 1 } }
        return 0;
    };
    my %mp;
    if ($find->($top)) {
        for my $st (reverse @path) {
            my $m = own_for(@$st);
            $mp{$_} = $m->{ $mp{$_} } // $mp{$_} for keys %mp;
            for my $k (keys %$m) { $mp{$k} //= $m->{$k} unless grep { $_ eq $k } values %mp }
        }
    }
    delete $mp{$_} for grep { $mp{$_} eq $_ } keys %mp;
    return $own_names{$f} = \%mp;
}
sub inst_renamed {
    my ($f, $ln, $rest) = @_;
    my $bn = read_base_names($f);
    my @parts = split /\x01/, $rest, -1;
    my (@groups, $pi);
    $pi = -1;
    for my $i (0 .. $#parts) {
        my $isb = $i == 0 ? $parts[$i] =~ /^[A-Z][A-Za-z]*\s+\[/ : $parts[$i] =~ /^\s+[A-Z][A-Za-z]*\s+\[/;
        if ($isb || !@groups) { $pi++ if $isb; push @groups, { pi => ($isb ? $pi : -1), idx => [] } }
        push @{ $groups[-1]{idx} }, $i;
    }
    for my $g (@groups) {
        my %map;
        if ($g->{pi} >= 0) {
            my $k = "$f\x02$ln\x02$g->{pi}";
            my $bp = $parts[ $g->{idx}[0] ];
            while ($bp =~ /\[([^\]]+)\]/g) {
                my $sb = act_name($1); my $si = act_name(bound_button($1));
                next if $replaced{$k} && $replaced{$k}{$sb};
                next unless exists $bn->{$sb} && exists $rnames{$si};
                my @br = @{ $bn->{$sb}{roles} }; my @ir = @{ $rnames{$si} };
                my @iv = map { (ref $_ && $_->[0] eq "pvar") ? do { (my $x = $_->[1][0][0]) =~ s/^\x27|\x27$//g; $x } : () } @{ $head{$si} // [] };
                my @bv = @{ $bn->{$sb}{vars} };
                my %inb = map { $_ => 1 } (@br, @bv);
                my %ini = map { $_ => 1 } (grep { defined } @ir, @iv);
                # A name the instance puts for a name of its base by the instantiation\x27s words is renamed so first, wherever the base\x27s schema bears it and the instance\x27s bears the instance\x27s name and not the base\x27s, whatever the places of the two in the schemas (own_names, above): Guardians\x27 enter, of Wendy and Alice, is Organisation\x27s, of Alice and Orga.
                my $own = own_names($f);
                for my $o (keys %$own) { $map{$o} //= $own->{$o} if $inb{$o} && !$ini{$o} && $ini{ $own->{$o} } }
                for my $i (0 .. $#br) { next unless defined $ir[$i]; $map{ $br[$i] } //= $ir[$i] if $br[$i] ne $ir[$i] && !$ini{ $br[$i] } && !$inb{ $ir[$i] } }
                for my $j (0 .. $#bv) { next unless defined $iv[$j]; $map{ $bv[$j] } //= $iv[$j] if $bv[$j] ne $iv[$j] && !$ini{ $bv[$j] } && !$inb{ $iv[$j] } }
            }
        }
        next unless %map;
        my $alt = join("|", map { quotemeta } sort { length($b) <=> length($a) || $a cmp $b } keys %map);
        $parts[$_] =~ s/\b($alt)\b/$map{$1}/g for @{ $g->{idx} };
    }
    return join("\x01", @parts);
}
# An instance\x27s own names for the schemas of its base (GSC 22605d6, Section 8, Definition Parametric contract: "An instantiation may also write names of its own for schemas of the contract, a different one for each, so that an act bears the name its instance gives it"): the bindings of instance_named(<name>, Base, Bindings, Additions) in the NAME.glp beside the contract\x27s NAME.txt, a schema of the base bound to the instance\x27s name for it, as ward_acquaint = agent_acquaint (Agents.glp).  A button of a line the instance inherits that names a schema so bound bears the instance\x27s name, [Ward acquaint] reading as [Agent acquaint], so that an act bears the name its instance gives it and the instance\x27s own line of that button replaces it.
my %bind;
sub read_bindings {
    my ($f) = @_;
    (my $g = $f) =~ s/\.txt$/.glp/;
    return unless -f $g;
    open my $h, "<", $g or return;
    my $t = do { local $/; <$h> };
    close $h;
    $t =~ s/^\s*%.*$//mg;
    $t =~ /\binstance_named\(\s*\w+\s*,\s*\w+\s*,\s*\[/g or return;
    my ($bs) = inside($t, pos($t) - 1);
    while ($bs =~ /(\w+)\s*=\s*(\w+)/g) { $bind{$1} = $2 }
}
sub bound_button {
    my ($b) = @_;
    my $a = act_name($b);
    return $b unless exists $bind{$a};
    (my $n = $bind{$a}) =~ s/_/ /g;
    return ucfirst $n;
}
sub read_txt {
    my ($f) = @_;
    open(my $h, "<", $f) or die "pragmas.sh: cannot read $f\n";
    my @l = <$h>; close $h; chomp @l;
    if (%bind && $f ne $top) { for (@l) { next if /^%/; s{\[([^\]]+)\]}{"[" . bound_button($1) . "]"}ge } }
    return @l;
}
sub q_atom {   # a GLP quoted name holding the text $s
    my ($s) = @_; $s =~ s/\\/\\\\/g; $s =~ s/\x27/\\\x27/g; return "\x27$s\x27";
}
sub q_string { # a quoted name holding the GLP source of the string $s
    my ($s) = @_; $s =~ s/\\/\\\\/g; $s =~ s/"/\\"/g; return q_atom("\"$s\"");
}
# The contracts a contract names on the line saying what it is written from, the first after its title and before its names declaration, in GSC\x27s form (GSC df5e454, Section 2: "A contract written from others says so in its first line, after its title, by the relations of Section 8: each relation is followed by the contract it is to, or by `it\x27 for the contract named last, the relations joined by `and\x27, and an instantiation says after the contract\x27s name, between commas, what it writes for what, as ``CSSN instantiates GSNW, a child for a ward and a parent for a guardian, and restricts and extends it\x27\x27"): "<NAME> <relation> <OTHER> (...)." or with no parentheses, the relation one of GSC\x27s five, amends, extends, instantiates, implements and restricts (GSC, Section 8, Relations among Contracts, Definitions Amendment, extension, instantiation and Implementation, restriction); relations that share their contract joined by "and", as "restricts and extends it"; several contracts of one relation joined by commas and "and", as "GSNP amends GSN and GFP, and GAI."; and an instantiation\x27s words, "a child for a ward and a parent for a guardian", between commas after the contract\x27s name, which name no contract and which it may omit, as "MCS instantiates GBank and amends it." (GSC 30013e6, Section 2: "an instantiation may say after the contract\x27s name, between commas, what it writes for what"); nothing where the contract has no such line.  A name is read whole with a hyphened part, as GDO-L.
sub relation_names {
    my ($f, @l) = @_;
    my @body = grep { !/^%/ && /\S/ } @l;
    return () unless @body >= 2;
    # A name is one of the table\x27s, the longest first ($known, above; calculus/NAMES.txt), and a word that is none of them is no name: a first line "<NAME> <relation> ..." whose first words are no name is refused, as a contract it names that is none.
    my $nm = $known;
    # composes, the composition of contracts (GSC, Section Relations among Contracts), names the contracts composed, "composes A with B", A or B "it", "the instance" or "the composition" where it is what the line has named before, as "GAI instantiates GnW, composes it with GFP and restricts the composition" (Agents.txt) and "GSNP composes GAI with GSN and amends the composition" (Provenance-Groups.txt; GAI #13, Code_inbox.md, 2026-11-10 20:39 UTC).
    # relaxes, the sixth relation of the Calculus (calculus/PARTS.txt, 2: "a relaxing citation ... adds the relaxed act and keeps the act cited"), as "GC relaxes and extends GN." (Code #33, vGLP_inbox.md, 2026-11-16 16:50 UTC).
    # weakens, a relation of the Calculus (calculus/tools/adopt.py, written_from), as "Feeds extends Labelled Overlay, and weakens it." (Code #37, 2026-11-24).
    # renames, lifts and unites, relations of the Calculus, read as instantiates, extends and extends (calculus/tools/relations.py, new_verbs: "weakens as restricts, amends as restricts and strengthens, unites and lifts as extends, and renames as instantiates with the words of gr/n\x27s pragma \x27Renaming of B: an organisation for a ward.\x27"), as "Children renames Warded Groups, and extends and weakens it." and "App unites Encounters, AppStore and Directory."; "the union" what the line has named before; and a contract named as "a weakening of", "a strengthening of", "an amendment of", "a renaming of" or "a lift of" it, the contract itself, as "Provenance Groups unites Groups and a strengthening of AI Agents" (calculus/tools/relations.py: "\x27a weakening of a lift of Bonds\x27: Bonds"; Code #37, 2026-11-24).  augments, read as restricts and strengthens (relations.py, new_verbs: "augments as restricts and strengthens"), and a contract named as "an augmentation of" it, as "Rides unites Community Network and Second-Score Auction, and extends and augments the union" (Calculus 1724963; CSSN #19 and Code #37, 2026-11-24).
    # inherits from, the Calculus\x27s one first line, "NAME inherits from A, B and C.", the contracts after it read as those of extends, the words of a renaming from its pragma "Renaming of B: ..." (Calculus calc11-no-parameters fe486d2, record item 71; Code #38, 2026-11-25).
    my $rel = qr/(?:amends|extends|instantiates|implements|restricts|strengthens|relaxes|weakens|composes|renames|lifts|unites|augments|inherits\s+from)\b/;
    my $back = qr/(?:it|them|the instance|the composition|the union)/;
    my $of = qr/(?:an?\s+(?:weakening|strengthening|amendment|renaming|lift|augmentation)\s+of\s+)*/;
    # The words of a renaming, the contract\x27s pragma "Renaming of B: a child for a ward and a parent for a guardian.", as an instantiation\x27s.
    for (@body) { push @{ $inst_words{$f}{$1} }, $2 if /^Renaming of ($nm):\s*(.*?)\.\s*$/ }
    if ($body[1] !~ /^$nm\s+$rel/ && $body[1] =~ /^((?:[A-Z][^\s,]*\s+){1,2}?)$rel/) {
        (my $w = $1) =~ s/\s+$//;
        die "pragmas.sh: $f: the line saying what the contract is written from, \"$body[1]\", names no contract at \"$w\", no name of calculus/NAMES.txt\n";
    }
    return () unless $body[1] =~ /^$nm\s+($rel.*?)\.\s*$/;
    my $line = $body[1];
    my $s = $1;
    $s =~ s/\s*\([^()]*\)//g;
    my $no = sub { die "pragmas.sh: $f: the line saying what the contract is written from, \"$line\", names no contract at \"$_[0]\"\n" };
    my @names;
    while (1) {
        # The relations that share a contract: "restricts and extends".
        $s =~ s/^($rel)\s*// or die "pragmas.sh: $f: the line saying what the contract is written from, \"$line\", names no relation at \"$s\"\n";
        my @rels = ($1);
        while ($s =~ s/^(?:,\s*(?:and\s+)?|and\s+)($rel)\s*//) { push @rels, $1 }
        # The contract they are to: "it", "the instance" or "the composition", what the line has named before, or contracts joined by commas and "and"; and, of composes, the contract it is composed with after "with", either of the two named or named before.
        my $target = sub {
            if ($s =~ s/^($back)(?=$|[\s,])//) { @names or $no->($1); return }
            $s =~ s/^$of($nm)(?=$|[\s,])// or $no->($s);
            push @names, $1;
            while ($s =~ s/^(?:\s*,\s*(?:and\s+)?|\s+and\s+)$of($nm)(?=$|[\s,])//) { push @names, $1 }
        };
        $target->();
        if (grep { $_ eq "composes" } @rels) {
            $s =~ s/^\s+with\s+// or die "pragmas.sh: $f: the line saying what the contract is written from, \"$line\", says composes with no \"with\" at \"$s\"\n";
            $target->();
        }
        # An instantiation\x27s words, between commas after the contract\x27s name, up to the next relation or the end of the line, which it may omit.
        # The Calculus writes an instantiation\x27s words after "with", up to the next relation or the end of the line, as "instantiates GnW with bylaws, an organisation for a ward and an officer for a guardian, and restricts, strengthens and extends it" (calculus/README.md, "The relations").
        if (grep { $_ eq "instantiates" } @rels and !grep { $_ eq "composes" } @rels and $s =~ s/^\s+with\s+((?:(?!\s*,?\s*(?:and\s+)?$rel).)*)//) { push @{ $inst_words{$f}{ $names[-1] } }, $1 }
        elsif (grep { $_ eq "instantiates" } @rels and $s =~ s/^\s*,(?!\s*(?:and\s+)?$rel)\s*((?:(?!,\s*(?:and\s+)?$rel).)*?)\s*(?=,\s*(?:and\s+)?$rel|$)//) {
            my $w = $1;
            push @{ $inst_words{$f}{ $names[-1] } }, $w;
            $w =~ /\bfor\b/ or die "pragmas.sh: $f: the line saying what the contract is written from, \"$line\", says between commas after the contract instantiated what it writes for what, and \"$w\" does not\n";
        }
        last if $s =~ /^\s*$/;
        $s =~ s/^\s*(?:,\s*(?:and\s+)?|and\s+)(?=$rel)// or do { (my $at = $s) =~ s/^\s*,?\s*(?:and\s+)?//; $no->($at) };
    }
    return @names;
}
sub visit {
    my ($f) = @_;
    return if $seen{$f}++;
    my @l = read_txt($f);
    my ($ext) = grep { /^% Extends:/ } @l;
    my @rel = relation_names($f, @l);
    if (@rel) {
        my $dir = $f =~ m{/} ? $f =~ s{/[^/]*$}{}r : ".";   # a file named with no directory is in the current one
        my %named;
        for my $n (@rel) {
            next if $named{$n}++;
            my $g = find_txt($n, $dir);
            defined $g or die "pragmas.sh: $f: the contract $n, which its line saying what it is written from names, has no " . ($file_of{$n} // $n) . ".txt\n";
            visit($g);
            push @{ $extends{$f} }, $g;
            $parent_name{$f}{$g} //= $n;
        }
    } elsif (defined $ext) {
        my $dir = $f =~ m{/} ? $f =~ s{/[^/]*$}{}r : ".";   # a file named with no directory is in the current one
        my %named;
        # The contracts it extends are named in the first sentence of the line, a file name such as GC.txt in it not a contract extended, nor a name in a reference in parentheses (calculus/NAMES.txt: "a reference in parentheses may follow a name").
        (my $first = $ext) =~ s/^% Extends:\s*//;
        $first =~ s/\.\s.*$//;
        $first =~ s/\s*\([^()]*\)//g;
        # A name is one of the table\x27s, the longest first, and a name with a hyphened part, as GDO-L, which GFF extends (Feeds.txt), is read whole and not as GDO ($known; calculus/NAMES.txt).
        for my $n ($first =~ /($known)(?![\x27\x{2019}]s|\.\w)/g) {
            next if $named{$n}++;
            my $g = find_txt($n, $dir);
            next unless defined $g;
            visit($g);
            push @{ $extends{$f} }, $g;
        }
    }
    push @order, $f;
}
if ($from) { visit($top); print Cwd::abs_path($_) . "\n" for @order; exit 0 }
read_bindings($top);
my @own = read_txt($top);
unless (grep { iface_head($_) } @own) { print "no_pragmas\n"; exit 0 }
visit($top);
my (@tabs, @lists, @acts);
# The types the contract\x27s schemas give their speech-act variables, typed(T, x) in the clause syntax of the NAME.glp beside each NAME.txt read, one occurrence typing the variable (programs/self.glp, SArg; vGLP 37fe038, Section 5.4), by schema and variable.
my %ftype;
for my $f (@order) {
    (my $g = $f) =~ s/\.txt$/.glp/;
    next unless -f $g;
    open my $h, "<", $g or next;
    my $t = do { local $/; <$h> };
    close $h;
    $t =~ s/^\s*%.*$//mg;
    while ($t =~ /\bsas\(\s*(\w+)\s*,/g) {
        my ($n, $at) = ($1, $-[0]);
        my ($body) = inside($t, index($t, "(", $at));
        while ($body =~ /\btyped\(\s*(\w+)\s*,\s*\x27?(\w+)\x27?\s*\)/g) { $ftype{$n}{$2} //= $1 }
        pos($t) = $at + 4;
    }
}
# The names: those the contract declares, on its line after the title and before clause 1, "Alice, Bob and Carol are persons." (Udi, 2026-10-25, to GSC #8; Knowledge.txt at GLP ec7c578c), the declaration being part of the contract the reader takes; and, of a contract that declares none, the words of a capital and lower-case letters its clauses write within a sentence, not first in it.
my %is_name;
for my $f (@order) {
    my $declared = 0;
    for my $x (read_txt($f)) {
        next if $x =~ /^%/;
        last if $x =~ /^\d+\./ || contract_end($x);
        # The declaration names every name it lists, of every kind, as "Alice, Bob and Erin are persons, Carol and Dave wards." names five (vGLP 722343d, Section 5.4: "a name the contract\x27s declaration gives a person or a ward"; GSC, Section 2: "a declaration of the contract\x27s names, saying what each names").
        # The declaration is the sentence of the line that says it, the line split at ".  " first, so that a definition before it on its line names no one: "The author of an item is its first signer.  Alice and Bob are persons." names Alice and Bob (the style guide, 23 and 36, the definitions before the names, GLP 1873085; GSC #16, Code_inbox.md, 2026-11-11 20:05 UTC; vGLP #26).
        for my $s (split /\.\s{2,}/, $x) {
            next unless $s =~ /^(.*?\s+(?:is|are)\s+(?:a\s+)?persons?\b[^.]*)\.?\s*$/;
            $declared = 1;
            $is_name{$_} = 1 for ($1 =~ /\b([A-Z][a-z]+)\b/g);
        }
    }
    next if $declared;
    for my $x (read_txt($f)) {
        last if contract_end($x);
        next unless $x =~ /^\d+\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?[^.:]+[.:]\s+(.*)$/;
        # A citation of a clause, "Community Bank:1", "Notes:3", names a contract and no person (calculus/NAMES.txt: "A citation is: NAME, a colon, the clause\x27s number, then the act\x27s name where one act is cited"): its name and number are read as one word that names no one.
        (my $cl = $1) =~ s/($known):\d+/cited/g;
        for my $sen (split /(?<=[.!?;:])\s+/, $cl) {
            my @w = ($sen =~ /([A-Za-z]+)/g);
            shift @w;
            for my $w (@w) { $is_name{$w} = 1 if $w =~ /^[A-Z][a-z]+$/ }
        }
    }
}
# The clauses of every contract read, by title, a contract own before those of the contracts it extends, so that a pragma line may name a clause of a contract its contract extends ("A contract that extends others has their pragmas with its own", vGLP, Section 5.4, "The contract words"), as the Guardianship line of Agents.txt names GnW:1.
my (%xclause, %xbody);
for my $f (reverse @order) {
    for my $x (read_txt($f)) {
        next if $x =~ /^%/;
        last if contract_end($x);
        if ($x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.:]+)[.:]\s*(.*)$/) {
            next if exists $xclause{$2};
            $xclause{$2} = $1; $xbody{$2} = $3;
        }
    }
}
# The lines of a contract after the line "Pragmas", each pragma line with the indented lines after it carried on it after the mark \x01 (below): an interface heading, "Pragmas" or "Interface pragmas", is the line "Pragmas", and an implementation heading and every line after it up to the next interface heading are skipped (iface_head, impl_head).
# The interface pragmas may open with the app\x27s look, "Look: <icon>, <family>" (GSC 34bc713, Section 2: "The interface pragmas may open with a line naming the app\x27s look, its icon and its family, as \x27Look: chat, social\x27, the colours of each family being one file of the repository beside the contracts"): read into $look{F} as [Icon, Family], and no tab.  A Look line anywhere else, or not of that form, is refused.  The compiler carries the look into the platform "among the pragmas of the contract the platform exports, where the app and the site that show it read it" (vGLP b413fe1, Section 5.4, "The contract’s words"), so the term printed below holds it, look(Icon, Family), or nolook where the contract writes none.
# A contract’s version, "Version: <Current>, compatible from <Back>." among the interface pragmas, both whole numbers, Back at most Current, at most one such line in a contract, an extension’s own line standing over the contract’s it extends, and version(1, 1) where none writes one (vGLP 81fb280, Section 5.4: the platform exports "version(Current, Back), its version and the earliest version of itself it is compatible with, both from a line of the pragmas"; the line’s form Code’s, GSA’s yes, Code_inbox.md, 2026-11-07 22:35 UTC and GSA, 2026-11-08): read into $version{F} as [Current, Back].  A Version line not of that form, a second one, or one with Back above Current, is refused.
my (%look, %version);
sub joined {
    my ($f) = @_;
    my (@m, $aft, $impl, $first, $logic);
    delete $version{$f};
    for my $x (read_txt($f)) {
        if (impl_head($x)) { $impl = 1; $logic = 0; next }
        if (iface_head($x)) { $impl = 0; $logic = 0; $aft = 1; $first = 1; push @m, "Pragmas"; next }
        if (logic_head($x) && !$aft) { $logic = 1; next }
        next if $impl || $logic;
        if ($aft && $x =~ /^Look\s*:/) {
            $x =~ /^Look:\s*(\S.*?),\s*([\w-]+)\s*$/ && $first
                or die "pragmas.sh: $f: \"$x\": the look is the line the interface pragmas open with, \"Look: <icon>, <family>\" (GSC, Section 2)\n";
            $look{$f} = [$1, $2];
            $first = 0;
            next;
        }
        if ($aft && $x =~ /^Version\s*:/) {
            $x =~ /^Version:\s*(\d+),\s*compatible from\s*(\d+)\s*\.?\s*$/
                or die "pragmas.sh: $f: \"$x\": a version line is \"Version: <Current>, compatible from <Back>.\", both whole numbers (vGLP, Section 5.4)\n";
            die "pragmas.sh: $f: \"$x\": a second version line, where a contract writes at most one\n" if $version{$f};
            die "pragmas.sh: $f: \"$x\": the version it is compatible from, $2, is above its version, $1\n" if $2 > $1;
            $version{$f} = [$1, $2];
            $first = 0;
            next;
        }
        $first = 0 if $aft && $x =~ /\S/;
        if ($aft && $x =~ /^\s+\S/ && @m && $m[-1] ne "Pragmas") { $m[-1] .= "\x01" . $x; next }
        push @m, $x;
    }
    return @m;
}
# The contracts each contract extends, directly or through another: $anc{F}{G} where F extends G.
my %anc;
sub ancestors { my ($f, $g) = @_; for my $h (@{ $extends{$g} // [] }) { next if $anc{$f}{$h}++; ancestors($f, $h) } }
ancestors($_, $_) for @order;
# The button lines of every contract read, each a line naming a person and the buttons they propose, the clause line or an indented line after it, with its question lines (vGLP 40f8444, Section 5.4, "The contract’s words": "A line follows for each other person who proposes, likewise, and for each person asked, their name, a colon, and the question put to them; a question line belongs to the button line nearest above it"), by its key, file, line and rank; and the buttons each bears.
my (%line_buttons, @line_keys, %line_shown, %line_same);
for my $f (@order) {
    my %own; my $after = 0; my $tabs = 1; my $ln = -1;
    for my $x (joined($f)) {
        $ln++;
        next if $x =~ /^%/;
        if (!$after) {
            $own{$2} = 1 if $x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.:]+)[.:]\s*(.*)$/;
            $after = 1 if $x eq "Pragmas";
            next;
        }
        next if $x =~ /^\s*$/;
        my ($title, $rest) = $x =~ /^([^:]+):\s*(.*)$/ or next;
        $tabs = 0 if $tabs && (exists $own{$title} || exists $xclause{$title});
        next if $tabs;
        my $pi = -1;
        for my $part (split /\x01/, $rest) {
            next unless $pi < 0 ? $part =~ /^[A-Z][A-Za-z]*\s+\[/ : $part =~ /^\s+[A-Z][A-Za-z]*\s+\[/;
            $pi++;
            my $k = "$f\x02$ln\x02$pi";
            push @line_keys, $k;
            $line_buttons{$k} = [ map { act_name($_) } ($part =~ /\[([^\]]+)\]/g) ];
            my $nm = shown_name($f);
            (my $txt = $part) =~ s/^\s+//;
            $line_shown{$k} = "$nm: $title: $txt";
            $line_same{$k} = "$title\x03$rest";
        }
    }
}
# A button of a contract is replaced where a contract extending it writes a button of its name: "where the contract writes a button of an inherited button’s name, its button, with its fields and its questions, replaces the inherited one, the other buttons of the inherited line standing" (vGLP 4236349, Section 5.4, "The contract’s words", its last sentence), as GSN’s [Close] on a line of its own would leave the [Create] of the line it was on.  $replaced{K}{B} is the line of the extending contract that replaces the button B of the line K, and a line all of whose buttons are replaced is gone, its questions with it.
for my $k (@line_keys) {
    my ($f) = split /\x02/, $k;
    for my $b (@{ $line_buttons{$k} }) {
        for my $k2 (@line_keys) {
            my ($g) = split /\x02/, $k2;
            next unless $anc{$g}{$f} && $g ne $f;
            if (grep { $_ eq $b } @{ $line_buttons{$k2} }) { $replaced{$k}{$b} = $k2; last }
        }
    }
}
sub all_replaced { my ($k) = @_; return 0 unless $replaced{$k}; return !grep { !$replaced{$k}{$_} } @{ $line_buttons{$k} } }
# A button is of the acts it names, so one name is one button, with one line: two kept lines writing a button of one name, or one line writing it twice, are a fault of the contract, refused, the button and both lines named (vGLP 40f8444, Section 5.4, "The contract’s words": "for each act they propose, its button, in brackets, bearing the act’s name"; "A button is of the acts it names"; vGLP #23, Code_inbox.md, 2026-11-05 13:33 UTC, 1).
my (%button_line, @dup_out);
for my $k (@line_keys) {
    my %once;
    my @orig = $line_shown{$k} =~ /\[([^\]]+)\]/g;
    for my $bi (0 .. $#{ $line_buttons{$k} }) {
        my ($b, $ob) = ($line_buttons{$k}[$bi], $orig[$bi]);
        next if $replaced{$k}{$b};
        if ($once{$b}++) {
            push @dup_out, "$line_shown{$k}: refused, the button [$ob] written twice on it";
            push @faults, "pragmas.sh: the line \"$line_shown{$k}\" writes the button [$ob] twice, and a button is of the acts it names, one name one button (vGLP, Section 5.4, \"The contract’s words\")\n";
            next;
        }
        # A line two contracts write alike, its clause\x27s lines the same, as two contracts written from GO restate its lines, is one line, taken once: "what two of them inherit from one contract is taken once, and a schema, a tab or a button of one name that two of them write differently is refused" (vGLP, Section 5.4, "The contract\x27s words"; GO #5, Code_inbox.md, 2026-11-11 20:17 UTC, 4; vGLP #26).
        if (exists $button_line{$b} && $line_same{ $button_line{$b} } eq $line_same{$k}) {
            $replaced{$k}{$b} = $button_line{$b};
            next;
        }
        if (exists $button_line{$b}) {
            push @dup_out, "$line_shown{$k}: refused, the button [$ob] written also by the line \"$line_shown{ $button_line{$b} }\"";
            push @faults, "pragmas.sh: the button [$ob] is written by two lines, \"$line_shown{ $button_line{$b} }\" and \"$line_shown{$k}\", and a button is of the acts it names, one name one button with one line (vGLP, Section 5.4, \"The contract’s words\")\n";
            next;
        }
        $button_line{$b} = $k;
    }
}
# Every button of every line kept, by the schema name it bears, so that a schema one button names by its own name is realised by no other.
for my $k (@line_keys) { $named_by_button{$_} = 1 for grep { !$replaced{$k}{$_} } @{ $line_buttons{$k} } }
for my $f (@order) {
    # A line indented after a pragma line continues it (GSC, Section 2, its first paragraph; the guide, step 7: "an indented line for each other proposer, written the same way; and an indented line for each other person asked, their name, a colon, the question"), carried on its line after the mark \x01.
    my @l = joined($f);
    my $ln = -1;
    my $name = shown_name($f);
    my %clause; my %body; my $after = 0; my $first = 1; my $tabs = 1; my $tab = "";
    for my $x (@l) {
        $ln++;
        next if $x =~ /^%/;
        if (!$after) {
            if ($x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.:]+)[.:]\s*(.*)$/) { $clause{$2} = $1; $body{$2} = $3 }
            $after = 1 if $x eq "Pragmas";
            next;
        }
        next if $x =~ /^\s*$/;
        my ($title, $rest) = $x =~ /^([^:]+):\s*(.*)$/
            or die "pragmas.sh: $f: a pragma with no title: $x\n";
        # The pragmas open with one line for each tab of the app, each naming the tab and what it lists, before the clause lines (GSC, Section 2, its first paragraph, as GSC ruled, Code_inbox.md, 2026-10-24 09:40 UTC): every line before the first that names a clause is a tab, and the first of them is the tab of the contract, where its acts are drawn.
        $tabs = 0 if $tabs && (exists $clause{$title} || exists $xclause{$title});
        if ($tabs) {
            $rest !~ /\x01/ or die "pragmas.sh: $f: the tab \"$title\" has an indented line\n";
            my @w;
            my @lw = map { lc } ($rest =~ /([A-Za-z0-9_]+)/g);
            # The predicate a tab lists is named by its name, an underscore read as a space, the longest such phrase in the line (vGLP 110caf4, Section 5.4, "The contract words"), a word also read without a final s, as "friends" names friend and "principal of" principal_of; the words of the line are given where the contract term is not.
            my ($best, @found) = (0);
            for my $i (0 .. $#lw) {
                for my $n (1 .. 3) {
                    last if $i + $n - 1 > $#lw;
                    my @ws = @lw[$i .. $i + $n - 1];
                    for my $ph (join("_", @ws), ($ws[-1] =~ /s$/ && length($ws[-1]) > 1) ? join("_", @ws[0 .. $#ws - 1], substr($ws[-1], 0, -1)) : ()) {
                        next unless $preds{$ph};
                        my $len = $n;
                        if ($len > $best) { ($best, @found) = ($len, $ph) }
                        elsif ($len == $best && !grep { $_ eq $ph } @found) { push @found, $ph }
                    }
                }
            }
            # An inherited tab stands while the contract has the predicate it lists: "an inherited tab standing while the contract has the predicate it lists, as an inherited button stands while the contract has an act it names" (vGLP e1d1b8a, Section 5.4, "The contract’s words"); one naming no predicate of the contract’s schemas is gone, as GSN’s Messages and Posts are in GSNP, and the first tab kept is the one the contract’s clause lines’ buttons go to.  A tab of the contract’s own that names none stands, naming none, so that the fault shows.
            if (!@found && $f ne $top) {
                push @lines_out, "$name: the tab $title: gone, the contract having no predicate it lists";
                next;
            }
            if (@found) { @w = @found; $tabpred{$_} = 1 for @found }
            else {
                for my $w (@lw) {
                    push @w, $w;
                    push @w, substr($w, 0, -1) if length($w) > 1 && $w =~ /s$/;
                }
            }
            push @tabs, { f => $f, name => $name, title => $title, list => "list(" . q_string($title) . ", [" . join(", ", map { q_atom($_) } @w) . "])" };
            $tab = $title if $first;
            $first = 0;
            next;
        }
        if (!exists $clause{$title} && exists $xclause{$title}) { $clause{$title} = $xclause{$title}; $body{$title} = $xbody{$title} }
        exists $clause{$title} or die "pragmas.sh: $f: the pragma \"$title\" names no clause of $name or of a contract it extends\n";
        # A run of buttons and its fields, held until every tab is read: the acts are printed after the pragmas (below), each schema the line fits with its proposer.
        my $emit = sub {
            my ($bs, $fs, $said, $by, $pl, $fit, $err, $reals, $who) = @_;
            push @pending, { buttons => $bs, fields => $fs, said => $said, by => $by, pl => $pl, fit => $fit, err => $err, reals => $reals, tab => $tab, f => $f, title => $title, who => $who, name => $name };
            push @lines_out, "\x03$#pending";
        };
        # A pragma line (GSC 25b6b3c, Section 2, its first paragraph; the guide, step 7; vGLP 8aa82f0, Section 5.4, "The contract words"): the line names the person who proposes, then a button for each act they propose and any field they fill; an indented line names another person who proposes, likewise, or a person asked, a colon and the question put to them, a question line belonging to the nearest button line above it.  "A name in a pragma is the schema role of that name, the schemas naming their roles as the clauses do" (GSC 25b6b3c, Section 2): each name is the role its schema term names so, $rnames{S}, and a name no role of the schema bears, in a question, is the agent variable of an act with one, as Carol of GKK:3 is c of introduce, filled by its field; any other is a fault.  "A button is offered only to the person named before it", by([G]), G the role of that person, which must be guarding; "the persons named before the questions are asked", each question put to the role of the person it names, the proposer having []; and "a person no line names is never asked", so a guarding role an act needs that the line names neither as its proposer nor as asked is a fault, the act never asking that person.  A question naming the one guarding role of an act is text of its request form.
        $rest = inst_renamed($f, $ln, $rest) if $f ne $top;
        if ($rest !~ /^\[/) {
            my @parts = split /\x01/, $rest;
            # A name in a pragma is the schema\x27s role of that name, a name the contract\x27s declaration gives a person or a ward (vGLP 722343d, Section 5.4), whichever contract\x27s declaration gave it: the declared names and the names of the roles of the contract\x27s schemas, each line\x27s names then held to the roles of the schemas its buttons name (below).
            my %nm = map { $_ => 1 } (keys %is_name, map { @{ $rnames{$_} // [] } } keys %rnames);
            my $alt = join("|", sort { length($b) <=> length($a) || $a cmp $b } keys %nm);
            my @props;
            for my $i (0 .. $#parts) {
                my $p = $parts[$i];
                if ($i == 0) {
                    $p =~ /^([A-Z][A-Za-z]*)\s+(\[.*)$/ or die "pragmas.sh: $f: the pragma \"$title\" has no button in brackets after its proposer\n";
                    push @props, { name => $1, tail => $2, asked => [] };
                    next;
                }
                if ($p =~ /^\s+([A-Z][A-Za-z]*)\s+(\[.*)$/) { push @props, { name => $1, tail => $2, asked => [] }; next }
                if ($p =~ /^\s+([A-Z][A-Za-z]*):\s*(\S.*)$/) {
                    my ($who, $q) = ($1, $2);
                    $nm{$who} or die "pragmas.sh: $f: the pragma \"$title\" asks $who, whom the contract does not name\n";
                    my @words;
                    for my $x (split /\b($alt)\b/, $q) {
                        next if $x eq "";
                        push @words, $nm{$x} ? ["person", $x] : ["text", $x];
                    }
                    push @{ $props[-1]{asked} }, [$who, \@words];
                    next;
                }
                die "pragmas.sh: $f: the pragma \"$title\" has an indented line it cannot read: $p\n";
            }
            for my $pi (0 .. $#props) {
                my $pr = $props[$pi];
                # A button an extending contract writes (%replaced, above) is replaced, with its fields, and the other buttons of the line stand, with the line’s questions; a line all of whose buttons are replaced is gone, its questions with it.
                my $lk = "$f\x02$ln\x02$pi";
                if (all_replaced($lk)) {
                    my %gs = map { (my $g = (split /\x02/, $_)[0]) =~ s{.*/}{}; $g => 1 } values %{ $replaced{$lk} };
                    push @lines_out, "$name: $title: $pr->{name} $pr->{tail}: replaced by the line of " . join(" and ", sort keys %gs) . (@{ $line_buttons{$lk} } == 1 ? " with a button of the same name" : " with buttons of the same names");
                    next;
                }
                my $tail = $pr->{tail};
                while ($tail ne "") {
                    # The fields after a button, up to the next button, are that button’s, each button checked against its own fields (vGLP 40f8444, Section 5.4, "The contract’s words": "for each act they propose, its button, in brackets, bearing the act’s name, and its fields, in angle brackets"; vGLP #23, Code_inbox.md, 2026-11-05 13:33 UTC, 2), as DGC’s "[Mint coin] <denomination> <amount> [Mint bond] <denomination> <maturity> <amount>" and GSN’s "[Create] <name> [Close]".
                    my @buttons;
                    push @buttons, $1 if $tail =~ s/^\[([^\]]+)\]\s*//;
                    @buttons or die "pragmas.sh: $f: the pragma \"$title\" has words it cannot read: $tail\n";
                    my @fields;
                    while ($tail =~ s/^<([^>]+)>\s*//) { push @fields, $1 }
                    $tail =~ /^(\[|$)/ or die "pragmas.sh: $f: the pragma \"$title\" has words it cannot read: $tail\n";
                    my $ba = act_name($buttons[0]);
                    if ($replaced{$lk} && $replaced{$lk}{$ba}) {
                        (my $g = (split /\x02/, $replaced{$lk}{$ba})[0]) =~ s{.*/}{};
                        push @lines_out, "$name: $title: $pr->{name} [$buttons[0]]" . join("", map { " <$_>" } @fields) . ": replaced by the line of $g with a button of the same name, the other buttons of the line standing";
                        next;
                    }
                    # An inherited button stands while the contract has an act it names, and goes with its fields and its questions where it has none (vGLP 40f8444, Section 5.4, "The contract’s words", its last sentence: "an inherited button, with its fields and its questions, stands while the contract has an act it names, an act the contract replaces being gone and its button with it"), as GC’s [Mint] in DGC, whose term has no mint; a button of the contract itself that names no act is a fault (below).
                    if ($f ne $top && @snames) {
                        my @gone = grep { my $a = act_name($_); !grep { exists $roles{$_} } realising($a) } @buttons;
                        if (@gone) {
                            push @lines_out, "$name: $title: $pr->{name} " . join("", map { "[$_]" } @gone) . ": gone, the contract having no act it names";
                            my %g = map { $_ => 1 } @gone;
                            @buttons = grep { !$g{$_} } @buttons;
                            next unless @buttons;
                        }
                    }
                    my (%said, %by, %plof, %fit, %err, %reals);
                    for my $button (@buttons) {
                        my $a = act_name($button);
                        my @real = realising($a);
                        $reals{$button} = [@real];
                        $covered{$_} = $button for @real;
                        # A button that covers several schemas realises, on this line, those the line fits: its proposer in a guarding role, each person asked in a guarding role, every other guarding role asked, each name of its questions a role or the agent variable (vGLP 516a0a2, Section 5.4; Section 6.2: "A button that covers several schemas is drawn once"); a schema of the button that no line fits is a fault, below.
                        my @why;
                        for my $S (@real) {
                          my $ok = eval {
                            exists $roles{$S} or die "pragmas.sh: $f: the pragma \"$title\" names $S, which the contract term has not\n";
                            exists $rnames{$S} or die "pragmas.sh: $f: the schema term of $S names no roles, and a name in a pragma is the schema role of that name (GSC, Section 2)\n";
                            my @g = guarding($S);
                            my @pp = person_places($S);
                            my $nr = scalar @{ $roles{$S} };
                            # The rank among the persons of S of the role each name bears, a listed role named by its base being no person.
                            my %rk;
                            for my $i (0 .. $#{ $rnames{$S} }) {
                                my ($r) = grep { $pp[$_] == $i + 1 } 0 .. $#pp;
                                $rk{ $rnames{$S}[$i] } //= $r + 1 if defined $r;
                            }
                            my @av = grep { $pp[$_] > $nr } 0 .. $#pp;
                            my $rank = sub {
                                my ($x) = @_;
                                return $rk{$x} if exists $rk{$x};
                                @av == 1 or die "pragmas.sh: $f: the pragma \"$title\" names $x, whom no role of $S bears, and $S has " . scalar(@av) . " agent variables\n";
                                return $av[0] + 1;
                            };
                            # A listed role is asked by its name, each person of its list (vGLP 9ee6f87, Section 5.4, “Speech-act schemas as local specifications”: the guard set of a part is the people in its guarding roles, “those in a listed guarding role being those of L”), its place the role’s own.
                            my %lpl;
                            for my $j (1 .. scalar @{ $roles{$S} }) { $lpl{ $rnames{$S}[$j - 1] } //= $j if $roles{$S}[$j - 1]{listed} && defined $rnames{$S}[$j - 1] }
                            my %q;
                            for my $x (@{ $pr->{asked} }) {
                                exists $rk{ $x->[0] } || exists $lpl{ $x->[0] } or die "pragmas.sh: $f: the pragma \"$title\" asks $x->[0], whom no role of $S bears\n";
                                my $place = exists $rk{ $x->[0] } ? $pp[ $rk{ $x->[0] } - 1 ] : $lpl{ $x->[0] };
                                grep { $_ == $place } @g or die "pragmas.sh: $f: the pragma \"$title\" asks $x->[0], who is in no guarding role of $S\n";
                                !exists $q{$place} or die "pragmas.sh: $f: the pragma \"$title\" asks $x->[0] twice\n";
                                $q{$place} = "[" . join(", ", map { $_->[0] eq "text" ? "text(" . q_string($_->[1]) . ")" : "person(" . $rank->($_->[1]) . ")" } @{ $x->[1] }) . "]";
                            }
                            exists $rk{ $pr->{name} } or die "pragmas.sh: $f: the pragma \"$title\" names $pr->{name} as the proposer of $S, whom no role of $S bears\n";
                            my $pl = $pp[ $rk{ $pr->{name} } - 1 ];
                            grep { $_ == $pl } @g or die "pragmas.sh: $f: the pragma \"$title\" names $pr->{name} as the proposer of $S, who is in no guarding role of it\n";
                            !exists $q{$pl} || @g == 1 or die "pragmas.sh: $f: the pragma \"$title\" asks $pr->{name}, the proposer of $S\n";
                            # A person the clause says must consent to its act is a guard of it who is not asked (vGLP, Section 5.4, "The contract\x27s words": "A clause may say that a person, or a ward, must consent to its act: they are a guard of the act, so it is never taken against them, and they are not asked ... no pragma line puts a question to them"), as Carol of "Alice may create Carol, and Carol must consent": no line names them, a line that asks them is refused, and the act carries consents(I), I their role, with the types of its fields.
                            my @mc = map { exists $rk{$_} ? $pp[ $rk{$_} - 1 ] : () } (($body{$title} // "") =~ /\b([A-Z][a-z]+) must consent\b/g);
                            @mc = grep { my $p = $_; $p != $pl && grep { $_ == $p } @g } @mc;
                            for my $p (@mc) { !exists $q{$p} or die "pragmas.sh: $f: the pragma \"$title\" asks a person who must consent to $S, and is not asked\n" }
                            $consent_of{$S} = [@mc];
                            my @un = grep { my $p = $_; $p != $pl && !exists $q{$p} && !grep { $_ == $p } @mc } @g;
                            !@un or die "pragmas.sh: $f: the pragma \"$title\" names no person in the guarding role " . join(", ", @un) . " of $S, who is then never asked\n";
                            # Two roles the clause says are possibly one person may be bound to one person, and a field offers that person for both places (GSC 316d875, Section 2; vGLP, Sections 5.4 and 6.2): same(K, J), K and J their places in the request, its listed roles left out.
                            my %rplace; { my $k = 0; my $jj = 0; for my $r (@{ $roles{$S} }) { $jj++; next if $r->{listed}; $k++; $rplace{$jj} = $k } }
                            my @same = map { "same($rplace{$_->[0]}, $rplace{$_->[1]})" } grep { exists $rplace{ $_->[0] } && exists $rplace{ $_->[1] } } @{ $ones{$S} // [] };
                            # A mark of an agent variable that is no role, or a role\x27s whose Other is a variable or any of the others: the places of the request, a variable\x27s place after those of the roles in the order of the schema\x27s variables, and "_" every other place of a person (vGLP, Sections 5.4 and 6.2).
                            if (@{ $vmarks{$S} // [] }) {
                                my %kplace = map { ("r$_" => $rplace{$_}) } keys %rplace;
                                my $nr = scalar keys %rplace; my $vi = 0;
                                for my $h (@{ $head{$S} // [] }) { $vi++; next unless ref $h && $h->[0] eq "pvar"; (my $vn = $h->[1][0][0]) =~ s/^\x27|\x27$//g; $kplace{"v$vn"} = $nr + $vi }
                                my %seen = map { $_ => 1 } @same;
                                for my $m (@{ $vmarks{$S} }) {
                                    my ($ma, $mb) = @$m;
                                    next unless exists $kplace{$ma};
                                    my @bs = $mb eq "_" ? (grep { $_ ne $ma } sort { $kplace{$::a} <=> $kplace{$::b} || $::a cmp $::b } keys %kplace) : (exists $kplace{$mb} ? ($mb) : ());
                                    for my $bb (@bs) { my $x = "same($kplace{$ma}, $kplace{$bb})"; push @same, $x unless $seen{$x}++ }
                                }
                            }
                            $by{$S} = @same ? "by([$pl], [" . join(", ", @same) . "])" : "by([$pl])";
                            $plof{$S} = $pl;
                            base_read($S, $pl);
                            $said{$S} = @g == 1
                                ? (exists $q{$pl} ? "said([$q{$pl}])" : undef)
                                : "said([" . join(", ", map { exists $q{$_} ? $q{$_} : "[]" } @g) . "])";
                            1;
                          };
                          if ($ok) { $fit{$S} = 1 }
                          else { push @why, $@ }
                        }
                        # A fault of the line is held, not raised, until it is known whether the line is kept: a line an extending contract’s line replaces is not checked (vGLP, Section 5.4, "The contract’s words": "where it writes a schema anew for a clause it inherits, its line for that clause replaces the inherited line").
                        $err{$button} = @real == 1 ? $why[0] : "pragmas.sh: $f: the pragma \"$title\" fits none of the schemas [$button] covers:\n" . join("", @why)
                            unless grep { $fit{$_} } @real;
                    }
                    $emit->(\@buttons, \@fields, \%said, \%by, \%plof, \%fit, \%err, \%reals, $pr->{name});
                }
            }
            next;
        }
        # A line whose buttons come first names no person before them, and "a button is offered only to the person named before it" (vGLP 8aa82f0, Section 5.4, "The contract words"): a fault, and the contract is not compiled.
        die "pragmas.sh: $f: the pragma \"$title\" names no person before its button, and a button is offered only to the person named before it\n";
    }
}
# A tab of an inherited tab’s name replaces it: a contract "has their tabs with its own, a tab of an inherited tab’s name replacing it" (vGLP 14d9064, Section 5.4, "The contract’s words", its last sentence), as GDO-L’s Pointing replaces GDO’s and DGC’s Coins those of GB and GC.  The tabs replacing a tab are those of its name in the contracts extending its contract, directly or through another; the last of them, those no other replaces, stand in its place.
sub replacing_tabs { my ($t) = @_; return grep { $_->{title} eq $t->{title} && $_->{f} ne $t->{f} && $anc{ $_->{f} }{ $t->{f} } } @tabs }
my %tab_placed;
for my $t (@tabs) {
    my @by = replacing_tabs($t);
    my @last = @by ? grep { !replacing_tabs($_) } @by : ($t);
    push @lines_out, "$t->{name}: the tab $t->{title}: replaced by the tab of " . join(" and ", map { $_->{name} } @last) . " of the same name" if @by;
    # A tab two contracts write alike, its name and its predicate, is one tab, taken once ("what two of them inherit from one contract is taken once", vGLP, Section 5.4, "The contract\x27s words"; vGLP #26).
    for my $k (@last) { push @lists, $k->{list} unless $tab_placed{ $k->{list} }++ }
}
# The places of a request name a field may name, as cvac.glp field_places counts them: its roles but its listed roles, then the variables of its head that a person or the state fills; each slot [kind, place in the request, key], the key of a role its name, "@Name", and of a variable its name.
sub field_slots {
    my ($S) = @_;
    my (@sl, $i);
    $i = 0;
    my $j = 0;
    for my $r (@{ $roles{$S} }) {
        $j++;
        next if $r->{listed};
        $i++;
        push @sl, ["role", $i, "@" . ($rnames{$S}[$j - 1] // $j), $j];
    }
    # A variable local to a role is no place of the request (vGLP 8a3ae79, Section 5.4, "Speech-act schemas as local specifications": "Every other variable is bound by the schema-binding, and a request names it"), as cvac.glp takes the head less its local variables.
    for my $v (@{ $head{$S} }) {
        (my $vn = $v->[1][0][0] // "") =~ s/^\x27|\x27$//g;
        next if $locals{$S} && $locals{$S}{$vn};
        $i++;
        next unless $v->[0] =~ /^(pvar|svar|num|real|const)$/;
        push @sl, [$v->[0], $i, $vn];
    }
    return @sl;
}
# The variables an atom names: a role by role(k), or by own(k) in a name term, as "rK"; an agent, speech-act or numeric variable by its name, as "vX", the speech acts of a name term among them, <u>.y naming u and y (GSC, Section 3.1: "a name term naming its base and the speech-act variables in it").
sub atom_keys {
    my ($t) = @_;
    return () if @$t == 1;
    my ($fn, $as) = @$t;
    return ("r$as->[0][0]") if ($fn eq "role" || $fn eq "own") && @$as == 1 && $as->[0][0] =~ /^\d+$/;
    my $uq = sub { (my $x = $_[0]) =~ s/^\x27|\x27$//g; return $x };
    return ("v" . $uq->($as->[0][0])) if $fn =~ /^(pvar|svar|num|real|const|own_var)$/ && @$as == 1;
    return (atom_keys($as->[0]), "v" . $uq->($as->[1][0])) if $fn eq "ext" && @$as == 2;
    return map { atom_keys($_) } @$as;
}
# Whether an atom is cleared, the wildcard among its arguments or as the speech act of a name term: it requires nothing (GSC, Definition "Speech-act schema"; vGLP 40f8444, Section 5.4, "Act schemas as local specifications": "the act deletes every atom of the state that it matches, and requires none").
sub cleared { my ($t) = @_; return 0 if !ref $t; return 1 if @$t == 1 && ($t->[0] eq "any"); return 1 if $t->[0] eq "ext_any"; return 0 if @$t == 1; return scalar grep { cleared($_) } @{ $t->[1] } }
# The variables the atoms the schema S requires at its role pl bind, a multiplicity not among them (vGLP 40f8444, Section 5.4, "The contract’s words": "The proposer’s machine finds in its own state every way to satisfy an act at the proposer’s role: every binding of the variables of the atoms the schema requires there under which the state holds those atoms and lacks the atoms the schema forbids there that the binding determines"): the atoms it keeps there and those it deletes, a deleted atom being required (GSC, Definition "Speech-act schema": "-(i) is a subset of =(i)"), but for a cleared one; and the proposer pl itself.
sub bound_keys {
    my ($S, $pl) = @_;
    my $rs = $roles{$S}[$pl - 1];
    my %b = ("r$pl" => 1);
    for my $a (@{ $rs->{atoms}[2] }, grep { !cleared($_) } @{ $rs->{atoms}[1] }) { $b{$_} = 1 for atom_keys($a) }
    return %b;
}
# The variables of the act S proposed at its role pl that a field names: "A field is for a variable of the schema that those atoms do not bind --- a text, a number, a new name, a person the proposer’s state does not name --- and the fields of a line name these variables, of the acts its button names together, in the schemas’ order" (vGLP 40f8444, Section 5.4, "The contract’s words"): the places of its request a field may name (field_slots) that the atoms it requires at pl do not bind, a variable local to a role being bound by that role and named by no request (vGLP 8a3ae79, Section 5.4, "Speech-act schemas as local specifications": "Every other variable is bound by the schema-binding, and a request names it").
# A variable the constraint determines from those the atoms bind and the other fields is no field: "A field is for a variable of the schema that those atoms do not bind and the constraint does not determine from the variables they bind and the other fields" (vGLP bcb2726, Section 5.4, "The contract’s words"), as GCC’s cmint’s i1, which i, bound by the issued count the treasurer holds, and the field k give; the slots are taken in the order of the request, a variable being determined from the bound ones and the fields before it.
sub field_keys3 {
    my ($S, $pl) = @_;
    my %b = bound_keys($S, $pl);
    my %fills = map { $_ => 1 } @{ $svars{$S} };
    my @out;
    for (field_slots($S)) {
        my $k = $_->[0] eq "role" ? "r$_->[3]" : "v$_->[2]";
        next unless !$b{$k} && ($_->[0] eq "role" || $_->[0] eq "pvar" || $fills{ $_->[2] });
        if ($_->[0] eq "num" && $cons{$S}) {
            my %known = (%b, map { ($_->[0] eq "role" ? "r$_->[3]" : "v$_->[2]") => 1 } @out);
            # A set variable is never known: the proposer chooses its size, a field, and the platform takes the set of that size, new names for an issue and the notes the proposer holds for a payment (Calculus #11, vGLP_inbox.md, 2026-11-24 18:12 UTC: "So Sum is a field in every case; SumSet is never one"), so size(SumSet) = Sum determines no Sum.
            for my $h (@{ $head{$S} }) { if ($h->[0] eq "set") { (my $x = $h->[1][0][0] // "") =~ s/^\x27|\x27$//g; delete $known{"v$x"} } }
            my %d = map { $_ => 1 } det_closure($cons{$S}, \%known);
            next if $d{$k};
        }
        push @out, $_;
    }
    return @out;
}
# The numeric variables a constraint gives finitely many values once those of %$known are bound, taken again with each it gives, as local.glp’s con_det and determined decide it: succ(X, Y) gives Y once X is bound and X once Y is; eq(X, Y) its one numeric variable not bound, where its coefficient in X - Y is not 0; and(C1, C2) what either gives; or(C1, C2) what both give; the rest give none.
sub nvar_key { my ($e) = @_; return undef unless @$e == 2 && $e->[0] eq "num"; my $x = $e->[1][0][0]; return undef if $x =~ /^-?\d+$/; $x =~ s/^\x27|\x27$//g; return "v$x" }
sub expr_nvars { my ($e) = @_; return () if @$e == 1; my $v = nvar_key($e); return ($v) if defined $v; return () if $e->[0] eq "num"; return map { expr_nvars($_) } @{ $e->[1] } if $e->[0] =~ /^(plus|minus|mul)$/; return map { expr_nvars($_) } @{ $e->[1][1][1] } if $e->[0] eq "fn" && @{ $e->[1] } == 2 && $e->[1][1][0] eq "[]"; return () }
sub const_of { my ($e) = @_; return 0 if @$e == 1; return $e->[1][0][0] + 0 if $e->[0] eq "num" && $e->[1][0][0] =~ /^-?\d+$/; my ($a, $b) = @{ $e->[1] }; return const_of($a) + const_of($b) if $e->[0] eq "plus"; return const_of($a) - const_of($b) if $e->[0] eq "minus"; return const_of($a) * const_of($b) if $e->[0] eq "mul"; return 0 }
sub lin_coef {
    my ($e, $v) = @_;
    return 0 if @$e == 1;
    if ($e->[0] eq "num") { my $k = nvar_key($e); return (defined $k && $k eq $v) ? 1 : 0 }
    my ($a, $b) = @{ $e->[1] };
    return lin_coef($a, $v) + lin_coef($b, $v) if $e->[0] eq "plus";
    return lin_coef($a, $v) - lin_coef($b, $v) if $e->[0] eq "minus";
    return (expr_nvars($a) ? lin_coef($a, $v) * const_of($b) : const_of($a) * lin_coef($b, $v)) if $e->[0] eq "mul";
    return 0;
}
sub con_det1 {
    my ($c, $known) = @_;
    return () if @$c == 1;
    my ($f, $as) = @$c;
    return (con_det1($as->[0], $known), con_det1($as->[1], $known)) if $f eq "and";
    if ($f eq "or") { my %d2 = map { $_ => 1 } con_det1($as->[1], $known); return grep { $d2{$_} } con_det1($as->[0], $known) }
    if ($f eq "eq") {
        my %u; $u{$_} = 1 for grep { !$known->{$_} } (expr_nvars($as->[0]), expr_nvars($as->[1]));
        my @u = keys %u;
        return () unless @u == 1;
        return lin_coef(["minus", [$as->[0], $as->[1]]], $u[0]) != 0 ? @u : ();
    }
    if ($f eq "succ") {
        my ($x, $y) = map { nvar_key($_) } @$as;
        my $bx = !defined $x || $known->{$x}; my $by = !defined $y || $known->{$y};
        return ($y) if $bx && !$by;
        return ($x) if $by && !$bx;
    }
    return ();
}
sub det_closure {
    my ($c, $known) = @_;
    my %k = %$known; my %d;
    while (1) {
        my @n = grep { !$k{$_} } con_det1($c, \%k);
        last unless @n;
        $k{$_} = $d{$_} = 1 for @n;
    }
    return keys %d;
}
# A variable by the name the schema gives it: a role by its name, as Bob, and any other by its own.
sub var_name { my ($sl) = @_; (my $n = $sl->[2]) =~ s/^@//; return $n }
my %kept_fit; my @line_of;
for my $i (0 .. $#pending) {
    my $pd = $pending[$i];
    my ($f, $title, $tab, $name) = @{$pd}{qw(f title tab name)};
    my @fields = @{ $pd->{fields} };
    my @real;
    my $failed = 0;
    for my $button (@{ $pd->{buttons} }) {
        if (exists $pd->{err}{$button}) { push @faults, $pd->{err}{$button}; $failed = 1; next }
        push @real, grep { $pd->{fit}{$_} } @{ $pd->{reals}{$button} };
    }
    my $shown = "$name: $title: $pd->{who} " . join("", map { "[$_]" } @{ $pd->{buttons} }) . join("", map { " <$_>" } @fields);
    if ($failed) { push @{ $line_of[$i] }, "$shown: refused, its buttons fitting no act"; next }
    $kept_fit{$_} = 1 for @real;
    # The variables the line’s fields must name: those of its button’s acts that the atoms each requires at its proposer’s role do not bind, of the acts together, in the schemas’ order, each schema in the order of its request, a variable merged in after the one before it (GSC, Code_inbox.md, 2026-10-24 08:06 UTC: "the fields the union of their variables in head order").
    my (@u, %uname);
    for my $S (@real) {
        my $at = -1;
        for my $sl (field_keys3($S, $pd->{pl}{$S})) {
            my $k = $sl->[2];
            $uname{$k} //= var_name($sl);
            my ($j) = grep { $u[$_] eq $k } 0 .. $#u;
            if (defined $j) { $at = $j; next }
            splice(@u, $at + 1, 0, $k); $at++;
        }
    }
    my $rule = join(" ", map { $uname{$_} } @u);
    # "the compiler refuses a line whose fields are not these" (vGLP 40f8444, Section 5.4, "The contract’s words").
    if (@fields < @u) {
        push @{ $line_of[$i] }, "$shown: refused; the fields the rule gives name " . ($rule eq "" ? "nothing" : $rule) . ", and " . join(", ", map { $uname{$_} } @u[scalar(@fields) .. $#u]) . (@u - @fields == 1 ? " is unnamed" : " are unnamed");
        push @faults, "pragmas.sh: $f: the pragma \"$title\" leaves unnamed the variable " . join(", ", map { $uname{$_} } @u[scalar(@fields) .. $#u]) . " of " . join(", ", @real) . ", which the atoms its proposer’s role requires do not bind (vGLP, Section 5.4, \"The contract’s words\": \"the fields of a line name these variables\")\n";
        next;
    }
    if (@fields > @u) {
        my @all = map { [ $_, $pd->{pl}{$_} ] } @real;
        my %bn; for my $x (@all) { my %b = bound_keys(@$x); for my $sl (field_slots($x->[0])) { my $k = $sl->[0] eq "role" ? "r$sl->[3]" : "v$sl->[2]"; $bn{ var_name($sl) } = 1 if $b{$k} && !($sl->[0] eq "role" && $sl->[3] == $x->[1]) } }
        my $bound = join(", ", sort keys %bn);
        push @{ $line_of[$i] }, "$shown: refused; the fields the rule gives name " . ($rule eq "" ? "nothing" : $rule) . ", and " . join(", ", map { "<$_>" } @fields[scalar(@u) .. $#fields]) . (@fields - @u == 1 ? " names a variable" : " name variables") . " the proposer’s state binds" . ($bound eq "" ? "" : " ($bound)");
        push @faults, "pragmas.sh: $f: the pragma \"$title\" has the field " . join(", ", map { "<$_>" } @fields[scalar(@u) .. $#fields]) . " beyond the variables of " . join(", ", @real) . " that the atoms its proposer’s role requires do not bind" . ($rule eq "" ? ", which are none" : ", $rule") . ($bound eq "" ? "" : "; those atoms bind $bound") . " (vGLP, Section 5.4, \"The contract’s words\": \"A field is for a variable of the schema that those atoms do not bind\")\n";
        next;
    }
    push @{ $line_of[$i] }, "$shown: fields " . ($rule eq "" ? "none" : $rule);
    my %fname; @fname{@u} = @fields;
    for my $S (@real) {
        my %mine = map { $_->[2] => 1 } field_keys3($S, $pd->{pl}{$S});
        my @sl = field_slots($S);
        # Each field at its place in the request, a set variable, which is no field, keeping its place (vGLP #32, 2026-11-24: the swap of Coins, its sets before its amounts).
        my @own; for my $sl (@sl) { $own[$sl->[1] - 1] = $mine{ $sl->[2] } ? $fname{ $sl->[2] } : "" } @own = map { $_ // "" } @own;
        # The type of each field for a speech-act variable the schema types, typed(T, x), ft(K, T) for its K-th field, as a seventh argument of the act where it has one, the compiler printing T as the field\x27s kind (vGLP 37fe038, Section 5.4: "a text by the type its variable carries in the schema where it carries one").
        my @ft = map { ($mine{ $sl[$_][2] } && $sl[$_][0] eq "svar" && $ftype{$S}{ $sl[$_][2] }) ? "ft(" . $sl[$_][1] . ", " . q_atom($ftype{$S}{ $sl[$_][2] }) . ")" : () } 0 .. $#sl;
        push @ft, map { "consents($_)" } @{ $consent_of{$S} // [] };
        pop @own while @own && $own[-1] eq "";
        my $button = (grep { grep { $_ eq $S } @{ $pd->{reals}{$_} } } @{ $pd->{buttons} })[0];
        push @acts, "act(" . q_atom($S) . ", " . q_string($button) . ", ["
            . join(", ", map { q_string($_) } @own) . "], " . ($pd->{said}{$S} // "none") . ", " . q_string($tab) . ", " . ($pd->{by}{$S} // "any") . (@ft ? ", [" . join(", ", @ft) . "]" : "") . ")";
    }
}
# A schema a button covers that no kept line fits has no proposer, and "a button is offered only to the person named before it" (vGLP 516a0a2, Section 5.4): a fault, and the contract is not compiled.
for my $S (sort keys %covered) {
    next if $kept_fit{$S};
    next unless exists $roles{$S} && guarding($S);
    push @faults, "pragmas.sh: the schema $S, which [$covered{$S}] covers, is fitted by no line: no person is named before a button for it\n";
}
# Each button line and what the rule gives it, on the standard error, where PRAGMAS_LINES is set.
print STDERR map { /^\x03(\d+)$/ ? map { "$_\n" } @{ $line_of[$1] // [] } : "$_\n" } @lines_out, @dup_out if $ENV{PRAGMAS_LINES};
# The standing lines of the implementation pragmas, of the contract and of every contract it extends (GSC 1a8e31c, Section 2: "An implementation pragma may declare that a person\x27s act stands for every act of a clause that the person guards: \x60\x60Encounter: Alice stands on [Open place] until [Close place]\x27\x27 says that Alice\x27s act Open place is her standing request of every act of Encounter she guards, so that she is asked nothing, and that her act Close place withdraws it"; vGLP 7c631b1, Section 5.4, "The contract\x27s words"): "<Clause>: <Name> stands on [<Start>] until [<Stop>]", read as standing(S, A, W) for each schema S of the clause, the schemas bearing its title as a button bears its name, A the schema of the button [<Start>] and W that of [<Stop>]; what follows on the line, after a semicolon, is how the app realises it, which no term carries.
my (@stands, %stand_seen, %stand_by_start);
for my $f (@order) {
    my $impl = 0;
    for my $x (read_txt($f)) {
        if (impl_head($x)) { $impl = 1; next }
        if (iface_head($x)) { $impl = 0; next }
        next unless $impl && $x =~ /\bstands on\b/;
        unless ($x =~ /^([^:]+?):\s+(\S.*?)\s+stands on \[([^\]]+)\] until \[([^\]]+)\](?:\s*[;.].*)?\s*$/) {
            push @faults, "pragmas.sh: $f: the standing line \"$x\" is not \"<Clause>: <Name> stands on [<Start>] until [<Stop>]\"\n";
            next;
        }
        my ($title, $b1, $b2) = ($1, $3, $4);
        my @sn = map { (my $n = lc $_) =~ s/\s+/_/g; $n } ($title, $b1, $b2);
        my @cl = realising($sn[0]);
        my ($start) = realising($sn[1]);
        my ($stop) = realising($sn[2]);
        if (@snames) {
            my %is = map { $_ => 1 } @snames;
            my @miss = grep { !$is{$_} } (@cl, $start, $stop);
            if (@miss) {
                push @faults, "pragmas.sh: $f: the standing line \"$x\" names " . join(", ", @miss) . ", no schema of the contract\n";
                next;
            }
        }
        push @{ $stand_by_start{$start} }, @cl;
        for my $S (@cl) {
            my $t = "standing(" . q_atom($S) . ", " . q_atom($start) . ", " . q_atom($stop) . ")";
            push @stands, $t unless $stand_seen{$t}++;
        }
    }
}
# The environment predicates the phone finds, from the implementation pragmas of the contract and of every contract it extends (vGLP, Section 5.4, "Speech-act schemas as local specifications": "An implementation pragma says, of each environment predicate other than the day, whether the phone finds it, as \x60Found by the phone: x is nearby y.\x27"), each phrase read by the Vocabulary of the Logic pragmas, as envkinds.sh reads it.  The phone finds a predicate only by a declaration of GLP\x27s networking seam (GLP, appendix-guards, "Networking seam": declare(nearby(U, S))), so a found predicate other than nearby is refused, named, a gap for GLP.
my (%voc_pred, @found, %found_seen);
for my $f (@order) {
    for my $x (read_txt($f)) {
        next unless $x =~ /^Vocabulary:\s*(.*)$/;
        for my $e (split /;\s*/, $1) {
            next unless $e =~ /^\s*(.*?)\s*=\s*([a-z_][A-Za-z0-9_]*)\s*\(/;
            my ($ph, $pd) = ($1, $2);
            $ph =~ s/\s+/ /g; $ph =~ s/\.$//;
            $voc_pred{$ph} //= $pd;
        }
    }
}
for my $f (@order) {
    my $impl = 0;
    for my $x (read_txt($f)) {
        if (impl_head($x)) { $impl = 1; next }
        if (iface_head($x)) { $impl = 0; next }
        next unless $impl && $x =~ /^Found by the phone:\s*(.*?)\.?\s*$/;
        for my $ph (split /;\s*/, $1) {
            $ph =~ s/\s+/ /g; $ph =~ s/^\s+|\s+$//g; $ph =~ s/\.$//;
            my $pd = $voc_pred{$ph};
            unless (defined $pd) { push @faults, "pragmas.sh: $f: the line \"Found by the phone\" names \"$ph\", which no Vocabulary entry has\n"; next }
            unless ($pd eq "nearby") { push @faults, "pragmas.sh: $f: the phone finds \"$ph\", $pd, for which GLP\x27s networking seam declares no stream (GLP, appendix-guards, \"Networking seam\"): a gap for GLP\n"; next }
            push @found, "found(" . q_atom($pd) . ")" unless $found_seen{$pd}++;
        }
    }
}
# The trust a standing request opens (vGLP, Section 5.4: "An implementation pragma may open a person\x27s phone to persons it does not know, over Bluetooth and the local network, from their standing request until they withdraw it, as Open place does"): an implementation line of the clause whose act makes a standing request, saying that the trust of Bluetooth and of the local network is open, as "Open place: while the place is open Alice\x27s app sets the trust of Bluetooth and of the local network open, and closed otherwise (GNA)."; trust(S) for each standing schema S whose standing request that act makes.
for my $f (@order) {
    my $impl = 0;
    for my $x (read_txt($f)) {
        if (impl_head($x)) { $impl = 1; next }
        if (iface_head($x)) { $impl = 0; next }
        next unless $impl && $x =~ /^([^:]+?):.*\btrust of Bluetooth and of the local network open\b/;
        my $t = $1;
        (my $a = lc $t) =~ s/\s+/_/g;
        my ($start) = realising($a);
        unless ($stand_by_start{$start}) { push @faults, "pragmas.sh: $f: the line \"$x\" opens the trust from the act of $t, which makes no standing request of the contract\n"; next }
        for my $S (@{ $stand_by_start{$start} }) { push @found, "trust(" . q_atom($S) . ")" unless $found_seen{"trust $S"}++ }
    }
}
if (@faults) { print STDERR @faults; exit 1 }
# The term printed, pragmas(Lists, Acts, Stands, Look, Version, Text) (README.md beside this file, 1): the look of the contract itself, look(Icon, Family) or nolook; its version, its own line or, having none, the line of the nearest contract it extends that has one, or version(1, 1); and its clauses as text, the lines of its NAME.txt after the meta-information up to its first pragmas heading, joined by a newline (vGLP b413fe1, Section 5.4: the platform exports "contract(Text, Schemas), the contract it is compiled from, its clauses as text and its schemas with the pragmas").
my $lk = $look{$top} ? "look(" . q_string($look{$top}[0]) . ", " . q_string($look{$top}[1]) . ")" : "nolook";
my ($vf) = grep { $version{$_} } reverse @order;
my $vn = $vf ? "version($version{$vf}[0], $version{$vf}[1])" : "version(1, 1)";
my @cl;
for my $x (@own) { next if $x =~ /^%/; last if contract_end($x); push @cl, $x }
pop @cl while @cl && $cl[-1] !~ /\S/;
my $tx = join("\n", @cl);
$tx =~ s/\\/\\\\/g; $tx =~ s/"/\\"/g; $tx =~ s/\n/\\n/g;
# A contract the phone finds a fact for carries a seventh argument, the predicates found, so that every other contract prints the term it printed before.
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @acts) . "], [" . join(", ", @stands) . "], $lk, $vn, " . q_atom("\"$tx\"") . (@found ? ", [" . join(", ", @found) . "]" : "") . ")\n";
' "$1" "$PROGRAMS" "${2:-}" "$FROM"
