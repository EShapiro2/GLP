#!/bin/bash
# The pragmas of a contract, as the term vGLP's compiler reads them (README.md beside this file, "The pragmas"; vGLP, Section 5.4, "The contract's words"; GSC, Section 2, its first paragraph): the lines after the line "Pragmas" of the contract's NAME.txt and of the NAME.txt of every contract it extends, by its "% Extends:" line and theirs, the contracts extended first.  A contract whose own NAME.txt has no line "Pragmas" has none, no_pragmas, and is compiled as before (Code #14's brief, 2026-10-23: "A contract with no pragmas --- GC, GB, GSNP, GAI, CSSN, GFWC today --- compiles as before but for the declarations").
#
#   bash programs/vglp/cva/compiler/pragmas.sh <NAME.txt>
#
# prints pragmas(Lists, Acts) or no_pragmas on one line, each string as its GLP source text inside a quoted name, since the compiler's printer writes a constant as it is.  The first line after "Pragmas" is a list, "Name: what it lists", list(Name, Words), Words the words of what it lists, in lower case, each also without a final s; every other line is "Clause: [Button] ... <field> ... sentence", a button for each act of the clause a person proposes, and an act for each button, act(Schema, Button, Fields, Sentence), Schema the button's text in lower case, each space an underscore, the name of the schema it proposes ("a button for each act of it a person proposes, in brackets, each bearing its act's name", GSC 2991379, Section 2), the fields and the sentence of the line each act's, the clause being one of that title in the same NAME.txt or, where it has none, in the NAME.txt of a contract it extends, Sentence said(Questions) or none, each question a sentence of the line after its fields, the question put to the person in each role whose will the act needs beyond the proposer's, in their order (GSC, Section 2, its first paragraph: "for each role whose will the act needs beyond the proposer's, in their order, the question the person in it is asked"), each its words, each word text(S) or person(I), a name the person I-th among the names the line's clause uses, in the order they first occur in it, so that the head's persons, its roles then its agent variables, are named in order by the clause's names (vGLP, Section 5.4, "The contract's words": "A question names the roles of its schema by the names its clause uses, in the order of the roles"); a name is a word of a capital and lower-case letters that a clause of the contract, or of a contract it extends, writes within a sentence and not first in it, as Alice, Bob, Carol, Dave and Alf.  Each act carries, last, the name of the tab of its contract, the first line after "Pragmas" of the NAME.txt the act's line is in: act(Schema, Button, Fields, Sentence, Tab).  A contract extended is found beside the contract, or else under programs/ by its NAME.txt.  Exits 1, naming the line, on a pragma it cannot read or a clause it cannot find; 2 on a bad argument.

set -u
[ $# -ge 1 ] && [ $# -le 2 ] && [ -f "$1" ] || { echo "usage: pragmas.sh <NAME.txt> [<contract term>]" >&2; exit 2; }
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
exec perl -CSD -Mutf8 -e '
use strict; use warnings;
my ($top, $programs, $term) = @ARGV;
# The schemas of the contract term, where it is given, each with the variables of its head after its roles, in order: a button names the schemas bearing its name or its name with a suffix after an underscore, as swap_cc of [Swap] (GSC, Code_inbox.md, 2026-10-24 08:06 UTC).
my (@snames, %svars, %preds);
$preds{$1} = 1 while defined $term && $term =~ /\batom\((\w+),/g;
if (defined $term && $term ne "") {
    while ($term =~ /\b[nc]?schema\((\w+), \[([^\]]*)\]/g) {
        my ($n, $a) = ($1, $2);
        next if exists $svars{$n};
        push @snames, $n;
        $svars{$n} = [ $a =~ /\w+\((\w+)\)/g ];
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
# Each schema, its roles, $roles{S}, each { guarded, listed, comps }, comps its four lists of atoms, each [P, [args]], in the order of rs/5 (added, deleted, required, forbidden); and its head, $head{S}, the variables after its roles.
my (%roles, %head);
sub find_schemas {
    my ($t) = @_;
    return if @$t == 1;
    if ($t->[0] =~ /^[nc]?schema$/ && @{ $t->[1][0] } == 1) {
        my $n = $t->[1][0][0];
        return if exists $roles{$n};
        $head{$n} = $t->[1][1][1];
        my @rs;
        for my $r (@{ $t->[1][-1][1] }) {
            my ($kind, $g, @c) = $r->[0] eq "crs" ? @{ $r->[1] } : (undef, @{ $r->[1] });
            push @rs, { guarded => ($g->[0] eq "unguarded" ? 0 : 1),
                        listed => ((defined $kind && $kind->[0] eq "listed") ? 1 : 0),
                        comps => [ map { [ map { [ $_->[1][0][0], $_->[1][1][1] ] } @{ $_->[1] } ] } @c ] };
        }
        $roles{$n} = \@rs;
        return;
    }
    find_schemas($_) for @{ $t->[1] };
}
if (defined $term && $term ne "") { $tsrc = $term; find_schemas(tparse()) }
# The schemas a button realises: those bearing its name, the button in lower case, or its name with a suffix after an underscore (GSC #7, GSC docs/design-decisions.md, 2026-10-24: "the schemas realising it are those bearing its name or its name with a suffix after an underscore"), as [Enter] realises enter and enter_2 and [Swap] swap_cc, but for a schema another button of the contract names by its own name, as [Post own] names post_own and [Post] does not; the button alone where the term is not given or none is found.
my %named_by_button;
sub realising {
    my ($act) = @_;
    return ($act) if !@snames;
    my @r = grep { $_ eq $act || (index($_, "${act}_") == 0 && !$named_by_button{$_}) } @snames;
    return @r ? @r : ($act);
}
# A meeting schema, as cvac.glp meeting_schema reads one: two roles, both guarding, whose atoms deleted or required at each name neither the other role nor a name term or a signature built on it.
sub is_meeting {
    my ($s) = @_;
    my $rs = $roles{$s};
    return 0 unless defined $rs && @$rs == 2;
    for my $i (0, 1) {
        my $o = 2 - $i;
        return 0 unless $rs->[$i]{guarded};
        for my $a (@{ $rs->[$i]{comps}[1] }, @{ $rs->[$i]{comps}[2] }) {
            return 0 if join(",", map { tstr($_) } @{ $a->[1] }) =~ /role\($o\)|own\($o\)|sgn\(\w+,$o\)/;
        }
    }
    return 1;
}
# The fields of a schema, as cvac.glp field_places counts them: the peer of a meeting schema, then the variables of its head a person fills, each by its name.
sub field_keys {
    my ($s) = @_;
    return ((is_meeting($s) ? ("#peer") : ()), @{ $svars{$s} });
}
# The union of the variables of the schemas, in head order: the variables of each schema merged in, each one not yet held placed after the one before it in that schema.
sub union_vars {
    my @u;
    for my $s (@_) {
        my $at = -1;
        for my $v (field_keys($s)) {
            my ($i) = grep { $u[$_] eq $v } 0 .. $#u;
            if (defined $i) { $at = $i; next }
            splice(@u, $at + 1, 0, $v); $at++;
        }
    }
    return @u;
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
# The roles of a schema S a line covers matched to those of the widest schema W of its buttons, whose roles the line names (vGLP 21fca00, Section 5.4, "The contract words": "each schema the line covers puts to each of its guarding roles other than the proposer’s the question of that role’s name"): the one-to-one map m of the roles of S into those of W that keeps the most atoms of S, an atom of a role r kept where the role m(r) of W has, in the same of its four lists, an atom of the same predicate whose arguments are those of the atom with each role(k) read as role(m(k)), and a role counted kept where m(r) is guarding and listed as r is; of two maps keeping as many, the first in role order.  No paper states the match; it is the compiler’s (README.md beside this file, 5).
sub role_map {
    my ($S, $W) = @_;
    my @sr = @{ $roles{$S} }; my @wr = @{ $roles{$W} };
    if ($S eq $W) { my @id = (1 .. scalar(@sr)); return @id }
    my ($best, @bm, @m) = (-1);
    my $score = sub {
        my $sc = 0;
        for my $r (0 .. $#sr) {
            my $w = $wr[$m[$r] - 1];
            $sc++ if $sr[$r]{guarded} == $w->{guarded} && $sr[$r]{listed} == $w->{listed};
            for my $c (0 .. 3) {
                ATOM: for my $a (@{ $sr[$r]{comps}[$c] }) {
                    my $want = join("|", map { $_->[0] eq "role" && @$_ == 2 ? "role(" . ($m[$_->[1][0][0] - 1] // 0) . ")" : tstr($_) } @{ $a->[1] });
                    for my $b (@{ $w->{comps}[$c] }) {
                        next unless $b->[0] eq $a->[0] && join("|", map { tstr($_) } @{ $b->[1] }) eq $want;
                        $sc++; next ATOM;
                    }
                }
            }
        }
        return $sc;
    };
    my $go; $go = sub {
        my ($k, %used) = @_;
        if ($k > $#sr) { my $v = $score->(); if ($v > $best) { $best = $v; @bm = @m } return }
        for my $j (1 .. scalar @wr) { next if $used{$j}; $m[$k] = $j; $go->($k + 1, %used, $j => 1) }
    };
    $go->(0);
    @bm or die "pragmas.sh: the roles of $S are not matched to those of $W\n";
    return @bm;
}
sub find_txt {
    my ($name, $beside) = @_;
    my $f = "$beside/$name.txt";
    return $f if -f $f;
    for my $g (glob("$programs/*/$name.txt"), glob("$programs/*/*/$name.txt")) { return $g }
    return undef;
}
sub read_txt {
    my ($f) = @_;
    open(my $h, "<", $f) or die "pragmas.sh: cannot read $f\n";
    my @l = <$h>; close $h; chomp @l; return @l;
}
sub q_atom {   # a GLP quoted name holding the text $s
    my ($s) = @_; $s =~ s/\\/\\\\/g; $s =~ s/\x27/\\\x27/g; return "\x27$s\x27";
}
sub q_string { # a quoted name holding the GLP source of the string $s
    my ($s) = @_; $s =~ s/\\/\\\\/g; $s =~ s/"/\\"/g; return q_atom("\"$s\"");
}
my %seen; my @order;
sub visit {
    my ($f) = @_;
    return if $seen{$f}++;
    my @l = read_txt($f);
    my ($ext) = grep { /^% Extends:/ } @l;
    if (defined $ext) {
        (my $dir = $f) =~ s{/[^/]*$}{};
        my %named;
        # The contracts it extends are named in the first sentence of the line, a file name such as GC.txt in it not a contract extended.
        (my $first = $ext) =~ s/^% Extends:\s*//;
        $first =~ s/\.\s.*$//;
        for my $n ($first =~ /\b([A-Z][A-Za-z]*[A-Z])\b(?![\x27\x{2019}]s|\.\w)/g) {
            next if $named{$n}++;
            my $g = find_txt($n, $dir);
            visit($g) if defined $g;
        }
    }
    push @order, $f;
}
my @own = read_txt($top);
unless (grep { $_ eq "Pragmas" } @own) { print "no_pragmas\n"; exit 0 }
visit($top);
my (@lists, @acts);
# Every button of every contract read, by the schema name it bears, so that a schema one button names is realised by no other.
for my $f (@order) {
    my $after = 0;
    for my $x (read_txt($f)) {
        if (!$after) { $after = 1 if $x eq "Pragmas"; next }
        while ($x =~ /\[([^\]]+)\]/g) { (my $a = lc $1) =~ s/ +/_/g; $named_by_button{$a} = 1 }
    }
}
# The names the clauses use: words of a capital and lower-case letters written within a sentence, not first in it.
my %is_name;
for my $f (@order) {
    for my $x (read_txt($f)) {
        last if $x eq "Pragmas";
        next unless $x =~ /^\d+\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?[^.]+\.\s+(.*)$/;
        for my $sen (split /(?<=[.!?;:])\s+/, $1) {
            my @w = ($sen =~ /([A-Za-z]+)/g);
            shift @w;
            for my $w (@w) { $is_name{$w} = 1 if $w =~ /^[A-Z][a-z]+$/ }
        }
    }
}
# The clauses of every contract read, by title, a contract own before those of the contracts it extends, so that a pragma line may name a clause of a contract its contract extends ("A contract that extends others has their pragmas with its own", vGLP, Section 5.4, "The contract words"), as the Guardianship line of GAI.txt names GnW:1.
my (%xclause, %xbody);
for my $f (reverse @order) {
    for my $x (read_txt($f)) {
        next if $x =~ /^%/;
        last if $x eq "Pragmas";
        if ($x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.]+)\.\s*(.*)$/) {
            next if exists $xclause{$2};
            $xclause{$2} = $1; $xbody{$2} = $3;
        }
    }
}
for my $f (@order) {
    my @l = read_txt($f);
    (my $name = $f) =~ s{.*/}{}; $name =~ s/\.txt$//;
    my %clause; my %body; my $after = 0; my $first = 1; my $tab = "";
    for my $x (@l) {
        next if $x =~ /^%/;
        if (!$after) {
            if ($x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.]+)\.\s*(.*)$/) { $clause{$2} = $1; $body{$2} = $3 }
            $after = 1 if $x eq "Pragmas";
            next;
        }
        next if $x =~ /^\s*$/;
        my ($title, $rest) = $x =~ /^([^:]+):\s*(.*)$/
            or die "pragmas.sh: $f: a pragma with no title: $x\n";
        if ($first) {
            $first = 0;
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
            if (@found) { @w = @found }
            else {
                for my $w (@lw) {
                    push @w, $w;
                    push @w, substr($w, 0, -1) if length($w) > 1 && $w =~ /s$/;
                }
            }
            push @lists, "list(" . q_string($title) . ", [" . join(", ", map { q_atom($_) } @w) . "])";
            $tab = $title;
            next;
        }
        if (!exists $clause{$title} && exists $xclause{$title}) { $clause{$title} = $xclause{$title}; $body{$title} = $xbody{$title} }
        exists $clause{$title} or die "pragmas.sh: $f: the pragma \"$title\" names no clause of $name or of a contract it extends\n";
        my $group = $rest;
        while ($group ne "") {
            my @buttons;
            my $tail = $group;
            while ($tail =~ s/^\[([^\]]+)\]\s*//) { push @buttons, $1 }
            @buttons or die "pragmas.sh: $f: the pragma \"$title\" has no button in brackets\n";
            # The names of the roles, listed after the buttons where the names of the clause do not occur in the order of the roles (vGLP 21fca00, Section 5.4, "The contract words": "the line lists the roles’ names after its button, in the order of the roles").
            my @listed_names;
            @listed_names = split /\s*,\s*/, $1 if $tail =~ s/^\(([^)]*)\)\s*//;
            my @fields;
            while ($tail =~ s/^<([^>]+)>\s*//) { push @fields, $1 }
            # The words up to the next button in brackets are this group of buttons, the rest the next group: a line may carry several, each its buttons, its fields and its questions, as an Escrow line its [Deposit], [Release] and [Return].
            $group = ($tail =~ s/\s*(\[.*)$//) ? $1 : "";
            # The questions, each its words, a name kept as the name: the persons of the widest schema of the buttons named by the names listed, or else by the names the clause uses, in the order they first occur in it (vGLP, Section 5.4, "The contract words": "A question names the roles of its schema by the names its clause uses, in the order of the roles").
            my (@names, @qs);
            if ($tail ne "") {
                if (@listed_names) { @names = @listed_names }
                else {
                    my %at;
                    for my $w ($body{$title} =~ /([A-Za-z]+)/g) { push @names, $w if $is_name{$w} && !$at{$w}++ }
                }
                my %nm = map { $_ => 1 } (keys %is_name, @names);
                my $alt = join("|", sort { length($b) <=> length($a) || $a cmp $b } keys %nm);
                for my $q (split /(?<=[.!?])\s+/, $tail) {
                    my @words;
                    for my $p (split /\b($alt)\b/, $q) {
                        next if $p eq "";
                        if ($nm{$p}) {
                            grep { $_ eq $p } @names or die "pragmas.sh: $f: the pragma \"$title\" names $p, whom its clause does not name\n";
                            push @words, ["person", $p];
                        }
                        else { push @words, ["text", $p] }
                    }
                    push @qs, \@words;
                }
            }
            # The questions of each schema the buttons realise, said(Qs) in the order of its guarding roles: the line’s one question put to each guarding role; or its questions, one per guarding role of the widest schema of its buttons, the one with the most, in the order of the roles, none left out (vGLP 21fca00, Section 5.4, "The contract words": "a line’s questions are one per guarding role of the widest schema of its clause, in the order of the roles, none left out, since any guard may propose"), each schema putting to each of its guarding roles the question of the role of the widest schema its role is matched to (role_map); each name of a question the place among the persons of the schema that bears it, person(K); a question naming a person the schema has no role for is a fault.
            my @group;
            for my $button (@buttons) { (my $a = lc $button) =~ s/ +/_/g; push @group, realising($a) }
            my %said;
            my @known = grep { exists $roles{$_} } @group;
            if (@qs && @known) {
                my ($W) = sort { scalar(guarding($b)) <=> scalar(guarding($a)) } @known;
                my @wg = guarding($W);
                @qs == 1 || @qs == @wg
                    or die "pragmas.sh: $f: the pragma \"$title\" puts " . scalar(@qs) . " questions, and its widest schema, $W, has " . scalar(@wg) . " guarding roles\n";
                my @wpp = person_places($W);
                my $nw = scalar @{ $roles{$W} };
                my %wname; for my $i (0 .. $#wpp) { $wname{ $wpp[$i] } = $names[$i] if $i <= $#names }
                my @wav; my $i = 0; for my $v (@{ $head{$W} }) { $i++; push @wav, $nw + $i if $v->[0] eq "pvar" }
                for my $S (@known) {
                    my @m = role_map($S, $W);
                    my $ns = scalar @{ $roles{$S} };
                    my @sav; my $j = 0; for my $v (@{ $head{$S} }) { $j++; push @sav, $ns + $j if $v->[0] eq "pvar" }
                    my @spp = person_places($S);
                    my %rank_of;
                    for my $r (0 .. $#spp) {
                        my $k = $spp[$r];
                        my ($ai) = grep { $sav[$_] == $k } 0 .. $#sav;
                        my $wk = $k <= $ns ? $m[$k - 1] : (defined $ai ? $wav[$ai] : undef);
                        my $n = defined $wk ? $wname{$wk} : undef;
                        $rank_of{$n} = $r + 1 if defined $n;
                    }
                    my $render = sub {
                        my ($q) = @_;
                        my @w;
                        for my $x (@$q) {
                            if ($x->[0] eq "text") { push @w, "text(" . q_string($x->[1]) . ")"; next }
                            exists $rank_of{ $x->[1] } or die "pragmas.sh: $f: the pragma \"$title\" puts to a role of $S a question naming $x->[1], whom $S has no role for\n";
                            push @w, "person($rank_of{ $x->[1] })";
                        }
                        return "[" . join(", ", @w) . "]";
                    };
                    my @out;
                    if (@qs == 1) { @out = ($render->($qs[0])) }
                    else {
                        my %qi; @qi{@wg} = (0 .. $#wg);
                        push @out, $render->($qs[ $qi{ $m[$_ - 1] } ]) for guarding($S);
                    }
                    $said{$S} = "said([" . join(", ", @out) . "])";
                }
            }
            elsif (@qs) {
                # No contract term: a name the K-th of the names, in order.
                my %at; @at{@names} = (1 .. scalar @names);
                my $sd = "said([" . join(", ", map { my $q = $_; "[" . join(", ", map { $_->[0] eq "text" ? "text(" . q_string($_->[1]) . ")" : "person($at{$_->[1]})" } @$q) . "]" } @qs) . "])";
                $said{$_} = $sd for @group;
            }
            for my $button (@buttons) {
                (my $act = lc $button) =~ s/ +/_/g;
                my @real = realising($act);
                if (@real == 1 && $real[0] eq $act) {
                    push @acts, "act(" . q_atom($act) . ", " . q_string($button) . ", ["
                        . join(", ", map { q_string($_) } @fields) . "], " . ($said{$act} // "none") . ", " . q_string($tab) . ")";
                    next;
                }
                # Several schemas realise the button: the fields name the union of their fields, the peer of a meeting schema and the variables in head order, and each schema has the fields of its variables, in its order, a field of a variable it does not have left out (GSC, ibid.: "the fields the union of their variables in head order, an inapplicable field left empty").
                my @u = union_vars(@real);
                @fields <= @u or die "pragmas.sh: $f: the pragma \"$title\" names more fields than its schemas have variables\n";
                my %name; @name{ @u[0 .. $#fields] } = @fields if @fields;
                for my $s (@real) {
                    my @own = map { exists $name{$_} ? $name{$_} : "" } field_keys($s);
                    pop @own while @own && $own[-1] eq "";
                    push @acts, "act(" . q_atom($s) . ", " . q_string($button) . ", ["
                        . join(", ", map { q_string($_) } @own) . "], " . ($said{$s} // "none") . ", " . q_string($tab) . ")";
                }
            }
        }
    }
}
# A schema two lines name, of a contract and of one it extends, keeps the line of the contract that extends the other, read last, as the Authorship line of GSNP names sign after the Developer line of GAI.
my (%last, @kept);
for my $i (0 .. $#acts) { my ($n) = $acts[$i] =~ /^act\((\x27[^\x27]*\x27)/; $last{$n} = $i }
for my $i (0 .. $#acts) { my ($n) = $acts[$i] =~ /^act\((\x27[^\x27]*\x27)/; push @kept, $acts[$i] if $last{$n} == $i }
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @kept) . "])\n";
' "$1" "$PROGRAMS" "${2:-}"
