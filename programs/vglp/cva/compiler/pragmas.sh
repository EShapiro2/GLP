#!/bin/bash
# The pragmas of a contract, as the term vGLP's compiler reads them (README.md beside this file, "The pragmas"; vGLP, Section 5.4, "The contract's words"; GSC, Section 2, its first paragraph): the lines after the line "Pragmas" of the contract's NAME.txt and of the NAME.txt of every contract it extends, by its "% Extends:" line and theirs, the contracts extended first.  A contract whose own NAME.txt has no line "Pragmas" has none, no_pragmas, and is compiled as before (Code #14's brief, 2026-10-23: "A contract with no pragmas --- GC, GB, GSNP, GAI, CSSN, GFWC today --- compiles as before but for the declarations").
#
#   bash programs/vglp/cva/compiler/pragmas.sh <NAME.txt>
#
# prints pragmas(Lists, Acts) or no_pragmas on one line, each string as its GLP source text inside a quoted name, since the compiler's printer writes a constant as it is.  The lines after "Pragmas" up to the first that names a clause are lists, one for each tab, "Name: what it lists", list(Name, Words); every other line is a clause's, in GSC's form, its proposer, its buttons and fields, and its indented lines (README.md beside this file, 1), each act act(Schema, Button, Fields, Sentence, Tab, Proposer), Sentence said(Questions) or none, a question's words text(S) or person(I), I the rank among the schema's persons of the role the name bears, by the names the schema term gives its roles (GSC 25b6b3c, Section 2), Proposer by([G]).  A contract extended is found beside the contract, or else under programs/ by its NAME.txt.  Exits 1, naming the line, on a pragma it cannot read or a clause it cannot find; 2 on a bad argument.

set -u
[ $# -ge 1 ] && [ $# -le 2 ] && [ -f "$1" ] || { echo "usage: pragmas.sh <NAME.txt> [<contract term>]" >&2; exit 2; }
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
exec perl -CSD -Mutf8 -e '
use strict; use warnings;
my ($top, $programs, $term) = @ARGV;
# The schemas of the contract term, where it is given, each with the variables of its head after its roles that the request names, in order: a button names the schemas bearing its name or its name with a suffix after an underscore, as swap_cc of [Swap] (GSC, Code_inbox.md, 2026-10-24 08:06 UTC).  A numeric variable local to a role, one that occurs only in atoms the schema requires or deletes at that one role, and in the constraint, is not named by the request, and no field names it (vGLP, Section 5.4, "Act schemas as local specifications": "Every other numeric variable is bound by the schema-binding, as a speech-act variable is, and a request names it, so the request form takes it"), as t, the date of the issuer, in the redeem_bc of GB.
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
# The numeric variables local to a role of the schema whose arguments are @args.
sub local_nums {
    my (@args) = @_;
    my $roles = $args[-1];
    $roles =~ s/^\[//; $roles =~ s/\]$//;
    my (%at, %elsewhere);
    my $r = 0;
    for my $spec (top_args($roles)) {
        $r++;
        my ($f) = $spec =~ /^(c?rs)\(/ or next;
        my ($body) = inside($spec, length $f);
        my @p = top_args($body);
        shift @p if $f eq "crs";
        for my $k (0 .. $#p) {
            for my $x ($p[$k] =~ /\bnum\((\w+)\)/g) {
                if ($k == 2 || $k == 3) { $at{$x}{$r} = 1 } else { $elsewhere{$x} = 1 }
            }
        }
    }
    return grep { !$elsewhere{$_} && keys %{ $at{$_} } == 1 } keys %at;
}
my (@snames, %svars, %preds);
$preds{$1} = 1 while defined $term && $term =~ /\batom\((\w+),/g;
if (defined $term && $term ne "") {
    while ($term =~ /\b([nc]?schema)\(/g) {
        my $kw = $1;
        my $open = pos($term) - 1;
        my ($body, $end) = inside($term, $open);
        pos($term) = $open + 1;
        my @args = top_args($body);
        # A schema whose head names its roles, schema(Name, Names, Vars, Roles), cschema(Name, Names, Vars, Reaches, Roles) or nschema(Name, Names, Vars, Constraint, Roles) (programs/gsc/README.md), carries its variables third.
        my $named = ($kw eq "schema" && @args == 4) || ($kw ne "schema" && @args == 5);
        my ($n, $a) = @args[0, $named ? 2 : 1];
        next if !defined $a || exists $svars{$n};
        push @snames, $n;
        my %local = map { $_ => 1 } local_nums(@args);
        $a =~ s/^\[//; $a =~ s/\]$//;
        $svars{$n} = [ grep { !($local{$_} && $a =~ /\bnum\(\Q$_\E\)/) } ($a =~ /\w+\((\w+)\)/g) ];
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
my (%roles, %head, %rnames);
sub find_schemas {
    my ($t) = @_;
    return if @$t == 1;
    if ($t->[0] =~ /^[nc]?schema$/ && @{ $t->[1][0] } == 1) {
        my $n = $t->[1][0][0];
        return if exists $roles{$n};
        my $named = ($t->[0] eq "schema" && @{ $t->[1] } == 4) || ($t->[0] ne "schema" && @{ $t->[1] } == 5);
        $head{$n} = $t->[1][$named ? 2 : 1][1];
        if ($named) { $rnames{$n} = [ map { (my $x = $_->[0]) =~ s/^\x27|\x27$//g; $x } @{ $t->[1][1][1] } ] }
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
my %seen; my @order;
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
# The names: those the contract declares, on its line after the title and before clause 1, "Alice, Bob and Carol are persons." (Udi, 2026-10-25, to GSC #8; GKK.txt at GLP ec7c578c), the declaration being part of the contract the reader takes; and, of a contract that declares none, the words of a capital and lower-case letters its clauses write within a sentence, not first in it.
my %is_name;
for my $f (@order) {
    my $declared = 0;
    for my $x (read_txt($f)) {
        next if $x =~ /^%/;
        last if $x =~ /^\d+\./ || $x eq "Pragmas";
        next unless $x =~ /^(.*)\s+(?:is|are)\s+(?:a\s+)?persons?\.\s*$/;
        $declared = 1;
        $is_name{$_} = 1 for ($1 =~ /\b([A-Z][a-z]+)\b/g);
    }
    next if $declared;
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
    # A line indented after a pragma line continues it (GSC, Section 2, its first paragraph; the guide, step 7: "an indented line for each other proposer, written the same way; and an indented line for each other person asked, their name, a colon, the question"), carried on its line after the mark \x01.
    {
        my (@m, $aft);
        for my $x (@l) {
            if ($aft && $x =~ /^\s+\S/ && @m && $m[-1] ne "Pragmas") { $m[-1] .= "\x01" . $x; next }
            $aft = 1 if $x eq "Pragmas";
            push @m, $x;
        }
        @l = @m;
    }
    (my $name = $f) =~ s{.*/}{}; $name =~ s/\.txt$//;
    my %clause; my %body; my $after = 0; my $first = 1; my $tabs = 1; my $tab = "";
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
            if (@found) { @w = @found }
            else {
                for my $w (@lw) {
                    push @w, $w;
                    push @w, substr($w, 0, -1) if length($w) > 1 && $w =~ /s$/;
                }
            }
            push @lists, "list(" . q_string($title) . ", [" . join(", ", map { q_atom($_) } @w) . "])";
            $tab = $title if $first;
            $first = 0;
            next;
        }
        if (!exists $clause{$title} && exists $xclause{$title}) { $clause{$title} = $xclause{$title}; $body{$title} = $xbody{$title} }
        exists $clause{$title} or die "pragmas.sh: $f: the pragma \"$title\" names no clause of $name or of a contract it extends\n";
        # The acts of a run of buttons and its fields: an act for each schema a button realises, its fields, its questions said(Qs) or none, the tab of its contract and its proposer, by(G) the guarding role G or any, every guarding role, where the line does not name the proposer.
        my $emit = sub {
            my ($bs, $fs, $said, $by) = @_;
            my @buttons = @$bs; my @fields = @$fs;
                for my $button (@buttons) {
                    (my $act = lc $button) =~ s/ +/_/g;
                    my @real = realising($act);
                    if (@real == 1 && $real[0] eq $act) {
                        push @acts, "act(" . q_atom($act) . ", " . q_string($button) . ", ["
                            . join(", ", map { q_string($_) } @fields) . "], " . ($said->{$act} // "none") . ", " . q_string($tab) . ", " . ($by->{$act} // "any") . ")";
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
                            . join(", ", map { q_string($_) } @own) . "], " . ($said->{$s} // "none") . ", " . q_string($tab) . ", " . ($by->{$s} // "any") . ")";
                    }
                }
        };
        # A pragma line (GSC 25b6b3c, Section 2, its first paragraph; the guide, step 7; vGLP 8aa82f0, Section 5.4, "The contract words"): the line names the person who proposes, then a button for each act they propose and any field they fill; an indented line names another person who proposes, likewise, or a person asked, a colon and the question put to them, a question line belonging to the nearest button line above it.  "A name in a pragma is the schema role of that name, the schemas naming their roles as the clauses do" (GSC 25b6b3c, Section 2): each name is the role its schema term names so, $rnames{S}, and a name no role of the schema bears, in a question, is the agent variable of an act with one, as Carol of GKK:3 is c of introduce, filled by its field; any other is a fault.  "A button is offered only to the person named before it", by([G]), G the role of that person, which must be guarding; "the persons named before the questions are asked", each question put to the role of the person it names, the proposer having []; and "a person no line names is never asked", so a guarding role an act needs that the line names neither as its proposer nor as asked is a fault, the act never asking that person.  A question naming the one guarding role of an act is text of its request form.
        if ($rest !~ /^\[/) {
            my @parts = split /\x01/, $rest;
            my %nm = map { $_ => 1 } keys %is_name;
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
            for my $pr (@props) {
                my $tail = $pr->{tail};
                while ($tail ne "") {
                    my @buttons;
                    while ($tail =~ s/^\[([^\]]+)\]\s*//) { push @buttons, $1 }
                    @buttons or die "pragmas.sh: $f: the pragma \"$title\" has words it cannot read: $tail\n";
                    my @fields;
                    while ($tail =~ s/^<([^>]+)>\s*//) { push @fields, $1 }
                    $tail =~ /^(\[|$)/ or die "pragmas.sh: $f: the pragma \"$title\" has words it cannot read: $tail\n";
                    my (%said, %by);
                    for my $button (@buttons) {
                        (my $a = lc $button) =~ s/ +/_/g;
                        for my $S (realising($a)) {
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
                            my %q;
                            for my $x (@{ $pr->{asked} }) {
                                exists $rk{ $x->[0] } or die "pragmas.sh: $f: the pragma \"$title\" asks $x->[0], whom no role of $S bears\n";
                                my $place = $pp[ $rk{ $x->[0] } - 1 ];
                                grep { $_ == $place } @g or die "pragmas.sh: $f: the pragma \"$title\" asks $x->[0], who is in no guarding role of $S\n";
                                !exists $q{$place} or die "pragmas.sh: $f: the pragma \"$title\" asks $x->[0] twice\n";
                                $q{$place} = "[" . join(", ", map { $_->[0] eq "text" ? "text(" . q_string($_->[1]) . ")" : "person(" . $rank->($_->[1]) . ")" } @{ $x->[1] }) . "]";
                            }
                            exists $rk{ $pr->{name} } or die "pragmas.sh: $f: the pragma \"$title\" names $pr->{name} as the proposer of $S, whom no role of $S bears\n";
                            my $pl = $pp[ $rk{ $pr->{name} } - 1 ];
                            grep { $_ == $pl } @g or die "pragmas.sh: $f: the pragma \"$title\" names $pr->{name} as the proposer of $S, who is in no guarding role of it\n";
                            !exists $q{$pl} || @g == 1 or die "pragmas.sh: $f: the pragma \"$title\" asks $pr->{name}, the proposer of $S\n";
                            my @un = grep { $_ != $pl && !exists $q{$_} } @g;
                            !@un or die "pragmas.sh: $f: the pragma \"$title\" names no person in the guarding role " . join(", ", @un) . " of $S, who is then never asked\n";
                            $by{$S} = "by([$pl])";
                            $said{$S} = @g == 1
                                ? (exists $q{$pl} ? "said([$q{$pl}])" : undef)
                                : "said([" . join(", ", map { exists $q{$_} ? $q{$_} : "[]" } @g) . "])";
                        }
                    }
                    $emit->(\@buttons, \@fields, \%said, \%by);
                }
            }
            next;
        }
        # A line whose buttons come first names no person before them, and "a button is offered only to the person named before it" (vGLP 8aa82f0, Section 5.4, "The contract words"): a fault, and the contract is not compiled.
        die "pragmas.sh: $f: the pragma \"$title\" names no person before its button, and a button is offered only to the person named before it\n";
    }
}
# A schema two lines name, of a contract and of one it extends, keeps the line of the contract that extends the other, read last, as the Authorship line of GSNP names sign after the Developer line of GAI.
my (%last, @kept);
for my $i (0 .. $#acts) { my ($n) = $acts[$i] =~ /^act\((\x27[^\x27]*\x27)/; $last{$n} = $i }
for my $i (0 .. $#acts) { my ($n) = $acts[$i] =~ /^act\((\x27[^\x27]*\x27)/; push @kept, $acts[$i] if $last{$n} == $i }
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @kept) . "])\n";
' "$1" "$PROGRAMS" "${2:-}"
