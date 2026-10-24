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
my (@snames, %svars);
if (defined $term && $term ne "") {
    while ($term =~ /\b[nc]?schema\((\w+), \[([^\]]*)\]/g) {
        my ($n, $a) = ($1, $2);
        next if exists $svars{$n};
        push @snames, $n;
        $svars{$n} = [ $a =~ /\w+\((\w+)\)/g ];
    }
}
# The schemas a button realises: its schema, the button in lower case, or, where the contract has none of that name, those named by it and a suffix after an underscore; the button alone where the term is not given or none is found.
sub realising {
    my ($act) = @_;
    return ($act) if !@snames || exists $svars{$act};
    my @r = grep { index($_, "${act}_") == 0 } @snames;
    return @r ? @r : ($act);
}
# The union of the variables of the schemas, in head order: the variables of each schema merged in, each one not yet held placed after the one before it in that schema.
sub union_vars {
    my @u;
    for my $s (@_) {
        my $at = -1;
        for my $v (@{ $svars{$s} }) {
            my ($i) = grep { $u[$_] eq $v } 0 .. $#u;
            if (defined $i) { $at = $i; next }
            splice(@u, $at + 1, 0, $v); $at++;
        }
    }
    return @u;
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
        for my $n ($ext =~ /\b([A-Z][A-Za-z]*[A-Z])\b(?![\x27\x{2019}]s)/g) {
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
            for my $w (map { lc } ($rest =~ /([A-Za-z0-9_]+)/g)) {
                push @w, $w;
                push @w, substr($w, 0, -1) if length($w) > 1 && $w =~ /s$/;
            }
            push @lists, "list(" . q_string($title) . ", [" . join(", ", map { q_atom($_) } @w) . "])";
            $tab = $title;
            next;
        }
        if (!exists $clause{$title} && exists $xclause{$title}) { $clause{$title} = $xclause{$title}; $body{$title} = $xbody{$title} }
        exists $clause{$title} or die "pragmas.sh: $f: the pragma \"$title\" names no clause of $name or of a contract it extends\n";
        my @buttons;
        my $tail = $rest;
        while ($tail =~ s/^\[([^\]]+)\]\s*//) { push @buttons, $1 }
        @buttons or die "pragmas.sh: $f: the pragma \"$title\" has no button in brackets\n";
        my @fields;
        while ($tail =~ s/^<([^>]+)>\s*//) { push @fields, $1 }
        my $said = "none";
        if ($tail ne "") {
            my (@names, %at);
            for my $w ($body{$title} =~ /([A-Za-z]+)/g) {
                next unless $is_name{$w};
                next if exists $at{$w};
                push @names, $w; $at{$w} = scalar @names;
            }
            my $alt = join("|", sort keys %is_name);
            my @qs;
            for my $q (split /(?<=[.!?])\s+/, $tail) {
                my @words;
                for my $p ($alt eq "" ? ($q) : (split /\b($alt)\b/, $q)) {
                    next if $p eq "";
                    if ($is_name{$p}) {
                        exists $at{$p} or die "pragmas.sh: $f: the pragma \"$title\" names $p, whom its clause does not name\n";
                        push @words, "person($at{$p})";
                    }
                    else { push @words, "text(" . q_string($p) . ")" }
                }
                push @qs, "[" . join(", ", @words) . "]";
            }
            $said = "said([" . join(", ", @qs) . "])";
        }
        for my $button (@buttons) {
            (my $act = lc $button) =~ s/ +/_/g;
            my @real = realising($act);
            if (@real == 1 && $real[0] eq $act) {
                push @acts, "act(" . q_atom($act) . ", " . q_string($button) . ", ["
                    . join(", ", map { q_string($_) } @fields) . "], $said, " . q_string($tab) . ")";
                next;
            }
            # Several schemas realise the button: the fields name the union of their variables in head order, and each schema has the fields of its variables, in its order, a field of a variable it does not have left out (GSC, ibid.: "the fields the union of their variables in head order, an inapplicable field left empty").
            my @u = union_vars(@real);
            @fields <= @u or die "pragmas.sh: $f: the pragma \"$title\" names more fields than its schemas have variables\n";
            my %name; @name{ @u[0 .. $#fields] } = @fields if @fields;
            for my $s (@real) {
                my @own = map { exists $name{$_} ? $name{$_} : "" } @{ $svars{$s} };
                pop @own while @own && $own[-1] eq "";
                push @acts, "act(" . q_atom($s) . ", " . q_string($button) . ", ["
                    . join(", ", map { q_string($_) } @own) . "], $said, " . q_string($tab) . ")";
            }
        }
    }
}
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @acts) . "])\n";
' "$1" "$PROGRAMS" "${2:-}"
