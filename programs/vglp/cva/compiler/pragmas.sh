#!/bin/bash
# The pragmas of a contract, as the term vGLP's compiler reads them (README.md beside this file, "The pragmas"; vGLP, Section 5.4, "The contract's words"; GSC, Section 2, its first paragraph): the lines after the line "Pragmas" of the contract's NAME.txt and of the NAME.txt of every contract it extends, by its "% Extends:" line and theirs, the contracts extended first.  A contract whose own NAME.txt has no line "Pragmas" has none, no_pragmas, and is compiled as before (Code #14's brief, 2026-10-23: "A contract with no pragmas --- GC, GB, GSNP, GAI, CSSN, GFWC today --- compiles as before but for the declarations").
#
#   bash programs/vglp/cva/compiler/pragmas.sh <NAME.txt>
#
# prints pragmas(Lists, Acts) or no_pragmas on one line, each string as its GLP source text inside a quoted name, since the compiler's printer writes a constant as it is.  The first line after "Pragmas" is a list, "Name: what it lists", list(Name, Words), Words the words of what it lists, in lower case, each also without a final s; every other line is "Clause: [Button] ... <field> ... sentence", a button for each act of the clause a person proposes, and an act for each button, act(Schema, Button, Fields, Sentence), Schema the button's text in lower case, each space an underscore, the name of the schema it proposes ("a button for each act of it a person proposes, in brackets, each bearing its act's name", GSC 2991379, Section 2), the fields and the sentence of the line each act's, the clause being one of that title in the same NAME.txt, Sentence said(Words) or none, each word text(S) or person(I), Alice, Bob and Carol the persons 1, 2 and 3.  A contract extended is found beside the contract, or else under programs/ by its NAME.txt.  Exits 1, naming the line, on a pragma it cannot read or a clause it cannot find; 2 on a bad argument.

set -u
[ $# -eq 1 ] && [ -f "$1" ] || { echo "usage: pragmas.sh <NAME.txt>" >&2; exit 2; }
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
exec perl -CSD -Mutf8 -e '
use strict; use warnings;
my ($top, $programs) = @ARGV;
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
        for my $n ($ext =~ /\b([A-Z][A-Za-z]*[A-Z])\b/g) {
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
for my $f (@order) {
    my @l = read_txt($f);
    (my $name = $f) =~ s{.*/}{}; $name =~ s/\.txt$//;
    my %clause; my $after = 0; my $first = 1;
    for my $x (@l) {
        next if $x =~ /^%/;
        if (!$after) {
            if ($x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.]+)\./) { $clause{$2} = $1 }
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
            next;
        }
        exists $clause{$title} or die "pragmas.sh: $f: the pragma \"$title\" names no clause of $name\n";
        my @buttons;
        my $tail = $rest;
        while ($tail =~ s/^\[([^\]]+)\]\s*//) { push @buttons, $1 }
        @buttons or die "pragmas.sh: $f: the pragma \"$title\" has no button in brackets\n";
        my @fields;
        while ($tail =~ s/^<([^>]+)>\s*//) { push @fields, $1 }
        my $said = "none";
        if ($tail ne "") {
            my @words;
            for my $p (split /\b(Alice|Bob|Carol)\b/, $tail) {
                next if $p eq "";
                if ($p eq "Alice") { push @words, "person(1)" }
                elsif ($p eq "Bob") { push @words, "person(2)" }
                elsif ($p eq "Carol") { push @words, "person(3)" }
                else { push @words, "text(" . q_string($p) . ")" }
            }
            $said = "said([" . join(", ", @words) . "])";
        }
        for my $button (@buttons) {
            (my $act = lc $button) =~ s/ +/_/g;
            push @acts, "act(" . q_atom($act) . ", " . q_string($button) . ", ["
                . join(", ", map { q_string($_) } @fields) . "], $said)";
        }
    }
}
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @acts) . "])\n";
' "$1" "$PROGRAMS"
