#!/bin/bash
# The composition of several contracts, as vGLP's compiler takes them (vGLP 7c631b1, Section 5.4, "The contract's words": "The compiler takes one contract or several: of several it prints the platform of their composition, the union of their schemas and of their pragmas, which extends each, as the grassroots app's own platform is compiled from four contracts; what two of them inherit from one contract is taken once, and a schema, a tab or a button of one name that two of them write differently is refused").
#
#   bash programs/vglp/cva/compiler/compose.sh <NAME> <reading> <pragmas> <intro> [<NAME> <reading> <pragmas> <intro> ...]
#
# Each contract is given by its name, as the record names it (GSN), its reading, the term GSC's as_read/2 prints of its schemas, its pragmas, as pragmas.sh prints them of its NAME.txt and that reading, and its introduction predicate, introduction(P) or no_introduction.  Prints three lines, the reading, the pragmas and the introduction predicate of the composition, which the compiler's compile/5 takes as it takes one contract's; or, on a refusal, the line saying why on the standard error, and exits 1.  Exits 2 on a bad argument.
#
# The forms the paper leaves open, the composer's (README.md beside this file, "Composition"):
# - The schemas are taken in the order of the contracts, each contract's in its own order, a schema of a name already taken being the same schema or refused.  Two schemas are the same where their terms are the same up to the names of their variables, each name in pvar, svar, num, real, nvar, own_var and the speech act of sgn renamed in the order of its first occurrence, as GKK's introduce is pvar('Carol') in GSN.glp and pvar(c) in GC.glp; the first is kept.  The names of the roles are part of the schema, since a pragma names a role by them.
# - The environment predicates and the reflexive predicates are the union of the contracts', in their order.
# - The tabs, list(Name, Words), are taken by name, the same text once, and a tab of one name with different words refused; the buttons by their text, each contract's acts of one button together, the same acts once, and a button of one text whose acts differ refused; a standing declaration by its schema, likewise.  A contract with no pragmas adds none.
# - The introduction predicate is the one the contracts name, no_introduction where none does; two that name different ones are refused.
set -u
[ $# -ge 8 ] && [ $(( $# % 4 )) -eq 0 ] || { echo "usage: compose.sh <NAME> <reading> <pragmas> <intro> [<NAME> <reading> <pragmas> <intro> ...], two contracts or more" >&2; exit 2; }
exec perl -CSD -Mutf8 -e '
use strict; use warnings;
$SIG{__DIE__} = sub { print STDERR $_[0]; exit 1 };   # a refusal exits 1
# The arguments of the term at the top level of the text $s, split at its commas outside brackets and quotes.
sub top_args {
    my ($s) = @_;
    my (@a, $d, $cur, $q); $d = 0; $cur = ""; $q = "";
    my @c = split //, $s;
    for (my $i = 0; $i < @c; $i++) {
        my $ch = $c[$i];
        if ($q ne "") {
            $cur .= $ch;
            if ($ch eq "\\") { $cur .= $c[++$i] if $i + 1 < @c; next }
            $q = "" if $ch eq $q;
            next;
        }
        if ($ch eq "\x27" || $ch eq "\"") { $q = $ch; $cur .= $ch; next }
        if ($ch eq "," && $d == 0) { push @a, $cur; $cur = ""; next }
        $d++ if $ch eq "(" || $ch eq "[";
        $d-- if $ch eq ")" || $ch eq "]";
        $cur .= $ch;
    }
    push @a, $cur if $cur =~ /\S/;
    s/^\s+|\s+$//g for @a;
    return @a;
}
# The elements of the list text $s, "[]" or "[a, b, ...]".
sub elems {
    my ($s, $what) = @_;
    $s =~ s/^\s+|\s+$//g;
    $s =~ /^\[(.*)\]$/s or die "compose.sh: $what is not a list: $s\n";
    return top_args($1);
}
# The functor and the arguments of the compound text $s.
sub compound {
    my ($s) = @_;
    $s =~ /^([a-z][A-Za-z0-9_]*)\((.*)\)$/s or return ($s);
    return ($1, top_args($2));
}
# A schema with its variables renamed in the order of their first occurrence.
sub canonical {
    my ($s) = @_;
    my %ren; my $n = 0;
    my $nm = qr/\x27(?:[^\x27\\]|\\.)*\x27|[a-z][A-Za-z0-9_]*/;
    $s =~ s{\b(pvar|svar|num|real|nvar|own_var|sgn)\(($nm)}{"$1(" . ($ren{$2} //= "v" . ++$n)}ge;
    return $s;
}
sub union_into {   # union_into(\@into, \%seen, @xs): each x not yet in, in order
    my ($into, $seen, @xs) = @_;
    for my $x (@xs) { push @$into, $x unless $seen->{$x}++ }
}
my (@env, %env, @refl, %refl, @schemas, %schema, %schema_of, $has_env, $has_refl);
my (@tabs, %tab, %tab_of, @acts, %button, %button_of, @stands, %stand, %stand_of, $any_pragmas, $three);
my ($intro, $intro_of) = ("no_introduction", "");
my @args = @ARGV;
while (@args) {
    my ($name, $rd, $pr, $in) = splice(@args, 0, 4);
    # The reading: a list of schemas, reflexive(Preds, Schemas), or environment(Preds, Body).
    my $body = $rd; $body =~ s/^\s+|\s+$//g;
    my ($f, @a) = compound($body);
    if ($f eq "environment") {
        $has_env = 1; union_into(\@env, \%env, elems($a[0], "$name: its environment predicates"));
        $body = $a[1]; ($f, @a) = compound($body);
    }
    if ($f eq "reflexive") {
        $has_refl = 1; union_into(\@refl, \%refl, elems($a[0], "$name: its reflexive predicates"));
        $body = $a[1];
    }
    for my $s (elems($body, "$name: its schemas")) {
        my ($sf, $sn) = compound($s);
        defined $sn or die "compose.sh: $name: a schema with no name: $s\n";
        if (exists $schema{$sn}) {
            canonical($schema{$sn}) eq canonical($s)
                or die "compose.sh: the schema $sn is written differently by $schema_of{$sn} and $name, so their composition is refused (vGLP, Section 5.4, \"The contract\x27s words\")\n";
            next;
        }
        $schema{$sn} = $s; $schema_of{$sn} = $name; push @schemas, $s;
    }
    # The pragmas: no_pragmas, pragmas(Lists, Acts) or pragmas(Lists, Acts, Stands).
    $pr =~ s/^\s+|\s+$//g;
    unless ($pr eq "no_pragmas") {
        my ($pf, @pa) = compound($pr);
        $pf eq "pragmas" && (@pa == 2 || @pa == 3) or die "compose.sh: $name: its pragmas are not a pragmas term: $pr\n";
        $any_pragmas = 1;
        for my $l (elems($pa[0], "$name: its tabs")) {
            my (undef, $tn) = compound($l);
            if (exists $tab{$tn}) {
                $tab{$tn} eq $l or die "compose.sh: the tab $tn is written differently by $tab_of{$tn} and $name, so their composition is refused (vGLP, Section 5.4, \"The contract\x27s words\")\n";
                next;
            }
            $tab{$tn} = $l; $tab_of{$tn} = $name; push @tabs, $l;
        }
        my (%mine, @order);
        for my $x (elems($pa[1], "$name: its acts")) {
            my (undef, undef, $bt) = compound($x);
            push @order, $bt unless exists $mine{$bt};
            push @{ $mine{$bt} }, $x;
        }
        for my $bt (@order) {
            my $txt = join(", ", @{ $mine{$bt} });
            if (exists $button{$bt}) {
                $button{$bt} eq $txt or die "compose.sh: the button $bt is written differently by $button_of{$bt} and $name, so their composition is refused (vGLP, Section 5.4, \"The contract\x27s words\")\n";
                next;
            }
            $button{$bt} = $txt; $button_of{$bt} = $name; push @acts, @{ $mine{$bt} };
        }
        if (@pa == 3) {
            $three = 1;
            for my $x (elems($pa[2], "$name: its standing declarations")) {
                my (undef, $sn) = compound($x);
                if (exists $stand{$sn}) {
                    $stand{$sn} eq $x or die "compose.sh: the standing of $sn is written differently by $stand_of{$sn} and $name, so their composition is refused (vGLP, Section 5.4, \"The contract\x27s words\")\n";
                    next;
                }
                $stand{$sn} = $x; $stand_of{$sn} = $name; push @stands, $x;
            }
        }
    }
    # The introduction predicate.
    $in =~ s/^\s+|\s+$//g;
    unless ($in eq "no_introduction") {
        if ($intro eq "no_introduction") { ($intro, $intro_of) = ($in, $name) }
        elsif ($intro ne $in) { die "compose.sh: $intro_of and $name name different introduction predicates, $intro and $in, so their composition is refused\n" }
    }
}
my $reading = "[" . join(", ", @schemas) . "]";
$reading = "reflexive([" . join(", ", @refl) . "], $reading)" if $has_refl;
$reading = "environment([" . join(", ", @env) . "], $reading)" if $has_env;
my $pragmas = "no_pragmas";
if ($any_pragmas) {
    $pragmas = "pragmas([" . join(", ", @tabs) . "], [" . join(", ", @acts) . "]";
    $pragmas .= ", [" . join(", ", @stands) . "]" if $three;
    $pragmas .= ")";
}
print "$reading\n$pragmas\n$intro\n";
' "$@"
