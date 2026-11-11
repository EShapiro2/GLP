#!/bin/bash
# The composition of several contracts, as vGLP's compiler takes them (vGLP 668df7d, Section 5.4, "The contract's words": "The compiler takes one contract or several: of several it prints the platform of their composition, for two that amend a third the schemas of the third that both keep together with the schemas each adds, which is their union where both extend it, and their pragmas likewise, an inherited tab or button standing as above, as the grassroots app's own platform is compiled from four contracts; what two of them inherit from one contract is taken once, and a schema, a tab or a button of one name that two of them write differently is refused"; GSC, Section 8: "The composition of two contracts amending a third is the schemas of the third that both keep together with the schemas each adds; it amends each, and where both extend the third, as GSN and GC extend GKK, it is their union, which extends each").
#
#   bash programs/vglp/cva/compiler/compose.sh [--base <NAME> <reading> <pragmas>] <NAME> <reading> <pragmas> <intro> [<NAME> <reading> <pragmas> <intro> ...]
#
# Each contract is given by its name, as the record names it (GSN), its reading, the term GSC's as_read/2 prints of its schemas, its pragmas, as pragmas.sh prints them of its NAME.txt and that reading, and its introduction predicate, introduction(P) or no_introduction; the third contract they amend, where one is given, by its name, its reading and its pragmas after --base.  Prints three lines, the reading, the pragmas and the introduction predicate of the composition, which the compiler's compile/5 takes as it takes one contract's; or, on a refusal, the line saying why on the standard error, and exits 1.  Exits 2 on a bad argument.
#
# With a third contract, a schema of it is in the composition where every contract keeps it, the same schema, and not where one deletes it or replaces it, writing a schema of its name differently; each contract's schemas that the third lacks or writes differently are its added schemas, and are in the composition.  Without one, every schema of the contracts is, which is the composition where all extend the third.  The pragmas likewise: a tab, a button or a standing declaration of the third's pragmas is in the composition where every contract with pragmas has it as the third writes it, and each contract's that the third lacks or writes differently are; and then a button stands while the composition has an act it names and a tab while it has the predicate the tab lists (vGLP, Section 5.4: "an inherited tab standing while the contract has the predicate it lists, as an inherited button stands while the contract has an act it names").
#
# The forms the paper leaves open, the composer's (README.md beside this file, "Composition"):
# - The schemas are taken in the order of the contracts, each contract's in its own order, a schema of a name already taken being the same schema or refused.  Two schemas are the same where their terms are the same up to the names of their variables, each name in pvar, svar, num, real, nvar, own_var and the speech act of sgn renamed in the order of its first occurrence, as GKK's introduce is pvar('Carol') in GSN.glp and pvar(c) in GC.glp; the first is kept.  The names of the roles are part of the schema, since a pragma names a role by them.
# - The environment predicates and the reflexive predicates are the union of the contracts', in their order; and so are the parameters of a parametric contract, parametric(Params, D), the paper saying nothing of composing one: the composition is parametric in the union, outermost, where any contract is.
# - The tabs, list(Name, Words), are taken by name, the same text once, and a tab of one name with different words refused; the buttons by their text, each contract's acts of one button together, the same acts once, and a button of one text whose acts differ refused; a standing declaration by its schema, likewise.  A contract with no pragmas adds none.
# - The introduction predicate is the one the contracts name, no_introduction where none does; two that name different ones are refused.
# - Which contract is the third is cascade.sh's: the one contract every contract given is written from, itself or through others, that no other such contract is written from (pragmas.sh --from).
set -u
BASE=()
if [ "${1:-}" = "--base" ]; then
    [ $# -ge 4 ] || { echo "usage: compose.sh [--base <NAME> <reading> <pragmas>] <NAME> <reading> <pragmas> <intro> [<NAME> <reading> <pragmas> <intro> ...], two contracts or more" >&2; exit 2; }
    BASE=("$2" "$3" "$4"); shift 4
fi
[ $# -ge 8 ] && [ $(( $# % 4 )) -eq 0 ] || { echo "usage: compose.sh [--base <NAME> <reading> <pragmas>] <NAME> <reading> <pragmas> <intro> [<NAME> <reading> <pragmas> <intro> ...], two contracts or more" >&2; exit 2; }
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
my $why = "(vGLP, Section 5.4, \"The contract\x27s words\")";
# A reading: its environment predicates, its reflexive predicates, and its schemas, the text of each.
sub reading_parts {
    my ($name, $rd) = @_;
    my (@par, @env, @refl, $hp, $he, $hr);
    my $body = $rd; $body =~ s/^\s+|\s+$//g;
    my ($f, @a) = compound($body);
    # A parametric contract, parametric(Params, D), its parameters predicates and constants as any others (GSC, programs/gsc/self.glp: "A parametric contract is checked as it is written").
    if ($f eq "parametric") { $hp = 1; @par = elems($a[0], "$name: its parameters"); $body = $a[1]; ($f, @a) = compound($body) }
    if ($f eq "environment") { $he = 1; @env = elems($a[0], "$name: its environment predicates"); $body = $a[1]; ($f, @a) = compound($body) }
    if ($f eq "reflexive") { $hr = 1; @refl = elems($a[0], "$name: its reflexive predicates"); $body = $a[1] }
    my @s;
    for my $s (elems($body, "$name: its schemas")) {
        my ($sf, $sn) = compound($s);
        defined $sn or die "compose.sh: $name: a schema with no name: $s\n";
        push @s, [$sn, $s, canonical($s)];
    }
    return { par => \@par, env => \@env, refl => \@refl, hp => $hp, he => $he, hr => $hr, schemas => \@s };
}
# Pragmas: no_pragmas, pragmas(Lists, Acts), pragmas(Lists, Acts, Stands) or pragmas(Lists, Acts, Stands, Look, Version, Text) (README.md beside this file, 1); their tabs by name, their buttons by text, each with its acts together, and their standing declarations by schema, each [key, text, items], in order.
sub pragma_parts {
    my ($name, $pr) = @_;
    $pr =~ s/^\s+|\s+$//g;
    return undef if $pr eq "no_pragmas";
    my ($pf, @pa) = compound($pr);
    $pf eq "pragmas" && (@pa == 2 || @pa == 3 || @pa == 6) or die "compose.sh: $name: its pragmas are not a pragmas term: $pr\n";
    my (@tabs, @buttons, @stands, %mine, @order);
    for my $l (elems($pa[0], "$name: its tabs")) { my (undef, $tn) = compound($l); push @tabs, [$tn, $l, [$l]] }
    for my $x (elems($pa[1], "$name: its acts")) {
        my (undef, undef, $bt) = compound($x);
        push @order, $bt unless exists $mine{$bt};
        push @{ $mine{$bt} }, $x;
    }
    push @buttons, [$_, join(", ", @{ $mine{$_} }), $mine{$_}] for @order;
    if (@pa >= 3) { for my $x (elems($pa[2], "$name: its standing declarations")) { my (undef, $sn) = compound($x); push @stands, [$sn, $x, [$x]] } }
    return { n => scalar(@pa), tabs => \@tabs, buttons => \@buttons, stands => \@stands, look => $pa[3], version => $pa[4], text => $pa[5] };
}
# The composition of one kind of item, [key, text, payload] each, of the contracts, given in order, over the third\x27s, where one is given: an item of the third is in it where every contract has it as the third writes it, and each contract\x27s that the third lacks or writes differently is, two of one key written differently refused.  $same compares two texts.
sub compose_items {
    my ($what, $same, $base, @cs) = @_;   # $base: undef or [items]; @cs: [name, [items]] each
    my %b; if ($base) { $b{$_->[0]} = $_->[1] for @$base }
    my %kept;
    if ($base) {
        for my $k (keys %b) {
            my $all = 1;
            for my $c (@cs) {
                my ($it) = grep { $_->[0] eq $k } @{ $c->[1] };
                do { $all = 0; last } unless $it && $same->($it->[1], $b{$k});
            }
            $kept{$k} = 1 if $all;
        }
    }
    my (@out, %taken, %of);
    for my $c (@cs) {
        for my $it (@{ $c->[1] }) {
            my ($k, $t) = @$it;
            my $inherited = $base && exists $b{$k} && $same->($t, $b{$k});
            next if $inherited && !$kept{$k};
            if (exists $taken{$k}) {
                $same->($taken{$k}, $t) or die "compose.sh: the $what $k is written differently by $of{$k} and $c->[0], so their composition is refused $why\n";
                next;
            }
            $taken{$k} = $t; $of{$k} = $c->[0]; push @out, $it;
        }
    }
    return @out;
}
my $eq = sub { $_[0] eq $_[1] };
my $schema_same = sub { canonical($_[0]) eq canonical($_[1]) };
my @args = @ARGV;
my $nbase = shift @args;
my ($bname, $brd, $bpr) = $nbase ? splice(@args, 0, 3) : ();
my $bread = $nbase ? reading_parts($bname, $brd) : undef;
my $bprag = $nbase ? pragma_parts($bname, $bpr) : undef;
my (@par, %par, $has_par, @env, %env, @refl, %refl, $has_env, $has_refl, @creads, @cprags, $six, $three, $look, $version, @texts);
my ($intro, $intro_of) = ("no_introduction", "");
while (@args) {
    my ($name, $rd, $pr, $in) = splice(@args, 0, 4);
    my $r = reading_parts($name, $rd);
    $has_par ||= $r->{hp}; $has_env ||= $r->{he}; $has_refl ||= $r->{hr};
    union_into(\@par, \%par, @{ $r->{par} });
    union_into(\@env, \%env, @{ $r->{env} }); union_into(\@refl, \%refl, @{ $r->{refl} });
    push @creads, [$name, [map { [$_->[0], $_->[1]] } @{ $r->{schemas} }]];
    my $p = pragma_parts($name, $pr);
    if ($p) {
        push @cprags, [$name, $p];
        $three = 1 if $p->{n} >= 3;
        # The paper says nothing of the look, the version and the text of a composition: its look is the first contract\x27s that writes one, its version the first contract\x27s, and its text the contracts\x27 texts one after another.
        if ($p->{n} == 6) {
            $six = 1;
            $look = $p->{look} if !defined $look || $look eq "nolook";
            $version //= $p->{version};
            push @texts, $p->{text};
        }
    }
    # The introduction predicate.
    $in =~ s/^\s+|\s+$//g;
    unless ($in eq "no_introduction") {
        if ($intro eq "no_introduction") { ($intro, $intro_of) = ($in, $name) }
        elsif ($intro ne $in) { die "compose.sh: $intro_of and $name name different introduction predicates, $intro and $in, so their composition is refused\n" }
    }
}
my @schemas = map { $_->[1] } compose_items("schema", $schema_same, $bread ? [map { [$_->[0], $_->[1]] } @{ $bread->{schemas} }] : undef, @creads);
my %has_act = map { my (undef, $n) = compound($_); ($n => 1) } @schemas;
my $reading = "[" . join(", ", @schemas) . "]";
$reading = "reflexive([" . join(", ", @refl) . "], $reading)" if $has_refl;
$reading = "environment([" . join(", ", @env) . "], $reading)" if $has_env;
$reading = "parametric([" . join(", ", @par) . "], $reading)" if $has_par;
my $pragmas = "no_pragmas";
if (@cprags) {
    my $bp = $bprag;
    my @tabs = compose_items("tab", $eq, $bp ? $bp->{tabs} : ($nbase ? [] : undef), map { [$_->[0], $_->[1]{tabs}] } @cprags);
    my @buttons = compose_items("button", $eq, $bp ? $bp->{buttons} : ($nbase ? [] : undef), map { [$_->[0], $_->[1]{buttons}] } @cprags);
    my @stands = compose_items("standing of", $eq, $bp ? $bp->{stands} : ($nbase ? [] : undef), map { [$_->[0], $_->[1]{stands}] } @cprags);
    # A button stands while the composition has an act it names, a tab while it has the predicate it lists, a standing declaration while it has its schema.
    @buttons = grep { my $b = $_; grep { my (undef, $sn) = compound($_); $sn =~ s{^\x27|\x27$}{}g; $has_act{$sn} } @{ $b->[2] } } @buttons;
    @stands = grep { (my $k = $_->[0]) =~ s{^\x27|\x27$}{}g; $has_act{$k} } @stands;
    @tabs = grep {
        my (undef, undef, $ps) = compound($_->[1]);
        my @p = map { (my $q = $_) =~ s/^\x27|\x27$//g; $q } elems($ps, "a tab\x27s predicates");
        grep { my $q = $_; $reading =~ /\batom\(\x27?\Q$q\E\x27?,/ } @p;
    } @tabs;
    $pragmas = "pragmas([" . join(", ", map { $_->[1] } @tabs) . "], [" . join(", ", map { @{ $_->[2] } } @buttons) . "]";
    $pragmas .= ", [" . join(", ", map { $_->[1] } @stands) . "]" if $three || $six;
    if ($six) {
        my @inner = map { (my $t = $_) =~ s/^\x27"//; $t =~ s/"\x27$//; $t } @texts;
        $pragmas .= ", " . ($look // "nolook") . ", " . ($version // "version(1, 1)") . ", \x27\"" . join("\\\\n\\\\n", grep { $_ ne "" } @inner) . "\"\x27";
    }
    $pragmas .= ")";
}
print "$reading\n$pragmas\n$intro\n";
' "${#BASE[@]}" ${BASE[@]+"${BASE[@]}"} "$@"
