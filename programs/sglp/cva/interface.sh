#!/bin/bash
# The interface of a canonical CVA platform in sGLP's term form
# (programs/sglp/README.md, "The term form"; sGLP's task of 2026-10-18 20:50
# UTC, item 1), printed from the platform as the cascade emits it: its .vglp,
# as programs/vglp/cva/compiler prints it, and its .glp, the canonical
# compilation the REPL's :emit writes (vGLP, Section 5.4 and Definition
# "Canonical Compilation").
#
#   bash programs/sglp/cva/interface.sh <platform.vglp> <platform.glp> <NAME>
#
# prints, to its standard output, a module of one unit clause, interface(V), V
# the list the transformation reads as a source's interface:
#
#   type(T, Alts)       each type definition of the .vglp, in its order, and
#                       then each type of the root programs/self.glp that
#                       those or the volitional declarations name, directly
#                       or through another, and the .vglp does not define,
#                       in the root's order; the
#                       primitive and predefined types the transformation
#                       knows (Integer, Real, Number, String, Constant,
#                       Module) are not restated;
#   volitional(T, Qt, t, p(Ts))
#                       each volitional procedure declaration of the .vglp,
#                       procedure (T)*p(Ts) or (T?)*p(Ts), in its order, Qt
#                       and t the type constant and the functor of the ask
#                       its asking clause in the .glp writes,
#                       ask(Qt, t(X?)) or ask(Qt, t(X));
#   compiled(p, n)      each procedure the .glp declares, in its order.
#
# A type T is 'T', T? is dual('T'), a type T(T1, ..., Tn) is 'T'(T1, ...,
# Tn), a parameterised definition's name 'T'('P1', ..., 'Pn'), its
# parameters constants; an alternative that is a term keeps its functor,
# a list [] and [X | T] are themselves.  The lines are a GLP module: the
# declaration exported procedure interface(SglpTerms), SglpTerms the term
# form's type, which the root exposes.  Exits 0 when the .vglp holds a type
# definition and a volitional declaration whose asking clause the .glp
# holds, 1 otherwise, 2 on a bad argument.

set -u
[ $# -eq 3 ] || { echo "usage: interface.sh <platform.vglp> <platform.glp> <NAME>" >&2; exit 2; }
VGLP=$1; GLP=$2; NAME=$3
for f in "$VGLP" "$GLP"; do [ -f "$f" ] || { echo "interface.sh: no file $f" >&2; exit 2; }; done
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)/self.glp"
[ -f "$ROOT" ] || { echo "interface.sh: no root $ROOT" >&2; exit 2; }

perl -e '
use strict; use warnings;
my ($vglp, $glp, $root, $name) = @ARGV;
sub slurp { my $f = shift; open(my $h, "<", $f) or die "cannot read $f\n"; local $/; my $s = <$h>; close $h; return $s; }
# A source with its comments dropped, quoted atoms kept.
sub uncomment {
    my $s = shift; my $out = "";
    while (length $s) {
        if ($s =~ s/^(\x27(?:[^\x27\\]|\\.)*\x27)//s) { $out .= $1; }
        elsif ($s =~ s/^%[^\n]*//) { }
        elsif ($s =~ s/^([^\x27%]+)//s) { $out .= $1; }
        else { $out .= substr($s, 0, 1, ""); }
    }
    return $out;
}
# The tokens of a type expression.
sub tokens {
    my $s = shift; my @t;
    while (length $s) {
        if ($s =~ s/^\s+//) { next; }
        if ($s =~ s/^(\x27(?:[^\x27\\]|\\.)*\x27)//) { push @t, $1; next; }
        if ($s =~ s/^([A-Za-z_][A-Za-z0-9_]*)//) { push @t, $1; next; }
        if ($s =~ s/^([0-9]+(?:\.[0-9]+)?)//) { push @t, $1; next; }
        if ($s =~ s/^([()\[\],;|?])//) { push @t, $1; next; }
        die "interface.sh: cannot read the type text at: $s\n";
    }
    return @t;
}
# A type expression in the term form: parse_alt reads a term and its ?.
my @T;
sub peek { return @T ? $T[0] : ""; }
sub take { my $x = shift @T; die "interface.sh: the type text ends early\n" unless defined $x; return $x; }
sub expect { my $w = shift; my $x = take(); die "interface.sh: expected $w, read $x\n" unless $x eq $w; }
sub parse_alt { my $t = parse_term(); if (peek() eq "?") { take(); return "dual($t)"; } return $t; }
sub parse_args { my @a = (parse_alt()); while (peek() eq ",") { take(); push @a, parse_alt(); } expect(")"); return join(", ", @a); }
sub parse_term {
    my $x = take();
    if ($x eq "[") {
        if (peek() eq "]") { take(); return "[]"; }
        my @e = (parse_alt());
        while (peek() eq ",") { take(); push @e, parse_alt(); }
        my $tail = "";
        if (peek() eq "|") { take(); $tail = " | " . parse_alt(); }
        expect("]");
        return "[" . join(", ", @e) . $tail . "]";
    }
    if ($x =~ /^[A-Z_]/) {
        my $q = "\x27$x\x27";
        if (peek() eq "(") { take(); return "$q(" . parse_args() . ")"; }
        return $q;
    }
    if ($x =~ /^[a-z]/ || $x =~ /^\x27/ || $x =~ /^[0-9]/) {
        if (peek() eq "(") { take(); return "$x(" . parse_args() . ")"; }
        return $x;
    }
    die "interface.sh: unexpected $x in a type\n";
}
sub term_of { @T = tokens(shift); my $t = parse_alt(); die "interface.sh: trailing text in a type\n" if @T; return $t; }
sub alts_of {
    @T = tokens(shift); my @a = (parse_alt());
    while (peek() eq ";") { take(); push @a, parse_alt(); }
    die "interface.sh: trailing text in a type definition\n" if @T;
    return @a;
}
# The type definitions of a source, in order: [name, head term, [alts], names used].
sub type_defs {
    my $s = uncomment(shift); my @d;
    while ($s =~ /^[ \t]*([A-Z][A-Za-z0-9_]*)[ \t]*(\(([^()]*)\))?[ \t]*::=(.*?)\.[ \t]*$/msg) {
        my ($n, $ps, $body) = ($1, $3, $4);
        my $head = "\x27$n\x27";
        if (defined $ps) { $head .= "(" . join(", ", map { s/^\s+|\s+$//gr } map { "\x27" . (s/^\s+|\s+$//gr) . "\x27" } split(/,/, $ps)) . ")"; }
        my %used; $used{$_} = 1 for ($body =~ /\b([A-Z][A-Za-z0-9_]*)\b/g);
        push @d, [$n, $head, $body, [sort keys %used]];
    }
    return @d;
}
my $v = slurp($vglp); my $g = uncomment(slurp($glp));
my @defs = type_defs($v);
my %defined = map { $_->[0] => 1 } @defs;
# The root types the definitions name, directly or through another.
my %prim = map { $_ => 1 } qw(Integer Real Number String Constant Module);
my @rootdefs = type_defs(slurp($root));
my %rootdef = map { $_->[0] => $_ } @rootdefs;
my %need; my @todo = map { @{$_->[3]} } @defs;
{ my $vu0 = uncomment($v); while ($vu0 =~ /^[ \t]*(?:exported[ \t]+)?procedure[ \t]*\([^)]*\)[ \t]*\*[ \t]*[a-z][A-Za-z0-9_]*[ \t]*\((.*?)\)[ \t]*\.[ \t]*$/msg) { push @todo, ($1 =~ /\b([A-Z][A-Za-z0-9_]*)\b/g); } }
while (@todo) {
    my $u = shift @todo;
    next if $defined{$u} || $prim{$u} || $need{$u} || !$rootdef{$u};
    $need{$u} = 1; push @todo, @{$rootdef{$u}->[3]};
}
# The asking clauses of the .glp: the ask each procedure writes.
my %ask;
while ($g =~ /^([a-z][A-Za-z0-9_]*)\(.*?\[\s*ask\(\s*(\x27(?:[^\x27\\]|\\.)*\x27)\s*,\s*([a-z][A-Za-z0-9_]*)\(\s*X\??\s*\)\s*\)\s*\|\s*D\?\s*\]\s*\)\s*:-/mg) {
    $ask{$1} = [$2, $3] unless $ask{$1};
}
# The volitional declarations of the .vglp.
my $vu = uncomment($v); my @vols;
while ($vu =~ /^[ \t]*(?:exported[ \t]+)?procedure[ \t]*\([ \t]*(.*?)[ \t]*\)[ \t]*\*[ \t]*([a-z][A-Za-z0-9_]*)[ \t]*\((.*?)\)[ \t]*\.[ \t]*$/msg) {
    my ($t, $p, $args) = ($1, $2, $3);
    my $tt = term_of($t);
    my $a = $ask{$p} or die "interface.sh: no asking clause of $p in the .glp\n";
    my $qt = $a->[0]; $qt =~ s/\x27/\\\x27/g; $qt = "\x27$qt\x27";
    @T = tokens($args); my $at = @T ? parse_args_list() : "";
    push @vols, "volitional($tt, $qt, $a->[1], $p($at))";
}
sub parse_args_list { my @a = (parse_alt()); while (peek() eq ",") { take(); push @a, parse_alt(); } die "interface.sh: trailing text in a declaration\n" if @T; return join(", ", @a); }
# The procedures the .glp declares.
my @comp; my %seen;
while ($g =~ /^[ \t]*(?:exported[ \t]+)?procedure(?:\([^)]*\))?[ \t]+([a-z][A-Za-z0-9_]*)[ \t]*(\((.*?)\))?[ \t]*\.[ \t]*$/msg) {
    my ($p, $args) = ($1, $3);
    my $n = 0;
    if (defined $args && $args =~ /\S/) { my $d = 0; $n = 1; for my $c (split //, $args) { $d++ if $c eq "(" || $c eq "["; $d-- if $c eq ")" || $c eq "]"; $n++ if $c eq "," && $d == 0; } }
    my $k = "$p/$n"; next if $seen{$k}++; push @comp, "compiled($p, $n)";
}
die "interface.sh: the .vglp holds no type definition\n" unless @defs;
die "interface.sh: the .vglp holds no volitional declaration\n" unless @vols;
my @items;
push @items, "type($_->[1], [" . join(", ", alts_of($_->[2])) . "])" for @defs;
for my $r (@rootdefs) { push @items, "type($r->[1], [" . join(", ", alts_of($r->[2])) . "])" if $need{$r->[0]}; }
push @items, @vols, @comp;
print "%% The interface of the canonical CVA platform of $name in sGLP\x27s term form\n";
print "%% (programs/sglp/README.md, \"The term form\"), printed by programs/sglp/cva/interface.sh\n";
print "%% from platform.vglp and platform.glp beside it: the types the platform defines\n";
print "%% and those of the root it names, its volitional procedure declarations, and\n";
print "%% compiled(P, N) of each procedure of its canonical compilation.  An owner\x27s\n";
print "%% sGLP source is transformed against it (programs/sglp/transform.sh --interface).\n";
print "\nexported procedure interface(SglpTerms).\ninterface([\n";
print join(",\n", map { "  $_" } @items), "\n]).\n";
' "$VGLP" "$GLP" "$ROOT" "$NAME"
