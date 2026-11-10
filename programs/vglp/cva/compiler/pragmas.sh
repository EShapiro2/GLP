#!/bin/bash
# The pragmas of a contract, as the term vGLP's compiler reads them (README.md beside this file, "The pragmas"; vGLP, Section 5.4, "The contract's words"; GSC, Section 2, its first paragraph): the lines after the line "Pragmas" of the contract's NAME.txt and of the NAME.txt of every contract it extends, by its "% Extends:" line and theirs, the contracts extended first.  A contract whose own NAME.txt has no line "Pragmas" has none, no_pragmas, which the compiler refuses: "the compiler prints a platform only from a contract with pragmas, and refuses one with none" (vGLP ffcb093, Section 5.4, "The contract's words").  A contract "has their tabs with its own, a tab of an inherited tab's name replacing it" (vGLP 14d9064, Section 5.4, "The contract's words", its last sentence): the tab it writes stands in the place of the inherited one, which is gone.  A button is of the acts it names, and a button replaces a button (vGLP 4236349, Section 5.4, "The contract's words", its last sentence): "an inherited button, with its fields and its questions, stands while the contract has an act it names, an act the contract replaces being gone and its button with it; and where the contract writes a button of an inherited button's name, its button, with its fields and its questions, replaces the inherited one, the other buttons of the inherited line standing".  A line's fields name, in the schemas' order, the variables of its button's acts that the atoms each requires at the proposer's role do not bind, and a line whose fields are not these is refused (ibid.).  The fields after a button, up to the next button, are that button's, and two lines that stand writing one button are refused, one name being one button with one line (ibid.: "for each act they propose, its button, in brackets, bearing the act's name, and its fields, in angle brackets"; "A button is of the acts it names").
#
#   bash programs/vglp/cva/compiler/pragmas.sh <NAME.txt>
#
# prints pragmas(Lists, Acts) or no_pragmas on one line, each string as its GLP source text inside a quoted name, since the compiler writes a word as it is, the reader reading the double-quoted string back as the word.  The lines after "Pragmas" up to the first that names a clause are lists, one for each tab, "Name: what it lists", list(Name, Words); every other line is a clause's, in GSC's form, its proposer, its buttons and fields, and its indented lines (README.md beside this file, 1), each act act(Schema, Button, Fields, Sentence, Tab, Proposer), Sentence said(Questions) or none, a question's words text(S) or person(I), I the rank among the schema's persons of the role the name bears, by the names the schema term gives its roles (GSC 25b6b3c, Section 2), Proposer by([G]).  A contract extended is found beside the contract, or else under programs/ by its NAME.txt.  Exits 1, naming the line, on a pragma it cannot read or a clause it cannot find, and on every line it refuses, each named with the variable its fields leave unnamed or the field beyond them; 2 on a bad argument.  With PRAGMAS_LINES set it prints on the standard error each button line and the variables its fields name by the rule, or why it is refused, replaced or gone.

# A NAME.txt may carry two kinds of pragmas under their headings, "Interface pragmas" and "Implementation pragmas", "Pragmas" alone meaning the interface ones (GSC, sections/02-contracts.tex:3; GSA, sections/s3-super-app.tex:69): the interface pragmas are read as above, and the implementation pragmas are skipped, no paper yet saying what form they take in the app's code.

set -u
[ $# -ge 1 ] && [ $# -le 2 ] && [ -f "$1" ] || { echo "usage: pragmas.sh <NAME.txt> [<contract term>]" >&2; exit 2; }
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
exec perl -CSD -Mutf8 -e '
use strict; use warnings;
my ($top, $programs, $term) = @ARGV;
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
# The variables local to a role of the schema whose arguments are @args: "A variable that occurs only in atoms the schema requires or deletes at one role, and in the constraint, is local to that role" (vGLP 8a3ae79, Section 5.4, "Speech-act schemas as local specifications"), an agent, speech-act or numeric variable alike, inside a name term or a signed term included; each named by its name, as the head writes it.
sub var_tokens {
    my ($t) = @_;
    $t =~ s/\batom\(\w+,/atom(/g;
    return map { (my $x = $_) =~ s/^\x27|\x27$//g; $x } ($t =~ /(\x27(?:[^\x27\\]|\\.)*\x27|\b[A-Za-z_]\w*\b)(?!\s*\()/g);
}
sub local_vars {
    my ($roles) = @_;
    $roles =~ s/^\[//; $roles =~ s/\]$//;
    my (%at, %elsewhere);
    my $r = 0;
    for my $spec (top_args($roles)) {
        $r++;
        my ($f) = $spec =~ /^(c?rs)\(/ or next;
        my ($body) = inside($spec, length $f);
        my @p = top_args($body);
        if ($f eq "crs") { my $kind = shift @p; $elsewhere{$_} = 1 for var_tokens($kind) }
        for my $k (1 .. $#p) {
            for my $x (var_tokens($p[$k])) {
                if ($k == 2 || $k == 3) { $at{$x}{$r} = 1 } else { $elsewhere{$x} = 1 }
            }
        }
    }
    return grep { !$elsewhere{$_} && keys %{ $at{$_} } == 1 } keys %at;
}
my (@snames, %svars, %preds, %locals);
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
        next if !defined $a || exists $svars{$n};
        push @snames, $n;
        my $rarg = $kw eq "qschema" ? $args[$named ? 5 : 4] : $args[-1];
        my %local = map { $_ => 1 } local_vars($rarg);
        $a =~ s/^\[//; $a =~ s/\]$//;
        my @hv = map { (my $x = $_) =~ s/^\x27|\x27$//g; $x } ($a =~ /\w+\((\x27(?:[^\x27\\]|\\.)*\x27|\w+)\)/g);
        $locals{$n} = { map { $_ => 1 } grep { $local{$_} } @hv };
        $svars{$n} = [ grep { !$local{$_} } @hv ];
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
my (%roles, %head, %rnames, %ones, %tabpred, @pending, %covered, @lines_out, @faults, %cons);
sub find_schemas {
    my ($t) = @_;
    return if @$t == 1;
    if ($t->[0] =~ /^[ncq]?schema$/ && @{ $t->[1][0] } == 1) {
        my $n = $t->[1][0][0];
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
                (my $o = $e->[1][1][0]) =~ s/^\x27|\x27$//g;
                my ($j) = grep { $rnames{$n}[$_ - 1] eq $o } 1 .. scalar @{ $rnames{$n} };
                push @{ $ones{$n} }, [$i, $j] if defined $j;
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
                        atoms => [ map { [ map { $_->[0] eq "times" ? $_->[1][1] : $_ } @{ $_->[1] } ] } @c ] };
        }
        $roles{$n} = \@rs;
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
        for my $k (0 .. 3) { for my $a (@{ $rs->{atoms}[$k] }) { $named_at{$_}{$r} = 1 for atom_keys($a) } }
    }
    for my $x (keys %{ $locals{$n} || {} }) { my @rr = keys %{ $named_at{"v$x"} || {} }; $role_of{"v$x"} = $rr[0] if @rr == 1 }
    my $more = 1;
    while ($more) { $more = 0;
    for my $c (con_conjs($cons{$n})) {
        next unless $c->[0] eq "eq";
        my %vs = map { $_ => 1 } (expr_nvars($c->[1][0]), expr_nvars($c->[1][1]));
        my @nl = grep { !$role_of{$_} } keys %vs;
        my %lr = map { $role_of{$_} => 1 } grep { $role_of{$_} } keys %vs;
        next unless @nl == 1 && keys %lr == 1;
        my ($v) = @nl; my ($i) = keys %lr;
        next unless lin_coef(["minus", [$c->[1][0], $c->[1][1]]], $v) != 0;
        my @at = keys %{ $named_at{$v} || {} };
        next unless @at == 1 && $at[0] == $i;
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
my %named_by_button;
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
sub find_txt {
    my ($name, $beside) = @_;
    my $f = "$beside/$name.txt";
    return $f if -f $f;
    for my $g (glob("$programs/*/$name.txt"), glob("$programs/*/*/$name.txt")) { return $g }
    return undef;
}
# The headings of the pragmas (GSC, Section 2, its first paragraph, sections/02-contracts.tex:3; GSA, sections/s3-super-app.tex:69; GSC #13, Code_inbox.md, 2026-11-07 19:29 UTC, 2): a NAME.txt may carry "Interface pragmas" and "Implementation pragmas", "Pragmas" alone meaning the interface ones.  The interface pragmas are read as the pragmas were; the implementation pragmas are skipped, no paper yet saying what form they take in the app\x27s code.
sub iface_head { my ($x) = @_; return $x eq "Pragmas" || $x eq "Interface pragmas" }
sub impl_head { my ($x) = @_; return $x eq "Implementation pragmas" }
sub any_head { my ($x) = @_; return iface_head($x) || impl_head($x) }
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
# The contracts a contract names on the line saying what it is written from, the first after its title and before its names declaration, "<NAME> <relation> <OTHER> (...)." or with no parentheses, the relation one of GSC\x27s five, amends, extends, instantiates, implements and restricts (GSC, Section 8, Relations among Contracts, Definitions Amendment, extension, instantiation and Implementation, restriction), several contracts joined by "and", and several relations joined by "and", each with the contracts after it, as "GAI extends GnW and amends GFP (...)." (GAI.txt; GAI #12, Code_inbox.md, 2026-11-08 08:10 UTC) (style guide 36; vGLP #23, Code_inbox.md, 2026-11-06 07:17 UTC, 1); nothing where the contract has no such line.  A name is read whole with a hyphened part, as GDO-L.
sub relation_names {
    my ($f, @l) = @_;
    my @body = grep { !/^%/ && /\S/ } @l;
    return () unless @body >= 2;
    my $nm = qr/[A-Z][A-Za-z0-9]*(?:-[A-Z][A-Za-z0-9]*)*/;
    my $rel = qr/(?:amends|extends|instantiates|implements|restricts)/;
    return () unless $body[1] =~ /^$nm\s+($rel\s+.*?)\.\s*$/;
    my $of = $1;
    $of =~ s/\s*\([^()]*\)//g;
    # Each relation with the contracts after it, up to the next relation: "extends GnW and amends GFP" is extends GnW, amends GFP.
    my @names;
    for my $part (split /(?:\s*,\s*|\s+)(?:and\s+)?(?=$rel\s)/, $of) {
        $part =~ s/^$rel\s+// or die "pragmas.sh: $f: the line saying what the contract is written from, \"$body[1]\", names no relation at \"$part\"\n";
        for my $n (split /\s*,\s*(?:and\s+)?|\s+and\s+/, $part) {
            $n =~ /^$nm$/ or die "pragmas.sh: $f: the line saying what the contract is written from, \"$body[1]\", names no contract at \"$n\"\n";
            push @names, $n;
        }
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
        (my $dir = $f) =~ s{/[^/]*$}{};
        my %named;
        for my $n (@rel) {
            next if $named{$n}++;
            my $g = find_txt($n, $dir);
            defined $g or die "pragmas.sh: $f: the contract $n, which its line saying what it is written from names, has no $n.txt\n";
            visit($g);
            push @{ $extends{$f} }, $g;
        }
    } elsif (defined $ext) {
        (my $dir = $f) =~ s{/[^/]*$}{};
        my %named;
        # The contracts it extends are named in the first sentence of the line, a file name such as GC.txt in it not a contract extended.
        (my $first = $ext) =~ s/^% Extends:\s*//;
        $first =~ s/\.\s.*$//;
        # A name may carry a hyphened part, as GDO-L, which GFF extends (GFF.txt), read whole and not as GDO.
        for my $n ($first =~ /\b([A-Z][A-Za-z]*[A-Z](?:-[A-Z][A-Za-z]*)*)\b(?!-|[\x27\x{2019}]s|\.\w)/g) {
            next if $named{$n}++;
            my $g = find_txt($n, $dir);
            next unless defined $g;
            visit($g);
            push @{ $extends{$f} }, $g;
        }
    }
    push @order, $f;
}
my @own = read_txt($top);
unless (grep { iface_head($_) } @own) { print "no_pragmas\n"; exit 0 }
visit($top);
my (@tabs, @lists, @acts);
# The names: those the contract declares, on its line after the title and before clause 1, "Alice, Bob and Carol are persons." (Udi, 2026-10-25, to GSC #8; GKK.txt at GLP ec7c578c), the declaration being part of the contract the reader takes; and, of a contract that declares none, the words of a capital and lower-case letters its clauses write within a sentence, not first in it.
my %is_name;
for my $f (@order) {
    my $declared = 0;
    for my $x (read_txt($f)) {
        next if $x =~ /^%/;
        last if $x =~ /^\d+\./ || any_head($x);
        # The declaration names every name it lists, of every kind, as "Alice, Bob and Erin are persons, Carol and Dave wards." names five (vGLP 722343d, Section 5.4: "a name the contract\x27s declaration gives a person or a ward"; GSC, Section 2: "a declaration of the contract\x27s names, saying what each names").
        next unless $x =~ /^(.*?\s+(?:is|are)\s+(?:a\s+)?persons?\b[^.]*)\.(?:\s|$)/;
        $declared = 1;
        $is_name{$_} = 1 for ($1 =~ /\b([A-Z][a-z]+)\b/g);
    }
    next if $declared;
    for my $x (read_txt($f)) {
        last if any_head($x);
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
        last if any_head($x);
        if ($x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.]+)\.\s*(.*)$/) {
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
    my (@m, $aft, $impl, $first);
    delete $version{$f};
    for my $x (read_txt($f)) {
        if (impl_head($x)) { $impl = 1; next }
        if (iface_head($x)) { $impl = 0; $aft = 1; $first = 1; push @m, "Pragmas"; next }
        next if $impl;
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
my (%line_buttons, @line_keys, %line_shown);
for my $f (@order) {
    my %own; my $after = 0; my $tabs = 1; my $ln = -1;
    for my $x (joined($f)) {
        $ln++;
        next if $x =~ /^%/;
        if (!$after) {
            $own{$2} = 1 if $x =~ /^(\d+)\.\s+(?:[\x{25A0}\x{25C6}\x{25CB}]\s+)?([^.]+)\.\s*(.*)$/;
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
            $line_buttons{$k} = [ map { (my $a = lc $_) =~ s/ +/_/g; $a } ($part =~ /\[([^\]]+)\]/g) ];
            (my $nm = $f) =~ s{.*/}{}; $nm =~ s/\.txt$//;
            (my $txt = $part) =~ s/^\s+//;
            $line_shown{$k} = "$nm: $title: $txt";
        }
    }
}
# A button of a contract is replaced where a contract extending it writes a button of its name: "where the contract writes a button of an inherited button’s name, its button, with its fields and its questions, replaces the inherited one, the other buttons of the inherited line standing" (vGLP 4236349, Section 5.4, "The contract’s words", its last sentence), as GSN’s [Close] on a line of its own would leave the [Create] of the line it was on.  $replaced{K}{B} is the line of the extending contract that replaces the button B of the line K, and a line all of whose buttons are replaced is gone, its questions with it.
my %replaced;
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
    (my $name = $f) =~ s{.*/}{}; $name =~ s/\.txt$//;
    my %clause; my %body; my $after = 0; my $first = 1; my $tabs = 1; my $tab = "";
    for my $x (@l) {
        $ln++;
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
                    (my $ba = lc $buttons[0]) =~ s/ +/_/g;
                    if ($replaced{$lk} && $replaced{$lk}{$ba}) {
                        (my $g = (split /\x02/, $replaced{$lk}{$ba})[0]) =~ s{.*/}{};
                        push @lines_out, "$name: $title: $pr->{name} [$buttons[0]]" . join("", map { " <$_>" } @fields) . ": replaced by the line of $g with a button of the same name, the other buttons of the line standing";
                        next;
                    }
                    # An inherited button stands while the contract has an act it names, and goes with its fields and its questions where it has none (vGLP 40f8444, Section 5.4, "The contract’s words", its last sentence: "an inherited button, with its fields and its questions, stands while the contract has an act it names, an act the contract replaces being gone and its button with it"), as GC’s [Mint] in DGC, whose term has no mint; a button of the contract itself that names no act is a fault (below).
                    if ($f ne $top && @snames) {
                        my @gone = grep { (my $a = lc $_) =~ s/ +/_/g; !grep { exists $roles{$_} } realising($a) } @buttons;
                        if (@gone) {
                            push @lines_out, "$name: $title: $pr->{name} " . join("", map { "[$_]" } @gone) . ": gone, the contract having no act it names";
                            my %g = map { $_ => 1 } @gone;
                            @buttons = grep { !$g{$_} } @buttons;
                            next unless @buttons;
                        }
                    }
                    my (%said, %by, %plof, %fit, %err, %reals);
                    for my $button (@buttons) {
                        (my $a = lc $button) =~ s/ +/_/g;
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
                            my @un = grep { $_ != $pl && !exists $q{$_} } @g;
                            !@un or die "pragmas.sh: $f: the pragma \"$title\" names no person in the guarding role " . join(", ", @un) . " of $S, who is then never asked\n";
                            # Two roles the clause says are possibly one person may be bound to one person, and a field offers that person for both places (GSC 316d875, Section 2; vGLP, Sections 5.4 and 6.2): same(K, J), K and J their places in the request, its listed roles left out.
                            my %rplace; { my $k = 0; my $jj = 0; for my $r (@{ $roles{$S} }) { $jj++; next if $r->{listed}; $k++; $rplace{$jj} = $k } }
                            my @same = map { "same($rplace{$_->[0]}, $rplace{$_->[1]})" } grep { exists $rplace{ $_->[0] } && exists $rplace{ $_->[1] } } @{ $ones{$S} // [] };
                            $by{$S} = @same ? "by([$pl], [" . join(", ", @same) . "])" : "by([$pl])";
                            $plof{$S} = $pl;
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
    for my $k (@last) { push @lists, $k->{list} unless $tab_placed{$k}++ }
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
        next unless $v->[0] =~ /^(pvar|svar|num|real)$/;
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
    return ("v" . $uq->($as->[0][0])) if $fn =~ /^(pvar|svar|num|real|own_var)$/ && @$as == 1;
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
            my %d = map { $_ => 1 } det_closure($cons{$S}, \%known);
            next if $d{$k};
        }
        push @out, $_;
    }
    return @out;
}
# The numeric variables a constraint gives finitely many values once those of %$known are bound, taken again with each it gives, as local.glp’s con_det and determined decide it: succ(X, Y) gives Y once X is bound and X once Y is; eq(X, Y) its one numeric variable not bound, where its coefficient in X - Y is not 0; and(C1, C2) what either gives; or(C1, C2) what both give; the rest give none.
sub nvar_key { my ($e) = @_; return undef unless @$e == 2 && $e->[0] eq "num"; my $x = $e->[1][0][0]; return undef if $x =~ /^-?\d+$/; $x =~ s/^\x27|\x27$//g; return "v$x" }
sub expr_nvars { my ($e) = @_; return () if @$e == 1; my $v = nvar_key($e); return ($v) if defined $v; return () if $e->[0] eq "num"; return map { expr_nvars($_) } @{ $e->[1] } if $e->[0] =~ /^(plus|minus|mul)$/; return () }
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
        my @own = map { $mine{ $_->[2] } ? $fname{ $_->[2] } : "" } field_slots($S);
        pop @own while @own && $own[-1] eq "";
        my $button = (grep { grep { $_ eq $S } @{ $pd->{reals}{$_} } } @{ $pd->{buttons} })[0];
        push @acts, "act(" . q_atom($S) . ", " . q_string($button) . ", ["
            . join(", ", map { q_string($_) } @own) . "], " . ($pd->{said}{$S} // "none") . ", " . q_string($tab) . ", " . ($pd->{by}{$S} // "any") . ")";
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
my (@stands, %stand_seen);
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
        for my $S (@cl) {
            my $t = "standing(" . q_atom($S) . ", " . q_atom($start) . ", " . q_atom($stop) . ")";
            push @stands, $t unless $stand_seen{$t}++;
        }
    }
}
if (@faults) { print STDERR @faults; exit 1 }
# The term printed, pragmas(Lists, Acts, Stands, Look, Version, Text) (README.md beside this file, 1): the look of the contract itself, look(Icon, Family) or nolook; its version, its own line or, having none, the line of the nearest contract it extends that has one, or version(1, 1); and its clauses as text, the lines of its NAME.txt after the meta-information up to its first pragmas heading, joined by a newline (vGLP b413fe1, Section 5.4: the platform exports "contract(Text, Schemas), the contract it is compiled from, its clauses as text and its schemas with the pragmas").
my $lk = $look{$top} ? "look(" . q_string($look{$top}[0]) . ", " . q_string($look{$top}[1]) . ")" : "nolook";
my ($vf) = grep { $version{$_} } reverse @order;
my $vn = $vf ? "version($version{$vf}[0], $version{$vf}[1])" : "version(1, 1)";
my @cl;
for my $x (@own) { next if $x =~ /^%/; last if any_head($x); push @cl, $x }
pop @cl while @cl && $cl[-1] !~ /\S/;
my $tx = join("\n", @cl);
$tx =~ s/\\/\\\\/g; $tx =~ s/"/\\"/g; $tx =~ s/\n/\\n/g;
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @acts) . "], [" . join(", ", @stands) . "], $lk, $vn, " . q_atom("\"$tx\"") . ")\n";
' "$1" "$PROGRAMS" "${2:-}"
