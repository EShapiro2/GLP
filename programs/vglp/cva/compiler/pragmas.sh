#!/bin/bash
# The pragmas of a contract, as the term vGLP's compiler reads them (README.md beside this file, "The pragmas"; vGLP, Section 5.4, "The contract's words"; GSC, Section 2, its first paragraph): the lines after the line "Pragmas" of the contract's NAME.txt and of the NAME.txt of every contract it extends, by its "% Extends:" line and theirs, the contracts extended first.  A contract whose own NAME.txt has no line "Pragmas" has none, no_pragmas, and is compiled as before (Code #14's brief, 2026-10-23: "A contract with no pragmas --- GC, GB, GSNP, GAI, CSSN, GFWC today --- compiles as before but for the declarations").
#
#   bash programs/vglp/cva/compiler/pragmas.sh <NAME.txt>
#
# prints pragmas(Lists, Acts) or no_pragmas on one line, each string as its GLP source text inside a quoted name, since the compiler's printer writes a constant as it is.  The lines after "Pragmas" up to the first that names a clause are lists, one for each tab, "Name: what it lists", list(Name, Words), Words the words of what it lists, in lower case, each also without a final s; every other line is "Clause: [Button] ... <field> ... sentence", a button for each act of the clause a person proposes, and an act for each button, act(Schema, Button, Fields, Sentence), Schema the button's text in lower case, each space an underscore, the name of the schema it proposes ("a button for each act of it a person proposes, in brackets, each bearing its act's name", GSC 2991379, Section 2), the fields and the sentence of the line each act's, the clause being one of that title in the same NAME.txt or, where it has none, in the NAME.txt of a contract it extends, Sentence said(Questions) or none, each question a sentence of the line after its fields, the question put to the person in each role whose will the act needs beyond the proposer's, in their order (GSC, Section 2, its first paragraph: "for each role whose will the act needs beyond the proposer's, in their order, the question the person in it is asked"), each its words, each word text(S) or person(I), a name the person I-th among the names the line's clause uses, in the order they first occur in it, so that the head's persons, its roles then its agent variables, are named in order by the clause's names (vGLP, Section 5.4, "The contract's words": "A question names the roles of its schema by the names its clause uses, in the order of the roles"); a name is a word of a capital and lower-case letters that a clause of the contract, or of a contract it extends, writes within a sentence and not first in it, as Alice, Bob, Carol, Dave and Alf.  Each act carries, last, the name of the tab of its contract, the first line after "Pragmas" of the NAME.txt the act's line is in: act(Schema, Button, Fields, Sentence, Tab).  A contract extended is found beside the contract, or else under programs/ by its NAME.txt.  Exits 1, naming the line, on a pragma it cannot read or a clause it cannot find; 2 on a bad argument.

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
        my ($n, $a) = @args[0, 1];
        next if !defined $a || exists $svars{$n};
        push @snames, $n;
        my %local = map { $_ => 1 } local_nums(@args);
        $a =~ s/^\[//; $a =~ s/\]$//;
        $svars{$n} = [ grep { !($local{$_} && $a =~ /\bnum\(\Q$_\E\)/) } ($a =~ /\w+\((\w+)\)/g) ];
        # The peer of a meeting schema is the first place a field names, as cvac.glp counts them (field_places, peer_first): a person the act names by key, as the field child of the enter schema of CSSN.
        unshift @{ $svars{$n} }, "#peer" if meeting($args[-1]);
    }
}
# Whether the roles of a schema, the text of its role list, make it a meeting schema: two party roles, both guarded, whose atoms required or deleted at each name neither the other role nor a name term built on it (cvac.glp, meeting_schema; vGLP, Section 5.4, "The compilation").
sub meeting {
    my ($roles) = @_;
    $roles =~ s/^\[//; $roles =~ s/\]$//;
    my @rs = top_args($roles);
    return 0 unless @rs == 2;
    for my $r (0, 1) {
        my ($f) = $rs[$r] =~ /^(rs)\(/ or return 0;
        my ($body) = inside($rs[$r], length $f);
        my @p = top_args($body);
        return 0 unless $p[0] eq "guarded";
        my $o = 2 - $r;
        return 0 if "$p[2] $p[3]" =~ /\b(?:role|own)\($o\)/;
    }
    return 1;
}
# The schemas a button realises (the ruling of GSC, Code_inbox.md, 2026-10-24 08:06 UTC: the button names the schemas bearing its name or its name with a suffix after an underscore): the schema of its name, the button in lower case, and every schema named by it and a suffix that no other button names exactly, as [Post own] names post_own and so [Post] does not; the button alone where the term is not given or none is found.
my %seen; my @order;
my %exact;
sub realising {
    my ($act) = @_;
    return ($act) if !@snames;
    unless (%exact) {
        $exact{""} = 1;
        for my $f (@order) {
            my $after = 0;
            for my $x (read_txt($f)) {
                $after = 1, next if $x eq "Pragmas";
                next unless $after;
                for my $b ($x =~ /\[([^\]]+)\]/g) { (my $e = lc $b) =~ s/ +/_/g; $exact{$e} = 1 }
            }
        }
    }
    my @r = grep { $_ eq $act || (index($_, "${act}_") == 0 && !$exact{$_}) } @snames;
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
        my $group = $rest;
        while ($group ne "") {
            my @buttons;
            my $tail = $group;
            while ($tail =~ s/^\[([^\]]+)\]\s*//) { push @buttons, $1 }
            @buttons or die "pragmas.sh: $f: the pragma \"$title\" has no button in brackets\n";
            my @fields;
            while ($tail =~ s/^<([^>]+)>\s*//) { push @fields, $1 }
            # The words up to the next button in brackets are this group of buttons, the rest the next group: a line may carry several, each its buttons, its fields and its questions, as an Escrow line its [Deposit], [Release] and [Return].
            $group = ($tail =~ s/\s*(\[.*)$//) ? $1 : "";
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
}
# A schema two lines name, of a contract and of one it extends, keeps the line of the contract that extends the other, read last, as the Authorship line of GSNP names sign after the Developer line of GAI.
my (%last, @kept);
for my $i (0 .. $#acts) { my ($n) = $acts[$i] =~ /^act\((\x27[^\x27]*\x27)/; $last{$n} = $i }
for my $i (0 .. $#acts) { my ($n) = $acts[$i] =~ /^act\((\x27[^\x27]*\x27)/; push @kept, $acts[$i] if $last{$n} == $i }
print "pragmas([" . join(", ", @lists) . "], [" . join(", ", @kept) . "])\n";
' "$1" "$PROGRAMS" "${2:-}"
