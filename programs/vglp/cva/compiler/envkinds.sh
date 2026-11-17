#!/bin/bash
# The kind of each environment predicate of a contract, as its implementation pragmas say it (vGLP 922ac53, Section 5.4, "Speech-act schemas as local specifications": "An implementation pragma says, of each environment predicate other than the day, whether the phone finds it, as `Found by the phone: x is nearby y.', or the developer gives it, as `Given by the developer: the fraction.'"; the form agreed by GSC #19 and Calculus #8, vGLP_inbox.md, 2026-11-17 12:11 UTC).
#
#   bash programs/vglp/cva/compiler/envkinds.sh <NAME.txt>
#
# prints one line for each predicate an implementation pragma names, "given <predicate>" or "found <predicate>", of the contract and of every contract it is written from (pragmas.sh --from), the contract's own first.  A line under the heading "Implementation pragmas" reads "Given by the developer: <phrases>." or "Found by the phone: <phrases>.", the phrases joined by ";" as the Logic pragmas' line "Environment:" joins them, each phrase that of an entry of the line "Vocabulary:" of the contract or of one it is written from, "<phrase> = <predicate>(<arguments>)", which names the predicate.  A phrase no Vocabulary entry has is refused, named, and the script exits 1.
set -u
[ $# -eq 1 ] && [ -f "$1" ] || { echo "usage: envkinds.sh <NAME.txt>" >&2; exit 2; }
HERE="$(cd "$(dirname "$0")" && pwd)"
FILES=$(bash "$HERE/pragmas.sh" --from "$1" 2>/dev/null)
[ -n "$FILES" ] || FILES="$1"
exec perl -CSD -Mutf8 -e '
use strict; use warnings;
$SIG{__DIE__} = sub { print STDERR $_[0]; exit 1 };
my @files = @ARGV;
my %pred_of;
for my $f (@files) {
    open my $h, "<:encoding(UTF-8)", $f or die "envkinds.sh: $f: cannot be read\n";
    while (my $l = <$h>) {
        chomp $l;
        next unless $l =~ /^Vocabulary:\s*(.*)$/;
        for my $e (split /;\s*/, $1) {
            next unless $e =~ /^\s*(.*?)\s*=\s*([a-z_][A-Za-z0-9_]*)\s*\(/;
            my ($ph, $p) = ($1, $2);
            $ph =~ s/\s+/ /g; $ph =~ s/\.$//;
            $pred_of{$ph} //= $p;
        }
    }
    close $h;
}
my (%seen, @out);
for my $f (reverse @files) {
    open my $h, "<:encoding(UTF-8)", $f or die "envkinds.sh: $f: cannot be read\n";
    my $impl = 0;
    while (my $l = <$h>) {
        chomp $l;
        if ($l =~ /^(Interface pragmas|Implementation pragmas|Pragmas|Logic pragmas)\.?\s*$/) { $impl = ($1 eq "Implementation pragmas"); next }
        next unless $impl;
        next unless $l =~ /^(Given by the developer|Found by the phone):\s*(.*?)\.?\s*$/;
        my $k = $1 eq "Given by the developer" ? "given" : "found";
        for my $ph (split /;\s*/, $2) {
            $ph =~ s/\s+/ /g; $ph =~ s/^\s+|\s+$//g; $ph =~ s/\.$//;
            exists $pred_of{$ph} or die "envkinds.sh: $f: the line \"$1\" names \"$ph\", which no Vocabulary entry has\n";
            my $p = $pred_of{$ph};
            push @out, "$k $p" unless $seen{$p}++;
        }
    }
    close $h;
}
print "$_\n" for @out;
' $FILES
