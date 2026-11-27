# fns.pl --- the functions a file of typed GLP procedures gives, read by
# pragmas.sh and libfns.sh beside it (README.md, items 44 and 47).
use strict; use warnings;

sub fns_q_atom {   # a GLP quoted name holding the text $s
    my ($s) = @_; $s =~ s/\\/\\\\/g; $s =~ s/\x27/\\\x27/g; return "\x27$s\x27";
}

# fns_of_file($file, \@fns, \@lines, \%seen): each procedure the file declares
# is a function, fn(Name, Kinds, Kind), the kinds of its arguments and of its
# value, its last argument, by their declared types, pushed on @fns, and the
# file's lines pushed on @lines, which the platform carries.  A file that
# exports a procedure gives as functions only the procedures it exports, the
# others being their helpers; one that exports none gives every procedure it
# declares.  The lines are carried without the word exported, the platform
# exporting what it exports.
sub fns_of_file {
    my ($ff, $fns, $lines, $seen) = @_;
    open(my $h, "<", $ff) or die "pragmas.sh: cannot read $ff\n";
    my @fl = <$h>; close $h; chomp @fl;
    my $exp = grep { /^exported\s+procedure\s/ } @fl;
    for my $x (@fl) {
        my $e = ($x =~ s/^exported\s+(procedure\s)/$1/);
        if ($x =~ /^procedure\s+([a-z]\w*)\((.*)\)\.\s*$/ && ($e || !$exp)) {
            my ($n, $a) = ($1, $2);
            my ($d, $cur, @ts) = (0, "");
            for my $ch (split //, $a) { $d++ if $ch eq "(" || $ch eq "["; $d-- if $ch eq ")" || $ch eq "]"; if ($ch eq "," && $d == 0) { push @ts, $cur; $cur = "" } else { $cur .= $ch } }
            push @ts, $cur;
            # The kind of each place, by its declared type: a number (Integer, Number), a constant (Constant, String) or other, which the compiler does not take.
            my @k = map { (my $t = $_) =~ s/^\s+|\s+$//g; $t =~ s/\?$//; $t =~ /^(Integer|Number)$/ ? "num" : $t =~ /^(Constant|String)$/ ? "const" : "other" } @ts;
            my $v = pop @k;
            die "pragmas.sh: $ff: the function $n is declared twice\n" if $seen->{$n}++;
            push @$fns, "fn($n, [" . join(", ", @k) . "], $v)";
        }
        push @$lines, fns_q_atom($x);
    }
}

# lib_fns($programs, $term, \@fns, \@lines, \%seen): the functions of the
# standard library, programs/lib/numbers/numbers.glp, whole numbers' functions
# whose owner is the library (GLP #120, #123), where the contract's reading,
# $term, applies one of them, fn(Name, ...), as a function the owner of a value
# type gives; nothing where it applies none, so that such a contract prints
# what it printed before.  A contract whose owner also gives a function of a
# name the library exports is refused, the platform carrying both.
sub lib_fns {
    my ($programs, $term, $fns, $lines, $seen) = @_;
    return unless defined $term && length $term;
    my $lf = "$programs/lib/numbers/numbers.glp";
    return unless -f $lf;
    open(my $h, "<", $lf) or die "pragmas.sh: cannot read $lf\n";
    my @ex = map { /^exported\s+procedure\s+([a-z]\w*)\(/ ? ($1) : () } <$h>;
    close $h;
    return unless grep { $term =~ /\bfn\(\s*\Q$_\E\s*,/ } @ex;
    for my $n (@ex) { die "pragmas.sh: the function $n is given by the contract\x27s owner and by the standard library, $lf\n" if $seen->{$n} }
    fns_of_file($lf, $fns, $lines, $seen);
}
1;
