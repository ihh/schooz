#!/usr/bin/env perl -w

print "1..2\n";

my $guile = "guile";
my @version = `$guile --version`;

print grep (/Guile 1\.8/, @version) ? "ok" : "not ok",
    " 1 - guile is version 1.8\n",
    map (" $_", @version);

my @transcript = `cat t/rock.walkthru | $guile --debug -s t/rock.guile-1.8.scm`;
my @expected = `cat t/rock.transcript`;

my $match = @transcript == @expected && grep ($transcript[$_] ne $expected[$_], 0..$#transcript) == 0;

print $match ? "ok" : "not ok",
    " 2 - rock transcript matches exactly\n",
    map (" $_", @transcript);
