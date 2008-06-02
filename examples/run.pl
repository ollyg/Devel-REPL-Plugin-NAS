#!/usr/bin/perl

use Devel::REPL;

my $repl = Devel::REPL->new;
$repl->load_plugin('NAS');
$repl->run;

