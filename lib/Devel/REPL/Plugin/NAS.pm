package Devel::REPL::Plugin::NAS;
use Devel::REPL::Plugin;
use strict;

our $VERSION = '0.0701';
$VERSION = eval $VERSION; # numify for warning-free dev releases

# $Id$

use namespace::clean except => ['meta'];

BEGIN {
    # insert a new token qc{} for device commands into PPI
    {
        use PPI;
        no warnings 'once';
        $PPI::Token::Word::QUOTELIKE{qc} = 'Quote::Interpolate';
        $PPI::Token::_QuoteEngine::Full::quotes{qc} = {
            operator  => 'qc',
            braced    => undef,
            separator => undef,
            _sections => 1
        };
    }

    package # hide from PAUSE
        StringWithNewline;
    use Moose::Util::TypeConstraints;
    
    subtype 'StringWithNewline'
        => as 'Str',
        => where { $_ =~ m/\n$/ };
    
    coerce 'StringWithNewline'
        => from 'Str',
        => via {
            my $val = $_;
            chomp $_;
            "$_\n";
        };
    
    no Moose::Util::TypeConstraints;
}

# load required and helpful plugins (safely)
sub BEFORE_PLUGIN {
    my $self = shift;
    for my $plugin (qw(
        LexEnv
        Turtles
        FancyPrompt
        OutputCache
        MultiLine::PPI
        Completion
    )) {
        $self->load_plugin($plugin);
    }
}

sub AFTER_PLUGIN {
    my $self = shift;

    # N::A::S cmd with -options
    $self->add_turtles_matcher(
        qr/^
            \#(nas) \s+
            ( (?:\-\w+\s+)* ) # options for nas
            (.*)              # the command
        /x
    );

    # N::A::S connect helper with three args
    $self->add_turtles_matcher(
        qr/^
            \#(nas_connect) \s+
            (\S+)  \s+          # host
            (\S+)  \s+          # user
            (.*)                # pass
        /x
    );

    $self->formatted_eval('use Net::Appliance::Session 1.23');
    $self->formatted_eval('use File::Slurp'); # very useful for NAS
    $self->fancy_prompt(\&nas_prompt);
    $self->fancy_continuation_prompt(\&nas_continuation_prompt);
}

# used by format_error to show the last command sent
has 'nas_command_cache' => (
    is      => 'rw',
    isa     => 'StringWithNewline',
    default => "\n",
    coerce  => 1,
);

# are we in Perl or CLI mode?
has 'nas_cli_mode' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 0,
);

# #nas turtle toggle to get _ munged into a scalar
# -scalar or -array
has 'nas_want_scalar_result' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 0,
);

# #nas turtle toggle to display NAS cmd response 
# -our or -noout
has 'nas_display_command_response' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 1,
);

# takes perl code and rewrites qc{} into $s->cmd()
sub munge_qc {
    my ($self, $code) = @_;

    my $doc = PPI::Document->new(\$code)
        or return '';
    my $quotes = $doc->find('Token::Quote::Interpolate') || [];
    foreach my $tok ( @$quotes ) {
        next if $tok !~ m/^qc(.)(.*)(.)/s; # weak, FIXME
        $tok->{content} = qq# \$s->cmd("$2") #;
    }

    return $doc->serialize;
}

# make q{} work whether in Perl mode or #perl turtle
around 'mangle_line' => sub {
    my ( $orig, $self, $code ) = @_;
    return $self->$orig( $self->munge_qc($code) );
};

# stop the interpolated list print indenting problem
around 'print' => sub {
    my ( $orig, $self, @args ) = @_;
    {
        local $" = '';
        return $self->$orig( @args );
    }
};

# TURTLE COMMAND for calling perl from NAS CLI mode
sub expr_command_perl {
    my ( $self, $feval, $code ) = @_;
    return $self->$feval( $code );
}

# intercept device cli commands in NAS CLI mode,
# send them off to nas_run_command()
around 'formatted_eval' => sub {
    my ( $orig, $self, @args ) = @_;

    if ($self->nas_cli_mode and !$self->match_turtles(@args)) {
        return $self->nas_run_command(@args);
    }
    return $self->$orig(@args);
};

# TURTLE COMMAND for disconnecting from device
sub command_nas_disconnect {
    my ( $self, $feval, @args ) = @_;
    return $self->nas_disconnect;
}

# TURTLE COMMAND for switching into Perl mode
sub command_nas_perl {
    my ( $self, $feval, @args ) = @_;
    $self->nas_cli_mode(0);
    return 'Switched into Perl mode.';
}

# TURTLE COMMAND for switching into NAS CLI mode
sub command_nas_cli {
    my ( $self, $feval, @args ) = @_;
    $self->nas_cli_mode(1);
    return 'Switched into NAS CLI mode.';
}

# TURTLE COMMAND helper for N::A::S->connect()
sub command_nas_connect {
    my ( $self, $feval, $host, $user, $pass ) = @_;
    die unless $host;

    my $s = $self->get_nas_session;
    my $my = (blessed($s) ? '' : 'my ');

    my @ret = $self->$feval(qq#
        $my \$s = Net::Appliance::Session->new('$host');
        \$s->connect(Name => '$user', Password => '$pass');
    #);
    $self->nas_cli_mode(1);
    return @ret;
};

# TURTLE COMMAND for calling NAS CLI from Perl mode
sub command_nas {
    my ( $self, $feval, $opts, $code ) = @_;
    die unless $code;

    my %opts = map { $_ => 1 } ( split /\s+/, $opts );

    my $out = $self->nas_display_command_response;
    $out = 0 if exists $opts{'-noout'};
    $out = 1 if exists $opts{'-out'};

    my $want_scalar = $self->nas_want_scalar_result;
    $want_scalar = 1 if exists $opts{'-scalar'};
    $want_scalar = 0 if exists $opts{'-array'};

    my @output = $self->nas_run_command($code);

    $self->output_cache->[-1] = join '', @output
        if $want_scalar;

    return @output if $out;
    return 1;
}

# does the heavy lifting of running an NAS CLI command safely
sub nas_run_command {
    my ($self, $code, @args) = @_;
    my $s = $self->get_nas_session;
    return $s if !blessed($s);

    my $telnet = *$s->{net_telnet};
    my $output = '';
    open( my $log, '>', \$output );

    my $saved_log = $telnet->{inputlog};
    $telnet->{inputlog} = $log;

    #$s->input_log( *STDOUT );
    my @output = $self->eval(qq# \$s->cmd("$code") #);
    $telnet->{inputlog} = $saved_log;
    close $log;

    $self->nas_command_cache($code);
    if ( $self->is_error($output[0]) ) {
        return $self->format_error(@output);
    }

    return @output;
}

# check for N::A::S Exception (or Devel::REPL X)
around 'is_error' => sub {
    my ( $orig, $self, @args ) = @_;
    my $e = $args[0];

    $self->$orig(@args) or is_nas_exception($args[0]);
};

# special dump of NAS Exception object on NAS error
around 'format_error' => sub {
    my ( $orig, $self, @args ) = @_;

    if (is_nas_exception($args[0])) {
        my $e = $args[0];
        my @message = (
            "Error returned from Net::Appliance::Session!\n",
            'Last command sent: '. $self->nas_command_cache,
            'Last response    : '. $e->lastline,
            'Fault Description: '. $e->message,
        );
        push @message, "Net::Telnet error: ". $e->errmsg ."\n"
            if $e->errmsg =~ m/\S/;
        return @message;
    }
    return $self->$orig(@args);
};

# skip Devel::REPL's error message if it's a NAS Exception
around 'error_return' => sub {
    my ( $orig, $self, @args ) = @_;

    if (is_nas_exception($args[1])) {
        return $args[1];
    }
    return $self->$orig(@args);
};

sub is_nas_exception {
    my $e = shift;
    return ( blessed($e) and $e->isa("Net::Appliance::Session::Exception") );
}

# grab $s if we can, non intrusively
# FIXME: this should probably use FindVariable plugin instead
sub get_nas_session {
    my $self = shift;
    my $s = ${ $self->lexical_environment->get_member_ref('$', '_', '$s') };
    if (!defined $s) {
        # get_member_ref autovivifies $s, so zap it
        my $context = $self->lexical_environment->get_context('_');
        delete $context->{'$s'};
        $self->lexical_environment->set_context('_', $context);
        return "Error! - No Net::Appliance::Session session is active.\n";
    }
    return $s;
}

# command completion - only works when in NAS CLI mode
# requires updated version of N::A::S
around '_completion' => sub {
    my ( $orig, $self, $text, $line, $start, $end ) = @_;

    return $self->$orig($text, $line, $start, $end)
        if ! $self->nas_cli_mode;

    substr($line, $end) = '';
    my @output = $self->nas_run_command($line .'?');
    pop @output;
    $self->print("\n", @output);
    $self->term->reset_line_state;
    $self->$orig($text, $line, $start, $end);
};

# after running main loop, cleanly disconnect
after 'run' => sub {
    my $self = shift;
    $self->nas_disconnect;
};

# for Plugin::FancyPrompt
sub nas_prompt {
    my $self = shift;
    if ($self->nas_cli_mode) {
        my $s = $self->get_nas_session;
        return 'not_connected> ' if !blessed($s);
        if (my $last = $s->last_prompt) {
            return "$last ";
        }
        else {
            $self->nas_disconnect;
            return 'not_connected> ';
        }
    }
    return sprintf 're.pl:%03d%s> ',
                   $self->lines_read,
                   $self->can('line_depth') ? ':' . $self->line_depth : '';
}

# for Plugin::FancyPrompt
sub nas_continuation_prompt {
    my $self = shift;
    return sprintf 're.pl:%03d%s* ',
                   $self->lines_read, $self->line_depth;
}

# does heavy lifting for user disconnect turtle or end of session disconnect
sub nas_disconnect {
    my $self = shift;
    my $fh = $self->out_fh;

    if (blessed($self->get_nas_session)) {
        print $fh "Closing Net::Appliance::Session connection.\n";
        $self->nas_cli_mode(0);
        return $self->eval('$s->close');
    }
    $self->print("\n");
}

1;

__END__

=head1 NAME

Devel::REPL::Plugin::NAS - Add Perl to your network devices' command line interfaces

=head1 VERSION

This document refers to version 0.0701 of Devel::REPL::Plugin::NAS

=head1 WARNING

This is an I<ALPHA RELEASE>. I'd really appreciate any bug reports; you can
use the CPAN RT bug tracking system, or email me directly at the address at
the bottom of this page.

You probably also want to download the latest C<Devel::REPL> code from its
subversion repository, as it contains many updates to the version on CPAN.

=head1 PURPOSE

Whilst running an automated interactive session on a network device (e.g. a
router) using L<Net::Appliance::Session>, the device may throw an error. You
can then be dropped into a 'shell' on the device, for manual debugging, but
the shell also has Perl bells and whistles.

Alternatively, if used standalone, this module makes it seem like your network
device manufacturer embedded Perl in their device's CLI. That's pretty cool.

=head1 SYNOPSIS

 my $repl = Devel::REPL->new;
 $repl->load_plugin('NAS');
 $repl->run;

Or,

 $> /usr/bin/nsh hostname.example.com
 Username [oliver]:
 Password: 
 Net::Appliance::Session=GLOB(0x8e7db7c)
 
 TEST_3750# dir flash:
 Directory of flash:/
 
     4  -rwx        7874  May 22 2008 11:43:53 +01:00  config.text
   459  drwx         192  Jan 31 2007 15:53:38 +00:00  c3750-ipbasek9-mz.122-25.SEE2
 15998976 bytes total (6618624 bytes free)
 
 TEST_3750# #nas_perl
 Switched into Perl mode.
 re.pl:003:0> write_file ('config.text', qc{more flash:config.text});
 1
 re.pl:004:0> ^D
 Closing Net::Appliance::Session connection.
 $> 

=head1 DESCRIPTION

There is a module, L<Net::Appliance::Session> (NAS), which allows you to
automate interactive sessions with a network device CLI (e.g. a router or
switch). It's like a smarter and more Perlish version of Expect. A couple of
users asked me whether NAS could provide better handling of errors received
from the remote device, rather than simply die'ing.

Around the same time, I was reading about Cisco having included the tcl
scripting language in their IOS software, and thinking about an IOS with an
embedded Perl. Time passed, and then I started playing with the L<Devel::REPL>
interactive shell for Perl, and realised there was the potential to create a
Perl/NAS shell.

This module is a plugin for C<Devel::REPL> which allows the one shell to be
used for both Perl commands as well as sending commands to a device connected
via C<Net::Appliance::Session>. There is special support for managing the
connection to your device, and easily issueing commands either to Perl or the
device, in succession, or combining the two to grab NAS output into Perl
variables.

=head1 USAGE

Load up the plugin either by including it in your C<repl.rc> file, or running
the following small Perl program:

 my $repl = Devel::REPL->new;
 $repl->load_plugin('NAS');
 $repl->run;

At this point you'll be at the REPL shell, which has a funny-looking prompt
telling you the line number and script name. You can issue any Perl syntax
here to be executed, including multi-line statements (e.g. C<for> loops),
loading of modules, and so on. Lexical variables (created using C<my>) will
persist as long as the shell is running.

 re.pl:001:0> print "Hello, World";
 Hello World
 
Once you're done with seeing how cool that is, you can make a connection to
your network device using a C<Devel::REPL> I<macro> (stricty, known as a
I<turtle>). REPL macros begin with the C<#> character, and sometimes take
command arguments; in this case the hostname, username and password:

 re.pl:002:0> #nas_connect hostname.example username password
 $Net_Appliance_Session1 = Net::Appliance::Session=GLOB(0x92165ac);

You are shown the return value of the statement, which happens in this case to
be a C<Net::Appliance::Session> object, if the connection was successful.
You'll get an error message if the connection wasn't successful:

 re.pl:002:0> #nas_connect hostname.example username notmypassword
 Error returned from Net::Appliance::Session!
  Last command sent: 
  Last response    : Permission denied, please try again.
  Fault Description: Login failed to remote host at (eval 230) line 8
  Net::Telnet error: pattern match timed-out

At this point, the REPL switches from I<Perl mode> into I<NAS CLI mode>. It's
the same REPL process, but your commands are sent straight to the network
device, rather than being interpreted as Perl. The prompt also changes:

 TEST_3750# show int status | incl 15
 Fa1/0/15  OWL visitor        notconnect   97           auto   auto 10/100BaseTX
 
 TEST_3750# conf t
 Enter configuration commands, one per line.  End with CNTL/Z.
 
 TEST_3750(config)#

To disconnect from the device and quit the REPL in one go, hit C<Control+d>.
To just disconnect from the device but remain at the REPL shell in Perl mode,
use the following macro (from either NAS CLI or Perl mode):

 TEST_3750(config)# #nas_disconnect
 Closing Net::Appliance::Session connection.

=head1 FEATURES AND AVAILABLE COMMANDS

If that were all there was to it, you may as well be using SSH directly. But
no, as mentioned above, you can issue Perl from within NAS CLI mode, issue
network device commands from within Perl mode, and more.

=head2 Switching between Perl and NAS CLI mode

There are two macros for moving between Perl and NAS CLI mode. To clarify, the
mode is only setting the default action for commands received by the REPL
shell - whether they are interpreted as Perl, or sent to the network device.
You can still perform the I<other> kind of command from either mode, as we'll
see in the next section.

To switch from Perl mode to NAS CLI mode:

 re.pl:008:0> #nas_cli
 Switched into NAS CLI mode.
 TEST_3750# show int status
 
To switch from NAS CLI mode to Perl mode:

 TEST_3750# #nas_perl
 Switched into Perl mode.
 re.pl:011:0> print "Hello, world"

=head2 The Quoted (interpolated) Command operator

Let's say you want to run a command on your network device, and store the
results of that in an array. So far we've only seen how to use I<either> Perl
mode I<or> NAS CLI mode. The Quoted (interpolated) Command operator is a
convenience feature to help you out in this situation.

This operator is just like the other interpolated Perl quote-like operators
such as C<qx{}> and C<qq{}>, except that it is C<qc{}> (for Quoted Command).
The same rules apply for substituting the curly brace characters as do for the
other Perl quote-like operators. Here is an example:

 re.pl:015:0> my @output = qc{ show int status }

I've found this to be one of the most useful features - being able to grab
command output and munge it in Perl, whilst keeping the remote session open.

=head2 One-off commands in an alternate mode

If you're in NAS CLI mode, and you want to run a quick bit of Perl (remember,
all lexical variables persist, which is handy), then use the C<#perl> macro:

 TEST_3750# #perl print "Hello again, World";
 Hello again, World

Likewise, to send a command to the network device when you're in Perl mode,
use the C<#nas> macro:

 re.pl:012:0> #nas show int status | incl 14
 Fa1/0/14  OWL VPN            notconnect   98           auto   auto 10/100BaseTX

As you can see, by default the output from the command is printed out to the
shell. There's an option to suppress this, if you want, which is enabled by
adding a flag C<-noout> to the macro, like so:

 re.pl:013:0> #nas -noout show int status | incl 14
 re.pl:014:0>

=head2 Command results cache

This module will store the output from all Perl and network device commands in
a cache. This is very useful should you want to post-process any of the data
output by a command.

By default all output is stored in array context, which means that if there
are many lines returned each one goes into an element of an array, and a
reference to I<that> array gets stored in the cache.

If you'd prefer to have the output from a network device command stored as one
big scalar (all the lines joined together), then this is possible via the
C<#nas> macro, by adding the C<-scalar> flag, like so:

 re.pl:016:0> #nas -scalar show int status

The output cache itself, including all Perl and NAS CLI mode output, is
available via the C<< $_REPL->output_cache >> array reference. The most recent
command's output is available via the C<_> REPL magic variable (no sigil!).

=head2 Accessing the C<Net::Appliance::Session> object

Simply, this is available as C<$s> in your REPL shell. That means it's a good
idea not to create a lexical scalar variable of the same name. Note that C<$s>
will not exist when there isn't an instantiated object to store, for example
before issueing the C<#nas_connect> macro or after issueing the C<#disconnect>
macro.

One handy thing to do if you're really stuck is drop to lower level testing:

 re.pl:017:0> $s->input_log ( *STDOUT )
 *main::STDOUT
 
 re.pl:018:0> $s->cmd('show int status | incl 15');
 show int status | incl 15
 Fa1/0/15  OWL visitor        notconnect   97           auto   auto 10/100BaseTX
 TEST_3750#Fa1/0/15  OWL visitor        notconnect   97           auto   auto 10/100BaseTX
 
 re.pl:019:0> 

=head2 Additionally loaded modules

Currently the L<File::Slurp> module is loaded by this plugin, if available. It
provides the C<read_file> and C<write_file> functions which are extremely
useful for dumping data from a network device (such as files on flash memory).
For further details pleae see the manual page for that module.

=head1 TODO

=over 4

=item Currently no way to specify the C<Net::Appliance::Session> Transport
when using the connect macro.

=item There's also no way to specify the NAS Phrasebook, which means it
defaults to Cisco IOS.

=back

=head1 REQUIREMENTS

Other than the standard contents of the Perl distribution, you will need:

=over 4

=item *

L<Devel::REPL>

=item *

L<Net::Appliance::Session> >= 1.23

=back

To run the bundled C</usr/bin/nsh> program, you'll need the L<IO::Prompt>
module.

=head1 AUTHOR

Oliver Gorwits C<< <oliver.gorwits@oucs.ox.ac.uk> >>

=head1 ACKNOWLEDGEMENTS

All the helpful people in C<#moose> on IRC.

=head1 COPYRIGHT & LICENSE

Copyright (c) Oliver Gorwits 2008.

This library is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut

