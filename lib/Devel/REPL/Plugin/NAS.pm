package Devel::REPL::Plugin::NAS;
use Devel::REPL::Plugin;

our $VERSION = 0.0001_01;
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

    package StringWithNewline;
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
        History
        FancyPrompt
        OutputCache
        MultiLine::PPI
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

    my $doc = PPI::Document->new(\$code);
    my $quotes = $doc->find('Token::Quote::Interpolate') || [];
    foreach my $tok ( @$quotes ) {
        next if $tok !~ m/^qc(.)(.*)(.)/s; # weak, FIXME
        $tok->{content} = qq# \$s->cmd(" $2 ") #;
    }

    return $doc->serialize;
}

# make q{} work whether in Perl mode or #perl turtle
around 'mangle_line' => sub {
    my ( $orig, $self, $code ) = @_;
    return $self->$orig( $self->munge_qc($code) );
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

    my @ret = $self->$feval(qq#
        my \$s = Net::Appliance::Session->new('$host');
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
    my @output = $self->eval(qq# \$s->cmd(" $code ") #);
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

# before and after main run loop, set NAS environment, etc
around 'run' => sub {
    my ( $orig, $self, @args ) = @_;

    $self->fancy_prompt(\&nas_prompt);
    $self->fancy_continuation_prompt(\&nas_continuation_prompt);
    $self->formatted_eval('use Net::Appliance::Session');
    $self->$orig(@args);
    $self->nas_disconnect;
};

# for Plugin::FancyPrompt
sub nas_prompt {
    my $self = shift;
    if ($self->nas_cli_mode) {
        my $s = $self->get_nas_session;
        return 'not_connected> ' if !blessed($s);
        return $s->last_prompt .' ';
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
        print $fh "\nClosing Net::Appliance::Session connection.\n";
        return $self->eval('$s->close');
    }
    $self->print("\n");
}

1;

__END__

=head1 NAME

Devel::REPL::Plugin::NAS - Add Perl to your network devices' command line interfaces

=head1 VERSION

This document refers to version 0.0001_01 of Devel::REPL::Plugin::NAS

=head1 WARNING

This is an I<ALPHA RELEASE>. I'd really appreciate any bug reports; you can
use the CPAN RT bug tracking system, or email me directly at the address at
the bottom of this page.

You probably also want to download the latest C<Devel::REPL> code from its
subversion repository, as it contains many updates to the version on CPAN.

=head1 PURPOSE

Whilst running an automated interactive session on a network device (e.g.  a
router) using L<Net::Appliance::Session>, the device may throw an error. You
can be dropped into a 'shell' on the device, for manual debugging, but the
shell also has Perl bells and whistles.

Alternatively, if used standalone, this module makes it seem like your network
device manufacturer embedded Perl in their device's CLI. That's pretty cool.

=head1 SYNOPSIS

 my $repl = Devel::REPL->new;
 $repl->load_plugin('NAS');
 $repl->run;
 
You're now at a Devel::REPL shell.
 
 re.pl:001:0> 3+3
 6
 
 re.pl:002:0> #nas_connect hostname.example username password
 $Net_Appliance_Session1 = Net::Appliance::Session=GLOB(0x92165ac);
 
You're now conected via SSH to the device and at its CLI.
 
 TEST_3750# show int status | incl 15
 Fa1/0/15  OWL visitor        notconnect   97           auto   auto 10/100BaseTX
 
 TEST_3750# conf t
 Enter configuration commands, one per line.  End with CNTL/Z.

 TEST_3750(config)# exit
 
Run a one-off perl command:

 TEST_3750# #perl 3+6
 9

Switch to Perl mode:

 TEST_3750# #nas_perl
 Switched into Perl mode.
 re.pl:008:0> 3+9
 12

Run a one-off command on the device:

 re.pl:009:0> #nas show int status | incl 14
 Fa1/0/14  OWL VPN            notconnect   98           auto   auto 10/100BaseTX

Use a Quoted Command operator to run device comands from within Perl code:

 re.pl:010:0> my @output = qc{ show int status };

Switch back to device command mode:

 re.pl:011:0> #nas_cli
 Switched into NAS CLI mode.
 TEST_3750# 

Press C<control+d> to cleanly disconnect, from Perl or NAS CLI mode.

=head1 DESCRIPTION


=head1 USAGE


=head1 CAVEATS


=head1 TODO

=over 4

=item Fix command output, as it suffers from the 'stringified list indent'
issue.

=back

=head1 REQUIREMENTS

Other than the standard contents of the Perl distribution, you will need:

=over 4

=item L<Devel::REPL>

=item L<Net::Appliance::Session>

=back

=head1 AUTHOR

Oliver Gorwits C<< <oliver.gorwits@oucs.ox.ac.uk> >>

=head1 ACKNOWLEDGEMENTS

All the helpful people in C<#moose> on IRC.

=head1 COPYRIGHT & LICENSE

Copyright (c) Oliver Gorwits 2008. All Rights Reserved.

This program is free software; you can redistribute it and/or modify it under
the terms of version 2 of the GNU General Public License as published by the
Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
St, Fifth Floor, Boston, MA 02110-1301 USA

=cut

