package Devel::REPL::Plugin::NAS;
use Devel::REPL::Plugin;

our $VERSION = 0.0001_01;
# $Id$

use namespace::clean except => ['meta'];

with qw( Devel::REPL::Plugin::LexEnv );
with qw( Devel::REPL::Plugin::Turtles );
with qw( Devel::REPL::Plugin::History );
with qw( Devel::REPL::Plugin::OutputCache );
with qw( Devel::REPL::Plugin::MultiLine::PPI );

BEGIN {
    $PPI::Token::Word::QUOTELIKE{qc} = 'Quote::Interpolate';
    $PPI::Token::_QuoteEngine::Full::quotes{qc} = {
        operator  => 'qc',
        braced    => undef,
        separator => undef,
        _sections => 1
    };

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

has 'nas_command_cache' => (
    is      => 'rw',
    isa     => 'StringWithNewline',
    default => "\n",
    lazy    => 1,
    coerce  => 1,
);

has 'nas_command_mode' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 0,
    lazy    => 1,
);

has 'nas_want_scalar_result' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 0,
    lazy    => 1,
);

has 'nas_display_command_response' => (
    is      => 'rw',
    isa     => 'Bool',
    default => 1,
    lazy    => 1,
);

# matches: #nas -echo -scalar show interface status
sub AFTER_PLUGIN {
    my $self = shift;
    $self->add_turtles_matcher(
        qr/^
            \#(nas) \s+
            ( (?:\-\w+\s+)* ) # options for nas
            (.*)              # the command
        /x
    );
    $self->add_turtles_matcher(
        qr/^
            \#(nas_connect) \s+
            (\S+)  \s+          # host
            (\S+)  \s+          # user
            (.*)                # pass
        /x
    );
}

# a magic turtle walking across your screen...
sub expr_command_p {
    my ( $self, $feval, $code ) = @_;

    my $doc = PPI::Document->new(\$code);
    my $quotes = $doc->find('Token::Quote::Interpolate') || [];
    foreach my $tok ( @$quotes ) {
        next if $tok !~ m/^qc(.)(.*)(.)/s;
        $tok->{content} = qq# \$s->cmd(" $2 ") #;
    }

    $self->$feval( $doc->serialize );
}

# for when we're in command mode
around 'formatted_eval' => sub {
    my ( $orig, $self, @args ) = @_;

    if ($self->nas_command_mode and !$self->match_turtles(@args)) {
        return $self->nas_run_command(@args);
    }
    return $self->$orig(@args);
};

# a magic turtle walking across your screen...
sub command_nas_perl {
    my ( $self, $feval, @args ) = @_;
    $self->nas_command_mode(0);
    return 1;
}

# a magic turtle walking across your screen...
sub command_nas_cmd {
    my ( $self, $feval, @args ) = @_;
    $self->nas_command_mode(1);
    return 1;
}

# a magic turtle walking across your screen...
sub command_nas_connect {
    my ( $self, $feval, $host, $user, $pass ) = @_;
    die unless $host;
    #my $pass = join '', @pass;

    $self->formatted_eval("\$s = Net::Appliance::Session->new('$host')");
    $self->formatted_eval("\$s->connect( Name => '$user', Password => '$pass' )");
};


# a magic turtle walking across your screen...
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

sub nas_run_command {
    my ($self, $code, @args) = @_;
    my $s = ${ $self->lexical_environment->get_member_ref('$', '_', '$s') };
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

# check for NAS Exception
around 'is_error' => sub {
    my ( $orig, $self, @args ) = @_;
    my $e = $args[0];

    $self->$orig(@args) or _is_nas_exception($args[0]);
};

# special dump of NAS Exception object
around 'format_error' => sub {
    my ( $orig, $self, @args ) = @_;

    if (_is_nas_exception($args[0])) {
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

around 'error_return' => sub {
    my ( $orig, $self, @args ) = @_;

    if (_is_nas_exception($args[1])) {
        return $args[1];
    }
    return $self->$orig(@args);
};

sub _is_nas_exception {
    my $e = shift;
    return ( blessed($e) and $e->isa("Net::Appliance::Session::Exception") );
}

around 'run' => sub {
    my ( $orig, $self, @args ) = @_;
    my $fh = $self->out_fh;

    $self->formatted_eval('use Net::Appliance::Session');
    $self->$orig(@args);

    print $fh "\nClosing Net::Appliance::Session connection.\n";
    $self->print( $self->formatted_eval('$s->close') );
};

1;

__END__

=head1 NAME

Devel::REPL::Plugin::NAS - Add Perl to your network devices' shells!

=head1 VERSION

This document refers to version 0.0001_01 of Devel::REPL::Plugin::NAS

=head1 WARNING

I am subscribing to the I<release early, release often> method, here. This is
my first use of L<Moose> in a little while, and also my first extension to
L<Devel::REPL>. So please go easy if there's something brain-dead in here. You
can find me on IRC as C<oliver> in C<#moose>.

You probably also want to download the latest C<Devel::REPL> code from its
subversion repository, as the CPAN version contains bugs.

=head1 PURPOSE

You are attempting to debug a command sequence at the CLI of a network device,
such as a switch or router. This module works with C<Devel::REPL> to provide
you with a shell which has Perl, but also sends commands to your device using
L<Net::Appliance::Session>. It might help to think of this as if your device
manufacturer had embedded a Perl interpreter in their CLI.

=head1 SYNOPSIS

 system$ re.pl


use Devel::REPL;
my $repl = Devel::REPL->new;
  $repl->load_plugin($_) for qw(History LexEnv);
    $repl->run

use Net::Appliance::Session;
my $s = Net::Appliance::Session->new('test.frodo.ox.ac.uk');
$s->connect( Name => 'oliver', Password => '');
$_REPL->load_plugin('NAS');
$_REPL->load_plugin('OutputCache');

=head1 DESCRIPTION


=head1 USAGE


=head1 CAVEATS


=head1 TODO

=over 4

=item Fix command output, as it suffers from the 'stringified list indent'
issue.

=back

=head1 SEE ALSO

=over 4

=item L<Net::Appliance::Session>

=back

=head1 REQUIREMENTS

Other than the standard contents of the Perl distribution, you will need:

=over 4

=item L<Moose>

=item L<Devel::REPL>

=item L<PPI>

=item L<Net::Appliance::Session>

=back

=head1 AUTHOR

Oliver Gorwits C<< <oliver.gorwits@oucs.ox.ac.uk> >>

=head1 ACKNOWLEDGEMENTS

All the helpful people in C<#moose> on IRC.

=head1 COPYRIGHT & LICENSE

Copyright (c) The University of Oxford 2008. All Rights Reserved.

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


