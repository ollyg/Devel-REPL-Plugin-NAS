use 5.008004;
use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    NAME              => 'Devel::REPL::Plugin::NAS',
    VERSION_FROM      => 'lib/Devel/REPL/Plugin/NAS.pm', # finds $VERSION
    PREREQ_PM         => {
        Devel::REPL => 0,
        Net::Appliance::Session => 1.23,
    },

    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Devel/REPL/Plugin/NAS.pm',
       AUTHOR         => 'Oliver Gorwits <oliver.gorwits@oucs.ox.ac.uk>') : ()),

    dist        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean       => { FILES => 'Devel-REPL-Plugin-NAS-*' },
    realclean   => { FILES => 'Makefile.old' },
    EXE_FILES   => [ 'usr/bin/nsh' ],
);
