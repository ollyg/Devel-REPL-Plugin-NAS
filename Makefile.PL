use 5.008004;
use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    NAME              => 'Devel::REPL::Plugin::NAS',
    VERSION_FROM      => 'lib/Devel/REPL/Plugin/NAS.pm', # finds $VERSION
    PREREQ_PM         => {
        Devel::REPL => 0,
        Net::Appliance::Session => 0,
    },
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT       => 'Add Perl to your network devices\' command line interfaces',
       AUTHOR         => 'Oliver Gorwits <oliver.gorwits@oucs.ox.ac.uk>') : ()),
);
